;==============================================================================
;
;	Method:		chamfer_match
;
;	Description:
;				Coregisters 2 CT images using chamfer matching as described
;				in van Herk M and Kooy HM. Automatic three-dimensional
;				correlation of CT-CT, CT-MRI, and CT-SPECT using chamfer
;				matching. Med. Phys. 21(7):1163-1178 (1994).
;
;	Version:
;				071107
;				First version.
;
;				071123
;				Version used to generate data sent to Huan on 071116.
;
;				080306
;				Changed cost fuctions so that penalties are graded instead
;				of uniform.
;				Changed from transform_volume to apply_transform which
;				accounts for anisotropic voxels.
;				Added display during matching.
;
;				080813
;				Added 3D range selection (instead of just z).
;
;				090727
;				Added PIXSIZE keyword to transform_volume to account for
;				voxel anisotropy.  Added mPixDims to common vars.
;
;	Inputs:
;				2 sets of raw data (fixed and moving)
;
;	Outputs:
;				The moving image is over-written.
;				Returns the 6-parameter transformation matrix.
;
;	Required Modules:
;				textbox.pro
;
;	Written by:
;				Maggie Kusano, November 7, 2007
;
;==============================================================================

function chamfer_match, $
	IMG1=pFixedImg, $
	PIXDIMS1=fixedPixDims, $
	IMG2=pMovingImg, $
	PIXDIMS2=movingPixDims, $	; must be equal to fPixDims
	LIMITS=limits, $			; rot and trans limits in deg and mm
	RANGE=range, $
	MODVAL=modval, $
	SIMPLEX=simplex, $
	POWELL=powell, $
	CHAMFER=chamfer, $
	CITY=city, $
	DT_FILE=dtFile, $
	WRITE_FILE=writeFile, $
	DISPLAY=display, $
	DEBUG=debug, $
	TRANS_VALS=p				; Out: transform values (in deg and pix)

common cost_params, dImg, mCoords, maxVals, bDisplay, dispDims, dispImg, $
		fContMask, zRange, mPixDims

; Make sure image dimensions are the same
fImgDims = size( (*pFixedImg), /DIM )
mImgDims = size( (*pMovingImg), /DIM )
if (total( fImgDims ne mImgDims ) ne 0) or (total( fixedPixDims ne movingPixDims ) ne 0) then begin
	void = dialog_message( /ERROR, 'Image dimensions do not match. Returning.' )
	return, -1L
endif
fPixDims = fixedPixDims
mPixDims = movingPixDims

if n_elements( range ) eq 0 then begin
	range = [[0,0,0],[mImgDims-1]]
endif
if n_elements( modval ) eq 0 then modval = 1
; Select type of optimization
if n_elements( simplex ) eq 0 and n_elements( powell ) eq 0 then simplex = 1b

bDisplay = 0b
if n_elements(display) ne 0 then bDisplay=1b
bDebug = 0b
if n_elements(debug) ne 0 then bDebug=1b

; Prep output
p = [0,0,0,0,0,0]
t3dMat = identity(4)

; Set parameter limits
maxVals = fltarr(6)
if n_elements( limits ) ne 6 then limits = [30,30,30,50,50,50]
maxVals = limits
;maxVals[3:5] /= fPixDims

; Feature extraction (bone)
fMask = (*pFixedImg gt 1400);[*,*,range[0]:range[1]]
mMask = (*pMovingImg gt 1400);[*,*,range[0]:range[1]]
tmpDims = size( fMask, /DIM )

; Contour skull and spine
fContMask = fMask * 0b
mContMask = mMask * 0b
for iSlice=0, tmpDims[2]-1 do begin
	fImg = reform( fMask[*,*,iSlice] )
	if total( fImg ) gt 0 then begin
		indices = contour_mask( fImg, /INDICES, SIZE_THRESH=10 )
		if indices[0] ne -1L then begin
			mask = bytarr( tmpDims[0], tmpDims[1] )
			mask[indices] = 1b
			fContMask[0,0,iSlice] = mask
		endif
	endif
	mImg = reform( mMask[*,*,iSlice] )
	if total( mImg ) gt 0 then begin
		indices = contour_mask( mImg, /INDICES, SIZE_THRESH=10 )
		if indices[0] ne -1L then begin
			mask = bytarr( tmpDims[0], tmpDims[1] )
			mask[indices] = 1b
			mContMask[0,0,iSlice] = mask
		endif
	endif
endfor

points = where( mContMask ne 0, nPoints )
indices = indgen( nPoints )
points = points[where( (indices mod modval) eq 0 )]
mCoords = array_indices( mContMask, points )
indices = where( (mCoords[0,*] ge range[0,0]) and (mCoords[0,*] le range[0,1]) $
		and (mCoords[1,*] ge range[1,0]) and (mCoords[1,*] le range[1,1]) $
		and (mCoords[2,*] ge range[2,0]) and (mCoords[2,*] le range[2,1]), count )
if count gt 0 then mCoords = [mCoords[0,indices],mcoords[1,indices],mCoords[2,indices]]

; Convert moving image to distance image if necessary
if keyword_set( dtFile ) ne 0 then begin
	dImg = fltarr( fImgDims )
	openr, lun, /GET_LUN, dtFile
	readu, lun, dImg
	free_lun, lun
	writeFile = ''
endif else begin
	if keyword_set( chamfer ) then begin
		dImg = dt( fContMask, /CHAMFER )
	endif else begin
		dImg = dt( fContMask, /CITY )
	endelse
endelse

; Estimate z offset for initial simplex
zShift = align_vert( pFixedImg, pMovingImg )
yShift = align_y( pFixedImg, pMovingImg )

if bDisplay then begin
	device, DECOMPOSED=0
	loadct, 0, /SILENT
	topClr = !D.TABLE_SIZE-1
	tvlct, 255, 0, 0, topClr	; red
	tvlct, 0, 255, 0, topClr-1	; green
	dispImg = bytscl( *pFixedImg, TOP=topClr-2 )
	dispDims = fImgDims*fPixDims
	window, /FREE, TITLE='Chamfer matching...', $
		XPOS=0, YPOS=0, $
		XSIZE=dispDims[0]*2, YSIZE=dispDims[1]
endif

; Optimize cost
if keyword_set( simplex ) then begin

	; Downhill simplex
	retVal = amoeba( 1.0e-6, FUNCTION_NAME='cost', P0=[0,0,0,0,yShift,zShift], SCALE=maxVals, $
			NCALLS=nCalls, FUNCTION_VALUE=values )

	if n_elements( retVal ) eq 1 then begin
		void = dialog_message( /ERROR, 'amoeba failed to converge' )
		return, -1L
	endif

	if bDebug then begin
		print, 'rot (deg): ', strtrim( retVal[0:2] )
		print, 'shift (pix): ', strtrim( retVal[3:5] )
		print, 'nCalls: ', strtrim( nCalls )
		print, 'cost: ', strtrim( values[0], 2 )
	endif

	; Restart
	p = amoeba( 1.0e-6, FUNCTION_NAME='cost', P0=retVal, SCALE=maxVals, $
			NCALLS=nCalls, FUNCTION_VALUE=values )

	if n_elements( p ) eq 1 then begin
		void = dialog_message( /ERROR, 'amoeba failed to converge' )
		return, -1L
	endif

	if bDebug then begin
		print, 'rot (deg): ', strtrim( p[0:2] )
		print, 'shift (pix): ', strtrim( p[3:5] )
		print, 'nCalls: ', strtrim( nCalls )
		print, 'cost: ', strtrim( values[0], 2 )
	endif

endif else begin

	; Powell
	p = [0,0,0,0,0,0]
	xi = identity(6)
	powell, p, xi, 1.0e-8, fmin, 'cost', /DOUBLE
	if bDebug then print, p

endelse

mContMask = transform_volume( mContMask*100, PIXSIZE=mPixDims, ROT=p[0:2], TRANS=p[3:5], MISSING=0 )
retImg = transform_volume( (*pMovingImg), PIXSIZE=mPixDims, ROT=p[0:2], TRANS=p[3:5], MISSING=0 )

if bDisplay then begin
	indices = where( mContMask ne 0, count )
	dispImg = (*pFixedImg)
	maxVal = max(*pFixedImg) + 1
	dispImg[indices] = maxVal
	tv3d, dispImg, RED=maxVal, TITLE='Transformed moving contours'
endif

if keyword_set( writeFile ) then begin
	file = file_search( writeFile, COUNT=nFound )
	if nFound ne 0 then begin
		; File exists. Prompt to overwrite
		writeFile = dialog_pickfile( TITLE='Select file to write distance transform to', $
				 /OVERWRITE, FILTER=fileName, DEFAULT='img', GET_PATH=path  )
	endif
	if writeFile ne '' then begin
		openw, lun, /GET_LUN, writeFile
		writeu, lun, dImg
		free_lun, lun
		fileBase = (strsplit( file_basename( writeFile ), '.', /EXTRACT ))[0]
		filePath = file_dirname( writeFile, /MARK )
		txtFile = filePath + fileBase + '_info.txt'
		openw, lun, /GET_LUN, txtFile
		printf, lun, "Image Info"
		printf, lun, "=========="
		printf, lun, "nPx = ", strtrim( fImgDims[0], 2 )
		printf, lun, "nPy = ", strtrim( fImgDims[1], 2 )
		printf, lun, "nSlices = ", strtrim( fImgDims[2], 2 )
		printf, lun, "pixSizeX (mm) = ", strtrim( fPixDims[0], 2 )
		printf, lun, "pixSizeY (mm) = ", strtrim( fPixDims[1], 2 )
		printf, lun, "pixSizeZ (mm) = ", strtrim( fPixDims[2], 2 )
		printf, lun, "datatype = ", strtrim( size( dImg, /TNAME ) )
		free_lun, lun
	endif
endif

beep
if bDebug then print, 'Done'
return, retImg

end



function cost, p

common cost_params, dImg, mCoords, maxVals, bDisplay, dispDims, dispImg, $
		fContMask, zRange, mPixDims

dims = size( dImg, /DIM )
t3d, /RESET
t3d, TRANS=-(dims-1)/2.0
t3d, SCALE=mPixDims
t3d, ROT=p[0:2]
t3d, SCALE=1.0/mPixDims
t3d, TRANS=(dims-1)/2.0+p[3:5]
coords = vert_t3d( mCoords )

; Use mean instead of RMS
f = interpolate( dImg, coords[0,*], coords[1,*], coords[2,*], MISSING=-1 )

if bDisplay then begin
	tImg = bytscl( dispImg[*,*,dims[2]/2], TOP=253b )
	sImg = bytscl( reform(dispImg[dims[0]/2,*,*]), TOP=253b )
	indices = where( fContMask[*,*,dims[2]/2] ne 0, count )
	if count ne 0 then tImg[indices] = 255b
	indices = where( fContMask[dims[0]/2,*,*] ne 0, count )
	if count ne 0 then sImg[indices] = 255b
	indices = where( round(coords[2,*]) eq dims[2]/2, count )
;	indices = where( (reform( coords[2,*] ) gt dims[2]/2.0-0.5) $
;			and (reform( coords[2,*] ) lt dims[2]/2.0+0.5), count )
	if count ne 0 then tImg[coords[0,indices],coords[1,indices]] = 254b
	indices = where( round(coords[0,*] ) eq dims[0]/2, count )
;	indices = where( (reform( coords[0,*] ) gt dims[0]/2.0-0.5) $
;			and (reform( coords[0,*] ) lt dims[0]/2.0+0.5), count )
	if count ne 0 then sImg[coords[1,indices],coords[2,indices]] = 254b
	tv, congrid( tImg, dispDims[0], dispDims[1] ), 0, 0
	tv, congrid( sImg, dispDims[1], dispDims[2] ), dispDims[0], 0
	xyouts, 1, 1, strjoin( strtrim( string(p, FORMAT='(f6.2)') ), ', ' ), /DEVICE

endif

valid = where( f ne -1, nValid )
if nValid gt 0 then begin
	c = double(total(f[valid], /DOUBLE)) / nValid
endif else begin
	; Penalize if we're outside the image boundaries
	c = 100d
endelse

;c = sqrt( total(f^2, /DOUBLE) ) / (n_elements(coords[0,*])-1)

; Constrain parameters by increasing cost if params are out of bounds
for i=0, n_elements(p)-1 do begin
	if p[i] lt -maxVals[i] then begin
		c = c + 100*(maxVals[i]-p[i]);/maxVals[i]
	endif else if p[i] gt maxVals[i] then begin
		c = c + 100*(p[i]-maxVals[i]);/maxVals[i]
	endif
endfor

return, c

end ; of cost
