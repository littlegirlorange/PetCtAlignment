
;==============================================================================
;
;	Method:		coreg2PetCtRois
;
;	Description:
;				Aligns 2 sets of raw PET/CT/ROI data.
;				Assumes the PETs and CTs are all in the same data space
;				(same dimensions and pixel sizes).
;
;	Version:
;				070703
;				Aligns the data based on the last frame of each PET dataset.
;
;				070704
;				Aligns the data based on the CTs.
;
;				071111
;				Changed alignment from ITK's mutual information routine to
;				a custom chamfer matching algorithm.
;
;				071123
;				Version used to generate data given to Huan 071116.
;
;				080306
;				Switched from transform_volume to apply_transform which
;				accounts for anisotropic voxels.
;
;				080307
;				Switched back to transform_volume.  It's okay to have
;				anisotropic voxels as long as you consistently work in
;				voxels, not mm.
;
;				080813
;				Added 3D range selection for chamfer matching (instead of just z).
;
;				090727
;				Back to accounting for anisotropic voxels in transform_volume.
;
;	Inputs:
;				2 sets of raw PET/CT/ROI data
;
;	Outputs:
;				The second PET/CT is overwritten with aligned values
;
;	Required Modules:
;				textbox.pro
;				chamfer_match.pro
;
;==============================================================================

@'.\chamfer_match.pro'

function coreg2PetCtRois, $
	CT1			= pCT1, $
	PET1		= pPET1, $
	PETFILE1	= petFile1, $
	PET_FRAMES1	= petFrames1, $
	ROI1		= pROI1, $
	ROIFILE1	= roiFile1, $
	ROI_COUNT1	= nROIs1, $
	PIXDIMS1	= pixDims1, $
	CT2			= pCT2, $
	PET2		= pPET2, $
	PETFILE2	= petFile2, $
	PET_FRAMES2	= petFrames2, $
	ROI2		= pROI2, $
	ROIFILE2	= roiFile2, $
	ROI_COUNT2	= nROIs2, $
	PIXDIMS2 	= pixDims2, $
	RANGE		= range, $
	DT_FILE		= dtFile, $
	WRITE_PATH	= writePath, $
	ROT			= rot, $
	TRANS		= trans, $
	DISPLAY		= bDisplay

bOk = 0b

dims1 = size( *pCT1, /DIM )
dims2 = size( *pCT2, /DIM )
type = size( *pCT1, /TYPE )
if ( n_elements( dims1 ) ne 3 ) or ( n_elements( dims2 ) ne 3 ) then begin
	void = dialog_message( /ERROR, $
			'Data must be 3D. Returning.' )
	return, bOk
endif

if total( dims1 eq dims2 ) ne 3 then begin
	void = dialog_message( /ERROR, $
			'Data dimensions do not match. Returning.' )
	return, bOk
endif

if total( pixDims1 eq pixDims2 ) ne 3 then begin
	void = dialog_message( /ERROR, $
			'Data dimensions do not match. Returning.' )
	return, bOk
endif

if not keyword_set( bDisplay ) then bDisplay = 0b

if n_elements( petFile1 ) eq 0 then begin
	dims = size( *pPET1, /DIM )
	if n_elements( dims ) eq 4 then begin
		nFrames1 = dims[3]
	endif else begin
		nFrames1 = 1
	endelse
endif else begin
	nFrames1 = petFrames1
	openu, petLun1, /GET_LUN, petFile1
	pet1 = assoc( petLun1, intarr( dims1 ) )
endelse
if n_elements( petFile2 ) eq 0 then begin
	dims = size( *pPET2, /DIM )
	if n_elements( dims ) eq 4 then begin
		nFrames2 = dims[3]
	endif else begin
		nFrames2 = 1
	endelse
endif else begin
	nFrames2 = petFrames2
	openu, petLun2, /GET_LUN, petFile2
	pet2 = assoc( petLun2, intarr( dims2 ) )
endelse
if nFrames1 ne nFrames2 then begin
	void = dialog_message( /ERROR, $
			'Number of PET frames do not match. Returning.' )
	return, bOk
endif

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; ;Uncomment the following lines if you want to use dicom ROIs as well
;
;if n_elements( roiFile2 ) ne 0 then begin
;	openu, roiLun2, /GET_LUN, roiFile2
;	rois2 = assoc( roiLun2, bytarr( dims2 ) )
;endif
; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<,,<

if n_elements( range ) eq 0 then range = [0, dims1[2]-1]

if bDisplay then begin

THRESH = 0.3

; Set up display
device, DECOMPOSED=0
loadct, 0, /SILENT
topClr = !D.TABLE_SIZE-1
tvlct, 255, 0, 0, topClr	; red
tvlct, 0, 255, 0, topClr-1	; green

dispDims = dims1*pixDims1/2

; Align CTs

; Display orthogonal views before alignment
xSlice = fix(dims1[0]/2.0)
ySlice = fix(dims1[1]/2.0)
zSlice = fix(dims1[2]/2.0)

regions1a = *pCT1 gt mean( *pCT1 )
regions1b = *pCT1 gt THRESH*max( *pCT1 )
regions2a = *pCT2 gt mean( *pCT2 )
regions2b = *pCT2 gt THRESH*max( *pCT2 )

window, /FREE, XSIZE=dispDims[0]*3, YSIZE=dispDims[1]*3, $
		TITLE='CT'
wId = !D.window

; Fixed images
maxVal = max( *pCT1 )
dispImg = bytscl( congrid( reform((*pCT1)[xSlice,*,*]), $
		dispDims[1], dispDims[2], /INTERP ), MAX=maxVal )
indices1ax = contour_mask2( congrid( reform(regions1a[xSlice,*,*]), $
		dispDims[1], dispDims[2] ) )
if indices1ax[0] ne -1L then dispImg[indices1ax] = topClr
indices1bx = contour_mask2( congrid( reform(regions1b[xSlice,*,*]), $
		dispDims[1], dispDims[2] ) )
if indices1bx[0] ne -1L then dispImg[indices1bx] = topClr
wset, wId & tv, dispImg, 0, dispDims[1]*2
dispImg = bytscl( congrid( reform((*pCT1)[*,ySlice,*]), $
		dispDims[0], dispDims[2], /INTERP ), MAX=maxVal )
indices1ay = contour_mask2( congrid( reform(regions1a[*,ySlice,*]), $
		dispDims[0], dispDims[2] ) )
if indices1ay[0] ne -1L then dispImg[indices1ay] = topClr
indices1by = contour_mask2( congrid( reform(regions1b[*,ySlice,*]), $
		dispDims[0], dispDims[2] ) )
if indices1by[0] ne -1L then dispImg[indices1by] = topClr
wset, wId & tv, dispImg, dispDims[0], dispDims[1]*2
dispImg = bytscl( congrid( reform((*pCT1)[*,*,zSlice]), $
		dispDims[0], dispDims[1], /INTERP ), MAX=maxVal )
indices1az = contour_mask2( congrid( reform(regions1a[*,*,zSlice]), $
		dispDims[0], dispDims[1] ) )
if indices1az[0] ne -1L then dispImg[indices1az] = topClr
indices1bz = contour_mask2( congrid( reform(regions1b[*,*,zSlice]), $
		dispDims[0], dispDims[1] ) )
if indices1bz[0] ne -1L then dispImg[indices1bz] = topClr
wset, wId & tv, dispImg, dispDims[0]*2, dispDims[1]*2
xyouts, 2, dispDims[1]*3-2, 'Fixed'

; Moving images
maxVal = max( *pCT2 )
dispImg = bytscl( congrid( reform((*pCT2)[xSlice,*,*]), $
		dispDims[1], dispDims[2], /INTERP ), MAX=maxVal )
indices2ax = contour_mask2( congrid( reform(regions2a[xSlice,*,*]), $
		dispDims[1], dispDims[2] ) )
if indices1ax[0] ne -1L then dispImg[indices1ax] = topClr
if indices2ax[0] ne -1L then dispImg[indices2ax] = topClr-1
indices2bx = contour_mask2( congrid( reform(regions2b[xSlice,*,*]), $
		dispDims[1], dispDims[2] ) )
if indices1bx[0] ne -1L then dispImg[indices1bx] = topClr
if indices2bx[0] ne -1L then dispImg[indices2bx] = topClr-1
wset, wId & tv, dispImg, 0, dispDims[1]
dispImg = bytscl( congrid( reform((*pCT2)[*,ySlice,*]), $
		dispDims[0], dispDims[2], /INTERP ), MAX=maxVal )
indices2ay = contour_mask2( congrid( reform(regions2a[*,ySlice,*]), $
		dispDims[0], dispDims[2] ) )
if indices1ay[0] ne -1L then dispImg[indices1ay] = topClr
if indices2ay[0] ne -1L then dispImg[indices2ay] = topClr-1
indices2by = contour_mask2( congrid( reform(regions2b[*,ySlice,*]), $
		dispDims[0], dispDims[2] ) )
if indices1by[0] ne -1L then dispImg[indices1by] = topClr
if indices2by[0] ne -1L then dispImg[indices2by] = topClr-1
wset, wId & tv, dispImg, dispDims[0], dispDims[1]
dispImg = bytscl( congrid( reform((*pCT2)[*,*,zSlice]), $
		dispDims[0], dispDims[1], /INTERP ), MAX=maxVal )
indices2az = contour_mask2( congrid( reform(regions2a[*,*,zSlice]), $
		dispDims[0], dispDims[1] ) )
if indices1az[0] ne -1L then dispImg[indices1az] = topClr
if indices2az[0] ne -1L then dispImg[indices2az] = topClr-1
indices2bz = contour_mask2( congrid( reform(regions2b[*,*,zSlice]), $
		dispDims[0], dispDims[1] ) )
if indices1bz[0] ne -1L then dispImg[indices1bz] = topClr
if indices2bz[0] ne -1L then dispImg[indices2bz] = topClr-1
wset, wId & tv, dispImg, dispDims[0]*2, dispDims[1]
xyouts, 2, dispDims[1]*2-2, 'Moving'

endif ; bDisplay

; Chamfer match the images.
; Allow up to 5 cm initial displacement in any direction
transLims = 50.0 / pixDims1
if not keyword_set( dtFile ) then dtFile = ''
if not keyword_set( writePath ) eq 0 then writePath = ''
ct2 = chamfer_match( IMG1=pCT1, PIXDIMS1=pixDims1, $
		IMG2=pCT2, PIXDIMS2=pixDims2, $
		LIMITS=[10,10,10,transLims[0],transLims[1],transLims[2]+6], RANGE=range, $
		/SIMPLEX, /CHAMFER, /DEBUG, TRANS_VALS=p, /DISPLAY, $
		WRITE_FILE=writePath, DT_FILE=dtFile )

if n_elements( ct2 ) gt 1 then *pCT2 = ct2
ct2 = 0
beep
rot = p[0:2]
trans = p[3:5]

if bDisplay then begin

; Display aligned images
regions2a = *pCT2 gt mean( *pCT2 )
regions2b = *pCT2 gt THRESH*max( *pCT2 )

; Aligned
maxVal = max( *pCT2 )
dispImg = bytscl( congrid( reform((*pCT2)[xSlice,*,*]), $
		dispDims[1], dispDims[2], /INTERP ), MAX=maxVal )
indices2ax = contour_mask2( congrid( reform(regions2a[xSlice,*,*]), $
		dispDims[1], dispDims[2] ) )
if indices1ax[0] ne -1L then dispImg[indices1ax] = topClr
if indices2ax[0] ne -1L then dispImg[indices2ax] = topClr-1
indices2bx = contour_mask2( congrid( reform(regions2b[xSlice,*,*]), $
		dispDims[1], dispDims[2] ) )
if indices1bx[0] ne -1L then dispImg[indices1bx] = topClr
if indices2bx[0] ne -1L then dispImg[indices2bx] = topClr-1
wset, wId & tv, dispImg, 0, 0
dispImg = bytscl( congrid( reform((*pCT2)[*,ySlice,*]), $
		dispDims[0], dispDims[2], /INTERP ), MAX=maxVal )
indices2ay = contour_mask2( congrid( reform(regions2a[*,ySlice,*]), $
		dispDims[0], dispDims[2] ) )
if indices1ay[0] ne -1L then dispImg[indices1ay] = topClr
if indices2ay[0] ne -1L then dispImg[indices2ay] = topClr-1
indices2by = contour_mask2( congrid( reform(regions2b[*,ySlice,*]), $
		dispDims[0], dispDims[2] ) )
if indices1by[0] ne -1L then dispImg[indices1by] = topClr
if indices2by[0] ne -1L then dispImg[indices2by] = topClr-1
wset, wId & tv, dispImg, dispDims[0], 0
dispImg = bytscl( congrid( reform((*pCT2)[*,*,zSlice]), $
		dispDims[0], dispDims[1], /INTERP ), MAX=maxVal )
indices2az = contour_mask2( congrid( reform(regions2a[*,*,zSlice]), $
		dispDims[0], dispDims[1] ) )
if indices1az[0] ne -1L then dispImg[indices1az] = topClr
if indices2az[0] ne -1L then dispImg[indices2az] = topClr-1
indices2bz = contour_mask2( congrid( reform(regions2b[*,*,zSlice]), $
		dispDims[0], dispDims[1] ) )
if indices1bz[0] ne -1L then dispImg[indices1bz] = topClr
if indices2bz[0] ne -1L then dispImg[indices2bz] = topClr-1
wset, wId & tv, dispImg, dispDims[0]*2, 0
xyouts, 2, dispDims[1]-2, 'Moving - aligned'

endif ; bDisplay

; Align PETs
for iFrame=0, nFrames1-1 do begin

	; Read frames
	if n_elements( petFile1 ) eq 0 then begin
		img1 = (*pPET1)[*,*,*,iFrame]
	endif else begin
		img1 = pet1[iFrame]
	endelse
	if n_elements( petFile2 ) eq 0 then begin
		img2 = (*pPET2)[*,*,*,iFrame]
	endif else begin
		img2 = pet2[iFrame]
	endelse

	if bDisplay then begin

	; Display orthogonal views before alignment
	xSlice = fix(dims1[0]/2.0)
	ySlice = fix(dims1[1]/2.0)
	zSlice = fix(dims1[2]/2.0)

	regions1a = img1 gt mean( img1 )
	regions1b = img1 gt THRESH*max( img1 )
	regions2a = img2 gt mean( img2 )
	regions2b = img2 gt THRESH*max( img2 )

	window, /FREE, XSIZE=dispDims[0]*3, YSIZE=dispDims[1]*3, $
			TITLE='PET frame '+strtrim(iFrame,2)
	wId = !D.window

	; Fixed
	maxVal = max( img1 )
	dispImg = bytscl( congrid( reform(img1[xSlice,*,*]), $
			dispDims[1], dispDims[2], /INTERP ), MAX=maxVal )
	indices1ax = contour_mask2( congrid( reform(regions1a[xSlice,*,*]), $
			dispDims[1], dispDims[2] ) )
	if indices1ax[0] ne -1L then dispImg[indices1ax] = topClr
	indices1bx = contour_mask2( congrid( reform(regions1b[xSlice,*,*]), $
			dispDims[1], dispDims[2] ) )
	if indices1bx[0] ne -1L then dispImg[indices1bx] = topClr
	wset, wId & tv, dispImg, 0, dispDims[1]*2
	dispImg = bytscl( congrid( reform(img1[*,ySlice,*]), $
			dispDims[0], dispDims[2], /INTERP ), MAX=maxVal )
	indices1ay = contour_mask2( congrid( reform(regions1a[*,ySlice,*]), $
			dispDims[0], dispDims[2] ) )
	if indices1ay[0] ne -1L then dispImg[indices1ay] = topClr
	indices1by = contour_mask2( congrid( reform(regions1b[*,ySlice,*]), $
			dispDims[0], dispDims[2] ) )
	if indices1by[0] ne -1L then dispImg[indices1by] = topClr
	wset, wId & tv, dispImg, dispDims[0], dispDims[1]*2
	dispImg = bytscl( congrid( reform(img1[*,*,zSlice]), $
			dispDims[0], dispDims[1], /INTERP ), MAX=maxVal )
	indices1az = contour_mask2( congrid( reform(regions1a[*,*,zSlice]), $
			dispDims[0], dispDims[1] ) )
	if indices1az[0] ne -1L then dispImg[indices1az] = topClr
	indices1bz = contour_mask2( congrid( reform(regions1b[*,*,zSlice]), $
			dispDims[0], dispDims[1] ) )
	if indices1bz[0] ne -1L then dispImg[indices1bz] = topClr
	wset, wId & tv, dispImg, dispDims[0]*2, dispDims[1]*2
	xyouts, 2, dispDims[1]*3-2, 'Fixed', COLOR=255b

	; Moving
	maxVal = max( img2 )
	dispImg = bytscl( congrid( reform(img2[xSlice,*,*]), $
			dispDims[1], dispDims[2], /INTERP ), MAX=maxVal )
	indices2ax = contour_mask2( congrid( reform(regions2a[xSlice,*,*]), $
			dispDims[1], dispDims[2] ) )
	if indices1ax[0] ne -1L then dispImg[indices1ax] = topClr
	if indices2ax[0] ne -1L then dispImg[indices2ax] = topClr-1
	indices2bx = contour_mask2( congrid( reform(regions2b[xSlice,*,*]), $
			dispDims[1], dispDims[2] ) )
	if indices1bx[0] ne -1L then dispImg[indices1bx] = topClr
	if indices2bx[0] ne -1L then dispImg[indices2bx] = topClr-1
	wset, wId & tv, dispImg, 0, dispDims[1]
	dispImg = bytscl( congrid( reform(img2[*,ySlice,*]), $
			dispDims[0], dispDims[2], /INTERP ), MAX=maxVal )
	indices2ay = contour_mask2( congrid( reform(regions2a[*,ySlice,*]), $
			dispDims[0], dispDims[2] ) )
	if indices1ay[0] ne -1L then dispImg[indices1ay] = topClr
	if indices2ay[0] ne -1L then dispImg[indices2ay] = topClr-1
	indices2by = contour_mask2( congrid( reform(regions2b[*,ySlice,*]), $
			dispDims[0], dispDims[2] ) )
	if indices1by[0] ne -1L then dispImg[indices1by] = topClr
	if indices2by[0] ne -1L then dispImg[indices2by] = topClr-1
	wset, wId & tv, dispImg, dispDims[0], dispDims[1]
	dispImg = bytscl( congrid( reform(img2[*,*,zSlice]), $
			dispDims[0], dispDims[1], /INTERP ), MAX=maxVal )
	indices2az = contour_mask2( congrid( reform(regions2a[*,*,zSlice]), $
			dispDims[0], dispDims[1] ) )
	if indices1az[0] ne -1L then dispImg[indices1az] = topClr
	if indices2az[0] ne -1L then dispImg[indices2az] = topClr-1
	indices2bz = contour_mask2( congrid( reform(regions2b[*,*,zSlice]), $
			dispDims[0], dispDims[1] ) )
	if indices1bz[0] ne -1L then dispImg[indices1bz] = topClr
	if indices2bz[0] ne -1L then dispImg[indices2bz] = topClr-1
	wset, wId & tv, dispImg, dispDims[0]*2, dispDims[1]
	xyouts, 2, dispDims[1]*2-2, 'Moving', COLOR=255b

	endif ; bDisplay

	; Align
	img2 = transform_volume( img2, PIXSIZE=pixDims1, ROT=p[0:2], TRANS=p[3:5] )

	; Write aligned moving image
	if n_elements( img2 ) gt 1 then begin
		if n_elements( petFile2 ) eq 0 then begin
			(*pPET2)[0,0,0,iFrame] = img2
		endif else begin
			pet2[iFrame] = img2
		endelse
	endif

	if bDisplay then begin

	; Display aligned images
	regions2a = img2 gt mean( img2 )
	regions2b = img2 gt THRESH*max( img2 )

	; Aligned
	maxVal = max( img2 )
	dispImg = bytscl( congrid( reform(img2[xSlice,*,*]), $
			dispDims[1], dispDims[2], /INTERP ), MAX=maxVal )
	indices2ax = contour_mask2( congrid( reform(regions2a[xSlice,*,*]), $
			dispDims[1], dispDims[2] ) )
	if indices1ax[0] ne -1L then dispImg[indices1ax] = topClr
	if indices2ax[0] ne -1L then dispImg[indices2ax] = topClr-1
	indices2bx = contour_mask2( congrid( reform(regions2b[xSlice,*,*]), $
			dispDims[1], dispDims[2] ) )
	if indices1bx[0] ne -1L then dispImg[indices1bx] = topClr
	if indices2bx[0] ne -1L then dispImg[indices2bx] = topClr-1
	wset, wId & tv, dispImg, 0, 0
	dispImg = bytscl( congrid( reform(img2[*,ySlice,*]), $
			dispDims[0], dispDims[2], /INTERP ), MAX=maxVal )
	indices2ay = contour_mask2( congrid( reform(regions2a[*,ySlice,*]), $
			dispDims[0], dispDims[2] ) )
	if indices1ay[0] ne -1L then dispImg[indices1ay] = topClr
	if indices2ay[0] ne -1L then dispImg[indices2ay] = topClr-1
	indices2by = contour_mask2( congrid( reform(regions2b[*,ySlice,*]), $
			dispDims[0], dispDims[2] ) )
	if indices1by[0] ne -1L then dispImg[indices1by] = topClr
	if indices2by[0] ne -1L then dispImg[indices2by] = topClr-1
	wset, wId & tv, dispImg, dispDims[0], 0
	dispImg = bytscl( congrid( reform(img2[*,*,zSlice]), $
			dispDims[0], dispDims[1], /INTERP ), MAX=maxVal )
	indices2az = contour_mask2( congrid( reform(regions2a[*,*,zSlice]), $
			dispDims[0], dispDims[1] ) )
	if indices1az[0] ne -1L then dispImg[indices1az] = topClr
	if indices2az[0] ne -1L then dispImg[indices2az] = topClr-1
	indices2bz = contour_mask2( congrid( reform(regions2b[*,*,zSlice]), $
			dispDims[0], dispDims[1] ) )
	if indices1bz[0] ne -1L then dispImg[indices1bz] = topClr
	if indices2bz[0] ne -1L then dispImg[indices2bz] = topClr-1
	wset, wId & tv, dispImg, dispDims[0]*2, 0
	xyouts, 2, dispDims[1]-2, 'Moving - aligned', COLOR=255b

	endif ; bDisplay

	beep

endfor

; Align ROIs
for iROI=0, nROIs2-1 do begin
	img = rois2[iROI]
	img = transform_volume( bytscl(img), PIXSIZE=pixDims1, ROT=p[0:2], TRANS=p[3:5] )
	rois2[iROI] = img gt 128b
endfor

; Clean up
if obj_valid( obj ) then obj_destroy, obj

if bDisplay then begin
	close, 1
	close, 2
	close, 3
endif

close, /ALL

print, 'Done'
beep

end