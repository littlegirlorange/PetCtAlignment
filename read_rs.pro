;==============================================================================
;
;	Method:	read_rs
;
;	Description:
;
;			Converts contours in DICOM RS structure set format to raw data
;			arrays of the same dimensions as the original DICOM image data.
;
;	Version:
;			080122 - Cleaned up code for Huan.
;
;			071101 - First working version.
;
;	Syntax:
;			read_rs
;
;	Return Value:
;			none
;
;	Arguments:
;			RSFILE			- [in, optional]
;							  Path to DICOM RS structure set to convert. If not
;							  given, the program will prompt.
;
;			PHILIPS_CTFILE	- [in, optional]
;							  Path to CT data on which contours were originally
;							  drawn in Philips native format.  Specify either
;							  PHILIPS_CTFILE or DICOM_CTFILE but not both.
;							  If not given, the program will prompt.
;
;			DICOM_CTFILE	- [in, optional]
;							  Path to CT data on which contours were originally
;							  drawn in DICOM format.  Specify either
;							  PHILIPS_CTFILE or DICOM_CTFILE but not both.
;							  If not given, the program will prompt.
;
;			WRITE_CT		- [in, optional]
;							  If set, writes the raw CT data array.  Set by
;							  default.
;
;			DISPLAY			- [in, optional]
;							  If set, displays the images and overlayed
;							  contours.  Set by default.
;
;	Required Modules:
;			allo_volume_read.pro
;			contour_mask.pro
;			idl_allolist_struct.pro
;			idl_mhdr_struct.pro
;			idl_subhdr_struct.pro
;			select_contours.pro
;
;	Written By:
;			Maggie Kusano, January 22, 2008
;
;==============================================================================

function read_rs, $
RSFILE			= rsFile, $
PHILIPS_CTFILE	= philipsCTFile, $
DICOM_CTFILE	= dicomCTFile, $
CT_DATA			= pCT, $
ALL_ROIS		= allROIs, $
ROI_NAMES		= roiNames, $
ROI_COUNT		= nConts, $
ROI_DATA		= pROIs, $
ROI_ASSOC_FILE	= roiAssocFile, $
PATNAME			= patName, $
STUDYDATE		= studyDate, $
DISPLAY			= bDisplay, $
WRITE			= bWrite

; Set up display (optional)
if keyword_set( bDisplay ) then begin
	device, DECOMPOSED=0
	loadct, 0, /SILENT ; Grayscale
	topClr = !D.TABLE_SIZE-1

	; Set the top colors to red and green for ROI display
	tvlct, 255, 0, 0, topClr	; red
	tvlct, 0, 255, 0, topClr-1	; green

	scrnSize = get_screen_size()
	maxWinSize = scrnSize-18
	nDispCols = 6
	nRealCols = nDispCols
endif

; Prompt for RS file if not given
if n_elements( rsFile ) eq 0 then begin
    rsFile = dialog_pickfile( TITLE='Select RS file', $
    		FILTER='RS*.dcm', GET_PATH=path, /MUST_EXIST )
    if rsFile eq '' then begin
       blah = dialog_message( /ERROR, 'No RS file selected. Returning.' )
       return, 0b
    endif
endif else begin
	path=file_dirname( rsFile, /MARK_DIRECTORY )
endelse

; Prompt for CT file if not given
if n_elements( philipsCTFile ) ne 0 then begin
	ctFile = philipsCTFile
	ctType = 'raw'
endif else if n_elements( dicomCTFile ) ne 0 then begin
	ctFile = dicomCTFile
	ctType = 'dcm'
endif else begin
	ctFile = dialog_pickfile( TITLE='Select CT file', $
			FILTER=['CT*.dcm', '*.syn'], PATH=path, /MUST_EXIST )
	if ctFile eq '' then begin
		blah = dialog_message( /ERROR, 'No CT file selected. Returning.' )
		return, 0b
	endif
	fileParts = strsplit( ctFile, '.', /EXTRACT )
	fileExt = fileParts[n_elements(fileParts)-1]
	if strcmp( fileExt, 'syn', /FOLD_CASE ) then begin
		ctType = 'raw'
	endif else if strcmp( fileExt, 'dcm', /FOLD_CASE ) then begin
		ctType = 'dcm'
	endif else begin
		blah = dialog_message( /ERROR, 'Invalid CT type. File extension must be dcm or syn. Returning.' )
		return, 0b
	endelse
endelse

; Get CT img data and header info
print, 'Reading CT image information'
if ctType eq 'raw' then begin

	ctArray = allo_volume_read( ctFile, $
			MAIN_HEADER=mhdr, SUB_HEADER=shdrs )

	imgDims = size( ctArray, /DIM )
	pixDims = fltarr(3)
	pixDims[0] = shdrs[0].pix_spacing[0]
	pixDims[1] = shdrs[0].pix_spacing[1]
	zSpacings = abs( shdrs[0:(imgDims[2]-2)].img_posz - shdrs[1:(imgDims[2]-1)].img_posz )
	zRatios = abs(zSpacings-zSpacings[0])/zSpacings[0]
	indices = where( zRatios gt 0.0001, count )
	if count gt 0 then begin
		pixDims[2] = -1
	endif else begin
		pixDims[2] = mean( zSpacings )
	endelse
	imgPos = fltarr(3,imgDims[2])
	imgPos[0,*] = shdrs.img_posx
	imgPos[1,*] = shdrs.img_posy
	imgPos[2,*] = shdrs.img_posz

endif else if ctType eq 'dcm' then begin

	; Retrieve a list of all CT .dcm files in the selected directory
	; and determine the number of image slices
	ctFileBase = strsplit( file_basename( ctFile ), '.', /EXTRACT )
	ctFilePath = file_dirname( ctFile, /MARK )
	ctFiles = file_search( ctfilePath+ctFileBase[0]+'*.dcm', COUNT=count )
	if count le 1 then begin
		ctFiles = file_search( ctFilePath+ctFileBase[0]+'*.*.img', COUNT=count )
		if count le 1 then begin
			; Look for Slicer anonymized DICOM files
			ctFiles = file_search( ctFilePath+'image*.dcm', COUNT=count )
			if count le 1 then return, 0b
		endif
	endif

	nCTSlices = n_elements( ctFiles )

	bOk = query_dicom( ctFile, info )
	if not bOk then begin
		void = dialog_message( /ERROR, 'Error reading '+ctFile+'. Returning.' )
		return, 0b
	endif else begin
		imgDims = [info.dimensions[0], info.dimensions[1], nCTSlices]
		ctArray = make_array( imgDims, TYPE=info.pixel_type )
	endelse

	oDcmImage = obj_new( 'IDLffDICOM' )
	isOk = oDcmImage->read( ctFile )
	pixSpacingStr = *(oDcmImage->getValue( '0028'x, '0030'x, /NO_COPY ))[0]
	pixDims = [float( strsplit( pixSpacingStr, '\', /EXTRACT ) ),-1]
	firstSeriesNo = fix( *(oDcmImage->getValue( '0020'x, '0011'x, /NO_COPY ))[0] )

	imgPos = fltarr(3,nCTSlices)
	imgNos = intarr(nCTSlices)

	obj_destroy, oDcmImage

	for i=0, nCTSlices-1 do begin

		; Get the image position
		oDcmImage = obj_new( 'IDLffDICOM' )
		bOk = oDcmImage->read( ctFiles[i] )
		if not bOk then begin
			void = dialog_message( /ERROR, $
					'Error reading '+ctFiles[i]+'. Returning.' )
			obj_destroy, oDcmImage
			return, 0b
		endif

		imgPosStr = *(oDcmImage->getValue( '0020'x, '0032'x, /NO_COPY ))[0]
		imgPos[*,i] = strsplit( strcompress( imgPosStr, /REMOVE_ALL ), '\', /EXTRACT )

		; Get the image and series numbers
		imgNos[i] = *(oDcmImage->getValue( '0020'x, '0013'x, /NO_COPY ))[0]
		seriesNo = *(oDcmImage->getValue( '0020'x, '0011'x, /NO_COPY ))[0]

		if seriesNo ne firstSeriesNo then begin
			void = dialog_message( /ERROR, $
					'Multiple CT series detected in this folder. Returning.' )
			obj_destroy, oDcmImage
			return, 0b
		endif

		; Get the image
		ctArray[0,0,i] = *(oDcmImage->getValue( '7fe0'x, '0010'x, /NO_COPY ))[0]

		; Destroy this object
		obj_destroy, oDcmImage

	end

	; Sort the images according to their locations
;	order = sort( imgNos[*] )
	order = sort( imgPos[2,*] )
	imgNos = imgNos[order]
	ctFiles = ctFiles[order]
	ctArray = ctArray[*,*,order]
	imgPos = imgPos[*,order]

	zSpacings = abs( imgPos[2,0:(imgDims[2]-2)] - imgPos[2,1:(imgDims[2]-1)] )
	zRatios = abs(zSpacings-zSpacings[0])/zSpacings[0]
	indices = where( zRatios gt 0.0001, count )
	if count gt 0 then begin
		pixDims[2] = -1
		void = dialog_message( 'Non-uniform slice spacing' )
	endif else begin
		pixDims[2] = mean(zSpacings)
	endelse

endif

; Decode DICOM header
obj = obj_new( 'IDLffDICOM' )
bOk = obj->read( rsFile )

patName		= obj->getValue( '0010'x, '0010'x, /NO_COPY )
studyDate	= obj->getValue( '0008'x, '0020'x, /NO_COPY )
patName		= strtrim( strjoin( strsplit( *patName[0], '^', /EXTRACT), '_'), 2 )
patName		= strtrim( strjoin( strsplit( patName, /EXTRACT), '_'), 2 )
studyDate	= strtrim( *studyDate[0], 2 )

nameRefs = obj->getReference( '3006'x,'0026'x )
seqRefs = obj->getReference( '3006'x,'0040'x )
dataRefs = obj->getReference( '3006'x,'0050'x )
nNames = n_elements( nameRefs )
nSeqs = n_elements( seqRefs )

; If file contains more than one contour, prompt user for contours to save
if nNames eq 1 then allROIs=1b
contIndices = select_contours( rsFile, ALL_ROIS=allROIs )
nConts = n_elements( contIndices )

if nSeqs lt max(contIndices) then begin
	nameRefs = nameRefs[nNames-nSeqs:nNames-1]
	if n_elements( allROIs ) eq 0 then begin
		contIndices = contIndices-(nNames-nSeqs)
	endif else begin
		contIndices = contIndices[nNames-nSeqs:nNames-1]-(nNames-nSeqs)
	endelse
	nConts = n_elements( contIndices )
	print, 'Warning: Contour names and data do not match. Fudging...'
;    void = dialog_message( /ERROR, 'Number of contour sequences and contour names do not match. Returning.' )
;    obj_destroy, obj
;    return, 0b
endif

; Loop through selected ROIs
roiNames = strarr( nConts )

if n_elements( roiAssocFile ) ne 0 then begin
	openw, roiLun, /GET_LUN, roiAssocFile
	roiData = assoc( roiLun, bytarr( imgDims[0], imgDims[1], imgDims[2] ) )
endif else begin
	roiData = bytarr( imgDims[0], imgDims[1], imgDims[2], nConts )
endelse

for iCont=0, nConts-1 do begin

	print, 'Decoding contour ', strtrim(iCont+1,2), ' of ', strtrim(nConts,2)
	contArray = bytarr( imgDims )
	contIndex = contIndices[iCont]
	roi = bytarr( imgDims[0], imgDims[1], imgDims[2] )

	; Claim this ROI's spot in the ROI data array just in case we
	; crap out somewhere along the way
	if n_elements( roiAssocFile ) ne 0 then roiData[iCont] = roi

	; Get contour data references (one reference per slice)
	if contIndex lt n_elements(seqRefs)-1 then begin
		contDataRefs = dataRefs[where( (dataRefs gt seqRefs[contIndex]) and $
				(dataRefs lt seqRefs[contIndex+1]) )]
	endif else begin
		contDataRefs = dataRefs[where(dataRefs gt seqRefs[contIndex])]
	endelse

	bOk = 1b
	minSlice = imgDims[2]
	maxSlice = 0

	for iRef=0, n_elements(contDataRefs)-1 do begin

		; Get point coordinates (in pixels) from string values (in mm)
		pPtString = obj->getValue( REFERENCE=contDataRefs[iRef] )
		ptString = strcompress( *(pPtString)[0], /REMOVE_ALL )
		ptr_free, pPtString
		pts = strsplit( ptString, '\', /EXTRACT )
		nPts = fix( n_elements(pts)/3.0 )
		if nPts lt 3 then continue
		ptArrayInMM = fltarr(3,nPts)
		for iPt=0, nPts-1 do begin
			ptArrayInMM[0,iPt] = pts[iPt*3]
			ptArrayInMM[1,iPt] = pts[iPt*3+1]
			ptArrayInMM[2,iPt] = pts[iPt*3+2]
		endfor

		; Determine which slice this contour belongs to (to the nearest 1/100th of a mm)
		difference = round( abs( imgPos[2,*]-ptArrayInMM[2,0] )*100 )
		slice = (where( difference eq min( difference ), count ))[0]
		if count eq 0 then continue

		; Find max and min slices for display purposes
		if slice lt minSlice then minSlice = slice
		if slice gt maxSlice then maxSlice = slice

		; Convert from mm to pixels
		ptArray = fltarr(2,nPts)
		ptArray[0,*] = (ptArrayInMM[0,*] - imgPos[0,slice])/pixDims[0]
		ptArray[1,*] = (ptArrayInMM[1,*] - imgPos[1,slice])/pixDims[1]
		; Get rid of out-of-range points
		okIndices = where( (ptArray[0,*] lt imgDims[0]) $
				and (ptArray[1,*] lt imgDims[1]), count )
		if count gt 3 then begin
			ptArray = ptArray[*,okIndices]
		endif else begin
			continue
		endelse

		maskIndices = polyfillv( ptArray[0,*], ptArray[1,*], imgDims[0], imgDims[1] )
		if maskIndices[0] ne -1 then begin
			mask = roi[*,*,slice]
			mask[maskIndices] = 1b
			roi[0,0,slice] = mask
		endif

	endfor

	if n_elements( roiAssocFile ) eq 0 then begin
		roiData[*,*,*,iCont] = roi
	endif else begin
		roiData[iCont] = roi
	endelse

	pROIName = obj->getValue( REFERENCE=nameRefs[contIndex] )
	roiNames[iCont] = *(pROIName)[0]
	ptr_free, pROIName

	if keyword_set( bDisplay ) and (iCont eq 0) then begin

		firstSlice = (minSlice - 10) > 0
		lastSlice = (maxSlice + 10) < (imgDims[2]-1)
		firstBuffer = minSlice-firstSlice
		lastBuffer = lastSlice-maxSlice
		nImgs = lastSlice-firstSlice+1
		nRealRows = ceil( float(nImgs) / nDispCols )

		dispImgSizeX = maxWinSize[0] / nDispCols
		dispImgSizeY = fix( dispImgSizeX * float(imgDims[1])/imgDims[0] )
		realX = nRealCols * dispImgSizeX
		realY = nRealRows * dispImgSizeY

		wBase = widget_base( TITLE=strtrim(roiNames[iCont],2) )
		wDraw = widget_draw( wBase, $
							  XSIZE=realX, YSIZE=realY, $
							  /SCROLL, $
							  X_SCROLL_SIZE=maxWinSize[0], $
							  Y_SCROLL_SIZE=maxWinSize[1] )
		widget_control, wBase, /REALIZE

		; Scale images and ROIs to fit the display
		dispImgs = congrid( ctArray[*,*,firstSlice:lastSlice], $
				dispImgSizeX, dispImgSizeY, nImgs, /INTERP )
		dispRegion = congrid( roi[*,*,firstSlice:lastSlice], $
				dispImgSizeX, dispImgSizeY, nImgs, /INTERP )

		dispImgs = bytscl( dispImgs, TOP=topClr-2 )
		dispRegion = dispRegion gt 0

		; Draw
		for i=0, nImgs-1 do begin
			img = reform( dispImgs[*,*,i] )
			indices = contour_mask( reform(dispRegion[*,*,i]) )
			if n_elements(indices) gt 1 then img[indices] = topClr-1
			tv, img, i
			xLoc = (i mod nRealCols)*dispImgSizeX + 1
			yLoc = dispImgSizeY*nRealRows - (i/nRealCols)*dispImgSizeX - 10
			xyouts, xLoc, yLoc, $
					strtrim(firstSlice+i,2), $
					CHARSIZE=1, $
					COLOR=topClr, $
					/DEVICE
		endfor

	endif

endfor

if keyword_set( bWrite ) then begin

	path = file_dirname( rsFile, /MARK )
	folder = dialog_pickfile( TITLE='Select folder to write to', $
			/DIRECTORY, PATH=path, GET_PATH=writeDir )
	if folder eq '' then begin
		void = dialog_message( 'Data not written.' )
	endif

	; Get patID and study time
	parts = strsplit( file_dirname( rsFile ), "\", /EXTRACT, COUNT=nParts )
	study = parts[nParts-1]
	patID = parts[nParts-2]

	; Check for existing files
	fileName = writeDir + patID + '_' + study + '_CT.img'
	foundFiles = file_search( fileName, COUNT=nFiles )

	if nFiles gt 0 then begin
		bOk = dialog_message( /QUESTION, 'One or more files exist. Overwrite?' )
	endif else begin
		bOk = 'Yes'
	endelse

	if bOk eq 'No' then begin
		julian = systime( /JULIAN )
		caldat, julian, month, day, year, hour, minute, second
		suffix = '_' + string( year, month, day, hour, minute, second, $
				FORMAT='(I4, 5I2)' )
		suffix = strjoin( strsplit( suffix, ' ', /EXTRACT ), '0' )
	endif else begin
		suffix = ''
	endelse

	; CT
	fileName = writeDir + patID + '_' + study + suffix + '_CT.img'
	openw, fileOutUnit, fileName, /GET_LUN
	writeu, fileOutUnit, ctArray
	free_lun, fileOutUnit
	fileBase = (strsplit( file_basename( fileName ), ".", /EXTRACT ))[0]
	txtFileName = writeDir + fileBase + '_info.txt'
	openw, txtFileOutUnit, txtFileName, /GET_LUN
	printf, txtFileOutUnit, "Image Info"
	printf, txtFileOutUnit, "=========="
	printf, txtFileOutUnit, "nPx = ", strtrim( imgDims[0], 2 )
	printf, txtFileOutUnit, "nPy = ", strtrim( imgDims[1], 2 )
	printf, txtFileOutUnit, "nSlices = ", strtrim( imgDims[2], 2 )
	printf, txtFileOutUnit, "pixSizeX (mm) = ", strtrim( pixDims[0], 2 )
	printf, txtFileOutUnit, "pixSizeY (mm) = ", strtrim( pixDims[1], 2 )
	printf, txtFileOutUnit, "pixSizeZ (mm) = ", strtrim( pixDims[2], 2 )
	printf, txtFileOutUnit, "type = " + strtrim( size( ctArray, /TYPE ) )
	printf, txtFileOutUnit, "type name = " + strtrim( size( ctArray, /TNAME ) )
	free_lun, txtFileOutUnit

	; ROIs
	for iROI=0, nConts-1 do begin
		fileName = writeDir + patID + '_' + study + suffix + '_' + roiNames[iROI] + '.img'
		openw, fileOutUnit, fileName, /GET_LUN
		writeu, fileOutUnit, roiData[*,*,*,iROI]
		free_lun, fileOutUnit
		fileBase = (strsplit( file_basename( fileName ), ".", /EXTRACT ))[0]
		txtFileName = writeDir + fileBase + '_info.txt'
		openw, txtFileOutUnit, txtFileName, /GET_LUN
		printf, txtFileOutUnit, "Image Info"
		printf, txtFileOutUnit, "=========="
		printf, txtFileOutUnit, "nPx = ", strtrim( imgDims[0], 2 )
		printf, txtFileOutUnit, "nPy = ", strtrim( imgDims[1], 2 )
		printf, txtFileOutUnit, "nSlices = ", strtrim( imgDims[2], 2 )
		printf, txtFileOutUnit, "pixSizeX (mm) = ", strtrim( pixDims[0], 2 )
		printf, txtFileOutUnit, "pixSizeY (mm) = ", strtrim( pixDims[1], 2 )
		printf, txtFileOutUnit, "pixSizeZ (mm) = ", strtrim( pixDims[2], 2 )
		printf, txtFileOutUnit, "type = ", strtrim( size( roiData, /TYPE ) )
		printf, txtFileOutUnit, "type name = ", strtrim( size( roiData, /TNAME ) )
		free_lun, txtFileOutUnit
	endfor

endif ; bWrite

if n_elements( roiAssocFile ) eq 0 then begin
	pROIs = ptr_new( roiData, /NO_COPY )
endif else begin
	close, roiLun
	free_lun, roiLun
endelse

obj_destroy, obj

print, 'Done read_rs'
beep
return, 1b

end