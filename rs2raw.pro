;==============================================================================
;
;  Method:		rs2raw
;
;  Description:	Converts DICOM RT structure sets to raw data arrays.  Array
;				size is based on the size of the CT (Philips or DICOM format)
;				on which the contours were generated data size.
;
;  Version:		090722
;				First version - for Mike.
;
;  Syntax:		array = rs2raw( CT_FILE=ctFile, RS_FILE=rsFile $
;						[, ROIS=ptr][, ROI_NAMES=array] $
;						[, DIMS=vector][, PIXSIZE=vector] $
;						[, /DISPLAY][, /WRITE] )
;
;  Return Value:
;		Returns the raw data array or -1L if unsuccessful.
;
;  Inputs:		DICOM RT structure set and corresponding CT data (Philips or
;				DICOM format)
;
;  Outputs:		Aligned pre and post PET and CT data (ints) and corresponding
;				text files with image and pixel dimensions and frame times:
;					<pat_name>_pre_CT.img/.txt			(pre CT)
;					<pat_name>_pre_PET.img/.txt			(pre PET)
;					<pat_name>_pre_<roi_name>.img/txt	(pre roi)
;					<pat_name>_intra_CT.img/.txt		(intra CT)
;					<pat_name>_intra_PET.img/.txt		(intra PET)
;					<pat_name>_intra_<roi_name>.img/txt	(intra roi)
;
;  Required Modules:
;				allo_volume_read.pro
;				dcm2rawNonUniform.pro
;				gemini2raw.pro
;				idl_exthdr_struct.pro
;				idl_mhdr_struct.pro
;				idl_subhdr_struct.pro
;				read_rs.pro
;				textbox.pro
;
;  Written By:	Maggie Kusano, July 22, 2009
;
;==============================================================================

function rs2raw, $
	CT_FILE		= ctFile, $
	RS_FILE		= rsFile, $
	ROI_NAMES	= roiNames, $
	N_ROIS		= nROIs, $
	DIMS		= ctDims, $
	PIXSIZE		= ctPixSize, $
	DISPLAY		= bDisplay, $
	WRITE_ROIS	= bWrite, $
	WRITE_CT	= bWriteCT

if not keyword_set( bDisplay ) then bDisplay = 0b
if not keyword_set( bWrite ) then bWrite = 0b
if not keyword_set( bWriteCT ) then bWriteCT = 0b

if n_elements( ctFile ) eq 0 then begin
	ctFile = dialog_pickfile( GET_PATH=path, $
			FILTER=['*.img', '*.*.img', '*.syn', '*.dcm'], $
			TITLE='Select the CT file' )
	if ctFile eq '' then return, -1L
endif
if n_elements( rsFile ) eq 0 then begin
	rsFile = dialog_pickfile( PATH=path, $
			FILTER=['*.img', '*.*.img', '*.dcm'], $
			TITLE='Select the RS file' )
	if rsFile eq '' then return, -1L
endif

; Get CT data
bDicom = query_dicom( ctFile )
if bDicom then begin
	bOk = dcm2rawNonUniform( FILE=ctFile, DATA=pCT, DIMS=ctDims, PIXSIZE=ctPixSize, $
		POS=ctPos, PATNAME=patName, STUDYDATE=studyDate )
endif else begin
	bOk = gemini2raw( FILE=ctFile, DATA=pCT, DIMS=ctDims, PIXSIZE=ctPixSize, $
			POS=ctPos, PATNAME=patName, STUDYDATE=studyDate )
endelse
if total( *pCT ) eq 0 then begin
	ptr_free, pCT
	return, -1L
endif

print, "CT patient name: " + strtrim( patName, 2 )
print, "CT study date: " + strtrim( studyDate, 2 )

; Get RS data
if bDicom then begin
	bOk = read_rs( RSFILE=rsFile, $
			DICOM_CTFILE=ctFile, $
			ALL_ROIS=allROIs, $
			ROI_NAMES=roiNames, $
			ROI_COUNT=nROIs, $
			ROI_DATA=pROIs, $
			PATNAME=rsPatName, $
			STUDYDATE=rsStudyDate )
endif else begin
	bOk = read_rs( RSFILE=rsFile, $
			PHILIPS_CTFILE=ctFile, $
			ALL_ROIS=allROIs, $
			ROI_NAMES=roiNames, $
			ROI_COUNT=nROIs, $
			ROI_DATA=pROIs, $
			PATNAME=rsPatName, $
			STUDYDATE=rsStudyDate )
endelse
print, "RS patient name: " + strtrim( rsPatName, 2 )
print, "RS study date: " + strtrim( rsStudyDate, 2 )

; Fill slice gaps
for iROI=0, nROIs-1 do begin
	roi = (*pROIs)[*,*,*,iROI]
	sliceTotals = total( total( roi, 1 ), 1)
	roiSlices = where( sliceTotals ne 0, nROISlices )
	if nROISlices le 1 then continue
	maxSlice = max( roiSlices, MIN=minSlice )
	iSlice = minSlice+1
	while iSlice lt maxSlice do begin
		if sliceTotals[iSlice] eq 0 then begin
			; Find the nearest non-zero slices
			iBefore = iSlice-1
			iAfter = roiSlices[(where( roiSlices gt iSlice ))[0]]
			nInterpSlices = iAfter-iBefore-1
			xs = indgen( ctDims[0] )
			ys = indgen( ctDims[1] )
			zs = findgen( nInterpSlices ) / (nInterpSlices+1)
			newSlices = interpolate( [[[roi[*,*,iBefore]]],[[roi[*,*,iAfter]]]], $
				  xs, ys, zs, /GRID )
			(*pROIs)[*,*,iSlice:iAfter-1,iROI] = newSlices
			iSlice = iAfter+1
		endif else begin
			iSlice++
		endelse
	endwhile
endfor

; Reverse data (it's upside down)
*pCT = reverse( temporary(*pCT), 2 )
*pROIs = reverse( temporary(*pROIs), 2 )

; Write ROIs, if set
if bWrite then begin
	path = file_dirname( rsFile, /MARK )

	; Get patID and study time
	parts = strtrim(strsplit( file_dirname( rsFile ), "\", /EXTRACT, COUNT=nParts ),2)
	if nParts gt 3 then begin
		study = parts[nParts-1]
		patID = parts[nParts-2]
		prefix = patID + '_' + study
	endif else begin
		prefix = rsPatName
	endelse

	; Prompt
	text = textbox( TITLE='Write data to', $
					LABEL=['File name prefix:'], $
					VALUE=[prefix], $
					CANCEL=bCancel )
	if bCancel then begin
		void = dialog_message( 'ROIs for ' + rsPatName + ' not written' )
		ptr_free, pCT, pROIs
		return, -1L
	endif

	for iROI=0, nROIs-1 do begin
		fileName = path + prefix + '_' + strtrim(roiNames[iROI],2) + '.img'
		foundFiles = file_search( fileName, COUNT=nFiles )
		if nFiles gt 0 then begin
			bOk = dialog_message( /QUESTION, TITLE=fileName, $
					'File exists. Overwrite?' )
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
		fileName = path + prefix + suffix + '_' + strtrim(roiNames[iROI],2) + '.img'
		openw, fileOutUnit, fileName, /GET_LUN
		writeu, fileOutUnit, (*pROIs)[*,*,*,iROI]
		close, fileOutUnit
		free_lun, fileOutUnit
		fileBase = (strsplit( file_basename( fileName ), ".", /EXTRACT ))[0]
		txtFileName = path + fileBase + '_info.txt'
		openw, txtFileOutUnit, txtFileName, /GET_LUN
		printf, txtFileOutUnit, "Image Info"
		printf, txtFileOutUnit, "=========="
		printf, txtFileOutUnit, "nPx = ", strtrim( ctDims[0], 2 )
		printf, txtFileOutUnit, "nPy = ", strtrim( ctDims[1], 2 )
		printf, txtFileOutUnit, "nSlices = ", strtrim( ctDims[2], 2 )
		printf, txtFileOutUnit, "pixSizeX (mm) = ", strtrim( ctPixSize[0], 2 )
		printf, txtFileOutUnit, "pixSizeY (mm) = ", strtrim( ctPixSize[1], 2 )
		printf, txtFileOutUnit, "pixSizeZ (mm) = ", strtrim( ctPixSize[2], 2 )
		printf, txtFileOutUnit, "type = " + strtrim( size( *pROIs, /TYPE ) )
		printf, txtFileOutUnit, "type name = " + strtrim( size( *pROIs, /TNAME ) )
		close, txtFileOutUnit
		free_lun, txtFileOutUnit
	endfor
endif

if bWriteCT then begin

	fileName = path + prefix + '_CT.img'
	foundFiles = file_search( fileName, COUNT=nFiles )
	if nFiles gt 0 then begin
		bOk = dialog_message( /QUESTION, TITLE=fileName, $
				'File exists. Overwrite?' )
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
	fileName = path + prefix + suffix + '_CT.img'
	openw, fileOutUnit, fileName, /GET_LUN
	writeu, fileOutUnit, (*pCT)
	free_lun, fileOutUnit
	fileBase = (strsplit( file_basename( fileName ), ".", /EXTRACT ))[0]
	txtFileName = path + fileBase + '_info.txt'
	openw, txtFileOutUnit, txtFileName, /GET_LUN
	printf, txtFileOutUnit, "Image Info"
	printf, txtFileOutUnit, "=========="
	printf, txtFileOutUnit, "nPx = ", strtrim( ctDims[0], 2 )
	printf, txtFileOutUnit, "nPy = ", strtrim( ctDims[1], 2 )
	printf, txtFileOutUnit, "nSlices = ", strtrim( ctDims[2], 2 )
	printf, txtFileOutUnit, "pixSizeX (mm) = ", strtrim( ctPixSize[0], 2 )
	printf, txtFileOutUnit, "pixSizeY (mm) = ", strtrim( ctPixSize[1], 2 )
	printf, txtFileOutUnit, "pixSizeZ (mm) = ", strtrim( ctPixSize[2], 2 )
	printf, txtFileOutUnit, "type = " + strtrim( size( *pCT, /TYPE ) )
	printf, txtFileOutUnit, "type name = " + strtrim( size( *pCT, /TNAME ) )
	close, txtFileOutUnit
	free_lun, txtFileOutUnit

endif ; bWriteCT

rois = *pROIs
ptr_free, pCT, pROIs
print, 'Done rs2raw'
return, rois

end ; rs2raw

