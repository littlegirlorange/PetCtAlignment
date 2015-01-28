;==============================================================================
;
;	Method:		dcm2raw
;
;	Description:
;				Reads a DICOM format image series and returns the raw data
;				array with header info and/or saves the data in MK format
;				(.img/_info.txt)
;
;	Inputs:
;				DICOM image series
;
;	Outputs:
;	
; Required Modules:
;       dialog_select
;       
;	Written by:
;	      Maggie Kusano, August 10, 2004
;
; Version:
;       040810
;       Implemented DICOM read of PET and CT images.
;       Some CT DICOM files contain more than one image (a 64x64 and a
;       512x512).  Needed to add code to check for multiple images.
;       Currently, the largest of the bunch is used for further
;       processing.
;
;       040811
;       Sorted images according to image number since they may not
;       be correctly ordered when read by IDL.
;       Implemented basic algorithm based on Image Position information
;       contained in the DICOM header.
;
;       040817
;       First fully functional version.
;       Discovered that the PET and CT images are aligned at the centre
;       (e.g. the centre of the 3D arrays are aligned), so the pixel
;       values just need to be interpolated from PET-size to CT size
;       and then the data needs to be padded or trimmed around the
;       edges depending on whether the FOV of the PET data is smaller
;       or bigger than the CT FOV.
;       The Image Position information in the DICOM header is of no use.
;       Only need the number of rows, columns, and slices and the pixel
;       size and spacing to calculate the conversion.
;       Appears that IDL/my computer is only able to handle two large
;       image arrays at once.  Unused arrays must be cleared (set to 0)
;       to avoid crashes.
;
;       040818
;       Cleaned up code.
;
;       041117
;       Made image write optional.  If the user opts to save the image
;       data, he/she may select a sub-range of images slices to save.
;
;       041123 get_tumour_slices
;       Hacked the code so that the user is able to read/write CT images
;       only (instead of PET/CT data).  No alignment is performed.  The
;       DICOM file is read and a subset of the data array is saved.
;
;       050404 get_pet_ct_bkgd
;       Back to saving PET/CT data.  The program will suggest a slice range
;       based on DICOM RS contours (converted to int arrays by Ananth's
;       ICAS program).  Otherwise, the user can select a subrange of data
;       to save.
;
;       060118
;       Version to handle Huan's PET/CT head and neck data.  Can no longer
;       use instance number (0020 0013) to order the CT images since
;       the numbers are not unique.  Now use the third field (z) of
;       image position (0020 0032) to order both the PET and CT data.
;       Also, the slice spacing of the H&N data is not uniform, so this
;       new version interpolates the image position data into uniformly
;       spaced slices, distanced by the smallest slice spacing (usually
;       1.6 mm).
;
;       060315
;       New data now has uniform slice spacing so ditched interpolation.
;       Changed array storage to use HD space instead of RAM so that
;       both the PET and CT can be kept in memory at the same time.
;       This was done so that slice range selection can be done at the
;       end, after the user has seen both the PET and the CT data.
;       No longer reverse the arrays along Z so the slices run inf-sup,
;       consistent with the DICOM data and ICAS output.
;       The user is now prompted for each file saved (PET, CT and ROI)
;       so that alternative file names can be chosen.
;       Fixed contour display so that all contours are shown, not just
;       the largest for each slice (ditched contour_mask).
;       Finally works for Huan's data, the specifications for which are:
;         CT  - Image Type = DERIVED\SECONDARY\AXIAL
;             (3.2 mm slice spacing)
;         PET - Image Type = DERIVED\PRIMARY
;             (resampled PETs)
;
;       060315 align_pet_ct
;
;       080602 dcm2raw
;       Version for dynamic PET project.
;
;==============================================================================

function dcm2rawNonUniform, $
	FILE		= file, $
	DATA		= pData, $
	DIMS		= dims, $
	PIXSIZE		= pixSize, $
	POSITION	= pos, $
	PATNAME		= patName, $
	STUDYDATE	= studyDate, $
	WEIGHT		= weight, $
	SUVSCL		= suvScl, $
	ACQTIME		= acqTime, $
	WRITE		  = bWriteImages

if n_elements( bWriteImages ) eq 0 then bWriteImages = 0b

; Get image list
if n_elements( file ) eq 0 then begin
	file = dialog_pickfile( FILTER = '*', $
			  GET_PATH = fileDir, $
			  TITLE='Select the first DICOM file in the series' )
	if file eq '' then return, 0b
endif else begin
	fileDir = file_dirname( file, /MARK )
endelse

; Get the base name of the series
fileBase = ''
fileBase = (strsplit( file_basename( file ), '.', /EXTRACT ))[0]

; Retrieve a list of all files in the current directory with this basename
fileList = file_search( fileDir + fileBase+'*', COUNT=nFiles )
if nFiles eq 1 then begin
	; Plastimatch anonymized DICOM of the form imageXXX.dcm
	fileList = file_search( fileDir + 'image*.dcm', COUNT=nFiles )
	if nFiles eq 1 then return, 0b
endif

; Get header information from the selected file
oFirstImage = obj_new( 'IDLffDICOM' )
isOk = oFirstImage->read( file )
pixSize		= oFirstImage->getValue( '0028'x, '0030'x, /NO_COPY )
patName		= oFirstImage->getValue( '0010'x, '0010'x, /NO_COPY )
studyDate	= oFirstImage->getValue( '0008'x, '0020'x, /NO_COPY )
modality	= oFirstImage->getValue( '0008'x, '0060'x, /NO_COPY )
patName		= strtrim( strjoin( strsplit( *patName[0], '^', /EXTRACT), '_'), 2 )
patName		= strtrim( strjoin( strsplit( patName, /EXTRACT), '_'), 2 )
studyDate	= strtrim( *studyDate[0], 2 )
modality	= strtrim( *modality[0], 2 )
if modality eq 'PT' then begin
	weight	= float( string( *(oFirstImage->getValue( '0010'x, '1030'x, /NO_COPY ))[0] ) )
	suvScl	= float( string( *(oFirstImage->getValue( '7053'x, '1000'x, /NO_COPY ))[0] ) )
endif else begin
	weight = 0.0
	suvScl = 1.0
endelse

print, "Processing " + modality + " data for patient: ", patName

; Read the files
acqArr = strarr( nFiles )
posArr = strarr( nFiles )
zPosArr = fltarr( nFiles )
spacArr = strarr( nFiles )
rowsArr = strarr( nFiles )
colsArr = strarr( nFiles )
pixArr = strarr( nFiles )

nTimes = 0
curAcqTime = '-1'
bNotified = 0b

for i=0, nFiles-1 do begin

	oDcmImage = obj_new( 'IDLffDICOM' )
	isOk = oDcmImage->read( fileList[i] )
	pAcqTime	= oDcmImage->getValue( '0008'x, '0032'x, /NO_COPY )
	pImgPos		= oDcmImage->getValue( '0020'x, '0032'x, /NO_COPY )
	pImgSpac 	= oDcmImage->getValue( '0018'x, '0088'x, /NO_COPY )
	if not ptr_valid(pImgSpac) then begin
		pImgSpac	= oDcmImage->getValue( '0018'x, '0050'x, /NO_COPY )
		if not bNotified then begin
			print, 'Warning: Slice spacing not found, using slice thickness instead.'
			bNotified = 1b
		endif
	endif
	pRows		= oDcmImage->getValue( '0028'x, '0010'x, /NO_COPY )
	pCols		= oDcmImage->getValue( '0028'x, '0011'x, /NO_COPY )
	pPixSpac	= oDcmImage->getValue( '0028'x, '0030'x, /NO_COPY )
	acqArr[i]	= strtrim( *pAcqTime[0], 2 )
	posArr[i]	= strtrim( *pImgPos[0], 2 )
	zPosArr[i]	= (float( strsplit( posArr[i], '\', /EXTRACT ) ))[2]
	spacArr[i]	= strtrim( *pImgSpac[0], 2 )
	rowsArr[i]	= strtrim( *pRows[0], 2 )
	colsArr[i]	= strtrim( *pCols[0], 2 )
	pixArr[i]	= strtrim( *pPixSpac[0], 2 )

	; Destroy the IDL DICOM object
	obj_destroy, oDcmImage

end

; Prompt if we have more than one series
indices = uniq( acqArr, sort( acqArr ) )
times = acqArr[indices]
acqTime = double( strmid( times, 0, 2 ) )*60*60 $
		+ double( strmid( times, 2, 2 ) )*60 $
		+ double( strmid( times, 4, 2 ) )

if modality eq 'CT' then begin

	nTimes = n_elements( indices )

	if nTimes gt 1 then begin

		nSlices = intarr( nTimes )
		list = strarr( nTimes )
		for i=0, nTimes-1 do begin
			void = where( acqArr eq acqArr[indices[i]], count )
			nSlices[i] = count
			msg = strtrim(i+1,2) + ': ' + rowsArr[indices[i]] + '\' $
					+ colsArr[indices[i]] + '\' + strtrim(nSlices[i],2) + 'pix, ' $
					+ pixArr[indices[i]] + '\' + spacArr[indices[i]] + 'mm'
			list[i] = msg
		endfor
		iSeries = (dialog_select( LIST=list, TITLE='Multiple series found. Select series.' ))[0]
		if iSeries eq -1L then return, 0b

		; Trim down to selected series
		indices = where( acqArr eq acqArr[indices[iSeries]], nSlices )
		fileList = fileList[indices]
		posArr = posArr[indices]
		zPosArr = zPosArr[indices]
		spacArr = spacArr[indices]
		rowsArr = rowsArr[indices]
		colsArr = colsArr[indices]
		pixArr = pixArr[indices]

	endif else begin

		nSlices = nFiles

	endelse

endif else begin ; PET

	nSlices = nFiles

endelse

; Sort according to image z position
order = sort( zPosArr )
zPosArr = zPosArr[order]
fileList = fileList[order]
posArr = posArr[order]
spacArr = spacArr[order]
pixArr = pixArr[order]
nPx = colsArr[0]
nPy = rowsArr[0]
pixSize	= float( strsplit( pixArr[0], '\', /EXTRACT ) )

biggestImgDim = 0
biggestImgIndex = 0

if query_dicom( fileList[0], info ) then begin

	if info.num_images gt 1 then begin
		; Find the largest image (ignore thumbnail)
		for i=0, info.num_images-1 do begin
			isOk = query_dicom( fileList[0], tmpInfo, IMAGE_INDEX=i )
			if tmpInfo.dimensions[0] gt biggestImgDim then begin
				biggestImgDim = tmpInfo.dimensions[0]
				biggestImgIndex = i
			endif
		endfor
	endif

endif else begin

	void = dialog_message( 'Error reading DICOM file. Returning.' )
	return, 0b

endelse

; Read image data
data = intarr( nPx, nPy, nSlices )
for i=0, nSlices-1 do begin
	img = $
		read_dicom( fileList[i], IMAGE_INDEX=biggestImgIndex )
	data[0,0,i] = img
endfor

; Destroy the IDL DICOM object
obj_destroy, oFirstImage

pos = fltarr( 3, nSlices )
for i=0, nSlices-1 do begin
	pos[*,i] = float( strsplit( posArr[i], '\', /EXTRACT ) )
endfor

if nTimes gt 1 then begin
	; Average image slice thickness
	dataTmp = intarr( nPx, nPy, nSlices/2 )
	posTmp = fltarr( 3, nSlices/2 )
	for iSlice=0, nSlices/2-1 do begin
		dataTmp[*,*,iSlice] = data[*,*,iSlice*2]
		posTmp[*,iSlice] = pos[*,iSlice*2]
;		dataTmp[*,*,iSlice] = fix((data[*,*,iSlice*2] + data[*,*,iSlice*2+1])/2.0)
;		posTmp[*,iSlice] = (pos[*,iSlice*2] + pos[*,iSlice*2+1])/2.0
	endfor
	data = dataTmp
	pos = posTmp
	dataTmp = 0
	posTmp = 0
endif
sliceThickness = mean( (pos[2,*]-shift(pos[2,*],1))[1:n_elements(pos[2,*])-1] )
pixSize = [pixSize[0], pixSize[1], sliceThickness]
dims = size( data, /DIM )

if bWriteImages then begin

	file = fileDir + patName + '_' + studyDate + '_' + modality + '.img'
	fileOutName = dialog_pickfile( /WRITE, FILE=file, /OVERWRITE_PROMPT, $
			DEFAULT_EXTENSION='img', GET_PATH=writeDir )
	if fileOutName ne '' then begin

		; Write image data
		openw, fileOutUnit, fileOutName, /GET_LUN
		for i=0, nSlices-1 do begin
			writeu, fileOutUnit, data ; int
		endfor
		free_lun, fileOutUnit
		close, lun
		file_delete, TMP_FILE, /QUIET

		; Write text file info
		fileBase = (strsplit( file_basename( fileOutName ), ".", /EXTRACT ))[0]
		txtFileOutName = writeDir + fileBase + "_info.txt"
		openw, txtFileOutUnit, txtFileOutName, /GET_LUN
		printf, txtFileOutUnit, "Image Info"
		printf, txtFileOutUnit, "=========="
		printf, txtFileOutUnit, "nPx = ", strtrim( nPx, 2 )
		printf, txtFileOutUnit, "nPy = ", strtrim( nPy, 2 )
		printf, txtFileOutUnit, "nSlices = ", strtrim( nSlices, 2 )
		printf, txtFileOutUnit, "pixSizeX (mm) = ", strtrim( pixSize[0], 2 )
		printf, txtFileOutUnit, "pixSizeY (mm) = ", strtrim( pixSize[0], 2 )
		printf, txtFileOutUnit, "pixSizeZ (mm) = ", strtrim( sliceThickness, 2 )
		if modality eq 'PT' then begin
			printf, txtFileOutUnit, "suv scale = ", strtrim( suvScl, 2 )
			printf, txtFileOutUnit, "weight (kg) = ", strtrim( weight, 2 )
		endif
		free_lun, txtFileOutUnit
	endif else begin
		writeDir = WRITE_DIR
		void = dialog_message( 'File name not selected.  Data not written.' )
	endelse

	pData = 1L

endif else begin; bWriteImages

	pData = ptr_new( data, /NO_COPY )

endelse

return, 1b

end ; of dcm2raw