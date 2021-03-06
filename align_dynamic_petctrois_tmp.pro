;==============================================================================
;
;  METHOD
;		align_dynamic_petctrois
;
;  DESCRIPTION
;		Reads dynamic H&N PET/CT and RT structure set data and saves all in mk
;		format.  PET/CT data may be in DICOM or Philips native format.
;		RT structure sets must be in DICOM format.
;		To begin, PETs are aligned and resized to their respective CTs using
;		header position info.  RT structure sets are also decoded in CT space.
;
;  SYNTAX
;		align_dynamic_petctrois
;
;  INPUTS
;		The program searches for the following inputs using the directories
;		and filters specified in the user_prefs file.  If not found, the
;		program will prompt.
;
;		; The patient ID number
;			PAT_NO 		= '012'
;		; The root read/write directory
;			MAIN_DIR 	= "C:\Users\makusan\Documents\Research\DynamicPET\Sunnybrook Data\"
;		; The directory to write to
;			WRITE_DIR 	= main_dir + "Coreg\"
;		; The directory to read from
;			READ_DIR 	= main_dir
;		; The directory to save temporary files to
;			TEMP_DIR = write_dir
;		; The subfolders in READ_DIR to find pre data
;			CT0_DIR		= "CTAC\"
;			PET0_DIR	= "PET\"
;			RS0_DIR		= "Contours\"
;		; File name filters to apply when searching for pre data
;			CT0_FILTER	= "CT*.syn"
;			PET0_FILTER	= "*ctac_zm.img"
;			RS0_FILTER	= "RS*.dcm"
;		; File naming preferences
;			patName 	= "S"+PAT_NO
;			prePrefix 	= patName + '_pre'
;		; Write images (only set to 0b for debugging)
;			bWriteImages = 1b
;			bDisplay 	= 0b
;
;  OUTPUTS
;		Aligned pre and intra PET, CT (ints) and ROI (bytes) data arrays and
;		corresponding text files with image and pixel dimensions, frame times
;		and alignment parameters:
;			<prePrefix>_CT.img/.txt			(pre CT)
;			<prePrefix>_PET.img/.txt		(pre PET)
;			<prePrefix>_<roi_name>.img/txt	(pre roi)
;			<postPrefix>_CT.img/.txt		(intra CT)
;			<postPrefix>_PET.img/.txt		(intra PET)
;			<postPrefix>_<roi_name>.img/txt	(intra roi)
;
;  REQUIRED MODULES
;		getPetCtROIs
;		alignDataSets
;		textbox
;		coreg2petCtRois
;		contour_mask
;		dcm2rawNonUniform
;		gemini2raw
;		read_rs
;		align_pet2ct
;		align_ct2pet
;		chamfer_match
;		align_vert
;		align_y
;		dt
;		transform_volume
;		dialog_select
;		select_contours
;		allo_volume_read
;		idl_mhdr_struct
;		idl_subhdr_struct
;		idl_exthdr_struct
;
;  WRITTEN BY
;		Maggie Kusano, April 10, 2012
;
;  VERSION
;		071112
;		Version adapted from get_pet_ct_slope
;
;		071123
;		Version used to generate data sent to Huan on 071116.
;
;		090306
;		Version used for Ian Poon's CARO abstract.
;
;		090824
;		Reversed alignment (now align pre to intra) for Mike.
;
;		090829
;		Version passed to Mike.
;
;		120410
;		Version for PVC of carotids. Removed coregistration of pre- and intra-.
;
;==============================================================================

pro align_dynamic_petctrois

@ user_prefs

;==============================================================================
;
;	Get PET/CT data
;

; Pre-treatement data
ct0Path = READ_DIR+CT0_DIR
ctFile0 = (file_search( ct0Path+CT0_FILTER , COUNT=count ))[0]
if count eq 0 then begin
	ctFile0 = dialog_pickfile( PATH=ct0Path, $
			GET_PATH=ct0Path, $
			FILTER=['*.img', '*.*.img', '*.syn', '*.dcm'], $
			TITLE='Select a single pre-treatment CT file' )
	if ctFile0 eq '' then return
endif

; Get patient time (pre or intra) and patient id from file path
parts = strsplit( ct0Path, '\', /EXTRACT, COUNT=nParts )
patId = parts[nParts-1]
time = parts[nParts-2]
pet0Path = READ_DIR+PET0_DIR+time+'\'+patId+'\'
petFile0 = (file_search( pet0Path+PET0_FILTER, COUNT=count ))[0]
if count eq 0 then begin
	petFile0 = dialog_pickfile( PATH=pet0Path, $
			GET_PATH=pet0Path, $
			FILTER=['*.img', '*.*.img', '*.syn', '*.dcm'], $
			TITLE='Select a single pre-treatment PET file' )
	if petFile0 eq '' then return
endif

rs0Path = READ_DIR+RS0_DIR+time+'\'+patId+'\'
rsFile0 = (file_search( rs0Path, '*.img', COUNT=count ))[0]
if count ne 1 then begin
	rsFile0 = dialog_pickfile( PATH=rs0Path, $
			FILTER=['*.dcm','*.img'], $
			TITLE='Select pre-treatment contour file' )
endif

petDataFile0 = TEMP_DIR+'pet0.tmp'
roiDataFile0 = TEMP_DIR+'rois0.tmp'

bOk = getPetCtRois( $
		PET_FILE=petFile0, $
		PET_ASSOC_FILE=petDataFile0, $
		PET_DIMS=petDims0, $
		PET_PIXSIZE=petPixSize0, $
		PET_FRAME_TIMES=times0, $
		PET_POS=petPos0, $
		PET_SCL=petScl0, $
		CT_FILE=ctFile0, $
		CT_DATA=pCT0, $
		CT_DIMS=ctDims0, $
		CT_PIXSIZE=ctPixSize0, $
		ROI_FILE=rsFile0, $
		ROI_ASSOC_FILE=roiDataFile0, $
		ROI_COUNT=nRois0, $
		ROI_NAMES=roiNames0, $
		/COREG2CT )

if not bOk then begin
	if ptr_valid( pCT0 ) then ptr_free, pCT0
	return
endif

if not bOk then begin
	ptr_free, pCT0
	close, /ALL
	return
endif

; Flip images (they're upside-down)
*pCT0 = reverse( temporary( *pCT0 ), 2 )
openu, petLun0, /GET_LUN, petDataFile0
tmpPet0 = assoc( petLun0, intarr( ctDims0 ) )
for iFrame=0, n_elements(times0)-1 do begin
	tmpPet0[iFrame] = reverse( temporary( tmpPet0[iFrame] ), 2 )
endfor
tmpPet0 = 0

openu, roiLun0, /GET_LUN, roiDataFile0
rois0 = assoc( roiLun0, bytarr( ctDims0 ) )
for iROI=0, nROIs0-1 do begin
	tmp = reverse( rois0[iRoi], 2 )
	rois0[iRoi] = tmp
endfor
rois0 = 0
tmp = 0
close, roiLun0 & free_lun, roiLun0

;==============================================================================
;
;	Display images
;
if bDisplay then begin

; Reset display and load grayscale colour table
device, DECOMPOSED=0
loadct, 0, /SILENT
topClr = !D.TABLE_SIZE-1

; Display pre- images
roiMask = bytarr( ctDims0 )
if nROIs0 ne 0 then begin
	openr, lun, /GET_LUN, roiDataFile0
	rois = assoc( lun, bytarr( ctDims0 ) )
	for iROI=0, nROIs0-1 do begin
		roiMask = roiMask or (rois[iROI] gt 0)
	endfor
	close, lun & free_lun, lun
endif
dispImg = bytscl( *pCT0, TOP=254 )

; Contour pre ROIs for display, if any
void = where( roiMask ne 0, count )
if count ne 0 then begin
	for iSlice=0, ctDims0[2]-1 do begin
		slice = roiMask[*,*,iSlice]
		dispSlice = dispImg[*,*,iSlice]
		indices = where( slice ne 0, count )
		if count eq 0 then continue
		contIndices = contour_mask( slice, /INDICES )
		dispSlice[contIndices] = 255b
		dispImg[0,0,iSlice] = dispSlice
	endfor
endif
tv3d, dispImg, RED=255, TITLE='Pre-treatment CT & contours'

close, /ALL

endif

;==============================================================================
;
;	Write images
;
if bWriteImages then begin

	print, 'Writing images for patient ', patId

	folder = dialog_pickfile( TITLE='Select folder to write to', $
			/DIRECTORY, PATH=WRITE_DIR, GET_PATH=writeDir )

	if folder eq '' then begin
		void = dialog_message( 'Data not written.' )
	endif else begin

		; Check for existing files
		fileName = writeDir + patId + '_' + time + '_CT.img'
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

		; Pre-treatment CT
		preCTFileName = writeDir + patId + '_' + time + suffix + '_CT.img'
		openw, fileOutUnit, preCTFileName, /GET_LUN
		writeu, fileOutUnit, (*pCT0)
		close, fileOutUnit & free_lun, fileOutUnit
		fileBase = (strsplit( file_basename( preCTFileName ), ".", /EXTRACT ))[0]
		txtFileName0 = writeDir + fileBase + '_info.txt'
		openw, txtFileOutUnit, txtFileName0, /GET_LUN
		printf, txtFileOutUnit, "Image Info"
		printf, txtFileOutUnit, "=========="
		printf, txtFileOutUnit, "nPx = ", strtrim( ctDims0[0], 2 )
		printf, txtFileOutUnit, "nPy = ", strtrim( ctDims0[1], 2 )
		printf, txtFileOutUnit, "nSlices = ", strtrim( ctDims0[2], 2 )
		printf, txtFileOutUnit, "pixSizeX (mm) = ", strtrim( ctPixSize0[0], 2 )
		printf, txtFileOutUnit, "pixSizeY (mm) = ", strtrim( ctPixSize0[1], 2 )
		printf, txtFileOutUnit, "pixSizeZ (mm) = ", strtrim( ctPixSize0[2], 2 )
		printf, txtFileOutUnit, "type = ", strtrim( size( (*pCT0), /TYPE ) )
		printf, txtFileOutUnit, "type name = ", strtrim( size( (*pCT0), /TNAME ) )
		close, txtFileOutUnit & free_lun, txtFileOutUnit

		; Pre-treatment PET
		openr, lun, /GET_LUN, petDataFile0
		fileName = writeDir + patId + '_' + time + suffix + '_PET.img'
		file_copy, petDataFile0, fileName, /OVERWRITE
		file_delete, petDataFile0
		fileBase = (strsplit( file_basename( fileName ), ".", /EXTRACT ))[0]
		txtFileName0 = writeDir + fileBase + '_info.txt'
		openw, txtFileOutUnit, txtFileName0, /GET_LUN
		printf, txtFileOutUnit, "Image Info"
		printf, txtFileOutUnit, "=========="
		printf, txtFileOutUnit, "nPx = ", strtrim( ctDims0[0], 2 )
		printf, txtFileOutUnit, "nPy = ", strtrim( ctDims0[1], 2 )
		printf, txtFileOutUnit, "nSlices = ", strtrim( ctDims0[2], 2 )
		printf, txtFileOutUnit, "pixSizeX (mm) = ", strtrim( ctPixSize0[0], 2 )
		printf, txtFileOutUnit, "pixSizeY (mm) = ", strtrim( ctPixSize0[1], 2 )
		printf, txtFileOutUnit, "pixSizeZ (mm) = ", strtrim( ctPixSize0[2], 2 )
		printf, txtFileOutUnit, "type = ", strtrim( size( (*pCT0), /TYPE ) )
		printf, txtFileOutUnit, "type name = ", strtrim( size( (*pCT0), /TNAME ) )
		printf, txtFileOutUnit, "nFrames = ", strtrim( n_elements(times0), 2 )
		printf, txtFileOutUnit, "frame times (s) = " + strjoin( strtrim(times0,2), ', ' )
		printf, txtFileOutUnit, "suv scale = " + strjoin( strtrim(petScl0,2), ', ' )
		close, txtFileOutUnit & free_lun, txtFileOutUnit

		; Pre-treatment ROIs
		openr, lun, /GET_LUN, roiDataFile0
		rois = assoc( lun, bytarr( ctDims0 ) )
		for iROI=0, nROIs0-1 do begin
			fileName = writeDir + patId + '_' + time + suffix + '_' + roiNames0[iROI] + '.img'
			openw, fileOutUnit, fileName, /GET_LUN
			writeu, fileOutUnit, rois[iROI]
			close, fileOutUnit & free_lun, fileOutUnit
			fileBase = (strsplit( file_basename( fileName ), ".", /EXTRACT ))[0]
			txtFileName0 = writeDir + fileBase + '_info.txt'
			openw, txtFileOutUnit, txtFileName0, /GET_LUN
			printf, txtFileOutUnit, "Image Info"
			printf, txtFileOutUnit, "=========="
			printf, txtFileOutUnit, "nPx = ", strtrim( ctDims0[0], 2 )
			printf, txtFileOutUnit, "nPy = ", strtrim( ctDims0[1], 2 )
			printf, txtFileOutUnit, "nSlices = ", strtrim( ctDims0[2], 2 )
			printf, txtFileOutUnit, "pixSizeX (mm) = ", strtrim( ctPixSize0[0], 2 )
			printf, txtFileOutUnit, "pixSizeY (mm) = ", strtrim( ctPixSize0[1], 2 )
			printf, txtFileOutUnit, "pixSizeZ (mm) = ", strtrim( ctPixSize0[2], 2 )
			printf, txtFileOutUnit, "type = ", strtrim( size( rois[iROI], /TYPE ) )
			printf, txtFileOutUnit, "type name = ", strtrim( size( rois[iROI], /TNAME ) )
			close, txtFileOutUnit & free_lun, txtFileOutUnit
		endfor
		close, lun & free_lun, lun
		file_delete, roiDataFile0

	endelse

endif

ptr_free, pCT0

beep
print, 'Done align_dynamic_petctrois'

end ; of align_dynamic_petctrois