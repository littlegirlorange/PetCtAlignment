;==============================================================================
;
;  METHOD
;		align_pre2post_dynamic_petctrois
;
;  DESCRIPTION
;		Aligns pre- and intra-treatment dynamic H&N PET/CT and RT structure
;		set data (aligns pre to intra).  PET/CT data may be in DICOM or
;		Philips native format.  RT structure sets must be in DICOM format.
;		To begin, PETs are aligned and resized to their respective CTs using
;		header position info.  RT structure sets are also decoded in CT space.
;		The pre study is then aligned to the intra CT by chamfer matching.
;
;  SYNTAX
;		align_pre2post_dynamic_petctrois
;
;  INPUTS
;		The program searches for the following inputs using the directories
;		and filters specified in the user_prefs file.  If not found, the
;		program will prompt.
;
;		; The patient ID number
;			PAT_NO 		= '012'
;		; The root read/write directory
;			MAIN_DIR 	= "c:\documents and settings\mkusano\work\dynamic pet\data\"
;		; The directory to write to
;			WRITE_DIR 	= main_dir + "Pre2Intra\"
;		; The directory to read from
;			READ_DIR 	= main_dir + "Raw PET-CT\s-"+PAT_NO+"\"
;		; The directory to save temporary files to
;			TEMP_DIR = write_dir
;		; The subfolders in READ_DIR to find pre data
;			CT0_DIR		= "s0\"
;			PET0_DIR	= "s0\"
;			RS0_DIR		= "s0\"
;		; File name filters to apply when searching for pre data
;			CT0_FILTER	= "CT*.syn"
;			PET0_FILTER	= "*ctac_zm.img"
;			RS0_FILTER	= "RS*.dcm"
;		; The subfolders in READ_DIR to find intra data
;			CT1_DIR		= "s1\"
;			PET1_DIR	= "s1\"
;			RS1_DIR		= "s1\"
;		; File name filters to apply when searching for intra data
;			CT1_FILTER	= CT0_FILTER
;			PET1_FILTER	= PET0_FILTER
;			RS1_FILTER	= "S-012*.dcm"
;		; File naming preferences
;			patName 	= "S"+PAT_NO
;			prePrefix 	= patName + '_pre'
;			postPrefix 	= patName + '_intra'
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
;		Maggie Kusano, March 6, 2009
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
;==============================================================================

pro align_pre2post_dynamic_petctrois

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

pet0Path = READ_DIR+PET0_DIR
petFile0 = (file_search( pet0Path+PET0_FILTER, COUNT=count ))[0]
if count eq 0 then begin
	petFile0 = dialog_pickfile( PATH=pet0Path, $
			GET_PATH=pet0Path, $
			FILTER=['*.img', '*.*.img', '*.syn', '*.dcm'], $
			TITLE='Select a single pre-treatment PET file' )
	if petFile0 eq '' then return
endif

rs0Path = READ_DIR+RS0_DIR
rsFile0 = (file_search( rs0Path+RS0_FILTER, COUNT=count ))[0]
if count ne 1 then begin
	rsFile0 = dialog_pickfile( PATH=rs0Path, $
			FILTER=['*.dcm','*.img'], $
			TITLE='Select pre-treatment contour file' )
endif

if n_elements( prePrefix ) eq 0 then $
	prePrefix = (strsplit( file_basename( petFile0 ), '_.', /EXTRACT ))[0]

; Post-treatment data
ct1Path = READ_DIR+CT1_DIR
ctFile1 = (file_search( ct1Path+CT1_FILTER , COUNT=count ))[0]
if count eq 0 then begin
	ctFile1 = dialog_pickfile( PATH=ct1Path, $
			FILTER=['*.img', '*.*.img', '*.syn', '*.dcm'], $
			TITLE='Select a single intra-treatment CT file' )
	if ctFile1 eq '' then return
endif

pet1Path = READ_DIR+PET1_DIR
petFile1 = (file_search( pet1Path+PET1_FILTER, COUNT=count ))[0]
if count eq 0 then begin
	petFile1 = dialog_pickfile( PATH=pet1Path, $
			FILTER=['*.img', '*.*.img', '*.syn', '*.dcm'], $
			TITLE='Select a single intra-treatment PET file' )
	if petFile1 eq '' then return
endif

rs1Path = READ_DIR+RS1_DIR
rsFile1 = (file_search( rs1Path+RS1_FILTER, COUNT=count ))[0]
if count ne 1 then begin
	rsFile1 = dialog_pickfile( PATH=rs1Path, $
			FILTER=['*.dcm','*.img'], $
			TITLE='Select intra-treatment contour file' )
endif

if n_elements( postPrefix ) eq 0 then $
	postPrefix = (strsplit( file_basename( petFile1 ), '_.', /EXTRACT ))[0]

; Distance transform
dtFile = dialog_pickfile( PATH=READ_DIR, $
		FILTER='*dt.img', $
		TITLE='Select distance transform file' )
if dtFile eq '' then begin
	writeFile = dialog_pickfile( PATH=WRITE_DIR, $
			FILE=postPrefix+'_chamfer_dt.img', $
			FILTER='*_dt.img', $
			TITLE='Select file to write distance transform to' )
endif else begin
	writeFile = ''
endelse

petDataFile0 = TEMP_DIR+'pet0.tmp'
petDataFile1 = TEMP_DIR+'pet1.tmp'
roiDataFile0 = TEMP_DIR+'rois0.tmp'
roiDataFile1 = TEMP_DIR+'rois1.tmp'

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

bOk = getPetCtRois( $
		PET_FILE=petFile1, $
		PET_ASSOC_FILE=petDataFile1, $
		PET_DIMS=petDims1, $
		PET_PIXSIZE=petPixSize1, $
		PET_FRAME_TIMES=times1, $
		PET_POS=petPos1, $
		PET_SCL=petScl1, $
		CT_FILE=ctFile1, $
		CT_DATA=pCT1, $
		CT_DIMS=ctDims1, $
		CT_PIXSIZE=ctPixSize1, $
		ROI_FILE=rsFile1, $
		ROI_ASSOC_FILE=roiDataFile1, $
		ROI_COUNT=nRois1, $
		ROI_NAMES=roiNames1, $
		/COREG2CT )

if not bOk then begin
	ptr_free, pCT0, pCT1
	close, /ALL
	return
endif

; Make sure image and pixel dims match
if (total( ctDims0-ctDims1 ) ne 0) or $
   (total( ctPixSize0-ctPixSize1 ) ne 0) then begin

   ; Make the pre-PET/CT the same size as the post
	bOk = alignDataSets( DATA1=pCT1, PIXSIZE1=ctPixSize1, $
			DATA2=pCT0, PIXSIZE2=ctPixSize0, /CENTER )
	if not bOk then begin
		void = dialog_message( /ERROR, 'Error resizing pre-CT. Returning' )
		ptr_free, pCT0, pCT1
		close, /ALL
		return
	endif
	bOk = alignDataSets( DATA1=pCT1, PIXSIZE1=ctPixSize1, $
			DATAFILE2=petDataFile0, DIMS2=[ctDims0,n_elements(times0)], $
			PIXSIZE2=ctPixSize0, /CENTER  )
	if not bOk then begin
		void = dialog_message( /ERROR, 'Error resizing pre-PET. Returning' )
		return
	endif
	if nROIs0 ne 0 then begin
		openu, lun, /GET_LUN, roiDataFile0
		rois = assoc( lun, bytarr( ctDims0 ) )
		tmpFile = TEMP_DIR + 'rois0.tmp2'
		openw, tmpLun, /GET_LUN, tmpFile
		tmp = assoc( tmpLun, bytarr( ctDims1 ) )
		for iRoi=0, nRois0-1 do begin
			pRoi = ptr_new( rois[iRoi] )
			bOk = alignDataSets( DATA1=pCT1, PIXSIZE1=ctPixSize1, $
					DATA2=pRoi, DIMS2=ctDims0, $
					PIXSIZE2=ctPixSize0, /CENTER, /NN  )
			tmp[iRoi] = *pROI
			ptr_free, pRoi
			if not bOk then begin
				void = dialog_message( /ERROR, 'Error resizing pre-ROIs. Returning' )
				close, /ALL
				return
			endif
		endfor
		close, lun & free_lun, lun
		close, tmpLun & free_lun, tmpLun
		file_delete, roiDataFile0
		wait, 3
		file_move, tmpFile, roiDataFile0
	endif
	ctPixSize0 = ctPixSize1
	ctDims0 = ctDims1
endif

;; Flip images (they're upside-down)
*pCT0 = reverse( temporary( *pCT0 ), 2 )
*pCT1 = reverse( temporary( *pCT1 ), 2 )
openu, petLun0, /GET_LUN, petDataFile0
openu, petLun1, /GET_LUN, petDataFile1
tmpPet0 = assoc( petLun0, intarr( ctDims0 ) )
tmpPet1 = assoc( petLun1, intarr( ctDims0 ) )
for iFrame=0, n_elements(times0)-1 do begin
	tmpPet0[iFrame] = reverse( temporary( tmpPet0[iFrame] ), 2 )
	tmpPet1[iFrame] = reverse( temporary( tmpPet1[iFrame] ), 2 )
endfor
tmpPet0 = 0
tmpPet1 = 0

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; ;Uncomment the following lines if you want to use dicom ROIs as well

;openu, roiLun0, /GET_LUN, roiDataFile0
;openu, roiLun1, /GET_LUN, roiDataFile1
;rois0 = assoc( roiLun0, bytarr( ctDims0 ) )
;rois1 = assoc( roiLun1, bytarr( ctDims0 ) )
;for iROI=0, nROIs0-1 do begin
;	tmp = reverse( rois0[iRoi], 2 )
;	rois0[iRoi] = tmp
;endfor
;for iROI=0, nROIs1-1 do begin
;	tmp = reverse( rois1[iRoi], 2 )
;	rois1[iRoi] = tmp
;endfor
;rois0 = 0
;rois1 = 0
;tmp = 0
;close, roiLun0 & free_lun, roiLun0
;close, roiLun1 & free_lun, roiLun1
; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

; Show moving CT
tv3d, (*pCT0), TITLE='Pre-treatment CT'

; Create a post-CT bone mask.  Sum slices and find mins to locate
; spine-only region
ct1Mask = *pCT0 gt 1400
nBonePixels = total( total( ct1Mask, 1 ), 1 )
window, 1 & plot, nBonePixels

; Prompt user to select a subvolume to align (e.g. just the neck)
bCancel = 0b
bDone = 0b
iFirst = [0,0,0]
iLast = ctDims0-1

if n_elements( RANGE ) eq 0 then begin
	bDone = 0b
endif else begin
	if (range[0] ge 0) and (range[1] le ctDims0[1]-1) then begin
		iFirst = [0,0,range[0]]
		iLast = [ctDims0[0]-1,ctDims0[1]-1,range[1]]
		bDone = 1b
	endif else begin
		bDone = 0b
	endelse
endelse

while bDone eq 0b do begin

	text = textbox( TITLE='Enter data range to align', $
					LABEL=['x0:','x1:','y0:','y1:','z0:','z1:'], $
					VALUE=[strtrim(iFirst[0],2),strtrim(iLast[0],2),$
						   strtrim(iFirst[1],2),strtrim(iLast[1],2),$
						   strtrim(iFirst[2],2),strtrim(iLast[2],2)], $
					CANCEL=bCancel )
	iFirstTmp = fix( [text[0],text[2],text[4]] )
	iLastTmp = fix( [text[1],text[3],text[5]] )

	if not bCancel then begin
		if (iFirstTmp[0] le ctDims0[0]-1) and (iFirstTmp[0] le iLastTmp[0]) and (iLastTmp[0] ge 0) $
			and (iFirstTmp[1] le ctDims0[1]-1) and (iFirstTmp[1] le iLastTmp[1]) and (iLastTmp[1] ge 0) $
			and (iFirstTmp[2] le ctDims0[2]-1) and (iFirstTmp[2] le iLastTmp[2]) and (iLastTmp[2] ge 0) then begin
			iFirst = iFirstTmp
			iLast = iLastTmp
			bDone = 1b
		endif else begin
			void = dialog_message( 'Invalid range' )
			bDone = 0b
		endelse
	endif else begin
		iFirst = [0,0,0]
		iLast = ctDims0-1
		bDone = 1b
	endelse

endwhile

; Coregister pre and intra scans using chamfer matching
bOk = coreg2PetCtRois( CT1=pCT1, PETFILE1=petDataFile1, $
		PET_FRAMES1=n_elements(times1), PIXDIMS1=ctPixSize1, $
		ROIFILE1=roiDataFile1, ROI_COUNT1=nROIs1, $
		CT2=pCT0, PETFILE2=petDataFile0, $
		PET_FRAMES2=n_elements(times0), PIXDIMS2=ctPixSize1, $
		ROIFILE2=roiDataFile0, ROI_COUNT2=nROIs0, $
		RANGE=[[iFirst],[iLast]], WRITE_PATH=writeFile, DT_FILE=dtFile, $
		ROT=rot, TRANS=trans )

;==============================================================================
;
;	Display images
;
;if bDisplay then begin

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

; Display intra- images
roiMask = bytarr( ctDims1 )
if nROIs1 ne 0 then begin
	openr, lun, /GET_LUN, roiDataFile1
	rois = assoc( lun, bytarr( ctDims1 ) )
	for iROI=0, nROIs1-1 do begin
		roiMask = roiMask or (rois[iROI] gt 0)
	endfor
	close, lun & free_lun, lun
endif
dispImg = bytscl( *pCT1, TOP=254 )

; Contour intra ROIs for display, if any
void = where( roiMask ne 0, count )
if count ne 0 then begin
	for iSlice=0, ctDims1[2]-1 do begin
		slice = roiMask[*,*,iSlice]
		dispSlice = dispImg[*,*,iSlice]
		indices = where( slice ne 0, count )
		if count eq 0 then continue
		contIndices = contour_mask( slice, /INDICES )
		dispSlice[contIndices] = 255b
		dispImg[0,0,iSlice] = dispSlice
	endfor
endif
tv3d, dispImg, RED=255, TITLE='Intra-treatment CT & contours'

close, /ALL

;==============================================================================
;
;	Write images
;
if bWriteImages then begin

	print, 'Writing images for patient ', prePrefix, postPrefix

	folder = dialog_pickfile( TITLE='Select folder to write to', $
			/DIRECTORY, PATH=WRITE_DIR, GET_PATH=writeDir )

	if folder eq '' then begin

		void = dialog_message( 'Data not written.' )

	endif else begin

		; Check for existing files
		fileName = writeDir + prePrefix + '_CT.img'
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

		; Intra-treatment CT
		postCTFileName = writeDir + postPrefix + suffix + '_CT.img'
		openw, fileOutUnit, postCTFileName, /GET_LUN
		writeu, fileOutUnit, (*pCT1)
		close, fileOutUnit & free_lun, fileOutUnit
		fileBase = (strsplit( file_basename( postCTFileName ), ".", /EXTRACT ))[0]
		txtFileName = writeDir + fileBase + "_info.txt"
		openw, txtFileOutUnit, txtFileName, /GET_LUN
		printf, txtFileOutUnit, "Image Info"
		printf, txtFileOutUnit, "=========="
		printf, txtFileOutUnit, "nPx = ", strtrim( ctDims1[0], 2 )
		printf, txtFileOutUnit, "nPy = ", strtrim( ctDims1[1], 2 )
		printf, txtFileOutUnit, "nSlices = ", strtrim( ctDims1[2], 2 )
		printf, txtFileOutUnit, "pixSizeX (mm) = ", strtrim( ctPixSize1[0], 2 )
		printf, txtFileOutUnit, "pixSizeY (mm) = ", strtrim( ctPixSize1[1], 2 )
		printf, txtFileOutUnit, "pixSizeZ (mm) = ", strtrim( ctPixSize1[2], 2 )
		printf, txtFileOutUnit, "type = ", strtrim( size( (*pCT1), /TYPE ) )
		printf, txtFileOutUnit, "type name = ", strtrim( size( (*pCT1), /TNAME ) )
		close, txtFileOutUnit & free_lun, txtFileOutUnit

		; Pre-treatment CT
		preCTFileName = writeDir + prePrefix + suffix + '_CT.img'
		openw, fileOutUnit, preCTFileName, /GET_LUN
		writeu, fileOutUnit, (*pCT0)
		close, fileOutUnit & free_lun, fileOutUnit
		fileBase = (strsplit( file_basename( preCTFileName ), ".", /EXTRACT ))[0]
		txtFileName0 = writeDir + fileBase + '_info.txt'
		openw, txtFileOutUnit, txtFileName0, /GET_LUN
		printf, txtFileOutUnit, "Image Info"
		printf, txtFileOutUnit, "=========="
		printf, txtFileOutUnit, "nPx = ", strtrim( ctDims1[0], 2 )
		printf, txtFileOutUnit, "nPy = ", strtrim( ctDims1[1], 2 )
		printf, txtFileOutUnit, "nSlices = ", strtrim( ctDims1[2], 2 )
		printf, txtFileOutUnit, "pixSizeX (mm) = ", strtrim( ctPixSize1[0], 2 )
		printf, txtFileOutUnit, "pixSizeY (mm) = ", strtrim( ctPixSize1[1], 2 )
		printf, txtFileOutUnit, "pixSizeZ (mm) = ", strtrim( ctPixSize1[2], 2 )
		printf, txtFileOutUnit, "type = ", strtrim( size( (*pCT0), /TYPE ) )
		printf, txtFileOutUnit, "type name = ", strtrim( size( (*pCT0), /TNAME ) )
		printf, txtFileOutUnit, "fixed img = ", strtrim( postCTFileName, 2 )
		line =  "alignment range = " + $
			"[" + strjoin( strtrim( iFirst,2 ) + [replicate( ',', n_elements(iFirst)-1 ), '' ]) + "], " + $
			"[" + strjoin( strtrim( iLast, 2 ) + [replicate( ',', n_elements(iLast)-1 ), '' ]) + "]"
		printf, txtFileOutUnit, line
		line =  "rot (deg) = " + $
			"[" + strjoin( strtrim( rot, 2 ) + [replicate( ',', n_elements(rot)-1 ), '']) + "]"
		printf, txtFileOutUnit, line
		line = "trans (mm) = " + $
			"[" + strjoin( strtrim( trans*ctPixSize1, 2 ) + [replicate( ',', n_elements(trans)-1 ), '']) + "]"
		printf, txtFileOutUnit, line
		close, txtFileOutUnit & free_lun, txtFileOutUnit

		; Intra-treatment PET
		fileName = writeDir + postPrefix + suffix + '_PET.img'
		file_copy, petDataFile1, fileName, /OVERWRITE
		file_delete, petDataFile1
		fileBase = (strsplit( file_basename( fileName ), ".", /EXTRACT ))[0]
		txtFileName0 = writeDir + fileBase + '_info.txt'
		openw, txtFileOutUnit, txtFileName0, /GET_LUN
		printf, txtFileOutUnit, "Image Info"
		printf, txtFileOutUnit, "=========="
		printf, txtFileOutUnit, "nPx = ", strtrim( ctDims1[0], 2 )
		printf, txtFileOutUnit, "nPy = ", strtrim( ctDims1[1], 2 )
		printf, txtFileOutUnit, "nSlices = ", strtrim( ctDims1[2], 2 )
		printf, txtFileOutUnit, "pixSizeX (mm) = ", strtrim( ctPixSize1[0], 2 )
		printf, txtFileOutUnit, "pixSizeY (mm) = ", strtrim( ctPixSize1[1], 2 )
		printf, txtFileOutUnit, "pixSizeZ (mm) = ", strtrim( ctPixSize1[2], 2 )
		printf, txtFileOutUnit, "type = ", strtrim( size( (*pCT1), /TYPE ) )
		printf, txtFileOutUnit, "type name = ", strtrim( size( (*pCT1), /TNAME ) )
		printf, txtFileOutUnit, "nFrames = ", strtrim( n_elements(times0), 2 )
		printf, txtFileOutUnit, "frame times (s) = " + strjoin( strtrim(times1,2), ', ' )
		printf, txtFileOutUnit, "suv scale = " + strjoin( strtrim(petScl1,2), ', ' )
		close, txtFileOutUnit & free_lun, txtFileOutUnit

		; Pre-treatment PET
		fileName = writeDir + prePrefix + suffix + '_PET.img'
		file_copy, petDataFile0, fileName, /OVERWRITE
		file_delete, petDataFile0
		fileBase = (strsplit( file_basename( fileName ), ".", /EXTRACT ))[0]
		txtFileName0 = writeDir + fileBase + '_info.txt'
		openw, txtFileOutUnit, txtFileName0, /GET_LUN
		printf, txtFileOutUnit, "Image Info"
		printf, txtFileOutUnit, "=========="
		printf, txtFileOutUnit, "nPx = ", strtrim( ctDims1[0], 2 )
		printf, txtFileOutUnit, "nPy = ", strtrim( ctDims1[1], 2 )
		printf, txtFileOutUnit, "nSlices = ", strtrim( ctDims1[2], 2 )
		printf, txtFileOutUnit, "pixSizeX (mm) = ", strtrim( ctPixSize1[0], 2 )
		printf, txtFileOutUnit, "pixSizeY (mm) = ", strtrim( ctPixSize1[1], 2 )
		printf, txtFileOutUnit, "pixSizeZ (mm) = ", strtrim( ctPixSize1[2], 2 )
		printf, txtFileOutUnit, "type = ", strtrim( size( (*pCT0), /TYPE ) )
		printf, txtFileOutUnit, "type name = ", strtrim( size( (*pCT0), /TNAME ) )
		printf, txtFileOutUnit, "nFrames = ", strtrim( n_elements(times0), 2 )
		printf, txtFileOutUnit, "frame times (s) = " + strjoin( strtrim(times0,2), ', ' )
		printf, txtFileOutUnit, "suv scale = " + strjoin( strtrim(petScl0,2), ', ' )
		printf, txtFileOutUnit, "fixed img = " + strtrim( postCTFileName, 2 )
		line =  "alignment range = " + $
			"[" + strjoin( strtrim( iFirst,2 ) + [replicate( ',', n_elements(iFirst)-1 ), '' ]) + "], " + $
			"[" + strjoin( strtrim( iLast, 2 ) + [replicate( ',', n_elements(iLast)-1 ), '' ]) + "]"
		printf, txtFileOutUnit, line
		line =  "rot (deg) = " + $
			"[" + strjoin( strtrim( rot, 2 ) + [replicate( ',', n_elements(rot)-1 ), '']) + "]"
		printf, txtFileOutUnit, line
		line = "trans (mm) = " + $
			"[" + strjoin( strtrim( trans*ctPixSize1, 2 ) + [replicate( ',', n_elements(trans)-1 ), '']) + "]"
		printf, txtFileOutUnit, line
		close, txtFileOutUnit & free_lun, txtFileOutUnit

;+++++++++++++++++++++++++++++++++++++++++++++
; ;Uncomment the following lines if you want to use dicom ROIs as well
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		; Intra-treatment ROIs
;		openr, lun, /GET_LUN, roiDataFile1
;		rois = assoc( lun, bytarr( ctDims0 ) )
;		for iROI=0, nROIs1-1 do begin
;			fileName = writeDir + postPrefix + suffix + '_' + roiNames1[iROI] + '.img'
;			openw, fileOutUnit, fileName, /GET_LUN
;			writeu, fileOutUnit, rois[iROI]
;			close, fileOutUnit & free_lun, fileOutUnit
;			fileBase = (strsplit( file_basename( fileName ), ".", /EXTRACT ))[0]
;			txtFileName0 = writeDir + fileBase + '_info.txt'
;			openw, txtFileOutUnit, txtFileName0, /GET_LUN
;			printf, txtFileOutUnit, "Image Info"
;			printf, txtFileOutUnit, "=========="
;			printf, txtFileOutUnit, "nPx = ", strtrim( ctDims1[0], 2 )
;			printf, txtFileOutUnit, "nPy = ", strtrim( ctDims1[1], 2 )
;			printf, txtFileOutUnit, "nSlices = ", strtrim( ctDims1[2], 2 )
;			printf, txtFileOutUnit, "pixSizeX (mm) = ", strtrim( ctPixSize1[0], 2 )
;			printf, txtFileOutUnit, "pixSizeY (mm) = ", strtrim( ctPixSize1[1], 2 )
;			printf, txtFileOutUnit, "pixSizeZ (mm) = ", strtrim( ctPixSize1[2], 2 )
;			printf, txtFileOutUnit, "type = ", strtrim( size( rois[iROI], /TYPE ) )
;			printf, txtFileOutUnit, "type name = ", strtrim( size( rois[iROI], /TNAME ) )
;			close, txtFileOutUnit & free_lun, txtFileOutUnit
;		endfor
;		close, lun & free_lun, lun
;		file_delete, roiDataFile1
;
;		; Pre-treatment ROIs
;		openr, lun, /GET_LUN, roiDataFile0
;		rois = assoc( lun, bytarr( ctDims0 ) )
;		for iROI=0, nROIs0-1 do begin
;			fileName = writeDir + prePrefix + suffix + '_' + roiNames0[iROI] + '.img'
;			openw, fileOutUnit, fileName, /GET_LUN
;			writeu, fileOutUnit, rois[iROI]
;			close, fileOutUnit & free_lun, fileOutUnit
;			fileBase = (strsplit( file_basename( fileName ), ".", /EXTRACT ))[0]
;			txtFileName0 = writeDir + fileBase + '_info.txt'
;			openw, txtFileOutUnit, txtFileName0, /GET_LUN
;			printf, txtFileOutUnit, "Image Info"
;			printf, txtFileOutUnit, "=========="
;			printf, txtFileOutUnit, "nPx = ", strtrim( ctDims1[0], 2 )
;			printf, txtFileOutUnit, "nPy = ", strtrim( ctDims1[1], 2 )
;			printf, txtFileOutUnit, "nSlices = ", strtrim( ctDims1[2], 2 )
;			printf, txtFileOutUnit, "pixSizeX (mm) = ", strtrim( ctPixSize1[0], 2 )
;			printf, txtFileOutUnit, "pixSizeY (mm) = ", strtrim( ctPixSize1[1], 2 )
;			printf, txtFileOutUnit, "pixSizeZ (mm) = ", strtrim( ctPixSize1[2], 2 )
;			printf, txtFileOutUnit, "type = ", strtrim( size( rois[iROI], /TYPE ) )
;			printf, txtFileOutUnit, "type name = ", strtrim( size( rois[iROI], /TNAME ) )
;			printf, txtFileOutUnit, "fixed img = ", strtrim( postCTFileName, 2 )
;			line = "alignment range = " + $
;					"[" + strjoin( strtrim( iFirst,2 ) $
;					+ [replicate( ',', n_elements(iFirst)-1 ), '' ]) + "], " + $
;					"[" + strjoin( strtrim( iLast, 2 ) $
;					+ [replicate( ',', n_elements(iLast)-1 ), '' ]) + "]"
;			printf, txtFileOutUnit, line
;			line = "rot (deg) = " + $
;					"[" + strjoin( strtrim( rot, 2 ) $
;					+ [replicate( ',', n_elements(rot)-1 ), '']) + "]"
;			printf, txtFileOutUnit, line
;			line = "trans (mm) = " + $
;					"[" + strjoin( strtrim( trans*ctPixSize1, 2 ) $
;					+ [replicate( ',', n_elements(trans)-1 ), '']) + "]"
;			printf, txtFileOutUnit, line
;			close, txtFileOutUnit & free_lun, txtFileOutUnit
;		endfor
;		close, lun & free_lun, lun
;		file_delete, roiDataFile0
;+++++++++++++++++++++++++++++++++++++++++++++
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

	endelse

endif

ptr_free, pCT0, pCT1

beep
print, 'Done align_pre2post_dynamic_petctrois'

end ; of align_pre2post_dynamic_petctrois