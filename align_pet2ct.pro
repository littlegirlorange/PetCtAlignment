;==============================================================================
;
;	Method:		align_pet2ct
;
;	Description:
;				Reads DICOM format PET and CT images for a patient, resamples
;				and aligns the PET to the CT and returns the data as integer arrays
;
; Inputs:
;
;
; Outputs:
;
;
; Required Modules:
;       textbox.pro
; 
; Written By:
;       Maggie Kusano, August 10, 2004
;       
;	Version:
;				040810 align_pet_ct
;				Implemented DICOM read of PET and CT images.
;				Some CT DICOM files contain more than one image (a 64x64 and a
;				512x512).  Needed to add code to check for multiple images.
;				Currently, the largest of the bunch is used for further
;				processing.
;
;				040811
;				Sorted images according to image number since they may not
;				be correctly ordered when read by IDL.
;				Implemented basic algorithm based on Image Position information
;				contained in the DICOM header.
;
;				040817
;				First fully functional version.
;				Discovered that the PET and CT images are aligned at the centre
;				(e.g. the centre of the 3D arrays are aligned), so the pixel
;				values just need to be interpolated from PET-size to CT size
;				and then the data needs to be padded or trimmed around the
;				edges depending on whether the FOV of the PET data is smaller
;				or bigger than the CT FOV.
;				The Image Position information in the DICOM header is of no use.
;				Only need the number of rows, columns, and slices and the pixel
;				size and spacing to calculate the conversion.
;				Appears that IDL/my computer is only able to handle two large
;				image arrays at once.  Unused arrays must be cleared (set to 0)
;				to avoid crashes.
;
;				040818
;				Cleaned up code.
;
;				041117
;				Made image write optional.  If the user opts to save the image
;				data, he/she may select a sub-range of images slices to save.
;
;				041123 get_tumour_slices
;				Hacked the code so that the user is able to read/write CT images
;				only (instead of PET/CT data).  No alignment is performed.  The
;				DICOM file is read and a subset of the data array is saved.
;
;				050404 get_pet_ct_bkgd
;				Back to saving PET/CT data.  The program will suggest a slice range
;				based on DICOM RS contours (converted to int arrays by Ananth's
;				ICAS program).  Otherwise, the user can select a subrange of data
;				to save.
;
;				060118
;				Version to handle Huan's PET/CT head and neck data.  Can no longer
;				use instance number (0020 0013) to order the CT images since
;				the numbers are not unique.  Now use the third field (z) of
;				image position (0020 0032) to order both the PET and CT data.
;				Also, the slice spacing of the H&N data is not uniform, so this
;				new version interpolates the image position data into uniformly
;				spaced slices, distanced by the smallest slice spacing (usually
;				1.6 mm).
;
;				060315
;				New data now has uniform slice spacing so ditched interpolation.
;				Changed array storage to use HD space instead of RAM so that
;				both the PET and CT can be kept in memory at the same time.
;				This was done so that slice range selection can be done at the
;				end, after the user has seen both the PET and the CT data.
;				No longer reverse the arrays along Z so the slices run inf-sup,
;				consistent with the DICOM data and ICAS output.
;				The user is now prompted for each file saved (PET, CT and ROI)
;				so that alternative file names can be chosen.
;				Fixed contour display so that all contours are shown, not just
;				the largest for each slice (ditched contour_mask).
;				Finally works for Huan's data, the specifications for which are:
;					CT	- Image Type = DERIVED\SECONDARY\AXIAL
;						  (3.2 mm slice spacing)
;					PET - Image Type = DERIVED\PRIMARY
;						  (resampled PETs)
;
;				060321 align_pet_ct
;
;				060901 align_ct2pet
;				
;				130724 Added nearerst neighbour interpolation option for PET sampling.
;
;==============================================================================

pro align_pet2ct, $
		PET_IMG		= pPET, $
		PET_FILE	= petFile, $
		PET_DIMS	= petDims, $
		PET_PIXSIZE	= petPixSize, $
		PET_POS		= petPos, $
		PET_TYPE	= petType, $
		CT_IMG		= pCT, $
		CT_FILE		= ctFile, $
		CT_DIMS		= ctDims, $
		CT_PIXSIZE	= ctPixSize, $
		CT_POS		= ctPos, $
		FIRST		= bFirst, $
		CENTER		= bCenter, $
		NN        = bNN, $
		WRITE_DIR	= writeDir, $
		READ_DIR	= readDir, $
		DISPLAY		= bDisplay

catch, theError
if theError ne 0 then begin
   catch, /CANCEL
   ok = dialog_message(!Error_State.Msg)
   return
endif

; Make sure required parameters are defined
if (not keyword_set(petPixSize)) or (not keyword_set(ctPixSize)) then return
if (not keyword_set(pPET)) and (not keyword_set(petFile)) then return
if (not keyword_set(pCT)) and (not keyword_set(ctFile)) then return
if (keyword_set(petPos) eq 0) or (keyword_set(ctPos) eq 0) then begin
	if (not keyword_set(bCenter)) and (not keyword_set(bFirst)) then begin
		bCenter = 1b
	endif
endif else begin
	bPos = 1b
endelse
if (keyword_set(bCenter)) or (keyword_set(bFirst)) then bPos = 0b
if not keyword_set(bNN) then bNN = 0b
if n_elements( petType ) eq 0 then petType = 2

if n_elements( petFrame ) eq 0 then petFrame = 0

if n_elements( ctFile ) eq 0 then begin
	; We've been passed data. Determine the data dimensions.
	info = size( *pCT, /STRUCT )
	ctDims = info.dimensions
	ct = *pCT
endif else begin
	; We've been passed a file and data dimensions.  Get the data.
	openu, ctLun, /GET_LUN, ctFile
	ct = assoc( ctLun, intarr( ctDims[0], ctDims[1], ctDims[2] ) )
endelse

if n_elements( petFile ) eq 0 then begin
	petDims = size( *pPET, /DIM )
	if n_elements(petDims) eq 4 then begin
		tmpPet = intarr( ctDims[0], ctDims[1], ctDims[2], petDims[3] )
	endif else begin
		tmpPet = intarr( ctDims[0], ctDims[1], ctDims[2], 1 )
	endelse
endif else begin
	openu, petLun, /GET_LUN, petFile
	pet = assoc( petLun, make_array( petDims[0], petDims[1], petDims[2], TYPE=petType ) )
	tmpPetFile = petfile+'.tmp2'
	openw, tmpPetLun, /GET_LUN, tmpPetFile
	tmpPet = assoc( tmpPetLun, make_array( ctDims[0], ctDims[1], ctDims[2], TYPE=petType ) )
endelse

if n_elements(petDims) eq 4 then begin
	nPETFrames = petDims[3]
endif else begin
	nPETFrames = 1
	petDims = [petDims,1]
endelse

; Calculate BLHC offset
if keyword_set( bPos ) then begin
	offset = ctPos[*,0] - petPos[*,0]
endif else if keyword_set( bCenter ) then begin
	offset = -(ctDims*ctPixSize - petDims[0:2]*petPixSize)/2
endif else begin
	offset = -(ctDims*ctPixSize - petDims[0:2]*petPixSize)/2
	offset[2] = 0.0
endelse

; Convert to PET pix
offsetInPetPix = offset / petPixSize

; Convert PET to CT-sized pixels
; If x or y offset gt 0, CT is inside PET -> want +offset
; If z offset is gt 0, CT is outside PET -> want -offset
xLocs = findgen( ctDims[0] )*ctPixSize[0]/petPixSize[0] + offsetInPetPix[0]
yLocs = findgen( ctDims[1] )*ctPixSize[1]/petPixSize[1] + offsetInPetPix[1]
zLocs = findgen( ctDims[2] )*ctPixSize[2]/petPixSize[2] + offsetInPetPix[2]
if keyword_set(bNN) then begin
  xLocs = round(xLocs)
  yLocs = round(yLocs)
  zLocs = round(zLocs)
endif
for iFrame=0, nPETFrames-1 do begin
	if n_elements( petFile ) eq 0 then begin
		tmpPet[*,*,*,iFrame] = interpolate( temporary( (*pPET)[*,*,*,iFrame] ), $
				xLocs, yLocs, zLocs, /GRID, MISSING=0 )
	endif else begin
		frame = pet[iFrame]
		tmpPet[iFrame] = interpolate( temporary( frame ), $
				xLocs, yLocs, zLocs, /GRID, MISSING=0 )
	endelse
endfor

if n_elements( petFile ) eq 0 then begin
	ptr_free, pPET
	pPET = ptr_new( reform(tmpPet), /NO_COPY )
endif else begin
	close, petLun
	close, tmpPetLun
	free_lun, petLun, tmpPetLun
	file_delete, petfile
	wait, 3
	file_move, tmpPetFile, petFile
endelse

tmpPet = 0

;==============================================================================
;
;	Display the PET and CT data (with contours)
;

if keyword_set( bDisplay ) then begin

print, 'Displaying images...'

; Reset display and load grayscale colour table
device, DECOMPOSED=0
loadct, 0, /SILENT
topClr = !D.TABLE_SIZE-1

; Set the top colors to red and green for ROI display
tvlct, 255, 0, 0, topClr	; red
tvlct, 0, 255, 0, topClr-1	; green

; Determine display geometry
scrnSize = get_screen_size()
maxWinSize = scrnSize[1]*0.9
nDispCols = 5
nDispRows = 5
nRealCols = nDispCols
nRealRows = ceil( float(petDims[2]) / nDispCols )

dispImgSize = maxWinSize / nDispCols
dispWinX = dispImgSize * nDispCols
dispWinY = dispImgSize * nDispRows
realX = nRealCols * dispImgSize
realY = nRealRows * dispImgSize

; Create a scrollable draw widget
wBase1 = widget_base( TITLE='CT Images' )
wDraw1 = widget_draw( wBase1, $
					  XSIZE=realX, YSIZE=realY, $
					  /SCROLL, $
					  X_SCROLL_SIZE=dispWinX, Y_SCROLL_SIZE=dispWinY )

widget_control, wBase1, /REALIZE

; Scale images and ROIs to fit the display and superimpose ROIs on CT
dispSeries = congrid( ct, dispImgSize, dispImgSize, ctDims[2] )
dispSeries = bytscl( dispSeries, TOP=topClr-2 )

; Draw
for i=0, ctDims[2]-1 do begin
	tv, dispSeries[*,*,i], i
	xLoc = (i mod nRealCols)*dispImgSize + 1
	yLoc = dispImgSize*nRealRows - (i/nRealCols)*dispImgSize - 10
	xyouts, xLoc, yLoc, $
			strtrim(i, 2), $
			CHARSIZE=1, $
			COLOR=topClr, $
			/DEVICE
endfor

; Delete the dispSeries and dispRoiSeries to clear memory but save the indices
dispSeries = 0
dispRoiSeries = 0
roiBoundsArray = 0

; Create a scrollable draw widget
wBase2 = widget_base( TITLE='PET Images (in CT space)' )
wDraw2 = widget_draw( wBase2, $
					  XSIZE=realX, YSIZE=realY, $
					  /SCROLL, $
					  X_SCROLL_SIZE=dispWinX, Y_SCROLL_SIZE=dispWinY )

widget_control, wBase2, /REALIZE

; Scale images and ROIs to fit the display and superimpose ROIs on PET
dispSeries = congrid( frame, dispImgSize, dispImgSize, ctDims[2] )
dispSeries = bytscl( dispSeries, TOP=topClr-2 )

; Draw
for i=0, ctDims[2]-1 do begin
	tv, dispSeries[*,*,i], i
	xLoc = (i mod nRealCols)*dispImgSize + 1
	yLoc = dispImgSize*nRealRows - (i/nRealCols)*dispImgSize - 10
	xyouts, xLoc, yLoc, $
			strtrim(i, 2), $
			CHARSIZE=1, $
			COLOR=topClr, $
			/DEVICE
endfor

; Clear memory
dispSeries = 0

endif ; bDisplay

;==============================================================================
;
;	Write to files for further analysis
;

if keyword_set( writeDir ) then begin

	print, 'Writing data...'

	firstSlice = 0
	lastSlice = ctDims[2]-1

		bOk = 0b
		while not bOk do begin

			; Prompt user to select slice range to save
			text = textbox( TITLE='Enter slice range to save', $
								LABELARRAY=['First slice:', 'Last slice:'], $
								VALUEARRAY=['0', strtrim(ctDims[2]-1, 2)], $
								CANCEL=cancel )

			if cancel ne 1 then begin
				firstSlice = uint( text[0] )
				lastSlice = uint( text[1] )

				if ( firstSlice le lastSlice ) and $
				   ( firstSlice ge 0 ) and ( firstSlice le (ctDims[2]-1) ) and $
				   ( lastSlice ge 0 ) and (lastSlice le (ctDims[2]-1) ) then begin
				   bOk = 1b
				endif
			endif else begin
				bOk = 1b
			endelse

		endwhile

	; Write CT file
	file = WRITE_DIR + ctPatName + "_" + ctStudyDate + "_ct.img"
	ctFileOutName = dialog_pickfile( /WRITE, FILE=file, /OVERWRITE_PROMPT, $
			DEFAULT_EXTENSION='img', GET_PATH=writeDir )
	if ctFileOutName ne '' then begin
		openw, ctFileOutUnit, ctFileOutName, /GET_LUN
		writeu, ctFileOutUnit, ct
		free_lun, ctFileOutUnit


		; Write text file info
		fileBase = (strsplit( file_basename( ctFileOutName ), ".", /EXTRACT ))[0]
		txtFileOutName = writeDir + fileBase + "_info.txt"
		openw, txtFileOutUnit, txtFileOutName, /GET_LUN
		printf, txtFileOutUnit, "Image Info"
		printf, txtFileOutUnit, "=========="
		printf, txtFileOutUnit, "nPx = ", strtrim( ctDims[0], 2 )
		printf, txtFileOutUnit, "nPy = ", strtrim( ctDims[1], 2 )
		printf, txtFileOutUnit, "nSlices = ", strtrim( ctDims[2], 2 )
		printf, txtFileOutUnit, "pixSizeX (mm) = ", strtrim( ctPixSize[0], 2 )
		printf, txtFileOutUnit, "pixSizeY (mm) = ", strtrim( ctPixSize[1], 2 )
		printf, txtFileOutUnit, "pixSizeZ (mm) = ", strtrim( ctPixSize[2], 2 )
		printf, txtFileOutUnit, "first slice relative to original CT series = ", strtrim( firstSlice, 2 )
		printf, txtFileOutUnit, "last slice relative to original CT series = ", strtrim( lastSlice, 2 )
		free_lun, txtFileOutUnit
	endif else begin
		writeDir = WRITE_DIR
		void = dialog_message( 'CT file not selected.  Data not written.' )
	endelse

	; Write new PET file
	file = writeDir + petPatName + "_" + petStudyDate + "_pet.img"
	petFileOutName = dialog_pickfile( /WRITE, FILE=file, /OVERWRITE_PROMPT, $
			DEFAULT_EXTENSION='img', GET_PATH=writeDir )
	if petFileOutName ne '' then begin

		if n_elements( petFile ) eq 0 then begin
			openw, petFileOutUnit, petFileOutName, /GET_LUN
			writeu, petFileOutUnit, *pPET
			free_lun, petFileOutUnit
		endif else begin
			file_copy, petFile, petFileOutName
		endelse

		; Write text file info
		fileBase = (strsplit( file_basename( petFileOutName ), '.', /EXTRACT ))[0]
		txtFileOutName = writeDir + fileBase + "_info.txt"
		openw, txtFileOutUnit, txtFileOutName, /GET_LUN
		printf, txtFileOutUnit, "Image Info"
		printf, txtFileOutUnit, "=========="
		printf, txtFileOutUnit, "nPx = ", strtrim( ctDims[0], 2 )
		printf, txtFileOutUnit, "nPy = ", strtrim( ctDims[1], 2 )
		printf, txtFileOutUnit, "nSlices = ", strtrim( ctDims[2], 2 )
		printf, txtFileOutUnit, "nFrames = ", strtrim( petDims[3], 2 )
		printf, txtFileOutUnit, "pixSizeX (mm) = ", strtrim( ctPixSize[0], 2 )
		printf, txtFileOutUnit, "pixSizeY (mm) = ", strtrim( ctPixSize[1], 2 )
		printf, txtFileOutUnit, "pixSizeZ (mm) = ", strtrim( ctPixSize[2], 2 )
		printf, txtFileOutUnit, "first slice relative to original CT series = ", strtrim( firstSlice, 2 )
		printf, txtFileOutUnit, "last slice relative to original CT series = ", strtrim( lastSlice, 2 )
		free_lun, txtFileOutUnit
	endif else begin
		void = dialog_message( 'PET file not selected.  Data not written.' )
	endelse

endif ; bWriteImages

print, "Done" & beep

end ; of align_pet2ct