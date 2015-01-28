;==============================================================================
;
;	Method:		alignDataSets
;
;	Description:
;				Rebins and crops a data array according to the size and
;				dimensions of a second data array.
;
;	Version:
;				080627 First working version (for Huan)
;
;				080729 Modified for Mike's dynamic PET data
;
;	Inputs:
;
;	Outputs:
;
;	Required Modules:
;
;	Written by: Maggie Kusano, June 27, 2008
;
;==============================================================================

function alignDataSets, $
		DATA1		= pData1, $
		DATAFILE1	= dataFile1, $
		DIMS1		= dims1, $
		TYPE1		= type1, $
		PIXSIZE1	= pixSize1, $
		POSITION1	= pos1, $
		DATA2		= pData2, $
		DATAFILE2	= dataFile2, $
		DIMS2		= dims2, $
		TYPE2		= type2, $
		PIXSIZE2	= pixSize2, $
		POSITION2	= pos2, $
		CENTER		= bCenter, $
		FIRST		= bFirst, $
		WRITE_DIR	= writeDir, $
		READ_DIR	= readDir, $
		NN			= bNN, $
		DISPLAY		= bDisplay

; Make sure required parameters are defined
if (not keyword_set(pixSize1)) or (not keyword_set(pixSize2)) then return, 0b
if (not keyword_set(pData1)) and (not keyword_set(dataFile1)) then return, 0b
if (not keyword_set(pData2)) and (not keyword_set(dataFile2)) then return, 0b
if (keyword_set(pos1) eq 0) or (keyword_set(pos2) eq 0) then begin
	if (not keyword_set(bCenter)) and (not keyword_set(bFirst)) then begin
		bCenter = 1b
	endif
endif else begin
	bPos = 1b
endelse
if (keyword_set(bCenter)) or (keyword_set(bFirst)) then bPos=0b
if keyword_set(pData1) then begin
	type1 = size( *pData1, /TYPE )
endif else begin
	if n_elements( type1 ) eq 0 then type1 = 2
endelse
if keyword_set(pData2) then begin
	type2 = size( *pData2, /TYPE )
endif else begin
	if n_elements( type2 ) eq 0 then type2 = 2
endelse

if n_elements( dataFile1 ) eq 0 then begin
	dims1 = size( *pData1, /DIM )
	type1 = size( *pData1, /TYPE )
endif else begin
	openu, lun1, /GET_LUN, dataFile1
	data1 = assoc( lun1, make_array( dims1[0], dims1[1], dims1[2], TYPE=type1 ) )
endelse
if n_elements( dataFile2 ) eq 0 then begin
	dims2 = size( *pData2, /DIM )
	type2 = size( *pData2, /TYPE )
	tmpDims2 = dims2
	tmpDims2[0:2] = dims1[0:2]
	tmpData2 = make_array( tmpDims2, TYPE=type2 )
endif else begin
	openu, lun2, /GET_LUN, dataFile2
	data2 = assoc( lun2, make_array( dims2[0], dims2[1], dims2[2], TYPE=type2 ) )
	tmpDataFile2 = dataFile2 + '.tmp2'
	openw, tmpLun2, /GET_LUN, tmpDataFile2
	tmpData2 = assoc( tmpLun2, make_array( dims1, TYPE=type2 ) )
endelse

nFrames = 1
if n_elements(dims2) gt 3 then begin
	nFrames = dims2[3]
	dims2 = dims2[0:2]
endif

; Calculate BLHC offset
if keyword_set( bPos ) then begin
	offset = pos1[*,0] - pos2[*,0]
endif else if keyword_set( bCenter ) then begin
	offset = -(dims1*pixSize1 - dims2*pixSize2)/2
endif else begin
	offset = -(dims1*pixSize1 - dims2*pixSize2)/2
	offset[2] = 0.0
endelse

; Convert to data2 pix
offsetInData2Pix = offset / pixSize2

; Convert data2 to data1-sized pixels
xLocs = findgen( dims1[0] )*pixSize1[0]/pixSize2[0] + offsetInData2Pix[0]
yLocs = findgen( dims1[1] )*pixSize1[1]/pixSize2[1] + offsetInData2Pix[1]
zLocs = findgen( dims1[2] )*pixSize1[2]/pixSize2[2] + offsetInData2Pix[2]
if keyword_set(bNN) then begin
	xLocs = round( xLocs )
	yLocs = round( yLocs )
	zLocs = round( zLocs )
endif
if nFrames gt 1 then begin
	for iFrame=0, nFrames-1 do begin
		if n_elements( dataFile2 ) eq 0 then begin
			tmpData2[*,*,*,iFrame] = interpolate( (*pData2)[*,*,*,iFrame], $
					xLocs, yLocs, zLocs, /GRID, MISSING=0 )
		endif else begin
			tmpData2[iFrame] = interpolate( data2[iFrame], $
					xLocs, yLocs, zLocs, /GRID, MISSING=0 )
		endelse
	endfor
endif else begin
	if n_elements( dataFile2 ) eq 0 then begin
		tmpData2 = interpolate( *pData2, xLocs, yLocs, zLocs, /GRID, MISSING=0 )
	endif else begin
		tmpData2[0] = interpolate( data2[0], xLocs, yLocs, zLocs, /GRID, MISSING=0 )
	endelse
endelse

if n_elements( dataFile2 ) eq 0 then begin
	ptr_free, pData2
	pData2 = ptr_new( tmpData2, /NO_COPY )
	tmpData2 = 0
endif else begin
	close, lun2
	close, tmpLun2
	free_lun, lun2, tmpLun2
	file_delete, dataFile2
	wait, 3
	file_move, tmpDataFile2, dataFile2
endelse

;==============================================================================
;
;	Display the data (with contours)
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
nRealRows = ceil( float(dims1[2]) / nDispCols )

dispImgSize = maxWinSize / nDispCols
dispWinX = dispImgSize * nDispCols
dispWinY = dispImgSize * nDispRows
realX = nRealCols * dispImgSize
realY = nRealRows * dispImgSize

; Create a scrollable draw widget
wBase1 = widget_base( TITLE='Data1 Images' )
wDraw1 = widget_draw( wBase1, $
					  XSIZE=realX, YSIZE=realY, $
					  /SCROLL, $
					  X_SCROLL_SIZE=dispWinX, Y_SCROLL_SIZE=dispWinY )

widget_control, wBase1, /REALIZE

; Scale images and ROIs to fit the display and superimpose ROIs on data1
if n_elements( dataFile1 ) eq 0 then begin
	dispSeries = congrid( *pData1, dispImgSize, dispImgSize, dims1[2] )
endif else begin
	dispSeries = congrid( data1[0], dispImgSize, dispImgSize, dims1[2] )
endelse
dispSeries = bytscl( dispSeries, TOP=topClr-2 )

; Draw
for i=0, dims1[2]-1 do begin
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
wBase2 = widget_base( TITLE='Data2 Images (in Data1 space)' )
wDraw2 = widget_draw( wBase2, $
					  XSIZE=realX, YSIZE=realY, $
					  /SCROLL, $
					  X_SCROLL_SIZE=dispWinX, Y_SCROLL_SIZE=dispWinY )

widget_control, wBase2, /REALIZE

; Scale images and ROIs to fit the display and superimpose ROIs on data2
if n_elements( dataFile2 ) eq 0 then begin
	if nFrames gt 1 then begin
		dispSeries = congrid( (*pData2)[*,*,*,0], dispImgSize, dispImgSize, dims1[2] )
	endif else begin
		dispSeries = congrid( (*pData2), dispImgSize, dispImgsize, dims1[2] )
	endelse
endif else begin
	dispSeries = congrid( data2[0], dispImgSize, dispImgsize, dims1[2] )
endelse
dispSeries = bytscl( dispSeries, TOP=topClr-2 )

; Draw
for i=0, dims1[2]-1 do begin
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

	;-------------------------Needs to be fixed/updated-------------------------

	; Write data2 files
	ctFileOutName = dialog_pickfile( /WRITE, FILE=file, /OVERWRITE_PROMPT, $
			DEFAULT_EXTENSION='img', GET_PATH=writeDir )
	if ctFileOutName ne '' then begin
		openw, ctFileOutUnit, ctFileOutName, /GET_LUN
		writeu, ctFileOutUnit, *pCT
		free_lun, ctFileOutUnit


		; Write text file info
		fileBase = (strsplit( file_basename( ctFileOutName ), ".", /EXTRACT ))[0]
		txtFileOutName = writeDir + fileBase + "_info.txt"
		openw, txtFileOutUnit, txtFileOutName, /GET_LUN
		printf, txtFileOutUnit, "Image Info"
		printf, txtFileOutUnit, "=========="
		printf, txtFileOutUnit, "nPx = ", strtrim( ctDims[0], 2 )
		printf, txtFileOutUnit, "nPy = ", strtrim( ctDims[1], 2 )
		printf, txtFileOutUnit, "nSlices (this array) = ", strtrim( ctDims[2], 2 )
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
		openw, petFileOutUnit, petFileOutName, /GET_LUN
		writeu, petFileOutUnit, *pPET
		free_lun, petFileOutUnit

		; Write text file info
		fileBase = (strsplit( file_basename( petFileOutName ), '.', /EXTRACT ))[0]
		txtFileOutName = writeDir + fileBase + "_info.txt"
		openw, txtFileOutUnit, txtFileOutName, /GET_LUN
		printf, txtFileOutUnit, "Image Info"
		printf, txtFileOutUnit, "=========="
		printf, txtFileOutUnit, "nPx = ", strtrim( ctDims[0], 2 )
		printf, txtFileOutUnit, "nPy = ", strtrim( ctDims[1], 2 )
		printf, txtFileOutUnit, "nSlices (this array) = ", strtrim( ctDims[2], 2 )
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

return, 1b

end ; of align_pet2ct