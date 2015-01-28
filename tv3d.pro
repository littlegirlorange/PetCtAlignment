pro tv3d, stack, $
	DIM=dim, $
	IMGSIZEX=imgSizeX, $
	COLS=nDispCols, ROWS=nDispRows, $
	CTABLE=cTable, $
	TITLE=title, $
	RED=redVal, $
	WIDGET=wBase

if n_elements(dim) eq 0 then dim=2 ; z dim by default
if n_elements(cTable) eq 0 then cTable = 0 ; gscale by default
if n_elements(title) eq 0 then title = 'Images'

nRedIndices = 0
if n_elements(redVal) ne 0 then begin
	redIndices = where( stack eq redVal, nRedIndices )
endif

dims = size( stack, /DIM )
if n_elements(dims) le 2 then return

case dim of
2: begin
	x = dims[0]
	y = dims[1]
end
1: begin
	x = dims[0]
	y = dims[2]
end
0: begin
	x = dims[1]
	y = dims[2]
end
else:
endcase

; Remember colour table
tvlct, rOrig, gOrig, bOrig, /GET
device, DECOMPOSED=0
loadct, cTable, /SILENT
topClr = !D.TABLE_SIZE-1

; Set the top color to red for annot display
tvlct, 255, 0, 0, topClr	; red

; Determine display geometry
maxWinSize = get_screen_size()
maxWinSize[0] -= 12
maxWinSize[1] -= 24

if n_elements(imgSizeX) eq 0 then begin
	if n_elements(nDispCols) eq 0 then nDispCols = 8
endif else begin
	nDispCols = fix(maxWinSize[0]/float(imgSizeX))
endelse
if n_elements(nDispCols) eq 0 then nDispCols = 8
if n_elements(nDispRows) eq 0 then nDispRows = 8

nRealCols = nDispCols
nRealRows = ceil( float(dims[dim]) / nDispCols )

dispImgSizeX = maxWinSize[0] / nDispCols
dispImgSizeY = dispImgSizeX * y/x
dispWinX = maxWinSize[0]
dispWinY = maxWinSize[1]
realX = nRealCols * dispImgSizeX
realY = nRealRows * dispImgSizeY

; Create a scrollable draw widget
wBase = widget_base( TITLE=title, /COL )
wDraw = widget_draw( wBase, $
					  XSIZE=realX, YSIZE=realY, $
					  /SCROLL, $
					  X_SCROLL_SIZE=dispWinX, Y_SCROLL_SIZE=dispWinY )

widget_control, wBase, /REALIZE

; Scale images
case dim of
2: begin
	dispSeries = congrid( stack, dispImgSizeX, dispImgSizeY, dims[dim] )
	if n_elements(redVal) ne 0 then begin
		redSeries = congrid( (stack eq redVal)*255b, dispImgSizeX, dispImgSizeY, dims[dim] )
	endif
end
1: begin
	dispSeries = congrid( stack, dispImgSizeX, dims[dim], dispImgSizeY )
	if n_elements(redVal) ne 0 then begin
		redSeries = congrid( (stack eq redVal)*255b, dispImgSizeX, dims[dim], dispImgSizeY )
	endif
end
0: begin
	dispSeries = congrid( stack, dims[dim], dispImgSizeX, dispImgSizeY )
	if n_elements(redVal) ne 0 then begin
		redSeries = congrid( (stack eq redVal)*255b, dims[dim], dispImgSizeX, dispImgSizeY )
	endif
end
else:
endcase

dispSeries = bytscl( dispSeries, TOP=topClr-1 )
if n_elements(redVal) ne 0 then begin
	redIndices = where( redSeries ge (255.0/27.0), count )
	if count gt 0 then dispSeries[redIndices] = 255b
endif

; Draw
case dim of
2: begin
	for i=0, dims[dim]-1 do begin
		tv, dispSeries[*,*,i], i
		xLoc = (i mod nRealCols)*dispImgSizeX + 1
		yLoc = dispImgSizeY*nRealRows - (i/nRealCols)*dispImgSizeY - 10
		xyouts, xLoc, yLoc, $
				strtrim(i, 2), $
				CHARSIZE=1, $
				COLOR=topClr, $
				/DEVICE
	endfor
end
1: begin
	for i=0, dims[dim]-1 do begin
		tv, reform(dispSeries[*,i,*]), i
		xLoc = (i mod nRealCols)*dispImgSizeX + 1
		yLoc = dispImgSizeY*nRealRows - (i/nRealCols)*dispImgSizeY - 10
		xyouts, xLoc, yLoc, $
				strtrim(i, 2), $
				CHARSIZE=1, $
				COLOR=topClr, $
				/DEVICE
	endfor
end
0: begin
	for i=0, dims[dim]-1 do begin
		tv, reform(dispSeries[i,*,*]), i
		xLoc = (i mod nRealCols)*dispImgSizeX + 1
		yLoc = dispImgSizeY*nRealRows - (i/nRealCols)*dispImgSizeY - 10
		xyouts, xLoc, yLoc, $
				strtrim(i, 2), $
				CHARSIZE=1, $
				COLOR=topClr, $
				/DEVICE
	endfor
end
else:
endcase

; Restore orig colour table
tvlct, rOrig, gOrig, bOrig

end