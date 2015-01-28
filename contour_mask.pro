function contour_mask, $
	mask, $
	LEVEL=level, $
	COORDS=bCoords, $
	ROIS=bROIs, $
	INDICES=bIndices, $
	LARGEST_REGION=bLargestRegion, $
	SIZE_THRESHOLD=minArea, $
	NROIS=nContours

if keyword_set( level ) eq 0b then level = max( mask )
if keyword_set( minArea ) eq 0b then minArea = 0

; Contour the mask
contour, mask, LEVELS=level, PATH_INFO=pathInfo, PATH_XY=pathXY, $
        /PATH_DATA_COORDS
if n_elements( pathInfo ) eq 0 then $
	return, -1L

; Select all exterior contours
iids = where( pathInfo[*].high_low eq 1, nContours )
if nContours eq  0 then $
	return, -1L

if keyword_set(bROIs) then begin
	; Return a ROI group
	oROIGroup = obj_new( 'IDLgrROIGroup' )
endif else if keyword_set(bCoords) then begin
	; Return the vertex coordinates of the largest contour
	largestContourData = -1L
	largestArea = 0
endif else if keyword_set(bLargestRegion) then begin
	largestRegion = mask * 0b
	largestArea = 0
endif else begin
	; Return the vertex indices of all contours
	contouredMask = mask * 0b
end

for i=0, nContours-1 do begin

    contId = iids[i]
    iStart = pathInfo[contId].offset
    iFinish = iStart + pathInfo[contId].n - 1

    ; Grab the contour path.
    tmpContourData = FLTARR( 2,pathInfo[contId].n )
    tmpContourData[0,*] = pathXY[0, iStart:iFinish]
    tmpContourData[1,*] = pathXY[1, iStart:iFinish]

    ; Create a temp ROI object
    oROI = obj_new( 'IDLgrROI', tmpContourData, TYPE=2 )

	if keyword_set(bROIs) then begin
		; Remember this ROI
		oROIGroup->add, oROI
	endif else if keyword_set(bCoords) then begin
		; Look for the largest ROI
		bOk = oROI->computeGeometry( AREA=area )
		if abs(area) gt largestArea then begin
			largestArea = abs(area)
			largestContourData = tmpContourData
		endif
		if obj_valid( oROI ) then obj_destroy, oROI
	endif else if keyword_set(bLargestRegion) then begin
		;Look for the largest ROI
		bOk = oROI->computeGeometry( AREA=area )
		if abs(area) gt largestArea then begin
			largestArea = abs(area)
			largestRegion = oROI->computeMask( DIM=size( mask, /DIM ), MASK_RULE=2 )
		endif
		if obj_valid( oROI ) then obj_destroy, oROI
	endif else begin
		bOk = oROI->computeGeometry( AREA=area )
		if abs(area) gt minArea then begin
			contouredMask = contouredMask or $
					oROI->computeMask( DIM=size( mask, /DIM ), MASK_RULE=0 )
		endif
	    ; Delete the temp ROI object
    	if obj_valid( oROI ) then obj_destroy, oROI
    endelse

endfor

if keyword_set(bCoords) then begin
	return, largestContourData
endif else if keyword_set(bROIs) then begin
	return, oROIGroup
endif else if keyword_set(bLargestRegion) then begin
	return, largestRegion
endif else begin
	contourIndices = where( contouredMask ne 0 )
	return, contourIndices
endelse

end ; of contour_mask