function getPetCtRois, $
		PET_FILE	= petFile, $
		PET_DATA	= pPET, $
		PET_ASSOC_FILE = petDataFile, $
		PET_DIMS	= petDims, $
		PET_ORIG_DIMS = petOrigDims, $
		PET_ORIG_PIXSIZE = petOrigPixSize, $
		PET_PIXSIZE	= petPixSize, $
		PET_FRAMES	= nPETFrames, $
		PET_FRAME_TIMES	= petTimes, $
		PET_POS		= petPos, $
		PET_SCL		= petScl, $
		CT_FILE		= ctFile, $
		CT_DATA		= pCT, $
		CT_DIMS		= ctDims, $
		CT_PIXSIZE	= ctPixSize, $
		CT_POS		= ctPos, $
		ALL_ROIS	= allROIs, $
		ROI_FILE	= rsFile, $			; in
		ROI_DATA	= pROIs, $			; out
		ROI_ASSOC_FILE = roiAssocFile, $	; out
		ROI_NAMES	= roiNames, $		; out
		ROI_COUNT	= nRois, $			; out
		COREG2PET	= bCoreg2Pet, $
		COREG2CT	= bCoreg2Ct, $
		FIRST		= bFirst, $
		CENTER		= bCenter, $
		PATNAME		= patName, $
		STUDYDATE	= studyDate, $
		WEIGHT		= weight, $
		DISPLAY		= bDisplay

; Get CT data
bDicom = query_dicom( ctFile )
if bDicom then begin
	bOk = dcm2rawNonUniform( FILE=ctFile, DATA=pCT, DIMS=ctDims, PIXSIZE=ctPixSize, $
		POS=ctPos, PATNAME=patName, STUDYDATE=studyDate )
endif else begin
	bOk = gemini2raw( FILE=ctFile, DATA=pCT, DIMS=ctDims, PIXSIZE=ctPixSize, $
			POS=ctPos, PATNAME=patName, STUDYDATE=studyDate )
endelse
if total( *pCT ) eq 0 then return, 0b
print, "CT patient name: " + strtrim( patName, 2 )
print, "CT study date: " + strtrim( studyDate, 2 )

; Get RS data
if rsFile ne '' then begin
	if bDicom then begin
		bOk = read_rs( RSFILE=rsFile, $
			DICOM_CTFILE=ctFile, $
			ALL_ROIS=allROIs, $
			ROI_NAMES=roiNames, $
			ROI_COUNT=nROIs, $
			ROI_ASSOC_FILE=roiAssocFile, $
			ROI_DATA=pROIs, $
			PATNAME=rsPatName, $
			STUDYDATE=rsStudyDate )
	endif else begin
		bOk = read_rs( RSFILE=rsFile, $
			PHILIPS_CTFILE=ctFile, $
			ALL_ROIS=allROIs, $
			ROI_NAMES=roiNames, $
			ROI_COUNT=nROIs, $
			ROI_ASSOC_FILE = roiAssocFile, $
			ROI_DATA=pROIs, $
			PATNAME=rsPatName, $
			STUDYDATE=rsStudyDate )
	endelse
	print, "RS patient name: " + strtrim( rsPatName, 2 )
	print, "RS study date: " + strtrim( rsStudyDate, 2 )

	; Fill slice gaps
	if n_elements( roiAssocFile ) ne 0 then begin
		; Working with assoc files
		openu, roiLun, /GET_LUN, roiAssocFile
		rois = assoc( roiLun, bytarr( ctDims[0], ctDims[1], ctDims[2] ) )
		for iROI=0, nROIs-1 do begin
			roi = rois[iROI]
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
					roi[*,*,iSlice:iAfter-1] = newSlices gt 0
					iSlice = iAfter+1
				endif else begin
					iSlice++
				endelse
			endwhile
			rois[iROI] = roi
		endfor
		close, roiLun
		free_lun, roiLun
	endif else begin
		; Working with pointers
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
	endelse

endif else begin
	if n_elements( roiAssocFile ) eq 0 then pROIs = ptr_new()
	roiNames = ""
	nROIs = 0
endelse

if bOk eq 0b then return, 0b

; Get PET data
bDicom = query_dicom( petFile )
if bDicom then begin
	bOk = dcm2rawNonUniform( FILE=petFile, OUT_FILE=petDataFile, DIMS=petDims, PIXSIZE=petPixSize, $
		POS=petPos, ACQTIME=petTimes, SUVSCL=petScl, WEIGHT=weight )
endif else begin
	bOk = gemini2raw( FILE=petFile, OUT_FILE=petDataFile, DIMS=petDims, PIXSIZE=petPixSize, $
			POS=petPos, FRAME_TIMES=petTimes, SCALES=petScl, WEIGHT=weight )
endelse
if bOk eq 0b then return, 0b
petOrigDims = petDims
petOrigPixSize = petPixSize

; Coregister PET and CT
if keyword_set( bCoreg2PET ) or not keyword_set( bCoreg2CT ) then begin
	align_ct2pet, PET_FILE=petDataFile, PET_POS=petPos, $
		PET_PIXSIZE=petPixSize, $
		CT_IMG=pCT, CT_POS=ctPos, $
		CT_PIXSIZE=ctPixSize, $
		DISPLAY=bDisplay
	ctDims = petDims
	ctPixSize = petPixSize
endif else begin
	if petPos[2,0] eq 0 then begin
		bFirst = 1b
		bCenter = 0
	endif else begin
		if keyword_set( bCenter ) then begin
			bCenter = 1
		endif else begin
			bCenter = 0
		endelse
		bFirst = 0b
	endelse
	align_pet2ct, PET_FILE=petDataFile, PET_POS=petPos, $
		PET_DIMS=petDims, PET_PIXSIZE=petPixSize, $
		CT_IMG=pCT, CT_POS=ctPos, $
		CT_PIXSIZE=ctPixSize, $
		DISPLAY=bDisplay, $
		FIRST=bFirst, CENTER=bCenter
	petDims = ctDims
	petPixSize = ctPixSize
endelse

return, 1

end ; getGeminiPetCt

