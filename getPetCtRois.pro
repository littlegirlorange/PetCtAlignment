;==============================================================================
;
;  METHOD
;   getPetCtROIs
;
;  DESCRIPTION
;   Reads dynamic PET/CT and RT structure set data and saves all in mk
;   format.  PET/CT data may be in DICOM or Philips native format.
;   RT structure sets must be in DICOM format.
;   To begin, PETs are aligned and resized to their respective CTs using
;   header position info.  RT structure sets are also decoded in CT space.
;   Returns header information and pointers to image data arrays if the
;   <PET or CT or ROI>_DATA keyword is used or will ASSOC the image data to
;   the file specified by the <PET or CT or ROI>_ASSOC_FILE keyword.
;
;  SYNTAX
;   bOk = getPetCtROIs(...)
;
;  INPUTS
;
;  OUTPUTS
;
;  REQUIRED MODULES
;   dcm2rawNonUniform
;   gemini2raw
;   read_rs
;   select_contours
;   align_pet2ct
;   align_ct2pet
;   allo_volume_read
;   idl_mhdr_struct
;   idl_subhdr_struct
;   idl_exthdr_struct
;
;  WRITTEN BY
;   Maggie Kusano, August 2, 2013
;
;  VERSION
;   071112
;   Version adapted from get_pet_ct_slope
;
;   071123
;   Version used to generate data sent to Huan on 071116.
;
;   090306
;   Version used for Ian Poon's CARO abstract.
;
;   090824
;   Reversed alignment (now align pre to intra) for Mike.
;
;   090829
;   Version passed to Mike.
;
;   120410
;   Version for PVC of carotids. Removed coregistration of pre- and intra-.
;
;   130731
;   Version for SKA-M analysis.
;
;==============================================================================

function getPetCtRois, $
		PET_FILE	= petFile, $
		PET_DATA	= pPET, $
		PET_ASSOC_FILE = petDataFile, $
		PET_DIMS	= petDims, $
		PET_ORIG_DIMS = petOrigDims, $
		PET_ORIG_PIXSIZE = petOrigPixSize, $
		PET_PIXSIZE	= petPixSize, $
		PET_FRAMES	= nPETFrames, $
		PET_FRAME_START	= startTimes, $
		PET_FRAME_END = endTimes, $
		PET_POS		= petPos, $
		PET_SCL		= petScl, $
		PET_PATNAME	= petPatName, $
		PET_STUDYDATE 	= petStudyDate, $
		CT_FILE		= ctFile, $
		CT_DATA		= pCT, $
		CT_DIMS		= ctDims, $
		CT_PIXSIZE	= ctPixSize, $
		CT_POS		= ctPos, $
		CT_PATNAME	= ctPatName, $
		CT_STUDYDATE	= ctStudyDate, $
		ALL_ROIS	= allROIs, $
		ROI_FILE	= rsFile, $			; in
		ROI_DATA	= pROIs, $			; out
		ROI_ASSOC_FILE = roiAssocFile, $	; out
		ROI_NAMES	= roiNames, $		; out
		ROI_COUNT	= nRois, $			; out
		ROI_PATNAME	= rsPatName, $
		ROI_STUDYDATE	= rsStudyDate, $
		COREG2PET	= bCoreg2Pet, $
		COREG2CT	= bCoreg2Ct, $
		FIRST		= bFirst, $
		CENTER		= bCenter, $
		NN        = bNN, $
		PATNAME		= patName, $
		STUDYDATE	= studyDate, $
		WEIGHT		= weight, $
		ACTIVITY	= activity, $
		DISPLAY		= bDisplay

; Get CT data
bDicom = query_dicom( ctFile )
if bDicom then begin
	bOk = dcm2rawNonUniform( FILE=ctFile, DATA=pCT, DIMS=ctDims, PIXSIZE=ctPixSize, $
		POS=ctPos, PATNAME=ctPatName, STUDYDATE=ctStudyDate )
endif else begin
	bOk = gemini2raw( FILE=ctFile, DATA=pCT, DIMS=ctDims, PIXSIZE=ctPixSize, $
			POS=ctPos, PATNAME=ctPatName, STUDYDATE=ctStudyDate, WEIGHT=weight )
endelse
if total( *pCT ) eq 0 then return, 0b
print, "CT patient name: " + strtrim( ctPatName, 2 )
print, "CT study date: " + strtrim( ctStudyDate, 2 )

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
if n_elements( petFile ) ne 0 then begin
bDicom = query_dicom( petFile )
if bDicom then begin
	bOk = dcm2rawNonUniform( FILE=petFile, OUT_FILE=petDataFile, $
			DIMS=petDims, PIXSIZE=petPixSize, $
			POS=petPos, ACQTIME=petTimes, SUVSCL=petScl, WEIGHT=weight, $
			PATNAME=petPatName, STUDYDATE=petStudyDate )
endif else begin
	bOk = gemini2raw( FILE=petFile, OUT_FILE=petDataFile, $
			DIMS=petDims, PIXSIZE=petPixSize, $
			POS=petPos, FRAMESTART=startTimes, FRAMEEND=endTimes, $
			SCALES=petScl, WEIGHT=weight, ACTIVITY=activity, $
			PATNAME=petPatName, STUDYDATE=petStudyDate )
endelse
if bOk eq 0b then return, 0b
petOrigDims = petDims
petOrigPixSize = petPixSize

print, "PET patient name: " + strtrim( petPatName, 2 )
print, "PET study date: " + strtrim( petStudyDate, 2 )

; Coregister PET, CT and ROIs
bCTAscending = ctPos[2,0] lt ctPos[2,ctDims[2]-1]
bPETAscending = petPos[2,0] lt petPos[2,petDims[2]-1]

if keyword_set( bCoreg2PET ) or not keyword_set( bCoreg2CT ) then begin
	; Flip the CT to match the PET orientation if necessary
	if bCTAscending ne bPETAscending then begin
		ctPos = reverse( ctPos, 2 )
		*pCT = reverse( temporary(*pCT), 3 )
	endif
	; Align the CT to the PET
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
	align_ct2pet, PET_FILE=petDataFile, PET_POS=petPos, $
		PET_DIMS=petDims, PET_PIXSIZE=petPixSize, $
		CT_IMG=pCT, CT_POS=ctPos, $
		CT_PIXSIZE=ctPixSize, $
		DISPLAY=bDisplay, $
		FIRST=bFirst, CENTER=bCenter
	; Align ROIs to PET
	if n_elements( roiAssocFile ) ne 0 then begin
		; Working with assoc files
		openu, roiLun, /GET_LUN, roiAssocFile
		rois = assoc( roiLun, bytarr( ctDims[0], ctDims[1], ctDims[2] ) )
		tmpRoiFile = roiAssocFile + '.tmp'
		openw, tmpLun, /GET_LUN, tmpRoiFile
		tmpRois = assoc( tmpLun, bytarr( petDims[0], petDims[1], petDims[2] ) )
		for iROI=0, nROIs-1 do begin
			pRoi = ptr_new(rois[iROI])
			; Flip the ROI to match the PET orientation
			if bCTAscending ne bPETAscending then $
				*pROI = reverse( temporary(*pROI), 3 )
			align_ct2pet, PET_FILE=petDataFile, PET_POS=petPos, $
					PET_DIMS=petDims, PET_PIXSIZE=petPixSize, $
					CT_IMG=pRoi, CT_POS=ctPos, $
					CT_PIXSIZE=ctPixSize, $
					DISPLAY=bDisplay, $
					FIRST=bFirst, CENTER=bCenter
			tmpRois[iROI] = *pRoi
			ptr_free, pROI
		endfor
		close, roiLun & free_lun, roiLun
		close, tmpLun & free_lun, tmpLun
		file_delete, roiAssocFile
		wait, 3
		file_move, tmpRoiFile, roiAssocFile
	endif else begin
		; Working with a pointer to the data
		tmpRois = bytarr( petDims[0], petDims[1], petDims[2], nROIs )
		for iROI=0, nROIs-1 do begin
			pRoi = ptr_new((*pROIs)[iROI])
			align_ct2pet, PET_FILE=petDataFile, PET_POS=petPos, $
					PET_DIMS=petDims, PET_PIXSIZE=petPixSize, $
					CT_IMG=pRoi, CT_POS=ctPos, $
					CT_PIXSIZE=ctPixSize, $
					DISPLAY=bDisplay, $
					FIRST=bFirst, CENTER=bCenter
			tmpRois[*,*,*,iRoi] = *pRois
			ptr_free, pRoi
		endfor
		ptr_free, pRois
		pRois = ptr_new( tmpROIs )
	endelse

	ctDims = petDims[0:2]
	ctPixSize = petPixSize
endif else begin
	; Flip the PET to match the CT if necessary
	if bCTAscending ne bPETAscending then begin
		petPos = reverse( petPos, 2 )
		nFrames = n_elements( startTimes )
		if n_elements( petDataFile ) eq 0 then begin
			for iFrame=0, nFrames-1 do begin
				(*pPET)[*,*,*,iFrame] = reverse( temporary((*pPET)[*,*,*,iFrame]), 3 )
			endfor
		endif else begin
			openr, petLun, /GET_LUN, petDataFile
			pet = assoc( petLun, intarr( petDims[0], petDims[1], petDims[2] ) )
			tmpPetfile = petfile+'.tmp2'
			openw, tmpPetLun, /GET_LUN, tmpPetFile
			tmpPet = assoc( tmpPetLun, intarr( petDims[0], petDims[1], petDims[2] ) )
			for iFrame=0, nFrames-1 do begin
				frame = pet[iFrame]
				tmpPet[iFrame] = reverse( temporary(frame), 3 )
			endfor
			close, petLun, tmpPetLun & free_lun, petLun, tmpPetLun
			file_delete, petDataFile
			wait, 3
			file_move, tmpPetFile, petDataFile
		endelse
	endif
	; Align the PET to the CT
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
		FIRST=bFirst, CENTER=bCenter, NN=bNN
	petDims = ctDims
	petPixSize = ctPixSize
endelse
endif ; have PET data

return, 1

end ; getGeminiPetCt

