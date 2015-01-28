pro run_align_dynamic_petCtRois

; Get file paths and filters from user_prefs file.
@ user_prefs

logFileName = "veinAlignmentLog.txt"

; Open log file.
logFile = WRITE_DIR + logFileName
openw, logLun, /GET_LUN, logFile

roiFiles = file_search( CT_DIR, RS_FILTER, COUNT=nFiles )

for iFile=0, nFiles-1 do begin

	rsFile = roiFiles[iFile]
	printf, logLun, "Aligning file " + strtrim(iFile+1,2) + " of " + strtrim(nFiles,2) + "..."
	print, "Aligning file " + strtrim(iFile+1,2) + " of " + strtrim(nFiles,2) + "..."
	parts = strsplit( rsFile, '\', /EXTRACT )
	indices = where( (parts eq 'Pre') or (parts eq 'Intra'), nFound )
	scan = parts[indices[0]]
	indices = where( strmatch( parts, 'S-0*', /FOLD_CASE ) eq 1, nFound )
	patId = parts[indices[0]]

	; Get corresponding DICOM CT file.
	ctFiles = file_search( CT_DIR+scan+"\"+patId+"\"+CT_FILTER, COUNT=nCT )
	seriesIds = strarr(nCT)
	for i=0, nCT-1 do begin
		parts = strsplit( file_basename( ctFiles[i] ), '.', /EXTRACT )
		seriesIds[i] = parts[0]
	endfor
	indices = uniq( seriesIds, sort(seriesIds) )
	nUniq = n_elements( indices )
	if nUniq ne 1 then begin
		; Find the series with the most files.
		uniqSeriesIds = seriesIds[indices]
		maxFiles = 0
		seriesId = -1
		for i=0, nUniq-1 do begin
			void = where( seriesIds eq uniqSeriesIds[i], count )
			if count gt maxFiles then begin
				maxFiles = count
				seriesId = uniqSeriesIds[i]
			endif
		endfor
		ctFile = ctFiles[(where( seriesIds eq seriesId ))[0]]
	endif else begin
		ctFile = ctFiles[0]
	endelse


	; Get corresponding Philips format PET file.
	petFiles = file_search( PET_DIR+scan+"\"+patId+"\"+PET_FILTER, COUNT=nPet )
	if nPet ne 1 then begin
		printf, logLun, "Error: Can't locate PET data."
		printf, logLun, ""
		continue
	endif else begin
		petFile = petFiles[0]
	endelse

	; Align.
	bOk = align_dynamic_petCtRois( $
			CTFILE=ctFile, PETFILE=petFile, RSFILE=rsFile, $
			CTPATNAME=ctPatName, CTSTUDYDATE=ctStudyDate, $
			PETPATNAME=petPatName, PETSTUDYDATE=petStudyDate, $
			RSPATNAME=rsPatName, RSSTUDYDATE=rsStudyDate, $
			EXIT_MESSAGE=exitMessage )
	if not bOk then begin
		printf, logLun, exitMessage
		printf, logLun, ""
	endif

	printf, logLun, "CT file:  " + strtrim(ctFile,2)
	printf, logLun, "PET file: " + strtrim(petFile,2)
	printf, logLun, "RS file:  " + strtrim(rsFile,2)
	printf, logLun, "CT pat name:  " + strtrim(ctPatName,2)
	printf, logLun, "PET pat name: " + strtrim(petPatName,2)
	printf, logLun, "RS pat name:  " + strtrim(rsPatName,2)
	printf, logLun, "CT study date:  " + strtrim(ctStudyDate,2)
	printf, logLun, "PET study date: " + strtrim(petStudyDate,2)
	printf, logLun, "RS study date:  " + strtrim(rsStudyDate,2)
	printf, logLun, ""

endfor ; nFiles

free_lun, logLun

end ; of run_align_dynamic_petCtRois
