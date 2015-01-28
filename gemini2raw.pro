;==============================================================================
;
; Method:   gemini2raw
;
; Description:
;       Reads a Philips format image series and returns the raw data
;       array with header info and/or saves the data in MK format
;       (.img/_info.txt)
;
; Inputs:
;       Philips PET, CT or SPECT file
;
; Outputs:

; 
; Required Modules:
;
;       
; Written by:
;       Maggie Kusano, August 10, 2004
;
; Version:
;       130802 dcm2raw
;       Version for dynamic PET analysis.
;
;==============================================================================

function gemini2raw, $
		FILE		= file, $
		DATA		= pData, $
		OUT_FILE	= dataFile, $
		DIMS		= dims, $
		PIXSIZE		= pixSize, $
		POSITION	= pos, $
		FRAMESTART	= startTimes, $
		FRAMEEND	= endTimes, $
		SCALES		= suvscls, $
		PATNAME		= patName, $
		STUDYDATE	= studyDate, $
		WEIGHT		= weight, $				; g
		ACTIVITY	= activity, $			; MBq
		INJ_TIME	= injectionTime, $
		WRITE		= bWriteImages

if n_elements( bWriteImages ) eq 0 then bWriteImages = 0b

if n_elements( file ) eq 0 then begin
	file = dialog_pickfile( FILTER='*', $
			GET_PATH=fileDir, $
			TITLE='Select Philips format image file' )
	if file eq '' then return, 0b
endif else begin
	fileList = file_search( file, COUNT=nFiles )
	if nFiles eq 0 then begin
		file = dialog_pickfile( FILTER='*', $
				GET_PATH=fileDir, $
				TITLE='Select Philips format image file' )
		if file eq '' then return, 0b
	endif else begin
		fileDir = file_dirname( file, /MARK )
	endelse
endelse

if n_elements( dataFile ) eq 0 then begin
	pData = ptr_new( allo_volume_read( file, $
			MAIN_HEADER=mhdr, SUB_HEADER=shdrs, $
			SLICE_NUMBERS=slices, FRAME_NUMBERS=frames ) )
	if n_elements(*pData) eq 0 then return, 0b
endif else begin
	bOk = allo_volume_read( file, $
			MAIN_HEADER=mhdr, SUB_HEADER=shdrs, $
			SLICE_NUMBERS=slices, FRAME_NUMBERS=frames, $
			ASSOC_FILE=dataFile )
	if bOk eq 0b then return, 0b
endelse

nFrames = max(frames)
if nFrames gt 1 then begin
	dims = intarr(4)
	dims[3] = nFrames
endif else begin
	dims = intarr(3)
endelse
dims[0] = shdrs[0].xdim
dims[1] = shdrs[0].ydim
dims[2] = mhdr.nslice
weight = float(mhdr.weight)
activity = float(mhdr.activity)
injectionTime = double(mhdr.hrinj) * 60 * 60 + double(mhdr.mininj) * 60

if n_elements( dataFile ) ne 0 then begin
	openu, uLun, /GET_LUN, dataFile
	assocVar = assoc( uLun, intarr(dims[0], dims[1], dims[2]) )
endif

pixSize = fltarr(3)
pixSize[0] = shdrs[0].pix_spacing[0]
pixSize[1] = shdrs[0].pix_spacing[1]
pixSize[2] = abs( shdrs[0].img_posz-shdrs[1].img_posz )
pos = fltarr(3,dims[2])
pos[0,*] = (shdrs.img_posx)[0:dims[2]-1]
pos[1,*] = (shdrs.img_posy)[0:dims[2]-1]
pos[2,*] = (shdrs.img_posz)[0:dims[2]-1]
;if pos[2,0] gt pos[2,dims[2]-1] then begin
;	pos = reverse( pos, 2 )
;	for iFrame=0, nFrames-1 do begin
;		if n_elements( dataFile ) eq 0 then begin
;			(*pData)[*,*,*,iFrame] = reverse( temporary((*pData)[*,*,*,iFrame]), 3 )
;		endif else begin
;			frame = assocVar[iFrame]
;			assocVar[iFrame] = reverse( temporary(frame), 3 )
;		endelse
;	endfor
;endif
patName = strjoin( strsplit( strtrim( mhdr.patient_name, 2 ), /EXTRACT), '_' )
studyYr = strtrim( mhdr.yrcre, 2 )
if strlen( studyYr ) lt 4 then studyYr = '19' + studyYr
studyMo = strtrim( mhdr.mocre, 2 )
if strlen( studyMo ) lt 2 then studyMo = '0' + studyMo
studyDay = strtrim( mhdr.daycre, 2 )
if strlen( studyDay ) lt 2 then studyDay = '0' + studyDay
studyDate = studyYr + studyMo + studyDay

; Translate frame times
;times = dblarr( nFrames*dims[2] )
;suvScls = shdrs.suvscl
;for iShdr=0, nFrames*dims[2]-1 do begin
;	acqTime = double( shdrs[iShdr].strhr ) * 60 * 60 $
;			+ double( shdrs[iShdr].strmin ) * 60 $
;			+ double( shdrs[iShdr].strsec )
;	times[iShdr] = acqTime
;
;	; Scale to SUVs (x1000 to maintain decimals) while we're at it
;	if iShdr mod nFrames eq 0 then begin
;		iFrame = iShdr/nFrames
;		if n_elements( dataFile ) eq 0 then begin
;			frame = (*pData)[*,*,*,iFrame] * suvScls[iShdr] * 1000
;			(*pData)[*,*,*,iFrame] = frame
;		endif else begin
;			frame = assocVar[iFrame] * suvScls[iShdr] * 1000
;			assocVar[iFrame] = frame
;		endelse
;	endif
;
;endfor

startTimes = dblarr( nFrames )
endTimes = dblarr( nFrames )
suvScls = dblarr( nFrames )
for iFrame=0, nFrames-1 do begin
	iShdr = iFrame * dims[2]
	acqTime = double( shdrs[iShdr].strhr ) * 60 * 60 $
			+ double( shdrs[iShdr].strmin ) * 60 $
			+ double( shdrs[iShdr].strsec )
	startTimes[iFrame] = acqTime
	acqTime = double( shdrs[iShdr].endhr ) * 60 * 60 $
			+ double( shdrs[iShdr].endmin ) * 60 $
			+ double( shdrs[iShdr].endsec )
	endTimes[iFrame] = acqTime

	; Scale to SUVs (x1000 to maintain decimals) while we're at it
	suvScls[iFrame] = shdrs[iShdr].suvscl
	if suvScls[iFrame] ne 0 then begin
		if n_elements( dataFile ) eq 0 then begin
			frame = (*pData)[*,*,*,iFrame] * shdrs[iShdr].suvscl * 1000
			(*pData)[*,*,*,iFrame] = frame
		endif else begin
			frame = assocVar[iFrame] * shdrs[iShdr].suvscl * 1000
			assocVar[iFrame] = frame
		endelse
	endif
endfor

if n_elements( dataFile ) eq 0 then begin
	if nFrames gt 1 then begin
		*pData = reform( temporary(*pData), dims[0], dims[1], dims[2], nFrames )
	endif
endif else begin
	close, uLun
	free_lun, uLun
endelse

return, 1b

end ; gemini2raw

