
; NAME:
;   allo_volume_read
;
; AUTHOR:
;   Jeff Kolthammer, 04 June 2002
;
; DESCTIPTION:
;   Read in a petview-formatted image or interpolated sinogram file.  Return the image as a
;   useful IDL volume for analysis.  Only return the raw values in the file (no processing or
;   scaling here - let someone else's program do that).  Optionally return via keywords the
;   header information, matrix (slice, frame) numbers.
;
; MODIFICATIONS:
;   18 June 2003 (JAK) Pull this routine out of allegrobox.pro and maintain it separately.
;   20 June 2003 (JAK) add debug switch and mode - try to determine why some files read and some don't
;   02 December 2003 (JAK) add tilt number output
;   05 December 2003 (JAK) use directory pointers in read to account for extended header or aberrations
;   13 September 2004 (JAK) clean up for addition to luminary tools package
;   20 November 2004 (Manoj) add section to read in extended header
;
; EXAMPLE:
;   Here, we read an uncompressed imagio-formatted image file, scale it to actual counts and show a movie of
;   slice 38 of each frame in IDL.
;      IDL> my_img = allo_volume_read('./p0s0_torso.img',sub_header=shdr,slice_numbers=sln)
;      IDL> my_img = float(my_img)
;      IDL> for i=0, n_elements(my_img[0,0,*])-1 do my_img[*,*,i] = my_img[*,*,i]*shdr[i].imgscl
;      IDL> slice_38s = my_img[*,*,where(sln eq 38)]
;      IDL> for i=0,n_elements(slice_38s)-1 do tvscl, congrid(slice_38s[*,*,i],288,288,cubic=-.5)
;
;-
;

function allo_volume_read, $       ; parameters follow (default value)
       filename, $         ; filename of .scn or .img file  (run dialog_pickfile())
       path=path, $        ; if filename not specified, start pickfile dialog in this path ('')
       title=title, $	   ; if filename not specified, use this title for the pickfile dialog
       main_header=main_header, $   ; output - main header in struct form (output)
       sub_header=sub_header, $     ; output - vector of sub headers in struct form (output)
       identifying_numbers = identifying_numbers, $  ; output - vector of matrix numbers for each matrix returned from function (output)
       slice_numbers = slice_numbers, $ ; output - vector of slice numbers calculated from matrix numbers (output)
       frame_numbers=frame_numbers, $   ; output - vector of frame numbers calcualted from matrix numbers (output)
       tilt_numbers = tilt_numbers, $   ; output - vector of tilt numbers calculated from matrix numbers (output)
       debug = debug, $        ; provide intermediate outputs for debugging a file read (FALSE)
       doswap = doswap, $       ; swap_endian the output volume (TRUE)
       extended_header=extended_header, $ ;output - extended header in struct form (output)
       assoc_file = assocFile



if n_elements(filename) eq 0 then filename = dialog_pickfile(path=path, title=title)
if n_elements(debug) eq 0 then debug = 0
if n_elements(doswap) eq 0 then doswap = 1
if n_elements(assocFile) eq 0 then begin
	assocFile = ''
endif else begin
	i = 0
	while file_test( assocFile ) do begin
		parts = strsplit( file_basename(assocFile), '.', /EXTRACT, COUNT=nParts )
		parts[nParts-2] += strtrim(i++,2)
		basename = strjoin( parts, '.' )
		dirname = file_dirname( assocFile, /MARK )
		assocFile = dirname + basename
	endwhile
endelse
resolve_routine, "idl_mhdr_struct", /no_recompile
resolve_routine, "idl_subhdr_struct", /no_recompile
idl_mhdr_struct, main_hdr
idl_subhdr_struct, sub_hdr

;Added section to read in extended header - Manoj 11/29/04
resolve_routine, "idl_exthdr_struct", /no_recompile
idl_exthdr_struct, extd_hdr

openr, lun, filename, /get_lun

    ; read in all directory entries (DE) first
    ;

directoryEntry = lonarr(128)
filesize = (fstat(lun)).size    ; might come in handy
readu, lun, main_hdr       ; get that main header, first DR always follows
main_hdr = swap_endian(main_hdr)
nDirectoryEntry = 0L       ; number of DR in file
nMatrices = 0L       ; number of non-header matrices in file

    ; count number of DE in file
    ;

lastPtr = 0L & nextPtr = 512L     ; start at 512, beginning of first DR
thisRecord = 0L
while (nextPtr gt lastPtr) do begin
    if debug then print, 'DR pointers: ', nextPtr, lastPtr,nextPtr/512L
    point_lun, lun, nextPtr+4  ; bump ahead to second word of this DR
    lastPtr = nextPtr
    readu, lun, thisRecord        ; read in record number of next DR

    nextPtr = (swap_endian(thisRecord)-1)*512L     ; point to a byte position in the file
    nDirectoryEntry++      ; count the current (not next) DR
endwhile

    ; fill a list of DE, now that we know how many there are
    ;

if debug then print, 'Number of DR = ', nDirectoryEntry
directoryEntryList = lonarr(128,nDirectoryEntry)
nextPtr = 512L  ; we know where the first DE sits
for iDirectoryEntry = 0, nDirectoryEntry-1 do begin
    point_lun, lun, nextPtr
    readu, lun, directoryEntry
    directoryEntry = swap_endian(directoryEntry)
    directoryEntryList[*,iDirectoryEntry] = directoryEntry
    nextPtr = (directoryEntry[1]-1)*512L
    if debug then print, "# of used entries = ",directoryEntry[3]
    nMatrices += directoryEntry[3]
endfor

    ; decremement nMatrices if the first one was an extended header block
    ;

if (directoryEntryList[4,0] eq '7fffffff'x) then begin
    boolExtendedHeader = 1
    nMatrices--
    ;Added section to read in extended header - Manoj 11/29/04
    point_lun, lun, (directoryEntryList[5,0]-1)*512L ; 1st record number of extd header
    readu, lun, extd_hdr
    extd_hdr = swap_endian(extd_hdr)
endif

    ; look at second subheader to define size of matrix (first might be a header)
    ;

point_lun, lun, (directoryEntryList[9,0]-1)*512L
readu, lun, sub_hdr
sub_hdr = swap_endian(sub_hdr)

    ; declare data spaces now that we know data sizes
    ;

sub_hdrList = replicate(sub_hdr,nMatrices)
if assocFile ne '' then begin
	openw, wLun, /GET_LUN, assocFile
endif else begin
	vol_out = intarr(sub_hdr.xdim,sub_hdr.ydim,nMatrices)
endelse
matrix = intarr(sub_hdr.xdim, sub_hdr.ydim)
identNumbers = lonarr(nMatrices)
if debug then print, 'Number of img matrices in file = ', nMatrices

    ; read in the data, one DE at a time then one matrix at a time
    ;

iMatrix = 0L
for iDirectoryEntry = 0, nDirectoryEntry-1 do begin
    for iRecord = 0, directoryEntryList[3,iDirectoryEntry]-1 do begin
       identNumber = directoryEntryList[4*iRecord+4,iDirectoryEntry]
       if debug then print, iDirectoryEntry, iRecord, identNumber, iMatrix
       if (identNumber ne '7fffffff'x) then begin    ; this is good data, not a header block
         identNumbers[iMatrix] = identNumber
         point_lun, lun, (directoryEntryList[4*iRecord+5,iDirectoryEntry]-1)*512L
         readu, lun, sub_hdr
         sub_hdrList[iMatrix] = swap_endian(sub_hdr)
         readu, lun, matrix
         if assocFile ne '' then begin
         	writeu, wLun, swap_endian(matrix)
         endif else begin
         	vol_out[*,*,iMatrix] = swap_endian(matrix)
         endelse
         iMatrix++
       endif
    endfor
endfor

close, lun
free_lun, lun

main_header = main_hdr
sub_header = sub_hdrList
tilt_numbers = ishft(identNumbers,-28)
slice_numbers = ishft(identNumbers,-16) and '00ff'x
frame_numbers = fix(identNumbers)
identifying_numbers = identNumbers
extended_header=extd_hdr	;Added section to read in extended header - Manoj 11/29/04

if assocFile ne '' then begin
	close, wLun
	free_lun, wLun
	return, 1
endif else begin
	return, vol_out
endelse

end
