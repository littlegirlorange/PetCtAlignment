;==============================================================================
;
;	Method:		dt
;
;	Description:
;				Computes the distance transform (DT) of a binary image.
;
;				Based on the algorithms described by:
;
;					Grevara GJ. Distance transform algorithms and their
;					implementation and evaluation. In: Deformable Models:
;					Biomedical and Clinical Applications (Topics in Biomedical
;					Engineering. International Book Series). Suri JS and Farag
;					AA, Eds. Springer-Verlag, New York. 2007:33-60.
;
;					Borgefors, G. On digital distance transforms in three
;					dimensions. Computer Vision and Image Understanding 1996
;					64(3):368-376.
;
;	Version:
;				071123
;				Original test version.  City block and chamfer only.
;
;	Inputs:
;				Binary image (bytarr).  NB: voxels must be isotropic.
;
;	Outputs:
;				Distance transform (fltarr).
;
;	Required Modules:
;
;	Written by:
;				Maggie Kusano, November 23, 2007
;
;==============================================================================
;
function dt, $
	mask, $
	s=s, $
	CITYBLOCK=city, $
	CHAMFER=chamfer

dims = size( mask, /DIM )
maxDist = total( dims )

d = [1,maxDist,maxDist]
type = 'city'
if keyword_set( chamfer ) then begin
	d=[3,4,5]
	type = 'chamfer'
endif
if n_elements(s) eq 0 then s = [1,1,1]

dImg = fix( (mask eq 0b)*maxDist + (mask gt 0b)*0 )

if type eq 'city' then begin

	; x dir
	for i=1, dims[0]-1 do begin
		dImg[i,*,*] = min( [[[reform(dImg[i-1,*,*])+s[0]]], [[reform(dImg[i,*,*])]]], DIM=3 )
	endfor
	for i=dims[0]-2, 0, -1 do begin
		dImg[i,*,*] = min( [[[reform(dImg[i+1,*,*])+s[0]]], [[reform(dImg[i,*,*])]]], DIM=3 )
	endfor

	; y dir
	for j=1, dims[1]-1 do begin
		dImg[*,j,*] = min( [[[reform(dImg[*,j-1,*])+s[1]]], [[reform(dImg[*,j,*])]]], DIM=3 )
	endfor
	for j=dims[1]-2, 0, -1 do begin
		dImg[*,j,*] = min( [[[reform(dImg[*,j+1,*])+s[1]]], [[reform(dImg[*,j,*])]]], DIM=3 )
	endfor

	; z dir
	for k=1, dims[2]-1 do begin
		dImg[*,*,k] = min( [[[reform(dImg[*,*,k-1])+s[2]]], [[reform(dImg[*,*,k])]]], DIM=3 )
	endfor
	for k=dims[2]-2, 0, -1 do begin
		dImg[*,*,k] = min( [[[reform(dImg[*,*,k+1])+s[2]]], [[reform(dImg[*,*,k])]]], DIM=3 )
	endfor

endif else begin

	indices = where ( dImg ne 0, nIndices )
	first = array_indices( dImg, indices[0] )
	last = array_indices( dImg, indices[nIndices-1] )

	; was 1
	for k=(first[2]>1), dims[2]-1 do begin
		for j=(first[1]>1), dims[1]-2 do begin
			for i=(first[0]>1), dims[0]-2 do begin
				dImg[i,j,k] = min( [dImg[i-1,j+1,k-1]+d[2], dImg[i,j+1,k-1]+d[1], dImg[i+1,j+1,k-1]+d[2], $
				                    dImg[i-1,j,k-1]+d[1],   dImg[i,j,k-1]+d[0],   dImg[i+1,j,k-1]+d[1],   $
				                    dImg[i-1,j-1,k-1]+d[2], dImg[i,j-1,k-1]+d[1], dImg[i+1,j-1,k-1]+d[2], $
				                    dImg[i-1,j,k]+d[0],     dImg[i,j,k],                                  $
				                    dImg[i-1,j-1,k]+d[1],   dImg[i,j-1,k]+d[0],   dImg[i+1,j-1,k]+d[1]] )
			endfor
		endfor
	endfor

	; was dims[?]-2
	for k=(last[2]<(dims[2]-2)), 0, -1 do begin
		for j=(last[1]<(dims[1]-2)), 1, -1 do begin
			for i=(last[0]<(dims[0]-2)), 1, -1 do begin
				dImg[i,j,k] = min( [dImg[i-1,j+1,k]+d[1],   dImg[i,j+1,k]+d[0],   dImg[i+1,j+1,k]+d[1],   $
									                        dImg[i,j,k],          dImg[i+1,j,k]+d[0],     $
								    dImg[i-1,j-1,k+1]+d[2], dImg[i,j-1,k+1]+d[1], dImg[i+1,j-1,k+1]+d[2], $
				                    dImg[i-1,j,k+1]+d[1],   dImg[i,j,k+1]+d[0],   dImg[i+1,j,k+1]+d[1],   $
				                    dImg[i-1,j+1,k+1]+d[2], dImg[i,j+1,k+1]+d[1], dImg[i+1,j+1,k+1]+d[2]] )

			endfor
		endfor
	endfor

endelse

if type eq 'chamfer' then dImg /= 3.0
return, dImg

end ; of dt