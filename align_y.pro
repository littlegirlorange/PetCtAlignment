;==============================================================================
;
;	Method:		align_y
;
;	Description:
;				Estimates shift in y required to align 2 CT images.
;
;	Version:
;				071205
;				Original test version.
;
;	Inputs:
;				2 sets of raw data (fixed and moving)
;
;	Outputs:
;				Returns the y-shift (in pixels)
;
;	Required Modules:
;				textbox.pro
;
;	Written by:
;				Maggie Kusano, June 8, 2008
;
;==============================================================================
;
function align_y, pFImg, pMImg

fImgDims = size( *pFImg, /DIM )

; Feature extraction (bone)
fMask = (*pFImg gt 1400)
mMask = (*pMImg gt 1400)
fIndices = where( fMask ne 0, count )
fCoords = array_indices( fMask, fIndices )
fYMax = max( fCoords[1,*] )
mIndices = where( mMask ne 0, count )
mCoords = array_indices( mMask, mIndices )
mYMax = max( mCoords[1,*] )

yShift = fYMax - mYMax

return, yShift

end ; of align_vert