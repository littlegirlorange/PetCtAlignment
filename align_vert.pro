;==============================================================================
;
;	Method:		align_vert
;
;	Description:
;				Estimates vertical shift required to align 2 CT images.
;
;	Version:
;				071205
;				Original test version.
;
;	Inputs:
;				2 sets of raw data (fixed and moving)
;
;	Outputs:
;				Returns the z-shift
;
;	Required Modules:
;				textbox.pro
;
;	Written by:
;				Maggie Kusano, December 5, 2007
;
;==============================================================================
;
function align_vert, pFImg, pMImg

IMG_SIZE_X = 128b

fImgDims = size( *pFImg, /DIM )

; Feature extraction (bone)
fMask = (*pFImg gt 1400)
mMask = (*pMImg gt 1400)
;
;tv3d, fMask, IMGSIZEX=IMG_SIZE_X, TITLE='Fixed mask'
;tv3d, mMask, IMGSIZEX=IMG_SIZE_X, TITLE='Moving mask'

fCount = total( total( fMask, 1 ), 1 )
mCount = total( total( mMask, 1 ), 1 )

lag = indgen( fImgDims[2] ) - fImgDims[2]/2

correl = c_correlate( fCount, mCount, lag )

print, correl

d = -lag[ (where( correl eq max( correl ) ))[0] ]

return, d

end ; of align_vert