;DESCIPTION:
;	Main header definition for petview-formatted image or  sinogram file
;
;
; MODIFICATIONS:
;	30 November, 2004 (Manoj) -updated Main header tags to match 
;				   main header version 11
;			           sub-header version 1
;



pro idl_mhdr_struct, main_header

	main_header = { $

		; each section is 16 bytes

		top: byte([1B,0B,0B,0B,0B,22B]), $	; values from "main header file format: memory map"
		file_fmt: 0u, $	; integer file format
		scan_geom: 0u, $
		hw_config: 0u, $
		fill1: byte([0B,64B,0B,91B]), $
		edit_flag: 0u, $
		fill2: bytarr(14), $
		fill3: bytarr(16), $
		fill4: byte([0B,7B,255B,255B]), $
		filtyp: 0u, $
		fill5: byte([0B,0B,0B,0B,0B,0B,0B,0B,0B,32B]), $
		fill6: byte([255B,255B]), $
		daycre: 0u, $
		mocre: 0u, $
		yrcre: 0u, $
		hrcre: 0u, $
		mincre: 0u, $
		seccre: 0u, $
		duratn: 0u, $

		shdtyp: 0u, $
		sngpscl: 0u, $
		singopt: 0u, $
		pscale: 0., $	; 
		detectorRadius: 0., $			;added 11/30/04
		virtualXtal: 0u, $			;added 11/30/04
		phiMashing: 0u, $			;added 11/30/04
		polygonSides: 0u, $			;added 11/30/04
		XtalsPerSide: 0u, $			;added 11/30/04
		nXtalRows: 0u, $			;added 11/30/04
		crystalThickness: 0., $			;added 11/30/04
		xXtalPitch: 0., $			;added 11/30/04
		zXtalPitch: 0., $			;added 11/30/04
		axialFOV: 0., $				;added 11/30/04
		rphiType: 0u, $				;added 11/30/04
		sliceType: 0u, $			;added 11/30/04
		delayType: 0u, $			;added 11/30/04


				;fill7: bytarr(6),  *** Deleted ***
				;fill8: intarr(8),  *** Deleted ***
				;fill9: fix([0,0,0,0,0,0,0,1]),  *** Deleted ***

		fill7: uint(['001B'x,'ffff'x]), $	;modified 11/30/04
		pattyp: 0u, $
		scntyp: 0u, $
		numray: 0u, $
		numang: 0u, $
		slcthk: 0u, $
		isotop: 0u, $
		slope: 0., $	; stretches to next section

		; last 2 bytes of slope
		intcpt: 0., $
		injtim: 0u, $
		polygonVertAt0deg: 0., $		;added 11/30/04

		
				;fill11: uintarr(2), *** Deleted ***

		nslice: 0u, $
		nframe: 0u, $
		bthday: 0u, $
		bthmo: 0u, $
		bthyr: 0u, $
		ssn: bytarr(10), $
		ntilt: 0u, $
		petnum: 0u, $
		fill8: uint(['0021'x,'ffff'x]), $	;modified tag name 11/30/04
		activity: 0., $
		weight: 0UL, $
		hrinj: 0u, $
		mininj: 0u, $
		srcRadius: 0., $			;added 11/30/04
		srcZpos: 0., $				;added 11/30/04
		halfLife: 0., $				;added 11/30/04

				;fill13: uintarr(6), *** Deleted ***

		concfac: 0., $
		concfac_bgsub: 0., $
		dmax: 0., $
		dline: 0., $
		angmax: 0., $
		x0: 0., $
		y0: 0., $
		z0: 0., $
		nevent: 0., $				;changed to float 11/30/04
		nsino: 0., $				;changed to float 11/30/04
		eglob_low: 0u, $
		eglob_up: 0u, $
		eloc_low: 0u, $
		eloc_up: 0u, $
		orient_hf: 0u, $			;modified tag name 11/30/04
		scan_swrel: bytarr(6), $		;changed to char(bytarr) 11/30/04

		petct_sepdist: 0u, $			;added 11/30/04
		petct_landmrk: 0u, $			;added 11/30/04
		petct_tstmp1: 0u, $			;added 11/30/04
		petct_tstmp2: 0u, $			;added 11/30/04
		
				;fill14: uintarr(4), *** Deleted ***

		tbl_direction: 0u, $
		orient_ps: 0u, $			;modified tag name 11/30/04
		petct_zoffset: 0u, $			;added 11/30/04
		petct_xshift: 0u, $			;added 11/30/04
		petct_yshift: 0u, $			;added 11/30/04
		petct_zshift: 0u, $			;added 11/30/04
		petct_acqflgs: 0u, $			;added 11/30/04
		petct_xoffset: 0u, $			;added 11/30/04
		petct_yoffset: 0u, $			;added 11/30/04
		petct_axrot: 0u, $			;added 11/30/04
		petct_horzrot: 0u, $			;added 11/30/04
		petct_vertrot: 0u, $			;added 11/30/04
		frontLeadDiameter: 0., $		;added 11/30/04
		backLeadDiameter: 0., $			;added 11/30/04
		leadSeparation: 0., $			;added 11/30/04
		ndelays: 0., $				;added 11/30/04

				;fill15: uintarr(6), *** Deleted ***
				;fill16: uintarr(8), *** Deleted ***
				;fill17: uintarr(4), *** Deleted ***

		slcsep: 0., $
		petct_valid: 0u, $			;added 11/30/04

				;fill18: 0u, *** Deleted ***

		fctrfil: bytarr(20), $
		baselin: bytarr(20), $
		dstpkfl: bytarr(20), $
		aqprotocol_name: bytarr(20), $
		aqprotocol_type: 0u, $
		patient_name: bytarr(30), $
		reslice_ang1: 0., $
		reslice_ang2: 0., $
		reslice_ang3: 0., $
		minslc: 0u, $
		maxslc: 0u, $
		minfrm: 0u, $
		maxfrm: 0u, $
		scanner_maxslice: 0u, $
		fill9: 0u, $				;modified tag name 11/30/04
		rebin_type: 0u, $
		scnorigin: bytarr(16), $
		accNum: bytarr(16), $ 			;added 11/30/04
		movementCoinc: 0u, $			;added 11/30/04
		movementSing: 0u, $			;added 11/30/04
		crbTstampPeriod: 0u, $			;added 11/30/04
		fill10: bytarr(10), $			;modified tag contents 11/30/04
		trailexists: 0u, $
		trailbeg: 0., $
		fill11: 0u $				;modified tag name 11/30/04

	}
end
