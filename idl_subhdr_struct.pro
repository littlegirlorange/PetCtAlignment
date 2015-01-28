;DESCIPTION:
;	Subheader definition for petview-formatted image or  sinogram file
;
;
; MODIFICATIONS:
;	30 November, 2004 (Manoj) -updated subheader tags to match 
;					subheader version # 1
;					main header version 11


pro idl_subhdr_struct, subheader

subheader = { $

	fill1: uint(['0100'x,'0001'x,'0016'x,'ffff'x,'0019'x,'003e'x,'003f'x]), $ 
						;modified entry 11/30/04

	version: 0u, $ 		     		;added 11/30/04
	
			;fill2: uintarr(16), $	***deleted***

	atten_corr: bytarr(16), $    		;added 11/30/04
	actual_bedpos: 0.,  $	     		;added 11/30/04
	orientation0: 0., $			;added 11/30/04
	orientation1: 0., $			;added 11/30/04
	orientation2: 0., $			;added 11/30/04
	fill2: uint(['0025'x,'ffff'x]), $	;modified tag 11/30/04

			;fill4: uintarr(6), ***deleted***
			;fill5: uintarr(29), ***deleted***
			

	orientation3: 0., $			;added 11/30/04
	orientation4: 0., $			;added 11/30/04
	orientation5: 0., $			;added 11/30/04
	card_fr_time: 0UL ,  $ 			;added 11/30/04
	card_high_rr: 0UL ,  $ 			;added 11/30/04
	card_low_rr: 0UL ,  $ 			;added 11/30/04
	card_tr_time: 0UL ,  $ 			;added 11/30/04
	scatter_corr: bytarr(16), $		;added 11/30/04
	deadtime_corr: 0u, $			;added 11/30/04
	randoms_corr: 0u, $			;added 11/30/04
	det_norm: 0u, $				;added 11/30/04
	nu_radsamp_corr: 0u, $			;added 11/30/04
	pat_mot_corr: 0u, $			;added 11/30/04
	echo_time: 0., $			;added 11/30/04
	exposure_time: 0., $			;added 11/30/04
	img_posx:    0., $			;added 11/30/04
	img_posy:    0., $			;added 11/30/04

	fill3: uint(['0011'x,'ffff'x]), $ 	;modified tag 11/30/04
	datype: 0u, $

			;fill7: uintarr(2), ***deleted***

	img_posz:    0., $			;added 11/30/04
	xdim: 0u, $
	ydim: 0u, $
	slcnum: 0u, $
	tiltnum: 0u, $
	gatint: 0u, $
	
			;fill8: uintarr(7), ***deleted***

	cntloss_corr: 0u, $			;added 11/30/04
	pix_spacing: fltarr(2), $		;added 11/30/04
	xray_current: 0., $	;mem-map shows 4 bytes ??. added 11/30/04
	suvscl: 0., $
	
			;fill9: uintarr(4), ***deleted***
	
	kvp: 0., $				;added 11/30/04
	Dslice_loc: 0., $			;added 11/30/04
	magfac: 0., $
	imgscl: 0., $
	imgmin: 0u, $
	imgmax: 0u, $
	
			;fill10: 0u, ***deleted***

	decay_corr: 0u, $			;added 11/30/04
	scnscl: 0., $
	strhr: 0u, $
	strmin: 0u, $
	strsec: 0u, $
	scnmin: 0u, $
	scnmax: 0u, $
	endhr: 0u, $
	endmin: 0u, $
	endsec: 0u, $
	midtim: 0., $
	fill4: 0u, $				;modified tag 11/30/04
	mseclen: 0u, $
	scnlen: 0u, $
	imgsum: 0., $
	scnsum: 0., $
	
			;fill12: uintarr(4), ***deleted***
	
	bgdelrt: 0., $				;added 11/30/04
	enddelrt: 0., $				;added 11/30/04
	bgsngrt: 0., $
	bgcoincrt: 0., $
	endsngrt: 0., $
	endcoincrt: 0., $
	deadtimefac: 0., $
	bedpos: 0u, $
	deadtime_bgsub: 0., $
	fill5: 0u, $				;added 11/30/04
	sop_uid: bytarr(64), $			;added 11/30/04
	recon_method: bytarr(16), $		;added 11/30/04
	fill6: bytarr(176) $			;added 11/30/04

	;	*** remaining tags removed since all      ****
	;	*** reconinfo moved to extended header     ***

			;reconinfo_recon_swrel: uintarr(3), $
			;reconinfo_analy_swrel: uintarr(3), $
			;reconinfo_recprotocol_name: bytarr(20), $
			;reconinfo_insinofile: bytarr(20),$
			;rc_slcdd: 0u, $
			;rc_slc_space: 0u, $
			;rc_slc_thick: 0u, $
			;rc_frame_add: 0u, $
			;rc_frame_space: 0u, $
			;rc_frame_think: 0u, $
			;rc_fltr_type: 0u, $
			;rc_smoth: 0u, $
			;rc_bgsub_type: 0u, $
			;rc_edge_exp: 0u, $
			;rc_bckang_avg: 0u, $
			;reconinfo_bck_coeff: 0., $
			;rc_bck_wid: 0u, $
			;attncor_type: 0u, $
			;reconinfo_attn_coef: 0., $
			;reconinfo_regfile: bytarr(20), $
			;reconinfo_proc_transinofile: bytarr(20), $
			;reconinfo_skull_comp: 0., $
			;rc_norm_type: 0u, $
			;rc_smp_norm: 0u, $
			;reconinfo_axnfile: bytarr(20), $
			;reconinfo_effnormfile: bytarr(20), $
			;rc_gap_comp: 0u, $
			;rc_algtype_em: 0u, $
			;rc_num_iter: 0u, $
			;rc_iter_em: 0u, $
			;rc_subset_em: 0u, $
			;rc_nsmooth_em: 0u, $
			;rc_nrepeat_em: 0u, $
			;rc_bckslc_avg: 0u, $
			;rc_dead_corr: 0u, $
			;rc_decay_corr: 0u, $
			;reconinfo_transinofile: bytarr(20), $
			;reconinfo_blnksinofile: bytarr(20), $
			;reconinfo_tran_ray_fwhm: 0., $
			;reconinfo_tran_axl_fwhm: 0., $
			;rc_surv_mask: 0u, $
			;rc_preflt_typ: 0u, $
			;rc_tr_posttyp: 0u, $
			;rc_algtype_tr: 0u, $
			;rc_iter_tr: 0u, $
			;rc_ubset_tr: 0u, $
			;rc_nsmooth_tr: 0u, $
			;rc_nrepeat_tr: 0u $
	}

end
