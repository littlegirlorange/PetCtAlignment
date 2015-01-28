;DESCIPTION:
;	Extended header definition for petview-formatted image or  sinogram file
;
;
; MODIFICATIONS:
;	

pro idl_exthdr_struct, extended_header

		extended_header = { $
		dpat_name: 			bytarr(64),	$
		Dpat_id: 			bytarr(64), 	$
		study_uid: 			bytarr(64), 	$
		series_uid: 			bytarr(64), 	$
		view_code: 			bytarr(20), 	$ ; Memory map shows char[20] instead of char[21]??

		sortproto_name: 		bytarr(20), 	$
		fill1: 				bytarr(4),  	$
		req_phys:			bytarr(64), 	$
		card_phstate:			0u,	    	$
		fill2:				bytarr(146),	$
		referring_physician:		bytarr(64), 	$
		study_ID:			bytarr(16), 	$
		fill3:				0u,	    	$
		Dslice_thick:			0.,		$
		sex:				byte(0B),	$
		table_height:			0.,		$
		fill4:				byte(0B),	$
		card_bt_rej:			0u,		$
		card_fr_type:			0u,		$
		Dmanufacture_model_name:	bytarr(64),	$
		Dimage_type:			bytarr(64),	$
		min_bed_pos:			0.,		$
		max_bed_pos:			0.,		$
		der_filled:			0u,		$
		series_number:			0UL,		$
		study_date:			0UL,		$
		study_time:			0UL,		$
		acq_time:			0UL,		$
		card_slc_dir:			0u,		$
		fill5:				bytarr(44),	$
		acq_date:			0UL,		$
		fill6:				bytarr(212), 	$
		contr_bolus_agent:		bytarr(64),	$
		sop_uid:			bytarr(64),	$
		frame_ref_uid:			bytarr(64),	$
		pps_file:			bytarr(30),	$ ; Memory map shows char[30] instead of char[31]??

		worklist_file:			bytarr(30),	$ ; Memory map shows char[30] instead of char[31]??

		fill7:				bytarr(4),	$
		recon_swrel:			bytarr(6),	$ ; Memory map shows char[6] instead of char[7]??

		analy_swrel:			bytarr(6),	$ ; Memory map shows char[6] instead of char[7]??

		recprotocol_name:		bytarr(19),	$ ; Memory map shows char[19] instead of char[20]??

		insinofile:			bytarr(19),	$ ; Memory map shows char[19] instead of char[20]??

		slc_add:			0u,		$
		slc_space:			0u,		$
		slc_thick:			0u,		$
		frame_add:			0u,		$
		frame_space:			0u,		$
		frame_thick:			0u,		$
		fltr_type:			0u,		$
		smoth:				0u,		$
		scatcorr_typ:			0u,		$
		edge_exp:			0u,		$
		bckang_avg:			0u,		$
		bck_coeff:			0.,		$
		bck_wid:			0u,		$
		attncor_type:			0u,		$
		attncor_ecc:			0u,		$
		attn_coeff:			0.,		$
		regfile:			bytarr(19),	$ ; Memory map shows char[19] instead of char[20]??

		proc_transinofile:		bytarr(19),	$ ; Memory map shows char[19] instead of char[20]??

		skull_comp:			0.,		$
		norm_type:			0u,		$
		smp_norm:			0u,		$
		axnfile:			bytarr(19),	$ ; Memory map shows char[19] instead of char[20]??

		effnormfile:			bytarr(19),	$ ; Memory map shows char[19] instead of char[20]??

		gap_comp:			0u,		$
		algtype_em:			0u,		$
		num_iter:			0u,		$
		iter_em:			0u,		$
		subset_em:			0u,		$
		nsmooth_em:			0u,		$
		nrepeat_em:			0u,		$
		bckslc_avg:			0u,		$
		dead_corr:			0u,		$
		decay_corr:			0u,		$
		transinofile:			bytarr(19),	$ ; Memory map shows char[19] instead of char[20]??

		blnksinofile:			bytarr(19),	$ ; Memory map shows char[19] instead of char[20]??

		tran_ray_fwhm:			0.,		$
		tran_axl_fwhm:			0.,		$
		surv_mask:			0u,		$
		preflt_type:			0u,		$
		postflt_type:			0u,		$
		tr_posttyp:			0u,		$
		algtype_tr:			0u,		$
		iter_tr:			0u,		$
		subset_tr:			0u,		$
		nsmooth_tr:			0u,		$
		nrepeat_tr:			0u,		$
		attn_corr_3d:			0u,		$
		ramla_no_it:			0u,		$
		ramla_sysac:			0u,		$ ; Memory map shows short as opposed to int

		ramla_lamda:			fltarr(5),	$ ; Memory map shows float[5] as opposed to float[6]

		ramla_blrad:			0.,		$
 		ramla_blalpha:			0.,		$
		ramla_bcc_rsz:			0.,		$
		fill8:				bytarr(476)  	$
	}
end





































		

		






