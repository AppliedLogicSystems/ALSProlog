
tmp_ld_tmp(Y,X) :- append(Y,[X],PL), join_path(PL,Z), simple_load(Z).

bldit(CorePath, SVIm)
	:- 
	split_path(CorePath, CPL),
	append(CPL, [tcltk_interface,common,'tcltk_util.pro'], TcltkUtilList),
	join_path(TcltkUtilList, TcltkUtil),
	simple_load(TcltkUtil),

	append(CPL,[alsp_src,builtins],BPL),
	tmp_ld_tmp(BPL,'blt_dvsh.pro'), 
	tmp_ld_tmp(BPL,'dbg_class.pro'), 
	tmp_ld_tmp(BPL,'projects.pro'), 

	append(CPL,[als_dev,alsdev,'demo15.pro'], Demo15List), 
	join_path(Demo15List, Demo15),
		simple_load(Demo15),
	append(CPL,[als_dev,alsdev,'serial_cmn.pro'], SerialList), 
	join_path(SerialList, Serial),
		simple_load(Serial),

	append(CPL,[alsp_src,library],LPL),
	tmp_ld_tmp(LPL,'listutl1.pro'),
	tmp_ld_tmp(LPL,'miscterm.pro'),
	tmp_ld_tmp(LPL,'msc_ioin.pro'),
	tmp_ld_tmp(LPL,'mscioout.pro'),
	tmp_ld_tmp(LPL,'strctutl.pro'),
	tmp_ld_tmp(LPL,'strings.pro'),
	tmp_ld_tmp(LPL,'tcl_sppt.pro'),
	tmp_ld_tmp(LPL,'tk_alslib.pro'),
	tmp_ld_tmp(LPL,'typecomp.pro'),
	tmp_ld_tmp(LPL,'misc_db.pro'),

	abolish(bldit,2),
	call(SVIm),
	abolish(tmp_ld_tmp, 2).

module builtins.
tdvf :- demo_init.
endmod.
