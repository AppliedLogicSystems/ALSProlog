:- 
	join_path([library,listutl1], F1),
	join_path([library,miscterm], F2),
	join_path([library,msc_ioin], F3),
	join_path([library,mscioout], F4),
	join_path([library,strctutl], F5),
	join_path([library,strings], F6),
	join_path([library,tcl_sppt], F7),
	join_path([library,tk_alslib], F8),
	join_path([library,typecomp], F9),
	force_libload_all([F1,F2,F3,F4,F5,F6,F7,F8,F9]).

:-consult(blt_dvsh), consult(dbg_class), consult(projects).
