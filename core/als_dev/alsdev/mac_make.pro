:-
	consult(
		[debugger, tcltk,
		 blt_dvsh, dbg_class, projects, 
		':alsdir:library:listutl1',
		':alsdir:library:miscterm',
		':alsdir:library:msc_ioin',
		':alsdir:library:strctutl',
		':alsdir:library:strings',
		':alsdir:library:tcl_sppt',
		':alsdir:library:tk_alslib'
		]).
	
%:- attach_image('ALS Prolog PPC', [select_lib(builtins, [debugger]),
%	select_lib(library,[miscterm,msc_ioin,strctutl,strings,tcl_sppt,
%						tk_alslib, listutl1])]).


:- attach_image('ALS Prolog PPC', []).
