/*============================================================*
 |      cmn_utils.pro
 |  Copyright (c) 1997-8 Applied Logic Systems, Inc.
 |
 |		Common utilities
 |
 |	Author: Ken Bowen
 *============================================================*/

module app_utils.
use tk_alslib.
use tcltk.

export save_settings/1.
export load_app_settings/1.
export info_record/2.
export info_record/3.
export interleave/3.


/*-----------------------------------------------------------------------
 *-----------------------------------------------------------------------*/
save_settings(File)
	:-
	(user:working_dir(WD) ->
		change_cwd(WD)
		;
		true
	),
	file_extension(FileName,File,lsl),
	open(FileName, write, OS),
	unwind_protect( save_app_settings(OS), close(OS)	),
    user:app_home_dir(HomeDir),
	join_path([HomeDir,lastloc], LastLoc),
%sprintf(atom(M1),'lastloc f=%t', [LastLoc]),
%info_dialog(M1),
	user:determine_seriessource(SS),
	open(LastLoc,write,OS2),
	write(OS2, WD), nl(OS2),
	write(OS2, SS), nl(OS2),
	close(OS2).

/*-----------------------------------------------------------------------
 *-----------------------------------------------------------------------*/
load_app_settings([]).
load_app_settings([Term | SettingsTerms])
	:-
	load_app_setting_term(Term),
	load_app_settings(SettingsTerms).

load_app_setting_term(agv(Tag)=Val)
	:-!,
	tcl_call(tcli, [set_tcl_ga,agv,Tag,Val], _).

load_app_setting_term(agv(Tag1,Tag2)=Val)
	:-!,
	tcl_call(tcli, [set_tcl_ga2,agv,Tag1,Tag2,Val], _).

load_app_setting_term(_).

/*-----------------------------------------------------------------------
 *-----------------------------------------------------------------------*/
info_update(Pred, Arg1)
	:-
	Fact0 =.. [Pred,_],
	(retract(Fact0) -> true ; true),
	Fact =.. [Pred,Arg1],
	assert(Fact).

/*-----------------------------------------------------------------------
 *-----------------------------------------------------------------------*/
info_update(Pred, Arg1, Arg2)
	:-
	Fact0 =.. [Pred,Arg1,_],
	(retract(Fact0) -> true ; true),
	Fact =.. [Pred,Arg1,Arg2],
	assert(Fact).

/*-----------------------------------------------------------------------
 *-----------------------------------------------------------------------*/
interleave([], _, []) :-!.
interleave([Elt], _, [Elt]) :-!.
interleave([Elt | Elts], What, [Elt, What | XElts])
	:-
	interleave(Elts, What, XElts).

endmod.






