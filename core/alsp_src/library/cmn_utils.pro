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

export load_app_settings/1.
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

export info_record/2.
info_update(Pred, Arg1)
	:-
	Fact0 =.. [Pred,_],
	(retract(Fact0) -> true ; true),
	Fact =.. [Pred,Arg1],
	assert(Fact).

export info_record/3.
info_update(Pred, Arg1, Arg2)
	:-
	Fact0 =.. [Pred,Arg1,_],
	(retract(Fact0) -> true ; true),
	Fact =.. [Pred,Arg1,Arg2],
	assert(Fact).

export interleave/3.
interleave([], _, []) :-!.
interleave([Elt], _, [Elt]) :-!.
interleave([Elt | Elts], What, [Elt, What | XElts])
	:-
	interleave(Elts, What, XElts).

endmod.






module builtins.

export basic_app_init/0.
basic_app_init
	:-
	init_tk_alslib,
	tcl_call( tcli, [package,require,'Iwidgets'], _),
	tcl_call( tcli, [package,require,'Tktable'], _),
	set_prolog_flag(unknown,fail).

	%% cmd_comps(CmdLine, App, Path, A1Path,A1Tail)
export cmd_comps/5.
cmd_comps([A0], App, Path, '','')
	:-!,
	path_directory_tail(A0,Path,App).

cmd_comps([A0,A1 | _], App, Path, A1Path,A1Tail)
	:-!,
	path_directory_tail(A0,Path,App),
	path_directory_tail(A1,A1Path,A1Tail).

cmd_comps([A0], App, Path, Path,App)
	:-
	path_directory_tail(A0,Path,App).

:- module_closure(negotiate_path,3,negotiate_path3).
negotiate_path3(Mod, Branch, HomeDir, TclPath)
	:-
	negotiate_path4(Mod, tcli, Branch, HomeDir, TclPath).
negotiate_path4(Mod, TclInterp, Branch, HomeDir, TclPath)
	:-
	pbi_get_command_line(CmdLine),
	cmd_comps(CmdLine, App, Path, A1Path,A1Tail),
	user:this_app_name(App0),
	((Mod:this_app_name(App)) ->
		split_path(Path, PathList),
		split_path(Branch, BranchElts),
		append(PathList, BranchElts, TclPathList),
		join_path(TclPathList, TclPath),
		HomeDir = Path
		;
		TclPath = app_files,
		((A1Path = '', Branch \= '.') ->
			HomeDir = '.'
			;
			HomeDir = A1Path
		)
	),
	tcl_call(TclInterp, [set,'APPTCLPATH',TclPath], _).

%		builtins:canon_path(Path, HomeDir)
%		builtins:canon_path(A1Path, HomeDir)
/*
export adjust_locn_for_runcmd/1.
	%% CMD = command line to start the app:
adjust_locn_for_runcmd(RunCMD)
	:-
	path_elements(RunCMD, PE),
	dreverse(PE,RPE),
	RPE = [_ | RDPE],
	dreverse(RDPE,DPE),
	path_elements(ImageDir, DPE),
	(ImageDir \= '' ->
		change_cwd(ImageDir)
		;
		true
	).
*/

export stock_splash/4.
stock_splash(TclPath, File, TclInterp, RemoveCmd)
	:-
	path_elements(TclPath, TPL),
	append(TPL, [images,File], TPLI),
	tcl_call(TclInterp, [file,join | TPLI], SPP),
	sprintf(atom(Splashy), 'image create photo app_splash_gif -file "{%t}"',[SPP]),
	CL= [
	'wm withdraw .',
	'toplevel .stock_spash_screen -bd 2 -relief flat',
	'wm withdraw .stock_spash_screen ',

	Splashy,
	'wm overrideredirect .stock_spash_screen 1 ',
	'label .stock_spash_screen.label -image app_splash_gif -bd 1 -relief flat ',
	'pack .stock_spash_screen.label -side top -expand 1 -fill both ',
	'wm geometry .stock_spash_screen +200+200 ',
	'wm deiconify .stock_spash_screen ',
	'update idletasks '],

	list_tcl_eval(CL, TclInterp, _),
	tcl_call(TclInterp, [update],_),

	RemoveCmd = tcl_eval(TclInterp, 'destroy .stock_spash_screen', _).

list_tcl_eval([], TclInterp, _).
list_tcl_eval([L | CL], TclInterp, _)
	:-
	tcl_eval(TclInterp,L, R),
	list_tcl_eval(CL, TclInterp, _).


export repair_path/1.
repair_path(ProjectFile)
	:-
	als_system(SysList),
	dmember(os=OpSys, SysList),
	OpSys = mswin32,
	!,
	atom_length(ProjectFile, Len),
	change_to_win_slashes(0,Len,ProjectFile).

repair_path(_).

change_to_win_slashes(CurPos,Len,ProjectFile)
	:-
	CurPos < Len,
	!,
	'$uia_peekb'(ProjectFile,CurPos,CharCode),
	(CharCode =:= 0'/ ->
		'$uia_pokeb'(ProjectFile,CurPos,0'\\)
		;
		true
	),
	NextPos is CurPos + 1,
	change_to_win_slashes(NextPos,Len,ProjectFile).

change_to_win_slashes(CurPos,Len,ProjectFile).


export relpath_from_to/3.
relpath_from_to('',ProjectFile,ProjectFile) :-!.

relpath_from_to(HomePath,ProjectFile,ProjectFile)
	:-
	builtins:is_absolute_path(ProjectFile),
	builtins:is_absolute_path(HomePath),
	sub_atom(ProjectFile,1,1,DD1),
	sub_atom(HomePath,1,1,DD2),
	DD1 \= DD2,
	!.

relpath_from_to(HomePath,ProjectFile,ProjectFile)
	:-
	path_elements(ProjectFile, [ProjectFile]),
	!.

relpath_from_to(HomePath,ProjectFile,RelPrjFile)
	:-
	path_elements(HomePath, HPEs),
	path_elements(ProjectFile, PFEs),
	remove_common_head(HPEs,PFEs,HPT,PFT),
	change_to_2dots(HPT, HPT2D),
	append(HPT2D,PFT,RPFEs),
	path_elements(RelPrjFile,RPFEs).

remove_common_head([],PFT,[],PFT).
remove_common_head(HPT,[],HPT,[]).
remove_common_head([X | HPEs],[X | PFEs],HPT,PFT)
	:-!,
	remove_common_head(HPEs,PFEs,HPT,PFT).
remove_common_head(HPT,PFT,HPT,PFT).

change_to_2dots([], []).
change_to_2dots([X | T], ['..' | CT])
	:-
	change_to_2dots(T, CT).


export setup_project_filename/3.
setup_project_filename(InFilename, Ext, FileName)
	:-
	(file_extension(InFilename,BaseFile,_) ->
		file_extension(FileName,BaseFile,Ext)
		;
		BaseFile = InFilename,
		file_extension(FileName,InFilename,Ext)
	).

export write_exports/2.
write_exports([], Stream) 
	:- 
	nl( Stream).
write_exports([PredArity | Rest], Stream) 
	:-
    printf( Stream,'export %t.\n', [PredArity ]), 
	printf( Stream,':-dynamic(%t).\n', [PredArity ]),
    write_exports( Rest, Stream).




endmod.
