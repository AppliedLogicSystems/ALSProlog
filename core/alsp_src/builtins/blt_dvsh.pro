/*=============================================================*
 |		blt_dvsh.pro
 |	Copyright (c) 1986-1996 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |	The tcltk gui environmental shell
 |
 |	Author: Ken Bowen
 |	Creation Date: 1997
 *=============================================================*/
 %% Loaded directly - see end of file:
%:-[db_srctr].

module builtins.
use tcltk.
use tk_alslib.


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%% 			TCLTK-BASED SHELL STARTUP			%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% Cold startup in saved image, from TTY-command line start (i.e,unix):
export start_alsdev/0.
start_alsdev
	:-
	make_clinfo(CLInfo, alsdev, true), 	% verbosity = quiet
	get_command_line_info(DefaultShellCall,CommandLine,ResidualCommandLine,CLInfo),
	assertz(command_line(ResidualCommandLine)),
	setup_debugger_stubs,
	setup_search_dirs(CLInfo),
	assert(save_clinfo(CLInfo)),
	library_setup,
	init_tk_alslib(shl_tcli,Shared),
builtins:sys_searchdir(ALSDIRPath),
path_elements(ALSDIRPath, Elements),
append(Elements, [images], ImagesList),
join_path(ImagesList, ImagesPath),
alsdev_splash(ImagesPath),
	load_cl_files(CLInfo),
	process_cl_asserts(CLInfo),
	alsdev(Shared).

	%% Warm startup (given that tty start_shell(prolog_shell) has run):
export alsdev/0.
alsdev
	:-
	consultmessage(CurValue),
	set_consult_messages(true),
	init_tk_alslib(shl_tcli,Shared),
	set_consult_messages(CurValue),
	alsdev(Shared).

export alsdev/1.
alsdev(Shared)
	:-
	sys_env(OS, _, _),
	(OS = macos ->
		tcl_call(shl_tcli, 'source -rsrc alsdev', _)
	  ;
	  (
	    pathPlusFile(Shared, 'alsdev.tcl', ALSDEVTCL),
	    tcl_call(shl_tcli, [source, ALSDEVTCL], _)
	  )
	),
		%% At this point, the windows have been created;

	tcl_call(shl_tcli, [destroy,'.als_splash_screen'], _),

	open(tk_win(shl_tcli, '.topals.text'), read, ISS, 
		[alias(shl_tk_in_win)
			,prompt_goal(user_prompt_goal(shl_tk_out_win))
		]),
	open(tk_win(shl_tcli, '.topals.text'), write, OSS, 
		[alias(shl_tk_out_win),write_eoln_type(lf)
		]),
	set_associated_output_alias(shl_tk_in_win, shl_tk_out_win),
	catenate('WaitForLine','.topals.text',WaitVar),
	catenate('DataLine','.topals.text',DataVar),

	tcl_call(shl_tcli, [set,WaitVar,0],_),
	tcl_call(shl_tcli, [set,DataVar,""],_),
	tcl_call(shl_tcli, 
		[set_top_bindings,'.topals.text',shl_tk_in_win,WaitVar,DataVar],_),
    sio:set_input(ISS),
    sio:set_output(OSS),

    %% Debugger streams: Initially regular debug io to console window:


/*****
		%	open(console('debugger output'),write, OutDStream,
	cancel_alias(debugger_output),
    open(tk_win(shl_tcli, '.topals.text'),write, OutDStream,
	 ['$stream_identifier'(-4), % alias(debugger_output),
	 	buffering(line),type(text), alias(console_debugger_output),
	 	maxdepth(8), line_length(76),
	 	depth_computation(nonflat)]),
	assign_alias(debugger_output, OutDStream),

		%	open(console('debugger input'), read, InDStream,
	cancel_alias(debugger_input),
    open(tk_win(shl_tcli, '.topals.text'), read, InDStream,
	 ['$stream_identifier'(-3), % alias(debugger_input),
	  	blocking(true), alias(console_debugger_input),
	 	prompt_goal(flush_output(debugger_output))]),
	assign_alias(debugger_input, InDStream),
*****/

    %% Debugger streams: The debugger window text window:
	%% Initially these don't have the debugger_[input,output] aliases;
	%% we cancel_alias & set_alias between the two pairs of streams,
	%% as the debugger window is popped up/down:


		%	open(console('debugger output'),write, OutGuiDStream,
	cancel_alias(debugger_output),
    open(tk_win(shl_tcli, '.debugwin.text'),write, OutGuiDStream,
	 ['$stream_identifier'(-8), alias(gui_debugger_output),
	 	buffering(line),type(text),
	 	maxdepth(8), line_length(76),
	 	depth_computation(nonflat)]),
	assign_alias(debugger_output, OutGuiDStream),

		%	open(console('debugger input'), read, InGuiDStream,
	cancel_alias(debugger_input),
    open(tk_win(shl_tcli, '.debugwin.text'), read, InGuiDStream,
	 ['$stream_identifier'(-7), blocking(true),alias(gui_debugger_input),
	 	prompt_goal(flush_output(debugger_output))]),
	assign_alias(debugger_input, InGuiDStream),
    %% Error stream
		%	open(console_error('error output'),write,OutEStream,
	cancel_alias(error_stream),
    open(tk_win(shl_tcli, '.topals.text'),write,OutEStream,
	 ['$stream_identifier'(-5), alias(error_stream),
	 	buffering(line),type(text)]),


    %% Establish additional aliases

	cancel_alias(warning_input),
    sio:set_alias(warning_input, ISS),
	cancel_alias(warning_output),
    sio:set_alias(warning_output, OSS),

	sio:reset_user(ISS,OSS),

	set_prolog_flag(windows_system, tcltk),
	current_prolog_flag(windows_system, Which),
	set_consult_messages(false),

	retract(save_clinfo(CLInfo)),
	ss_load_dot_alspro(CLInfo),
%	setup_prolog_flags,

	install_alarm_handler,
	shell_alarm_interval(AlarmIntrv),
	alarm(AlarmIntrv,AlarmIntrv),

	debugger:init_visual_debugger,
	change_debug_io(debugwin),

	get_cwd(CurDir),
	tcl_call(shl_tcli, [show_dir_on_main, CurDir], _),

	builtins:prolog_shell(ISS,OSS,alsdev).

alsdev_splash(Path)
	:-
	sys_env(OS, _, _),
	(OS = macos ->
		tcl_call(shl_tcli, 'source -rsrc als_splash', _)
		;
		(
			pathPlusFile(Path, 'als_splash.tcl', SplashFile),
			tcl_call(shl_tcli, [source, SplashFile], _)			
		)
	),
	tcl_call(shl_tcli, [splash, Path], _).

push_prompt(tcltk,OutStream,Prompt1)
	:-!,
	nl(OutStream),
	flush_output(OutStream),
	tcl_call(shl_tcli, [set_prompt_mark, '.topals.text'], _).



	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%			ALARM MANAGEMENT
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

install_alarm_handler
	:-
	asserta(global_handler(sigalrm,builtins,alarm_handler)).

alarm_handler(EventId, Goal, Context) 
	:-
	EventId \== sigalrm,
	!,
	propagate_event(EventId,Goal,Context).

alarm_handler(_,ModuleAndGoal,_) 
	:-
	tcl_call(shl_tcli, [wake_up_look_around], WW),
	disp_alarm_handler(WW, ModuleAndGoal).

disp_alarm_handler(-1, ModuleAndGoal)
	:-!,
	trigger_event(sigint, ModuleAndGoal).

disp_alarm_handler(_, ModuleAndGoal)
	:-
	ModuleAndGoal.

shell_alarm_interval(1.05).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%			MISC
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/***********
setup_prolog_flags
	:-
	sys_env(OS, _, _),
	not (OS = macos), 
	!,
	static_flags_info(StaticEqsList),
	add_static_items_to_menu(StaticEqsList, 
				shl_tcli, '.topals.mmenb.prolog.static').
setup_prolog_flags.

add_static_items_to_menu([], Interp, MenuPath).
add_static_items_to_menu([Item | List], Interp, MenuPath)
	:-
	sprintf(atom(Label), '%t', [Item]),
	tcl_call(Interp, [MenuPath,add,command,'-label',Label], _),
	add_static_items_to_menu(List, Interp, MenuPath).

***********/

endmod.   % builtins

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%			%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


module alsdev.
use tcltk.
use tk_alslib.

alsdev_default_setup(SystemDefaults)
	:-
	find_alsdev_ini(Items),
	window_defaults_setup('.topals',	Items,SystemDefaults),
	window_defaults_setup('.debugwin',	Items,SystemDefaults),
	window_defaults_setup('.document',	Items,SystemDefaults).

:- dynamic(alsdev_ini_path/1).

find_alsdev_ini(Items)
	:-
	sys_env(unix,_,_),
	!,
	getenv('HOME', HomeDir),
	split_path(HomeDir, HomeDirList),
	PrefsFile = '.alsdev',
	append(HomeDirList, [PrefsFile], PrefsFileList),

	fin_find_alsdev_ini(PrefsFileList, Items).

find_alsdev_ini(Items)
	:-
		%% not in unix:
	builtins:sys_searchdir(SSD),
	split_path(SSD, SSDList),
	append(ImageDirList, [alsdir], SSDList),
	(sys_env(mswin32,_,_) ->
		PrefsFile = 'alsdev.ini'
		;
			%% Macintosh:
		PrefsFile = 'alsdev_prefs'
	),
	append(ImageDirList, [PrefsFile], PrefsFileList),

	fin_find_alsdev_ini(PrefsFileList, Items).

fin_find_alsdev_ini(PrefsFileList, Items)
	:-
	join_path(PrefsFileList, PrefsFilePath),
	exists_file(PrefsFilePath),
	!,
	assert(alsdev_ini_path(PrefsFilePath)),
	grab_terms(PrefsFilePath, Items).

fin_find_alsdev_ini(PrefsFileList, [])
	:-
	join_path(PrefsFileList, PrefsFilePath),
		% Make a trivial file; put space in it to avoid
		% os problems with empty files...
	open(PrefsFilePath, write, S, []),
	put_code(S, 0' ),
	close(S),
	assert(alsdev_ini_path(PrefsFilePath)).

window_defaults_setup(WinGroup,Items,SystemDefaults)
	:-
	(dmember(window_settings(WinGroup,IniSettings), Items) -> 
		true 
		; 
		IniSettings = []),
	establish_window_defaults(IniSettings, SystemDefaults, WinGroup).

establish_window_defaults(Ini, SystemDefaults, WinGroup)
	:-
	setup_win_default(background,Back,Ini,SystemDefaults),
	setup_win_default(foreground,Fore,Ini,SystemDefaults),
	setup_win_default(selectbackground,SelectBack,Ini,SystemDefaults),
	setup_win_default(selectforeground,SelectFore,Ini,SystemDefaults),
	setup_win_default(font,Font,Ini,SystemDefaults),
	setup_win_default(tabs,Tabs,Ini,SystemDefaults),

	tcl_call(shl_tcli, [set_proenv,WinGroup,background,Back], _),
	tcl_call(shl_tcli, [set_proenv,WinGroup,foreground,Fore], _),
	tcl_call(shl_tcli, [set_proenv,WinGroup,selectbackground,SelectBack], _),
	tcl_call(shl_tcli, [set_proenv,WinGroup,selectforeground,SelectFore], _),
	tcl_call(shl_tcli, [set_proenv,WinGroup,font,Font], _),
	tcl_call(shl_tcli, [set_proenv,WinGroup,tabs,Tabs], _).


setup_win_default(What,Var,Ini,SystemDefaults)
	:-
	dmember(What = Var, Ini),
	!.
setup_win_default(What,Var,Ini,SystemDefaults)
	:-
	dmember([What,Var], SystemDefaults),
	Var \= '',
	!.
setup_win_default(background,'#d9d9d9', _,_).
setup_win_default(foreground,'Black', _,_).
setup_win_default(selectbackground,'#c3c3c3', _,_).
setup_win_default(selectforeground,'Black', _,_).
setup_win_default(font,'user 10 normal', _,_).
setup_win_default(tabs,'', _,_).

change_settings(WinSettingsVals, WinGroup)
	:-
	WinSettingsVals = [Back, Fore, SelectBack, SelectFore, Font, Tabs],
	WinSettings = 
		window_settings(WinGroup, [background=Back, foreground=Fore, 
					 selectbackground=SelectBack, selectforeground=SelectFore, 
					 font=Font, tabs=Tabs ]),
	modify_settings(WinSettings,window_settings, 2, WinGroup).

modify_settings(NewTerm, Functor, Arity, Arg1)
	:-
	alsdev_ini_path(ALSDEVINIPath),
	!,
	grab_terms(ALSDEVINIPath, OldTerms),
	replace_items(OldTerms, NewTerm, Functor, Arity, Arg1, NewTerms),
	open(ALSDEVINIPath, write, OutS, []),
	write_clauses(OutS, NewTerms, [quoted(true)]),
	close(OutS).

modify_settings(NewTerm, Functor, Arity, Arg1)
	:-
	open('alsdev.ini', write, OutS, []),
	write_clauses(OutS, [NewTerm]),
	close(OutS).

replace_items([],  NewTerm, Functor, Arity, Arg1,  [NewTerm]).
replace_items([Old | OldTerms],  NewTerm, Functor, Arity, Arg1,  [NewTerm | OldTerms])
	:-
	functor(Old, Functor, Arity),
	arg(1, Old, Arg1),
	!.
replace_items([Old | OldTerms],  NewTerm, Functor, Arity, Arg1,  [Old | NewTerms])
	:-
	replace_items(OldTerms,  NewTerm, Functor, Arity, Arg1,  NewTerms).

setup_defaults([], _).
setup_defaults([Tag=Value | TextSettings], Group)
	:-
	tcl_call(shl_tcli, [set_proenv,text,Tag,Value], _),
	setup_defaults(TextSettings, Group).



	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%			RECONSULT ETC.						%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
do_reconsult(PathAtom)
	:-
	sys_env(OS,_,_),
	(xconsult:source_level_debugging(on) ->
		ThePath = source(PathAtom)
		;
		ThePath = PathAtom
	),
	perf_reconsult(OS, ThePath).
*/

do_reconsult(PathAtom)
	:-
	sys_env(OS,_,_),
	perf_reconsult(OS, PathAtom).

perf_reconsult(unix, PathAtom)
	:-!,
	reconsult(PathAtom).

perf_reconsult(mswin32, PathAtom0)
	:-!,
		%% repair path (G:/ ...)
	repair_path(PathAtom0, PathAtom),
	reconsult(PathAtom).

perf_reconsult(macos, PathAtom)
	:-!,
		%% any repairs ??
	reconsult(PathAtom).

repair_path(source(PathAtomIn), source(PathAtomOut))
	:-!,
	repair_path(PathAtomIn, PathAtomOut).
repair_path(PathAtomIn, PathAtomOut)
	:-
	atom_codes(PathAtomIn, PAInCs),
	repair_path0(PAInCs, PAOutCs),
	atom_codes(PathAtomOut, PAOutCs).

repair_path0([], []).
repair_path0([0'/ | PAInCs], [0'\\ | PAOutCs])
	:-
	repair_path0(PAInCs, PAOutCs).
repair_path0([C | PAInCs], [C | PAOutCs])
	:-
	repair_path0(PAInCs, PAOutCs).

clear_workspace
	:-
	findall(MM, non_system_module(MM), UserMods),
	clear_each_module(UserMods).

clear_each_module([]).
clear_each_module([M | UserMods])
	:-
	printf(shl_tk_out_win, 'Module %t cleared.\n',[M]),
	findall(P/A, procedures(M,P,A,_), ML),
	M:abolish_list(ML),
	clear_each_module(UserMods).

non_system_module(M)
	:-
	modules(M, _),
	not((builtins:noshowmod(M))),
	not(library_module(M)).

	%% Needs to be done better:
library_module(avl).
library_module(cref).
library_module(macroxp).
library_module(shellmak).
library_module(ttyshlmk).

do_source_tcl(TclInterp, File)
	:-
	tcl_call(TclInterp, [source, File], X),
	printf(user_output, 'Tcl file %t sourced in Tcl interpreter %t\n', [File,TclInterp]).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%			DEFSTRUCT HANDLING					%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


install_defstruct(Basic, Elts)
	:-
	Basic = [IName,Make,Access,Set],
	atomread(IName,Name),
	rd_defstrc_elts(Elts, EltsTerms),
	DS = defStruct(Name, [
			propertiesList = EltsTerms,
			accessPred = Access,
			setPred = Set,
			makePred = Make,
			structLabel = Name
			] ),
	file_select_dialog(shl_tcli,
						[title = 'Defstruct file name:',
						 defaultname = deflttyp,
						 mode = save_as,
						 ext = '.typ',
						 filetypes = [ ['*.typ',[typ]],['All Files',['*']] ]
						],
						DefstrFile),
	(exists_file(DefstrFile) ->
		open(DefstrFile,append,TmpTS,[])
		;
		path_elements(DefstrFile,PathElts),
		last(PathElts, BasicFile),
		open(DefstrFile,write,TmpTS,[]),
		defstrfile_header(BasicFile, TmpTS)
	),
	write_defstruct(TmpTS, DS),
	close(TmpTS),
	sprintf(atom(Msg),'Defstruct %t added to file %t',[Name,DefstrFile]),
	info_dialog(shl_tcli, Msg, 'Defstruct finished').

defstrfile_header(BasicFile, S)
	:-
	date(Date),time(Time),
	printf(S,'/*========================================================================*\n',[]),
	printf(S,' |\t\t%s\n', [BasicFile]),
	printf(S,' |\n', []),
	printf(S,' |\tStarted %t - %t\n', [Date,Time]),
	printf(S,' *========================================================================*/\n\n',[]).

rd_defstrc_elts([], []).
rd_defstrc_elts([Elt | Elts], [EltTerm | EltsTerms])
	:-
	rd_defstruct_elt(Elt, EltTerm),
	rd_defstrc_elts(Elts, EltsTerms).

rd_defstruct_elt(Elt, EltTerm)
	:-
	atom_codes(Elt, ECs),
	asplit0(ECs, 0'/, NameCs, TailCs),
	asplit0(TailCs, 0'%, DefaultCs, CmtCs),
	bufread(NameCs, Name, [ vars_and_names(Vars1,VNames1)]),
	Vars1=VNames1,
	(DefaultCs = [] ->
		Default = nil
		;
		catch((
			bufread(DefaultCs, IIDefault,
				[vars_and_names(Vars2,VNames2),syntax_errors(quiet)]),
			Vars2=VNames2,
			(IIDefault = end_of_file -> Default = nil ; Default = IIDefault)
			),
			_,
			Default = nil)
	),
	atom_codes(Cmt, CmtCs),
	EltTerm = [Name, Default, Cmt].

write_defstruct(S, 
	defStruct(Name, [
			propertiesList = EltsTerms,
			accessPred = Access,
			setPred = Set,
			makePred = Make,
			structLabel = Name
			] )		)
	:-
	printf(S, 'defStruct(%t, [\n', [Name],[quoted(true)]),
	printf(S, '   propertiesList = [\n', []),
	write_defstr_props(EltsTerms, S),
	printf(S, '      ],\n', []),
	printf(S, '      accessPred = %t,\n', [Access],[quoted(true)]),
	printf(S, '      setPred = %t,\n',    [Set],[quoted(true)]),
	printf(S, '      makePred = %t,\n',   [Make],[quoted(true)]),
	printf(S, '      structLabel = %t\n',[Name],[quoted(true)]),
	printf(S, '      ]).\n\n', []).

write_defstr_props([],_).
write_defstr_props([Elt], S)
	:-
	write_defstr_propelt(Elt, S).
write_defstr_props([Elt | EltsTerms], S)
	:-
	write_defstr_propelt(Elt, S),
	put_code(S, 0',),
	nl(S),
	write_defstr_props(EltsTerms, S).

	%% EltTerm = [Name, Default, Cmt]
write_defstr_propelt([Name, Default, Cmt], S)
	:-
	printf(S, '      %t', [Name],[quoted(true),line_length(1000)]),
	(Default = nil ->
		true
		;
		printf(S, '/%t', [Default],[quoted(true),line_length(1000)])
	),
	printf(S, '\t\t- %t', [Cmt],[quoted(true),line_length(1000)]).

read_typ_file(FileName, DefStrNames, OriginalTypDefs)
	:-
	grab_terms(FileName, OriginalTypDefs),
	findall(NN,  member(defStruct(NN, _), OriginalTypDefs), DefStrNames).

edit_defstruct
	:-
	file_select_dialog(shl_tcli,
						[title = 'Defstruct file to open:',
						 ext = '.typ',
						 filetypes = [ ['*.typ',['.typ']],['All Files',['*']] ]
						],
						DefstrFile),
	read_typ_file(DefstrFile, DefStrNames, OriginalTypDefs),
	popup_select_items(shl_tcli, DefStrNames, [title='DefStruct to Edit:'], [EditDefStr]),
	write(got=EditDefStr),nl,
	dmember(defStruct(EditDefStr, EditDefStrBody), OriginalTypDefs).


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%			PROJECT HANDLING					%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- make_gv('ProjectsOpen').
:- setProjectsOpen([]).


open_project(ProjectFile)
	:-
printf(user_output, 'Wants to open project file %t\n', [ProjectFile]),
	getProjectsOpen(OpenProjects),
		%% can go away when multiple projects can be open:
	OpenProjects = [],
	!,
	read_project_file(ProjectFile, ProjectTerm, ProjectData),
	setup_project(ProjectTerm, ProjectData),
	setProjectsOpen([ProjectTerm | OpenProjects]).

open_project(ProjectFile)
	:-
	getProjectsOpen([CurProjectTerm | _ ]),
	project_name(CurProjectTerm, CurProjectName),
	sprintf(atom(Command),
			'Project %t still open. Close it first',
			[CurProjectName]),
	user_dialog(Command, _, [type=command]).



new_project
	:-
printf(user_output, 'Wants to start new project\n', []),
	tcl_call(shl_tcli, input_item, ItemIn),
write(item_gotten=ItemIn),nl.

save_project
	:-
printf(user_output, 'Wants to save project\n', []).

save_as_project(ProjectFile)
	:-
printf(user_output, 'Wants to save project as file %t\n', [ProjectFile]).

close_project
	:-
printf(user_output, 'Wants to close project\n', []).

add_file_to_project(File)
	:-
printf(user_output, 'Wants to add file %t to project\n', [File]).

delete_file_from_project
	:-
printf(user_output, 'Wants to delete file from project\n', []).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%% 		
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup_for_debugging(Which)
	:-
	xconsult:change_source_level_debugging(Which).

endmod.   % alsdev.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%% 		VISUAL DEBUGGER ROUTINES				%%%%%
	%%%%% 		  -- portions in module builtins		%%%%% 
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


module builtins.


change_debug_io(console)
	:-
	current_alias(debugger_input, CurDBInStream),
	current_alias(debugger_output, CurDBOutStream),
	stream_identifier(CurDBInStream, CurInSID),
	stream_identifier(CurDBOutStream, CurOutSID),
	(CurInSID = -3 -> true
		;
		current_alias(console_debugger_input, NewDBInStream),
		cancel_alias(debugger_input),
		assign_alias(debugger_input, NewDBInStream)
	),
	(CurOutSID = -4 -> true
		;
		current_alias(console_debugger_output, NewDBOutStream),
		cancel_alias(debugger_output),
		assign_alias(debugger_output, NewDBOutStream)
	),
	debugger:set_debug_io(nowins).

change_debug_io(debugwin)
	:-
	current_alias(debugger_input, CurDBInStream),
	current_alias(debugger_output, CurDBOutStream),
	stream_identifier(CurDBInStream, CurInSID),
	stream_identifier(CurDBOutStream, CurOutSID),
	(CurInSID = -7 -> true
		;
		current_alias(gui_debugger_input, NewDBInStream),
		cancel_alias(debugger_input),
		assign_alias(debugger_input, NewDBInStream)
	),
	(CurOutSID = -8 -> true
		;
		current_alias(gui_debugger_output, NewDBOutStream),
		cancel_alias(debugger_output),
		assign_alias(debugger_output, NewDBOutStream)
	),
	debugger:set_debug_io(tcltk).

		%%-----------------------------------------
		%%	Spy choices, etc. - in module builtins
		%%-----------------------------------------

export sys_modules/1.
sys_modules([
	builtins,syscfg,rel_arith,xconsult,sio,
	pgm_info,debugger,tcltk,windows,tk_alslib,alsdev,utilities,
	alsshell,avl,cref,macroxp,objects,shellmak,ttyshlmk
	]).

export non_sys_modules/1.
non_sys_modules(Mods)
	:-
	findall(M, modules(M,_),L),
	sys_modules(SysMs),
	list_diff(L, SysMs, Mods).

export module_preds/2.
export module_preds/3.

module_preds(M,L)
	:-
	findall(PA, 
		(all_procedures(M,P,A,R), R\=0,atom_codes(P,[PC0|_]),
			PC0<129, catenate([P,'/',A],PA)
		), 
		L0),
	(L0 = [] ->
		catenate('No predicates defined in ',M,Msg),
		L = [Msg] 
		; 
		sort(L0, L1),
		L = L1
	).

module_preds(Mod,S,L)
	:-
	findall(PA, (debugger:spying_on(CallForm,Mod),
					functor(CallForm,P,A),
					catenate([P,'/',A],PA)  ),
					S0),
	sort(S0, S),

	findall(PA, 
		(all_procedures(Mod,P,A,R), R\=0,atom_codes(P,[PC0|_]),
			PC0<129, catenate([P,'/',A],PA)
		), 
		L0),
	(L0 = [] ->
		catenate('No predicates defined in ',Mod,Msg),
		L = [Msg] 
		; 
		sort(L0, L1),
		remove(S, L1, L)
	).

remove([], L, L).
remove([Item | S], L1, L)
	:-
	delete_sorted(L1, Item, L2),
	remove(S, L2, L).

delete_sorted([], Item, []).
delete_sorted([Item | L], Item, L)
	:-!.
delete_sorted([H | L], Item, L)
	:-
	H @> Item,
	!.

delete_sorted([H | L1], Item, [H | L2])
	:-
	delete_sorted(L1, Item, L2).

endmod.	%% builtins

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%% 		VISUAL DEBUGGER ROUTINES				%%%%%
	%%%%% 		  -- portions in module builtins		%%%%% 
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

module debugger.
use tcltk.

init_visual_debugger
	:- 
	clause(get_db_file_recs(_),B),
	!.

init_visual_debugger
	:- 
	make_gv('_db_file_recs'), set_db_file_recs([]),
	make_gv('_dbfr_tbl'),
	functor(DBFRTBL, dbfrt, 100), 
	set_all_args(1,100,DBFRTBL,0),
	set_dbfr_tbl(DBFRTBL),
	make_gv('_mrfcg'), set_mrfcg(0).

get_src_trace_rec(FileName, Rec)
	:-
	get_db_file_recs(RecsList),
	member(Rec, RecsList),
	access_dbstr(filename, Rec, FileName),
	!.

add_src_trace_rec(Rec)
	:-
	get_db_file_recs(RecsList),
	set_db_file_recs([Rec | RecsList]).

remove_src_trace_rec(Rec)
	:-
	get_db_file_recs(RecsList),
	list_delete(RecsList,Rec,RestRecsList),
	set_db_file_recs(RestRecsList).

get_st_rec_by_fcg(FCGNum, Rec)
	:-
	get_dbfr_tbl(DBFRTBL),
	arg(FCGNum, DBFRTBL, Rec).


start_src_trace(BaseFileName, SrcFilePath)
	:-
	init_visual_debugger,
	set_mrfcg(0),
	tcl_call(shl_tcli, [ensure_db_showing], _),
		%% Old Rec exists, but window has been destroyed, so
		%% remove old rec, and start over:
	!,
	continue_start_src_trace(BaseFileName, SrcFilePath).

continue_start_src_trace(BaseFileName, SrcFilePath)
	:-
%write(continue_start_src_trace(BaseFileName, SrcFilePath)),nl,flush_output,
	(get_src_trace_rec(SrcFilePath, OldRec) ->
		access_dbstr(winname, OldRec, OldWinName),
		tcl_call(shl_tcli, [careful_withdraw,OldWinName], _),
		remove_src_trace_rec(OldRec)
		;
		true),
	create_src_trace(SrcFilePath, WinName, TextWinName, BaseFileName, 
					 NumLines, LineIndex),
	inverted_index(LineIndex, InvertedLineIndex),
	builtins:file_clause_group(BaseFileName, FCG),
	xmake_dbstr(Rec,
		[SrcFilePath,FCG,WinName, TextWinName,NumLines, 
			LineIndex,InvertedLineIndex,0,0]),
	add_src_trace_rec(Rec),
	get_dbfr_tbl(DBFRTBL),
	mangle(FCG, DBFRTBL, Rec),
	!,
	tcl_call(shl_tcli, [wm,deiconify,WinName], _).

inverted_index(LineIndex, InvertedLineIndex)
	:-
	inverted_index(LineIndex, 0, InvertedList),
	InvertedLineIndex =.. [ili | InvertedList].

inverted_index([], _, []).
inverted_index([NLCs | LineIndex], CurCharCount, [CurCharCount | List])
	:-
	NxtCharCount is CurCharCount + NLCs + 1,
	inverted_index(LineIndex, NxtCharCount, List).

create_src_trace(SrcFilePath, WinName, TextWinName, BaseFileName, 
				 NumLines, LineIndex)
	:-
	tcl_call(shl_tcli, [load_document,SrcFilePath],WinName),
	catenate(WinName,'.text',TextWinName),
	(BaseFileName = user -> 
		LoadRes = [0, []]
		;
		tcl_call(shl_tcli, [line_index_file,SrcFilePath],LoadRes),
		LoadRes = [NumLines, LineIndex]
	),
	tcl_call(shl_tcli, [TextWinName,configure,'-state',disabled],_),
	!,
	tcl_call(shl_tcli, [raise,'.debugwin',WinName],_).

check_leashing(Call,Exit,Redo,Fail)
	:-
	(leashed(call) -> Call = 1 ; Call = 0),
	(leashed(exit) -> Exit = 1 ; Exit = 0),
	(leashed(redo) -> Redo = 1 ; Redo = 0),
	(leashed(fail) -> Fail = 1 ; Fail = 0).

exec_toggle_leash(Port)
	:-
	(leashed(Port) ->
		retract(leashed(Port))
		;
		assert(leashed(Port))
	).

source_trace_closedown(STWin)
	:-
	debugger_abort.

set_debugwin_width(M5Size, MainWinSize)
	:-
	M1 is M5Size//5,
	NChars is (MainWinSize - 3*M1)//M1,
	set_line_length(debugger_output, NChars).


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Handle hidden debugging goals:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%%---------------------------------------------------
	%% 		'$dbg_aph'
	%%
	%% '$dbg_aph'(CG,Start,End)
	%%
	%% - Occurs as 1st goal in body (immediately after neck);
	%% - CG is the clause group for the clause;
	%% - (Start,End) are the file offsets of the head
	%%    of the clause.
	%%---------------------------------------------------

/*
showGoalToUserWin(Port,Box,Depth, Module, Goal, debug)
	:-
	get_mrfcg(ClsGrp),
printf('%t-%t(%t) %t <%t>\n',[Box,Depth,Port,Goal,ClsGrp]),flush_output,
	fail.
*/

	%% '$dbg_aph' ::

showGoalToUserWin(call,Box,Depth, Module, '$dbg_aph'(ClsGrp,Start,End), debug)
	:-
	color_my_port(ClsGrp,Start,End,call,head_tag),
	!.

	%% The call to color_my_port could fail on the very first call to
	%% a predicate defined in a file which has not yet been set up for
	%% debugging; this alternate clause tries to get the file set up:
showGoalToUserWin(call,Box,Depth, Module, '$dbg_aph'(ClsGrp,Start,End), debug)
	:-!,
	builtins:file_clause_group(BaseFileName, ClsGrp),
	builtins:consulted(BaseFileName, SrcFilePath, ObpPath, DebugType, Options),
	continue_start_src_trace(BaseFileName, SrcFilePath),
	!,
	color_my_port(ClsGrp,Start,End,call,head_tag).

showGoalToUserWin(fail,Box,Depth, Module, '$dbg_aph'(ClsGrp,Start,End), debug)
	:-!,
	color_my_port(ClsGrp,Start,End,fail,head_tag).
	
showGoalToUserWin(redo,Box,Depth, Module, '$dbg_aph'(ClsGrp,Start,End), debug)
	:-!,
	color_my_port(ClsGrp,Start,End,redo,head_tag).
	
showGoalToUserWin(exit,Box,Depth, Module, '$dbg_aph'(ClsGrp,Start,End), debug)
	:-!,
	fcg_change(Port,'$dbg_aph',ClsGrp,Start,End).

	%% '$dbg_aphe' ::

	% "call" here is successful exit from the clause:
showGoalToUserWin(call,Box,Depth, Module, '$dbg_aphe'(ClsGrp,Start,End), debug)
	:-!,
	color_my_port(ClsGrp,Start,End,exit,head_tag).

showGoalToUserWin(redo,Box,Depth, Module, '$dbg_aphe'(ClsGrp,Start,End), debug)
	:-!,
	color_my_port(ClsGrp,Start,End,redo,head_tag).

showGoalToUserWin(fail,Box,Depth, Module, '$dbg_aphe'(ClsGrp,Start,End), debug)
	:-!.

showGoalToUserWin(exit,Box,Depth, Module, '$dbg_aphe'(ClsGrp,Start,End), debug)
	:-!.

	%%---------------------------------------------------
	%% 		'$dbg_apf'
	%%
	%% '$dbg_apf'(CG,Start,End)
	%%
	%% - Occurs as sole goal in body of (originally) a fact.
	%% - CG is the clause group for the clause;
	%% - (Start,End) are the file offsets of the clause
	%%    head.
	%%---------------------------------------------------

showGoalToUserWin(call,Box,Depth, Module, '$dbg_apf'(ClsGrp,Start,End), debug)
	:-!,
	color_my_port(ClsGrp,Start,End,exit,head_tag).

showGoalToUserWin(redo,Box,Depth, Module, '$dbg_apf'(ClsGrp,Start,End), debug)
	:-!.

showGoalToUserWin(Port,Box,Depth, Module, '$dbg_apf'(ClsGrp,Start,End), debug)
	:-!.

	%%--------------------------------------------------------------------
	%% 		'$dbg_apg' and  '$dbg_apge'
	%%
	%% '$dbg_apg'(CG,Start,End)
	%%
	%% - Occurs (multiply) as goal in body, before the
	%%   goal it describes; 
	%% - CG is the clause group for the clause;
	%% - (Start,End) are the file offsets of the (real)
	%%    body goal immediately following this;
	%% -  '$dbg_apge'(CG,Start,End) uses the same args,
	%%    and occurs immediately after the goal:
	%%
	%% ...,'$dbg_apg'(CG,Start,End),Goal,'$dbg_apge'(CG,Start,End),,...
	%%
	%% NOTE: The first time we hit the call port for a given goal,
	%% we should all the offset/index computation, and store it in some
	%% associative array that we need to add to the STRec.
	%%--------------------------------------------------------------------

	%% '$dbg_apg' ::

	%% This is real call: first entry to the goal
showGoalToUserWin(call,Box,Depth, Module, '$dbg_apg'(ClsGrp,Start,End), debug)
	:-!,
	color_my_port(ClsGrp,Start,End,call,call_tag),
	!.

	%% "redo" here is actually failure of the acutal goal:
showGoalToUserWin(redo,Box,Depth, Module, '$dbg_apg'(ClsGrp,Start,End), debug)
	:-!,
	color_my_port(ClsGrp,Start,End,fail,call_tag).

showGoalToUserWin(Port,Box,Depth, Module, '$dbg_apg'(ClsGrp,Start,End), debug)
	:-!.

	%% '$dbg_apge' ::

	%% "call" here is really exit for the acutal goal:
showGoalToUserWin(call,Box,Depth, Module, '$dbg_apge'(ClsGrp,Start,End), debug)
	:-!,
	color_my_port(ClsGrp,Start,End,exit,call_tag).

	%% "redo" here is really redo for the acutal goal:
showGoalToUserWin(redo,Box,Depth, Module, '$dbg_apge'(ClsGrp,Start,End), debug)
	:-!,
	color_my_port(ClsGrp,Start,End,redo,call_tag).

showGoalToUserWin(_,Box,Depth, Module, '$dbg_apge'(ClsGrp,Start,End), debug)
	:-!.

	%%---------------------------------------------------
	%%		ALL OTHERS
	%%---------------------------------------------------
showGoalToUserWin(Port,Box,Depth, Module, XGoal, Response)
	:-
	printf(debugger_output,'(%d) %d %t: ', [Box,Depth,Port]),

%% should not be necessary, but there is a bug somewhere
%% in printf which allows it to be re-satisfied::!!::
	!,

	get_mrfcg(MRFCG),
	write_term(debugger_output, Module:XGoal,    [lettervars(false)]),
	flush_output(debugger_output),
	(MRFCG = 0 -> true
		;
		((Port=call; Port=redo) -> true
			;
			re_color_port(Port, MRFCG, STRec)
		)
	),
	(((Port = exit; Port = fail), Box = 1, Depth=1) ->
		db_boundary(Box,Depth,general,Port,XGoal,MRFCG),
		nl(debugger_output),
		Response = debug
		;
		tcl_call(shl_tcli, [set_status_debugwin,Port,Box,Depth], _),
		getResponse(tcltk,Port,Box,Depth, Module, XGoal, Response)
	),
	!.

color_my_port(ClsGrp,Start,End,Port,TagName)
	:-
	tktxt_info(ClsGrp,TextWin,Start,End,STRec,
				StartLine,StartChar,EndLine,EndChar),
	access_dbstr(TagName, STRec, LastCallTag),
	restore_remove(LastCallTag, TextWin, TagName),
	tcl_call(shl_tcli,
			[assign_tag,TextWin,TagName,StartLine,StartChar,EndLine,EndChar],
			_),
	port_color(Port, Color),
	fcg_change(Port,'??',ClsGrp,Start,End),
	set_mrfcg(ClsGrp),
	!,
	display_st_record(StartLine,StartChar,EndLine,EndChar, 
						Color,TextWin, TagName, STRec).

clear_my_tag(ClsGrp,Start,End,TagName)
	:-
	tktxt_info(ClsGrp,TextWin,Start,End,STRec,
				StartLine,StartChar,EndLine,EndChar),
	access_dbstr(TagName, STRec, LastCallTag),
	restore_remove(LastCallTag, TextWin, TagName),
	tcl_call(shl_tcli,
			[assign_tag,TextWin,TagName,StartLine,StartChar,EndLine,EndChar],
			_),
	clear_tag(TagName,TextWin).

re_color_port(Port, MRFCG, STRec)
	:-
	get_st_rec_by_fcg(MRFCG, STRec),
	access_dbstr(textwin, STRec, TextWin),
	port_color(Port, Color),
	configure_tag(call_tag, TextWin,['-background',Color]).

	%blue:
port_color(call, '#00acff').
port_color(exit, green).
	%red:
port_color(fail, '#fe0076').
port_color(redo, yellow).

db_boundary(Box,Depth,Desc,Port,XGoal,MRFCG)
	:-
	Box=1,
	Depth=1,
	!,
	((Port = exit; Port = fail) -> clean_up_src_win(MRFCG) ; true).

db_boundary(Box,Depth,Desc,Port,XGoal,MRFCG).

fcg_change(Port,DBG,ClsGrp,Start,End)
	:-
	get_mrfcg(MRFCG),
	MRFCG \= ClsGrp,
	!,
	flush_output,
	((Port = exit; Port = fail) -> 
		clean_up_src_win(MRFCG) 
		; 
		true).

fcg_change(Port,DBG,ClsGrp,Start,End).

clean_up_src_win(MRFCG)
	:-
	get_st_rec_by_fcg(MRFCG, Rec),
	Rec \= 0,
	!,
	access_dbstr(textwin, Rec, TextWin),
		% clear_tag(call_tag,TextWin),
	tcl_call(shl_tcli, [clear_tag, call_tag, TextWin], _),
	tcl_call(shl_tcli, [TextWin,tag, delete, call_tag], _),
		% clear_tag(head_tag,TextWin),
	tcl_call(shl_tcli, [clear_tag, head_tag, TextWin], _),
	tcl_call(shl_tcli, [TextWin,tag, delete, head_tag], _).

clean_up_src_win(MRFCG).

getresponse2(tcltk,Port,Box,Depth,Module,Goal,Response)
	:-
	short_deb_resps(Resps),
	tcl_call(shl_tcli, [wait_for_debug_response],RawResponse),
	(sub_atom(RawResponse,1,1,'B') ->
		nl(debugger_output),flush_output(debugger_output),
		sub_atom(RawResponse, 2, 1, RR)
		;
		RR=RawResponse
	),
	dmember(RR-Resp0,Resps),
	!,
	act_on_response(Resp0,Port,Box,Depth, Module,Goal,tcltk,Response).
		

gui_spy(Module, PredDesc)
	:-
	(sub_atom(PredDesc,0,_,_,'No predicates ') ->
		true
		;
		atomread(PredDesc, Pred/Arity),
		spy(Module, Pred, Arity)
	).

set_chosen_spypoints([], Mod).
set_chosen_spypoints([PredDesc | TclPAList], Mod)
	:-
	gui_spy(Mod, PredDesc),
	set_chosen_spypoints(TclPAList, Mod).

tktxt_info(ClsGrp,TextWin,Start,End,Rec,
			StartLine,StartChar,EndLine,EndChar)
	:-
	get_st_rec_by_fcg(ClsGrp, Rec),
	Rec \= 0,
	access_dbstr(textwin, Rec, TextWin),
	access_dbstr(numlines, Rec, NumLines),
	access_dbstr(invlineindex, Rec, InvLineIndex),
	find_line_pos(1,NumLines,Start,InvLineIndex,StartLine),
	arg(StartLine,InvLineIndex,SLOffset),
	StartChar is Start - SLOffset,
	find_line_pos(1,NumLines,End,InvLineIndex,EndLine),
	arg(EndLine, InvLineIndex, ELOffset),
	!,
	EndChar is End - ELOffset.


display_st_record(StartLine,StartChar,EndLine,EndChar, 
						Color,TextWin, TagName, Rec)
	:-
	configure_tag(TagName, TextWin,['-background',Color]),
	tcl_call(shl_tcli,[see_text,TextWin,StartLine,StartChar,EndLine,EndChar],_),
	!,
	set_dbstr(TagName, Rec, i(StartLine,StartChar,EndLine,EndChar)).

restore_remove(i(StartLine,StartChar,EndLine,EndChar), TextWin, TagName)
	:-!,
	tcl_call(shl_tcli, [clear_tag, TagName, TextWin], _),
	tcl_call(shl_tcli, [TextWin,tag, delete, TagName], _).

restore_remove(_, _, _).

assign_tag(TagName,TextWinName,StartIndex,EndIndex) 
	:-
	tcl_call(shl_tcli, [TextWinName,tag,add,TagName,StartIndex,EndIndex], _).

configure_tag(TagName,TextWinName,Pairs)
	:-
	dappend([TextWinName,tag,configure,TagName],Pairs,Cmd),
	!,
	tcl_call(shl_tcli, Cmd, _).

clear_tag(TagName,TextWinName)
	:-
	tcl_call(shl_tcli, [clear_tag,TagName,TextWinName], _).

find_line_pos(Offset, ILI, LN)
	:-
	functor(ILI,_,NN),
	find_line_pos(1,NN,Offset,ILI,LN).

find_line_pos(LowLN,HighLN,Offset,ILI,LowLN)
	:-
	HighLN is LowLN + 1, 
	!.

find_line_pos(LowLN,HighLN,Offset,ILI,LN)
	:-
	Diff is (HighLN - LowLN) // 2,
	(Diff = 0 -> MidLN is LowLN + 1 ; MidLN is LowLN + Diff),
	arg(MidLN, ILI, MidStart),
	disp_find_line_pos(MidStart,MidLN,LowLN,HighLN,Offset,ILI,LN).

disp_find_line_pos(MidStart,MidLN,LowLN,HighLN,Offset,ILI,LN)
	:-
	MidStart =< Offset,
	!,
	find_line_pos(MidLN,HighLN,Offset,ILI,LN).

disp_find_line_pos(MidStart,MidLN,LowLN,HighLN,Offset,ILI,LN)
	:-
	find_line_pos(LowLN,MidLN,Offset,ILI,LN).

clear_source_traces
	:-
	get_db_file_recs(RecsList),
	clear_source_trace_wins(RecsList),
	set_mrfcg(0).

clear_source_trace_wins([]).
clear_source_trace_wins([Rec | RecsList])
	:-
	access_dbstr(fcg_num,Rec,FCG),
	clean_up_src_win(FCG),
	clear_source_trace_wins(RecsList).

	%%-------------------------------------------------
	%		SPYPOINTS
	%%-------------------------------------------------
install_new_spypoints([], _) :-!.
install_new_spypoints(NewSpyListIn, Module)
	:-
	fixup_tcltk_pa_descs(NewSpyListIn, NewSpyList),
    dbg_spyoff,
	install_spypoints_in_mod(NewSpyList, Module),
    setPrologInterrupt(spying),
    setDebugInterrupt(spying),
	printf(debugger_output,
			'Spypoints set in module %t on:\n\t%t\n',
			[Module, NewSpyList]),
    dbg_spyon.


fixup_tcltk_pa_descs([], []).
fixup_tcltk_pa_descs([Desc | ListIn], [P/A | List])
	:-
	atomread(Desc,P/A), 
	fixup_tcltk_pa_descs(ListIn, List).

install_spypoints_in_mod([], Module).
install_spypoints_in_mod([P/A | NewSpyList], Module)
	:-
    install_spypoint(Module,P,A),
	install_spypoints_in_mod(NewSpyList, Module).

remove_old_spypoints(OldSpyListIn, Module)
	:-
	fixup_tcltk_pa_descs(OldSpyListIn, OldSpyList),
	remove_spypoints_in_mod(OldSpyList, Module),
    check_spyoff.

remove_spypoints_in_mod([], Module).
remove_spypoints_in_mod([P/A | NewNoSpyList], Module)
	:-
	nospy(Module,P,A),
	remove_spypoints_in_mod(NewNoSpyList, Module).

	%% dumps procedure clauses with complete info, 
	%% including hidden debug calls:

export debug_dump/1.
debug_dump(PA) 
	:-
	builtins:clauses_for_listing(PA,DBR),
	builtins:'$source'(DBR,S,show_pp),
	builtins:write_out(S),
	fail.
debug_dump(_).


endmod.




module alsdev.

%%%%%%%%%%%%%%%%%%%%%%%%%%

export col2/1.
col2(B)
	:-
	tcl_call(shl_tcli, [do_2col,B], X).


endmod.


/*-------------------------------------------------------------*
 *-------------------------------------------------------------*/

module utilities.
use debugger.
endmod.

module debugger.
use utilities.


%--- dbstr defStruct ---

export access_dbstr/3.
export set_dbstr/3.
access_dbstr(filename,_A,_B) :- arg(1,_A,_B).
set_dbstr(filename,_A,_B) :- mangle(1,_A,_B).

access_dbstr(fcg_num,_A,_B) :- arg(2,_A,_B).
set_dbstr(fcg_num,_A,_B) :- mangle(2,_A,_B).

access_dbstr(winname,_A,_B) :- arg(3,_A,_B).
set_dbstr(winname,_A,_B) :- mangle(3,_A,_B).

access_dbstr(textwin,_A,_B) :- arg(4,_A,_B).
set_dbstr(textwin,_A,_B) :- mangle(4,_A,_B).

access_dbstr(numlines,_A,_B) :- arg(5,_A,_B).
set_dbstr(numlines,_A,_B) :- mangle(5,_A,_B).

access_dbstr(linesizes,_A,_B) :- arg(6,_A,_B).
set_dbstr(linesizes,_A,_B) :- mangle(6,_A,_B).

access_dbstr(invlineindex,_A,_B) :- arg(7,_A,_B).
set_dbstr(invlineindex,_A,_B) :- mangle(7,_A,_B).

access_dbstr(head_tag,_A,_B) :- arg(8,_A,_B).
set_dbstr(head_tag,_A,_B) :- mangle(8,_A,_B).

access_dbstr(call_tag,_A,_B) :- arg(9,_A,_B).
set_dbstr(call_tag,_A,_B) :- mangle(9,_A,_B).

export make_dbstr/1.
make_dbstr(_A) :- _A'=..'[dbstr,_B,_C,_D,_E,0,[],[],0,0].

export make_dbstr/2.
make_dbstr(_A,_B) :-
        struct_lookup_subst(
            [filename,fcg_num,winname,textwin,numlines,linesizes,
                invlineindex,head_tag,call_tag],
            [_C,_D,_E,_F,0,[],[],0,0],_B,_G),
        _A '=..' [dbstr|_G].



export xmake_dbstr/2.
xmake_dbstr(dbstr(_A,_B,_C,_D,_E,_F,_G,_H,_I),[_A,_B,_C,_D,_E,_F,_G,_H,_I]).

endmod.

module utilities.
typeProperties(dbstr,
    [filename,fcg_num,winname,textwin,numlines,linesizes,invlineindex,
        head_tag,call_tag]).
noteOptionValue(dbstr,_A,_B,_C) :- set_dbstr(_A,_C,_B).
endmod.



