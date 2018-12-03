/*=============================================================*
 |		blt_dvsh.pro
 |	Copyright (c) 1986-1996 Applied Logic Systems, Inc.
 |
 |	The tcltk gui environmental shell
 |
 |	Author: Ken Bowen
 |	Creation Date: 1997
 *=============================================================*/

		%%%!!!+++%%%!!!+++%%%!!!+++%%%!!!+++%%%!!!+++%%%!!!+++%%%!!!+++
		%!%!%! WARNING WARNING WARNING WARNING
		%  THE RESATISFIABILITY % BUG
		%  --- Load both debugger.pro and blt_dvsh.pro from source 
		%	(*.pro after *.obp's removed); 
		%  The improper restisfiability will occur when tracing 
		%  ?- p(X) in
		%
		%  p(X) :- q(X).
		%  q(X) :- r(X).
		%  r(a).
		%  r(b).
		%
		%  ---- remove the *.obp for this file, but make sure the
		%  the debugger is loaded from debugger.obp.; everything will
		%  be ok.
		%
		%	
		%%%!!!+++%%%!!!+++%%%!!!+++%%%!!!+++%%%!!!+++%%%!!!+++%%%!!!+++

/***
mkw32 :-
	reconsult(projects),
	reconsult(dbg_class),
	save_image('ALS Prolog',[select_lib(builtins, [debugger]),
		select_lib(library,[listutl1,miscterm,msc_ioin,objects,strctutl,strings,
					tcl_sppt,tk_alslib,typecomp])]).
***/

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%% 		PORTIONS OF IDE IN MODULE BUILTINS		%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

module builtins.

use tcltk.
use tk_alslib.

:- abolish(halt,0).

export halt/0.
halt :-
	tcl_call(shl_tcli, [exit_prolog], _).
%	pbi_halt.

%:- abolish('$start',0).
%'$start' :- start_alsdev.

:- abolish(continue_prolog_loop,1).

continue_prolog_loop(halt)
	:-!,
	halt.

continue_prolog_loop(exit)
	:-!,
	fail.

continue_prolog_loop(Status).

	
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%% 			TCLTK-BASED SHELL STARTUP			%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% Cold startup in saved image, from TTY-command line start (i.e,unix):

report_error(Error) :-
        catch(report_error0(Error), _, (pbi_write(Error), pbi_nl)).
report_error0(Error) :-
	tk_new(I),
	sprintf(atom(ErrorStr), '%t', [Error]),
	tcl_call(I, [tk_messageBox,
		'-icon', error, '-message', ErrorStr, '-type', ok], _).

:- dynamic(user:alsdev_history_file_locn/1).
:- dynamic(user:no_load_prev_alsdev_history/0).

export start_alsdev/0.
start_alsdev :-
	catch(start_alsdev0, Error, report_error(Error)).

start_alsdev0
	:-
		%% Used by some routines in builtins
		%% to  detect presence  of alsdev:
	assert(alsdev_running),
	builtins:sys_searchdir(ALSDIRPath),
	split_path(ALSDIRPath, ALSDIRPathList),

	setup_init_ide_classes(ALS_IDE_Mgr),

	join_path([ALSDIRPath,shared], Shared),
	tk_new(shl_tcli),

	tcl_call(shl_tcli, [wm,withdraw,'.'], _),
	tcl_call(shl_tcli, [set,'ALSTCLPATH',Shared], _),

	join_path([Shared,'als_tklib.tcl'],ALSTKLIB),
	tcl_call(shl_tcli, [source, ALSTKLIB], _),

	join_path([Shared, 'alsdev.tcl'], ALSDEVTCL),
	tcl_call(shl_tcli, [source, ALSDEVTCL], _),

	abolish(save_clinfo,1),
	make_clinfo(CLInfo, alsdev, true), 	% verbosity = quiet
	get_command_line_info(DefaultShellCall,CommandLine,ResidualCommandLine,alsdev,CLInfo),
	assertz(command_line(ResidualCommandLine)),

	setup_search_dirs(CLInfo),
	assert(save_clinfo(CLInfo)),

	library_setup(CLInfo),

/* WHY IS THIS MISSING?
#if (all_procedures(syscfg,intconstr,0,_))
		rel_arith:set_ics(cs(0,0,0))
#endif
*/
	append(ALSDIRPathList, [shared], ImagesList),
	join_path(ImagesList, ImagesPath),
	alsdev_splash(ImagesPath),

	process_cl_asserts(CLInfo),
	!,
	abolish(start_alsdev/0),
	alsdev(ALS_IDE_Mgr).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%       ALS_IDE  ObjectPro CLASS DEFINITIONS    %%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup_init_ide_classes(ALS_IDE_Mgr)
	:-

	pre_existing_srcmgrs(PreSM),
		%% For ALS IDE Project system:
	clone_stm_all(PreSM, InitSM),
	alsdev:setup_als_ide_mgr(ALS_IDE_Mgr),
		%% Since we're starting cold:
	(clause(get_primary_manager(_), _) ->
		true
		;
		make_gv('_primary_manager')
	),
	set_primary_manager(ALS_IDE_Mgr),
	setObjStruct(source_mgrs, ALS_IDE_Mgr, InitSM),
	send(ALS_IDE_Mgr, set_value(shell_module, alsdev)).

pre_existing_srcmgrs(PreSM)
	:-
	clause(get_primary_manager(_), _),
	get_primary_manager(OldMgr),
	OldMgr \= 0,
	accessObjStruct(source_mgrs, OldMgr, PreSM),
	!.
pre_existing_srcmgrs([]).

/*
:- defineClass(alsdev,
	[   name=als_ide_mgr,
		subClassOf=als_shl_mgr,
		module = alsdev,
		addl_slots=
			[ 
				debugger_mgr,   %% debugger state object
				cur_project,    %% current project manager object    
				edit_files,     %% list of files open for editing
				non_file_edits  %% list of non-file (new) windows open for editing
			],
		defaults= [ 
			edit_files = [], 
			non_file_edits = [] 
		]
	]).

        %%   SHL_SOURCE_HANDLER:
:- defineClass(alsdev,
	[   name=shl_source_handler,
		subClassOf=source_handler,
		export = yes,
		addl_slots= [ 
			tcl_doc_path,		%% Tcl id of edit window
			errors_display		%% nil / non-empty errs list
		],
		defaults= [ 
			tcl_doc_path		= nil,
			errors_display 		= nil
		]
	]).

        %%   SOURCE_TRACE_MGR:
:- defineClass(alsdev,
	[   name=source_trace_mgr,
		subClassOf=shl_source_handler,
		addl_slots=
			[
				debugger_mgr,		%% home to daddy...
				last_visual_load,	%% Time of last load of file text widget
				num_lines,			%% num lines in the file
				linesizes,			%% list of num chars in each line
				invlineindex,		%% list of char offsets to start of each line
				head_tag,			%% i(S,E) = last colored "matching head (aph)tag" lcn
				call_tag			%% i(S,E) = last colored "matching_call (apg)tag" lcn
			],
		defaults= [ 
			visible				= false,
			last_visual_load 	= 0,
			num_lines			= 0,
			head_tag			= 0,
			call_tag			= 0
			]
	]).
*/


clone_stm(SSH, STM)
	:-
	accessObjStruct(source_file,  SSH, SourceFile),
	accessObjStruct(base_file,    SSH, BaseFileName),
	accessObjStruct(ext,	 	  SSH, Ext),
	accessObjStruct(obp_file, 	  SSH, ObpFile),
	accessObjStruct(fcg, 	 	  SSH, FCG),
	accessObjStruct(consult_mode, SSH, ConsultMode),
	accessObjStruct(last_consult, SSH, LastConsult),
	alsdev:create_object(
		[instanceOf = source_trace_mgr,
		 handle = true,
		 values =
			[ 	source_type = file,
				base_file = BaseFileName,
				source_file = SourceFile,
				ext = 	Ext,
				obp_file =  ObpFile,
				fcg =  	FCG,
				consult_mode =  ConsultMode,
				last_consult =  LastConsult
			]
		],
		STM ).

clone_stm_all([], []).
clone_stm_all([fm(ID,SSH) | Input], [fm(ID,STM) | Output])
	:-
	clone_stm(SSH, STM),
	clone_stm_all(Input, Output).

%%%%%%%%%%----------------------------------------------------
export disp_ide/0.
disp_ide :-
	builtins:get_primary_manager(ALSMgr),
	write_term(user_output, ALSMgr, [maxdepth(3)]),
	nl(user_output),
	flush_output.

export disp_dbg_mgr/0.
disp_dbg_mgr :-
	builtins:get_primary_manager(ALSMgr),
	send(ALSMgr, get_value(debugger_mgr, Mgr)),
	write_term(user_output, Mgr, [maxdepth(3)]),
	nl(user_output),
	flush_output.

export disp_src_mgr/1.
disp_src_mgr(BN)
	:-
	builtins:get_primary_manager(ALSMgr),
	send(ALSMgr, get_value(source_mgrs, SML)),
	dmember(fm(BN, Mgr), SML),
	write_term(user_output, Mgr, [maxdepth(3)]),
	nl(user_output),
	flush_output.



%%%%%%%%%%----------------------------------------------------


	%% Warm startup (given that tty start_shell(prolog_shell) has run):
export alsdev/0.
alsdev
	:-
		%% For ALS IDE Project system:
	setup_init_ide_classes(ALS_IDE_Mgr),
		%% Since we're starting warm:
		% make_gv('_primary_manager'),
	set_primary_manager(ALS_IDE_Mgr),
	init_tk_alslib(shl_tcli,Shared),
	alsdev(Shared, ALS_IDE_Mgr).

:- dynamic(dvf/0).

export alsdev/1.
alsdev(ALS_IDE_Mgr)
	:-
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

	tcl_call(shl_tcli, [set_top_bindings,'.topals.text',shl_tk_in_win,WaitVar,DataVar],_),

    	sio:set_input(ISS),
    	sio:set_output(OSS),

	setup_debugger_streams(ISS,OSS),

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


		%% For ALS IDE Project system:
	alsdev:setup_ide_project_globals(ALS_IDE_Mgr),
	tcl_call(shl_tcli, [do_main_als_bindings],_),

	initial_flags_settings,
	initial_misc_settings,

	retract(save_clinfo(CLInfo)),
	ss_load_dot_alspro(CLInfo),

	arg(3, CLInfo, SpecFiles),
	alsdev:do_special_processing(SpecFiles),

	install_alarm_handler,
	shell_alarm_interval(AlarmIntrv),
	alarm(AlarmIntrv,AlarmIntrv),
	tcl_call(shl_tcli, [set_tcl_ga, proenv, heartbeat, AlarmIntrv], _),

	(clause(cl_debug_io(nowins),true) ->
		true
		;
		change_debug_io(debugwin)
	),
%	(current_prolog_flag(debug, on) ->
%		tcl_call(shl_tcli,[ensure_db_showing],_) ; true
%	),
	current_prolog_flag(debug, DebugValue),
	tcl_call(shl_tcli, [set_tcl_ga,proenv,debug,DebugValue], _),

	get_cwd(CurDir),
	tcl_call(shl_tcli, [show_dir_on_main, CurDir], _),
%	(clause(dvf,_) -> demo_init ; true),
	tdvf,
		%% for use by clear_workspace:
	findall(GVID, global_gv_info:gvi(GVID,_,_,_),SysGlobals),
	alsdev:assert(system_global_vars(SysGlobals)),

	alsdev:alsdev_ini_path(DotFilePath),
	handle_alsdev_history,
	tcl_call(shl_tcli, [manage_alsdev_history],_),
	prolog_shell(ISS,OSS,alsdev).

alsdev_splash(Path)
	:-
	sys_env(OS, _, _),
	(OS = macos ->
		tcl_call(shl_tcli, 'source -rsrc als_splash', _)
		;
		(
			join_path([Path, 'als_splash.tcl'], SplashFile),
			tcl_call(shl_tcli, [source, SplashFile], _)			
		)
	),
	tcl_call(shl_tcli, [splash, Path], _).


	/* ----------------------------
 	 | Manage alsdev_history file
 	 *----------------------------*/

handle_alsdev_history
	:-
	HistoryFile = '.alsdev_history',
        check_setup_alsdev_history_file(HistoryFile),
        check_load_prev_alsdev_history.

alsdev_history_file_locn(L) :- user:alsdev_history_file_locn(L), !.
alsdev_history_file_locn(home).

check_setup_alsdev_history_file(HistoryFile) :-
	builtins:ini_config_items(DotALSDEVItems),
	(member( alsdev_history_file_locn(HFL), DotALSDEVItems) -> true ; HFL = home), 
   	setup_alsdev_history_file(HFL, HistoryFile).

setup_alsdev_history_file(home, HistoryFile)
        :-!,
        getenv('HOME', UserHomePath),
        pathPlusFile(UserHomePath, HistoryFile, PathToHistoryFile),
	tcl_call(shl_tcli, [set_tcl_ga, proenv, alsdev_history_file, PathToHistoryFile],_).

setup_alsdev_history_file(local, HistoryFile)
        :-!,
	tcl_call(shl_tcli, [set_tcl_ga, proenv, alsdev_history_file, HistoryFile],_).

setup_alsdev_history_file(BadLocn, HistoryFile)
        :-
        write(user_output, 'Error: Bad History File Location' = BadLocn), nl,
        setup_alsdev_history_file(home, HistoryFile).

check_load_prev_alsdev_history
        :-
        user:no_load_prev_alsdev_history,
        !,
	tcl_call(shl_tcli, [set_tcl_ga, proenv, do_load_prev_alsdev_history, false],_).

check_load_prev_alsdev_history.


    %% Debugger streams: The debugger window text window:
	%% Initially these don't have the debugger_[input,output] aliases;
	%% we cancel_alias & set_alias between the two pairs of streams,
	%% as the debugger window is popped up/down:

setup_debugger_streams(ISS, OSS)
	:-
	clause(cl_debug_io(nowins),true),
	!,
	sio:set_alias(debugger_input, ISS),
	sio:set_alias(debugger_output, OSS).

setup_debugger_streams(_, _)
	:-
		%	open(console('debugger output'),write, OutGuiDStream,
	cancel_alias(debugger_output),
    open(tk_win(shl_tcli, '.debugwin.text'),write, OutGuiDStream,
	 ['$stream_identifier'(-8), alias(gui_debugger_output),
	 	buffering(line),type(text),
		line_length(76), 
%		maxdepth(8), depth_computation(nonflat)
		maxdepth(4), depth_computation(nonflat)
		]),
	assign_alias(debugger_output, OutGuiDStream),

		%	open(console('debugger input'), read, InGuiDStream,
	cancel_alias(debugger_input),
    open(tk_win(shl_tcli, '.debugwin.text'), read, InGuiDStream,
	 ['$stream_identifier'(-7), blocking(true),alias(gui_debugger_input),
	 	prompt_goal(flush_output(debugger_output))]),
	assign_alias(debugger_input, InGuiDStream).

push_prompt(tcltk,OutStream,Prompt1)
	:-!,
	nl(OutStream),
	flush_output(OutStream),
	tcl_call(shl_tcli, [set_prompt_mark, '.topals.text'], _).

export listener_prompt/0.
listener_prompt
	:-
	write(shl_tk_out_win, '?- '), 
	flush_output(shl_tk_out_win).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%			ALARM MANAGEMENT
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

install_alarm_handler
	:-
	sys_env(macos, _, _),
	enable_tcl_yield.
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
	%%%%%			MISC INTERFACE PREDICATES
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export change_heartbeat/1.
change_heartbeat(NewValue)
	:-
	abolish(shell_alarm_interval,1),
	assert(shell_alarm_interval(NewValue)).

export change_ide_stream_depth/1.
change_ide_stream_depth(NewMaxDepth)
	:-
 	stream_or_alias_ok(shl_tk_out_win, Stream),
    stream_wt_opts(Stream,WO),
    WO = wt_opts(LineLength,_,DepthComputation),	
    NewWO = wt_opts(LineLength,NewMaxDepth,DepthComputation),	
    set_stream_wt_opts(Stream,NewWO).

export change_ide_depth_type/1.
change_ide_depth_type(NewType)
	:-
 	stream_or_alias_ok(shl_tk_out_win, Stream),
	set_stream_wt_depth_computation(Stream,NewType).
/*
    stream_wt_opts(Stream,WO),
    WO = wt_opts(LineLength,MaxDepth,_),	
    NewWO = wt_opts(LineLength,MaxDepth,NewType),	
    set_stream_wt_opts(Stream,NewWO).
*/

export topals_settings_info/1.
topals_settings_info(TopALSSettings)
	:-
	sio:stream_or_alias_ok(user_output, OSS),
	sio:stream_wt_opts(OSS, DBGOPTs),
	shell_alarm_interval(AlarmIntrv),
	TopALSSettings = [stream_opts=DBGOPTs , heartbeat = AlarmIntrv].

export set_topals_settings_info/1.
set_topals_settings_info(TopALSSettings)
	:-
	(dmember( stream_opts=TopAlsOPTs, TopALSSettings) ->
		sio:stream_or_alias_ok(user_output, OSS),
		sio:set_stream_wt_opts(OSS, TopAlsOPTs)
		;
		true
	),
	(dmember(heartbeat = AlarmIntrv, TopALSSettings) ->
		change_heartbeat(AlarmIntrv)
		;
		true
	),
	TopAlsOPTs = wt_opts(_,PrintDepth,DepthType),
	tcl_call(shl_tcli, [set_tcl_ga, proenv, heartbeat, AlarmIntrv], _),
	tcl_call(shl_tcli, [set_tcl_ga, proenv, main_printdepth, PrintDepth], _),
	tcl_call(shl_tcli, [set_tcl_ga, proenv, main_depth_type, DepthType], _).

	/*-----------------------------------------------------------*
	 |	Extracts and sets prolog flags info from the info
	 |	obtained from alsdev.ini
	 *-----------------------------------------------------------*/

initial_flags_settings
	:-
	ini_config_items(Items),
	(dmember(prolog_value(prolog_flags,FlagsList),Items) ->
		set_flags_values(FlagsList)
		;
		true
	).

set_flags_values([]).
set_flags_values([[Flag,_,Val] | FlagsList])
	:-
	do_set_prolog_flag(Flag,Val),
	set_flags_values(FlagsList).

	/*-----------------------------------------------------------*
	 |	Extracts and sets other misc info such as print depth
	 |	obtained from alsdev.ini; must ensure that the streams
	 |	(e.g., debugger win stream) exists, etc., before running
	 |	this routine.
	 *-----------------------------------------------------------*/
initial_misc_settings
	:-
	ini_config_items(Items),
	(dmember(prolog_value(topals_settings,TopALSSettings),Items) ->
		set_topals_settings_info(TopALSSettings)
		;
		true
	),
	(dmember(prolog_value(debug_settings,DebugSettings),Items) ->
		debugger:set_debug_settings_info(DebugSettings)
		;
		true
	).

endmod.   % builtins



	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%% 		PORTIONS OF IDE IN MODULE ALSDEV		%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

module alsdev.
use tcltk.
use tk_alslib.
use alsshell.

	%%------------------------------------------
	%% Command line processing
	%%------------------------------------------

special_ss_parse_command_line(File, Tail, [], CLInfo)
	:-
	special_file_type(File, FileType, NoSuffixFile, Ext),
	!,
	store_special_file(FileType, File, NoSuffixFile, Ext, CLInfo).

special_file_type(File, prolog_project, NoSuffixFile, ppj)
	:-
	(file_extension(File, NoSuffixFile, ppj) ; file_extension(File, NoSuffixFile, 'PPJ')),
	!.

special_file_type(File, prolog_source, NoSuffixFile, pro)
	:-
	(file_extension(File, NoSuffixFile, pro) ; file_extension(File, NoSuffixFile, 'PRO')),
	!.

special_file_type(File, prolog_source, NoSuffixFile, pl)
	:-
	(file_extension(File, NoSuffixFile, pl) ; file_extension(File, NoSuffixFile, 'PL')),
	!.

special_file_type(File, prolog_script, NoSuffixFile, prs)
	:-
	file_extension(File, NoSuffixFile, prs),
	!.

special_file_type(File, other, NoSuffixFile, Ext)
	:-
	file_extension(File, NoSuffixFile, Ext),
	!.

store_special_file(FileType, File, NoSuffixFile, Ext, CLInfo)
	:-
	arg(3, CLInfo, SFiles),
	append(SFiles, [s(FileType, File, NoSuffixFile, Ext)], NewSFiles),
	mangle(3, CLInfo, NewSFiles).

do_special_processing([]).
do_special_processing([s(FileType, File, NoSuffixFile, Ext) | SFiles])
	:-
	special_cl_processing(FileType, File, NoSuffixFile, Ext),
	!,
	do_special_processing(SFiles).
do_special_processing([s(FileType, File, NoSuffixFile, Ext) | SFiles])
	:-!,
	printf(user_output,
		   'There was an error processing the %t command line file: %t\n',
		   [FileType, File]),
	do_special_processing(SFiles).
do_special_processing([Item | SFiles])
	:-!,
	printf(user_output, 'Error processing command line item: %t\n', [Item]),
	do_special_processing(SFiles).

special_cl_processing(prolog_project, File, NoSuffixFile, Ext)
	:-!,
	tcl_call(shl_tcli, [tkOpenDocument, File], _).

special_cl_processing(prolog_source, File, NoSuffixFile, Ext)
	:-!,
	canon_path(File, CanonSrcPath),
	exists_file(CanonSrcPath),
	builtins:get_primary_manager(ALSIDEMGR),
	send(ALSIDEMGR, open_edit_win(CanonSrcPath)).

special_cl_processing(prolog_script, File, NoSuffixFile, Ext)
	:-!,
	canon_path(File, CanonSrcPath),
	exists_file(CanonSrcPath),
	builtins:simple_load3(NoSuffixFile, CanonSrcPath, alsdev).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%	ALS IDE MANAGER CLASS & OBJECT 				%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
als_ide_mgrAction(WHAT, State)
	:-
pbi_write('WHAT'=WHAT),pbi_nl,
	fail.
*/

als_ide_mgrAction(refresh_wins, State)
    	:-
 	tcl_call(shl_tcli, [refresh_spy_preds_if_showing], _).

als_ide_mgrAction([Functor | Args], State)
	:-
	Msg =.. [Functor | Args],
	send_self(State, Msg).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%			%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		%% For ALS IDE Project system; called at 
		%% boot time:
setup_als_ide_mgr(ALSIDEObject)
	:-
		%% Get values for initial values in create_object:
	get_cwd(InitialDir),

	builtins:sys_searchdir(SSD),
	split_path(SSD,SSDL),
	append(SSDL, [library], LLL),
	join_path(LLL, LibPath),

	findall(searchdir(SD), (builtins:searchdir(SD)), SDList),

	create_object(
		[instanceOf=als_ide_mgr,
		 handle=true,
		 values = [
			initial_dir = InitialDir,
			prolog_library = LibPath,
			initial_search_dirs = SDList
		 ]
		], 
		ALSIDEObject),

	FCG_Index_Size = 50,		%% initial default; expandable later...
	functor(FCG_Index, fcgix, FCG_Index_Size),
	set_all_args(1, FCG_Index_Size, FCG_Index, nil),
	create_object(
		[instanceOf=debugger_mgr,
		 handle = true,
		 values = [
			fcg_index_size			= FCG_Index_Size,
			src_trace_mgrs_by_fcg	= FCG_Index
		 ]
		], 
		DebuggerObject),
	setObjStruct(debugger_mgr, ALSIDEObject, DebuggerObject).


setup_ide_project_globals(ALSIDEObject)
	:-
	send( ALSIDEObject, get_value(myHandle, ALSIDEHandle) ),
	tcl_call(shl_tcli, [set_tcl_ga, proenv, als_ide_mgr, ALSIDEHandle], _),
	accessObjStruct(debugger_mgr, ALSIDEObject, DebuggerObject),
	send( DebuggerObject, get_value(myHandle, DebuggerHandle) ),
	tcl_call(shl_tcli, [set_tcl_ga, proenv, debugger_mgr, DebuggerHandle], _).

	/*-----------------------------------------------------------*
	 |  1. Locates alsdev.ini (or .alsdev) & records path;
	 |	2. Asserts all the info it extracts, for later use;
	 |	3. Extracts basic geometry info for use in initial
	 |	   creation of .topals and .debugwin;
	 | * Called from alsdev.tcl: establish_defaults
	 *-----------------------------------------------------------*/
alsdev_ini_defaults(DefaultVals, TopGeom, DebugGeom, DebugVis)
	:-
	abolish(alsdev_ini_path,1),
	find_alsdev_ini(Items),
	builtins:assert(ini_config_items(Items)),
	(dmember(window_settings('.topals', TopIniSettings0), Items) -> 
		strip_tags(TopIniSettings0, TopIniSettings) 
		; 
		TopIniSettings = []
	),
	(dmember(window_settings('.debugwin', DebugIniSettings0), Items) -> 
		strip_tags(DebugIniSettings0, DebugIniSettings) 
		; 
		DebugIniSettings = []
	),
	(dmember(window_settings('.document', DocIniSettings0), Items) -> 
		strip_tags(DocIniSettings0, DocIniSettings) 
		; 
		DocIniSettings = []
	),
	DefaultVals = [TopIniSettings, DebugIniSettings, DocIniSettings],
	(dmember(window_position('.topals', TopGeom), Items) ->
		true ; TopGeom = [] ),
	(dmember(window_position('.debugwin', DebugGeom), Items) ->
		true ; DebugGeom = [] ),
	(dmember(visible('.debugwin',DebugVis), Items) -> 
		true
		;
		DebugVis = 0
	).
%	(dmember(visible('.debugwin',DebugVis), Items) -> 
%		(DebugVis = 1 ->
%			set_prolog_flag(debug,on)
%			;
%			set_prolog_flag(debug,off)
%		)
%		; 
%		DebugVis = 0,
%		set_prolog_flag(debug,off)
%	).

strip_tags([], []). 
strip_tags([(_ = V0) | TVs], [V | Vs])
	:-
	(V0 = '' -> 
		V = [] 
		; 
		V = V0
	),
	strip_tags(TVs, Vs).

:- dynamic(alsdev_ini_path/1).

getPrefsFilePath(unix,'.alsdev').

getPrefsFilePath(unix,PrefsFilePath)
	:-
	getenv('HOME', HomeDir),
	split_path(HomeDir, HomeDirList),
	PrefsFile = '.alsdev',
	append(HomeDirList, [PrefsFile], PrefsFileList),
	join_path(PrefsFileList, PrefsFilePath).

getPrefsFilePath(mswin32,PrefsFilePath)
	:-
	builtins:sys_searchdir(SSD),
	split_path(SSD, SSDList),
	(append(ImageDirList, [alsdir], SSDList) ; ImageDirList = SSDList),
	PrefsFile = 'alsdev.ini',
	append(ImageDirList, [PrefsFile], PrefsFileList),
	join_path(PrefsFileList, PrefsFilePath).

find_alsdev_ini(Items)
	:-
	sys_env(unix,_,_),
	!,
	getPrefsFilePath(unix,PrefsFilePath),
	finish_alsdev_ini(unix,PrefsFilePath,Items).

find_alsdev_ini(Items)
	:-   %% not in unix:
	sys_env(mswin32,_,_),
	!,
	getPrefsFilePath(mswin32,PrefsFilePath),
	finish_alsdev_ini(mswin32,PrefsFilePath,Items).

finish_alsdev_ini(unix,PrefsFilePath,Items)
	:-
	exists_file(PrefsFilePath),
	!,
	assert(alsdev_ini_path(PrefsFilePath)),
	grab_terms(PrefsFilePath, Items).

finish_alsdev_ini(mswin32,PrefsFilePath,Items)
	:-
	exists_file(PrefsFilePath),
	!,
	assert(alsdev_ini_path(PrefsFilePath)),
	grab_terms(PrefsFilePath, Items).

finish_alsdev_ini(_, PrefsFilePath,[])
	:-
	open(PrefsFilePath, write, S, []),
	put_code(S, 0' ),
	close(S),
	assert(alsdev_ini_path(PrefsFilePath)).

change_window_settings(WinSettingsVals, WinGroup)
	:-
	WinSettingsVals = [Back, Fore, SelectBack, SelectFore, Font, Tabs],
	WinSettings = 
		[ window_settings(WinGroup, 
		    [background=Back, foreground=Fore, 
		     selectbackground=SelectBack, selectforeground=SelectFore, 
			font=Font, tabs=Tabs ])
		],
	modify_settings(WinSettings).

modify_settings(NewTerms)
	:-
	alsdev_ini_path(ALSDEVINIPath),
	(exists_file(ALSDEVINIPath) ->
		grab_terms(ALSDEVINIPath, OldTerms),
		sort(OldTerms, SortedOldTerms)
		;
		SortedOldTerms = []
	),
	sort(NewTerms, SortedNewTerms),
	do_replace_items(SortedNewTerms, SortedOldTerms, NewRecords),
	open(ALSDEVINIPath, write, OutS, []),
	write_clauses(OutS, NewRecords, [quoted(true)]),
	!,
	close(OutS).

modify_settings(NewTerms, Functor, Arity, Arg1).


	%% Both lists are sorted:
do_replace_items([], OldTerms, OldTerms) :-!.
do_replace_items(NewTerms, [], NewTerms) :-!.
do_replace_items([New | NewTerms], OldTerms, Result)
	:-
	functor(New, Functor, Arity),
	arg(1, New, Arg1),
	replace_items(OldTerms, New, Functor, Arity, Arg1, Result, ResultTail, OldTermsTail),
	do_replace_items(NewTerms, OldTermsTail, ResultTail).


replace_items([],  New, Functor, Arity, Arg1,  [New | Tail], Tail, []).
replace_items([Old | OldTerms],  New, Functor, Arity, Arg1,  
		[New| Tail], Tail, OldTerms)
	:-
	functor(Old, Functor, Arity),
	arg(1, Old, Arg1),
	!.
replace_items([Old | OldTerms],  New, Functor, Arity, Arg1,  
		[New| Tail], Tail, [Old | OldTerms])
	:-
	New @< Old,
	!.
replace_items([Old | OldTerms],  New, Functor, Arity, Arg1,  
		[Old | Result], ResultTail, OldTail)
	:-
	replace_items(OldTerms,  New, Functor, Arity, Arg1,  Result, ResultTail, OldTail).

setup_defaults([], _).
setup_defaults([Tag=Value | TextSettings], Group)
	:-
	tcl_call(shl_tcli, [set_tcl_ga2,proenv,text,Tag,Value], _),
	setup_defaults(TextSettings, Group).

win_positions_for_exit(TopGeom, DebugGeom)
	:-
	tcl_call(shl_tcli, [get_tcl_ga,proenv,debugwin], DebugVis),
	WinSettings = [
		window_position('.topals',TopGeom), 
		visible('.debugwin',DebugVis), 
		window_position('.debugwin',DebugGeom) ],
	modify_settings(WinSettings).

	%% Saves other misc info too besides flags
save_prolog_flags
	:-
	changable_flags_info(FlagsList),
	topals_settings_info(TopALSSettings),
	debug_settings_info(DebugSettings),
	modify_settings(
		[prolog_value(prolog_flags,FlagsList),
		 prolog_value(topals_settings,TopALSSettings),
		 prolog_value(debug_settings,DebugSettings)
		]).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%			EDIT (FILE) WINDOWS					%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		%		edit_files,     %% list of files open for editing
		%		non_file_edits, %% list of non-file (new) windows open for editing

als_ide_mgrAction(open_edit_win(FileName), State)
	:-
	path_directory_tail(FileName, _, TF),
	file_extension(TF, BaseFileName, Ext),
	als_ide_mgrAction(open_edit_win(FileName, BaseFileName, Ext, false), State).

als_ide_mgrAction(open_edit_win_by_root(RootFileName,SearchList), State)
	:-
	file_extension(RootFileName, BaseFileName, Ext),
	accessObjStruct(edit_files, State, PrevEditFiles),
	send_self(State, obtain_src_mgr(BaseFileName, FileMgr)),
	send(FileMgr, open_edit_win_by_base(BaseFileName, Ext, SearchList)),
	send_self(State, record_src_mgr(BaseFileName, FileMgr)).

als_ide_mgrAction([open_edit_win, FileName, BaseFileName, Ext ], State)
	:-
	als_ide_mgrAction(open_edit_win(FileName, BaseFileName, Ext), State).

als_ide_mgrAction(open_edit_win(FileName, BaseFileName, Ext, IsExample), State)
	:-
	accessObjStruct(edit_files, State, PrevEditFiles),
	send_self(State, obtain_src_mgr(BaseFileName, FileMgr)),
	send(FileMgr, open_edit_win(FileName, BaseFileName, Ext, IsExample)),
	send_self(State, record_src_mgr(BaseFileName, FileMgr)).

als_ide_mgrAction(map_edit_wins, State)
	:-
	accessObjStruct(source_mgrs, State, SrcMgrsList),
	map_all_wins(SrcMgrsList).

map_all_wins([]).
map_all_wins([fm(_, FileMgr) | SrcMgrsList])
	:-
	send(FileMgr, map_win),
	map_all_wins(SrcMgrsList).


als_ide_mgrAction(unmap_edit_wins, State)
	:-
	accessObjStruct(source_mgrs, State, SrcMgrsList),
	unmap_all_wins(SrcMgrsList).

unmap_all_wins([]).
unmap_all_wins([fm(_, FileMgr) | SrcMgrsList])
	:-
	send(FileMgr, unmap_win),
	unmap_all_wins(SrcMgrsList).

	%% NEEDS MORE WORK:::
als_ide_mgrAction([open_non_file_edit_win, WinName, Title], State)
	:-
	send_self(State, obtain_src_mgr(Title, FileMgr)),
	send(FileMgr, open_edit_win('', '', WinName)).

/**
	accessObjStruct(non_file_edits, State, PrevEdits),
	setObjStruct(non_file_edits, State, [WinName | PrevEdits]).

als_ide_mgrAction([close_non_file_edit_win, WinName], State)
	:-
	accessObjStruct(non_file_edits, State, PrevEdits),
	list_delete(PrevEdits, WinName, NewEdits),
	setObjStruct(non_file_edits, State, NewEdits).
**/

als_ide_mgrAction([save_doc_as, WinName], State)
	:-
	accessObjStruct(edit_files, State, PrevEditFiles),
	accessObjStruct(non_file_edits, State, PrevEdits),
	fin_save_as(FileName,WinName,State,PrevEditFiles,PrevEdits).

fin_save_as(FileName,WinName,State,PrevEditFiles,PrevEdits)
	:-
	dmember( e(OtherFileName,WinName), PrevEditFiles),
	!,
	list_delete(PrevEditFiles, e(OtherFileName,WinName), InterEditFiles),
	setObjStruct(edit_files, State, [e(FileName,WinName) | InterEditFiles]).

fin_save_as(FileName,WinName,State,PrevEditFiles,PrevEdits)
	:-
	list_delete(PrevEdits, WinName, NewEdits),
	setObjStruct(non_file_edits, State, NewEdits),
	setObjStruct(edit_files, State, [e(FileName,WinName) | PrevEditFiles]).


rename_anon_doc(TclWin, File, SrcHandlerHandle, Flag)
	:-
	builtins:get_primary_manager(State),
	accessObjStruct(source_mgrs, State, PrevMgrsList),
	path_directory_tail(File, _, RootFile),
	(file_extension(RootFile, BaseFile, Ext) -> true ; BaseFile = RootFile, Ext=''),
	fin_rename_anon_doc(PrevMgrsList,BaseFile,Ext,TclWin,File,SrcHandlerHandle,Flag,State).

fin_rename_anon_doc(PrevMgrsList,BaseFile,Ext,TclWin,File,SrcHandlerHandle,Flag,State)
	:-
	dmember(fm(BaseFile, FileMgr), PrevMgrsList),
	!,
	Flag = not_ok.

fin_rename_anon_doc(PrevMgrsList,BaseFile,Ext,TclWin,File,SrcHandlerHandle,Flag,State)
	:-
	send(SrcHandlerHandle, get_value(source_type, file)),
	send(SrcHandlerHandle, get_value(source_file, SrcFile)),
	(SrcFile = nil ; SrcFile = ''),
	!,
	send(SrcHandlerHandle, create_file_from_win(BaseFile,Ext,File,TclWin)),
	send(SrcHandlerHandle, get_value(myName,FileMgr)),
			%% Primary ide mgr:
	send_self(State, record_src_mgr(BaseFile, FileMgr)),
	Flag = ok.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%											%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

als_ide_mgrAction( change_prolog_flag(Flag,Value), State)
	:-
	tcl_call(shl_tcli, [set_tcl_ga,proenv,Flag,Value], _).

%	check_for_debugger_toggle(Flag,Value).

check_for_debugger_toggle(debug,on)
	:-!,
	tcl_call(shl_tcli, [set_tcl_ga,proenv,debugwin,1], _),
	tcl_call(shl_tcli, [catch,exec_toggle_debugwin], _).
check_for_debugger_toggle(debug,off)
	:-!,
	tcl_call(shl_tcli, [set_tcl_ga,proenv,debugwin,0], _),
	tcl_call(shl_tcli, [catch,exec_toggle_debugwin], _).
check_for_debugger_toggle(_,_).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%		PROJECTS								%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* ... see als_ide_mgrAction/2 clauses in projects.pro ... */

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%			RECONSULT ETC.						%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_reconsult(PathAtom)
	:-
	sys_env(OS,_,_),
	perf_reconsult(OS, PathAtom),
	listener_prompt.

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
	:-!,
	repair_path0(PAInCs, PAOutCs).
repair_path0([C | PAInCs], [C | PAOutCs])
	:-
	repair_path0(PAInCs, PAOutCs).

clear_workspace
	:-
	findall(MM, non_system_module(MM), UserMods),
	clear_each_module(UserMods),
	close_down_nonsystem_streams,
	destroy_all_tcl_interpreters,
	remove_non_system_global_vars,
	listener_prompt.

clear_each_module([]).
clear_each_module([M | UserMods])
	:-
	printf(shl_tk_out_win, 'Module %t cleared.\n',[M]),
	abolish_module(M),
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
	init_tk_alslib(TclInterp, _),
	tcl_call(TclInterp, [source, File], X),
	printf(user_output, 'Tcl file %t sourced in Tcl interpreter %t\n', [File,TclInterp]).


start_visual_tcl
	:-
	builtins:sys_searchdir(SSD),
	split_path(SSD,SSDElts0),
	fix_disk(SSDElts0, SSDElts),
	parent_path(PP),
	append(SSDElts,[PP,vtcl],VTPathHomeElts),
	join_path(unix,VTPathHomeElts, VTCL_HOME0),
	catenate(['"',VTCL_HOME0, '"'], VTCL_HOME),

	append(SSDElts,[PP,vtcl,'vt.tcl'],VTPathElts),
	join_path(unix,VTPathElts, VTPath),

	init_tk_alslib(vttlci,_),
	tcl_eval(vttlci,'set argc 0',_),
	tcl_eval(vttlci,'set argv {}',_),
	tcl_call(vttlci,[source,VTPath],_),
	tcl_eval(vttlci,[set_tcl_ga,vTcl,'VTCL_HOME',VTCL_HOME],_),
	tcl_call(vttlci,['vTcl:prolog_intf'],_).

fix_disk([Head0 | SSDElts0], [Head | SSDElts0])
	:-
	sub_atom(Head0,0,2,_,Head).


remove_non_system_global_vars
	:-
	system_global_vars(SysGlobals),
	remove_non_system_global_vars(SysGlobals).

	%% Note that the module Mod has already been wiped out;
	%% Here we need only remove the  global_gv_info entry:
remove_non_system_global_vars(SysGlobals)
	:-
	global_gv_info:gvi(GVID,N,Mod,Val),
	not(dmember(GVID, SysGlobals)),
	global_gv_info:retract(gvi(GVID,N,Mod,Val)),
	fail.
remove_non_system_global_vars(SysGlobals).



	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%			DEFSTRUCT HANDLING					%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_typ
	:-
	file_select_dialog(shl_tcli,[filetypes=[['typ files',['*.typ']]]],FileName),
	(FileName = '' ->
		true
		;
		builtins:comptype(FileName)
	).
			

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
	%%%%% 		
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% Set up the connection to the TclPro debugger for
	%% interpreter TclInterp; assumes that prodebug.tcl can 
	%% be found in ALSTCLPATH:
init_tcl_debugger(TclInterp, ALSTCLPATH)
	:-
	tcl_call(TclInterp, [file,join,ALSTCLPATH,'prodebug.tcl'], PDFPath),
	tcl_call(TclInterp, [source, PDFPath], _),
	tcl_call(TclInterp, [debugger_init], _).

%setup_for_debugging(Which)
%	:-
%	xconsult:change_source_level_debugging(Which),
%	builtins:do_set_prolog_flag(debug,Which).
%
%switch_debugging_setup(Which)
%	:-
%	tcl_call(shl_tcli, [switch_debug_setup,Which], _).

endmod.   % alsdev.


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%% 		VISUAL DEBUGGER ROUTINES				%%%%%
	%%%%% 		  -- portions in module BUILTINS		%%%%% 
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% LIBRARY LOAD RECORDING:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- abolish(record_lib_load, 1).
	%% Needs to be expanded:
	%% must make its assert in module builtins:
record_lib_load(Desc)
	:-
		%% the incoming Desc is an atom, looking like: Dir/File
	split_path(Desc,[Dir,File]),
	assertz(loaded_builtins_file(File,Dir)).


endmod.	%% builtins


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%% 		VISUAL DEBUGGER ROUTINES				%%%%%
	%%%%% 		  -- portions in module debugger		%%%%% 
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


module debugger.
use tcltk.

:- abolish(ensure_db_showing, 0).

ensure_db_showing
	:-
 	tcl_call(shl_tcli, [ensure_db_showing], _).


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

start_src_trace(Flag,BaseFileName, SrcFilePath, CG, ALSMgr, SrcMgr)
	:-
	send(ALSMgr, get_value(debugger_mgr, DbgrMgr)),
	send(DbgrMgr, ensure_db_showing),
	send(DbgrMgr, insert_by_fcg(CG, SrcMgr)),
	send(DbgrMgr, set_value(mrfcg, CG)),
	!,
	current_prolog_flag(debug, DBF),
	(DBF = on ->
		send(SrcMgr, start_src_trace(BaseFileName, SrcFilePath, CG))
		;
		true
	).

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
	catenate(WinName, '.text', TextWinName),
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

set_debugwin_width(MSize, MainWinSize)
	:-
	max(floor(1.8 * (MainWinSize)//MSize), 5, NChars),
	sio:is_stream(debugger_output, DS),
	set_line_length(DS, NChars).

export debug_settings_info/1.
debug_settings_info(DebugSettings)
	:-
	sio:stream_or_alias_ok(debugger_output, OSS),
	sio:stream_wt_opts(OSS, DBGOPTs),
	findall(leashed(Port), leashed(Port), Leashing),
	DebugSettings = [stream_opts=DBGOPTs | Leashing].

export set_debug_settings_info/1.
set_debug_settings_info([])
	:-!.
set_debug_settings_info(DebugSettings)
	:-
	(dmember( stream_opts=DBGOPTs, DebugSettings) ->
		sio:stream_or_alias_ok(debugger_output, OSS),
		sio:set_stream_wt_opts(OSS, DBGOPTs)
		;
		true
	),
	findall(leashed(Port), member(leashed(Port), DebugSettings), Leashing),
	abolish(leashed, 1),
	assert_all(Leashing),

	check_leashing(Call,Exit,Redo,Fail),
	tcl_call(shl_tcli, [set_tcl_ga2, proenv, leash, call, Call], _),
	tcl_call(shl_tcli, [set_tcl_ga2, proenv, leash, exit, Exit], _),
	tcl_call(shl_tcli, [set_tcl_ga2, proenv, leash, redo, Redo], _),
	tcl_call(shl_tcli, [set_tcl_ga2, proenv, leash, fail, Fail], _),
	DBGOPTs = wt_opts(_,PrintDepth,DepthType),
	tcl_call(shl_tcli, [set_tcl_ga, proenv, debug_print_depth, PrintDepth], _),
	tcl_call(shl_tcli, [set_tcl_ga, proenv, db_flatness, DepthType], _).


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% THIS IS THE ACTUAL CALL FROM THE LOW-LEVEL DEBUGGER
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	/*---
	'$dbg_aph'(ClsGrp,Start,End)
	'$dbg_aphe'(ClsGrp,Start,End)
	'$dbg_apf'(ClsGrp,Start,End)
	'$dbg_apg'(ClsGrp,Start,End)
	'$dbg_apge'(ClsGrp,Start,End)
	 *---*/
export v_showGoalToUserWin/6.
v_showGoalToUserWin(Port,Box,Depth, Module, Goal, Response)
	:-
	functor(Goal, FF, 3),
	dmember(FF, ['$dbg_aph', '$dbg_aphe', '$dbg_apf', '$dbg_apg', '$dbg_apge']),
	arg(1, Goal, CG),
	builtins:get_primary_manager(ALSIDEMGR),
	send(ALSIDEMGR, get_value( debugger_mgr, DBGMGR)),
	send(DBGMGR,  mgr_by_cg(CG, SrcMgr)),
	send(SrcMgr,  ensure_window_open),
	accessObjStruct(mrfcg, DBGMGR, MRFCG),
	check_win_cleanup(Port, MRFCG, CG, SrcMgr, DBGMGR),
	!,
	showGoalToUserWin(Port,Box,Depth, Module, Goal, Response, DBGMGR, SrcMgr).

v_showGoalToUserWin(Port,Box,Depth, Module, Goal, Response)
	:-
	builtins:get_primary_manager(ALSIDEMGR),
	send(ALSIDEMGR, get_value( debugger_mgr, DBGMGR)),
	!,
	vv_showGoalToUserWin(Port,Box,Depth, Module, Goal, DBGMGR, Response).

vv_showGoalToUserWin(Port,Box,Depth, Module, Goal, DBGMGR, Response)
	:-
	accessObjStruct(mrfcg, DBGMGR, CG),
	(CG > 0 ->
		send(DBGMGR,  get_mrfcg(CG, SrcMgr))
		;
		true
	),
	!,
	showGoalToUserWin_other(Port,Box,Depth, Module, Goal, Response, CG, DBGMGR, SrcMgr).

		%% Staying in the same file; nothing to do:
check_win_cleanup(_, CG, CG, _, _) :-
	!.

		%% Now we have just switched between files; if Port is
		%% call or redo, need to remove trace coloring, etc., 
		%% from window we are leaving; if Port is exit or fail, 
		%% nothing to do:
check_win_cleanup(Port, MRFCG, ClsGrp, SrcMgr, DBGMGR)
	:-
	dmember(Port, [call, redo]),
	!,
	check_presence_and_cleanup(SrcMgr, MRFCG, DBGMGR).

check_win_cleanup(_, _, _, _, _).

check_presence_and_cleanup(SrcMgr, MRFCG, DBGMGR)
	:-
	send(DBGMGR, mgr_by_cg(MRFCG, MRSrcMgr)),
	send(MRSrcMgr, clear_decorations),
	send( SrcMgr, start_src_trace),
	send(SrcMgr, raise).

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

/*----- For debugging:
showGoalToUserWin(Port,Box,Depth, Module, Goal, debug, DBGMGR, SrcMgr)
	:-
printf('%t-%t(%t) %t \n',[Box,Depth,Port,Goal]),flush_output,
	fail.
*/

	%% '$dbg_aph' ::

showGoalToUserWin(call,Box,Depth, Module, '$dbg_aph'(ClsGrp,Start,End), debug, DBGMGR, SrcMgr)
	:-
	color_my_port(ClsGrp,Start,End,call,head_tag, DBGMGR, SrcMgr),
	!.

	%% The call to color_my_port could fail on the very first call to
	%% a predicate defined in a file which has not yet been set up for
	%% debugging; this alternate clause tries to get the file set up:

showGoalToUserWin(call,Box,Depth, Module, '$dbg_aph'(ClsGrp,Start,End), debug, DBGMGR, SrcMgr)
	:-!,
	send(DBGMGR,  mgr_by_cg(ClsGrp, SrcMgr)),

	send(DBGMGR,  mgr_by_cg(ClsGrp, NewSrcMgr)),
	send(DBGMGR,  set_value(mrfcg, ClsGrp)),
	send(NewSrcMgr, start_src_trace),
	!,
	color_my_port(ClsGrp,Start,End,call,head_tag, DBGMGR, NewSrcMgr).

showGoalToUserWin(fail,Box,Depth, Module, '$dbg_aph'(ClsGrp,Start,End), debug, DBGMGR, SrcMgr)
	:-!,
	color_my_port(ClsGrp,Start,End,fail,head_tag, DBGMGR, SrcMgr).
	
showGoalToUserWin(redo,Box,Depth, Module, '$dbg_aph'(ClsGrp,Start,End), debug, DBGMGR, SrcMgr)
	:-!,
	color_my_port(ClsGrp,Start,End,redo,head_tag, DBGMGR, SrcMgr).
	
showGoalToUserWin(exit,Box,Depth, Module, '$dbg_aph'(ClsGrp,Start,End), debug, DBGMGR, SrcMgr)
	:-!,
	fcg_change(Port,'$dbg_aph',ClsGrp,Start,End, DBGMGR).

	%% '$dbg_aphe' ::

	% "call" here is successful exit from the clause:
showGoalToUserWin(call,Box,Depth, Module, '$dbg_aphe'(ClsGrp,Start,End), debug, DBGMGR, SrcMgr)
	:-!,
	color_my_port(ClsGrp,Start,End,exit,head_tag, DBGMGR, SrcMgr).

showGoalToUserWin(redo,Box,Depth, Module, '$dbg_aphe'(ClsGrp,Start,End), debug, DBGMGR, SrcMgr)
	:-!,
	color_my_port(ClsGrp,Start,End,redo,head_tag, DBGMGR, SrcMgr).

showGoalToUserWin(fail,Box,Depth, Module, '$dbg_aphe'(ClsGrp,Start,End), debug, DBGMGR, SrcMgr)
	:-!.

showGoalToUserWin(exit,Box,Depth, Module, '$dbg_aphe'(ClsGrp,Start,End), debug, DBGMGR, SrcMgr)
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

showGoalToUserWin(call,Box,Depth, Module, '$dbg_apf'(ClsGrp,Start,End), debug, DBGMGR, SrcMgr)
	:-
	color_my_port(ClsGrp,Start,End,exit,head_tag, DBGMGR, SrcMgr),
	!.

	%% The call to color_my_port could fail on the very first call to
	%% a predicate defined in a file which has not yet been set up for
	%% debugging; this alternate clause tries to get the file set up:

showGoalToUserWin(call,Box,Depth, Module, '$dbg_apf'(ClsGrp,Start,End), debug, DBGMGR, SrcMgr)
	:-!,
	builtins:file_clause_group(BaseFileName, ClsGrp),
	send(SrcMgr, start_src_trace),
	!,
	color_my_port(ClsGrp,Start,End,exit,head_tag, DBGMGR, SrcMgr).






showGoalToUserWin(redo,Box,Depth, Module, '$dbg_apf'(ClsGrp,Start,End), debug, DBGMGR, SrcMgr)
	:-!.

showGoalToUserWin(Port,Box,Depth, Module, '$dbg_apf'(ClsGrp,Start,End), debug, DBGMGR, SrcMgr)
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
showGoalToUserWin(call,Box,Depth, Module, '$dbg_apg'(ClsGrp,Start,End), debug, DBGMGR, SrcMgr)
	:-!,
	color_my_port(ClsGrp,Start,End,call,call_tag, DBGMGR, SrcMgr),
	!.

	%% "redo" here is actually failure of the acutal goal:
showGoalToUserWin(redo,Box,Depth, Module, '$dbg_apg'(ClsGrp,Start,End), debug, DBGMGR, SrcMgr)
	:-!,
	color_my_port(ClsGrp,Start,End,fail,call_tag, DBGMGR, SrcMgr).

showGoalToUserWin(Port,Box,Depth, Module, '$dbg_apg'(ClsGrp,Start,End), debug, DBGMGR, SrcMgr)
	:-!.

	%% '$dbg_apge' ::

	%% "call" here is really exit for the acutal goal:
showGoalToUserWin(call,Box,Depth, Module, '$dbg_apge'(ClsGrp,Start,End), debug, DBGMGR, SrcMgr)
	:-
	color_my_port(ClsGrp,Start,End,exit,call_tag, DBGMGR, SrcMgr),
	!.

	%% The call to color_my_port could fail on the a return (=dbg_apge, here) to
	%% a predicate defined in a file for which the window has not yet been set up for
	%% debugging; this alternate clause tries to get the file set up:
	%% {This can happen if we spy on predicate defined in File#2, but we
	%% start by running a predicate in File#1 which leads to File#2}

showGoalToUserWin(call,Box,Depth, Module, '$dbg_apge'(ClsGrp,Start,End), debug, DBGMGR, SrcMgr)
	:-!,
	send(DBGMGR,  mgr_by_cg(ClsGrp, SrcMgr)),

	send(DBGMGR,  mgr_by_cg(ClsGrp, NewSrcMgr)),
	send(DBGMGR,  set_value(mrfcg, ClsGrp)),
	send(NewSrcMgr, start_src_trace),
	!,
	color_my_port(ClsGrp,Start,End,exit,call_tag, DBGMGR, NewSrcMgr).

	%% "redo" here is really redo for the acutal goal:
showGoalToUserWin(redo,Box,Depth, Module, '$dbg_apge'(ClsGrp,Start,End), debug, DBGMGR, SrcMgr)
	:-!,
	color_my_port(ClsGrp,Start,End,redo,call_tag, DBGMGR, SrcMgr).

showGoalToUserWin(_,Box,Depth, Module, '$dbg_apge'(ClsGrp,Start,End), debug, DBGMGR, SrcMgr)
	:-!.

	%%---------------------------------------------------
	%%		ALL OTHERS
	%%---------------------------------------------------
showGoalToUserWin_other(Port,Box,Depth, Module, XGoal, Response, MRFCG, DBGMGR, SrcMgr)
	:-
	printf(debugger_output,'(%d) %d %t: ', [Box,Depth,Port]), 

%% should not be necessary, but there is a bug somewhere
%% in printf which allows it to be re-satisfied::!!::
	!,
	write_term(debugger_output, Module:XGoal, [lettervars(false)]),
	flush_output(debugger_output),
	re_color_port(Port, MRFCG, SrcMgr),
	!,
		% clean up if Port = exit; Port = fail
	(dbg_boundary(MRFCG,Box,Depth,general,Port,XGoal,DBGMGR, SrcMgr) ->
		nl(debugger_output),
		Response = debug
		;
		tcl_call(shl_tcli, [set_status_debugwin,Port,Box,Depth], _),
		getResponse(tcltk,Port,Box,Depth, Module, XGoal, Response)
	),
	!.

color_my_port(ClsGrp,Start,End,Port,TagName, DBGMGR, SrcMgr)
	:-
	tktxt_info(SrcMgr, ClsGrp,TextWin,Start,End,StartLine,StartChar,EndLine,EndChar),
	accessObjStruct(TagName, SrcMgr, LastCallTag),

	restore_remove(LastCallTag, TextWin, TagName),
	tcl_call(shl_tcli,
			[assign_tag,TextWin,TagName,StartLine,StartChar,EndLine,EndChar],
			_),
	accessObjStruct(tcl_doc_path, SrcMgr, Win),
	catenate(Win, '.ltext', LTextWin),
	(TagName = head_tag ->
		(dmember(Port, [call,exit]) ->
			accessObjStruct(head_tag, SrcMgr, PrevHT),
			(PrevHT = i(PSL,_,_,_) ->
				catenate(PSL, '.0', PSLI),
				tcl_call(shl_tcli, [LTextWin, insert, PSLI, ' '], _)
				;
				true
			),
			catenate(StartLine, '.0', SLE),
			tcl_call(shl_tcli, [LTextWin, insert, SLE, '>'], _)
			; 
			true
		)
		;
		true
	),
	port_color(Port, Color),
	fcg_change(Port,'??',ClsGrp,Start,End, DBGMGR),
	send(DBGMGR, set_value(mrfcg, ClsGrp)),
	!,
	display_st_record(StartLine,StartChar,EndLine,EndChar,Color,TextWin,LTextWin,TagName,SrcMgr).

re_color_port(_, 0, _) :-!.
re_color_port(call, _, _) :-!.
re_color_port(redo, _, _) :-!.
re_color_port(Port, MRFCG, SrcMgr)
	:-
	accessObjStruct(tcl_doc_path, SrcMgr, Win),
	!,
	(Win \= nil ->
		catenate(Win, '.text', TextWin),
		port_color(Port, Color),
		!,
		configure_tag(call_tag, TextWin,['-background',Color])
		;
		true
	).  

	%blue:
port_color(call, '#00acff').
port_color(exit, green).
	%red:
port_color(fail, '#fe0076').
port_color(redo, yellow).

dbg_boundary(MRFCG,1,1,Desc,Port,XGoal,DBGMGR, SrcMgr)
	:-
	((Port = exit; Port = fail) -> 
		clean_up_src_win(MRFCG, SrcMgr) 
		; 
		true
	).

fcg_change(Port,DBG,ClsGrp,Start,End, DBGMGR)
	:-
	send(DBGMGR, get_value(mrfcg, ClsGrp)),
	MRFCG \= ClsGrp,
	!,
	flush_output,
	((Port = exit; Port = fail) -> 
		clean_up_src_win(MRFCG, SrcMgr) 
		; 
		true).

fcg_change(Port,DBG,ClsGrp,Start,End, DBGMGR).


clean_up_src_win(0, SrcMgr) :-!.
clean_up_src_win(MRFCG, SrcMgr)
	:-
	send(SrcMgr, clear_decorations).

getresponse2(tcltk,Port,Box,Depth,Module,Goal,Response)
	:-
	short_deb_resps(Resps),
	tcl_call(shl_tcli, [wait_for_debug_response],RawResponse),
	(sub_atom(RawResponse,0,1,_,'B') ->
		nl(debugger_output),flush_output(debugger_output),
		sub_atom(RawResponse, 1, 1, _, RR)
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

tktxt_info(SrcMgr,ClsGrp,TextWin,Start,End,StartLine,StartChar,EndLine,EndChar)
	:-
	accessObjStruct(tcl_doc_path, SrcMgr, Win),
	Win \= nil,
	catenate(Win, '.text', TextWin),
	accessObjStruct(num_lines, SrcMgr, NumLines),
	accessObjStruct(invlineindex, SrcMgr, InvLineIndex),
	find_line_pos(1,NumLines,Start,InvLineIndex,StartLine),
	arg(StartLine,InvLineIndex,SLOffset),
	StartChar is Start - SLOffset,
	find_line_pos(1,NumLines,End,InvLineIndex,EndLine),
	arg(EndLine, InvLineIndex, ELOffset),
	!,
	EndChar is End - ELOffset.


display_st_record(StartLine,StartChar,EndLine,EndChar, 
						Color,TextWin, LTextWin,TagName, SrcMgr)
	:-
	(TagName = call_tag ->
		configure_tag(TagName, TextWin,['-background',Color])
		;
		true
	),
	tcl_call(shl_tcli,[see_text,TextWin,LTextWin,StartLine,StartChar,EndLine,EndChar],_),
	!,
	setObjStruct(TagName, SrcMgr, i(StartLine,StartChar,EndLine,EndChar)).

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

	%%-------------------------------------------------
	%		SPYPOINTS
	%%-------------------------------------------------
install_new_spypoints([], _) :-!.
install_new_spypoints(NewSpyListIn, Module)
	:-
	fixup_tcltk_pa_descs(NewSpyListIn, NewSpyList),
    dbg_spyoff,
	debug_io(DebugIOChannel),
	install_spypoints_in_mod(NewSpyList, DebugIOChannel, [], Module),
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

install_spypoints_in_mod([], _, _, Module).
install_spypoints_in_mod([P/A | NewSpyList], DebugIOChannel, CGsSetup, Module)
	:-
	setup_debug(DebugIOChannel, Module, P, A, CGsSetup, NextCGsSetup),
    install_spypoint(Module,P,A),
	install_spypoints_in_mod(NewSpyList, DebugIOChannel, NextCGsSetup, Module).

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

remove_all_spypoints
	:-
	nospy.

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

export reset_all_spypoints/0.
reset_all_spypoints
	:-
    dbg_spyoff,
%	findall(Mod-List, mspylist(Mod,List), SpyList),
	allspylist(SpyList),
	reset_the_spypoints(SpyList),
    setPrologInterrupt(spying),
    setDebugInterrupt(spying),
    dbg_spyon.

mspylist(Mod,List)
	:-
	has_spypoint(Mod),
	findall(Pred/Arity,
			(debugger:spying_on(CallForm,Mod), 
			 functor(CallForm,Pred,Arity) ),
			List).

allspylist(List)
	:-
	findall(Mod/(Pred/Arity),
			(debugger:spying_on(CallForm,Mod), 
			 functor(CallForm,Pred,Arity) ),
			List0),
	group_mods(List0, List).

group_mods(List0, List)
	:-
	group_mods(List0, [], List).

group_mods([], List, List).
group_mods([M/(P/A) | Rest], CurList, List)
	:-
	insert_mp(CurList,M,P/A,NextList),
	group_mods(Rest, NextList, List).

insert_mp([],M,PA,[M-[PA]]).
insert_mp([M-ML | CurList],M,PA,[M-[PA | ML] | CurList])
	:-!.
insert_mp([Group | CurList],M,PA,[Group | NextList])
	:-
	insert_mp(CurList,M,PA,NextList).


has_spypoint(Mod)
	:-
	debugger:spying_on(_,Mod),
	!.

reset_the_spypoints([]).
reset_the_spypoints([Mod-MList | SpyList])
	:-
	reset_module_spypoints(MList, Mod),
	printf(debugger_output,
			'Spypoints reset in module %t on:\n\t%t\n',
			[Mod, MList]),
	reset_the_spypoints(SpyList).


reset_module_spypoints([], _).
reset_module_spypoints([Pred/Arity | MList], Mod)
	:-
	reset_this_spypoint(Mod,Pred,Arity), 
	reset_module_spypoints(MList, Mod).

reset_this_spypoint(Mod,Pred,Arity)
	:-
	get_fcg(Mod,Pred,Arity,CG,DefiningMod),
	builtins:get_primary_manager(ALSMgr),
	send(ALSMgr, obtain_src_mgr_by_cg(CG, FileMgr)),
	send(FileMgr, ensure_window_open),
	dbg_spy(Mod,Pred,Arity).




endmod.


module alsdev.

check_reload_consults
	:-
	builtins:get_primary_manager(ALSMgr),
	accessObjStruct(source_mgrs, ALSMgr, MgrsList),
	findall(F, 
			(member(fm(B,M), MgrsList),
			  not member(B, [tcltk,tcltk_util,tclintf,alspro,'.alspro']),
			  accessObjStruct(source_file, M, F) ),
			LoadedFiles),
	(LoadedFiles \= [] ->
		Msg = 'Reload consulted files for source code tracing?',
		yes_no_dialog(shl_tcli, Msg, 'Reload Files', Ans)
		;
		Ans = 'No'
	),
	consult_all(LoadedFiles),
	reset_all_spypoints.


consult_all([]).
consult_all([F | Files])
	:-
	consult(F),
	consult_all(Files).

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
make_dbstr(_A) :- _A =.. [dbstr,_B,_C,_D,_E,0,[],[],0,0].

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

module alsdev.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%			SYNTAX ERROR REPORTING				%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export ide_rt_err_report/2.
ide_rt_err_report(syntax(Context,P1,P2,P3,ErrorMessage,LineNumber,Stream), [])
	:-
	write(shl_tk_out_win,'----------------------'),nl(shl_tk_out_win),
	write(shl_tk_out_win,ErrorMessage), nl(shl_tk_out_win),
	write(shl_tk_out_win,'----------------------'),nl(shl_tk_out_win).

endmod.

module builtins.


/*****
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% ERROR OUTPUT PREDICATES
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export ide_prolog_system_error/2.

	%% New rt_ reader:
ide_prolog_system_error(
	error(syntax_error,[_,syntax(Context,P1,P2,P3,ErrorMessage,LineNumber,Stream)]), [])
	:-
	ide_prolog_system_error(syntax(Context,P1,P2,P3,ErrorMessage,LineNumber,Stream), []).

ide_prolog_system_error(syntax(Context,P1,P2,P3,ErrorMessage,LineNumber,Stream), _) 
	:-
	sio:is_stream(Stream,Stream0),
	sio:is_input_stream(Stream0),
	!,
	EType = 'Syntax error',
	sio:stream_type(Stream0,StreamType),
	sio:stream_name(Stream0,StreamName),
	(StreamType = file ->
		path_directory_tail(StreamName, _, File),
		;	
		File = StreamName
	),
	sprintf(atom(EMsg), '%s\n%t: %t, line %d:\n%s\n',
		[Context,EType,File,LineNumber,ErrorMessage]),
	info_dialog(shl_tcli, EMsg, 'Syntax Error:').



ide_prolog_system_error(s(ErrorCode,Stream), Args) 
	:-
	expand_code(ErrorCode, Pattern, EType),
	!,
	sio:stream_type(Stream,StreamType),
	sio_linenumber(Stream,LineNumber),
	sio:stream_name(Stream,StreamName),

	OutPattern = '%t: %t stream %t,line %d:\n     ',
	OutArgs = [StreamType,StreamName,LineNumber],
	pse_out(error_stream, EType, OutPattern, OutArgs),
	printf(error_stream,Pattern, Args),
	flush_output(error_stream).

ide_prolog_system_error(qc_failed(ErrorCode,Name,LineNumber),Args) 
	:-
	expand_code(ErrorCode, Pattern, EType),
	!,
	catenate('%t,line %t: ', Pattern, OutPattern),
	OutArgs = [Name,LineNumber | Args],
	pse_out(error_stream, EType, OutPattern, OutArgs),
	flush_output(error_stream).

ide_prolog_system_error(error(W,L),_) 
	:-
	decode_error(W, L, Pattern, Args),
	!,
	pse_out(error_stream, 'Error: ', Pattern, Args),
	print_error_goal_attributes(L),
	printf(error_stream,'- %-14s %t\n',
		['Throw pattern:',error(W,L)],
		[quoted(true),maxdepth(4),indent(17)]),
	flush_output(error_stream).
	
ide_prolog_system_error(ErrorCode, Args) 
	:-
	expand_code(ErrorCode, Pattern, EType),
	pse_out(error_stream, EType, Pattern, Args),
	flush_output(error_stream).

expand_code(EWCode, Pattern, '\nError: ')
	:-
	error_code(EWCode, Pattern),
	!.

expand_code(EWCode, Pattern, '\nWarning: ')
	:-
	warning_code(EWCode, Pattern).

expand_code(EWCode, Pattern, '')
	:-
	info_code(EWCode, Pattern).

pse_out(Stream, EType, Pattern, Args)
	:-
	printf(Stream, '%t',[EType]),
	printf(Stream, Pattern, Args, [quoted(true), maxdepth(6)]).

print_error_goal_attributes([]) :- !.
print_error_goal_attributes([H|T]) 
	:-
	print_error_goal_attribute(H),
	print_error_goal_attributes(T).
print_error_goal_attributes(Other) 
	:-
	print_error_goal_attribute(Other).

print_error_goal_attribute(M:G) 
	:-!,
	printf(error_stream,'- %-14s %t\n',
		['Goal:',M:G],[quoted(true),maxdepth(5),indent(17)]),
	flush_output(error_stream).

print_error_goal_attribute(Huh) 
	:-
	functor(Huh,AttribName0,1),
	!,
	arg(1,Huh,AttribValue),
	atom_concat(AttribName0,':',AttribName),
	printf(error_stream,'- %-14s %t\n',
			[AttribName,AttribValue], 
			[quoted(true),maxdepth(5),indent(17)]),
	flush_output(error_stream).

print_error_goal_attribute(Other) 
	:-
	printf(error_stream,'- %-14s %t\n',
			['Error Attribute:', Other],
			[quoted(true),maxdepth(10),indent(17)]),
	flush_output(error_stream).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% WARNING OUTPUT PREDICATES
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 	%% Print all advisory and warning messages to
 	%% warning_output stream.

export ide_prolog_system_warning/2.

ide_prolog_system_warning(error(W,L),_) 
	:-
	decode_error(W, L, Pattern, Args),
	!,
	pse_out(error_stream, 'Warning: ', Pattern, Args),
	print_error_goal_attributes(L),
	flush_output(error_stream).

ide_prolog_system_warning(ErrorCode, Args) 
	:-
	expand_code(ErrorCode, Pattern, EType),
	printf(warning_output, '%t',[EType]),
	printf(warning_output, Pattern, Args, [quoted(true), maxdepth(9)]),
	flush_output(warning_output).


export als_advise/1.
als_advise(FormatString) 
	:-
	als_advise(warning_output, FormatString, []).

export als_advise/2.
als_advise(FormatString, Args) 
	:-
	als_advise(warning_output, FormatString, Args).

export als_advise/3.
als_advise(Stream, FormatString, Args) 
	:-
	printf(Stream, FormatString, Args),
	flush_output(Stream).

*********/
endmod.

module alsdev.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% ACTIONS FOR shl_source_handler
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*----------------------------------------
	object_classes:defineClass(builtins,
	[   name=source_handler,
		subClassOf=genericObjects,
		export = yes,
		addl_slots=
			[ 
				source_file, 		%% OS path to the ...
				base_file,			%% underlying file name
				obp_file,			%% OS path to obp file if exists, or nil
				fcg, 				%% File clause group # for this (consulted) file
				consult_mode,		%% normal/debug
				last_consult		%% Time of last consult,
			],
		defaults= [ 
			source_file		= '',
			base_file		= '',
			fcg				= 0,
			consult_mode	= normal,
			last_consult	= 0
			]
	])

        %%   SHL_CONSULTED_FILE_MGR  --- in ALSDEV:
	object_classes:defineClass(alsdev,
	[   name=shl_source_handler,
		subClassOf=source_handler,
		export = yes,
		addl_slots= [ 
			tcl_doc_path,
			errors_display		%% nil / non-empty errs list
		],
		defaults= [ 
			tcl_doc_path		= nil,
			errors_display 		= nil
		]
	])
 *---------------------------------------*/

shl_source_handlerAction(open_edit_win(FileName), State)
	:-
	path_directory_tail(FileName, _, TF),
	file_extension(TF, BaseFileName, Ext),
	shl_source_handlerAction(open_edit_win(FileName, BaseFileName, Ext), State).

	%% Non-file document case:
shl_source_handlerAction(open_edit_win('', BaseFileName, TclWin), State)
	:-!,
	setObjStruct(tcl_doc_path, State, TclWin),
	accessObjStruct(myHandle, State, MyHandle),
	tcl_call(shl_tcli, [set_tcl_ga2,proenv,TclWin,src_handler,MyHandle], _).

/*
shl_source_handlerAction(open_edit_win(FileName, BaseFileName, Ext), State)
	:-
	setObjStruct(source_file, State, FileName),
	setObjStruct(ext, State, Ext),
	shl_source_handlerAction(complete_open_edit_win(FileName,_), State).
*/

	%% File document case:
shl_source_handlerAction(open_edit_win(FileName, BaseFileName, Ext), State)
	:-
	shl_source_handlerAction(open_edit_win(FileName, BaseFileName, Ext, false), State).

shl_source_handlerAction(open_edit_win(FileName, BaseFileName, Ext, IsExample), State)
	:-
	setObjStruct(source_file, State, FileName),
	setObjStruct(ext, State, Ext),
	shl_source_handlerAction(complete_open_edit_win(FileName,_,IsExample), State).

shl_source_handlerAction(open_edit_win_by_base(BaseFileName, Ext, SearchList), State)
	:-
	accessObjStruct(source_file, State, FileName),
	FileName \= nil, FileName \='',
	!,
	shl_source_handlerAction(complete_open_edit_win(FileName,TclWin, false), State).

shl_source_handlerAction(open_edit_win_by_base(BaseFileName, Ext, SearchList), State)
	:-
	file_extension(FileDesc, BaseFileName, Ext),
	path_to_try_from_desc(SearchList, FileDesc, FileTryPath),
	!,
	shl_source_handlerAction(complete_open_edit_win(FileTryPath,TclWin, false), State),
	setObjStruct(source_file, State, FileTryPath),
	setObjStruct(ext, State, Ext).

shl_source_handlerAction(complete_open_edit_win(FileName,TclWin,IsExample), State)
	:-
	tcl_call(shl_tcli, [load_document, FileName], TclWin),
	setObjStruct(tcl_doc_path, State, TclWin),
	accessObjStruct(myHandle, State, MyHandle),
	tcl_call(shl_tcli, [set_tcl_ga2,proenv,TclWin,src_handler,MyHandle], _),
	(IsExample=true ->
		tcl_call(shl_tcli, [set_tcl_ga2,proenv,TclWin,is_example,true], _)
		;
		true
	).

shl_source_handlerAction(close_edit_win, State)
	:-
	accessObjStruct(tcl_doc_path, State, TclWin),
%	tcl_call(shl_tcli, [dispose_document_window, TclWin], _),
			%% MUST close down any source trace stuff later:::
			%%		send_self(close_source_trace, State),
	setObjStruct(errors_display, State, []),
	setObjStruct(tcl_doc_path, State, nil).

shl_source_handlerAction(ensure_window_open, State)
	:-
	accessObjStruct(tcl_doc_path, State, TclDocPath),
	fin_ensure_window_open(TclDocPath, State).

fin_ensure_window_open(nil, State)
	:-!,
	accessObjStruct(source_file, State, FileName),
	send_self(State, complete_open_edit_win(FileName,TclWin)).

fin_ensure_window_open(TclWin, State)
	:-
	tcl_call(shl_tcli, [wm, deiconify, TclWin], _).


shl_source_handlerAction(close_and_reopen, State)
	:-
	accessObjStruct(tcl_doc_path, State, TclWin),
	tcl_call(shl_tcli, [close_and_reopen, TclWin], _).

/*
shl_source_handlerAction(create_file_from_win(BaseFile,Ext,File,TclWin), State)
	:-
	setObjStruct(source_file, State, FileName),
	setObjStruct(ext, State, Ext),

	tcl_call(shl_tcli, [load_document, FileName], TclWin),
	setObjStruct(tcl_doc_path, State, TclWin),
	accessObjStruct(myHandle, State, MyHandle),
	tcl_call(shl_tcli, [set_tcl_ga2,proenv,TclWin,src_handler,MyHandle], _).
*/


shl_source_handlerAction(unmap_win, State)
	:-
	accessObjStruct(tcl_doc_path, State, TclWin),
	(TclWin \= nil ->
		tcl_call(shl_tcli, ['Window', iconify, TclWin], _)
		;
		true
	).

shl_source_handlerAction(map_win, State)
	:-
	accessObjStruct(tcl_doc_path, State, TclWin),
	(TclWin \= nil ->
		tcl_call(shl_tcli, [wm, deiconify, TclWin], _)
		;
		true
	).


shl_source_handlerAction(display_file_errors(NErrs, SPath, ErrsList), State)
	:-
	ErrsList = [],
	!,
	accessObjStruct(tcl_doc_path, State, TclWin),
	(TclWin \= nil ->
		send_self(State, clear_decorations),
		tcl_call(shl_tcli, [close_error_annotations, TclWin], _)
		;
		true
	).

display_file_errors_here.
		%% Here, ErrsList \= []:
shl_source_handlerAction(display_file_errors(NErrs, SPath, ErrsList), State)
	:-
display_file_errors_here,
	SourceFile = SPath,
	accessObjStruct(tcl_doc_path, State, InitTclWin),
	(InitTclWin \= nil ->
		tcl_call(shl_tcli, [close_and_reopen, InitTclWin], _),
		send_self(State, clear_decorations),
		TclWin = InitTclWin
		;
		shl_source_handlerAction(open_edit_win(SourceFile), State),
		accessObjStruct(tcl_doc_path, State, TclWin)
	),
	setObjStruct(errors_display, State, ErrsList),
	setObjStruct(source_file, State, SourceFile),
	tcl_call(shl_tcli, [add_line_numbers_and_syn_errs,TclWin],_),
	catenate([TclWin,'.listbox'], ErrListWin),
	indicate_errors(ErrsList, TclWin, ErrListWin).

indicate_errors([], DocID, _).
indicate_errors([Error | ErrsList], DocID, ErrWin)
	:-
	indic_err(Error, DocID, ErrWin),
	indicate_errors(ErrsList, DocID, ErrWin).

	%% P1: StartPos - char offset in file
	%% P2: CaratPos - char offset on line for error
	%% P3: Pos3
indic_err( error(syntax_error,
				[_,syntax(Context,P1,P2,P3,ErrorMessage,LineNumber,FS)]),
			DocID, ErrWin)
	:-!,
	tcl_call(shl_tcli, [syn_err_msg,ErrWin,LineNumber,ErrorMessage,DocID],_),
	tcl_call(shl_tcli, [err_indic,DocID,LineNumber,P1,P2,P3],_).

indic_err(prolog_system_error(s(ErrCode,Stream),[LineNumber|_]), DocID, ErrWin)
	:-
	builtins:expand_code(ErrCode, ErrorMessage, EType),
	!,
	tcl_call(shl_tcli, [syn_err_msg,ErrWin,LineNumber,ErrorMessage,DocID],_),
	tcl_call(shl_tcli, [err_indic0,DocID,LineNumber],_).


indic_err(Error, DocID, ErrWin).


export showsm/1.
showsm(BaseFile)
	:-
	builtins:get_primary_manager(ALSMgr),
	send(ALSMgr, obtain_src_mgr(BaseFile, FileMgr)),
	accessObjStruct(source_file, FileMgr, SF),
	accessObjStruct(base_file,FileMgr, BF),
	accessObjStruct(obp_file,FileMgr, OF),
	accessObjStruct(fcg,FileMgr, FCG),
	accessObjStruct(consult_mode,FileMgr, CM),
	accessObjStruct(last_consult, FileMgr, LC),
	accessObjStruct(tcl_doc_path,FileMgr, TDP),
	accessObjStruct(errors_display, FileMgr, ED),

	write_clauses([ source_file = SF, base_file = BF,
		obp_file = OF, fcg = FCG, consult_mode = CM,
		last_consult = LC, tcl_doc_path = TDP, errors_display = ED]).

/*
export showsms/0.
showsms
	:-
	builtins:get_primary_manager(ALSMgr),
	accessObjStruct(source_mgrs, ALSMgr, SrcMgrsList),
	show_sm_list(SrcMgrsList).

show_sm_list(V)
	:-
	var(V),
	!,
	printf(user_output,'!!SMList tail is variable!!\n',[]),flush_output.

show_sm_list([]).
show_sm_list([SrcM | SrcMgrsList])
	:-
	short_show_sm(SrcM),
	show_sm_list(SrcMgrsList).

short_show_sm(SrcM)
	:-
	accessObjStruct(base_file,SrcM, BF),
	write_clause(bf=BF), flush_output.
*/

endmod.
