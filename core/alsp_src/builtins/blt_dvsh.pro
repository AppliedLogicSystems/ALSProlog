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


	%% For now, needs to be in module user:
make_shell_prompts(alsdev, 1,'?- ','?-_').

make_shell_prompts(alsdev, N,Prompt1,Prompt2)
	:-
	N > 1,
	!,
	sprintf(PL1,'Break (%d) ?- ',[N]),
	sprintf(PL2,'          ?-_',[]),
	name(Prompt1,PL1),
	name(Prompt2,PL2).


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
	init_tk_alslib(shl_tcli,Shared),
	alsdev_splash(Shared),
	make_clinfo(CLInfo, alsdev, true), 	% verbosity = quiet
	get_command_line_info(DefaultShellCall,CommandLine,ResidualCommandLine,CLInfo),
	assertz(command_line(ResidualCommandLine)),
	setup_debugger_stubs,
	setup_search_dirs(CLInfo),
	ss_load_dot_alspro(CLInfo),
	library_setup,
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
	pathPlusFile(Shared, 'alsdev.tcl', ALSDEVTCL),
	tcl_call(shl_tcli, [source, ALSDEVTCL], _),
		%% At this point, the windows have been created;

	tcl_call(shl_tcli, [destroy,'.als_splash_screen'], _),

	open(tk_win(shl_tcli, '.topals.txwin.text'), read, ISS, 
		[alias(shl_tk_in_win)
			,prompt_goal(user_prompt_goal(shl_tk_out_win))
		]),
	open(tk_win(shl_tcli, '.topals.txwin.text'), write, OSS, 
		[alias(shl_tk_out_win)
		]),
	set_associated_output_alias(shl_tk_in_win, shl_tk_out_win),
	catenate('WaitForLine','.topals.txwin.text',WaitVar),
	catenate('DataLine','.topals.txwin.text',DataVar),

	tcl_call(shl_tcli, [set,WaitVar,0],_),
	tcl_call(shl_tcli, [set,DataVar,""],_),
	tcl_call(shl_tcli, 
		[set_top_bindings,'.topals.txwin.text',shl_tk_in_win,WaitVar,DataVar],_),
    sio:set_input(ISS),
    sio:set_output(OSS),

    %% Debugger streams: Initially regular debug io to console window:


/*****
		%	open(console('debugger output'),write, OutDStream,
	cancel_alias(debugger_output),
    open(tk_win(shl_tcli, '.topals.txwin.text'),write, OutDStream,
	 ['$stream_identifier'(-4), % alias(debugger_output),
	 	buffering(line),type(text), alias(console_debugger_output),
	 	maxdepth(8), line_length(76),
	 	depth_computation(nonflat)]),
	assign_alias(debugger_output, OutDStream),

		%	open(console('debugger input'), read, InDStream,
	cancel_alias(debugger_input),
    open(tk_win(shl_tcli, '.topals.txwin.text'), read, InDStream,
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
    open(tk_win(shl_tcli, '.debugwin.textwin.text'),write, OutGuiDStream,
	 ['$stream_identifier'(-8), alias(gui_debugger_output),
	 	buffering(line),type(text),
	 	maxdepth(8), line_length(76),
	 	depth_computation(nonflat)]),
	assign_alias(debugger_output, OutGuiDStream),

		%	open(console('debugger input'), read, InGuiDStream,
	cancel_alias(debugger_input),
    open(tk_win(shl_tcli, '.debugwin.textwin.text'), read, InGuiDStream,
	 ['$stream_identifier'(-7), blocking(true),alias(gui_debugger_input),
	 	prompt_goal(flush_output(debugger_output))]),
	assign_alias(debugger_input, InGuiDStream),

    %% Error stream
		%	open(console_error('error output'),write,OutEStream,
	cancel_alias(error_stream),
    open(tk_win(shl_tcli, '.topals.txwin.text'),write,OutEStream,
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

	setup_prolog_flags,

	install_alarm_handler,
	shell_alarm_interval(AlarmIntrv),
	alarm(AlarmIntrv,AlarmIntrv),

	debugger:init_visual_debugger,
	change_debug_io(debugwin),

	builtins:prolog_shell(ISS,OSS,alsdev).


alsdev_splash(TclPath)
	:-
%	subPath(TPL, TclPath),
%	append(TPL, [images,'turnstile_splash.gif'], TPLI),
%	tcl_call(shl_tcli, [file,join | TPLI], SPP),
	tcl_call(shl_tcli, [file,join,TclPath,'turnstile_splash.gif'], SPP),

	catenate('image create photo als_splash_gif -file ',SPP,Splashy),
	CL= [
		'wm withdraw .',
		'toplevel .als_splash_screen -bd 2 -relief flat',
		'wm withdraw .als_splash_screen ',
		Splashy,
		'wm overrideredirect .als_splash_screen 1 ',
		'label .als_splash_screen.label -image als_splash_gif -bd 1 -relief flat ',
		'pack .als_splash_screen.label -side top -expand 1 -fill both ',
		'wm geometry .als_splash_screen +270+200 ',
		'wm deiconify .als_splash_screen ',
		'update idletasks '],
	list_tcl_eval(CL, shl_tcli, _),
	tcl_call(shl_tcli, [update],_).







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

setup_prolog_flags
	:-
	static_flags_info(StaticEqsList),
	add_static_items_to_menu(StaticEqsList, 
				shl_tcli, '.topals.mmenb.settings.flags.static').


add_static_items_to_menu([], Interp, MenuPath).
add_static_items_to_menu([Item | List], Interp, MenuPath)
	:-
	sprintf(atom(Label), '%t', [Item]),
	tcl_call(Interp, [MenuPath,add,command,'-label',Label], _),
	add_static_items_to_menu(List, Interp, MenuPath).

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
	file_input_dialog('Defstruct file name:', Name,typ,DefstrFile),
	open(DefstrFile,append,TmpTS,[]),
	write_clause(TmpTS, DS, [quoted(true)]),
	close(TmpTS),
	sprintf(atom(Msg),'Defstruct %t added to file %t',[Name,DefstrFile]),
	info_dialog(Msg).

/*
open('tmpout.typ',append,TmpTS,[]),
write_clause(TmpTS, DS, [quoted(true)]),
close(TmpTS).
*/


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

endmod. %% builtins


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
	pgm_info,debugger,tcltk,windows,tk_alslib,
	alsshell,avl,cref,macroxp,objects,shellmak,ttyshlmk
	]).

export non_sys_modules/1.
non_sys_modules(Mods)
	:-
	findall(M, modules(M,_),L),
	sys_modules(SysMs),
	list_diff(L, SysMs, Mods).

export module_preds/2.
module_preds(user,L)
	:-!,
	findall(PA, 
			(all_procedures(user,P,A,R),P\=make_shell_prompts,R\=0,catenate([P,'/',A],PA)), 
			L0),
	(L0 = [] ->
		L = ['No predicates defined in user'] 
		; L = L0).

module_preds(M,L)
	:-
	findall(PA, 
			(all_procedures(M,P,A,R),R\=0,catenate([P,'/',A],PA)), 
			L0),
	(L0 = [] ->
		catenate('No predicates defined in ',M,Msg),
		L = [Msg] 
		; L = L0).

endmod.	%% builtins

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%% 		VISUAL DEBUGGER ROUTINES				%%%%%
	%%%%% 		  -- portions in module builtins		%%%%% 
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-[db_srctr].

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


	%% Expects FileName to exist and be accessible:
start_src_trace(FileName)
	:-
		%% Next 2 goals need only be here, because they
		%% will _ALWAYS_ run:
	init_visual_debugger,
	tcl_call(shl_tcli, [ensure_db_showing], _),
	get_src_trace_rec(FileName, Rec),
	access_dbstr(winname, Rec, WinName),
	tcl_call(shl_tcli, [win_exists,WinName], true),
	!,
	set_dbstr(head_tag, Rec, 0),
	set_dbstr(call_tag, Rec, 0),
	tcl_call(shl_tcli, [reset_st_win,WinName], _).

start_src_trace(FileName)
	:-
		%% Old Rec exists, but window has been destroyed, so
		%% remove old rec, and start over:
	(get_src_trace_rec(FileName, OldRec) ->
		remove_src_trace_rec(OldRec)
		;
		true),
	create_src_trace(FileName, WinName, TextWinName, BaseFileName, 
					 NumLines, LineIndex),
	inverted_index(LineIndex, InvertedLineIndex),
	builtins:file_clause_group(BaseFileName, FCG),
	xmake_dbstr(Rec,
		[FileName,FCG,WinName, TextWinName,NumLines, 
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


create_src_trace(FileName, WinName, TextWinName, BaseFileName, 
				 NumLines, LineIndex)
	:-
		%% deal with extension, full path completely (later):
	pathPlusFile(_,FF,FileName),
	(filePlusExt(BaseFileName,Ext,FF) ->
		true
		;
		BaseFileName = FF
	),
	catenate('.debug_st_',BaseFileName,WinName),
	catenate(WinName, '.textwin.text', TextWinName),
	catenate('Source Trace: ',FF, Title),
	tcl_call(shl_tcli, ['vTclWindow.debug_source_trace',WinName,Title],R),
	tcl_call(shl_tcli, [load_file_to_win,FileName,TextWinName],LoadRes),
	LoadRes = [NumLines, LineIndex].

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

	%% '$dbg_aph' ::

showGoalToUserWin(call,Box,Depth, Module, '$dbg_aph'(ClsGrp,Start,End), debug)
	:-!,
%printf('+#+%t: $dbg_aph(%t,%t,%t)\n',[call,ClsGrp,Start,End]),flush_output,
	color_my_port(ClsGrp,Start,End,call,head_tag).
	
showGoalToUserWin(fail,Box,Depth, Module, '$dbg_aph'(ClsGrp,Start,End), debug)
	:-!,
%printf('+#+%t: $dbg_aph(%t,%t,%t)\n',[fail,ClsGrp,Start,End]),flush_output,
	color_my_port(ClsGrp,Start,End,fail,head_tag),
	fail.
	
showGoalToUserWin(redo,Box,Depth, Module, '$dbg_aph'(ClsGrp,Start,End), debug)
	:-!,
%printf('+#+%t: $dbg_aph(%t,%t,%t)\n',[fail,ClsGrp,Start,End]),flush_output,
	color_my_port(ClsGrp,Start,End,redo,head_tag),
	fail.
	
showGoalToUserWin(Port,Box,Depth, Module, '$dbg_aph'(ClsGrp,Start,End), debug)
	:-!.
%printf('+#+%t: $dbg_aph(%t,%t,%t)\n',[Port,ClsGrp,Start,End]),flush_output.


	%% '$dbg_aphe' ::

	% "call" here is successful exit from the clause:
showGoalToUserWin(call,Box,Depth, Module, '$dbg_aphe'(ClsGrp,Start,End), debug)
	:-!,
%printf('-#-%t: $dbg_aphe(%t,%t,%t)\n',[Port,ClsGrp,Start,End]),flush_output,
	color_my_port(ClsGrp,Start,End,exit,head_tag).

showGoalToUserWin(redo,Box,Depth, Module, '$dbg_aphe'(ClsGrp,Start,End), debug)
	:-!,
%printf('-#-%t: $dbg_aphe(%t,%t,%t)\n',[Port,ClsGrp,Start,End]),flush_output,
	color_my_port(ClsGrp,Start,End,redo,head_tag),
%	clear_my_tag(ClsGrp,Start,End,TagName),
	fail.

showGoalToUserWin(fail,Box,Depth, Module, '$dbg_aphe'(ClsGrp,Start,End), debug)
	:-!,
	fail.

showGoalToUserWin(Port,Box,Depth, Module, '$dbg_aphe'(ClsGrp,Start,End), debug)
	:-!.
%printf('-#-%t: $dbg_aphe(%t,%t,%t)\n',[Port,ClsGrp,Start,End]),flush_output.

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
%printf('###call: $dbg_apf(%t,%t,%t)\n',[ClsGrp,Start,End]),flush_output,
	color_my_port(ClsGrp,Start,End,exit,head_tag).

showGoalToUserWin(redo,Box,Depth, Module, '$dbg_apf'(ClsGrp,Start,End), debug)
	:-!,
	fail.

showGoalToUserWin(_,Box,Depth, Module, '$dbg_apf'(ClsGrp,Start,End), debug)
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
%printf('###call: $dbg_apg(%t,%t,%t)\n',[ClsGrp,Start,End]),flush_output,
	color_my_port(ClsGrp,Start,End,call,call_tag),
	!.

	%% "redo" here is actually failure of the acutal goal:
showGoalToUserWin(redo,Box,Depth, Module, '$dbg_apg'(ClsGrp,Start,End), debug)
	:-!,
	color_my_port(ClsGrp,Start,End,fail,call_tag),
	fail.

showGoalToUserWin(Port,Box,Depth, Module, '$dbg_apg'(ClsGrp,Start,End), debug)
	:-!.
%printf('-#-%t: $dbg_apg(%t,%t,%t)\n',[Port,ClsGrp,Start,End]),flush_output.

	%% '$dbg_apge' ::

	%% "call" here is really exit for the acutal goal:
showGoalToUserWin(call,Box,Depth, Module, '$dbg_apge'(ClsGrp,Start,End), debug)
	:-!,
%printf('###call: $dbg_apge(%t,%t,%t)\n',[ClsGrp,Start,End]),flush_output,
	color_my_port(ClsGrp,Start,End,exit,call_tag).

	%% "redo" here is really redo for the acutal goal:
showGoalToUserWin(redo,Box,Depth, Module, '$dbg_apge'(ClsGrp,Start,End), debug)
	:-!,
%printf('###redo: $dbg_apge(%t,%t,%t)\n',[ClsGrp,Start,End]),flush_output,
	color_my_port(ClsGrp,Start,End,redo,call_tag).


showGoalToUserWin(_,Box,Depth, Module, '$dbg_apge'(ClsGrp,Start,End), debug)
	:-!.
%printf('-#-%t: $dbg_apge(%t,%t,%t)\n',[Port,ClsGrp,Start,End]),flush_output.

	%%---------------------------------------------------
	%%		ALL OTHERS
	%%---------------------------------------------------
showGoalToUserWin(Port,Box,Depth, Module, XGoal, Response)
	:-
	printf(debugger_output,'(%d) %d %t: ', [Box,Depth,Port]),
	write_term(debugger_output, Module:XGoal,    [lettervars(false)]),
	flush_output(debugger_output),
	get_mrfcg(MRFCG),

%printf('AllOthers:Port=%t MRFCG=%t\n',[Port,MRFCG]),

	(MRFCG = 0 -> true
		;
		((Port=call; Port=redo) -> true
			;
			re_color_port(Port, MRFCG, STRec)
		)
	),
	tcl_call(shl_tcli, [set_status_debugwin,Port,Box,Depth], _),
	!,
	getResponse(tcltk,Port,Box,Depth, Module, XGoal, Response).


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

port_color(call, blue).
port_color(exit, green).
port_color(fail, red).
port_color(redo, yellow).

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
	act_on_response(Resp0,Port,Box,Depth, Module,Goal,tcltk,Response).
		

gui_spy(Module, PredDesc)
	:-
	(sub_atom(PredDesc,0,_,_,'No predicates ') ->
		true
		;
		atomread(PredDesc, Pred/Arity),
		spy(Module, Pred, Arity)
	).


tktxt_info(ClsGrp,TextWin,Start,End,Rec,
			StartLine,StartChar,EndLine,EndChar)
	:-
	get_st_rec_by_fcg(ClsGrp, Rec),
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
	append([TextWinName,tag,configure,TagName],Pairs,Cmd),
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

endmod.





