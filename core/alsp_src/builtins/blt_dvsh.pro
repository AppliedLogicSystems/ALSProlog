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

export sd/0.
sd :- alsdev.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%% 			TCLTK-BASED SHELL STARTUP			%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
export alsdev/0.
alsdev
	:-
	consult(tcltk),
	tk_new(shl_tcli),
	builtins:sys_searchdir(ALSDIR),
	extendPath(ALSDIR, shared, Shared),
	pathPlusFile(Shared, 'alsdev.tcl', ALSDEVTCL),

	tcl_call(shl_tcli, [set,'ALSTCLPATH',Shared], _),
	tcl_call(shl_tcli, [source, ALSDEVTCL], _),
		%% At this point, the windows have been created;

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

%catenate(wait_loop,'.topals.txwin.text',WaitLoop),
%tcl_call(shl_tcli,[proc,WaitLoop,[],[


    sio:set_input(ISS),
    sio:set_output(OSS),

    %% Debugger streams: Initially regular debug io to console window:

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

    %% Debugger streams: The debugger window text window:
	%% Initially these don't have the debugger_[input,output] aliases;
	%% we cancel_alias & set_alias between the two pairs of streams,
	%% as the debugger window is popped up/down:

		%	open(console('debugger output'),write, OutGuiDStream,
    open(tk_win(shl_tcli, '.debugger_win.textwin.text'),write, OutGuiDStream,
	 ['$stream_identifier'(-8), alias(gui_debugger_output),
	 	buffering(line),type(text),
	 	maxdepth(8), line_length(76),
	 	depth_computation(nonflat)]),

		%	open(console('debugger input'), read, InGuiDStream,
    open(tk_win(shl_tcli, '.debugger_win.textwin.text'), read, InGuiDStream,
	 ['$stream_identifier'(-7), blocking(true),alias(gui_debugger_input),
	 	prompt_goal(flush_output(debugger_output))]),

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

	builtins:prolog_shell(ISS,OSS,alsdev).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%			HANDLING ^C and ^D					%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
export handle_ctl_d/1.
handle_ctl_d(Win)
	:-
	sio:current_input(Stream),
	sio:stream_name(Stream, SName),
	sio:stream_identifier(Stream, SID),
pbi_write(ctl_d(in=Win,ci_sname=SName,ci_sid=SID)),pbi_nl,pbi_ttyflush.
	%% Essentially need to simulate putting a ^D onto Stream;
	%% if SID = -1, have to make it act like getting ^D in the
	%% console in TTY mode (including ending [user] correctly).

export handle_ctl_c/1.
handle_ctl_c(Win)
	:-
	sio:current_input(Stream),
	sio:stream_name(Stream, SName),
	sio:stream_identifier(Stream, SID),
pbi_write(ctl_c(in=Win,ci_sname=SName,ci_sid=SID)),pbi_nl,pbi_ttyflush.
	%% Need to raise the ^C signal properly.


setup_prolog_flags
	:-
	static_flags_info(StaticEqsList),
	add_static_items_to_menu(StaticEqsList, 
				shl_tcli, '.topals.mmenb.settings.flags.static').

setup_dyn_flags 
	:-
	tcl_call(shl_tcli, [winfo,exists,'.dyn_flags.unknown'], UNK),
	(UNK = '1' ->
		true
		;
		changable_flags_info(DynList),
		add_dyn_items_menu(DynList, shl_tcli, '.dyn_flags', GlblTclVars)
	),
	tcl_call(shl_tcli, ['Window',show,'.dyn_flags'],_),
	mk_tcl_atom_list(GlblTclVars, AList),
	sprintf(atom(AcceptCmd),'{change_prolog_flags {%t}}', [AList]),
	tcl_call(shl_tcli, 
			['.dyn_flags.buttons.accept',configure,
				'-command',AcceptCmd], _).


				  

add_static_items_to_menu([], Interp, MenuPath).
add_static_items_to_menu([Item | List], Interp, MenuPath)
	:-
	sprintf(atom(Label), '%t', [Item]),
	tcl_call(Interp, [MenuPath,add,command,'-label',Label], _),
	add_static_items_to_menu(List, Interp, MenuPath).

add_dyn_items_menu([], _, _, []). 
add_dyn_items_menu([f(Flag,ValsList,Value) | DynList], Interp, 
					Win, [FlagValVar | RestFlagValVars])
	:-
	add_dyn_flag(Flag,ValsList,Value,Interp,Win,FlagValVar),
	add_dyn_items_menu(DynList, Interp, Win, RestFlagValVars).

add_dyn_flag(Flag,ValsList,Value,Interp,Win, FlagValVar)
	:-
	catenate(prolog_flag_,Flag,FlagValVar),
	mk_tcl_atom_list(ValsList, TclValsList),
	tcl_call(Interp, 
			[mk_labeled_option_button,Flag,Win,TclValsList,Value,FlagValVar],_).



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
	%%%%% 				DEBUGGER ROUTINES				%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	builtins,syscfg,rel_arith,xconsult,sio,pgm_info,debugger,tcltk,windows,
	alsshell,avl,cref,macroxp,objects,shellmak,ttyshlmk]
	).

export non_sys_modules/1.
non_sys_modules(Mods)
	:-
	findall(M, modules(M,_),L),
	sys_modules(SysMs),
	list_diff(L, SysMs, Mods).

export module_preds/2.
module_preds(M,L)
	:-
	findall(PA, 
			(all_procedures(M,P,A,R),R\=0,catenate([P,'/',A],PA)), 
			L).

endmod.	%% builtins

		%%---------------------------------------
		%%	Debugger tracing interaction
		%%		-- in module debugger
		%%---------------------------------------

module debugger.
use tcltk.

showGoalToUserWin(Port,Box,Depth, Module, XGoal, Response)
	:-
	printf(debugger_output,'(%d) %d %t: ', [Box,Depth,Port]),
	write_term(debugger_output, Module:XGoal,    [lettervars(false)]),
	flush_output(debugger_output),

	tcl_call(shl_tcli, [set_status_debugwin,Port,Box,Depth], _),
	getResponse(tcltk,Port,Box,Depth, Module, XGoal, Response).

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
		








endmod.

