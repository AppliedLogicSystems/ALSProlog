/*=============================================================*
 |		blt_shl.pro
 |	Copyright (c) 1986-1994 Applied Logic Systems, Inc.
 |
 |	The actual environmental shell; both the tty
 |	version, and the underlying portion of the 
 |	GUI version.
 |
 |	Authors: Kevin A. Buettner, Ken Bowen
 |	Original Creation Date: 3/20/86
 *=============================================================*/

module builtins.
use objects.
use sio.
 
/*-------------------------------------------------------------------------------*
 | start_shell/1
 | start_shell(DefaultShellCall)
 |
 | Performs startup initializations such as parsing the command line,
 | loading files, etc. prior to the starting a shell.  DefaultShellCall
 | is the shell call to make.
 *-------------------------------------------------------------------------------*/

start_shell(DefaultShellCall)
	:-
	%% Setup debugger entries (needs to be done early because
	%% ss_parse_command_line can cause debugger to be called
	%% [if it sees switch -nwd] )
		%% Conditional handles systems without alarm/2:
	(alarm(0,0) -> true ; true),
	builtins:libhide(debugger, [builtins,debugger],
				[ (trace)/0, (trace)/2, toggle_mod_show/1, leash/1,
		  		  (spy)/0, (spy)/1, (spy)/2, (spy_pat)/3, (spyWhen)/1,
		  		  (spyWhen)/2, (nospy)/0, (nospy)/1, (nospy)/3,
		  		  spying/0, debugging/0, list_spypoints/0 ]),

	%% Get the raw command line and assert it.
	abolish(command_line,1),
	pbi_get_command_line(RawCommandLine),
	assertz(command_line(RawCommandLine)),

	%% get the command line, but ignore the image name
	retract(command_line([ImageName|CommandLine])),
	!,
	CLInfo = clinfo(DefaultShellCall,	/* -g: goal to run */
					false,				/* -v/-q: verbosity */
					[],					/* files to consult */
					ImageName,
					default,			/* -nwd: debugger to set up */
					[],					/* -S init search list */
					other_flags),		/* room for expansion */

	debugger:nospy,
	ss_parse_command_line(CommandLine, ResidualCommandLine, CLInfo),
	assertz(command_line(ResidualCommandLine)),

	arg(3, CLInfo, Files),
	arg(1, CLInfo, ShellCall),
	arg(6, CLInfo, RevCmdLineSearch),
	dreverse(RevCmdLineSearch, CmdLineSearch),

	ss_init_searchdir(CmdLineSearch),
	ss_load_dot_alspro,

	(arg(2,CLInfo,true) -> consultmessage(on) ; consultmessage(off)),
	ss_load_files(Files),
	(arg(2,CLInfo,quiet) -> consultmessage(off) ; consultmessage(on)),
	!,
	user:ShellCall.

start_shell(_).

/*-----------------------------------------------------------------*
 | ss_parse_command_line/3
 | ss_parse_command_line(CommandLine, ResidualCommandLine, CLInfo),
 | ss_parse_command_line(+, -, +/-),
 *-----------------------------------------------------------------*/

	%% Empty/end of command line:
ss_parse_command_line([], [], CLInfo)
	:-!.

	%% -p: Start application part of command line:
ss_parse_command_line(['-p' | T], T, CLInfo)
	:-!.

	%% -p: Start application part of command line, pushing on image name:
ss_parse_command_line(['-P' | T], [ImageName | T], CLInfo)
	:-!,
	arg(4,CLInfo,ImageName).

	%% -g <Goal>: Start up goal:
ss_parse_command_line(['-g', GoalAtom | T], L, CLInfo)
	:-!,
	atom_codes(GoalAtom, GoalCodes),
	term_codes(Goal, GoalCodes),	%% FIXME: Catch syntax errors
	mangle(1,CLInfo,Goal),
	ss_parse_command_line(T,L,CLInfo).

	%% -b: "Batch" mode: exit after running -g startup goal (don't run default shell):
ss_parse_command_line(['-b' | T], L, CLInfo)
	:-!,
	mangle(1, CLInfo, true),
	ss_parse_command_line(T, L, CLInfo).

	%% -v: Turn on verbose mode:
ss_parse_command_line(['-v' | T], L, CLInfo)
	:-!,
	mangle(2, CLInfo, true),
	ss_parse_command_line(T, L, CLInfo).

	%% -q: Turn off verbose mode:
ss_parse_command_line(['-q' | T], L, CLInfo)
	:-!,
	mangle(2, CLInfo, quiet),
	ss_parse_command_line(T, L, CLInfo).

	%% -S: Atom - File to add to search list;
	%% in reverse order; later reverse it:
ss_parse_command_line(['-s', File | T], L, CLInfo)
	:-!,
	arg(6, CLInfo, PrevSL),
	mangle(6, CLInfo, [File | PrevSL]),
	ss_parse_command_line(T, L, CLInfo).

	%% -a: Atom - Assert Atom in module user.
	%% -A: Atom - Assert Atom in module user.
ss_parse_command_line(['-A', Expr | T], L, CLInfo)
	:-!,
	ss_parse_command_line(['-a', Expr | T], L, CLInfo).

ss_parse_command_line(['-a', Expr | T], L, CLInfo)
	:-!,
	cmd_line_A(Expr),
	ss_parse_command_line(T, L, CLInfo).

	%% -obp: Keep obp files in directory where image is running:
ss_parse_command_line(['-obp' | T], L, CLInfo)
	:-!,
	obp_in_cur,
	ss_parse_command_line(T, L, CLInfo).


	%% -nwd: Set debugger to "nowins"
ss_parse_command_line(['-nwd' | T], L, CLInfo)
	:-!,
	debugger:nospy,
	(debugger:set_debug_io(nowins),!;true),
	ss_parse_command_line(T, L, CLInfo).


	%% Otherwise: should be a file to be loaded:
ss_parse_command_line([File | T], L, CLInfo)
	:-
	arg(3, CLInfo, Files),
	append(Files, [File], NewFiles),
	mangle(3, CLInfo, NewFiles),
	ss_parse_command_line(T, L, CLInfo).

cmd_line_A(Expr)
	:-
	atomread(Expr,Mod:AX),
		%% Must intern the module:
	functor(FF,Mod,0), functor(FF,SMod,0),
	create_new_module(SMod),
	in_assrt(AX, SMod).
	
cmd_line_A(Expr)
	:-
	user:assert(Expr).

in_assrt((AX,BX), SMod)
	:-!,
	in_assrt(AX, SMod),
	in_assrt(BX, SMod).

in_assrt(AX, SMod)
	:-
	SMod:assert(AX).
/*---------------------------------------
 |	ss_init_searchdir/1
 |	ss_init_searchdir(CmdLinePaths)
 |	ss_init_searchdir(+)
 |	
 |	ss_init_searchdir0/1
 |	ss_init_searchdir0(CmdLinePaths)
 |	ss_init_searchdir0(+)
 |	
 |	split_paths/2
 |	split_paths(String,PathsList)
 |	split_paths(+,-)
 *--------------------------------------*/
ss_init_searchdir(CmdLineList)
	:-
	(getenv('ALSPATH',ALSPATH) ->
		split_paths(ALSPATH, ALSPATHS),
		append(CmdLineList, ALSPATHS, PATHS)
		;
		PATHS = CmdLineList
	),
	ss_init_searchdir0(PATHS).

split_paths(String, [Path | RestPaths])
	:-
	path_separator(PS),
	atom_split(String,PS,Path,RestPaths),
	!,
	split_paths(RestPaths).

split_paths(Path, [Path]).


ss_init_searchdir0([Path | Paths])
	:-
	assertz(searchdir(Path)),
	ss_init_searchdir0(Paths).
ss_init_searchdir0([]).

/*---------------------------------------
 |	ss_load_files/1
 |	ss_load_files(FileList)
 |	ss_load_files(+)
 *--------------------------------------*/
ss_load_files([]) 
	:-!.
ss_load_files([F | T])
	:-
	reconsult(F),
	ss_load_files(T).
    
/*---------------------------------------
 |	ss_load_dot_alspro
 *--------------------------------------*/
ss_load_dot_alspro
	:-
	als_system(L),
	(dmember(os=dos,L) ->
		File = 'alspro.pro' ; File = '.alspro' ),
	ss_load_dot_alspro(File).

ss_load_dot_alspro(AutoFile)
	:-
	exists_file(AutoFile),
	!,
	reconsult(AutoFile).
ss_load_dot_alspro(AutoFile)
	:-
		%% What about DOS (also Mac, etc.) here?:
	getenv('HOME',HOME),
	pathPlusFile(HOME,AutoFile,File),
	exists_file(File),
	!,
	reconsult(File).
ss_load_dot_alspro(_).

/*-------------------------------------------------
 | print_banner/1
 |		- prints out the initial banner
 *------------------------------------------------*/
print_banner(OutS,L) 
	:-
	system_name(L, Name),
	dmember(os_variation = OSVar, L),
	dmember(prologVersion = Version, L),
	dmember(wins=WinsName, L),
	name(WinsName, [InC | WNCs]),
	UInC is InC - 32,
	name(WBan, [UInC | WNCs]),
	!,
	printf(OutS,'%s Version %s [%s] (%s)\n',[Name,Version,OSVar,WBan]),
	printf(OutS,'   Copyright (c) 1987-95 Applied Logic Systems, Inc.\n\n',[]).

system_name(L, Name)
	:-
	dmember(processor = Proc, L),
	system_name_proc(Proc, Name).

system_name_proc('port_thread', 'ALS Prolog (Threaded)') :-!.
system_name_proc('port_byte', 'ALS Prolog (Byte)') :-!.
system_name_proc(_, 'ALS Prolog (Native)').

/*-------------------------------------------------------------------------*
 | prolog_shell is a top-level shell for submitting queries to the prolog
 | system and getting answers.  Unlike our previous shell written in C,
 | it knows about certain debugger global variables and so will interact well
 | with the debugger.
 *-------------------------------------------------------------------------*/
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% We store (read) prompts on a stack in a global 
	%% variable because we (may) come and go from the
	%% window system, so we can't hold on to them:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-	make_gv("_shell_prompts"), 	set_shell_prompts( [('?- ','?-_')] ).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% This is the default system tty shell:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prolog_shell
	:-
	prolog_shell(user_input,user_output).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Used to start both the default system shells, and
	%% also shells talking to windows:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prolog_shell(InStream,OutStream) 
	:-
	init_prolog_shell(InStream, OutStream,alspro,CurLevel,CurDebuggingState,Wins),
	prolog_shell_loop(InStream,OutStream,Wins),
	shell_exit(InStream, OutStream,CurLevel,CurDebuggingState).

/*-----------------------------------------------------------------------*
 | init_prolog_shell/6
 | init_prolog_shell(InStream, OutStream,ID,CurLevel,DebugState,Wins)
 | init_prolog_shell(+, +,+,-,-,-)
 |
 *-----------------------------------------------------------------------*/
module windows.

endmod.

:-dynamic(windows:'$toplevel$'/1).

export init_prolog_shell/6.
init_prolog_shell(InStream,OutStream,ID,CurLevel,CurDebuggingState,Wins)
	:-
	als_system(SysList),
	sio:input_stream_or_alias_ok(InStream, RealInStream),
	set_stream_pgoals(RealInStream, user_prompt_goal(OutStream) ),

	get_debugging_state(CurDebuggingState),
	set_debugging_state(debug_state(debug_off,0,1,0,debug_off)),
	get_shell_level(CurLevel),
	NewLevel is CurLevel+1,
	set_shell_level(NewLevel),
	make_prompts(ID, CurLevel,Prompt1,Prompt2),
		%% store prompts on a stack in a global variable
		%% because we (may) come and go from X, so we
		%% can't hold on to them:
	get_shell_prompts( CurPromptsStack ),
	set_shell_prompts( [(Prompt1,Prompt2) | CurPromptsStack] ),
	print_banner(OutStream,SysList),
	dmember(wins=InitWins, SysList),
	dmember(os=OS, SysList),
	dmember(os_variation=OSMinor, SysList),
	(InitWins = nowins ->
			Wins = nowins
			;
			(windows:'$toplevel$'(_) ->
/*
				Wins = InitWins
*/
				((OS=macos ; OSMinor = djgpp ; OSMinor = djgpp2)
					->
					Wins = InitWins/0
					;
					Wins = InitWins
				)

				;
				Wins = nowins
			)
	),
	push_prompt(Wins,OutStream,Prompt1).

push_prompt(nowins,_,_) :-!.
push_prompt(Wins,OutStream,Prompt1)
	:-
	put_atom(OutStream,Prompt1),
	flush_output(OutStream).


shell_exit(InStream, OutStream,Level,DebuggingState)
	:-
	set_shell_level(Level),
	set_debugging_state(DebuggingState),
	get_shell_prompts( [_ | CurPromptsStack] ),
	set_shell_prompts( CurPromptsStack ).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% The Main Loop:
	%% -- This replaces the old shell2/4
	%%
	%% The previous shell2/4 embodied the complete Prolog 
	%% read/execute loop.  However, the X version needs to
	%% break the loop by sending control out to X, and then
	%% returning control in to Prolog via a callback. 
	%% Roughly, sending control out to X corresponds to the
	%% issuance of a blocking read in the TTY version. The
	%% callback (attached to the user hitting return) 
	%% roughly corresponds to the completion of the blocking
	%% read (also driven by the user hitting return).
	%% In fact, what really occurs is that the Prolog system
	%% becomes a service layer: After startup, we simply jump
	%% out to X (XtMainLoop).  Then callbacks corresponding
	%% to returns cause the stream to be read and the term
	%% to be executed.
	%%
	%% So the organization of prolog_shell_loop/2 simply
	%% reflects the need to support this breakup.  It 
	%% allows us to still have the TTY version loop just
	%% like it did before, but it allows the X version to
	%% cleanly call the part (shell_read_execute/4) which reads 
	%% one term from the input stream and executes it.
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prolog_shell_loop(InStream,OutStream,Wins) 
	:-
		%% Read one term and execute it:
	shell_read_execute(InStream,OutStream,Wins,Status),
		%% Decide whether to continue or exit:
	continue_prolog_loop(Status),
	!,
	prolog_shell_loop(InStream,OutStream,Wins).

prolog_shell_loop(_,_,_).

export shell_read_execute/4.
shell_read_execute(InStream,OutStream,Wins,Status)
	:-
	shell_read(InStream,OutStream,InitGoal,NamesOfVars,Vars),
	shell_execute(InStream,OutStream,InitGoal,NamesOfVars,Vars,Wins,Status),
	!.

continue_prolog_loop(halt) 
	:-!, 
	halt.
continue_prolog_loop(Status).

	%% This is the same as the old shell_read/7, except that 
	%% instead of carrying the prompts around in variables,
	%% it gets them out of a global variable -- necessary if
	%% control goes out to X (XtMainLoop) each pass around
	%% the Prolog loop:
shell_read(InStream,OutStream,G,N,V) 
	:-
	get_shell_prompts( [(Prompt1,Prompt2) | _] ),
	flush_input(InStream),
	catch(trap(shell_read0(Prompt1,Prompt2,InStream,G,N,V), 
			   throw_cntrl_c),
		  _,
		  ( write(OutStream,' <input discarded>'),nl(OutStream),
		      flush_output(OutStream),
		      shell_read(InStream,OutStream,G,N,V)) 
		 ).

shell_read0(Prompt1,Prompt2,InStream,G,N,V) 
	:-
	sio:get_user_prompt(OldPrompt),
	sio:set_user_prompt(Prompt1),
	sio:skip_layout(InStream),
	!,
	sio:set_user_prompt(Prompt2),
	read_term(InStream,G,[vars_and_names(V,N)]),
	sio:set_user_prompt(OldPrompt).

shell_read0(Prompt1,Prompt2,InStream,stream_not_ready,[],[]).

	%% This contains the guts of the old shell2, following 
	%% the shell_read:
shell_execute(InStream,OutStream,InitGoal,[],[],Wins,continue)
	:-
	nonvar(InitGoal),
	InitGoal = stream_not_ready,
	!.

:-alsshell:dynamic(getTracingFlag/1).
:-dynamic(getTracingFlag/1).

shell_execute(InStream,OutStream,InitGoal,NamesOfVars,Vars,InWins,Status)
	:-
	sio:get_user_prompt(UsersPrompt),
	(alsshell:getTracingFlag(tracing) ->
		Goal = (trace InitGoal)
		;
		Goal = InitGoal
	),
	flush_input(InStream),
	sio:set_user_prompt(UsersPrompt),
	wins_wakeup(InWins, Wins, Alarm),
	!,
	((nonvar(Goal),Goal=halt) ->
		Status = halt
		;
		sio:input_stream_or_alias_ok(InStream, RealInStream),
		stream_blocking(RealInStream,OldBlocking),
		set_stream_blocking(RealInStream,true),
		catch(do_shell_query(Goal,NamesOfVars,Vars,Alarm,InStream,OutStream),
	      		Reason,
	      		( shell_exception(Reason)
					,set_stream_blocking(RealInStream,OldBlocking))
			  ),
		set_stream_blocking(RealInStream,OldBlocking),
		get_shell_prompts( [(Prompt1,_) | _] ),
		push_prompt(Wins,OutStream,Prompt1),
		Status = continue
	).

wins_wakeup(nowins, nowins, 0) :-!.
wins_wakeup(Wins/AlarmInterval, Wins, AlarmInterval) :-!.
wins_wakeup(Wins, Wins, 0).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% get_debugging_state(State)
	%% set_debugging_state(State)
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_debugging_state(debug_state(Int,Call,Depth,Retry,DInt)) 
	:-
	getPrologInterrupt(Int),
	getCall(Call),
	getDepth(Depth),
	getRetry(Retry),
	getDebugInterrupt(DInt).

set_debugging_state(debug_state(Int,Call,Depth,Retry,DInt)) 
	:-
	setPrologInterrupt(Int),
	setCall(Call),
	setDepth(Depth),
	setRetry(Retry),
	setDebugInterrupt(DInt).


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% make_prompts(Wins, Level,Prompt1,Prompt2)
	%%
	%% make_prompts is responsible for building the 
	%% prompts to be displayed.
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
make_prompts(nowins, N,Prompt1,Prompt2) 
	:-
	N > 0,
	!,
	sprintf(PL1,'Break (%d) ?- ',[N]),
	sprintf(PL2,'          ?-_',[]),
	name(Prompt1,PL1),
	name(Prompt2,PL2).

make_prompts(nowins, _,'?- ','?-_') 
	:-!.
make_prompts(Wins, N,'','').
*/

:-user:dynamic(make_shell_prompts/4).
make_prompts(ID, N,Prompt1,Prompt2) 
	:-
	user:make_shell_prompts(ID, N, Prompt1, Prompt2),
	!.

make_prompts(_, N,Prompt1,Prompt2) 
	:-
	N > 0,
	!,
	sprintf(PL1,'Break (%d) ?- ',[N]),
	sprintf(PL2,'          ?-_',[]),
	name(Prompt1,PL1),
	name(Prompt2,PL2).

make_prompts(_, _,'?- ','?-_') 
	:-!.

/*-----------------------------------------------------------------------*
 |	do_shell_query/5 executes the query and displays the answers.  The
 |	layering is necessary since a top level cut may cut away the top
 |	choice point.  A subsequent failure would then force the whole thing
 |	to fail.
 *-----------------------------------------------------------------------*/

do_shell_query(Goal0,VarNames,_,_,InStream,OutStream) 
	:-
	var(Goal0),
	VarNames = [VV | _],
	!,
			%% bad_goal: "Improper Goal: %t\n"
	prolog_system_error(bad_goal, [VV]).

do_shell_query(exit,_,_,_,_,_) 
	:-!,
	fail.

do_shell_query(end_of_file,_,_,_,InStream,_) 
	:-
	flush_input(InStream),      %% reset eof condition
	!,
	fail.

do_shell_query((?- Goal0),VarNames,Vars,Alarm,InStream,OutStream) 
	:-!,
	do_shell_query(Goal0,VarNames,Vars,Alarm,InStream,OutStream).

do_shell_query(Goal0,VarNames,Vars,AlarmIntrv,InStream,OutStream) 
	:-
	gc,			%% Let user start fresh
	getDebugInterrupt(DInt),
	setPrologInterrupt(DInt),
	setCall(0),		%% Start Call,
	setDepth(1),            %%    Depth
	setRetry(0),            %%    and Retry in case of debugging.
	set_alarm_clock(Goal0,AlarmIntrv),
	xform_command_or_query(Goal0,Goal1),
	do_shell_query2(user,Goal1),
	unset_alarm(AlarmIntrv),
	dbg_notrace,
	dbg_spyoff,
	showanswers(VarNames,Vars,InStream,OutStream),
	!.

set_alarm_clock((trace _),_) :-!.
set_alarm_clock(_,0) :-!.
set_alarm_clock(_,AlarmIntrv)
	:-
	AlarmIntrv > 0,
	!,
pbi_write(setting_alarm_clock(Goal0,AlarmIntrv)),pbi_nl,pbi_ttyflush,
	windows:set_event_handler(sigalrm,dev_alarm),
	alarm(AlarmIntrv,AlarmIntrv).
set_alarm_clock(_,_) :-!.

unset_alarm(0) :-!.
unset_alarm(AlarmIntrv)
	:-
	alarm(0,0).
unset_alarm(AlarmIntrv)
	:-
	alarm(AlarmIntrv,AlarmIntrv),
	!,
	fail.

do_shell_query(Goal,VarNames,Vars,AlarmIntrv,InStream,OutStream) 
	:-
	unset_alarm(AlarmIntrv),
	dbg_notrace,
	dbg_spyoff,
	print_no(OutStream).

export do_shell_query2/2.
do_shell_query2(Mod,Goal) 
	:-
	getPrologInterrupt(Int),
	(Int=debug_user ; Int=debug_init),
	!,
	dbg_spyon,
	callWithDelayedInterrupt(Mod,Mod:Goal).

do_shell_query2(Mod,Goal) 
	:-
	getPrologInterrupt(Int),
	(Int=spying ; Int=jumping),
	!,
	dbg_spyon,
	Mod:Goal.

do_shell_query2(Mod,Goal) 
	:-
	Mod:Goal.

showanswers(N,V,InStream,OutStream) 
	:-
	write_substs(OutStream,N,V),
	flush_output(OutStream),
	!,
	flush_input(InStream),
	sio:get_user_prompt(UsersPrompt),
	sio:set_user_prompt(''),
	sa_cont(InStream,UsersPrompt,OutStream).

showanswers(_,_,InStream,OutStream) 
	:-
	flush_input(InStream),  %% reset eof in event that calling goal set it
	print_yes(OutStream).

sa_cont(InStream,UsersPrompt,OutStream)
	:-
	get_code(InStream,C,[blocking(true)]),
	disp_sa_cont(C,InStream,UsersPrompt,OutStream).

disp_sa_cont(-2,InStream,UsersPrompt,OutStream)  %% stream not ready
	:-!,
	sio:wait_data(window, InStream, get_code(InStream,C)),
	sio:set_user_prompt(UsersPrompt),
	flush_input(InStream),
	succeed_or_fail(C,OutStream).

disp_sa_cont(C,InStream,UsersPrompt,OutStream)
	:-
	sio:set_user_prompt(UsersPrompt),
	flush_input(InStream),
	succeed_or_fail(C,OutStream).

print_no(OutStream) 
	:- 
	nl(OutStream),
	write(OutStream,'no.'),
	nl(OutStream).

print_yes(OutStream) 
	:-
	nl(OutStream),
	write(OutStream,'yes.'),
	nl(OutStream).

succeed_or_fail(0';,_) 
	:- !, fail.
succeed_or_fail(_,OutStream) 
	:- 
	print_yes(OutStream).

	%shell_exception(What) :- pbi_write(What),pbi_nl,fail. %% debug clause

shell_exception(abort) 
	:-!,
			%% abort:  "Execution aborted.\n"
	prolog_system_error(abort,[]).

shell_exception(stack_overflow(Goal)) 
	:-!,
	'$protect_bottom_stack_page',
			%% stack_over:  "Execution aborted due to stack overflow.\n"
	prolog_system_error(stack_over,[]).

shell_exception(heap_overflow(Goal)) 
	:-!,
	reset_wm_normal,
	prolog_system_error(heap_over,[]).

shell_exception(error(ET,ImpDef)) 
	:-!,
	gc,
	prolog_system_error(error(ET,ImpDef),[]).

shell_exception(Reason) 
	:-
	functor(Reason,EventId,1),
	!,
	gc,
	arg(1,Reason,Call),
			%% no_handler: "No handler for Event: %t\nGoal: %t\n"
	prolog_system_error(no_handler,[EventId,Call]),
			%% abort:  "Execution aborted.\n"
	prolog_system_error(abort,[]).

shell_exception(Reason) 
	:-
	gc,
	prolog_system_error(uncaught,[throw(Reason)]).

/*----------------------*
 | abort/0
 *----------------------*/
export abort/0.
abort 
	:- 
	throw(abort).

endmod.		%% blt_shl.pro: Development shell
