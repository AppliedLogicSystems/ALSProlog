/*=============================================================*
 |		blt_shl.pro
 |	Copyright (c) 1986-1996 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |	The actual environmental shell; both the tty
 |	version, and the underlying portion of the 
 |	GUI version.
 |
 |	Authors: Kevin A. Buettner, Ken Bowen
 |	Original Creation Date: 3/20/86
 |  Major revisions: 1995-96
 *=============================================================*/

module builtins.
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
	catch(start_shell0(builtins:prolog_shell), 
			E,
			(shell_exception(E), throw(E))
		).

:- dynamic(genFileLocn/3).

start_shell0(DefaultShellCall)
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
		  		  spying/0, debugging/0, list_spypoints/0,set_debug_io/1 ]),

	%% Get the raw command line and assert it.
	abolish(command_line,1),
	pbi_get_command_line(RawCommandLine),

	%% get the command line, but ignore the image name
	(RawCommandLine = [Image | CommandLine] ; CommandLine = []),
	!,
	CLInfo = clinfo(true,				/* -g: goal to run */
					false,				/* -v/-q: verbosity */
					[],					/* files to consult */
					ImageName,
					default,			/* -nwd: debugger to set up */
					[],					/* -s init search list */
					DefaultShellCall,	/* shell/or not */
					[]),				/* Command-line asserts */

	ss_parse_command_line(CommandLine, ResidualCommandLine, CLInfo),
	assertz(command_line(ResidualCommandLine)),

	arg(6, CLInfo, RevCmdLineSearch),
	dreverse(RevCmdLineSearch, CmdLineSearch),
	ss_init_searchdir(CmdLineSearch),
	ss_load_dot_alspro,

%	arg(7, CLInfo, CLShellCall),
%	arg(2,CLInfo,Verbosity),
%	(CLShellCall=(builtins:prolog_shell) -> 
%		ConsultNoise = Verbosity 
%		;
%		ConsultNoise = true
%	),
	arg(2,CLInfo,ConsultNoise),

	OutputStream = user_output,
	als_system(SysList),
	(ConsultNoise = true -> 
		true ; 
		print_banner(OutputStream,SysList)
	),
	(ConsultNoise = true -> true ;
		als_advise(OutputStream, 'Setting up library indicies...may take a moment...',[])),
	setup_libraries,
	(ConsultNoise = true -> true ; 
		als_advise(OutputStream, 'Done.\n',[]),
		nl(OutputStream),
		flush_output(OutputStream)),

	arg(3, CLInfo, Files),
	set_consult_messages(ConsultNoise),
	ss_load_files(Files),

	arg(8, CLInfo, CLAsserts),
	ss_cl_asserts(CLAsserts, OutputStream),
	!,
	setup_init_goal(CLInfo, ShellCall),
	user:ShellCall.

start_shell0(_).

	%%%%%%%

setup_init_goal(CLInfo, ShellCall)
	:-
	arg(7, CLInfo, CLShellCall),
	arg(1, CLInfo, CmdLineGoal),
	(CmdLineGoal = true ->	
		ShellCall = CLShellCall
		;
		ShellCall = (CmdLineGoal, CLShellCall)
	).

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

	%% -P: Start application part of command line, pushing on image name:
ss_parse_command_line(['-P' | T], [ImageName | T], CLInfo)
	:-!,
	arg(4,CLInfo,ImageName).

/*
	%% -ind: "Indirect" file: open, read 1 line, process the line, & continue:
ss_parse_command_line(['-ind', File | T], L, CLInfo)
	:-!,
	open(File,read,IS,[]),
	get_line(IS, Line),
	close(IS),
	atomread(Line, IndCmdLine),
	append(IndCmdLine, T, RestCL),
	ss_parse_command_line(RestCL, L, CLInfo).
*/

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
	mangle(7, CLInfo, true),
	ss_parse_command_line(T, L, CLInfo).

	%% -v: Turn on verbose mode:
ss_parse_command_line(['-v' | T], L, CLInfo)
	:-!,
	mangle(2, CLInfo, false),
	ss_parse_command_line(T, L, CLInfo).

	%% -q: Turn off verbose mode: (set = "true": be quiet)
ss_parse_command_line(['-q' | T], L, CLInfo)
	:-!,
	mangle(2, CLInfo, true),
	ss_parse_command_line(T, L, CLInfo).

	%% -s: Atom - File to add to search list;
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
	arg(8, CLInfo, PrevCmdLineAsserts),
	mangle(8, CLInfo, [Expr | PrevCmdLineAsserts]),
	ss_parse_command_line(T, L, CLInfo).

	%% For historical compatability:
	%% -obp: Keep obp files in directory where image is running:
ss_parse_command_line(['-obp' | T], L, CLInfo)
	:-!,
	generated_in_cur,
	ss_parse_command_line(T, L, CLInfo).

	%% "Generated In Current directory:"	
	%% -gic: Keep generated files in directory where image is running:
ss_parse_command_line(['-gic' | T], L, CLInfo)
	:-!,
	generated_in_cur,
	ss_parse_command_line(T, L, CLInfo).

	%% "Generated In Source directory:"	
	%% -gis: Keep generated files in directory where sources reside:
ss_parse_command_line(['-gis' | T], L, CLInfo)
	:-!,
	generated_with_src,
	ss_parse_command_line(T, L, CLInfo).

	%% "Generated In Architecture sub-directory of Sources directory:"	
	%% -gias: Keep generated files in arch subdirectory where sources reside:
ss_parse_command_line(['-gias' | T], L, CLInfo)
	:-!,
	generated_in_arch(src),
	ss_parse_command_line(T, L, CLInfo).

	%% "Generated In Architecture sub-directory of Current directory:"	
	%% -giac: Keep generated files in arch subdirectory of current dir:
ss_parse_command_line(['-giac' | T], L, CLInfo)
	:-!,
	generated_in_arch(cur),
	ss_parse_command_line(T, L, CLInfo).

	%% "Generated In exlicipt Location:"
	%% -gil: Keep generated files in explict Path dir:
ss_parse_command_line(['-gil', Path | T], L, CLInfo)
	:-!,
	generated_in_locn(Path),
	ss_parse_command_line(T, L, CLInfo).


	%% -nwd: Set debugger to "nowins"
ss_parse_command_line(['-nwd' | T], L, CLInfo)
	:-!,
	debugger:nospy,
	(debugger:set_debug_io(nowins),!;true),
	ss_parse_command_line(T, L, CLInfo).

	/* Skip -heap and -stack arguments because they 
       must be handled at the C level. */
ss_parse_command_line(['-heap', _ | T], L, CLInfo)
	:-!,
	ss_parse_command_line(T, L, CLInfo).

ss_parse_command_line(['-stack', _ | T], L, CLInfo)
	:-!,
	ss_parse_command_line(T, L, CLInfo).

	%% Specify non-default shell:
	%% -shell Shell : Use shell (alsdev only known):
ss_parse_command_line(['-shell', ShellName | T], L, CLInfo)
	:-!,
	mangle(7, CLInfo, ShellName),
	ss_parse_command_line(T, L, CLInfo).

	%% Otherwise: should be a file to be loaded:
ss_parse_command_line([File | T], L, CLInfo)
	:-
	arg(3, CLInfo, Files),
	append(Files, [File], NewFiles),
	mangle(3, CLInfo, NewFiles),
	ss_parse_command_line(T, L, CLInfo).



ss_cl_asserts([], OutputStream).
ss_cl_asserts([Expr | CLAsserts], OutputStream)
	:-
	catch( ss_cl_assert(Expr),
			_,
			ss_cl_assert_error(Expr, OutputStream)
		),
	ss_cl_asserts(CLAsserts, OutputStream).


ss_cl_assert(Expr)
	:-
	atomread(Expr, CLAssrt, [syntax_errors(quiet)]),
	ss_cl_assert0(CLAssrt).

ss_cl_assert_error(Expr, OutputStream)
	:-
	als_advise(OutputStream, 'Error reading command-line assert: %s', [Expr]),
	nl(OutputStream),
	flush_output(OutputStream).

ss_cl_assert0((Mod:AX))
	:-
	(modules(Mod,_) -> true ; create_new_module(Mod)),
	ss_cl_assert1(AX, Mod).

ss_cl_assert0(AX)
	:-
	ss_cl_assert1(AX, user).

ss_cl_assert1((AX, BX), Mod)
	:-!,
	ss_cl_assert1(AX, Mod),
	ss_cl_assert1(BX, Mod).

ss_cl_assert1(AX, Mod)
	:-
	Mod:assertz(AX).

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
	catch(	reconsult(F),
	      	Reason,
			shell_exception(Reason)
		),
	ss_load_files(T).
    
/*---------------------------------------
 |	ss_load_dot_alspro
 *--------------------------------------*/
ss_load_dot_alspro
	:-
	als_system(L),
	dmember(os=OS, L),
	(OS = macos -> Files = ['alspro.pro'] ;
		OS = mswin32 -> Files = ['alspro.pro'] ;
			Files = ['alspro.pro','.alspro']
	),
	ss_load_dot_alspros(Files).

ss_load_dot_alspros([]).
ss_load_dot_alspros([File | Files])
	:-
	ss_load_dot_alspro(File),
	!,
	ss_load_dot_alspros(Files).

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
 | print_banner/2
 |		- prints out the initial banner
 *------------------------------------------------*/
print_banner(OutS,L) 
	:-
	system_name(L, Name),
	dmember(os_variation = OSVar, L),
	dmember(prologVersion = Version, L),
	current_prolog_flag(windows_system, WinsName),
	name(WinsName, [InC | WNCs]),
	UInC is InC - 32,
	name(WBan, [UInC | WNCs]),
	!,
#if (syscfg:intconstr)
	printf(OutS,'CLP(BNR)(r) \[%s Version %s \[%s\]\]\n',[Name,Version,OSVar]),
#else
	printf(OutS,'%s Version %s \[%s\]\n',[Name,Version,OSVar]),
#endif
	printf(OutS,'   Copyright (c) 1987-96 Applied Logic Systems, Inc.\n\n',[]).

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
	prolog_shell(user_input,user_output,alspro).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Used to start both the default system shells, and
	%% also shells talking to windows:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prolog_shell(InStream,OutStream,ID) 
	:-
	init_prolog_shell(InStream, OutStream,ID,CurLevel,CurDebuggingState,Wins),
	prolog_shell_loop(Wins,InStream,OutStream),
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
	enable_security,
	gc,
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

	current_prolog_flag(windows_system, Wins),
	(Wins = tcltk -> print_banner(OutStream,SysList) ; true ),
%	dmember(os=OS, SysList),
%	dmember(os_variation=OSMinor, SysList),
	push_prompt(Wins,OutStream,Prompt1).

push_prompt(nowins,_,_) :-!.
push_prompt(tcltk,OutStream,Prompt1)
	:-
	nl(OutStream),
%	put_atom(OutStream,Prompt1),
	flush_output(OutStream),
	tcl_call(shl_tcli, [set_prompt_mark, '.topals.txwin.text'], _).
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
	%% So the organization of prolog_shell_loop/3 simply
	%% reflects the need to support this breakup.  It 
	%% allows us to still have the TTY version loop just
	%% like it did before, but it allows the X version to
	%% cleanly call the part (shell_read_execute/4) which reads 
	%% one term from the input stream and executes it.
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export als_exec/2.

als_exec( InputLine, InStreamAlias, Wins)
	:-
	add_to_stream_buffer(InStreamAlias, InputLine),
		%% get the associated output stream alias:
	associated_output_alias(InStreamAlias, OutStreamAlias),

	shell_read_execute(InStreamAlias,OutStreamAlias,Wins,Status),
	continue_prolog_loop(Status).

prolog_shell_loop(Wins,InStream,OutStream) 
	:-
		%% Read one term and execute it:
	shell_read_execute(InStream,OutStream,Wins,Status),
		%% Decide whether to continue or exit:
	continue_prolog_loop(Status),
	!,
	prolog_shell_loop(Wins,InStream,OutStream).

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
	!,
	sio:get_user_prompt(OldPrompt),
	catch((
		sio:set_user_prompt(Prompt1),
		sio:skip_layout(InStream),
		sio:set_user_prompt(Prompt2),
		read_term(InStream,G,[vars_and_names(V,N)])
	), Exception, (
		sio:set_user_prompt(OldPrompt),
		throw(Exception)
	)),
		
	sio:set_user_prompt(OldPrompt).

shell_read0(Prompt1,Prompt2,InStream,stream_not_ready,[],[]).

	%% This contains the guts of the old shell2, following 
	%% the shell_read:
shell_execute(InStream,OutStream,InitGoal,[],[],Wins,continue)
	:-
	nonvar(InitGoal),
	InitGoal = stream_not_ready,
	!.

module alsshell.
dummy.
:-dynamic(getTracingFlag/1).
endmod.

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
		catch(do_shell_query(Goal,NamesOfVars,Vars,Wins,Alarm,InStream,OutStream),
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
 |	do_shell_query/6 executes the query and displays the answers.  The
 |	layering is necessary since a top level cut may cut away the top
 |	choice point.  A subsequent failure would then force the whole thing
 |	to fail.
 *-----------------------------------------------------------------------*/

do_shell_query(Goal0,VarNames,_,_,_,InStream,OutStream) 
	:-
	var(Goal0),
	VarNames = [VV | _],
	!,
			%% bad_goal: "Improper Goal: %t\n"
	prolog_system_error(bad_goal, [VV]).

do_shell_query(exit,_,_,_,_,_,_) 
	:-!,
	fail.

do_shell_query(end_of_file,_,_,_,_,InStream,_) 
	:-
	flush_input(InStream),      %% reset eof condition
	!,
	fail.

do_shell_query((?- Goal0),VarNames,Vars,Wins,Alarm,InStream,OutStream) 
	:-!,
	do_shell_query(Goal0,VarNames,Vars,Wins,Alarm,InStream,OutStream).

do_shell_query(Goal0,VarNames,Vars,Wins,AlarmIntrv,InStream,OutStream) 
	:-
	gc,			%% Let user start fresh
	getDebugInterrupt(DInt),
	setPrologInterrupt(DInt),
	setCall(0),		%% Start Call,
	setDepth(1),            %%    Depth
	setRetry(0),            %%    and Retry in case of debugging.
%	set_alarm_clock(Goal0,AlarmIntrv),
	xform_command_or_query(Goal0,Goal1),
	do_shell_query2(user,Goal1),
%	unset_alarm(AlarmIntrv),
	dbg_notrace,
	dbg_spyoff,
	showanswers(VarNames,Vars,Wins,InStream,OutStream),
	!.

do_shell_query(Goal,VarNames,Vars,Wins,AlarmIntrv,InStream,OutStream) 
	:-
%	unset_alarm(AlarmIntrv),
	dbg_notrace,
	dbg_spyoff,
	print_no(OutStream).

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

/*
unset_alarm(0) :-!.
unset_alarm(AlarmIntrv)
	:-
	alarm(0,0).
unset_alarm(AlarmIntrv)
	:-
	alarm(AlarmIntrv,AlarmIntrv),
	!,
	fail.
*/
unset_alarm(0) :-!.
unset_alarm(AlarmIntrv)
	:-
	alarm(0,0).

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


showanswers(N,V,Wins,InStream,OutStream) 
	:-
	N = [_|_],
	write_substs(OutStream,N,V,NonAnonNames),
	flush_output(OutStream),
	!,
	flush_input(InStream),
	(NonAnonNames \= [] -> 
		sio:get_user_prompt(UsersPrompt),
		sio:set_user_prompt(''),
		sa_cont(InStream,UsersPrompt,Wins,OutStream)
		;
		print_yes(OutStream)
	).

showanswers(_,_,Wins,InStream,OutStream) 
	:-
	flush_input(InStream),  %% reset eof in event that calling goal set it
	print_yes(OutStream).

sa_cont(InStream,UsersPrompt,Wins,OutStream)
	:-
	get_code(InStream,C,[blocking(true)]),
	disp_sa_cont(C,Wins,InStream,UsersPrompt,OutStream).

disp_sa_cont(-2,tcltk,InStream,UsersPrompt,OutStream)  %% stream not ready
	:-!,
	sio:wait_data(tcltk, InStream, get_code(InStream,C)),
	sio:set_user_prompt(UsersPrompt),
	flush_input(InStream),
	succeed_or_fail(C,OutStream).

disp_sa_cont(C,Wins,InStream,UsersPrompt,OutStream)
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


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% General library setup (called when shell
	%% starts up; blt_lib.pro is (to be) no 
	%% longer loaded as part of builtins:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export setup_libraries/0.
setup_libraries
	:-
	sys_searchdir(ALSDIRPath),
	pathPlusFile(ALSDIRPath, '*', ALSDIRPattern),
	directory(ALSDIRPattern,1,SubdirList0),
	list_delete(SubdirList0, '.', SubdirList1),
	list_delete(SubdirList1, '..', SubdirList2),
	list_delete(SubdirList2, builtins, SubdirList),
	setup_local_libraries(SubdirList, ALSDIRPath),

	(getenv('ALS_LIBS', EnvLibsString) ->
		atom_codes(EnvLibsString, ELSCs),
		asplit0_all(ELSCs, 0',, EnvLibsStrings),
		all_to_atoms(EnvLibsStrings, EnvLibsList),
		setup_remote_libraries(EnvLibsList)
		;
		true
	).

setup_local_libraries([], _).
setup_local_libraries([Lib | LibsList], DirPath)
	:-
	pathPlusFile(DirPath, Lib, LibPath),
	setup_lib(LibPath),
	setup_local_libraries(LibsList, DirPath).

setup_remote_libraries([]).
setup_remote_libraries([LibPath | LibsList])
	:-
	setup_lib(LibPath),
	setup_remote_libraries(LibsList).

lib_extension(alb).

:- dynamic(als_lib_lcn/1).

export setup_lib/1.
setup_lib(LibPath)
	:-
	exists_file(LibPath),
	!,
	pathPlusFile(PathHead,LibDirName,LibPath),
	disp_setup_lib(LibDirName,LibPath,PathHead).

disp_setup_lib(library,LibPath,PathHead)
	:-
	als_lib_lcn(_),
	!.

disp_setup_lib(LibDirName,LibPath,PathHead)
	:-
	lib_extension(LibExt),
	filePlusExt('*',LibExt,Pattern),
	files(LibPath, Pattern, LibFileHeaders),
	install_lib_files(LibFileHeaders, LibPath),
	(LibDirName = library ->
		assert(als_lib_lcn(PathHead)) 
		; 
		true
	).

setup_lib(LibPath)
	:-
	write(setup_lib_file_bad_path=LibPath),nl, flush_output,!,
	prolog_system_warning(lib_pth, [LibPath] ).

install_lib_files([], LibPath).
install_lib_files([LibFileHd | LibFileHeaders], LibPath)
	:-
	install_lib_f(LibFileHd, LibPath),
	install_lib_files(LibFileHeaders, LibPath).

install_lib_f(LibFileHd, LibDirPath)
	:-
	pathPlusFile(LibDirPath, LibFileHd, HeaderFile),
	open(HeaderFile, read, IS, []),
	read(IS, LHTerm0),
	close(IS),
	(LHTerm0 = (:- LHTerm) ->
		call(builtins:LHTerm)
		;
		call(builtins:LHTerm0)
	).

/*!---------------------------------------------------------------------
 |	asplit0/4
 |	asplit0(AtomCs,Splitter,LeftPartCs,RightPartCs) 
 |	asplit0(+,+,-,-) 
 |
 |	- divides a list of character codes as determined by a character code
 |
 |	If AtomCs is a list of character codes, and if Splitter is the character 
 |	code of of a character, then, if the character with code Splitter occurs in
 |	AtomCs, LeftPart is the list consisting of that part of AtomCs from the
 |	left up to and including the leftmost occurrence of Splitter,
 |	and RightPart is the atom consisting of that part of AtomCs extending 
 |	from immediately after the end of LeftPart to the end of AtomCs.
 *!--------------------------------------------------------------------*/
export asplit0/4.
asplit0([Char|Rest],Splitter,[Char|R1],String2) 
	:-
	Char \= Splitter,!,
	asplit0(Rest,Splitter,R1,String2).

asplit0([Splitter|Rest],Splitter,[],Rest).

asplit0_all(Chars, Splitter, [Head | List])
	:-
	asplit0(Chars, Splitter, Head, Tail),
	!,
	asplit0_all(Tail, Splitter, List).

asplit0_all(Chars, Splitter, [Chars]).

all_to_atoms([], []).
all_to_atoms([String | Strings], [Atom | Atoms])
	:-
	atom_codes(Atom, String),
	all_to_atoms(Strings, Atoms).

endmod.		%% blt_shl.pro: Development shell

