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

:- defineClass(alsshell,
	[   name=shl_source_handler,
		subClassOf=source_handler,
		export = yes,
		addl_slots= [ ]
	]).

start_shell(DefaultShellCall) 
	:-
	catch(start_shell0(builtins:prolog_shell), 
			E,
			(shell_exception(E), throw(E))
		).

:- dynamic(genFileLocn/3).

start_shell0(DefaultShellCall)
	:-
	init_als_shl_mgr,
	make_clinfo(CLInfo, DefaultShellCall, false),	% verbosity = verbose
	get_command_line_info(DefaultShellCall,CommandLine,ResidualCommandLine,alsshell,CLInfo),

	assertz(command_line(ResidualCommandLine)),
	setup_search_dirs(CLInfo),
	assert(save_clinfo(CLInfo)),
	output_system_banner(CLInfo),

	sys_searchdir(ALSDIRPath),
	split_path(ALSDIRPath, ALSDIRPathList),
	dappend(ALSDIRPathList, [library,'objects.pro'], ObjPathList),
	join_path(ObjPathList, ObjectsPath),

	library_setup,
	load_cl_files(CLInfo),
	process_cl_asserts(CLInfo),
	!,
		%% 
	ss_load_dot_alspro(CLInfo),
	setup_init_goal(CLInfo, ShellCall),
	user:ShellCall.

start_shell0(_).

init_als_shl_mgr
	:-
	clause(get_primary_manager(_), _),
	!,
	get_primary_manager(MM),
	complete_init_als_shl_mgr(MM).

init_als_shl_mgr
	:-
	setup_als_shl_mgr(Mgr),
	make_gv('_primary_manager'),
	set_primary_manager(Mgr).

complete_init_als_shl_mgr(0)
	:-!,
	setup_als_shl_mgr(Mgr),
	set_primary_manager(Mgr).
complete_init_als_shl_mgr(_).

setup_als_shl_mgr(MgrObject)
	:-
		%% Get values for initial values in create_object:
	get_cwd(InitialDir),

	builtins:sys_searchdir(SSD),
	split_path(SSD,SSDL),
	append(SSDL, [library], LLL),
	join_path(LLL, LibPath),

	findall(searchdir(SD), (builtins:searchdir(SD)), SDList),

	create_object(
		[instanceOf=als_shl_mgr,
		 handle=true,
		 values = [
			initial_dir = InitialDir,
			prolog_library = LibPath,
			initial_search_dirs = SDList
		 ]
		], 
		MgrObject).

:- init_als_shl_mgr.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% "Early" shell setup support routines
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup_debugger_stubs
	:-
	%% Setup debugger entries (needs to be done early because
	%% ss_parse_command_line can cause debugger to be called
	%% [if it sees switch -nwd] )
	libhide(debugger, [builtins,debugger],
				[ (trace)/0, (trace)/2, toggle_mod_show/1, leash/1,
		  		  (spy)/0, (spy)/1, (spy)/2, (spy_pat)/3, (spyWhen)/1,
		  		  (spyWhen)/2, (nospy)/0, (nospy)/1, (nospy)/3,
		  		  spying/0, debugging/0, list_spypoints/0,set_debug_io/1 ]).

make_clinfo( CLInfo, DefaultShellCall, Verbosity)
	:-
	CLInfo = clinfo(true,				/* -g: goal to run */
					Verbosity,			/* -v/-q: verbosity */
					[],					/* files to consult */
					ImageName,
					default,			/* -nwd: debugger to set up */
					[],					/* -s init search list */
					DefaultShellCall,	/* shell/or not */
					[], 				/* Command-line asserts */
					true				/* Load .alspro: true = do it */
					). 				


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
ss_load_dot_alspro(CLInfo)
	:-
	arg(9, CLInfo, false),
	!.

ss_load_dot_alspro(CLInfo)
	:-
	arg(2, CLInfo, Verbosity),
	als_system(L),
	dmember(os=OS, L),
	(OS = macos -> Files = ['alspro.pro'] ;
		OS = mswin32 -> Files = ['alspro.pro'] ;
			Files = ['alspro.pro','.alspro']
	),
	ss_load_dot_alspros(Files, Verbosity).

ss_load_dot_alspros([], _).
ss_load_dot_alspros([File | Files], Verbosity)
	:-
	ss_load_the_dot_alspro(File, Verbosity),
	!,
	ss_load_dot_alspros(Files, Verbosity).

ss_load_the_dot_alspro(AutoFile, Verbosity)
	:-
	exists_file(AutoFile),
	!,
	consult(AutoFile, [consult(false),quiet(Verbosity)]).

ss_load_the_dot_alspro(AutoFile, Verbosity)
	:-
		%% What about DOS (also Mac, etc.) here?:
	getenv('HOME',HOME),
	pathPlusFile(HOME,AutoFile,File),
	exists_file(File),
	!,
	consult(File, [consult(false),quiet(Verbosity)]).

ss_load_the_dot_alspro(_, _).

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
	printf(OutS,'   Copyright (c) 1987-98 Applied Logic Systems, Inc.\n\n',[]),
	flush_output(OutS).

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
	push_prompt(Wins,OutStream,Prompt1).

push_prompt(nowins,_,_) :-!.

shell_exit(InStream, OutStream,Level,DebuggingState)
	:-
write(shell_exit-Level),nl,flush_output,
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

%prolog_shell_loop(_,_,_).
prolog_shell_loop(Wins,InStream,OutStream) 
	:-
write(prolog_shell_loop_2),nl,flush_output.

export shell_read_execute/4.
shell_read_execute(InStream,OutStream,Wins,Status)
	:-
	shell_read(InStream,OutStream,InitGoal,NamesOfVars,Vars),
	shell_execute(InStream,OutStream,InitGoal,NamesOfVars,Vars,Wins,Status),
	!.

continue_prolog_loop(halt) 
	:-!, 
	halt.
continue_prolog_loop(exit) 
	:-!, 
	fail.
continue_prolog_loop(end_of_file) 
	:-!, 
	fail.
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
		  Ball,
		  shell_read0_err_disp(Ball,InStream,OutStream,G,N,V)
		  ).

shell_read0(Prompt1,Prompt2,InStream,G,N,V) 
	:- !,
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

shell_read0_err_disp(Ball,InStream,OutStream,G,N,V)
	:-
	functor(Ball,error,_),
	arg(1,Ball,syntax_error),
	arg(2,Ball,[_, SyntaxErrInfo]),
	prolog_system_error(SyntaxErrInfo,[]),
	nl(OutStream),
	flush_output(OutStream),
	shell_read(InStream,OutStream,G,N,V).

shell_read0_err_disp(Ball,InStream,OutStream,G,N,V)
	:-
	write(OutStream,' <input discarded>'),
	nl(OutStream),
	flush_output(OutStream),
	shell_read(InStream,OutStream,G,N,V).

shell_execute(InStream,OutStream,InitGoal,[],[],Wins,continue)
	:-
	nonvar(InitGoal),
	InitGoal = stream_not_ready,
	!.

shell_execute(InStream,OutStream,InitGoal,NamesOfVars,Vars,Wins,Status)
	:-
	sio:get_user_prompt(UsersPrompt),
	Goal = InitGoal,
	flush_input(InStream),
	sio:set_user_prompt(UsersPrompt),
	!,
	((nonvar(Goal), special_shell_goals(Goal)) -> 
		Status = Goal 
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

special_shell_goals(halt).
special_shell_goals(exit).
special_shell_goals(end_of_file).

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

make_shell_prompts(alsdev, 1,'?- ','?-_').

make_shell_prompts(alsdev, N,Prompt1,Prompt2)
	:-
	N > 1,
	!,
	sprintf(PL1,'Break (%d) ?- ',[N]),
	sprintf(PL2,'          ?-_',[]),
	name(Prompt1,PL1),
	name(Prompt2,PL2).

:-user:dynamic(make_shell_prompts/4).
make_prompts(ID, N,Prompt1,Prompt2) 
	:-
	(user:make_shell_prompts(ID, N, Prompt1, Prompt2), ! ;
		make_shell_prompts(ID, N, Prompt1, Prompt2)
	),
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

/*
do_shell_query(exit,_,_,_,_,_,_) 
	:-!,
	fail.

do_shell_query(end_of_file,_,_,_,_,InStream,_) 
	:-
	flush_input(InStream),      %% reset eof condition
	!,
	fail.
do_shell_query(end_of_file,_,_,_,_,InStream,_) 
	:-
	flush_input(InStream),      %% reset eof condition
	!.
	fail.
*/

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
	xform_command_or_query(Goal0,Goal1),
	do_shell_query2(user,Goal1),
	dbg_notrace,
	dbg_spyoff,
	showanswers(VarNames,Vars,Wins,InStream,OutStream),
	!.

do_shell_query(Goal,VarNames,Vars,Wins,AlarmIntrv,InStream,OutStream) 
	:-
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

disp_sa_cont(C,Wins,InStream,UsersPrompt,OutStream)
	:-
	sio:set_user_prompt(UsersPrompt),
	flush_input(InStream),
	succeed_or_fail(C,OutStream).

print_no(OutStream) 
	:- 
	nl(OutStream),
	write(OutStream,'no.'),
	nl(OutStream),
	flush_output(OutStream).

print_yes(OutStream) 
	:-
	nl(OutStream),
	write(OutStream,'yes.'),
	nl(OutStream),
	flush_output(OutStream).

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

endmod.  %% builtins

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%  ALSSHELL COMPONENTS
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

module alsshell.

	%%------------------------------------------
	%% Command line processing
	%%------------------------------------------

special_ss_parse_command_line(File, Tail, Tail, CLInfo)
	:-
	arg(3, CLInfo, Files),
	append(Files, [File], NewFiles),
	mangle(3, CLInfo, NewFiles).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% ERROR OUTPUT PREDICATES
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- dynamic(shl_source_handlerAction/2).

shl_source_handlerAction(display_file_errors(NErrs, SPath, ErrsList), State)
	:-
	display_file_errors(SPath, ErrsList, FileMgr).

display_file_errors(SPath, ErrsList, FileMgr)
	:-
	printf(error_stream, '\nFile %t contained errors:\n', [SPath]),
	spit_errors(ErrsList, error_stream),
	nl(error_stream).

spit_errors([], _).
spit_errors([Error | ErrsList], Stream)
	:-
	spit_the_error(Error, Stream),
	spit_errors(ErrsList, Stream).

spit_the_error( 
	error(syntax_error, [_,syntax(Context,P1,P2,P3,ErrorMessage,LineNumber,FS)]),
	Stream)
	:-!,
	EType = ' Syntax error',
	printf(error_stream,'\n%s',[Context]),
	OutPattern = '\n   - line %d: %s\n',
	OutArgs = [LineNumber,ErrorMessage],
	printf(Stream, '%t',[EType]),
	printf(Stream, OutPattern, OutArgs, [quoted(true), maxdepth(6)]),
	flush_output(Stream).

spit_the_error(error(Error, Args), Stream)
	:-
	prolog_system_error(Error, Args).

spit_the_error(prolog_system_error(Error, Args), Stream)
	:-
	prolog_system_error(Error, Args).

export tty_prolog_system_error/2.

	%% New rt_ reader:
tty_prolog_system_error(
	error(syntax_error,[_,syntax(Context,P1,P2,P3,ErrorMessage,LineNumber,Stream)]), [])
	:-
	tty_prolog_system_error(syntax(Context,P1,P2,P3,ErrorMessage,LineNumber,Stream), []).

tty_prolog_system_error(syntax(Context,P1,P2,P3,ErrorMessage,LineNumber,Stream), _) 
	:-
%write(syntax_pos_info=p(P1,P2,P3)),nl,flush_output,
	sio:is_stream(Stream,Stream0),
	sio:is_input_stream(Stream0),
	!,
	EType = 'Syntax error',
	sio:stream_type(Stream0,StreamType),
	sio:stream_name(Stream0,StreamName),
	(StreamType = file ->
		pathPlusFile(_,File,StreamName)
		;	
		File = StreamName
	),
	printf(error_stream,'\n%s',[Context]),
	OutPattern = '%t, line %d: %s\n',
	OutArgs = [File,LineNumber,ErrorMessage],
	pse_out(error_stream, EType, OutPattern, OutArgs),
	flush_output(error_stream).

tty_prolog_system_error(s(ErrorCode,Stream), Args) 
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

tty_prolog_system_error(qc_failed(ErrorCode,Name,LineNumber),Args) 
	:-
	expand_code(ErrorCode, Pattern, EType),
	!,
	catenate('%t,line %t: ', Pattern, OutPattern),
	OutArgs = [Name,LineNumber | Args],
	pse_out(error_stream, EType, OutPattern, OutArgs),
	flush_output(error_stream).

tty_prolog_system_error(error(W,L),_) 
	:-
	decode_error(W, L, Pattern, Args),
	!,
	pse_out(error_stream, 'Error: ', Pattern, Args),
	print_error_goal_attributes(L),
	printf(error_stream,'- %-14s %t\n',
		['Throw pattern:',error(W,L)],
		[quoted(true),maxdepth(4),indent(17)]),
	flush_output(error_stream).
	
tty_prolog_system_error(ErrorCode, Args) 
	:-
	expand_code(ErrorCode, Pattern, EType),
	pse_out(error_stream, EType, Pattern, Args),
	flush_output(error_stream).

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

export tty_prolog_system_warning/2.

tty_prolog_system_warning(error(W,L),_) 
	:-
	decode_error(W, L, Pattern, Args),
	!,
	pse_out(error_stream, 'Warning: ', Pattern, Args),
	print_error_goal_attributes(L),
	flush_output(error_stream).

tty_prolog_system_warning(ErrorCode, Args) 
	:-
	expand_code(ErrorCode, Pattern, EType),
	printf(warning_output, '%t',[EType]),
	printf(warning_output, Pattern, Args, [quoted(true), maxdepth(9)]),
	flush_output(warning_output).





endmod.		%% blt_shl.pro: Development shell

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%% TTY vs IDE SHELL HOOKS
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
module debugger.

:- abolish(ensure_db_showing, 0).
ensure_db_showing
	:-
	pbi_write(ensure_db_showing_called),pbi_nl,pbi_ttyflush.

endmod.

module builtins.

:- abolish(record_lib_load, 1).
record_lib_load(Desc)
	:-
		%% the incoming Desc is an atom, looking like: Dir/File
	sub_atom(Desc, Before, 1, After, '/'),
	sub_atom(Desc, 0, Before, _, Dir),
	sub_atom(Desc, _, After, 0, File),
	assertz(loaded_builtins_file(File,Dir)).

endmod.

