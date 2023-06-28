/*================================================================*
 | 		debugger.pro  
 |	Copyright (c) 1988-97 Applied Logic Systems, Inc.
 |
 |	Non-interpretive debugger for Prolog
 |		- separates debugger core from I/O, and
 |		- includes TTY 
 |		- windowed I/O in vdebug.pro [in windows/alsdev]
 |
 | Authors:  Kevin Buettner, Keith Hughes, Ken Bowen
 | Creation:  02/16/88,11/88,5/89
 | Uses the new interrupt facilities
 |
 | Things yet to be done:
 |	* retry command with call number permitted -- this will involve
 |		changing the menu package
 |	* set print depth with depth on same line (no prompting)
 |	Revised window-based version: Begun March, 1992 - Ken Bowen
 |	Revisions: Fall, 1993 Kevin Buettner
 |	Revisions: March, 1994 Ken Bowen - Merge TTY & windowed versions
 *================================================================*/

module debugger. 		%% debugger segment
use windows.
use alsshell.

%use objects.

/*-------------------------------------------------------------------------*
 |	trace/0, trace/1, and trace/2.
 |
 |	trace is called when the user wishes to start the debugger.
 |	trace(Goal) or trace(Module,Goal) are called on specific goals.
 |	
 |	trace/1 actually ends up calling trace/2 due to the module closure
 |	created in blt_sys.pro.
 |
 *-------------------------------------------------------------------------*/
export trace/0.
export trace/1.
export trace/2.
	
'trace' :-
	(builtins:alsdev_running -> tcl_call(shl_tcli, [ensure_db_showing], _) ; true),
	check_debug_io,
	dbg_notrace,
	setPrologInterrupt(debug_user),
	setDebugInterrupt(debug_user),
	forcePrologInterrupt.

trace(Module,Goal) :-
	check_debug_io,
	setup_debug(Module, Goal),
	getPrologInterrupt(OldMagic),
	catch(trace0(Module,Goal), Reason, 
	      (dbg_notrace,setPrologInterrupt(OldMagic),throw(Reason))).

trace0(Module,Goal) :-
	getPrologInterrupt(OldMagic),
	initCallDepth,
	setPrologInterrupt(debug_user),
	dogoal(debug_user,Module,Goal),
	dbg_notrace,
	(setPrologInterrupt(OldMagic);
		setPrologInterrupt(debug_user),fail).
trace0(Module,Goal) :-
	dbg_notrace,
	setPrologInterrupt(skipping),
	fail.

/*-------------------------------------------------------------------------*
 | dbg_notrace is used to stop tracing.  It is detected by the debugger
 | explicitly.
 |
 | Note: dbg_notrace used to be defined here.  But it is needed by
 | prolog_shell/0 (found in blt_sys.pro).  It is defined as:
 |
 |	export dbg_notrace/0.
 |	dbg_notrace.
 *-------------------------------------------------------------------------*/

/*-------------------------------------------------------------------------*
 | setCall/1, getCall/1, setDepth/1, and getDepth/1 are access predicates
 | for accessing the Call and Depth global variables.  These are built
 | at load time by make_gv (defined in builtins.pro).  All of these
 | predicates take a single argument -- the value to either get or set.
 | The calls for creating these variables occur in blt_sys.pro
 |
 | setRetry/1 and getRetry/1 set and get the contents of the Retry variable.
 | Under normal circumstances, the value of this variable is 0.  When we are
 | retrying a goal, the Retry value is set to the call (box) number of the
 | goal that we wish to retry.  Failure is then forced (with no printing of
 | ports) until a fail port is found with call number less than or equal to
 | the Retry value.  At this point, the call may be easily restarted.  See
 | retryOrFail/6, and showGoal/6 for more details.
 |
 | Note: The above described access predicates used to be created here,
 | but have been moved to blt_sys.pro so that the Prolog shell may be
 | more intelligent about setting and resetting them.
 *-------------------------------------------------------------------------*/

/*-------------------------------------------------------------------------*
 | incDepth increments the Depth variable.
 *-------------------------------------------------------------------------*/

incDepth :-
    getDepth(Depth),
    NewDepth is Depth + 1,
    setDepth(NewDepth).

/*-------------------------------------------------------------------------*
 | incCall increments the call and returns the new call number
 *-------------------------------------------------------------------------*/
    
incCall(NewCall) :-
    getCall(Call),
    NewCall is Call + 1,
    setCall(NewCall).

/*-------------------------------------------------------------------------*
 | initCallDepth is called to initialize the call and depth global
 | variables.
 *-------------------------------------------------------------------------*/

initCallDepth :-
    setCall(0),
    setDepth(1).

/*-------------------------------------------------------------------------*
 | '$int'/3:	Interrupt Handler clauses.
 |
 |		We must concern ourselves with debug_init, debug_user,
 |		debug_noshow, and spying.
 |
 |
 |		'$int'(Magic, Module, Goal)
 |
 |		Magic will be one of debug_init, debug_user, debug_noshow,
 |		or spying.  In past versions of the debugger, the magic value
 |		was often a structure with the box number and depth as
 |		arguments. The problem with this approach was that it was
 |		clumsy to modify any one of the three components (the functor
 |		or either of its two arguments).  In addition, the global
 |		variable mechanism is less efficient when dealing with
 |		structures.  Since it is forced to make the heap safe for
 |		backtracking, the entire choice point structure had to be
 |		traversed.  A good deal of garbage would also be unnecessarily
 |		created whenever a global variable would be set (to a
 |		structure) thus forcing the garbage collector to run much
 |		more often.  With these considerations in mind, the
 |		design evolved so that these three components (interrupt tag,
 |		call number, and current depth) were separated. Now access to
 |		any of the three components is simplified and in addition we
 |		can set either of these three components in constant time since
 |		only constants are assigned to them.  These considerations
 |		should be kept in mind when it becomes tempting in the future
 |		to hack some feature into the system by making any one of these
 |		components an object which lives on the heap.
 |
 |		As alluded to in the above discussion, Magic is really an 
 |		interrupt tag.  It tells us when an interrupt occurs what
 |		piece of code should be executed.  
 |
 |		The "debug_init" tag indicates that tracing has been turned on.  
 |		The clauses and other code for '$int'/3 which deal with this tag
 |		maintain some additional choice points to make sure that we
 |		exit and fail properly back to code which isn't being traced.
 |
 |		The "debug_user" tag is the tag that will be normally set when
 |		creeping through user code.
 |
 |		The "debug_noshow" tag on the other hand indicates that we are creeping
 |		through a builtin or some other piece of code for which the
 |		details should not be visible.  When such a tag is encountered,
 |		execution will either continue on with the debug_noshow tag set
 |		or it will change to debug_user when a goal is encountered which
 |		is a user goal.
 |
 |		The "spying" tag indicates that the interrupt mechanism is not
 |		enabled so that the next goal is interrupted (at least not
 |		intentionally), but that we should run the code as normal until
 |		a procedure is run upon which we are spying.  At such a point
 |		an interrupt is generated by virtue of the fact that the
 |		procedure table entry has been modified.
 |
 |		When any one of these tags is encountered and the user decides
 |		to creep for a while, the Magic value will be set to either
 |		debug_noshow or debug_user depending upon whether or not
 |		the goal is a user goal or not.
 |
 |		The other two arguments to '$int'/3 are the module and the goal
 |		which were interrupted and which presumably we wish to trace.
 |
 |
 *-------------------------------------------------------------------------*/

module builtins.

	%% Testing:
%'$int'(What,Module,Goal) :- 
%	pbi_write('$int'(What,Module,Goal)),pbi_nl, pbi_ttyflush, fail.

%% === initialization of the debugger
'$int'(debug_init,Module,Goal) :-
	dbg_spyoff,
	debugger:debug_init(Module,Goal).

%% === debug_off indicates that debugging has been disabled
'$int'(debug_off,_,dbg_notrace) :-
	!.
'$int'(debug_off,Module,Goal) :-
	!,
	dbg_call(Module,Goal).


%% === debug_user is the normal interrupt when stepping through user goals
'$int'(debug_user,Module,dbg_notrace) :- 
	!.
'$int'(debug_user,Module,Goal) :-
	!,
	dbg_spyoff,
	debugger:dogoal(debug_user,Module,Goal).

%% === debug_noshow is the interrupt encountered when going through 
%%      builtins and other things that we don't want to show the gritty
%%      grimy details of
'$int'(debug_noshow,Module,dbg_notrace) :-
	debugger:disable_colon_interrupt,
	!.

'$int'(debug_noshow,builtins,Goal) :-
	functor(Goal,'$colon',3),
	!,
	dbg_spyoff,		
	%% FIXME: With dbg_spyoff here, we may be able to
	%% delete the following line
	debugger:disable_colon_interrupt,
	callWithDelayedInterrupt(builtins,Goal).

	%% For 'not':
'$int'(debug_noshow,builtins,Goal) :-
	functor(Goal,call2,2),
	!,
	dbg_spyoff,		
	%% FIXME: With dbg_spyoff here, we may be able to
	%% delete the following line
	debugger:disable_colon_interrupt,
	callWithDelayedInterrupt(builtins,Goal).

/*-------------------------------------------------------------------------*
'$int'(debug_noshow,objects,Goal) :-
	functor(Goal,'$colon',3),
	!,
	debugger:disable_colon_interrupt,
	callWithDelayedInterrupt(objects,Goal).
*/

'$int'(debug_noshow,Module,Goal) :-
	!,
	dbg_spyoff,
	debugger:dogoal(debug_noshow,Module,Goal).

%% === Leaping (spying) interrupts:
'$int'(spying,_,dbg_notrace) :- 
	!.
'$int'(spying,Module,Goal) :-
	debugger:spying_on(Goal,_),
	!,
	dbg_spyoff,
	setPrologInterrupt(debug_user),
debugger:ensure_db_showing,
	debugger:dogoal(spying,Module,Goal).

'$int'(spying,Module,Goal) :-
	!,
	dbg_call(Module,Goal).

%% === Jumping interrupts:
'$int'(jumping,_,dbg_notrace) :- 
	!.
'$int'(jumping,Module,Goal) :-
	debugger:spying_on(Goal,_),
	!,
	dbg_spyoff,
	setPrologInterrupt(debug_user),
debugger:ensure_db_showing,
	debugger:dogoal(jumping,Module,Goal).
'$int'(jumping,Module,Goal) :-
	!,
	dbg_call(Module,Goal).

%% === Skipping interrupts
'$int'(skipping,_,dbg_notrace) :-
	!.
'$int'(skipping,Module,Goal) :-
	!,
	dbg_call(Module,Goal).

endmod. %% builtins

/*-------------------------------------------------------------------------*
 |debug_init/2
 |
 | debug_init(Module,Goal)
 |
 | Handles the initial interrupt which leads us into tracing.  Note
 | that this debug_init is called for each top level goal.  Also note
 | that the call numbers are not reset.  The trace goal has to be
 | run to reset the call numbers.
 |
 *-------------------------------------------------------------------------*/
    
%% === The following are the stoppers.
is_stopper(Module, showanswers(_,_)).
is_stopper(Module, print_no).

%% === See if we got a dbg_notrace and if so shut the tracing off.
debug_init(Module, dbg_notrace)
	:-!,
    setPrologInterrupt(skipping).	%% set Magic back to its default value
debug_init(Module, Goal)
	:-
    is_stopper(Module,Goal),
    !,
    callWithDelayedInterrupt(Module,Goal). %% call the Goal but leave ouch on
debug_init(Module, Goal)
	:-
    dogoal(debug_init,Module,Goal).

/*-------------------------------------------------------------------------*
 | dogoal/3
 |
 | Get and update call number and depth and call dogoal/5
 |
 *-------------------------------------------------------------------------*/
    
dogoal(Magic,Module,Goal) 
	:-
    incCall(CallNum),
    getDepth(Depth),
    incDepth,
    strip_colon(Module,Goal,SModule,SGoal),
    dogoal(CallNum,Depth,Magic,SModule,SGoal).

strip_colon(M,G,M,G) :- 
	var(G),
	!.
strip_colon(_,(M1:G1),M2,G2) :-
	!,
	strip_colon(M1,G1,M2,G2).
strip_colon(_,:(M1,G1,_),M2,G2) :-		%% What about the cutpt??
	!,
	strip_colon(M1,G1,M2,G2).
strip_colon(_,'$colon'(M1,G1,_),M2,G2) :-	%% What about the cutpt??
	!,
	strip_colon(M1,G1,M2,G2).
strip_colon(M,G,M,G).

/*-------------------------------------------------------------------------*
 | dogoal/5
 |
 | dogoal(CallNumber,Depth,Magic,Module,Goal)
 |
 |		-- The main workhorse of tracing
 |
 | The second clause of the procedure handles the Call port of the trace.
 | This is the first time that we've seen this particular goal with the
 | given call number.
 |
 | The third clause is responsible for handling failure of the goal.  It
 | prints out the fact that we've failed and propogates failure back
 | by failing itself.
 *-------------------------------------------------------------------------*/

%% === Handle the Call port
dogoal(Box,Depth,EMagic,Module,Goal)
	:-
			%% Display the call port:
    showGoal(Box,Depth,call,Module,Goal,UserCommand),
			%% And do the call:
    execute(UserCommand,Module,Goal),
			%% Don't want to trace after coming out here --
			%%   that would show parts of the debugger code as
			%%   if it were part of the user's program!
			%% We might even sneak in at this point through a 
			%%   choice point.
    dbg_notrace,
			%% Don't want to interrupt on $colon
    disable_colon_interrupt,
			%% Set the old depth value back again:
    setDepth(Depth),
			%% get magic value to use when redoing:
    getPrologInterrupt(RMagic),
			%% Display the exit and redo ports:
    exitOrRedo(Box,Depth,EMagic,RMagic,Module,Goal).

%% === Goal has failed.
dogoal(Box,Depth,Magic,Module,Goal) :-
			%% Don't want to display debugger code:
    dbg_notrace,
    disable_colon_interrupt,
			%% Display fail port:
    showGoal(Box,Depth,fail,Module,Goal,FailCommand),
			%% Reset Depth and Magic
    setDepth(Depth),
    retryOrFail(FailCommand,Box,Depth,Magic,Module,Goal).

/*-----------------------------------------------------------------------
		Primary control flow subprocedures for dogoal:
 *----------------------------------------------------------------------*/

/*-------------------------------------------------------------------------*
 |	execute/3
 |	execute(UserCommand,Module,Goal) 
 |
 |	Depending on what the user wants, we either go ahead and trace 
 |	the goal by making sure the interrupt will fire the next time, 
 |	or we just call the goal and watch what happens.
 |
 |	Note that we need to interpret '$dbg_aph' and '$dbg_apg' because of
 |	the need to pass these hidden goals to showGoal, where the distinction
 |	is made between the TTY environment (info is unused; nothing shown),
 |	and the windowed environment (where it can be used for source trace).
 *-------------------------------------------------------------------------*/
execute(_,_,'$dbg_aph'(_,_,_)) :-!.
execute(_,_,'$dbg_apg'(_,_,_)) :-!.
execute(_,_,'$dbg_aphe'(_,_,_)) :-!.
execute(_,_,'$dbg_apge'(_,_,_)) :-!.
execute(_,_,'$dbg_apf'(_,_,_)) :-!.

%% === User said to trace the goal:
execute(debug,Module,Goal) :- !,
		%% Take the goal apart in order to decide how to handle it.
    functor(Goal,PredName,Arity),
		%% Figure out what module the thing should execute in.
    builtins:'$resolve_module'(Module,PredName,Arity,ResMod),
		%% execute_debug will decide what should be done.
    execute_debug(ResMod,PredName,Arity,Module,Goal).

%% ===  User said to execute Goal without trace:
execute(nodebug,Module,Goal) :- !,
    setPrologInterrupt(debug_off),
    dbg_call(Module,Goal).	

%% ===  User said to skip Goal (execute Goal with no spying):
execute(skip,Module,Goal) :- !,
	setPrologInterrupt(skipping),
		%% Call Goal without doing exception check.
	dbg_call(Module,Goal).

%% ===  User said to jump (execute Goal with spying on, but stop on
%%					next port of call).
execute(jump,Module,Goal) :-
	setPrologInterrupt(jumping),
	dbg_spyon,
	dbg_call(Module,Goal).

%% ===  User said to leap Goal (execute Goal with spying on):
execute(leap,Module,Goal) :- !,
		%% Enable spying:
	setPrologInterrupt(spying),
	dbg_spyon,
		%% Call Goal in the specified module without doing exception 
		%%   check
	dbg_call(Module,Goal).

%% ===  Fail the goal without attempting to execute it.
execute(fail,Module,Goal) :- !,
	setPrologInterrupt(debug_user),
	dbg_spyon,
	callWithDelayedInterrupt(builtins,fail).

/*-------------------------------------------------------------------------*
 | execute_debug/6 is called when we wish to trace the goal.  But we
 | must decide exactly how to proceed since it is not desirable to
 | see the guts of builtins.
 *-------------------------------------------------------------------------*/
    
%% === See if we have a "special" builtin -- hack for call/1 problem case:
execute_debug(ResMod,call,2,Module,call(SrcGoal,NN)) :-
	!,
	setPrologInterrupt(debug_user),
	callWithDelayedInterrupt(Module,SrcGoal).

%% === See if we have a "special" builtin such as setof or findall
execute_debug(ResMod,PredName,Arity,Module,Goal) :-
    noshow_special(ResMod,PredName,Arity),
    !,
    setPrologInterrupt(debug_noshow),
    enable_colon_interrupt,
    dbg_spyon,
    dbg_call(Module,Goal).

%% === See if we have an ordinary mundane builtin.
execute_debug(ResMod,PredName,Arity,Module,Goal) :-
	( noshow(ResMod,Goal),! ; noshow(Module,Goal) ),
    !,
    setPrologInterrupt(skipping),
    dbg_spyon,
    dbg_call(Module,Goal).
    
%% === Not a builtin -- It is something that we really want to trace.
execute_debug(_,_,_,Module,Goal) :-
    setPrologInterrupt(debug_user),
    callWithDelayedInterrupt(Module,Goal).

/*-------------------------------------------------------------------------*
 | continue/1
 | continue(UserCommand)
 |
 | continue/1 decides how the compuation will continue after a 
 | success.  If the debugger is to continue running, then we must
 | restart the debugger for the (real) continuation pointer. If 
 | not, we just want to continue  without resetting things up.
 *-------------------------------------------------------------------------*/

%% === Still want the debugger:
continue(debug) :-
	!,
	continue_debug.

%% === Don't want the debugger to continue:
continue(nodebug) :- 
	!,
	setPrologInterrupt(debug_off).

%% === Leaping
continue(leap) :- 
	!,
	setPrologInterrupt(spying),
	dbg_spyon.

%% === Skip on an exit port
continue(skip) :- 
	!,
	setPrologInterrupt(skipping).

%% === Jump on an exit port
continue(jump) :-
	!,
	setPrologInterrupt(jumping).

%% === Want to force a failure for some reason
continue(fail) :-
	callWithDelayedInterrupt(builtins,fail).

continue_debug :-
	getPrologInterrupt(debug_noshow),
	!,
	enable_colon_interrupt,
	dbg_spyon.
continue_debug :-
	setPrologInterrupt(debug_user),
	forcePrologInterrupt.


/*-------------------------------------------------------------------------*
 | fail/2
 | fail(UserCommand,Port)
 |
 | fail/2 decides how the compuation will continue after a failure.
 | If the debugger is to continue running after the failure, 
 | then we must restart the debugger for the failure. If not, 
 | we just want to fail without resetting things up.
 |
 | The port of call is necessary for deciding how to handle skip and jump.
 |
 *-------------------------------------------------------------------------*/

%% === Still want the debugger:
fail(debug,_) :-
    !,
    fail_debug.

%% === Don't want the debugger to continue:
fail(nodebug,_) :-
    !,
    setPrologInterrupt(debug_off),
    fail.

%% === Leaping
fail(leap,_) :-
    !,
    setPrologInterrupt(spying),
    dbg_spyon,
    fail.

%% === Skipping on a fail port
fail(skip,fail) :-
    !,
    setPrologInterrupt(skipping),
    fail.

%% === Skipping on a redo port
fail(skip,redo) :-
    !,
    setPrologInterrupt(skipping),
    fail.

%% === Jumping on a fail port
fail(jump,fail) :-
    !,
    setPrologInterrupt(jumping),
    dbg_spyon,
    fail.

%% === Jumping on a redo port
fail(jump,redo) :-
    !,
    setPrologInterrupt(jumping),
    dbg_spyon,
    fail.

%% === Want to force a failure (Yes, this is what happens anyway, but the
%%     user might for some perverse reason type f at the port and want
%%     failure. The retry mechanism also forces failure indiscrimately
%%     until the proper port is reached.  Thus, the following clause is
%%     important and necessary.)

fail(fail,_) :- !, 
	fail_debug.

/*-------------------------------------------------------------------------*
 | fail_debug
 *-------------------------------------------------------------------------*/

fail_debug :-
    getPrologInterrupt(debug_noshow),
    !,
    enable_colon_interrupt,
    dbg_spyon,
    fail.
fail_debug :-
    setPrologInterrupt(debug_user),
    callWithDelayedInterrupt(builtins,fail).

/*-------------------------------------------------------------------------*
 | exitOrRedo/6
 | exitOrRedo(Box,Depth,EMagic,RMagic,Module,Goal)
 |
 | This procedure organizes display of the exit and redo ports. (The
 | actual printing is handled by showGoal). When exitOrRedo is 
 | first called, the exit port is displayed. If failure occurs, a
 | redo port is displayed.
 *-------------------------------------------------------------------------*/

%% === Must treat cut specially for handling choice points:
exitOrRedo(Box,Depth,EMagic,RMagic,Module,Goal) :-
    Goal = '!'(_),
    !,
    showGoal(Box,Depth,exit,Module,Goal,UserCommand),
    setPrologInterrupt(EMagic),
    continue(UserCommand).

%% === clause for normal Goal exit:
exitOrRedo(Box,Depth,EMagic,RMagic,Module,Goal) :-
    showGoal(Box,Depth,exit,Module,Goal,UserCommand),
    setPrologInterrupt(EMagic),
    continue(UserCommand).

%% === clause for normal Goal redo:
exitOrRedo(Box,Depth,EMagic,RMagic,Module,Goal) :-
    dbg_notrace,				%% Don't want to trace debugger
    disable_colon_interrupt,
    showGoal(Box,Depth,redo,Module,Goal,UserCommand),
    setPrologInterrupt(RMagic),
    fail(UserCommand,RMagic).
    %%redo(UserCommand).			%% Fail as the user requests

/*-------------------------------------------------------------------------*
 | retryOrFail/6
 |
 | retryOrFail(FailCommand,Box,Depth,Magic,Module,Goal)
 |
 | This procedure is the heart of the retry mechanism.  As stated
 | earlier, the normal value for the Retry variable is zero.  The
 | Retry variable will get a non-zero value as the result of the
 | user requesting a retry.  From that point on, showGoal/6 detects
 | the fact that the Retry variable has a non-zero value and refuses
 | to print anything.  It also returns fail as the user command.
 | This then will force failure at every juncture possible until
 | the failure port is entered.  The failure port is handled by
 | the second clause of dogoal/5 (and this procedure).  When
 | this procedure is entered, the Retry value is obtained.  If the
 | Box number is less than or equal to the Retry value, then
 | we can do the retry by merely calling dogoal again.  Since
 | failure has occurred, the state of the computation should have
 | been restored to a state comparable to that when we first entered
 | the call port (modulo side effects).  Thus it is permissible
 | to simply call dogoal/5 again to rerun the goal.
 |
 | In the event that the Box number is not less than or equal to the
 | Retry value, the second clause will run.  This is what we want
 | to happen most of the time anyway.  All this clause does is
 | reset the Magic value and fail as the user desires.  There are
 | really two reasons for entering this clause.  The first is that
 | we aren't retrying anything and we want to take the normal action.
 | The second possibility is that we are retrying something, but we
 | haven't gotten back to where we need to be yet to do the retry.
 | Thus we still want to fail which again is what this clause does
 | for us.
 |		
 *-------------------------------------------------------------------------*/

%% === Handle the retry if possible
retryOrFail(FailCommand,Box,Depth,Magic,Module,Goal) :-
    getRetry(Retry),		%% Get the Retry global variable
    Box =< Retry,		%% See if we belong in this clause
    !,				%% Make sure that we are determinate
    setRetry(0),		%% Aren't retrying any more
    setCall(Box),		%% Reset the call number
    incDepth,			%% Advance the depth count
				%% Call dogoal again and start over with
				%%   the call port
    dogoal(Box,Depth,Magic,Module,Goal).

%% === Fail as the user wishes
retryOrFail(FailCommand,Box,Depth,Magic,Module,Goal) :-
    setPrologInterrupt(Magic),	%% Reset the magic value to what it was
				%% prior to attempting this goal. This
				%% is critical as we may be failing back
				%% into some builtins or other code
				%% which we don't want to show.  We
				%% also could be failing back into a
				%% spying phase.
    fail(FailCommand,fail).	%% Fail with the command obtained during
				%% execution of the second clause of 
				%% dogoal/5.

/*-------------------------------------------------------------------------*
 | enable_colon_interrupt and disable_colon_interrupt are called in order to
 | reenable or disable builtins:'$colon'/3 interrupts for the debug_noshow
 | mode of operation.
 *-------------------------------------------------------------------------*/

enable_colon_interrupt :-
    dbg_spy(builtins,'$colon',3),
    dbg_spy(builtins,call2,2).

disable_colon_interrupt :-
    getPrologInterrupt(debug_noshow),
    !,
    dbg_nospy(builtins,'$colon',3),
    dbg_nospy(builtins,call2,2).
disable_colon_interrupt.

/*-------------------------------------------------------------------------*
 | showGoal/6
 | showGoal(Box,Depth,Port,Module,Goal,UserResponse)
 |
 | This procedure displays the ports to the user and gets the
 | user's commands.
 *-------------------------------------------------------------------------*/
showGoal(Box,Depth,Port,Module,Goal,UserCommand)
	:-
	noshow(Module,Goal),
	!,
	(dmember(Port, [call,redo]) -> 
		UserCommand = skip 
		; 
		UserCommand = debug).

%% === Don't want to display a port if we are leaping.
showGoal(Box,Depth,Port,Module,Goal,leap) :-
	getPrologInterrupt(spying),
	!.

%% === Don't want to display a port if debugging is turned off
showGoal(Box,Depth,Port,Module,Goal,nodebug) :-
	getPrologInterrupt(debug_off),
	!.

%% === Main clause:
showGoal(Box,Depth,Port,Module,Goal,Response) :-
    getRetry(0),		%% Make sure Retry value is zero
    !,
    builtins:goalFix(Goal,XGoal,show_pp),
	getPrologInterrupt(CurInt),
	dbg_spyoff,
	dbg_notrace,
    showGoalToUser(Port,Box,Depth, Module, XGoal, Response),
	dbg_spyon,
	setPrologInterrupt(CurInt).
    
%% === Retry value was non-zero.  Force failure.
showGoal(Box,Depth,Port,Module,Goal,fail).

/*-------------------------------------------------------------------------
 |	noshow/2.
 |	noshow(Module,Goal)
 |
 |	noshow/2 specifies those predicates which should not be shown during
 |	debugging -- by module groups, various sends, and individual predicates.
 *------------------------------------------------------------------------*/

export noshow/2.
:- dynamic(noshow/2).
:- dynamic(excluded_object/1).
:- dynamic(excluded_message/1).

:- dynamic(skip_app_pred/2).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Skip these modules entirely:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%noshow(_, (not _)) :-!,fail.
%noshow(_, (\+ _)) :-!,fail.

noshow(Mod, _)
	:-
	noshow_module(Mod),
	!.

noshow_module(builtins).
noshow_module(debugger).
noshow_module(rel_arith).
noshow_module(sio).
noshow_module(tcltk).
noshow_module(tk_alslib).
noshow_module(xconsult).
noshow_module(global_gv_info).
%noshow_module(object_classes).

	%% Add any others (or can mtfapi go away now?):
noshow_module(alsdev).
noshow_module(alsshell).
noshow_module(avl).
noshow_module(cref).
noshow_module(macroxp).
noshow_module(pgm_info).
noshow_module(shellmak).
noshow_module(syscfg).
noshow_module(ttyshlmk).
noshow_module(utilities).
noshow_module(windows).
noshow_module(curl).


/****
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Skip any sends in these modules:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

noshow( Mod, send(_,_)) 
	:-
	noshow_send_mod(Mod), 
	!.
noshow_send_mod( builtins).
noshow_send_mod( debugger).
%noshow_send_mod( objects).
		%% either add others or make a generic thing:
noshow_send_mod( motif).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Skip sends to excluded objects: 
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

noshow( _, send(TgtObj,_))
	:-
	excluded_object(TgtObj),
	!.
excluded_object(debugger_object).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Skip sends of excluded messages: 
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
noshow( _, send(_,TgtMsg))
	:-
	excluded_message(TgtMsg),
	!.
excluded_message(newlineAction(_,_,_)).
****/

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Skip the following goals in any module:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

noshow(_,Goal) 
	:-
	noshow_goal(Goal),!.

noshow_goal( textModVerification(_,_,_) ).
noshow_goal( textModVerification(_,_,_,_) ).
noshow_goal( set_answer_var(_,_,_,_) ).
noshow_goal( set_answer_var(_,_,_) ).
noshow_goal( '$callback'(_,_,_) ).
noshow_goal( '$action'(_,_,_,_) ).

noshow_goal( run_debug_controls(_,_) ).
noshow_goal( source_trace(_,_) ).
noshow_goal( set_spy_point(_,_) ).
noshow_goal( change_tracing(_,_) ).
noshow_goal( set_noshow ).
noshow_goal( debug_graph(_,_) ).
noshow_goal( toggle_cb_notify ).
noshow_goal( toggle_action_notify ).
noshow_goal( toggle_null_send_notify ).

noshow_goal( dev_reconsult(_,_) ).
noshow_goal( edit_new(_,_) ).
noshow_goal( change_dir(_,_) ).
noshow_goal( quit_dialog(_,_,_) ).
noshow_goal( struct_define(_,_) ).
noshow_goal( comptype(_,_) ).

noshow_goal( top_dev_controls(_,_) ).
noshow_goal( open_debug(_,_) ).

export toggle_mod_show/1.
toggle_mod_show(Mod)
	:-
	noshow_module(Mod),
	!,
	retract(noshow_module(Mod)),
	printf('Debug showing enabled for module %t\n',[Mod]),
	flush_output.
toggle_mod_show(Mod)
	:-
	assert(noshow_module(Mod)),
	printf('Debug showing disabled for module %t\n',[Mod]),
	flush_output.

/*-------------------------------------------------------------------------*
 |	noshow_special/3
 |	noshow_special(Module,Predicate,Arity)
 |
 |	noshow_special/3 is used to tell about those builtins which can
 |	potentially call predicates which should be further traced. When
 |	creeping over these special builtins, it is not sufficient to turn 
 |	off the interrupts and let things go; we must interrupt every call.
 |	This is the purpose of the debug_noshow interrupt.
 *-------------------------------------------------------------------------*/
    
noshow_special(_,setof,3) :- !.
noshow_special(_,bagof,3) :- !.
noshow_special(_,findall,3) :- !.
noshow_special(_,catch,_) :- !.
noshow_special(_,unwind_protect,_) :- !.
noshow_special(_,catcher,_) :- !.
noshow_special(_,select_action,_) :- !.
noshow_special(_,';',_) :- !.	%% Disjunction
noshow_special(_,'->',_) :- !.	%% If-Then
noshow_special(_,',',_) :- !.	%% Conjunction
noshow_special(_,call,_) :- !.	%% Call
noshow_special(_,':',_) :- !.	%% Colon
noshow_special(_,call2,_) :- !.	%% Colon
noshow_special(_,'|',_) :- !.	%% Same as disjunction
noshow_special(_,not,_) :- !.	%% not
noshow_special(_,'\\+',_) :- !.	%% not again

%noshow_special(_,send,3) :- !.
%noshow_special(_,send,2) :- !.

/*-------------------------------------------------------------------------*
 | leash/1
 | leash(Leashing)
 |
 |   Turns on leashing of certain ports.  Also may be called with
 |   a variable to determine the current leashing.
 |
 |   The format of Leashing may be:
 |           all     -- leash all ports
 |           half    -- leash the redo and call ports
 |           redo    -- leash the redo port
 |           call    -- leash the call port
 |           fail    -- leash the fail port
 |           exit    -- leash the exit port
 |           A List  -- leash the ports contained in the list.
 *-------------------------------------------------------------------------*/

:-dynamic(leashed/1).

export leash/1.

leash(X) :-
   nonvar(X),
   !,
   abolish(leashed,1),
   do_leash(X).
leash(X) :- findall(L,leashed(L),X).

do_leash([]) :- !.
do_leash(all) :- 
   !,
   do_leash([call,redo,fail,exit]).
do_leash(half) :-
   !,
   do_leash([call,redo]).
do_leash(X) :-
   atom(X),
   isport(X),
   !,
   assert(leashed(X)).
do_leash([H|T]) :-
   do_leash(H),
   do_leash(T).

/*-------------------------------------------------------------------------*
 | leashed/1
 | leashed(Port)
 |
 | An enumeration of the ports which, by default, are leashed.
 | This is a dynamic predicate and is subject to change at runtime.
 *-------------------------------------------------------------------------*/

:- dynamic(leashed/1).

leashed(call).
leashed(exit).
leashed(redo).
leashed(fail).

/*-------------------------------------------------------------------------*
 | isport/1
 | isport(Port)
 |
 | An enumeration of all of the ports.
 *-------------------------------------------------------------------------*/

isport(call).
isport(redo).
isport(exit).
isport(fail).

/*-------------------------------------------------------------------------*
 | spy/0
 | spy
 |
 | Enables all spy points, but makes no changes to WHAT spy points
 | are set.
 *-------------------------------------------------------------------------*/

export 'spy'/0.

(spy) :-
	check_debug_io,
    setPrologInterrupt(spying),
    setDebugInterrupt(spying),
    dbg_spyon.

/*-------------------------------------------------------------------------*
 | spy/1
 | spy(PredPattern)
 |
 | Calls spy/3 to set a spy point on the given predicate, if
 | the module is specified.  If not, calls spy/2 to set spy
 | points on the given predicate in all modules in which
 | a name table entry for the procedure exists.
 *-------------------------------------------------------------------------*/

export 'spy'/1.

spy((Call when Condition)) :-
	!,
	spyWhen((Call when Condition)).

spy(Module : (Call when Condition)) :-
	!,
	spyWhen(Module : (Call when Condition)).

spy(Module:(Pred/Arity)) :- 
	!,
	spy(Module,Pred,Arity).

spy(Pred/Arity) :- 
	!,
	spy(Pred,Arity).

spy(Pred) :-
    atom(Pred),
    Pred \= [],
    !,
    spy_pat(_,Pred,_).

spy([]) :- !.

spy([Pat|More]) :-
   spy(Pat),
   spy(More).

/*-------------------------------------------------------------------------*
 | spy/2
 | spy(Pred,Arity)
 |
 | Installs a spy point on the given procedure in all modules where a
 | new name table entry for the procedure exists.  It then enables spy
 | points by calling dbg_spyon.
 *-------------------------------------------------------------------------*/

export 'spy'/2.

spy(Pred,Arity) :-
	check_debug_io,
	(nonvar(Arity) ->
		true
		;
		all_ntbl_entries(Module,Predicate,Arity,_)
	),
	findall(M, all_ntbl_entries(M,Pred,Arity,R), Modules),
	Modules \= [], 
	!,
	ensure_db_showing,
	dbg_spyoff,
	install_spypoints(Modules, Pred, Arity),
    	builtins:get_primary_manager(ALSMgr),
    	send(ALSMgr, refresh_wins),
	setPrologInterrupt(spying),
	setDebugInterrupt(spying),
    	printf(debugger_output,
           	'Spy point set on %t in modules %t.\n',
           	[Pred/Arity, Modules]),
	dbg_spyon.

spy(Pred,Arity) :-
    findall((M:(Pred/OtherArity)),
	 		all_ntbl_entries(M,Pred,OtherArity,R),Options),
	ensure_db_showing,
	printf(debugger_output,
		   '%t not defined in any module.\n', [Pred/Arity]),
    (   Options \= [] ->  
		printf(debugger_output,
	 	       'However, you may have meant to spy on one of the following:\n',
			   []),
		printf(debugger_output,'    %t.\n',Options)
    	;   
		true
    ),
	flush_output(debugger_output).

/*-------------------------------------------------------------------------*
 | spy_pat/3
 | spy_pat(M,P,A)
 |
 | Generalization of spy/2 in that M, P, or A may be uninstantiated.
 |
 | Note: It would be nice to expand this so that M,P,and A could match
 | arbitrary regular expressions.
 *-------------------------------------------------------------------------*/

export 'spy_pat'/3.

spy_pat(Module,Pred,Arity) 
	:-
	check_debug_io,
    findall(Module:Pred/Arity, all_ntbl_entries(Module,Pred,Arity,_), SpyList),
    SpyList \= [],
    !,
	ensure_db_showing,
    dbg_spyoff,
    setup_debug(Module, Pred, Arity),
    install_spypoints(SpyList),
    builtins:get_primary_manager(ALSMgr),
    send(ALSMgr, refresh_wins),
    setPrologInterrupt(spying),
    setDebugInterrupt(spying),
    printf(debugger_output,'Spy points set on: %t.\n', [SpyList]),
    dbg_spyon.

spy_pat(Module,Pred,Arity) 
	:-
	ensure_db_showing,
    printf(debugger_output,'No predicates match pattern %t.\n',[Module:Pred/Arity]).

install_spypoints([], Pred, Arity) :- !.

install_spypoints([Mod | RestModules], Pred, Arity) 
	:-
    install_spypoint(Mod,Pred,Arity),
    install_spypoints(RestModules, Pred, Arity).

install_spypoints([]) :- !.
install_spypoints([ M:P/A | RestSpyPoints]) 
	:-
    install_spypoint(M,P,A),
    install_spypoints(RestSpyPoints).

install_spypoint(Mod,Pred,Arity) 
	:-
    functor(CallForm,Pred,Arity),
    setup_debug(Mod, Pred, Arity),
    clause(spying_on(CallForm,Mod),true),
    !.

:- dynamic(spying_on/2).

install_spypoint(Mod,Pred,Arity) 
	:-
    functor(CallForm,Pred,Arity),
    assert(spying_on(CallForm,Mod)),
    builtins:dbg_spy(Mod,Pred,Arity).

/*-------------------------------------------------------------------------*
 | spy/3
 | spy(Module,Pred,Arity)
 |
 | Installs a spy point on the given procedure using dbg_spy.  Also
 | enables all spy points by calling dbg_spyon.
 *-------------------------------------------------------------------------*/

spy(Module,Predicate,Arity) :-
	check_debug_io,
	(nonvar(Arity) ->
		true
		;
		all_ntbl_entries(Module,Predicate,Arity,_)
	),
    functor(CallForm,Predicate,Arity),
    clause(spying_on(CallForm,Module),true),
    !,
	ensure_db_showing,
    printf(debugger_output,
		   'Already spying on %t\n', [Predicate/Arity]),
	flush_output(debugger_output).
     
spy(Module,Predicate,Arity) :-
	(nonvar(Arity) ->
		true
		;
		all_ntbl_entries(Module,Predicate,Arity,_)
	),
    check_existence(Module,Predicate,Arity,UMod),
    !,
    dbg_spyoff,
	setup_debug(Module, Predicate, Arity),
    functor(CallForm,Predicate,Arity),
    assert(spying_on(CallForm,Module)),
    builtins:dbg_spy(Module,Predicate,Arity),
    setPrologInterrupt(spying),
    setDebugInterrupt(spying),
	ensure_db_showing,
	printf(debugger_output,
		   'Spy point set on %t',[Module:Predicate/Arity]),
    (UMod=Module ->  
		true
    	;   
		printf(debugger_output, ' -exported from: %t',[UMod])
    ),
    nl(debugger_output),
    dbg_spyon.

spy(Module,Predicate,Arity) :-
    setof(OtherArity,
		  R^all_ntbl_entries(Module,Predicate,OtherArity,R),Arities),
	ensure_db_showing,
	printf(debugger_output,
		   '%t not defined in module %t or any modules it uses.\n',
		   [Predicate/Arity, Module]),
    (Arities \= [] ->  
	   printf(debugger_output,
		   'However, you may have meant to spy on %t/N ,where N is on the list:\n', 
		   [Predicate]),
	   printf(debugger_output, '      %t.\n', [Arities])
		;
		true
	  ).

check_existence(Module,Predicate,Arity,Module)
    :-
    '$procinfo'(_,Module,Predicate,Arity,_,ProcType),
    ( procinfo_prolog_proc(ProcType) ; procinfo_builtin_proc(ProcType) ), !.
check_existence(Module,Predicate,Arity,UMod)
    :-
    modules(Module,UseList),
    member(UMod,UseList),
    '$procinfo'(_, UMod, Predicate, Arity,_,ProcType),
    ( procinfo_prolog_proc(ProcType) ; procinfo_builtin_proc(ProcType) ),
    '$exported_proc'(UMod,Predicate,Arity), !.

procinfo_prolog_proc(0).
procinfo_builtin_proc(1).
procinfo_imported_proc(2).
procinfo_undefined_proc(3).
procinfo_unknown_type(-1).

/*-------------------------------------------------------------------------*
 |	spyWhen/1
 |	spyWhen(Module:Call)  [= spyWhen(Module,Call)]
 |	spyWhen/2
 |	spyWhen(Module,Call)  [= spyWhen(Module,Call,true)]
 |	spyWhen/3
 |	spyWhen(Module,Call,Condition)
 |
 |	Conditional spy points:
 |		In order for a call Goal to trigger spying/tracing,
 |		the Goal called must match the pattern specified in Call,
 |		and Condition must succed when run (by the debugger).
 *-------------------------------------------------------------------------*/

export spyWhen/1.
export spyWhen/2.

spyWhen(Module:WhenExpr) 
	:-!,
	check_debug_io,
	(WhenExpr = (Call when Condition) ->
		true;
		Call = WhenExpr, Condition = true
	),
	ensure_db_showing,
    install_when_spypoints([Module],Call,Condition).

spyWhen(WhenExpr) 
	:-
	check_debug_io,
	(WhenExpr = (Call when Condition) ->
		true;
		Call = WhenExpr, Condition = true
	),
	functor(Call, Pred, Arity),
	findall(M, all_ntbl_entries(M,Pred,Arity,R), Modules),
	Modules \= [], !,
	ensure_db_showing,
	install_when_spypoints(Modules,Call,Condition).

spyWhen(Module,Call) 
	:-
	check_debug_io,
    install_when_spypoints([Module],Call,true).

install_when_spypoints(Modules, Call, Condition)
	:-
    functor(Call, P, A),
    install_when_spypoints0(Modules, Call, P,A, Condition,OK,NotOK),
    (OK = [] ->  
		true
    	;   
		setDebugInterrupt(spying),
        setPrologInterrupt(spying),
		dbg_spyon
    ),
    printf(debugger_output,'Conditional spy point set on pattern: %t\n',[Call]),
    (Condition = true ->  
		true
    	;   
		printf(debugger_output,'--Under condition: %t\n',[Condition])
    ),
    printf(debugger_output,'  In modules: %t\n', [OK]),
    (NotOK = [] ->  
		true
    	;   
		printf(debugger_output,'  Not defined, so not set, in modules: %t\n',
				[NotOK])
    ).

install_when_spypoints0([], _, _,_,_,[],[]).
install_when_spypoints0([Mod | Modules], Call, P,A, Condition, [Mod | OK], NOK)
	:-
    check_existence(Mod,P,A,_),
	!,
	spyWhen(Condition, Mod, Call),
	install_when_spypoints0(Modules, Call, P,A, Condition, OK, NOK).
install_when_spypoints0([Mod | Modules], Call, P,A, Condition, OK, [Mod | NOK])
	:-
	install_when_spypoints0(Modules, Call, P,A, Condition, OK, NOK).

spyWhen(true,Module,Call) 
	:-!,
	functor(Call, Predicate, Arity),
    builtins:dbg_spy(Module,Predicate,Arity),
	functor(BCall, Predicate, Arity),
	assert((spying_on(BCall,Module) :- not(not((BCall = Call))))).
spyWhen(Condition,Module,Call) 
	:-
	functor(Call, Predicate, Arity),
	builtins:dbg_spy(Module,Predicate,Arity),
	functor(BCall, Predicate, Arity),
	assert((spying_on(BCall,Module) :- not(not((BCall = Call,Condition))))).

/*-------------------------------------------------------------------------*
 |	nospy/3
 |	nospy(Module,Predicate,Arity)
 |
 |	Removes a spy point from the specified predicate.
 *-------------------------------------------------------------------------*/

export nospy/3.

nospy(Module,Predicate,Arity) :-
    functor(CallForm,Predicate,Arity),
    clause(spying_on(CallForm,Module),Condition),
    !,
    dbg_nospy(Module,Predicate,Arity),
    (Condition = true -> retract(spying_on(CallForm,Module))
		      ;  retract((spying_on(CallForm,Module) :- Condition))),
    check_spyoff,
    builtins:get_primary_manager(ALSMgr),
    send(ALSMgr, refresh_wins),
    printf(debugger_output,'Spy point removed for %t:%t/%t\n',
    		[Module,Predicate,Arity]), 
    !.

nospy(Module,Predicate,Arity) :-
    printf(debugger_output,'Wasn\'t spying on %t:%t/%t\n',
    		[Module,Predicate,Arity]).

check_spyoff :- clause(spying_on(_,_),_),!.
check_spyoff :- dbg_spyoff.

/*-------------------------------------------------------------------------*
 | suppress_spying/0
 | suppress_spying
 |
 | Disables all spy points (using dbg_spyoff) without removing any
 | specific spy points from any predicates.
 *-------------------------------------------------------------------------*/

export suppress_spying/0.

suppress_spying :-
    dbg_spyoff.

/*-------------------------------------------------------------------------*
 | nospy/0
 | nospy
 |
 | Disables all spypoints (by removing them).  This will also remove
 | the spyWhen points.
 *-------------------------------------------------------------------------*/
    
export nospy/0.

'nospy' :-
	dbg_spyoff,
	findall(M:P-Tail, clause(spying_on(P,M),Tail), L),
	remove_spypoints(L),
        builtins:get_primary_manager(ALSMgr),
        send(ALSMgr, refresh_wins).

remove_spypoints([]) :- 
	!,
    	builtins:get_primary_manager(ALSMgr),
    	send(ALSMgr, refresh_wins).
remove_spypoints([M:Call-Tail | More]) :-
    functor(Call,P,A),
    dbg_nospy(M,P,A),
    retract_spypoint(Tail,M,Call),
    remove_spypoints(More).

retract_spypoint(true,M,Call) :- retract(spying_on(Call,M)), !.
retract_spypoint(Tail,M,Call) :- retract((spying_on(Call,M):-Tail)), !.

/*-------------------------------------------------------------------------*
 | spying/0
 | spying
 |
 | Tests to see if we are trapping on spy points.  If we are, it
 | succeeds.  If not, it fails.
 *-------------------------------------------------------------------------*/

export spying/0.

spying :- dbg_spying.

/*-------------------------------------------------------------------------*
 | nospy/1
 | nospy(PredPattern)
 |
 | Calls nospy/3 to remove a spy point on a given predicate.
 *-------------------------------------------------------------------------*/

export nospy/1.

nospy(Module:Pred/Arity) :-!,
   nospy(Module,Pred,Arity).
nospy(Pred/Arity) :-!,
	functor(CallForm,Pred,Arity),
	findall(M:CallForm-Tail, clause(spying_on(CallForm,M),Tail), L),
	remove_spypoints(L).
nospy(Pred) :-
	atom(Pred),
	findall(M:CallForm-Tail, (clause(spying_on(CallForm,M),Tail),functor(CallForm,Pred,_)), L),
	remove_spypoints(L).

nospy([]) :-!.
nospy([Pat|More]) :-
   nospy(Pat),
   nospy(More).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%    I/O Hooks    %%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%%
	%% First the parts of the I/O which are common, even
	%% if this cuts things up a bit artificially.
	%%

/*-------------------------------------------------------------------------*
 | getResponse/7
 | getResponse(Wins,Port,Box,Depth, Module, Goal, Response) 
 |
 | Prompt the user for what to do at this port.
 *-------------------------------------------------------------------------*/

%% === Port is leashed, so interact:
getResponse(Wins,Port,Box,Depth, Module, Goal, Response) 
	:-
    leashed(Port),
    !,
    getresponse2(Wins,Port,Box,Depth,Module,Goal,Response).

%% === Not a leashed port -- Keep debugging:

getResponse(Wins,_,Box,Depth,Module,Goal,debug) 
	:-
    nl(debugger_output).


/*-------------------------------------------------------------------------*
 | debugging/0
 |
 |	Display information about the debugger (spypoints, leasing, etc).
 |	It appears that this is the name that Quintus uses to display
 |	similar information.
 *-------------------------------------------------------------------------*/

export debugging/0.

debugging :-
    leash(Leashed),
    printf(debugger_output,
	   "Leasing is set on the following ports: %t\n",
	   [Leashed]),
    list_spypoints.

export list_spypoints/0.

list_spypoints :-
	clause(spying_on(_,_), _),
	!,
	printf(debugger_output, "Spypoints:\n", []),
	(
		clause(spying_on(CF,M), _),
		functor(CF,P,A),
		printf(debugger_output, "\t%t:%t/%t\n", [M,P,A]),
		fail
	    ;
		true
	).
list_spypoints :-
	printf(debugger_output, "No spypoints.\n", []).

export setup_debug/2.
setup_debug(Module, Call)
	:-
	functor(Call, Predicate, Arity),
	setup_debug(Module, Predicate, Arity).

export setup_debug/3.
setup_debug(Module, Predicate, Arity)
	:-
	debug_io(DebugIOChannel),
	setup_debug(DebugIOChannel, Module, Predicate, Arity).

setup_debug(DebugIOChannel, Module, Predicate, Arity)
	:-
	setup_debug(DebugIOChannel, Module, Predicate, Arity, [], _).


setup_debug(nowins, Module, Predicate, Arity, _, []) 
	:-!.
setup_debug(DebugIOChannel, Module, Predicate, Arity, CGsSetup, NextCGsSetup)
	:-
		% CG = ClauseGroup
	get_fcg(Module,Predicate,Arity,CG,DefiningMod),
	!,
	fin_setup_debug(CG, DefiningMod, Predicate, Arity, CGsSetup, NextCGsSetup).

setup_debug(DebugIOChannel, Module, Predicate, Arity, CGsSetup, NextCGsSetup)
	:-
	ensure_db_showing.

fin_setup_debug(CG, DefiningMod, Predicate, Arity, CGsSetup, CGsSetup)
	:-
	dmember(CG, CGsSetup),
	!.

fin_setup_debug(ClauseGroup, Module, Predicate, Arity, 
					CGsSetup, [ClauseGroup | CGsSetup])
	:-
	check_file_setup(Module, Predicate, Arity, SrcFilePath, BaseFileName, 
						DebugType, ClauseGroup, ALSMgr, SrcMgr),
	reload_debug(BaseFileName, SrcFilePath, DebugType, ClauseGroup, Flag),
	!,
	(builtins:clause(alsdev_running,_) ->
		(current_prolog_flag(debug,on) ->
			start_src_trace(Flag,BaseFileName, SrcFilePath, 
								ClauseGroup, ALSMgr, SrcMgr)
			;
			ensure_db_showing
		)
		;
		true
	).

fin_setup_debug(ClauseGroup, _, _, _, CGsSetup, [ClauseGroup | CGsSetup]).
	
get_fcg(Module,Predicate,Arity,ClauseGroup,Module)
	:-
	nonvar(Module),
	all_procedures(Module, Predicate, Arity, DBRef), 
	DBRef \= 0,
	!,
	'$clauseinfo'(DBRef,_,_,ClauseGroup).

get_fcg(Module,Predicate,Arity,ClauseGroup,DefiningMod)
	:-
	all_procedures(DefiningMod, Predicate, Arity, DBRef), 
	DBRef \= 0,
	'$clauseinfo'(DBRef,_,_,ClauseGroup).

/*---------------------------------------------------------------------*
check_file_setup(Module, Pred, Arity, SrcFilePath, BaseFileName,DebugType, ClauseGroup)
check_file_setup(+, +, +, -, -,-, -)
 *---------------------------------------------------------------------*/

check_file_setup(Module, Pred, Arity, SrcFilePath, BaseFileName,
					DebugType, ClauseGroup, ALSMgr, SrcMgr)
	:-
	builtins:file_clause_group(BaseFileName, ClauseGroup),
	builtins:get_primary_manager(ALSMgr),
	send(ALSMgr, obtain_src_mgr(BaseFileName, SrcMgr)),
	send(SrcMgr, get_value(base_file, BaseFileName)),
	send(SrcMgr, get_value(source_file, SrcFilePath)),
!,
	file_extension(SrcFilePath,_,Ext),
	Ext \= obp,
	send(SrcMgr, get_value(consult_mode, DebugType)).

check_file_setup(Module, Pred, Arity, SrcFilePath, BaseFileName,
					DebugType, ClauseGroup, ALSMgr, SrcMgr).

reload_debug(user,_, _,CG,nofile(user)) :-!.
reload_debug(BaseFileName,SrcFilePath, normal,CG,file)
	:-
	(file_extension(SrcFilePath,NoSuff,Ext), Ext \= '', !; NoSuff = SrcFilePath),
			%% Need to pass CG into consult:
	exists_file(SrcFilePath),
	!,
%	(current_prolog_flag(debug, off) ->
%		set_prolog_flag(debug, on),
%		consult(source(NoSuff),[quiet(true)])
%		;
%		true
%	),
	ensure_db_showing.
reload_debug(BaseFileName,SrcFilePath, normal,CG,nofile(BaseFileName)).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%    I/O Hooks    %%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export toggle_debug_io/0.
toggle_debug_io
	:-
	debug_io(Current),
	abolish(debug_io,1),
	dbg_io_opp(Current, Next),
	assert(debug_io(Next)).

	%% Default:
debug_io(nowins).

dbg_io_opp(nowins, WhichWins)
	:-!,
	current_prolog_flag(windows_system, WhichWins).
dbg_io_opp(_, nowins).

export set_debug_io/1.
set_debug_io(Where)
	:-
	abolish(debug_io,1),
	assert(debug_io(Where)).

check_debug_io
	:-
	debug_io(wins),
	!,
	alsshell:open_debug.
check_debug_io.
	
showGoalToUser(Port,Box,Depth, Module, XGoal, Response)
	:-
	debug_io(nowins),
	!,
	showGoalToUserTTY(Port,Box,Depth, Module, XGoal, Response).

showGoalToUser(Port,Box,Depth, Module, XGoal, Response)
	:-
	v_showGoalToUserWin(Port,Box,Depth, Module, XGoal, Response).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%% COMMOND I/O STUFF %%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		%% Items for debugger menu (menu/4 in Library: iolayer.pro)
		%% Compare usage in break package (blt_brk.pro)
deb_listOfCodes([
	a,					
	b,					
	c,
	d,					
	e,					
	f,
	i, 			
	j,					
	l,					
	m,
	n,					
	'N',				
	r,
	s,					
	t,					
	'?'
		]).

deb_choiceItems([
	    'Abort computation',
	    'Break to Prolog shell',
	    'Creep to next port', 
	    'Set print depth', 
	    'Exit Prolog',
	    'Fail goal', 
	    'Info: Print system statistics',
	    'Jump to next spy point or current port of call',
	    'Leap to next spy point',
	    'Change print mode', 
	    'Continue execution with tracing off',
	    'Turn off trace (in break)', 
	    'Retry goal',
	    'Skip to next port of call',
	    'Print stack Trace',
	    '?'			]).

deb_responses([
	abort,			
	break,			
	creep,
	set_print_depth,	
	exit,			
	fail,
	statistics,		
	jump,			
	leap,			
	change_print_mode,
	trace_off,		
	break_trace_off,	
	retry,
	skip,			
	stack_trace,
	help
	]).

deb_Options([		codes		= ListOfCodes,
			responses	= Responses,
			default	= c,
			title		= 'Debugger',
			indent		= '    ',
			prompt		= '? ' ] ) :-
	deb_listOfCodes(ListOfCodes),
	deb_responses(Responses).

		%% short form, used for non-menu interaction:
short_deb_resps([
	end_of_line-creep,  
	a-abort,            
	b-break, 			
	c-creep,
	d-set_print_depth,	
	e-exit,			
	f-fail,             
	i-statistics,       
	j-jump,
	l-leap,             
	m-change_print_mode,
	n-trace_off,		
	'N'-break_trace_off,	
	r-retry,            
	s-skip,
	t-stack_trace,
	'?'-'$badInput$', 		% = help
	_-'$badInput$'		%% default for all other input
		]).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%% TTY I/O Hooks   %%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

showGoalToUserTTY(exit,_,_,_,'$dbg_aph'(_,_,_),debug) :-!.
showGoalToUserTTY(exit,_,_,_,'$dbg_apg'(_,_,_),debug) :-!.
showGoalToUserTTY(exit,_,_,_,'$dbg_aphe'(_,_,_),debug) :-!.
showGoalToUserTTY(exit,_,_,_,'$dbg_apge'(_,_,_),debug) :-!.
showGoalToUserTTY(exit,_,_,_,'$dbg_apf'(_,_,_),debug) :-!.

showGoalToUserTTY(_,_,_,_,'$dbg_aph'(_,_,_),skip) :-!.
showGoalToUserTTY(_,_,_,_,'$dbg_apg'(_,_,_),skip) :-!.
showGoalToUserTTY(_,_,_,_,'$dbg_aphe'(_,_,_),skip) :-!.
showGoalToUserTTY(_,_,_,_,'$dbg_apge'(_,_,_),skip) :-!.
showGoalToUserTTY(_,_,_,_,'$dbg_apf'(_,_,_),skip) :-!.

showGoalToUserTTY(Port,Box,Depth, Module, XGoal, Response)
	:-
					    %% Display the port:
    writeGoal(Box,Depth,Port,Module,XGoal),
			   			%% And find out what do do:
    getResponse(tty,Port,Box,Depth, Module, XGoal, Response).



%% === Handle the interaction:

	%%----------------%%
	%%%%% TTY mode %%%%%
	%%----------------%%
getresponse2(tty,Port,Box,Depth,Module,Goal,Response) 
	:-
	short_deb_resps(Resps),
	getresponse_cont(Port,Box,Depth,Module,Goal,Resps,RR),
	!,
    act_on_response(RR,Port,Box,Depth, Module,Goal,tty,Response).

	%% First try to act without loading the menu code:
getresponse_cont(Port,Box,Depth,Module,Goal,Resps,Response)
	:-
	put_code(debugger_output,0'\?),
	flush_output(debugger_output),
	get_atomic_nonblank_char(debugger_input, R),
	dmember(R-Resp0,Resps),
	!,
	disp_getresponse_cont(Resp0, Port,Box,Depth,Module,Goal,Resps,Response).
	
	%% Now we have to load the menu code:
disp_getresponse_cont('$badInput$', Port,Box,Depth,Module,Goal,Resps,Response)
	:-
	deb_choiceItems(DebItems),
	deb_Options(Options),
	deb_listOfCodes(ListOfCodes),
	simple_menu(DebItems, R,
				[codes=ListOfCodes,return_code=true]),
	dmember(R-Resp0,Resps),
	!,
	disp_getresponse_cont(Resp0, Port,Box,Depth,Module,Goal,Resps,Response).

disp_getresponse_cont(Response,Port,Box,Depth,Module,Goal,Resps,Response).

	%%------------------%%
	%%%%% tcltk mode %%%%%
	%%------------------%%


%% === User wants to break to subsidiary Prolog shell.
act_on_response(break,Port,Box,Depth, Module,Goal,Wins,debug) 
	:-
	builtins:clause(alsdev_running,_),
	!,
	builtins:prolog_shell(user_input,user_output,alsdev).

act_on_response(break,Port,Box,Depth, Module,Goal,Wins,Response) :- !,
	telling(CurOut),
	tell(user_output),
	builtins:prolog_shell,
	tell(CurOut),
	show_again(Port,Box,Depth,Module,Goal,Wins,Response).

%% === User wants to exit.
act_on_response(exit,Port,Box,Depth, Module,Goal,Wins,Response) :- !,
	write(debugger_output,'Bye.'), 
	nl(debugger_output), 
	halt.

%% === User wants to abort.
act_on_response(abort,Port,Box,Depth, Module,Goal,Wins,nodebug) 
	:- !,
	debugger_abort.

export debugger_abort/0.
debugger_abort
	:-
	write(debugger_output,'Aborting from debugger.'),
	nl(debugger_output),
	dbg_notrace,
	dbg_spyoff,
	abort.
    
%% === User wants to turn off tracing when in a break.
act_on_response(break_trace_off,Port,Box,Depth, Module,Goal,Wins,nodebug) :- !,
	write(debugger_output,'Turning off tracing at:'),
	writeq(debugger_output,Module:Goal),
	nl(debugger_output),
	dbg_notrace,
	throw(breakhandler_debug(Module,Goal)).

%% === User wants to continue execution with tracing off
act_on_response(trace_off,Port,Box,Depth,Module,Goal,Wins,nodebug) :-
	!,
	suppress_spying.
    
%% === User wants to creep, skip, leap, or fail.
act_on_response(creep,Port,Box,Depth, Module,Goal,Wins,debug) :-  !.
act_on_response(skip,Port,Box,Depth, Module,Goal,Wins,skip) :-  !. 
act_on_response(leap,Port,Box,Depth, Module,Goal,Wins,leap) :-  !.
act_on_response(fail,Port,Box,Depth, Module,Goal,Wins,fail) :-  !.
act_on_response(jump,Port,Box,Depth, Module,Goal,Wins,jump) :-  !.

%% === User wants to do a retry.
act_on_response(retry,Port,Box,Depth, Module,Goal,Wins,fail) :- !,
	setRetry(Box).

%% === User wants to set the print depth.
act_on_response(set_print_depth, Port,Box,Depth, Module,Goal,Wins,Response) :-
	!,
	get_maxdepth(debugger_output,CurDepth),
	write(debugger_output,'Cur depth '=CurDepth),
	write(debugger_output,' New depth ='),
	read(debugger_input,ND),
	flush_input(debugger_input),
	set_maxdepth(debugger_output,ND), !,
	show_again(Port,Box,Depth,Module,Goal,Wins,Response).

%% === Changing the print mode
act_on_response(change_print_mode, Port,Box,Depth, Module,Goal,Wins,Response) :-
	!,
	get_depth_computation(debugger_output,DC0),
	(   DC0=flat
	->  DC1=nonflat
	;   DC1=flat),
	set_depth_computation(debugger_output,DC1),
	als_advise(debugger_output,"Depth computation is now %s\n",[DC1]),
	show_again(Port,Box,Depth,Module,Goal,Wins,Response).
    
%% === Print statistics
act_on_response(statistics, Port,Box,Depth, Module,Goal,Wins,Response) :-
	!,
	display_stats(debugger_output),
	show_again(Port,Box,Depth,Module,Goal,Wins,Response).

%% === Print stack trace
act_on_response(stack_trace, Port,Box,Depth, Module,Goal,Wins,Response) 
	:-
	builtins:clause(alsdev_running,_),
	!,
	builtins:get_primary_manager(ALSMgr),
	alsdev:accessObjStruct(debugger_mgr, ALSMgr,DebuggerMgr),
%	send(DebuggerMgr, show_stack),
	send(DebuggerMgr, show_stack_list),
%	show_again(Port,Box,Depth,Module,Goal,Wins,Response).
	getresponse2(Wins,Port,Box,Depth, Module, Goal, Response).

act_on_response(stack_trace, Port,Box,Depth, Module,Goal,Wins,Response) :-
	!,
	printf(debugger_output,'----begin stack trace----\n', []),
%	deb_stack_trace(1,1),
%	deb_stack_trace(1,1),
	builtins:stack_trace(1, debugger).
	printf(debugger_output,'----end stack trace----\n', []),
	show_again(Port,Box,Depth,Module,Goal,Wins,Response).


show_again(Port,Box,Depth,Module,Goal,Wins,Response) :-
	writeGoal(Box,Depth,Port,Module,Goal),
	getresponse2(Wins,Port,Box,Depth, Module, Goal, Response).

/*-------------------------------------------------------------------------*
 | writeGoal/5
 | writeGoal(Box,Depth,Port,Module,Goal) 
 |
 | Actually write the port out in a "nice" standard fashion.
 *-------------------------------------------------------------------------*/

writeGoal(Box,Depth,Port,Module,Goal) 
	:-
	printf(debugger_output,'(%d) %d %t: ', [Box,Depth,Port]),
	write_term(debugger_output, Module:Goal,	[lettervars(false)]),
	!,
	flush_output(debugger_output).


:- dynamic('$dbg_apg_special'/3).

endmod.					%% builtins:  debugger segment

module builtins.

export '$dbg_apg'/3.
export '$dbg_aph'/3.
export '$dbg_apge'/3.
export '$dbg_aphe'/3.
export '$dbg_apf'/3.


:- dynamic('$dbg_apg_special'/3).

'$dbg_apg'(A,B,C) :-  debugger:'$dbg_apg_special'(A,B,C),!.
'$dbg_apg'(_,_,_).
'$dbg_aph'(_,_,_).
'$dbg_apge'(_,_,_).
'$dbg_aphe'(_,_,_).
'$dbg_apf'(_,_,_).


endmod.

