/*
 * blt_event.pro	-- user level event handler
 *	Copyright (c) 1990-92 Applied Logic Systems, Inc.
 *
 * This module is the user level event handler.  It implements some of the
 * ideas presented in "Event Handling in Prolog", Micha Meier, Logic
 * Programming: Proceedings of the North American Conference 1989.
 *
 * Author: Kevin A. Buettner
 * Creation: 5/20/90
 * Revision History:
 */

module builtins.


/*
 * set_event_handler/2
 *
 * set_event_handler(EventId,Proc) is used to set a global event handler.
 * Proc should be a three argument handler procedure. See below.  EventId
 * is the name of the event which caused the interrupt.
 */

export set_event_handler/2.

:-	compiletime,
	module_closure(set_event_handler,2).


set_event_handler(Module, EventId, Proc) :-
	remove_event(EventId),
	add_event(EventId,Module,Proc).

remove_event(EventId) :-
	retract(global_handler(EventId,_,_)),
	!.
remove_event(EventId).


add_event(EventId,Module,Proc) :- 
	asserta(global_handler(EventId,Module,Proc)).



/*
 * global_handler/2 is the entry to the global handler.
 */

global_handler(EventId,Goal) :-
	global_handler(EventId,Module,Proc),
	!,
	call_handler(EventId,Goal,global_context,Module,Proc).
global_handler(EventId,Goal) :-
	ThrowTerm =.. [EventId,Goal],
	throw(ThrowTerm).

/*
 * global_handler/3 is a local database of event names and the handlers
 * to use for those events.
 *
 * global_handler(EventId, Module, Proc)
 */

global_handler(sigint,builtins,default_cntrl_c_handler).
global_handler(reisscntrl_c,builtins,silent_abort).
global_handler(libload,builtins,libload).
global_handler(prolog_error,builtins,prolog_error).
global_handler(undefined_predicate,builtins,undefined_predicate).


/*
 * call_handler(Event,Goal,Context,Module,Proc)
 *
 * call_handler/5 builds a handler and calls it.
 */

call_handler(Event,Goal,Context,Module,Proc) :-
	functor(Call,Proc,3),
	arg(1,Call,Event),
	arg(2,Call,Goal),
	arg(3,Call,Context),
	Module:Call.


/*
 * signal_handler is called by the primitive interrupt mechanism to deal
 * with signals.
 */

signal_handler(SigNum,Module,Goal) :-
	signal_name(SigNum,SigName),
	!,
	get_context(Context),
	propagate_event(SigName,Module:Goal,Context).

/*
 * signal_name/2 associates signal numbers with signal names.
 */

signal_name(1,sighup).
signal_name(2,sigint).		%% interrupt (as in Cntrl-C)
signal_name(3,sigquit).
signal_name(4,sigill).
signal_name(5,sigtrap).
signal_name(6,sigabrt).
signal_name(7,sigemt).
signal_name(8,sigfpe).
signal_name(9,sigkill).
signal_name(10,sigbus).
signal_name(11,sigsegv).
signal_name(12,sigsys).
signal_name(13,sigpipe).
signal_name(14,sigalrm).	%% alarm clock
signal_name(15,sigterm).
signal_name(16,sigurg).
signal_name(17,sigstop).
signal_name(18,sigtstp).
signal_name(19,sigcont).
signal_name(20,sigchld).
signal_name(21,sigttin).
signal_name(22,sigttou).
signal_name(23,sigio).
signal_name(24,sigxcpu).
signal_name(25,sigxfsz).
signal_name(26,sigvtalrm).
signal_name(27,sigprof).
signal_name(28,sigwinch).
signal_name(29,siglost).
signal_name(30,sigusr1).
signal_name(31,sigusr2).

signal_name(64,reisscntrl_c).		/* These numbers are from alssig.h */
signal_name(65,stack_overflow).
signal_name(66,libload).
signal_name(67,heap_overflow).
signal_name(68,prolog_error).
signal_name(69,undefined_predicate).


/*
 * propagate_event/3 is called to propagate events. (Pretty profound, huh?)
 *
 * propagate_event(EventId,Goal,Context)
 *
 */
export propagate_event/3.

propagate_event(EventId,Goal,context(Module,Proc,PrevContext)) :-
	!,
	call_handler(EventId,Goal,PrevContext,Module,Proc).
propagate_event(EventId,Goal,global_context) :-
	global_handler(EventId,Goal).


/*
 * trigger_event/2 is called to start the event handler for the given event
 * name and goal.
 */

export trigger_event/2.

trigger_event(EventId,ModuleAndGoal) :-
	get_context(Context),
	propagate_event(EventId,ModuleAndGoal,Context).


/*
 * trap(Goal,Handler) is used to install a local handler Handler.
 * Otherwise, it behaves like call/1.
 *
 * It must install the new handler on entry to the goal and failure back into
 * the goal.  When an exit occurs, or a failure out of the goal, the old
 * handler must be re-established.  There is a problem in the procedure in that
 * if a throw is performed in the handler, the context will not be set right.
 * The same is true for an abort.
 */

:-	compiletime,
	module_closure(trap,2).

trap(Module,Goal,Handler) :- 
	get_context(OldContext),
	catch(	trap(Module,Goal,Handler,OldContext),
		Anything,
		(set_context(OldContext),throw(Anything))).

trap(Module,Goal,Handler,OldContext) :-
	decompose_handler(Handler,Module,HMod,HProc),
	NewContext = context(HMod,HProc,OldContext),
	set_context(NewContext),
	trap_call(Module,Goal),
	trap_exit(OldContext,NewContext).

trap(_,_,_,OldContext) :-
	set_context(OldContext),
	fail.

trap_call(Module,Goal) :- Module:Goal.

trap_exit(OldContext,NewContext) :- set_context(OldContext).
trap_exit(OldContext,NewContext) :- set_context(NewContext), fail.

/*
 * set_context/1 and get_context/1 use global variables and are built by
 * the following call to make_gv.
 */

:- make_gv('_context'), set_context(global_context).

/*
 * PrologError is a global variable which contains the last prolog
 * error encountered.
 */

:- make_gv('PrologError').

/*
 * decompose_handler(Handler,Module,HMod,HProc) is called to get the module and
 * procedure in a handler.  This permits a handler in a different module
 * to be used.
 */

decompose_handler(M:P,_,M,P) :- !.
decompose_handler(P,M,M,P).


/*
 * Specific handlers
 */

default_cntrl_c_handler(_,M:G,_) :-
	breakhandler(M,G).

silent_abort(_,_,_) :-
    seen, told, abort.

libload(_,M:G,_) :-
	lib_load(M,G).

ignore_cntrl_c(sigint,_,_) :- 
	!,
	flush_input(user).
ignore_cntrl_c(Event,Goal,Context) :-
	propagate_event(Event,Goal,Context).

throw_cntrl_c(sigint,Goal,Context) :-
	throw(sigint(Goal)).
throw_cntrl_c(Event,Goal,Context) :-
	propagate_event(Event,Goal,Context).

prolog_error(_,_,_) :-
	getPrologError(PE),
	throw(PE).

undefined_predicate(_,Goal,_) :-
	get_PROLOG_flag(undefined_predicate,Val),
	undefined_predicate(Val,Goal).

undefined_predicate_fail(_,Goal,_) :-
	fail.

undefined_predicate(fail,Goal) :-
	!,
	fail.
undefined_predicate(error, Goal) :-
	!,
	existence_error(procedure,Goal,Goal).
undefined_predicate(warning, M:G) :-
	!,
	functor(G,P,A),
	als_advise('\nWarning: Undefined procedure %s:%s/%d called.\n',[M,P,A]),
	fail.
undefined_predicate(break, M:G) :-
	!,
	functor(G,P,A),
	als_advise('\nWarning: Undefined procedure %s:%s/%d called.\n',[M,P,A]),
	breakhandler(M,G).

/*
 * Application controlled interrupts of procedures.
 *
 *	This facility is very primitive and should be improved.
 *
 *	break_on/3 and break_off/3 cause an interrupt to be delivered when
 *	a certain procedure is called.  These are similar to spy and nospy.
 */

export break_on/3, break_off/3.

break_on(M,P,A) :-
	dbg_spy(M,P,A),
	dbg_spyon,
	getPrologInterrupt(Old),
	setPrologInterrupt(application_interrupt(Old)).

break_off(M,P,A) :-
	dbg_nospy(M,P,A).

/*
 * OLD STUFF!!!!!!!!
 *
 * The following stuff is old and should be phased out just as quick as
 * possible.
 */


/* -- old code --
/* 
 * Triggering certain predefined interrupts
 *
 * The following predicates are used to trigger interrupts
 */

:-	module_closure(calculation_error,1),
	module_closure(database_error,1),
	module_closure(evaluation_error,1),
	module_closure(implementation_error,1),
	module_closure(instantiation_error,1),
	module_closure(io_control_error,1),
	module_closure(io_end_of_file_error,1),
	module_closure(io_formatting_error,1),
	module_closure(operator_error,1),
	module_closure(overflow_error,1),
	module_closure(range_error,1),
	module_closure(syntax_error,1),
	module_closure(type_error,1),
	module_closure(undefined_predicate_error,1),
	module_closure(undefined_value_error,1),
	module_closure(underflow_error,1),
	module_closure(zero_divide_error,1),
	module_closure(argument_error,2).

calculation_error(Mod,Goal) :-
	trigger_event(calculation_error,Mod:Goal).
database_error(Mod,Goal) :-
	trigger_event(database_error,Mod:Goal).
evaluation_error(Mod,Goal) :-
	trigger_event(evaluation_error,Mod:Goal).
implementation_error(Mod,Goal) :-
	trigger_event(implementation_error,Mod:Goal).
instantiation_error(Mod,Goal) :-
	trigger_event(instantiation_error,Mod:Goal).
io_control_error(Mod,Goal) :-
	trigger_event(io_control_error,Mod:Goal).
io_end_of_file_error(Mod,Goal) :-
	trigger_event(io_end_of_file_error,Mod:Goal).
io_formatting_error(Mod,Goal) :-
	trigger_event(io_formatting_error,Mod:Goal).
operator_error(Mod,Goal) :-
	trigger_event(operator_error,Mod:Goal).
overflow_error(Mod,Goal) :-
	trigger_event(overflow_error,Mod:Goal).
range_error(Mod,Goal) :-
	trigger_event(range_error,Mod:Goal).
syntax_error(Mod,Goal) :-
	trigger_event(syntax_error,Mod:Goal).
type_error(Mod,Goal) :-
	trigger_event(type_error,Mod:Goal).
undefined_predicate_error(Mod,Goal) :-
	trigger_event(undefined_predicate_error,Mod:Goal).
undefined_value_error(Mod,Goal) :-
	trigger_event(undefined_value_error,Mod:Goal).
underflow_error(Mod,Goal) :-
	trigger_event(underflow_error,Mod:Goal).
zero_divide_error(Mod,Goal) :-
	trigger_event(zero_divide_error,Mod:Goal).

argument_error(Mod,Arg,Goal) :-
	var(Arg),
	!,
	trigger_event(instantiation_error,Mod:Goal).
argument_error(Mod,Arg,Goal) :-
	trigger_event(type_error,Mod:Goal).
-- old code -- */
/*
 * End of OLD STUFF!!!!!
 */

/*
 * Newer stuff.  The use of the stuff is encouraged.
 */


/*
 * type_error(Type,Culprit,Depth)
 * type_error(Type,Culprit,Goal)
 *
 *	Used to trigger a type error.
 *
 *	Type is an atom or other term indicating the type that was expected.
 *	Culprit is the term which did not match the expected type.
 *	Depth is the number of frames to go back to find the calling pattern
 *
 *	Note:	I am fairly certain that I don't like the Depth argument
 *		here.  I think that a more generic Environment argument
 *		is the way to go.  That way the programmer need not try
 *		to figure out the actual number of frames to where the
 *		error occurred.  A better implementation would be to use
 *		the als$cd directive to get the environment.
 *
 */

export type_error/3.
type_error(Type,Culprit,Depth) :-
	integer(Depth),
	!,
	frame_info(Depth,FI),
	setPrologError(error(type_error(Type,Culprit),[FI])),
	forcePrologError.
type_error(Type,Culprit,Goal) :-
	setPrologError(error(type_error(Type,Culprit),[Goal])),
	forcePrologError.

export instantiation_error/1.
instantiation_error(Depth) :-
	integer(Depth),
	!,
	frame_info(Depth,FI),
	setPrologError(error(instantiation_error,[FI])),
	forcePrologError.
instantiation_error(Goal) :-
	setPrologError(error(instantiation_error,[Goal])),
	forcePrologError.
    
export domain_error/3.
domain_error(Type,Culprit,Depth) :-
	integer(Depth),
	!,
	frame_info(Depth,FI),
	setPrologError(error(domain_error(Type,Culprit),[FI])),
	forcePrologError.
domain_error(Type,Culprit,Goal) :-
	setPrologError(error(domain_error(Type,Culprit),[Goal])),
	forcePrologError.

export existence_error/3.
existence_error(ObType,Culprit,Depth) :-
	integer(Depth),
	!,
	frame_info(Depth,FI),
	setPrologError(error(existence_error(ObType,Culprit),[FI])),
	forcePrologError.
existence_error(ObType,Culprit,Goal) :-
	setPrologError(error(existence_error(ObType,Culprit),[Goal])),
	forcePrologError.

export permission_error/4.
permission_error(OpType,ObType,Culprit,Depth) :-
	integer(Depth),
	!,
	frame_info(Depth,FI),
	setPrologError(error(permission_error(OpType,ObType,Culprit),[FI])),
	forcePrologError.
permission_error(OpType,ObType,Culprit,Goal) :-
	setPrologError(error(permission_error(OpType,ObType,Culprit),[Goal])),
	forcePrologError.

export resource_error/2.
resource_error(Resource,L) :-
	setPrologError(error(resource_error(Resource),L)),
	forcePrologError.

export system_error/1.
system_error(L) :-
	setPrologError(error(system_error,L)),
	forcePrologError.


/*
 * Argument checking predicates which will trigger an error if argument
 * is not of proper form.
 */

/*
 * atom_ok(Atom) will succeed if Atom is an atom, it will trigger an
 * instantiation error if Atom is a variable and a type error if Atom
 * is non-variable, but not an atom.  
 *
 * If an error is triggered, the error goal will be the one which called
 * atom_ok.  In order for the error throwing code to work properly, atom_ok
 * should not be called from a disjunction, or call and it should not be
 * the last goal in the clause.
 */

export atom_ok/1.

atom_ok(Atom) :-
	atom(Atom),
	!.
atom_ok(Var) :-
	var(Var),
	!,
	instantiation_error(2).
atom_ok(Other) :-
	type_error(atom,Other,2).


export var_or_atom_ok/1.

var_or_atom_ok(Var) :-
	var(Var),
	!.
var_or_atom_ok(Atom) :-
	atom(Atom),
	!.
var_or_atom_ok(Other) :-
	type_error(atom,Other,2).


export integer_ok/1.

integer_ok(Int) :-
	integer(Int),
	!.
integer_ok(Var) :-
	var(Var),
	!,
	instantiation_error(2).
integer_ok(Other) :-
	type_error(integer,Other,2).


export var_or_integer_ok/1.

var_or_integer_ok(Var) :-
	var(Var),
	!.
var_or_integer_ok(Int) :-
	integer(Int),
	!.
var_or_integer_ok(Other) :-
	type_error(integer,Other,2).

export var_ok/1.
var_ok(Var) :-
	var(Var),
	!.
var_ok(Other) :-
	type_error(variable,Other,2).

export nonvar_ok/1.
nonvar_ok(Var) :-
	nonvar(Var),
	!.
nonvar_ok(Other) :-
	instantiation_error(2).

export var_or_number_ok/1.

var_or_number_ok(Var) :-
	var(Var),
	!.
var_or_number_ok(Num) :-
	number(Num),
	!.
var_or_number_ok(Other) :-
	type_error(number,Other,2).

export number_ok/1.
number_ok(Var) :-
	var(Var),
	!,
	instantiation_error(2).
number_ok(Num) :-
	number(Num),
	!.
number_ok(Other) :-
	type_error(number,Other,2).



/*
 * char_list_ok/1 is different in that it will fail when any portion is var.
 */
export char_list_ok/1.

char_list_ok(Var) :-
	var(Var),
	!,
	fail.
char_list_ok([]) :-
	!.
char_list_ok([C|T]) :-
	var(C),
	!,
	fail.
char_list_ok([C|T]) :-
	character(C),
	!,
	char_list_ok(T).
char_list_ok(Other) :-
	domain_error(character_list,Other,2).

character(C) :-			%% should write in C or assembler
	atom(C),
	atom_length(C,1).


/*
 * code_list_ok/1 is different in that it will fail when any portion is var.
 */
export code_list_ok/1.

code_list_ok(Var) :-
	var(Var),
	!,
	fail.
code_list_ok([]) :-
	!.
code_list_ok([C|T]) :-
	var(C),
	!,
	fail.
code_list_ok([C|T]) :-
	integer(C),
	C >= 0, C < 256,	%% FIXME!
	!,
	code_list_ok(T).
code_list_ok(Other) :-
	domain_error(character_code_list,Other,2).


endmod.
