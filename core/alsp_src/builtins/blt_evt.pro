/*========================================================================
 |		blt_event.pro
 |	Copyright (c) 1990-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		-- user level event handler
 |
 | This module is the user level event handler.  It implements some of the
 | ideas presented in "Event Handling in Prolog", Micha Meier, Logic
 | Programming: Proceedings of the North American Conference 1989.
 |
 | Author: Kevin A. Buettner
 | Creation: 5/92
 | Revision History:
 *========================================================================*/

module builtins.

/*!----------------------------------------------------------------------
 |	set_event_handler/3
 |	set_event_handler(Module, EventId, Proc)
 |	set_event_handler(+, +, +)
 |
 |	set_event_handler/2
 |	set_event_handler(EventId, Proc)
 |	set_event_handler(+, +)
 |
 | 	-	installs a global event handler
 |
 | set_event_handler(Module,EventId,Proc) installs a global event handler.
 | set_event_handler/3 is a module closure of set_event_handler/2.
 |
 |	Module	- the module in which set_event_handler/2 was called
 |	EventId	- id of the event which is being handled
 |	Proc	- name of a 3-argument handler procedure to handle this event.
 *!----------------------------------------------------------------------*/

export set_event_handler/2.

:-	compiletime,
	module_closure(set_event_handler,2).

set_event_handler(Module, EventId, Proc)
	:-
	remove_event(EventId),
	add_event(EventId,Module,Proc).

remove_event(EventId)
	:-
	retract(global_handler(EventId,_,_)),
	!.
remove_event(EventId).

add_event(EventId,Module,Proc)
	:- 
	asserta(global_handler(EventId,Module,Proc)).

/*-----------------------------------------------------------*
 | global_handler/2 is the entry to the global handler.
 *-----------------------------------------------------------*/

global_handler(EventId,Goal) 
	:-
	global_handler(EventId,Module,Proc),
	!,
%pbi_write(global_handler(EventId,Goal)),pbi_nl,pbi_ttyflush,
	call_handler(EventId,Goal,global_context,Module,Proc).
global_handler(EventId,Goal) :-
	ThrowTerm =.. [EventId,Goal],
	throw(ThrowTerm).

/*-----------------------------------------------------------*
 | global_handler/3 is a local database of event names and 
 | the handlers to use for those events.
 |
 | global_handler(EventId, Module, Proc)
 *-----------------------------------------------------------*/

global_handler(sigint,builtins,default_cntrl_c_handler).
global_handler(reisscntrl_c,builtins,silent_abort).
global_handler(libload,builtins,libload).
global_handler(prolog_error,builtins,prolog_error).
global_handler(undefined_predicate,builtins,undefined_predicate).
global_handler(heap_overflow,builtins,heap_overflow).

/*--------------------------------------------------*
 | call_handler(Event,Goal,Context,Module,Proc)
 |
 | call_handler/5 builds a handler and calls it.
 *--------------------------------------------------*/

call_handler(Event,Goal,Context,Module,Proc) 
	:-
	functor(Call,Proc,3),
	arg(1,Call,Event),
	arg(2,Call,Goal),
	arg(3,Call,Context),
	Module:Call.

/*--------------------------------------------------------*
 | signal_handler is called by the primitive interrupt
 | mechanism to deal with signals.
 *--------------------------------------------------------*/

signal_handler(SigNum,Module,Goal) 
	:-
	signal_name(SigNum,SigName),
	!,
	get_context(Context),
	propagate_event(SigName,Module:Goal,Context).

/*-----------------------------------------------------*
 | propagate_event/3 is called to propagate events.
 |
 | propagate_event(EventId,Goal,Context)
 |
 *-----------------------------------------------------*/
export propagate_event/3.

propagate_event(EventId,Goal,context(Module,Proc,PrevContext)) :-
	!,
	call_handler(EventId,Goal,PrevContext,Module,Proc).

propagate_event(EventId,Goal,global_context) 
	:-
%pbi_write(propagate_event(EventId,Goal)),pbi_nl,pbi_ttyflush,
	global_handler(EventId,Goal).

/*-------------------------------------------------------*
 | trigger_event/2 is called to start the event handler 
 | for the given event name and goal.
 *-------------------------------------------------------*/

export trigger_event/2.

trigger_event(EventId,ModuleAndGoal) :-
%pbi_write(trigger_event(EventId,ModuleAndGoal)),pbi_nl,pbi_ttyflush,
	get_context(Context),
%pbi_write(trigger_event_context=Context),pbi_nl,pbi_ttyflush,
	propagate_event(EventId,ModuleAndGoal,Context).

/*------------------------------------------------------------------*
 | trap(Goal,Handler) 
 | -- used to install a local handler Handler.
 | Otherwise, it behaves like call/1.
 |
 | It must install the new handler on entry to the goal and failure 
 | back into the goal.  When an exit occurs, or a failure out of the 
 | goal, the old handler must be re-established.  There is a problem 
 | in the procedure in that if a throw is performed in the handler, 
 | the context will not be set right.  The same is true for an abort.
 *------------------------------------------------------------------*/
:-	compiletime,
	module_closure(trap,2).

trap(Module,Goal,Handler) 
	:- 
	get_context(OldContext),
	catch(	trap(Module,Goal,Handler,OldContext),
		Anything,
		(
			set_context(OldContext),throw(Anything)
		)).

trap(Module,Goal,Handler,OldContext) 
	:-
	decompose_handler(Handler,Module,HMod,HProc),
	NewContext = context(HMod,HProc,OldContext),
	set_context(NewContext),
	trap_call(Module,Goal),
	trap_exit(OldContext,NewContext).

trap(_,_,_,OldContext) 
	:-
	set_context(OldContext),
	fail.

trap_call(Module,Goal) 
	:- 
	Module:Goal.

trap_exit(OldContext,NewContext) 
	:- 
	set_context(OldContext).

trap_exit(OldContext,NewContext) 
	:- 
	set_context(NewContext), fail.

/*-------------------------------------------------------------*
 | set_context/1 and get_context/1 use global variables and 
 | are built by the following call to make_gv.
 *-------------------------------------------------------------*/
:- make_gv('_context'), set_context(global_context).

/*------------------------------------------------------------*
 | PrologError is a global variable which contains the last 
 | prolog | error encountered.
 *------------------------------------------------------------*/

:- make_gv('PrologError').

/*------------------------------------------------------------*
 | decompose_handler(Handler,Module,HMod,HProc) 
 | -- called to get the module and procedure in a handler.  
 | This permits a handler in a different module to be used.
 *------------------------------------------------------------*/

decompose_handler(M:P,_,M,P) :- !.
decompose_handler(P,M,M,P).

/*--------------------------*
 | Specific handlers
 *--------------------------*/

default_cntrl_c_handler(_,M:G,_) 
	:-
%pbi_write(default_cntrl_c_handler(M:G)),pbi_nl,pbi_ttyflush,
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

throw_cntrl_c(sigint,Goal,Context) 
	:-
	throw(sigint(Goal)).

throw_cntrl_c(Event,Goal,Context) 
	:-
	propagate_event(Event,Goal,Context).

prolog_error(_,_,_) 
	:-
	getPrologError(PE),
	throw(PE).

heap_overflow(_,Module:Goal,_)
	:-
	/* Remove the arguments from the Goal, because the args may be large
	   and should be garbage collected. */
	functor(Goal, F, N),
	resource_error(heap_space, Module:F/N).

undefined_predicate(_,Goal,_) 
	:-
	get_PROLOG_flag(unknown,Val),
	undefined_predicate(Val,Goal).

undefined_predicate_fail(_,Goal,_) 
	:-
	fail.

undefined_predicate(fail,Goal) 
	:-!,
	fail.
undefined_predicate(error, Goal) 
	:-!,
	existence_error(procedure,Goal,Goal).
undefined_predicate(Kind, M:G) 
	:-
	functor(G,P,A),
	prolog_system_warning( error(existence_error(procedure,(M:P/A)),[M:G]),0),
	(Kind = break -> breakhandler(M,G) ; fail).

/*---------------------------------------------------------------*
 | Application controlled interrupts of procedures.
 |
 | [This facility is very primitive and should be improved.]
 |
 | break_on/3 and break_off/3 
 | -- cause an interrupt to be delivered when a certain 
 | procedure is called.  These are similar to spy and nospy.
 *---------------------------------------------------------------*/
export break_on/3, break_off/3.

break_on(M,P,A) :-
	dbg_spy(M,P,A),
	dbg_spyon,
	getPrologInterrupt(Old),
	setPrologInterrupt(application_interrupt(Old)).

break_off(M,P,A) :-
	dbg_nospy(M,P,A).

/********************************************************** 
 | Triggering certain predefined events
 |
 | The following predicates are used to trigger events
 **********************************************************/

/*----------------------------------------------------------------
 | type_error(Type,Culprit,Depth)
 | type_error(Type,Culprit,Goal)
 |
 |	Used to trigger a type error.
 |
 |	Type is an atom or other term indicating the type that was expected.
 |	Culprit is the term which did not match the expected type.
 |	Depth is the number of frames to go back to find the calling pattern
 |
 |	Note:	I am fairly certain that I don't like the Depth argument
 |		here.  I think that a more generic Environment argument
 |		is the way to go.  That way the programmer need not try
 |		to figure out the actual number of frames to where the
 |		error occurred.  A better implementation would be to use
 |		the als$cd directive to get the environment.
 *----------------------------------------------------------------*/
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

/*----------------------------------------------------------------------
 | Argument checking predicates which will trigger an error if argument
 | is not of proper form.
 *----------------------------------------------------------------------*/

/*---------------------------------------------------------------------*
 | atom_ok(Atom) will succeed if Atom is an atom, it will trigger an
 | instantiation error if Atom is a variable and a type error if Atom
 | is non-variable, but not an atom.  
 |
 | If an error is triggered, the error goal will be the one which called
 | atom_ok.  In order for the error throwing code to work properly, atom_ok
 | should not be called from a disjunction, or call and it should not be
 | the last goal in the clause.
 *---------------------------------------------------------------------*/

export atom_ok/1.

atom_ok(Atom) 
	:-
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

export var_or_nonneg_integer_ok/1.

var_or_nonneg_integer_ok(Var) :-
	var(Var),
	!.
var_or_nonneg_integer_ok(Int) :-
	integer(Int),
	Int >= 0,
	!.
var_or_nonneg_integer_ok(Other) :-
	integer(Other),
	domain_error(not_less_than_zero, Other, 2).
var_or_nonneg_integer_ok(Other) :-
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

/*------------------------------------------------------*
 | char_list_ok/1 is different in that it will fail 
 | when any portion is var.
 *------------------------------------------------------*/
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
	type_error(list,Other,2).

character(C) :-			%% should write in C or assembler
	atom(C),
	atom_length(C,1).

/*--------------------------------------------------------*
 | code_list_ok/1 is different in that it will fail 
 | when any portion is var.
 *--------------------------------------------------------*/
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
	type_error(list,Other,2).

endmod.
