/*========================================================================
 |		blt_frez.pro
 |	Copyright (c) 1995 Applied Logic Systems, Inc.
 |
 |		-- freeze handling
 |
 | Author: Ken Bowen
 | Creation: 4/95
 *========================================================================*/

module builtins.

:-	make_gv('_$delay_terms'), 'set_$delay_terms'([]).	%% delay_terms_list
/*!----------------------------------------------------------------------
 *!----------------------------------------------------------------------*/

:- module_closure(freeze,2).  

export freeze/3.
freeze(Mod, Var, Goal)
	:-
	nonvar(Var),
	!,
	call(Goal).

freeze(Mod, Var, Goal)
	:-
	'$delay'(Var,Mod,Goal,DelayTerm),
	'get_$delay_terms'(DTL),
	'set_$delay_terms'([DelayTerm | DTL]).

export free_thawed/0.
free_thawed
	:-
	collect_thawed(ThawedList),
	'get_$delay_terms'(DTL),
	exact_remove(ThawedList,DTL,NewDTL),
	'set_$delay_terms'(NewDTL).

export exact_remove/3.
exact_remove([],DTL,DTL).
exact_remove([DT | ThawedList],DTL,NewDTL)
	:-
	xdelete(DTL,DT,InterDTL),
	exact_remove(ThawedList,InterDTL,NewDTL).

xdelete([],DT,[]).
xdelete([Item | DTL],DT,InterDTL)
	:-
	Item == DT,
	!,
	xdelete(DTL,DT,InterDTL).
xdelete([Item | DTL],DT,[Item | InterDTL])
	:-
	xdelete(DTL,DT,InterDTL).

/*!----------------------------------------------------------------------
 *!----------------------------------------------------------------------*/

delay_handler([]).
delay_handler('$delay'(_,Next,Module,Goal))
	:-
	Module:Goal,
	!,
	delay_handler(Next).

endmod.
