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

/*!----------------------------------------------------------------------
 *!----------------------------------------------------------------------*/

:- module_closure(freeze,2).  

export freeze/3.
freeze(Mod, Var, Goal)
	:-
	nonvar(Var),
	!,
	Mod:Goal.

freeze(Mod, Var, Goal)
	:-
	'$delay'(Var,Mod,Goal,DelayTerm).

/*!----------------------------------------------------------------------
 *!----------------------------------------------------------------------*/

:- module_closure(freeze_list_ground,2).  

export freeze_list_ground/3.
freeze_list_ground(Mod, [], Goal)
	:-
	Mod:Goal.

freeze_list_ground(Mod, [Var | Vars], Goal)
	:-
	nonvar(Var),
	!,
	get_vars(Var,[],NewVars),
	freeze_list_ground(Mod, NewVars, 
						freeze_list_ground(Mod, Vars, Goal)).

freeze_list_ground(Mod, [Var | Vars], Goal)
	:-
	'$delay'(Var,Mod,
			 freeze_list_ground(Mod,[Var | Vars], Goal),
			 DelayTerm).

/*!----------------------------------------------------------------------
 *!----------------------------------------------------------------------*/

delay_handler([]).
delay_handler('$delay'(_,Next,Module,Goal))
	:-
	Module:Goal,
	!,
	delay_handler(Next).

endmod.
