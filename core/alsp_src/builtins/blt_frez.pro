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

/*
export is_delay_var/1.
is_delay_var(V)
	:-
	var(V),
	'$is_delay_var'(X).
*/

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
	freeze_list_ground(Mod, NewVars, freeze_list_ground(Mod, Vars, Goal)).

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

/*!----------------------------------------------------------------------
 *!----------------------------------------------------------------------*/

'$combine_dvars'(R,F)
	:-
		%% F is the senior var;
		pbi_write('-----Combine delay vars: '-(R,F)),pbi_nl,pbi_ttyflush,

	'$delay_term_for'(R, R_DelayTerm),
	arg(4, R_DelayTerm, R_ConstrTerm),
%pbi_write('r-left'=R_ConstrTerm),pbi_nl,

	'$delay_term_for'(F, F_DelayTerm),
	arg(4, F_DelayTerm, F_ConstrTerm),
%pbi_write('f-right'=F_ConstrTerm),pbi_nl,
%pbi_ttyflush,

	comb_left_cstr(R_ConstrTerm, R, F_ConstrTerm, F).

		%% Compound constraint:
comb_left_cstr((RC1, Rest_R_C), R, F_Constr, F)
	:-
	(functor(RC1, intvl, 5) ->
			%% Has interval:
		comb_rt_cstr(F_Constr, F, RC1, Rest_R_C, R)
		;
			%% No interval:
		comb_rt_cstr(F_Constr, F, nil, (RC1, Rest_R_C), R)
	).

comb_left_cstr(R_Constr, R, F_Constr, F)
	:-
	(functor(R_Constr, intvl, 5) ->
			%% Has interval:
		comb_rt_cstr(F_Constr, F, R_Constr, true, R)
		;
			%% No interval:
		comb_rt_cstr(F_Constr, F, nil, R_Constr, R)
	).


comb_rt_cstr((FC1, Rest_F_C), F, R_Intv, R_Constr, R)
	:-
	(functor(FC1, intvl, 5) ->
			%% Has interval:
		cmbn(FC1, Rest_F_C, F, R_Intv, R_Constr, R)
		;
			%% No interval:
		cmbn(nil, (FC1, Rest_F_C), F, R_Intv, R_Constr, R)
	).


comb_rt_cstr(F_Constr, F, R_Intv, R_Constr, R)
	:-
	(functor(F_Constr, intvl, 5) ->
			%% Has interval:
		cmbn(F_Constr, true, F, R_Intv, R_Constr, R)
		;
			%% No interval:
		cmbn(nil, F_Constr, F, R_Intv, R_Constr, R)
	).


		%% No intvls:
cmbn(nil, F_Constr, F, nil, R_Constr, R)
	:-
			%% Combine both constraints,
			%% using a new variable:
	subst_var(F_Constr, F, G, G_CF),
	subst_var(R_Constr, R, G, G_CR),

			%% Freeze the combined constraints
			%% on the new variable:
	freeze(Mod, G, (G_CF, G_CR)),

			%% Bind both original vars to
			%% the new variable (must use this
			%% to avoid recursively invoking
			%% another var-var bind:
	'$bind_vars'(F, G),
	'$bind_vars'(R, G).


		%% R has intvl, but not F:
cmbn(nil, F_Constr, F, R_Intv, R_Constr, R)
	:-
	R_Intv \= nil,
	!,
			%% Combine both residual constraints,
			%% using a new variable:
	subst_var(F_Constr, F, G, G_CF),
	subst_var(R_Intv, R, G, G_Intv),

	(R_Constr = true ->
		FreezeGoal = (G_Intv, G_CF)
		;
		subst_var(R_Constr, R, G, G_CR),
		FreezeGoal = (G_Intv, (G_CR, G_CF))
	),
			%% Freeze the combined constraints
			%% on the new variable, with the
			%% interval from tacked on front:
	freeze(Mod, G, FreezeGoal),

			%% Bind both original vars to new var:
	'$bind_vars'(F, G),
	'$bind_vars'(R, G).

		%% F has intvl, but not R:
cmbn(F_Intv, F_Constr, F, nil, R_Constr, R)
	:- !,
			%% Combine both residual constraints,
			%% using a new variable:
	subst_var(F_Intv, R, G, G_Intv),
	subst_var(R_Constr, R, G, G_CR),
	(F_Constr = true ->
		FreezeGoal = (G_Intv, G_CR)
		;
		subst_var(F_Constr, F, G, G_CF),
		FreezeGoal = (G_Intv, (G_CF, G_CR))
	),
			%% Freeze the combined constraints
			%% on the new variable, with the
			%% interval from tacked on front:
	freeze(Mod, G, FreezeGoal),

			%% Bind both original vars to new var:
	'$bind_vars'(F, G),
	'$bind_vars'(R, G).

		%% Both have intvls:
cmbn(F_Intv, F_Constr, F, R_Intv, R_Constr, R)
	:-
		pbi_write('Both intvl case'),pbi_nl,pbi_ttyflush,
		pbi_write(f_intv=F_Intv),pbi_nl,pbi_ttyflush,
		pbi_write(r_intv=R_Intv),pbi_nl,pbi_ttyflush,
		pbi_write(calling-add_relation(==, R, F)),pbi_nl,pbi_ttyflush,
		%% Impose constraint equality between the vars:
	rel_arith:add_relation(==, R, F).
/*
cmbn(F_Intv, F_Constr, F, R_Intv, R_Constr, R)
	:-
	F_Intv = intvl(FType,FVar,_, FL,FU),
	R_Intv = intvl(RType,RVar,_, RL,RU),
	min(FL,RL,GL),
	max(FU,RU,GU),
	G_Intv = intvl(GType,GVar,_, GL,GU),
*/
	


subst_var(Term, V, W, W)
	:-
	var(Term),
	Term == V,
	!.

subst_var(Term, V, W, Term)
	:-
	var(Term),
	!.

subst_var([Term | Terms], V, W, [STerm | STerms])
	:-!,
	subst_var(Term, V, W, STerm),
	subst_var(Terms, V, W, STerms).
	
subst_var(Term, V, W, Result)
	:-
	functor(Term, Fnc, Arity),
	Arity > 0,
	!,
	Term =..[Fnc | Args],
	subst_var(Args, V, W, SArgs),
	Result =..[Fnc | SArgs].
	
subst_var(Term, V, W, Term).





endmod.
