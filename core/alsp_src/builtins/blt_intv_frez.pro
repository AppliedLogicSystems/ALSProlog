/*========================================================================
 |			blt_intv_frez.pro
 |	Copyright (c) 1995-2015 Applied Logic Systems, Inc.
 |
 |		-- freeze handling, interval-related part
 |
 | Author: Ken Bowen
 | Creation: 10/2015, split from blt_frez.pro (created 4/95)
 *========================================================================*/

:- dynamic(intconstrs/0).

'$combine_dvars'(R,F)
	:-
	intconstrs,				%% defined (or not) in main.c
	!,
		%% F is the senior var;
	'$delay_term_for'(R, R_DelayTerm),
	arg(4, R_DelayTerm, R_ConstrTerm),

	'$delay_term_for'(F, F_DelayTerm),
	arg(4, F_DelayTerm, F_ConstrTerm),

	comb_left_cstr(R_ConstrTerm, R, F_ConstrTerm, F).

'$combine_dvars'(R,F)
	:-
		%% F is the senior var;
	'$delay_term_for'(R, R_DelayTerm),
	arg(4, R_DelayTerm, R_Constr),

	'$delay_term_for'(F, F_DelayTerm),
	arg(4, F_DelayTerm, F_Constr),

	subst_var(F_Constr, F, NewVar, NewF_Constr),
	subst_var(R_Constr, R, NewVar, NewR_Constr),

	arg(3, F_DelayTerm, F_Mod),
	arg(3, R_DelayTerm, R_Mod),

	'$delay'(NewVar,F_Mod,(NewF_Constr, (R_Mod:NewR_Constr)),_),
	'$bind_vars'(F, NewVar),
	'$bind_vars'(R, NewVar).

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
	subst_var(F_Constr, F, NewVar, NewF_Constr),
	subst_var(R_Constr, R, NewVar, NewR_Constr),

	'$delay_term_for'(F, F_DelayTerm),
	arg(3, F_DelayTerm, F_Mod),

	'$delay_term_for'(R, R_DelayTerm),
	arg(3, R_DelayTerm, R_Mod),

	'$delay'(NewVar,F_Mod,(NewF_Constr, (R_Mod:NewR_Constr)),_),
	'$bind_vars'(F, NewVar),
	'$bind_vars'(R, NewVar).

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
%		pbi_write('Both intvl case'),pbi_nl,pbi_ttyflush,
%		pbi_write(f_intv=F_Intv),pbi_nl,pbi_ttyflush,
%		pbi_write(r_intv=R_Intv),pbi_nl,pbi_ttyflush,
%		pbi_write(calling-add_relation(==, R, F)),pbi_nl,pbi_ttyflush,
		%% Impose constraint equality between the vars:
	rel_arith:add_relation(==, R, F).

:-rel_arith:dynamic('$domain_term'/2).

export show_interval_binding/4.
show_interval_binding(N,S,VPairs,Stream)
	:-
	reified_interval(S, SPrt),
	!,
	nl(Stream),
	write_term(Stream,'%lettervar%'(N)=SPrt,[]).

show_interval_binding(N,S,VPairs,Stream)
	:-
	show_delay_binding(N,S,VPairs,Stream).


export reified_interval/2.
reified_interval(S, SPrt)
	:-
	var(S),
	extract_interval_bounds(S, LArg, UArg, Type),
	epsilon_show(Eps),
	Width is abs(UArg - LArg),
	(Width < Eps ->
		SPrt0 is (UArg + LArg)/2,
		(dmember(Type, [integer,boolean]) ->
			SPrt is floor(SPrt0)
			; 
			SPrt is SPrt0
		)
		;
		SPrt = [LArg, UArg]
	).
	
export reified_interval_list/2.
reified_interval_list([], []).
reified_interval_list([S | Ss], [SPrt | SPrts])
	:-
	reified_interval(S, SPrt),
	reified_interval_list(Ss, SPrts).

export extract_interval_bounds/3.
extract_interval_bounds(S, LArg, UArg)
	:-
	extract_interval_bounds(S, LArg, UArg, _).

export extract_interval_bounds/4.
extract_interval_bounds(S, LArg, UArg, Type)
	:-
	rel_arith:'$domain_term'(S, DomainTerm),
	rel_arith:valid_domain(DomainTerm, Type, LArg0, UArg0),
	!,
			%% show the associated domain; ignore
			%% any other constraints;
	(dmember(Type, [integer,boolean]) ->
		LArg is floor(LArg0),
		UArg is floor(UArg0)
		;
		LArg = LArg0, UArg = UArg0
	).

export extract_interval/2.
extract_interval(S, [LArg, UArg])
	:-
	'$is_delay_var'(S),
	!,
	extract_interval_bounds(S, LArg, UArg).
extract_interval(S, S).

export extract_interval_list/2.
extract_interval_list([], []).
extract_interval_list([S | Ss], [I | Is])
	:-
	extract_interval(S, I),
	extract_interval_list(Ss, Is).

show_delay_variable(VName,Var)
	:-
	sio:get_current_output_stream(Stream),
	printf(Stream, '\n%t = %t', [VName,Var]).


export exhibit_var/2.
exhibit_var(Name,Var)
	:-
	var(Var),
	!,
	sio:get_current_output_stream(Stream),
	sio_var_to_atom(Var,VarAtom),
	(extract_interval_bounds(Var, LArg, UArg, Type) ->
		show_var_intrv(Name, Var, LArg, UArg, Stream)
		;
		(show_delay_binding(VarAtom, Var, [], Stream) ->
			true 
			; 
			printf(Stream, '\n%t = %t', [VarAtom,Var])
		)
	).

exhibit_var(Name,Var)
	:-
	sio:get_current_output_stream(Stream),
	printf(Stream, '\n[%t] = %t', [Name, Var]).



