/*========================================================================
 |			blt_frez.pro
 |	Copyright (c) 1995-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		-- freeze handling
 |
 | Author: Ken Bowen
 | Creation: 4/95
 *========================================================================*/

#if (syscfg:freeze)

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
	'$is_delay_var'(Var),
	subst_var(Goal, Var, NewVar, NewGoal),
	'$delay'(NewVar,Mod,NewGoal,DelayTerm),
	!,
	Var = NewVar.

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

/***********************************
#if (syscfg:inconstrs)

'$combine_dvars'(R,F)
	:-
		%% F is the senior var;
%		pbi_write('-----Combine delay vars: '-(R,F)),pbi_nl,pbi_ttyflush,

	'$delay_term_for'(R, R_DelayTerm),
	arg(4, R_DelayTerm, R_ConstrTerm),

	'$delay_term_for'(F, F_DelayTerm),
	arg(4, F_DelayTerm, F_ConstrTerm),

	comb_left_cstr(R_ConstrTerm, R, F_ConstrTerm, F).

#else

'$combine_dvars'(R,F)
	:-
		%% F is the senior var;
%		pbi_write('-----Combine delay vars: '-(R,F)),pbi_nl,pbi_ttyflush,

	'$delay_term_for'(R, R_DelayTerm),
	arg(4, R_DelayTerm, R_ConstrTerm),

	'$delay_term_for'(F, F_DelayTerm),
	arg(4, F_DelayTerm, F_ConstrTerm),

	trailed_mangle(4, F_DelayTerm, (F_Constr, R_Constr)),
	'$bind_vars'(R, F).

#endif 
***********************************/

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% For showanswers, and relatives:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export show_v_list/1.
export show_v_list/2.

show_v_list([],[]).
show_v_list([X | L],[VN | LNs])
	:-
	show_variable(VN,X),
	show_v_list(L,LNs).
			 
show_v_list([]).
show_v_list([X | L])
	:-
	show_variable(X),
	show_v_list(L).


export show_variable/2.
show_variable(VName,Var)
	:-
	var(Var),
	!,
	sio:get_current_output_stream(Stream),
	(show_interval_binding(VName,Var,[],Stream) -> 
		true 
		;
		printf(Stream, '\n%t = %t', [VName,Var])
	).
		
show_variable(VName,Var)
	:-
	sio:get_current_output_stream(Stream),
	printf(Stream, '\n%t = %t', [VName,Var]).

export show_variable/1.
show_variable(Var)
	:-
	var(Var),
	!,
	sio_var_to_atom(Var,VarAtom),
	show_variable(VarAtom,Var).

show_variable(Var)
	:-
	printf(Stream, '\n<Var> = %t', [Var]).




:-rel_arith:dynamic('$domain_term'/2).

export show_interval_binding/4.
show_interval_binding(N,S,VPairs,Stream)
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
	),
	nl(Stream),
	!,
	write_term(Stream,'%lettervar%'(N)=SPrt,[]).

show_interval_binding(N,S,VPairs,Stream)
	:-
	show_delay_binding(N,S,VPairs,Stream).

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

export exhibit_var/1.
exhibit_var(Var)
	:-
	exhibit_var('', Var).

export exhibit_var/2.
exhibit_var(Name,Var)
	:-
	var(Var),
	!,
	sio:get_current_output_stream(Stream),
	sio_var_to_atom(Var,VarAtom),
	(extract_interval_bounds(Var, LArg, UArg, Type) ->
		SPrt = [LArg, UArg],
%		write_term(Stream,'%lettervar%'(VarAtom)=SPrt,[])
		nl(Stream),
		(Name \= '' ->
			put_code(Stream,0'[), write(Stream,Name), put_code(Stream,0'])
			;
			true
		),
		write_term(Stream,'%lettervar%'(VarAtom)=SPrt,[])
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

exhibit_var(Var)
	:-
	sio:get_current_output_stream(Stream),
	printf(Stream, '\n<Var> = %t', [Var]).
	


:-dynamic(freeze_disp_vns/0).
%freeze_disp_vns.

export show_delay_binding/4.
show_delay_binding(N, S, VPairs, Stream)
	:-
	'$delay_term_for'(S, VarDelayTerm),
	nl(Stream),
	write_term(Stream,'%lettervar%'(N),[]),
	(freeze_disp_vns ->
		sio_var_to_atom(S,DelayVarAtom),
		put_char(Stream,'['),
		put_atom(Stream, DelayVarAtom),
		put_char(Stream,']')
		;
		true
	),
	write_term(Stream,'->',[]),
	exact_merge(VPairs, [(S, N)], XVPairs),
	!,
	w_d_t(VarDelayTerm, Stream, XVPairs, Subsids).
		%% Need to recursively go on & display te
		%% var(pairs) on Subsids....

w_d_t(Term, Stream, XVPairs, [])
	:-
	var(Term),
	exact_lookup(XVPairs, Term, VName),
	!,
	put_atom(Stream, VName).

w_d_t(Term, Stream, _, [])
	:-
	var(Term),
	!,
	write_term(Stream, Term, []).

w_d_t('$delay'(DVar, _, _, FrozenTerm), Stream, XVPairs, Subsids )
	:-
	exact_lookup(XVPairs, DVar, VName),
	!,
	w_d_t(FrozenTerm, Stream, XVPairs, Subsids).

w_d_t('$delay'(DVar, _, _, FrozenTerm), Stream, XVPairs, 
			[(DVar, DVarAtom) | Subsids] )
	:-!,
	sio_var_to_atom(DVar,DVarAtom),
	put_atom(Stream, DVarAtom).

w_d_t(Term, Stream, _, [])
	:-
	atomic(Term),
	!,
	put_atom(Stream, Term).

w_d_t('%lettervar%'(QVarName), Stream, _, [])
	:- !,
	put_atom(Stream, QVarName).

w_d_t([], Stream, _, [])
	:-!,
	write_term(Stream, [], []).

w_d_t([FT | FTs], Stream, XVPairs, Subsids)
	:-!,
	put_char(Stream, '['),
	w_d_t_seq([FT | FTs], Stream, XVPairs, Subsids),
	put_char(Stream, ']').

w_d_t(Term, Stream, XVPairs, Subsids)
	:-
	Term =.. [Functor | Args],
	w_d_t_cmp(Functor,Args,Term, Stream, XVPairs, Subsids).

w_d_t_cmp(Functor,Args,Term, Stream, XVPairs,Subsids)
	:-
	functor(Term, _, 2), 
	sio:binop( Functor,_,_,_),
	!,
	Args = [Arg1, Arg2],
	w_d_t(Arg1, Stream, XVPairs, Subsids1),
	put_atom(Stream,Functor),
	w_d_t(Arg2, Stream, XVPairs, Subsids2),
	append(Subsids1, Subsids2, Subsids).
	
w_d_t_cmp(Functor,Args,Term, Stream, XVPairs, Subsids)
	:-
	write_term(Stream, Functor, []),
	put_char(Stream, '('),
	w_d_t_seq(Args, Stream, XVPairs, Subsids),
	put_char(Stream, ')').

w_d_t_seq([], Stream, XVPairs, []).

w_d_t_seq([Term], Stream, XVPairs, Subsids)
	:-!,
	w_d_t(Term, Stream, XVPairs, Subsids).
	
w_d_t_seq([Term | Terms], Stream, XVPairs, Subsids) 
	:-
	w_d_t(Term, Stream,  XVPairs, Subsids1),
	put_char(Stream, ','),
	w_d_t_seq(Terms, Stream,  XVPairs, Subsids2),
	append(Subsids1, Subsids2, Subsids).

exact_merge([], RightPairs, RightPairs).

exact_merge([(V,I) | LeftPairs], RightPairs, OutPairs)
	:-
	exact_mem(RightPairs, V),
	!,
	exact_merge(LeftPairs, RightPairs, OutPairs).

exact_merge([(V,I) | LeftPairs], RightPairs, [(V,I) | OutPairs])
	:-
	exact_merge(LeftPairs, RightPairs, OutPairs).

exact_mem([(W, _) | _], V)
	:-
	W == V, 
	!.	
exact_mem([ _ | RightPairs], V)
	:-
	exact_mem(RightPairs, V).

endmod.

#endif
