/*================================================================*
 |              int_cstr.pro
 |      Copyright (c) 1995 Applied Logic Systems, Inc.
 |      Copyright (c) 1995 Bell-Northern Research Ltd.
 |
 |	Lower level of the interval constraint subsystem
 |
 |      Original Author: William J. Older
 |      Original Creation Date: 08/27/92
 |      Relational Interval Arithmetic Subsystem of BNR Prolog V4.x
 |
 |  Revisions/Modifications: Ken Bowen
 |  Date begun: 05/10/95
 |
 |              Integration into ALS Prolog
 *================================================================*/

module rel_arith.

intvl(_,_,_,_).

/*---------------------------------------------------------------
 *--------------------------------------------------------------*/
type_and_bounds(real(L,U),    real,    L, U).
type_and_bounds(real,         real,    L, U).
type_and_bounds(integer(L,U), integer, L, U).
type_and_bounds(integer,      integer, L, U).
type_and_bounds(boolean(L,U), boolean, 0, 1).
type_and_bounds(boolean,      boolean, 0, 1).

/*---------------------------------------------------------------
 |	new_type_interval/2
 |	new_type_interval(Type,X)
 |	new_type_interval(+,+)
 |
 |	- makes X into an interval delay var of type Type
 |
 |  This is the only routine which creates interval variables.
 *--------------------------------------------------------------*/

	%% should make this (24) more accurate:
uia_space(_,UIA)
	:-
%	'$uia_alloc'(24,UIA).
	'$uia_alloc'(16,UIA).

new_type_interval(Type,X)
	:-
	type_and_bounds(Type, BareType, L, U),
	interval_bound(lower,L,L1,BareType),
	interval_bound(upper,U,U1,BareType),
	uia_space(BareType,UIA),
	'$uia_poked'(UIA,0,L1),
	'$uia_poked'(UIA,8,U1),
	freeze_goal_for(BareType, X, L1, U1, UIA, FreezeGoal),
	freeze(X, FreezeGoal).

freeze_goal_for(real,    X, L1, U1, UIA, intvl(real,   X,[],UIA) ).
freeze_goal_for(integer, X, L1, U1, UIA, intvl(integer,X,[],UIA) ).
freeze_goal_for(boolean, X, L1, U1, UIA, intvl(boolean,X,[],UIA) ).

new_combined_interval(Type, X)
	:-
	type_and_bounds(Type, BareType, L, U),
	interval_bound(lower,L,L1,BareType),
	interval_bound(upper,U,U1,BareType),
	uia_space(BareType,UIA),
	'$uia_poked'(UIA,0,L1),
	'$uia_poked'(UIA,8,U1),
	freeze_goal_for(BareType, X, L1, U1, UIA,TypeFreezeGoal),
	'$delay_term_for'(X, DelayTerm),
	arg(4, DelayTerm, OrigDomainTerm),
	trailed_mangle(4, DelayTerm, (OrigDomainTerm, TypeFreezeGoal)).

/*---------------------------------------------------------------
 |	Accessing and destructive changes to the components of an
 |	interval structure :
 |
 |		intvl(Type, TheVar, UsedByList, L, U, UIA)
 |
 *--------------------------------------------------------------*/

access_intvl_type(Intvl, NewType)
	:-
	arg(1, Intvl, NewType).

update_intvl_type(Intvl, NewType)
	:-
	trailed_mangle(1, Intvl, NewType).

update_lower_bd(Intvl, NewVal)
	:-
%	trailed_mangle(4, Intvl, NewVal),
	arg(4, Intvl, UIA),
	update_uia_dbl(Intvl,0,NewVal). %% 0 = lower

update_upper_bd(Intvl, NewVal)
	:-
%	trailed_mangle(5, Intvl, NewVal),
	arg(4, Intvl, UIA),
	update_uia_dbl(Intvl,1,NewVal). %% 1 = upper

access_used_by(Intvl, NewVal)
	:-
	arg(3, Intvl, NewVal).

update_used_by(Intvl, NewVal)
	:-
	trailed_mangle(3, Intvl, NewVal).

/*---------------------------------------------------------------
 *--------------------------------------------------------------*/

add_to_used_by([], _).
add_to_used_by([Var | VarList], Node)
	:-
	'$is_delay_var'(Var),
	!,
	'$domain_term'(Var, DomainTerm),
	access_used_by(DomainTerm, OldUsedByList),
	add_node_2_used_by(OldUsedByList, Node, DomainTerm),
	add_to_used_by(VarList, Node).

add_to_used_by([_ | VarList], Node)
	:-
	add_to_used_by(VarList, Node).

add_node_2_used_by([], Node, DomainTerm)
	:-!,
	update_used_by(DomainTerm, [Node]).

add_node_2_used_by([U | _], Node, DomainTerm)
	:-
	U == Node,
	!.

	%% _ can't be Node:
add_node_2_used_by(L, Node, DomainTerm)
	:-
	L = [_],
	!,
	trailed_mangle(2,L,[Node]).

	%% length(L) >= 2:
add_node_2_used_by(L, Node, DomainTerm)
	:-
	L = [ _ | LT],
	add_node_2_list(LT, L, Node).

add_node_2_list([], L, Node)
	:-!,
	trailed_mangle(2,L,[Node]).

add_node_2_list([H | RestLT], L, Node)
	:-
	H == Node,
	!.

add_node_2_list(LT, _, Node)
	:-
	LT = [_ | RestLT],
	add_node_2_list(RestLT, LT, Node).


show_used_by([])
	:-
	nl.

show_used_by([VN-Var | VarList])
	:-
	'$is_delay_var'(Var),
	!,
	'$domain_term'(Var, DomainTerm),
	access_used_by(DomainTerm, UsedByList),
	printf_opt('%t[%t] used_by=%t   ',[VN,Var,UsedByList],
					[lettervars(false) ,line_length(180)]),
	show_used_by(VarList).

show_used_by([VN-Var | VarList])
	:-
	printf_opt('%t[%t]-nd  ',[VN,Var], [lettervars(false) ,line_length(180)]),
	show_used_by(VarList).

show_used_by([_ | VarList])
	:-
	show_used_by(VarList).

/*
init_used_by([], _).

init_used_by([Var | VarList], Node)
	:-
	'$is_delay_var'(Var),
	!,
	'$domain_term'(Var, DomainTerm),
	set_used_by(DomainTerm, [Node]),
	init_used_by(VarList, Node).

init_used_by([_ | VarList], Node)
	:-
	init_used_by(VarList, Node).

*/




/*---------------------------------------------------------------
 *--------------------------------------------------------------*/
interval_bound(Side, Quant, Bound, Type)
	:-
	var(Quant),
	!,
	max_bound(Type, MaxBound),
	interval_bound_decide(Side, MaxBound, Bound).

interval_bound(_,Quant, Bound, _)
	:-
	Bound is float(Quant).

interval_bound_decide(upper, MaxBound, MaxBound).
interval_bound_decide(lower, MaxBound, Bound)
	:-
	Bound is - MaxBound.

max_bound(real, 1.0e100).
max_bound(integer, Y)
	:-
	maxint(X),
	Y is float(X).

%% Temporary hack until we get the maxint prolog flag stuff implemented:

maxint(123456789).


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Domain Queries
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domain_check(real, X)           :- domain(X,real(_,_)).
domain_check(integer, X)        :- domain(X,integer(_,_)).
domain_check(boolean, X)        :- domain(X,boolean(_,_)).

export domain/2.

domain(Var, Descriptor)
	:-
	'$domain'(Var, Type, LB, UB),
	Descriptor =.. [Type, LB, UB].

	%%%%!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	%%%%  WARNING!!!!
	%%%%  Do NOT attempt to coalesce the first two goals of the
	%%%%  following clause into the following natural single
	%%%%  goal:
	%%%%      '$delay_term_for'(Var, '$delay'(_,_,_,DomainTerm)),
	%%%%
	%%%%  Since the term '$delay'(_,_,_,DomainTerm) exists on the
	%%%%  heap, then when this is unified against the actual
	%%%%  delay term for Var, the (anonymous) initial variable
	%%%%  passes the delay variable test, so the anonymous
	%%%%  variable and Var are combined with combine_delays,
	%%%%  but this produces circulariy, .....
	%%%%    Also, replacing the arg(4...) call by a unification:
	%%%%
	%%%%    VarDelayTerm = '$delay'(_,_,_,DomainTerm),
	%%%%
	%%%%  has the same effect.
	%%%%-------------------------------------------------------
'$domain_term'(Var, DomainTerm)
	:-
	'$delay_term_for'(Var, VarDelayTerm),
	arg(4, VarDelayTerm, ConstraintTerm),
	domain_term_from_constr(ConstraintTerm, DomainTerm).

domain_term_from_constr((DomainTerm, _), DomainTerm) :-!.
domain_term_from_constr(DomainTerm, DomainTerm).

'$domain'(Var, Type, LB, UB)
	:-
	'$domain_term'(Var, DomainTerm),
	valid_domain(DomainTerm, Type, LB, UB).

valid_domain(( A, B ), Type, LB, UB)
	:-
	valid_domain( A, Type, LB, UB).

valid_domain(Intrv, Type, LB, UB)
	:-
	functor(Intrv,intvl,4),
	arg(1,Intrv,Type),
	arg(4,Intrv,UIA),
	'$uia_peekd'(UIA,0,LB),
	'$uia_peekd'(UIA,8,UB).

%%%%%%%%%%%%% NODE BUILDING%%%%%%%%%%%%%%%

point_interval( N,  N)
	:-
	integer(N), !.

point_interval( 0.0,0.0)          % dont fuzz 0.0
	:- !.

point_interval( X, PX)
	:-
	fuzz_float(X,XL,XH),
	PX::real(XL,XH).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export new_node/1.

new_node(Node)
	:-
	'$iterate'(Node).

export '$iterate'/1.
'$iterate'(Goal)
	:-
%printf_opt('Goal= %t\n',[Goal], [lettervars(false) ,line_length(100)]),
	Goal =.. [OP | Args],
	prim_op_code(OP,OpCd),

	XFGoal =.. [pop, OpCd,Z,X,Y,LinkArg],
	fixup_iter(Args, Z,X,Y,XFGoal),

%printf_opt('%t--XFGoal= %t\n',[Goal,XFGoal], [lettervars(false) ,line_length(100)]),
%show_used_by(['Z'-Z,'X'-X,'Y'-Y]),

	'$iter_link_net'(OpCd,Z,X,Y,XFGoal).

fixup_iter([Z,X], Z,X,0,XFGoal)
	:-
	add_to_used_by([Z,X], XFGoal).
		
fixup_iter([Z,X,Y], Z,X,Y,XFGoal)
	:-
	add_to_used_by([Z,X,Y], XFGoal).

prim_op_code(unequal, 0).
prim_op_code(equal, 1).
prim_op_code(greatereq, 2).
prim_op_code(higher, 3).
prim_op_code(add, 4).
prim_op_code(begin_tog, 5).
prim_op_code(finish_tog, 6).
prim_op_code(inf, 7).
prim_op_code(j_less, 8).
prim_op_code(k_equal, 9).
prim_op_code(lub, 10).
prim_op_code(mul, 11).
prim_op_code(narrower, 12).
prim_op_code(or, 13).
prim_op_code(pow_odd, 14).
prim_op_code(qpow_even, 15).
prim_op_code(rootsquare, 16).
prim_op_code(vabs, 17).
prim_op_code(wrap, 18).
prim_op_code(xp, 19).
prim_op_code(cos, 20).
prim_op_code(sin, 21).
prim_op_code(tan, 22).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%hack as compare on temporary basis:
breadth_first_compare(Left, Right, Op)
	:-
	compare(COp, Left, Right),
	cadj(COp, Op).

cadj( = ,  @= ).
cadj( < ,  @< ).
cadj( > ,  @> ).

/***********************
:-dynamic(it_debug_cq/0).
:-dynamic(it_debug_gl/0).

export '$iterate'/1.

'$iterate'(Goal)
	:-
	Goal =.. [OP | Args],
	prim_op_code(OP,OpCd),
	fixup_iter(Args, Z,X,Y),
	!,
	'$iter_link_net'(OpCd,Z,X,Y,Goal).

/*
fixup_iter([Z,X], Z,X,0).
fixup_iter([Z,X,Y], Z,X,Y).
*/

prim_op_code(unequal, 0).
prim_op_code(equal, 1).
prim_op_code(greatereq, 2).
prim_op_code(higher, 3).
prim_op_code(add, 4).
prim_op_code(begin_tog, 5).
prim_op_code(finish_tog, 6).
prim_op_code(inf, 7).
prim_op_code(j_less, 8).
prim_op_code(k_equal, 9).
prim_op_code(lub, 10).
prim_op_code(mul, 11).
prim_op_code(narrower, 12).
prim_op_code(or, 13).
prim_op_code(pow_odd, 14).
prim_op_code(qpow_even, 15).
prim_op_code(rootsquare, 16).
prim_op_code(vabs, 17).
prim_op_code(wrap, 18).
prim_op_code(xp, 19).
prim_op_code(cos, 20).
prim_op_code(sin, 21).
prim_op_code(tan, 22).
*************************/

endmod.		%% rel_arith
