/*================================================================*
 |              int_cstr.pro
 |      Copyright (c) 1995-96 Applied Logic Systems, Inc.
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

:- dynamic(debug_system_on/1).

export intvl/5.

/*
intvl(Type,Var,UsedBy,UIA,Punch)
	:-
	(Type = boolean ->
		LB = 0, UB = 1
		;
		xtract_bnds_uia(Type, UIA, LB, UB)
	),
	printf('intvl wakeup:type=%t Var=%t lb=%t ub=%t usedby=%t punch=%t\n',
				[Type,Var,LB,UB,UsedBy,Punch]),
%	printf('intvl wakeup:type=%t Var=%t lb=%t ub=%t punch=%t\n',
%				[Type,Var,LB,UB,Punch]),
trace,
	number(Punch),
	(number(Var) ->
		printf('Var is NUMBER: (v=%t p=%t)\n',[Var,Punch]),
		Var =:= Punch
		;
		printf('  - trying: %t =< %t\n',[Var,Punch]),
		{Var =< Punch},
		printf('  - OK(%t =< %t)\n',[Var,Punch]),
		printf('  - Trying: %t >= %t\n',[Var,Punch]),
		( {Var >= Punch} ->
			printf(" -- OK(%t >= %t)\n", [Punch,Var]),
			'$bind_vars'(Var, Punch)
			,printf(" -- =< >= success: bound var (v=%t p=%t)\n", [Var,Punch])
			;
			printf("!!!! =< >= FAILURE: var = %t\n", [Var]),
			fail
		)
	),
	printf('AtEnd:UB=%t\n',[UsedBy]).
*/

intvl(Type,Var,UsedBy,UIA,Punch)
	:-
/*
		xtract_bnds_uia(Type, UIA, LB, UB),
		printf('intvl wakeup:type=%t Var=%t lb=%t ub=%t usedby=%t punch=%t\n',
				[Type,Var,LB,UB,UsedBy,Punch]),
trace,
*/
	number(Punch),
	(number(Var) ->
		Var =:= Punch
		;
		{Var =< Punch},
		( {Var >= Punch} ->
			'$bind_vars'(Var, Punch)
			;
			fail
		)
	).


/*---------------------------------------------------------------
	type_and_bounds/5
	type_and_bounds(TypeDescrip, PrologType, Lower, Upper, TypeCode)
	type_and_bounds(+, -, -, -, -)

	TypeCode: boolean = 0; integer = 1; real = 2; 
 *--------------------------------------------------------------*/
type_and_bounds(real(L,U),    real,    L, U, 2).
type_and_bounds(real,         real,    L, U, 2).
type_and_bounds(real(R),      real,    L, U, 2)
	:-!, 
	float(R),
	fuzz_float(R,L,U).
type_and_bounds(integer(L,U), integer, L, U, 1).
type_and_bounds(integer,      integer, L, U, 1).

type_and_bounds(boolean(L,U), boolean, 0, 1, 0).
type_and_bounds(boolean,      boolean, 0, 1, 0).

/*---------------------------------------------------------------
 |	new_type_interval/2
 |	new_type_interval(TypeDescrip,X)
 |	new_type_interval(+,+)
 |
 |	- makes X into an interval delay var of type Type
 |
 |  This is the only routine which creates interval variables.
 |
 |	The structure of the term bound to X is:
 |
 |		intvl(PrologType,X,UsedBy,UIA,VRef)
 |
 |	where:
 |		type_and_bounds(TypeDescrip, PrologType, L, U, TC) holds
 |		UsedBy initially = [] (will be mangled later)
 |		UIA is 3 8-byte words:
 |			| L | U | TC |
 |
 |		The "correct" values of the endpoints (& the type) are
 |		what is stored in the UIA (for easy access from C); 
 |		these are set when the interval is initially created, and
 |		are typically updated from the C side; whenever an access
 |		is made to the end point values from the Prolog side, then
 |		the Prolog representations (L,U above) are updated from
 |		the values in UIA.
 |		VRef = v(X) where X is the variable of the interval freeze;
 |		This is used to allow a low-level C routine to get at
 |		the actual variable X even when X has been bound; this
 |		occurs when X is bound and the intvl/5 goal is woken up;
 |		we need (at the C level) to in essence add in the constraint
 |			{ X == V }, 
 |		where V is the value to which X has been bound. What we
 |		really do is call '$itbnd'(VRef, X) which is defined in C;
 |		it essentially does an arg(1,...) on VRef to obtain the
 |		pointer P to the original variable X, dereferences X to get
 |		value V, and the uses P,V to effectively add {X==V}.
 *--------------------------------------------------------------*/

uia_space(_,UIA)
	:-
	'$uia_alloc'(24,UIA).

new_type_interval(boolean(L,U),X)
	:-!,
	new_type_interval(boolean,X).

/*
new_type_interval(boolean,X)
	:-!,
	FreezeGoal = intvl(boolean,X, [], 1, 0),
	freeze(X, FreezeGoal).
*/

/*-------------------------------
	 This needs reorganization, so as not to create
	 the values L1, U1 in prolog, and then push them
	 into UIA, but to create UIA first, and then
	 combine the decision-making and creation, keeping
	 things like the creation of ieee-infinity down in C:
 *-------------------------------*/

new_type_interval(TypeDescrip,X)
	:-
	type_and_bounds(TypeDescrip, BareType, L, U, TC),
	interval_bound(lower,L,L1,BareType),
	interval_bound(upper,U,U1,BareType),
	uia_space(BareType,UIA),
%	'$uia_poked'(UIA,0,L1),
%	'$uia_poked'(UIA,8,U1),
	uia_poke_float(L1,UIA,0),
	uia_poke_float(U1,UIA,8),
	'$uia_pokel'(UIA,16,TC),
	FreezeGoal = intvl(BareType, X,[],UIA, 0),
	freeze(X, FreezeGoal).

uia_poke_float(-V,UIA,Off)
	:-!,
	uia_poke_float(V,-1,UIA,Off).

uia_poke_float(V,UIA,Off)
	:-
	uia_poke_float(V,1,UIA,Off).

uia_poke_float(V,Sgn,UIA,Off)
	:-
	number(V),
	!,
	NV is V*Sgn,
	'$uia_poked'(UIA,Off,NV).

uia_poke_float(V,Sgn,UIA,Off)
	:-
	is_symbolic_constant(V),
	uia_poke_fpconst(V,Sgn,UIA,Off).


	%% NEED::: DEAL WITH BOOLEANS:
new_combined_interval(TypeDescrip, X)
	:-
pbi_write(new_combined_interval(TypeDescrip)),pbi_nl,pbi_ttyflush,
	type_and_bounds(TypeDescrip, BareType, L, U, TC),
	interval_bound(lower,L,L1,BareType),
	interval_bound(upper,U,U1,BareType),
	uia_space(BareType,UIA),
	'$uia_poked'(UIA,0,L1),
	'$uia_poked'(UIA,8,U1),
	'$uia_pokel'(UIA,16,TC),
	freeze_goal_for(BareType, X, L1, U1, UIA,TypeFreezeGoal),
	'$delay_term_for'(X, DelayTerm),
	arg(4, DelayTerm, OrigDomainTerm),
	trailed_mangle(4, DelayTerm, (OrigDomainTerm, TypeFreezeGoal)).


/*---------------------------------------------------------------
 |	Accessing and destructive changes to the components of an
 |	interval structure :
 |
 |		intvl(Type, TheVar, UsedByList, UIA, VRef)
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
	arg(4, Intvl, UIA),
	update_uia_dbl(Intvl,0,NewVal). %% 0 = lower

update_upper_bd(Intvl, NewVal)
	:-
	arg(4, Intvl, UIA),
	update_uia_dbl(Intvl,1,NewVal). %% 1 = upper

access_used_by(Intvl, NewVal)
	:-
	arg(3, Intvl, NewVal).

update_used_by(Intvl, NewVal)
	:-
	trailed_mangle(3, Intvl, NewVal).

/*---------------------------------------------------------------
	update_uia_dbl(Intvl,0,NewVal). %% 0 = lower
	update_uia_dbl(Intvl,1,NewVal). %% 1 = upper
 *--------------------------------------------------------------*/

update_uia_dbl(Intvl,Which,NewVal)
	:-
	arg(4, Intvl, OldUIA),
	arg(1, Intvl, Type),
	type_and_bounds(Type, BareType, _, _, _),
	uia_space(BareType,NewUIA),

	'$uia_peekd'(OldUIA,0,OldL),
	'$uia_peekd'(OldUIA,8,OldU),
	'$uia_peekl'(OldUIA,16,OldTC),

	(Which = 0 ->
		'$uia_poked'(NewUIA,0,NewVal),
		'$uia_poked'(NewUIA,8,OldU)
		;
		'$uia_poked'(NewUIA,0,OldL),
		'$uia_poked'(NewUIA,8,NewVal)
	),
	'$uia_pokel'(NewUIA,16,OldTC),
	trailed_mangle(4, Intvl, NewUIA).


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

/*---------------------------------------------------------------
	%%% INFINITE BOUNDS handled with ieee infinity
 *--------------------------------------------------------------*/


#if (syscfg:ieee_fp)
interval_bound(Side,  Quant, Bound, Type)
	:-
	var(Quant),
	!,
	Quant = Bound,
	(Side = lower -> Bound = -0i ; Bound = 0i).

interval_bound(_,Quant, Bound, _)
	:-
	symbolic_constant(Quant, Bound),
	!.

interval_bound(_,Quant, Bound, _)
	:-
	Bound is float(Quant).

#else
interval_bound(Side,  Quant, Bound, Type)
	:-
	var(Quant),
	!,
	max_bound(Type, MaxBound),
	interval_bound_decide(Side, MaxBound, Bound).

interval_bound(_,Quant, Bound, _)
	:-
	symbolic_constant(Quant, Bound),
	!.

interval_bound(_,Quant, Quant, boolean)
	:-!.

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
			%% maxint(X),
	current_prolog_flag(max_integer, X), 
	Y is float(X).
#endif


/*---------------------------------------------------------------
	%%% INFINITE BOUNDS:
	Note that when type was originally "bare" - ie, of the form
			X::real or X::integer,
	an infinite interval was intended, and Quant remains 
	uninstantiated here; so we can instantiate it as we want in 
	clause #1; it is also possible that an infinite interval
	be fully specified, as follows, where <type> is "real" or
	"integer":
		X::<type>('-inf', '+inf')
		X::<type>(inf, inf)
		X::<type>(inf, '+inf')
		X::<type>('-inf', inf)

 I've left the existing code alone above, and created a version
 for the new code in comments below.

	This effectively inserts ieee infinity into the UIAs
 *--------------------------------------------------------------*/

%%%% First, replace type_and_bounds/5 above (~line 46) by this:
/*-----
type_and_bounds(real,         real,    '-inf', '+inf', 2).

type_and_bounds(real(L,U),    real,    CorrL,  CorrU,  2)
	:-
	corrected_sym(L, lower, CorrL),
	corrected_sym(U, upper, CorrU).

type_and_bounds(real(inf),    real,    '-inf', '+inf', 2)
	:-!.
type_and_bounds(real(R),      real,    L, U, 2)
	:-!, 
	float(R),
	fuzz_float(R,L,U).

type_and_bounds(integer,      integer, '-inf', '+inf', 2).

type_and_bounds(integer(L,U), integer, CorrL,  CorrU,  2)
	:-
	corrected_sym(L, lower, CorrL),
	corrected_sym(U, upper, CorrU).

type_and_bounds(integer(inf), integer, '-inf', '+inf', 2)
	:-!.

%% corrected_sym(Descrip, Side, CorrectedDescrip).

corrected_sym('inf', lower, '-inf') :-!.
corrected_sym('inf', upper, '+inf') :-!.
corrected_sym('-inf', lower, '-inf') :-!.
corrected_sym('+inf', upper, '+inf') :-!.
corrected_sym(Desc,  _,  Desc)
	:-
	number(Desc).
 -----*/
/*-----
	%% interval_bound(Side,ValDescrip,InternalVal, BareType)
		%% Now we can assume that ValDescrip is instantiated, either to
		%% a float, integer,, or to one of the symbols '-inf', '+inf':

interval_bound(Side,ValDescrip,InternalVal, BareType)

interval_bound(lower,'-inf',InternalVal, _)
	:-!,
	ieee_infinity(0, InternalVal).

interval_bound(upper,'+inf',InternalVal, _)
	:-!,
	ieee_infinity(1, InternalVal).

interval_bound(_,ValDescrip, Bound, _)
	:-
	Bound is float(ValDescrip).
 -----*/


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

/* REDUNDANT ??
valid_domain(( A, B ), Type, LB, UB)
	:-
	valid_domain( A, Type, LB, UB).
*/

valid_domain(Intrv, Type, LB, UB)
	:-
	functor(Intrv,intvl,5),
	arg(1,Intrv,Type),
	(Type = boolean ->
		LB = 0, UB = 1
		;
		arg(4,Intrv,UIA),
		xtract_bnds_uia(Type, UIA, LB, UB)
	).

/*
		(Type = real ->
			'$uia_peekd'(UIA,0,LB),
			'$uia_peekd'(UIA,8,UB)
			;
			'$uia_peekd'(UIA,0,ILB), LB is floor(ILB),
			'$uia_peekd'(UIA,8,IUB), UB is floor(IUB)
		)
	).
*/

xtract_bnds_uia(real, UIA, LB, UB)
	:-!,
	'$uia_peekd'(UIA,0,LB),
	'$uia_peekd'(UIA,8,UB).

xtract_bnds_uia(Type, UIA, LB, UB)
	:-
	'$uia_peekd'(UIA,0,ILB), LB is floor(ILB),
	'$uia_peekd'(UIA,8,IUB), UB is floor(IUB).

%%%%%%%%%%%%% NODE BUILDING%%%%%%%%%%%%%%%

'$restrict'(X, L, H)
	:-
	Y::real(L,H),
	'$iterate'(equal(X,Y,0)).

point_interval( N,  N)
	:-
	integer(N), !.

point_interval( 0.0,0.0)          % dont fuzz 0.0
	:- !.

point_interval(X, PX)
	:-
	fuzz_float(X, XL, XH),
	PX::real(XL,XH).

fuzz_float(-X, XL, XH)
	:-!,
	fuzz_float(X, -1, XL, XH).

fuzz_float(X, XL, XH)
	:-
	fuzz_float(X, 1, XL, XH).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export '$iterate'/1.
'$iterate'(Goal)
	:-
		%% Put #ifdef wrapper corresponding to DEBUGSYS around this:
	(debug_system_on(cstr_ig) ->
		printf_opt('Goal= %t\n',[Goal], [lettervars(false) ,line_length(100)])
		; true),
	Goal =.. [OP | Args],
	prim_op_code(OP,OpCd),

	XFGoal =.. [pop, OpCd,Z,X,Y,LinkArg],
	fixup_iter(Args, Z,X,Y,XFGoal),
	(debug_system_on(cstr_isv) ->
		exhibit_var('Z', Z),
		exhibit_var('X', X),
		exhibit_var('Y', Y),
		nl
		; true),

		%% Put #ifdef wrapper corresponding to DEBUGSYS around this:
%printf_opt('>>%t--XFGoal= %t\n',[Goal,XFGoal], [lettervars(false) ,line_length(100)]),
%show_used_by(['Z'-Z,'X'-X,'Y'-Y]),
	!,
	'$iter_link_net'(OpCd,Z,X,Y,XFGoal).

fixup_iter([Z,X], Z,X,0,XFGoal)
	:-
	add_to_used_by([Z,X], XFGoal).
		
fixup_iter([Z,X,Y], Z,X,Y,XFGoal)
	:-
	add_to_used_by([Z,X,Y], XFGoal).

prim_op_code(unequal,		0).
prim_op_code(equal,			1).
prim_op_code(greatereq,		2).
prim_op_code(higher,		3).
prim_op_code(add,			4).
prim_op_code(begin_tog, 	5).
prim_op_code(finish_tog,	6).
prim_op_code(inf,			7).
prim_op_code(j_less,		8).
prim_op_code(k_equal,		9).
prim_op_code(lub,			10).
prim_op_code(mul,			11).
prim_op_code(narrower,		12).
prim_op_code(or,			13).
prim_op_code(pow_odd,		14).
prim_op_code(qpow_even,		15).
prim_op_code(rootsquare,	16).
prim_op_code(vabs,			17).
prim_op_code(wrap,			18).
prim_op_code(xp,			19).
prim_op_code(cos,			20).
prim_op_code(sin,			21).
prim_op_code(tan,			22).
	%% Booleans:
prim_op_code(conjunction,	23).
prim_op_code(disjunction,	24).
prim_op_code(anynot,		25).
prim_op_code(bothnot,		26).
prim_op_code(exor,			27).
prim_op_code(negation,		28).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%hack as compare on temporary basis:
breadth_first_compare(Left, Right, Op)
	:-
	compare(COp, Left, Right),
	cadj(COp, Op).

cadj( = ,  @= ).
cadj( < ,  @< ).
cadj( > ,  @> ).

endmod.		%% rel_arith
