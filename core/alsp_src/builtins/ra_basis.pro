/*================================================================*
 |		ra_basis.pro
 |	Copyright (c) 1995 Applied Logic Systems, Inc.
 |	Copyright (c) 1995 Bell-Northern Research Ltd.
 |
 |	Builtin predicates for implementing the upper levels
 |	of the interval constraint subsystem in ALS Prolog
 |
 |	Original Author: William J. Older
 |	Original Creation Date: 08/27/92
 |      Relational Interval Arithmetic Subsystem of BNR Prolog V4.x
 |
 |  Revisions/Modifications: Ken Bowen
 |  Date begun: 05/10/95
 |
 | 		Integration into ALS Prolog
 |
 |  Formatting note: Tabstops =4
 *================================================================*/

module rel_arith.

/*
:- op(700,xfx,'::').
:- op(700,xfx,'<>').   
:- op(700,xfx,'@=').   
:- op(600,xfx,'<=').    % subinterval, member, "diode"
:- op(600,xfx, '|=').   % starts together
:- op(600,xfx, '=|').   % ends together
:- op(600,xfx,'=>').    % boolean implication
:- op(150,fx, '~').   	% boolean negation
*/


/*-----------------------------------------------*
 |  clp/1
 |  clp(Goals)
 |  clp(+)
 | 
 | Top-level recursive interpreter/driver 
 | Note: "show" clauses will disappear when 
 | debugging is complete.
 *-----------------------------------------------*/
:- module_closure('{}',1,clp0).
:- module_closure(clp,1,clp0).

clp0(M, X)
	:-
	clp(X, M).

export clp/2.
clp( (L, Ls), M)
	:-
	clp_eval(L, M),
	clp( Ls, M).

clp(G, M)
	:- clp_eval(G, M).

clp_eval( show(VName, Var), M )
	:-
	show_variable(VName, Var).

clp_eval( show(Var), M )
	:-!,
	show_variable(Var).

clp_eval( Expr, M )
	:-
	Expr =.. [R, X, Y],
	clp_arithmetic_relation(R),
	!,
	ria_relation(X, Y, R).

clp_eval( F, M )
	:-
	call(M:F).  % escape to Prolog

clp_arithmetic_relation( is ).
clp_arithmetic_relation( := ).
clp_arithmetic_relation( == ).
clp_arithmetic_relation( =< ).
clp_arithmetic_relation( >= ).
clp_arithmetic_relation( <> ).
clp_arithmetic_relation( >  ).
clp_arithmetic_relation( <  ).
clp_arithmetic_relation( <= ).
/*
clp_arithmetic_relation( '|=' ).
clp_arithmetic_relation( '=|' ).
*/
clp_arithmetic_relation( 'i=' ).
clp_arithmetic_relation( '=i' ).


export show_variable/1.
export show_variable/2.
	%% debugging:
show_variable(VName, Var)
	:-
	('$is_delay_var'(Var) ->
	   '$delay_term_for'(Var, Var_DelayTerm),
	   arg(4, Var_DelayTerm, ConstraintTerm),
	   domain_term_from_constr(ConstraintTerm, DomainTerm),
	   valid_domain(DomainTerm, Type, LArg, UArg),
	   arg(3, DomainTerm, NL),
   
	    printf_opt('\n%t[%t] = %t: [%t, %t]\n', [VName,Var,Type,LArg,UArg],
					[lettervars(false) ,line_length(100)]),
	    printf_opt('\tUsedBy: %t\n',[NL],[lettervars(false),line_length(100)])
	    ;
	    printf_opt('\n%t = %t\n', [VName,Var],[lettervars(false),line_length(100)])
	).

	%% debugging:
show_variable(Var)
	:-
	('$is_delay_var'(Var) ->
	    '$domain'(Var, Type, LB, UB),
	     printf_opt('\n%t = %t: [%t, %t]\n', [Var,Type,LB,UB],
					[lettervars(false),line_length(100)])
	     ;
	     printf_opt('\n%t\n', [Var],[lettervars(false),line_length(100)])
	).


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Symbolic Constants
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Set up pi -- dirty trick;
	%% After this runs, the clause for symbolic_constant(pi,PI) :-...
	%% is replaced by a fact symbolic_constant(pi,PIVal)
	%% with PIval computed to the accuracy of current system.
	%% Formula taken from djgpp mailing list:
	%% Morten Welinder <terra@diku.dk>
	%% Ref to: http://www.diku.dk/~terra/pi.html
	%%-------------------------------
	%% Planned extension/modification:
	%% Add user-extension predicate to control
	%% addition of new constants.
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
symbolic_constant(pi,PI)
	:-
	PI is 16.0*atan(1.0/5.0) - 4.0*atan(1.0/239.0),
	clause(symbolic_constant(pi,_),B,DBRef),
	asserta(symbolic_constant(pi,PI)),
	erase(DBRef).

pi(PI) 
	:-
	symbolic_constant(pi,PI).
	
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Domain Declarations
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*---------------------------------------------------------------
 |	::/2
 |	Vars :: Type
 |	+ :: +
 |
 |  Parameters:
 |	Vars:  Either a single variable or number, or a list of such;
 |	Type:  A type descriptor, as given below;
 |
 |  If Vars is a list, recursively causes the single-entity behavior
 |	to apply to each element of the list;  
 |
 |  If Var is a single uninstatiated Prolog variable: 
 |	causes Var to be (frozen) to a binding to an interval of the correct type;
 |
 |  If the attempt to bind Var (above) fails (possibly after an
 |	unfreezing), calls domain/2 to test the domain X is "frozen" to;
 |
 |  If Var is not a variable, succeeds iff Var is point value of appropriate type.
 |
 |  Acceptable (user-level) type descriptors, where Min,Max are the 
 |	minimal/maximal values of the various types:
 |	
 |		real(L,U)
 |		real [= real(Min,Max)]
 |		integer(L,U)
 |		integer [= integer(Min,Max)]
 |		boolean(L,U)
 |		boolean [= boolean(0,1)]
 *--------------------------------------------------------------*/
export '::'/2.

	%% X is a variable; freeze it to a domain:
X :: Type 
	:- 
	var(X), 
	!,
	declare_variable(Type,X).

	%% Process lists:
[] :: _ .

[X | Xs] :: Type
	:- 
	X  :: Type,
	Xs :: Type.

	%% X is neither a list, nor a variable;
	%% so, check that it is an entity of
	%% the correct type:
	%% -----------
		%% Query: do we want N :: real to succeed
		%% when N is an integer in the correct range)?

X :: real(L,U)		
	:- 
	is_type(real,X),
	L =< X, X =< U,
	!.
	
X :: integer(L,U)	
	:- 
	is_type(integer,X),
	L =< X, X =< U,
	!.

X :: boolean(L,U)	
	:- 
	is_type(boolean,X),
	!.

X :: Type			
	:- 
	is_type(Type,X),!.

is_type(real,X)
	:-
	float(X).

is_type(integer,X)
	:-
	integer(X).

is_type(boolean,0).
is_type(boolean,1).

/*---------------------------------------------------------------
 |	declare_variable/2
 |	declare_variable(Type,X)
 |	declare_variable(+,+)
 |
 |  A.  If X is an uninstatiated Prolog variable which is already
 |		frozen to some delayed goal, creates a new interval
 |		variable XX of type Type, and equates X = XX;
 |  B.  If X is an uninstatiated Prolog variable which is not a frozen
 |      	delay variable, freezes X to an interval domain
 |		structure of type Type.
 |  C.  If X is an instatiated Prolog variable, tests that 
 |		X is frozen to an interval domain structure of type Type.
 *--------------------------------------------------------------*/

/*
declare_variable(Type,X)
	:-
	var(X),
	'$is_delay_var'(X),
	!,
	(restrict_interval(Type,X) ->
		true
		;
		new_type_interval(Type,XX),
		X = XX
	).
*/
declare_variable(Type,X)
	:-
	var(X),
	'$is_delay_var'(X),
	!,
	restrict_interval(Type,X).

declare_variable(Type,X)
	:-
	var(X),
	new_type_interval(Type,X),
	!.

declare_variable(Type,X)
	:-
	domain_check(Type, X).

restrict_interval(Type,X)
	:-
	Type =.. [PrimType, RestL, RestU],
	'$delay_term_for'(X, XDelayTerm),
	arg(4, XDelayTerm, XCT),
	domain_term_from_constr(XCT, XDomTm),
	XDomTm = intvl(XType,_, _, XL,XU),

	(var(RestL) ->
		RestL = XL, NewL = XL
		;
		max(XL, RestL, NewL)
	),
	(var(RestU) ->
		RestU = XU, NewU = XU
		;
		min(XU, RestU, NewU)
	),
	combin_type([PrimType,XType], NewType),
	(XType \= NewType ->
		trailed_mangle(1, XCT, NewType)
		;
		true
	),
	!,
	trailed_mangle(4, XCT, NewL),
	trailed_mangle(5, XCT, NewU).

restrict_interval(Type,X)
	:-
	'$delay_term_for'(X, XDelayTerm),
	arg(4, XDelayTerm, XCT),
	domain_term_from_constr(XCT, XDomTm),
	XDomTm = intvl(Type,_, _, _,_),
	!.


restrict_interval(Type,X)
	:-
	new_type_interval(Type,XX),
	!,
	X = XX.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%           RIA Relations         %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*---------------------------------------------------------------
 |  ria_relation/3
 |  ria_relation( Left, Right, RelationName )
 |  ria_relation( +, +, +)
 | 
 |  Handles instances constraint arithmetic relations
 |
 |  RelationName is the name of a clp relation 
 |  		(see clp_arithmetic_relation/1);
 |
 |  Left, Right are expressions which are the left and right
 |  arguments of the relational statement being processed;
 *--------------------------------------------------------------*/
		%% Left is a variable, with the original relation expression
		%% of the form:
		%%		Left is Expr ; Left := Expr ; Left == Expr
		%%
ria_relation( Left, Expr, Operator )
	:- 
	var(Left),
	is_defining_rel(Operator),
	!, 
	define_interval(Left, Expr).  

		%% All other cases:
ria_relation( Left, Right, Operator )
	:-
	flatten_expr(Left,  LeftExpr,  LeftType),
	flatten_expr(Right, RightExpr, RightType),
	relation_type_match(Operator,  LeftType, RightType),
	!,
	add_relation(Operator, LeftExpr, RightExpr).

is_defining_rel( is ).
is_defining_rel( := ).
is_defining_rel( == ).

define_interval(Y, Expr)
	:-
	flatten_expr(Expr, YE, Type),
	declare_variable(Type,Y),
	add_relation(==, Y, YE).

relation_type_match( <> , T1, T2)
	:-!,
	discrete_type(T1),
	discrete_type(T2).
relation_type_match(_, _, _).

discrete_type(boolean).
discrete_type(integer).

/*---------------------------------------------------------------
 |	add_relation/3
 |	add_relation(Relation, Left, Right)
 |	add_relation(+, +, +)
 | 
 |  Adds a relation instance into the network; checks for 
 |  certain cases which can be eliminated.
 *--------------------------------------------------------------*/
	%% Arguments are identical, and relation is not <>
	%% (All other interval relations are reflexive?)
add_relation(Relation, Left, Right)
	:-
	Left == Right,		%% Identity in ISO Standard Prolog
	!,
	Relation \= ( <> ) .

	%% Both arguments are instantiated Prolog terms, so
	%% try to run the relation as an ordinary Prolog goal:
add_relation(Relation, Left, Right)
	:-
	nonvar(Left),
	nonvar(Right),
	!,
	Goal =.. [Relation, Left, Right],
	Goal.

	%% Otherwise, try to add the relation node into the network:
add_relation(Relation, Left, Right)
	:-
	fmap_rel(Relation, Left, Right, Node),
	!,
	new_node(Node).

/*-----------------------------------------------------------------
 |	flatten_expr/3
 |	flatten_expr(Expr, Value, Type)
 |	flatten_expr(+, -, -)
 |
 |  Performs various symplifications on Expr, to yield Value, and
 |  also computes the Type of Value
 |
 |	Inputs:
 |	Expr	= an arithmetic expression with either variables or numeric 
 |			  constants at leaves
 |	Outputs:
 |	Value	= the result of flattening Expr;
 |	Type	= the type of Value;
 *----------------------------------------------------------------*/

flatten_expr(V, V, Type)
	:-
	var(V),		%% has to be a constraint delay var; look up its type
	!,
	'$domain'(V, Type, _, _).

flatten_expr(C, Val, real) 
	:-
	symbolic_constant(C,Val0),
	!,
	point_interval(Val0, Val).

flatten_expr(N, N1, Type)
	:-
	number(N),
	!,
	is_type(Type,N),
	point_interval(N, N1).

	%% Monadic expressions - special:
flatten_expr(Expr, Y, Type)
	:-
	Expr =.. [Function, X],
	flatten_expr1(Function, X, Y, Type),
	!.

	%% Monadic expressions - general:
flatten_expr(Expr, Res, Type)
	:-
	Expr =.. [Function, X],
	down_type1(Function, Tp, Type),
	flatten_expr(X, XE, Tp),
	type1(Function, Tp, Type),
	add_rel_node(Function, [XE], Res, Type).

	%% Dyadic expressions - special:
flatten_expr(X**0, 1, integer)
	:-!,
	flatten_expr(X, _, _).

flatten_expr(X**1, Res, Type)
	:-!,
	flatten_expr(X, Res, Type).

flatten_expr(X**N, Res, Type)
	:-!,
	N1 is N,
	integer(N1),
	!,
	finish_flatten_power(N1,X,N,Res,Type). 

finish_flatten_power(N1,X,N,Res,Type)
	:-
	N1 < 0,
	!,
	N2 is -N1,
	flatten_expr((1/X)**N2, Res, Type).

finish_flatten_power(N1,X,N,Res,Type)
	:-
	(nonvar(Type) -> type_of_power(N1, PreType, Type) ; true),
	flatten_expr(X, XE, PreType),
	type_of_power(N1, PreType, Type),
	odd_even_power_op(N, Opn),
	add_rel_node(Opn, [XE, N1], Res, Type).

flatten_expr( X<>Y, Res, boolean)
	:-!,
	flatten_expr( '~'((X==Y)), Res, boolean).

flatten_expr( X<Y, Res, boolean)
	:-!,
	flatten_expr( '~'((X=<Y)), Res, boolean).

flatten_expr( X>Y, Res, boolean)
	:-!,
	flatten_expr( '~'((X=>Y)), Res, boolean).

flatten_expr( wrap(X,N), Res, real)
	:-!,
	number(N),
	N1 is N,
	flatten_expr(X, XE, real),
	add_rel_node(wrap, [XE, N1], Res, Type).
	
	%% Dyadic expressions - general:
flatten_expr(Expr, Res, Type)
	:-
	Expr =.. [Function, X, Y],
	Function \= wrap,
	Function \= '**',
	down_type2(Function, T1, T2, Type),
	flatten_expr(X, XE, T1),
	flatten_expr(Y, YE, T2),
	type2(Function, T1, T2, Type),
	normal_form(Function, [XE, YE], NF_Function, NF_Args),
	add_rel_node(NF_Function, NF_Args, Res, Type).

odd_even_power_op(2, root) :-!. 
odd_even_power_op(N, qpow_even) 
	:-
	0 is N mod 2, !.

odd_even_power_op(N, pow_odd).

/*-----------------------------------------------------------------
 |	Flattening special monadic expressions
 *----------------------------------------------------------------*/

flatten_expr1( sin,     X,Y, real ) 
	:- 
	pi(PI),
	trigonometric(sin, X, Y, PI).
flatten_expr1( cos,     X,Y, real ) 
	:- 
	pi(PI),
	trigonometric(cos, X, Y, PI).
flatten_expr1( tan,     X,Y, real ) 
	:- 
	pi(PI),
	P is PI/2,
	trigonometric(tan, X, Y, P).
flatten_expr1( midpoint,X,Y, real) 
	:- 
	'$domain'(X, _, L, U),
	Y is (L + U)/2.0 .
flatten_expr1( delta,   X,Y, real) 
	:- 
	'$domain'(X, _, L, U),
	Y is (U - L) .
flatten_expr1( median,  X,Y, real) 
	:- 
	'$domain'(X, _, L, U),
	intv_median(L,U,Y).
flatten_expr1( float,   X,Y, real) 
	:- 
	numeric(X), 
	Y is float(X).
flatten_expr1( round,   X,Y, integer) 
	:- 
	ground(X),
	!, 
	Y is round(X).
flatten_expr1( round,   X,Y, integer) 
	:- 
	Y::integer, 
	Y - 0.5 =< X, 
	X =< Y + 0.5 .
flatten_expr1( floor,   X,Y, integer) 
	:- 
	numeric(X),
	!, 
	Y is floor(X).
flatten_expr1( floor,   X,Y, integer) 
	:- 
	Y::integer, 
	Y =< X, 
	X < Y + 1  .
flatten_expr1( ceiling, X,Y, integer) 
	:- 
	ground(X),
	!, 
	Y is ceiling(X).
flatten_expr1( ceiling, X,Y, integer) 
	:- 
	Y::integer, 
	Y - 1 < X, 
	X =< Y.

trigonometric( F, X, Res, Period)
	:-
	flatten_expr( X, XE, real),
	add_rel_node( wrap, [XE,Period], Y, real),
	add_rel_node( F, [Y], Res, real).

/*-----------------------------------------------------------------
 |	Type Propagation & Inference
 *----------------------------------------------------------------*/

/*-----------------------------------------------------
 |	type_of_power/3
 |	type_of_power(N, InType, OutType)
 |	type_of_power(+, ?, ?)
 *----------------------------------------------------*/

type_of_power(N, integer, integer)
	:-
	N > 0.
type_of_power(N, _, real).

/*-----------------------------------------------------
 |	type1/3
 |	type1(Op, InType, OutType)
 |	type1(+, ?, ?)
 |		-- other modes??
 *----------------------------------------------------*/

type1( '~',   boolean, boolean) :-!.
type1( -,   integer, integer) :-!.
type1( abs, integer, integer) :-!.
type1( floor,     _, integer) :-!.
type1( ceiling,   _, integer) :-!.
type1( Op,        _, real).

/*-----------------------------------------------------
 |	type2/4
 |	type2(Op, In1, In2, OutType)
 |	type2(+, ?, ?, ?)
 |		-- other modes??
 *----------------------------------------------------*/

type2(or,   boolean, boolean, boolean).
type2(nor,  boolean, boolean, boolean).
type2(and,  boolean, boolean, boolean).
type2(nand, boolean, boolean, boolean).
type2(xor,  boolean, boolean, boolean).
type2( + ,  T1,  T2,     R) :- type2_real(T1, T2, R).
type2( - ,  T1,  T2,     R) :- type2_real(T1, T2, R).
type2( * ,  T1,  T2,     R) :- type2_real(T1, T2, R).
type2(min,  T1,  T2,     R) :- type2_real(T1, T2, R).
type2(max,  T1,  T2,     R) :- type2_real(T1, T2, R).
type2( ; ,  T1,  T2,     R) :- type2_real(T1, T2, R).
type2( == ,  _,  _,      boolean).
type2( >= ,  _,  _,      boolean).
type2( =< ,  _,  _,      boolean).
type2( / ,   _,  _,      real).    % division always produces real 
type2( // ,  _,  _,      integer). % int division always produces int 
type2(divf,  _,  T2,     R) :- type2_integer(T1, T2, R).
type2(divc,  _,  T2,     R) :- type2_integer(T1, T2, R).

type2_real(boolean, boolean, integer) :-!.
type2_real(boolean, integer, integer) :-!.
type2_real(integer, boolean, integer) :-!.
type2_real(integer, integer, integer) :-!.
type2_real(_,       _,       real).

type2_integer(boolean, boolean, integer).
type2_integer(boolean, integer, integer).
type2_integer(integer, boolean, integer).
type2_integer(integer, integer, integer).

	/*-----------------------------------------------------
	 | Down tree rules - for propagating forced boolean/integer 
	 | type down the tree.  These are derived from the rules 
	 | above (but run backwards) when    outgoing type is 
	 | determined or   ~,and, or,.. <> are involved
 	 *----------------------------------------------------*/

down_type1( '~', boolean, boolean):-!.
down_type1( F, T1,  T)
	:- 
	nonvar(T),
	!,
	type1(F,T1,T).  
down_type1( _, _, _).

down_type2(and, boolean, boolean, boolean):-!.
down_type2(nand,boolean, boolean, boolean):-!.
down_type2(or,  boolean, boolean, boolean):-!.
down_type2(nor, boolean, boolean, boolean):-!.
down_type2(xor, boolean, boolean, boolean):-!.
down_type2( F, T1, T2, T)
	:- 
	nonvar(T),
	!,
	type2( F, T1, T2, T).
down_type2( _, _, _, _).

combin_type(TypeList, real)
	:-
	dmember(real, TypeList), !.
combin_type(TypeList, integer)
	:-
	dmember(integer, TypeList), !.
combin_type(TypeList, boolean).

/*-----------------------------------------------------------------
 |	NORMAL FORM
 *----------------------------------------------------------------*/

/*-----------------------------------------------------------------
 |	normal_form/4
 |	normal_form(Function, F_Args, NF_Function, NF_Args)
 |	normal_form(+, +, -, -)
 *----------------------------------------------------------------*/
normal_form( + , [X, Y], * , [X, 2])
	:-
	breadth_first_compare(X, Y, @= ), !.

normal_form( * , [X, Y], root , [X, 2])
	:-
	breadth_first_compare(X, Y, @= ), !.

normal_form( F , [X, Y], F , [Y, X])
	:-
	commutes(F),
	breadth_first_compare(X, Y, @> ), !.

normal_form( =<, [X, Y], >=, [Y, X]) :-!.

normal_form(F, F_Args, F, F_Args).

commutes( + ).
commutes( * ).
commutes( min ).
commutes( max ).
commutes( ; ).
commutes( or ).
commutes( nor ).
commutes( and ).
commutes( nand ).
commutes( == ).
commutes( <> ).
commutes( xor ).

/*-----------------------------------------------------------------
 |	add_rel_node/4
 |	add_rel_node(Function, F_Args, Res, Type)
 |	add_rel_node(+, +, ?, ?)
 |
 |	Strategy: 
 |	1. try to eliminate locally by symbolic rewriting
 |	2. try to locate previously computed expressions (ig enabled)
 |	3. generate new node
 *----------------------------------------------------------------*/

	%% Try symbolic re-writing:
add_rel_node(F, [X,Y], Res, Type)
	:-
	reduce(F, X, Y, Res),
	!.

:-dynamic(global_common_subexpressions_enabled/0).

	%% Look for common subexpr:
add_rel_node(F, F_Args, Res, Type)
	:-
	global_common_subexpressions_enabled,
	find_existing_csx( F, F_Args, Res), 
	!. 

	%% Create new node:
add_rel_node(F, F_Args, Res, Type)
	:-
	Res :: Type,
	eval_riax(F, F_Args, Res ).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Debugging
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

show_node(Node)
	:-
	var(Node),
	'$delay_term_for'(Node, DT),
	!,
	printf_opt('%t : %t\n',[Node,DT],[lettervars(false),line_length(100)]).

/*-----------------------------------------------------------------
 |	reduce/4
 |	reduce(F, X, Y, Res)
 |	reduce(+, +, +, -)
 *----------------------------------------------------------------*/

reduce(F, X, Y, Res)
	:-
	number(X), number(Y),
	atom(F),
	!,
	sym_eval(F, X, Y, Z1),
	point_interval(Z1, Z).

reduce( -  , X, Y, 0) :- X @= Y.      % X - X = 0
reduce( +  , X, Z, X) :- Z @= 0.      % X + 0 = X
reduce( -  , X, Z, X) :- Z @= 0.      % X - 0 = X
reduce( /  , X, Y, 1) :- X @= Y.      % X / X = 1
reduce( // , X, Y, 1) :- X @= Y.      % X // X = 1
reduce( *  , Y, Z, 0) :- Z @= 0.      % Y *  0 = 0
reduce( *  , Y, U, Y) :- U @= 1.      % Y * 1 = Y
reduce( /  , Y, U, Y) :- U @= 1.      % Y / 1 = Y
reduce( // , Y, U, Y) :- U @= 1.      % Y // 1= Y
reduce( min, X, Y, X) :- X @= Y.      % min(X,X)=X
reduce( max, X, Y, X) :- X @= Y.      % max(X,X)=X
reduce( or , X, Z, 1) :- Z @= 1.      % X or 1 = 1
reduce( or , X, Z, X) :- Z @= 0.      % X or 0 = X
reduce( or , X, Y, X) :- X @= Y.      % X or X = X
reduce( and, X, Z, 0) :- Z @= 0.      % X and 0 = 0
reduce( and, X, Z, X) :- Z @= 1.      % X and 1 = X
reduce( and, X, Y, X) :- X @= Y.      % X and X = X
reduce( == , X, Y, B) :- eqop(X,Y,B).
reduce( xor, X, Y, B) :- neqop(X,Y,B).

X @= Y :- X == Y.

eqop(X,Y,1)  :- X@=Y,!.		% same const or same var
eqop(X,Y,0)  				% different constants
	:- 			
	number(X),
	number(Y).
neqop(X,Y,0) :- X@=Y,!.
neqop(X,Y,1) 
	:- 
	number(X),
	number(Y).

/*-----------------------------------------------------------------
 |	find_existing_csx/3
 |	find_existing_csx( F, F_Args, Res)
 |	find_existing_csx( +, +, -)
 |
 |	Searches for a node computing Oper in the interval constraint 
 |	network and returns the proper variable if found; else fails.
 |	(This predicate to be replaced largely by a primitive call.)
 *----------------------------------------------------------------*/

find_existing_csx( F, [X,Y], Z )
	:-
	ria_map2(F, X, Y, Z, G, N ),
	'$findCommonSubex'( G, N ).

find_existing_csx( F, [X], Z )
	:-
	ria_map1(F, X, Z, G, N ),
	!,
	'$findCommonSubex'( G, N ).

find_existing_csx( F, [X], Z )
	:-
	ria_map1r( F, X, Z, G, N, _ ),
	!,
	'$findCommonSubex'( G, N ).

/*-----------------------------------------------------------------
 |	EVALUATING RIA EXPRESSIONS:
 |
 |	Evaluate operations:
 |	- construct constraint network from list of primitive operations
 |	- periodic transcendental functions synthesized
 |	- find first fixed point (incrementally during construction)
 *----------------------------------------------------------------*/
/*-----------------------------------------------------------------
 |	eval_riax/3
 |	eval_riax( F, F_Args, Res )
 |	eval_riax( +, +, - )
 *----------------------------------------------------------------*/

	%% Unrestricted unary:
eval_riax(F, [X], Res )
	:-
	ria_map1(F, X, Res, Node, _ ),
	!,
	new_node(Node).

	%% Unary with restricted range:
eval_riax(F, [X], Res )
	:-
	ria_map1r(F, X, Res, Node, _, Restrict ),
	!,
	call(Restrict),
	new_node(Node).

	%% Unrestricted binary:
eval_riax(F, [X,Y], Res )
	:-
	ria_map2(F, X, Y, Res, Node, _ ),
	!,
	new_node(Node).

/*
eval_riax(Rel, [X,Y], _ )
	:-
	fmap_rel(Rel, X, Y, Node),
	!,
	new_node(Node).
*/

ria_map1(  - , X, Z, add(0,X,Z),    1).
ria_map1( exp, X, Z, xp(Z,X),   1).
ria_map1( ln,  X, Z, xp(X,Z),   2).
ria_map1( abs, X, Z, vabs(Z,X), 1).
ria_map1( sq,  X, Z, root(Z,X), 1).    
ria_map1( sin, X, Z, sin(Z,X),  1).    
ria_map1( cos, X, Z, cos(Z,X),  1).    
ria_map1( tan, X, Z, tan(Z,X),  1).    
ria_map1(  '~' , X, Z, falseimplied(X,Z),2).

ria_map1r(sqrt, X,     Z, root(X,Z),  2,  '$restrict'(Z,0,_) ).
ria_map1r(asin, X,     Z, sin(X,Z),   2,  '$restrict'(Z,-pi/2,pi/2) ).
ria_map1r(acos, X,     Z, cos(X,Z),   2,  '$restrict'(Z,0,pi) ).
ria_map1r(atan, X,     Z, tan(X,Z),   2,  '$restrict'(Z,-pi/2,pi/2) ).

ria_map2( +  ,       X,Y, Z, add(Z,X,Y)  ,1).
ria_map2( *  ,       X,Y, Z, mul(Z,X,Y)  ,1).
ria_map2( min,       X,Y, Z, inf(Z,X,Y)  ,1).
ria_map2( max,       X,Y, Z, lub(Z,X,Y)  ,1).
ria_map2(  - ,       X,Y, Z, add(X,Y,Z)  ,3).
ria_map2(  / ,       X,Y, Z, mul(X,Y,Z)  ,3).
ria_map2( and,       X,Y, Z, conjunction(Z,X,Y) ,1).
ria_map2( or,        X,Y, Z, disjunction(Z,X,Y) ,1).
ria_map2( nand,      X,Y, Z, anynot(Z,X,Y) , 1).
ria_map2( nor,       X,Y, Z, bothnot(Z,X,Y) ,1).
ria_map2( xor,       X,Y, Z, exor(Z,X,Y),1).
ria_map2(  ; ,       X,Y, Z, or(Z,X,Y),   1).
ria_map2( ==,        X,Y, B, k_equal(X,Y,B),3).
ria_map2( >=,        X,Y, B, j_less(X,Y,B), 3).
ria_map2( =<,        Y,X, B, j_less(X,Y,B), 3).
ria_map2( root,      X,N, Z, rootsquare(Z,X),  1).
ria_map2( qpow_even, X,N, Z, qpow_even(Z,X,N), 1).
ria_map2( pow_odd,   X,N, Z, pow_odd(Z,X,N),   1).
ria_map2( wrap,      X,N, Z, wrap(Z,X,N),      1).

fmap_rel( ==,   X,Y, equal(X,Y)).
fmap_rel( >=,   X,Y, greatereq(X,Y)).
fmap_rel( =<,   X,Y, greatereq(Y,X)).
fmap_rel( <,    X,Y, higher(X,Y)).
fmap_rel( >,    X,Y, higher(Y,X)).
fmap_rel( <>,   X,Y, unequal(X,Y)).
fmap_rel( <=,   X,Y, narrower(X,Y)).
/*
fmap_rel( '|=', X,Y, begin_tog(X,Y)).
fmap_rel( '=|', X,Y, finish_tog(X,Y)).
*/
fmap_rel( 'i=', X,Y, begin_tog(X,Y)).
fmap_rel( '=i', X,Y, finish_tog(X,Y)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%        solve
%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*------------------------------------------------------------------------------*
 |	solve/1
 |	solve(Var)
 |	solve(+)
 |
 |	solve/3
 |	solve(Var, Bnd, RelativeErr)
 |	solve(Var, Bnd, RelativeErr)
 |
 |	Var should be a variable which has been frozen to an interval structure;
 |	Bnd = integer; 2^Bnd is the limit on the number of solutions;
 |	RelativeErr = bound for error size;
 |
 |  Note: solve(X) is intended only for cases where a fixed point is a join of
 |  several smaller fix points, e.g. "points".  If a point in the middle of the
 |  interval  ( e.g. median or midpoint) can be found which is not a solution, the
 |  problem can be decomposed into two smaller problems. This is applied recursively
 |  until the interval becomes pointliike, the depth of recursion is exceeded,
 |  or no split point can be found.
 *------------------------------------------------------------------------------*/

export solve/1.
solve(X)
	:- 
	solve(X, 6, 0.0001).			% upto 2**6 solutions

solve( X, _, RelErr )
	:- 
	pointlike(X, RelErr), 			% bound or very small intervals
	!. 

solve( X, Bnd, RelErr ) 
	:- 
	var(X),
	!,
			%% What is this about? (Making sur it's an interval???):
%	$interval_type(X,_), 
	Bnd>=0, 
	'$domain'(X, _, L, U),
	D is U - L,
	min( 1.0e-100,  RelErr * D, Eps),
	i_solve(Bnd, X, Eps).


solve( [], _,  _ ).
solve( List, Bnd, RelErr ) 
	:-
	Bnd > 0,
	extract_intervals( List, Xs ),
	widest( Xs, X ), 
	not( pointlike( X, RelErr) ),
	'$domain'(X, _, L, U),
	D is U - L,
	min( 1.0e-100,  RelErr * D, Eps),
	!,
	multi_solve( Bnd, Xs, Eps).

pointlike( X, _) 
	:- 
	number(X),
	!.

pointlike( X, RelErr) 
	:- 
	'$domain'(X, _, L, U),
	Delta is U - L,
	Midpoint is (U + L)/2,
	Delta < (RelErr * Midpoint).

		%% split if possible:
i_solve(Bnd, X, Eps)
	:- 
	0 < Bnd ,  		
	Bnd1 is Bnd - 1,
	choose_split(X, M, Eps),
	!,
	('$iterate'( greatereq(M, X) ) ; '$iterate'( greatereq(X, M) ) ),
	i_solve(Bnd1, X, Eps).

		%% N=0, too small, or no suitable split point found:
i_solve(Bnd,X,Eps).        

		%% fails if interval is too small or not splittable:
choose_split(X, M, RelErr)
	:-   			
	'$domain'(X, _, L, U),
	median(L, U, M),
	(U - L)  >  RelErr * M,  	% interval is pointlike
		%% Change/Fix (??):
	not(X==M),
	!.

median( L, U, 0  ) :- L<0,     U>0.
median( L, U, 1  ) :- L<1.0,   U>1.0.
median( L, U, -1 ) :- L< -1.0, U> -1.0.
median( L, U,  M ) :- L==0.0,  M is U/3.0 .
median( L, U,  M ) :- U==0.0,  M is L/3.0 .
median( L, U,  M ) :- L>0,     U>0,  M is sqrt(L)*sqrt(U).
median( L, U,  M ) :- L<0,     U<0,  M is -sqrt(-L)*sqrt(-U).
		%% default to midpoint:
median( L, U,  M ) :- M is (L+U)/2.0 .   

multi_solve( 0, List, Eps) :-!.

multi_solve( Bnd, List, Eps)
	:-
	widest(List, X),
	!,
	multi_solve1( List, X, Eps, Bnd ).

multi_solve1( List, X, Eps, Bnd )
	:-
	pointlike( X, Eps ),
	!.

multi_solve1( List, X, Eps, Bnd )
	:-
	Bnd1 is Bnd - 1,
	i_solve( 1, X, Eps ),
	multi_solve( Bnd1, List, Eps ).

widest( [X | Xs], W)
	:-
	widest( Xs, X, W ).

widest( [], W, W ).

widest( [X | Xs], Y, W )
	:-
	'$domain'(X, _, XL, XU),
	DeltaX is XU - XL,
	'$domain'(Y, _, YL, YU),
	DeltaX is YU - YL,
	!,
	widest( Xs, X, W).

widest( [_ | Xs], Y, W )
	:-
	widest( Xs, Y, W ).

enumerate( List )
	:-
	classify_intervals( List, Bs, Ns, Rs ),
		%% do booleans first (smaller domains):
	bool_enumerate( Bs ),   
	int_enumerate(  Ns ),
	real_enumerate( Rs ).

classify_intervals( [], [], [], [] ).

classify_intervals( [X | List], Bs, Ns, Rs )
	:-
	'$domain'( X, Type, LB, UB ),
	!,
	dispatch_classify_intervals(Type, X, List, Bs, Ns, Rs ).

classify_intervals( [_ | List], Bs, Ns, Rs )
	:-
	classify_intervals( List, Bs, Ns, Rs ).

dispatch_classify_intervals(boolean, X, List, [X | Bs], Ns, Rs )
	:-
	classify_intervals( List, Bs, Ns, Rs ).

dispatch_classify_intervals(integer, X, List, Bs, [X | Ns], Rs )
	:-
	classify_intervals( List, Bs, Ns, Rs ).

dispatch_classify_intervals(real, X, List, Bs, Ns, [X | Rs] )
	:-
	classify_intervals( List, Bs, Ns, Rs ).

bool_enumerate([]).
bool_enumerate([ X | Xs ])
	:-
	boolean_generator( X ),
	bool_enumerate( Xs ).

boolean_generator( B )
	:-
	'$iterate'( equal(B, 0), _).

boolean_generator( B )
	:-
	'$iterate'( equal(B, 1), _).


integer_enumerate([]).
integer_enumerate([ X | Xs ])
	:-
	integer_generator( X ),
	integer_enumerate( Xs ).

integer_generator( X )
	:-
	integer(X),
	!.

integer_generator( X )
	:-
	'$domain'( X, Type, LB, _ ),
	L1 is round( LB ),
	int_choice( X, L1 ).
	
int_choice( X, L )
	:-
	'$iterate'( equal(X, L), _).

int_choice( X, L )
	:-
	'$iterate'( unequal(X, L), _),
	integer_generator( X ).

real_enumerate([]).

real_enumerate( X )
	:-
	solve( X ).



/*
enumerate(List, Exec_on_Backtrack)
	:-
	classify_intervals( List, Bs, Ns, Rs ),
		%% do booleans first (smaller domains):
	bool_enumerate_act( Bs, Exec_on_Backtrack ),   
	int_enumerate_act(  Ns, Exec_on_Backtrack ),
	real_enumerate( Rs ).
*/







endmod.
