/*
 * aexp.pro		-- expression parser
 *	Copyright (c) 1990-91 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 4/6/90
 *
 * Revision History:
 *	Revised:	5/7//91, kev,	added expression_additive/3 for
 *					88k implementation.
 *
 * Module Name:
 *	expressions
 *
 * Exported Procedures:
 *	expression/3	-- The first argument is the return value of the
 *			   expression.  The second and third arguments are
 *			   the list values typical of DCG rules.
 *
 *			   The expression will be evaluated as much as
 *			   possible.
 *
 *	expression_additive/3
 *			-- similar to expression/3, but only handles 
 *			   plus, minus, and lower level operators at top
 *			   level.  This permits expressions involving
 *			   angle brackets to be written if necessary.
 *			   Angle brackets can be confused with shift and
 *			   comparison operators.
 */

module expressions.

export expression/3.
export expression_additive/3.

expression(Val) --> e12(Val).

expression_additive(Val) --> e3(Val).

/*
 * e12	-- conditional expression
 */

e12(Val) -->	e11(V1), e12s(Val,V1).

e12s(Val,V1) -->
		[question], !, e11(V2), [colon], e11(V3),
		{eval_conditional(V1,V2,V3,Val)}.
e12s(V,V) -->	[].

eval_conditional(0,_,V,V) :- !.
eval_conditional(N,V,_,V) :- number(N), !.
eval_conditional(V1,V2,V3,conditional(V1,V2,V3)).

/*
 * e11	-- logical or
 */

e11(Val) -->	e10(V1), e11s(Val,V1).

e11s(Val,V1) --> [land], !, e11(V2), {eval_lor(V1,V2,V3)}, e11s(Val,V3).
e11s(V,V) -->	[].

eval_lor(0,0,0) :- !.
eval_lor(N1,_,1) :- number(N1), !.
eval_lor(_,N2,1) :- number(N2), !.
eval_lor(N1,N2,lor(N1,N2)).

/*
 * e10	-- logical and
 */

e10(Val) -->	e9(V1),	e10s(Val,V1).

e10s(Val,V1) --> [land], !, e10(V2), {eval_land(V1,V2,V3)}, e10s(Val,V3).
e10s(V,V) -->	[].

eval_land(0,_,0) :- !.
eval_land(_,0,0) :- !.
eval_land(N1,N2,1) :- number(N1),number(N2), !.
eval_land(N1,N2,land(N1,N2)).


/*
 * e9	-- bitwise inclusive or
 */

e9(Val) -->	e8(V1),	e9s(Val,V1).

e9s(Val,V1) -->	[or], !, e8(V2), {eval_or(V1,V2,V3)}, e9s(Val,V3).
e9s(V,V) -->	[].

eval_or(N1,N2,V) :- number(N1), number(N2), V is N1 \/ N2, !.
eval_or(N1,N2,or(N1,N2)).

/*
 * e8	-- bitwise exclusive or
 */

e8(Val) -->	e7(V1),	e8s(Val,V1).

e8s(Val,V1) -->	[carat], !, e7(V2), {eval_xor(V1,V2,V3)}, e8s(Val,V3).
e8s(V,V) -->	[].

eval_xor(N1,N2,V) :- number(N1), number(N2), V is N1 xor N2, !.
eval_xor(N1,N2,xor(N1,N2)).

/*
 * e7	-- bitwise and
 */

e7(Val) -->	e6(V1),	e7s(Val,V1).

e7s(Val,V1) -->	[and], !, e6(V2), {eval_and(V1,V2,V3)}, e7s(Val,V3).
e7s(V,V) -->	[].

eval_and(N1,N2,V) :- number(N1), number(N2), V is N1 /\ N2, !.
eval_and(N1,N2,and(N1,N2)).



/*
 * e6	-- equality operators
 */

e6(Val) -->	e5(V1), e6s(Val,V1).

e6s(Val,V1) -->	[EqOp], {isEqOp(EqOp)}, !,
		e5(V2), {e6s_eval(EqOp,V1,V2,V3)}, e6s(Val,V3).
e6s(V,V) -->	[].

isEqOp(equalEqual).
isEqOp(notequal).

e6s_eval(equalEqual,N1,N2,V) :- number(N1), number(N2), !, 
				eval_Equal(N1,N2,V).
e6s_eval(equalEqual,N1,N2,equal(N1,N2)) :- !.
e6s_eval(notequal,N1,N2,V) :-	number(N1), number(N2), !,
				eval_notEqual(N1,N2,V).
e6s_eval(notequal,N1,N2,not_equal(N1,N2)).

eval_Equal(N,N,1) :- !.
eval_Equal(_,_,0).

eval_notEqual(N,N,0) :- !.
eval_notEqual(_,_,1).

/*
 * e5	-- relational operators
 */

e5(Val) -->	e4(V1), e5s(Val,V1).

e5s(Val,V1) -->	[RelOp], {isRelOp(RelOp)}, !,
		e4(V2), {e5s_eval(RelOp,V1,V2,V3)}, e5s(Val,V3).
e5s(V,V) -->	[].

isRelOp(langle).
isRelOp(rangle).
isRelOp(lessOrEqual).
isRelOp(greaterOrEqual).

e5s_eval(langle,N1,N2,V) :- number(N1), number(N2), !, eval_less(N1,N2,V).
e5s_eval(langle,N1,N2,N1<N2) :- !.
e5s_eval(rangle,N1,N2,V) :- number(N1), number(N2), !, eval_less(N2,N1,V).
e5s_eval(rangle,N1,N2,N1>N2) :- !.
e5s_eval(lessOrEqual,N1,N2,V) :- number(N1), number(N2),
				 !, eval_lessOrEqual(N1,N2,V).
e5s_eval(lessOrEqual,N1,N2,N1=<N2) :- !.
e5s_eval(greaterOrEqual,N1,N2,V) :- number(N1), number(N2),
				    !, eval_lessOrEqual(N2,N1,V).
e5s_eval(greaterOrEqual,N1,N2,N1>=N2).

eval_less(N1,N2,1) :- N1 < N2,!.
eval_less(N1,N2,0).

eval_lessOrEqual(N1,N2,1) :- N1 =< N2, !.
eval_less(N1,N2,0).

/*
 * e4	-- shift level operators
 */

e4(Val) -->	e3(V1), e4s(Val,V1).

e4s(Val,V1) -->	[ShiftOp], {isShiftOp(ShiftOp)}, !,
		e3(V2), {e4s_eval(ShiftOp,V1,V2,V3)}, e4s(Val,V3).
e4s(V,V) -->	[].

isShiftOp(leftshift).
isShiftOp(rightshift).

e4s_eval(leftshift,N1,N2,V) :- number(N1), number(N2), V is N1<<N2, !.
e4s_eval(leftshift,N1,N2,N1<<N2) :- !.
e4s_eval(rightshift,N1,N2,V) :- number(N1), number(N2), V is N1>>N2, !.
e4s_eval(rightshift,N1,N2,N1>>N2).

/*
 * e3	-- additive level operators
 */

e3(Val) -->	e2(V1), e3s(Val,V1).

e3s(Val,V1) -->	[AddOp], {isAddOp(AddOp)}, !,
		e2(V2), {e3s_eval(AddOp,V1,V2,V3)}, e3s(Val,V3).
e3s(V,V) -->	[].

isAddOp(plus).
isAddOp(minus).

e3s_eval(plus,N1,N2,V) :- number(N1), number(N2), V is N1+N2, !.
e3s_eval(plus,N1,N2,N1+N2) :- !.
e3s_eval(minus,N1,N2,V) :- number(N1), number(N2), V is N1-N2, !.
e3s_eval(minus,N1,N2,N1-N2).

/*
 * e2	-- multiplicative level operators
 */

e2(Val) -->	e1(V1), e2s(Val,V1).

e2s(Val,V1) --> [MulOp], {isMulOp(MulOp)}, !, 
		e1(V2), {e2s_eval(MulOp,V1,V2,V3)}, e2s(Val,V3).
e2s(V,V) -->	[].

isMulOp(star).
isMulOp(slash).
isMulOp(percent).

e2s_eval(star,N1,N2,V) :- number(N1), number(N2), V is N1*N2, !.
e2s_eval(star,N1,N2,N1*N2) :- !.
e2s_eval(slash,N1,N2,V) :- number(N1), number(N2), V is N1/N2,!.
e2s_eval(slash,N1,N2,N1/N2) :- !.
e2s_eval(percent,N1,N2,V) :- number(N1),number(N2), V is N1 mod N2, !.
e2s_eval(percent,N1,N2,mod(N1,N2)).

/*
 * e1	-- unary operators
 */

e1(Val) --> [UnOp], {isUnary(UnOp)}, !, e0(E), {e1_eval(UnOp,E,Val)}.
e1(Val) --> e0(Val).

isUnary(minus).
isUnary(lnot).
isUnary(not).

e1_eval(minus,N,Val) :- number(N), Val is -N, !.
e1_eval(minus,E,-E).
e1_eval(lnot,E,Val) :- !, eval_lnot(E,Val).
e1_eval(not,N,Val) :- number(N), Val is \(N), !.
e1_eval(not,N,not(N)).

/*
 * e0	-- identifiers, numbers, and parenthesized expressions.
 */

e0(Id) --> [ident(Id)], !.
e0(Num) --> [number(Num)], !.
e0(Exp) --> [lparen], expression(Exp), [rparen].



eval_lnot(0,1) :- !.
eval_lnot(N,0) :- number(N), !.
eval_lnot(E,lnot(E)).

endmod.
