/*=========================================================
 |				blt_is.pro		
 |	Copyright (c) 1991-1996 Applied Logic Systems, Inc.
 |
 |			- extensible version of is/2
 |
 | Author: Kevin A. Buettner
 | Creation: 4/11/91
 | Revision History:
 *========================================================*/

module builtins.

export is/2.

Result is Expression :- is_eval(Expression,Result), !.

	/* fail on variables */
is_eval(Var, Res) :- var(Var), !, instantiation_error(builtins:(Res is Var)).
	/* numbers evaluate to themsevles */
is_eval(Num,Num) :- number(Num).
	/* binary operators */
is_eval(X+Y, Res)	:- Res is X+Y.
is_eval(X-Y, Res) 	:- Res is X-Y.
is_eval(X*Y, Res) 	:- Res is X*Y.
is_eval(X/Y, Res) 	:- Res is X/Y.
%is_eval(X^Y, Res) 	:- Res is X^Y.
is_eval(X /\ Y, Res)	:- Res is X/\Y.
is_eval(X \/ Y, Res)	:- Res is X\/Y.
is_eval(X xor Y, Res)	:- Res is X xor Y.
is_eval(X << Y, Res)	:- Res is X<<Y.
is_eval(X >> Y, Res)	:- Res is X>>Y.
is_eval(X // Y, Res)	:- Res is X//Y.
is_eval(X div Y, Res)	:- Res is X div Y.
is_eval(X mod Y, Res)	:- Res is X mod Y.
is_eval(atan2(X,Y), Res):- Res is atan2(X,Y).
is_eval(fmod(X,Y), Res) :- Res is fmod(X,Y).
is_eval(hypot(X,Y), Res):- Res is hypot(X,Y).
is_eval(jn(X,Y), Res)	:- Res is jn(X,Y).
is_eval(yn(X,Y), Res)	:- Res is yn(X,Y).
	/* unary operators */
is_eval(+X, Res)	:- Res is X.
is_eval(-X, Res)	:- Res is -X.
is_eval(\(X), Res)	:- Res is \(X).
is_eval(not(X), Res)	:- Res is not(X).
is_eval(exp(X), Res)	:- Res is exp(X).
is_eval(exp10(X), Res)	:- Res is exp10(X).
is_eval(log(X), Res)	:- Res is log(X).
is_eval(log10(X), Res)	:- Res is log10(X).
is_eval(sin(X), Res)	:- Res is sin(X).
is_eval(cos(X), Res)	:- Res is cos(X).
is_eval(tan(X), Res)	:- Res is tan(X).
is_eval(asin(X), Res)	:- Res is asin(X).
is_eval(acos(X), Res)	:- Res is acos(X).
is_eval(atan(X), Res)	:- Res is atan(X).
is_eval(round(X), Res)	:- Res is round(X).
is_eval(trunc(X), Res)	:- Res is trunc(X).
is_eval(floor(X), Res)	:- Res is floor(X).
is_eval(sqrt(X), Res)	:- Res is sqrt(X).
is_eval(abs(X), Res)	:- Res is abs(X).
is_eval(ceil(X), Res)	:- Res is ceil(X).
is_eval(cosh(X), Res)	:- Res is cosh(X).
is_eval(erf(X), Res)	:- Res is erf(X).
is_eval(erfc(X), Res)	:- Res is erfc(X).
is_eval(gamma(X), Res)	:- Res is gamma(X).
is_eval(j0(X), Res)	:- Res is j0(X).
is_eval(j1(X), Res)	:- Res is j1(X).
is_eval(sinh(X), Res)	:- Res is sinh(X).
is_eval(tanh(X), Res)	:- Res is tanh(X).
is_eval(y0(X), Res)	:- Res is y0(X).
is_eval(y1(X), Res)	:- Res is y1(X).
	/* symbols */
is_eval(heapused, Res)	:- Res is heapused.
is_eval(cputime, Res)	:- Res is cputime.
is_eval(realtime, Res)	:- Res is realtime.
is_eval(random, Res)	:- Res is random.
	/* extensions are asserted here */


export '<'/2, '>'/2, '=<'/2, '>='/2, '=:='/2, '=\='/2.

X < Y :- X < Y.
X > Y :- X > Y.
X =< Y :- X =< Y.
X >= Y :- X >= Y.
X =:= Y :- X =:= Y.
X =\= Y :- X =\= Y.



export extend_is/3.

extend_is(Result, Expression, EvalBody) :-
	reconsult_assertz_at_load_time((is_eval(Expression,Result) :- EvalBody)).


export unextend_is/3.

unextend_is(Result, Expression, EvalBody) :-
	retract((is_eval(Expression,Result) :- EvalBody)),
	!.


endmod.
