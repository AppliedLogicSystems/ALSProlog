

/*
 * diff.pro  -- a symbolic differentiator
 *
 * Copyright (c) 1986, 1988 by Applied Logic Systems
 *
 * Author:  Keith Hughes
 * Revision History: 
 *      mm/dd/yy,       Name	--	Reason
 *      mm/dd/yy,       Name	--	Reason
 */


/*	
 *	Description:
 *
 *      Predicate:      diff
 *      Normal usage:   diff
 *
 *      Function:       Reads an expression, and prints the result after
 *                      performing differentiation on it.
 *               
 *                      The Expression has the form
 * 
 *                      X:Equation
 *
 *                      where Equation is an expression with unknown X.
 *                      eg. x:(sin(cos(x))-cos(sin(x)).
 *
 *                      Enter stop to exit program.
 *                     
 */


:- op(700,xfx,:).

diff :-
   write('Expression: '),
   read(T),
   diff(T).

diff(stop) :- !.
diff(X:Expr) :-
   diff(X,Expr,T),!,
   simplify(T,Ans),
   write(Ans),nl,nl,
   diff.
diff(_) :-
   write('Illegal expression. Type  stop  to exit.'),nl,
   diff.

diff(X,X,1) :- !.
diff(_,Y,0) :-
   atomic(Y),!.
diff(X,U/V,(V*DU-U*DV)/(V^2)) :- !,
   diff(X,U,DU),
   diff(X,V,DV).
diff(X,Z*Y,DZ*Y+Z*DY) :- !,
   diff(X,Z,DZ),
   diff(X,Y,DY).
diff(X,Y+Z,DY+DZ) :- !,
   diff(X,Y,DY),
   diff(X,Z,DZ).
diff(X,Y-Z,DY-DZ) :- !,
   diff(X,Y,DY),
   diff(X,Z,DZ).
diff(X,-Y,-DY) :- !,
   diff(X,Y,DY).
diff(X,ln(Y),DY/Y) :- !,
   diff(X,Y,DY).
diff(X,cos(Y),-sin(Y)*DY) :- !,
   diff(X,Y,DY).
diff(X,sin(Y),cos(Y)*DY) :- !,
   diff(X,Y,DY).
diff(X,tan(Y),DY/(cos(Y)^2)) :- !,
   diff(X,Y,DY).

% simplify an expression to a very limited extent

simplify(A*B,C) :- !,
   simplify(A,ASimp),
   simplify(B,BSimp),
   mult(ASimp,BSimp,C).
simplify(A+B,C) :- !,
   simplify(A,ASimp),
   simplify(B,BSimp),
   add(ASimp,BSimp,C).
simplify(A-B,C) :- !,
   simplify(A,ASimp),
   simplify(B,BSimp),
   sub(ASimp,BSimp,C).
simplify(A/B,C) :- !,
   simplify(A,ASimp),
   simplify(B,BSimp),
   div(ASimp,BSimp,C).
simplify(A^B,C) :- !,
   simplify(A,ASimp),
   simplify(B,BSimp),
   power(ASimp,BSimp,C).
simplify(-A,B) :- !,
   simplify(A,C),
   neg(C,B).
simplify(A,A) :-
   atomic(A),!.
simplify(A,B) :-
   functor(A,Functor,Arity),
   functor(B,Functor,Arity),
   simplify(0,Arity,A,B).

simplify(E,E,_,_) :- !.
simplify(I,E,A,B) :-
   J is I+1,
   arg(J,A,AArg),
   arg(J,B,BArg),
   simplify(AArg,BArg),
   simplify(J,E,A,B).

add(0,U,U) :- !.
add(U,0,U) :- !.
add(A,B,C) :-
   number(A),number(B),!,
   C is A+B.
add(U,V,U+V).

sub(U,0,U) :- !.
sub(U,U,0) :- !.
sub(U,V,W) :-
   number(U),number(V),!,
   W is U-V.
sub(U,V,U-V).

mult(0,_,0) :- !.
mult(_,0,0) :- !.
mult(1,U,U) :- !.
mult(U,1,U) :- !.
mult(A,B,C) :-
   number(A),number(B),!,
   C is A*B.
mult(U,V,U*V).

div(0,V,0) :- !.
div(V,1,V) :- !.
div(U,V,W) :-
   number(U),number(V),!,
   W is U/V.
div(U,V,U/V).

neg(0,0) :- !.
neg(A,B) :-
   number(A),!,
   B is -A.
neg(-U,U) :- !.
neg(U,-U).

power(U,1,U) :- !.
power(U,0,1) :-
   U =:= 0,!.
power(U,P,A) :-
   number(U),number(P),!,
   A is U^P.
power(U^P,N,U^A) :- !,
   mult(N,P,A).
power(U,P,U^P).
