/*
 * callout1.pro		-- test callouts with garbage compaction
 *
 */

:- extend_is(One,one,(gc,One=1)).

t1(Y) :- garb1, X=2*3, Y is one+X.
t2(Y) :- garb2, X=2*3, Y is one+X.
t3(Y) :- garb1, X=2*3, Y is X+one.
t4(Y) :- garb2, X=2*3, Y is X+one.

garb1 :- _=f(x).
garb2 :- _=f(x,y).
