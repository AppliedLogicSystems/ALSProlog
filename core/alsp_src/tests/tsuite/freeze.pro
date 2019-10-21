:-[test].


test_freeze :- test([

	% Test freeze goal binding
	( freeze(V, length(V, L)), append("ab", "c", Q), V = Q, L == 3 ),
	( freeze(V, length(V, L)), V = [a,b,c], L == 3 ),

	test_gc,

	f_4,
	f_5,
	f_6,
	f_7,
	f_8,
	f_9,
	f_10,
	f_11,

	% Test freeze goal success/failure
	( freeze(V, true), V=a ),
	not(( freeze(V, fail), V=a )),

	true
]).

test_gc :- test([
	( freeze(S, produce2(0,S)), consume2(S) ),
	not((freeze(V, V=hi), gc, V==hello))
]).

produce2(N, [N | T])
        :-
        M is N+1,
        freeze(T, produce2(M,T)).

consume2([N | T])
        :-
        (0 is N mod 3 -> gc; true),
        (N < 20 ->
                consume2(T) 
                ; 
                N == 20).

f_4 :- j(X, Y), Y==9.

j(X,Y) :- freeze(W1, frump(W1,yum)), s0(W1).

j(X,9).

s0(W1)  :- W1 =def.

frump(A, A).


f_5 :-  k0(X),
        X == jj(foo,bar).

k0(jj(W1,W2)) :-
        freeze(W1,grump(W1,ym)),
        freeze(W2,silly(W2,zippo)),
       t01(W1,W2).

t01(W1,W2)
        :-
        [W1,W2]=[foo,bar].

grump(A,B).

silly(A,B).


f_6 :-  freeze(W1, silly(W1,yellow)),
        f(g(h(W1))) = f(g(h(hi88))),
        W1 == hi88.


f_7 :-  freeze(X, freeze(Y, foobar(X,Y))),
        g2a(X,Y),
        X == igloo,
        Y == inuit.

foobar(X,Y).

g2a(X,Y)
        :-
        X = igloo,
        g3a(Y).

g3a(inuit).


f_8 :-  freeze(A, foo1(A)),
        freeze(A, foo2(A)),
        goo(A).

foo1(X).
foo2(X).
goo(777).


f_9 :-	freeze(W1, silly(W1,yellow)),
        W1 = foaming,
        freeze(W2, grump(W2,blue)),
        W2=W1.

u3 :-	freeze(W1, silly(W1,yellow)),
        freeze(W2, grump(W2,blue)),
        W2=nonsense.

u4 :-	freeze(W1, silly(W1,yellow)),
        freeze(W2, grump(W2,blue)),
        W1=nonsense.

u5 :-	freeze(W1, silly(W1,yellow)),
        freeze(W2, grump(W2,blue)),
        ub(W1,W2).

ub(nonsense,silliness).


f_10 :- not(freeze_backtrack).

freeze_backtrack
        :-
        freeze(X, mary(X)), fred(X), fail.

mary(X).
fred(2).
fred(3).
fred(4).


f_11 :-
        fdtest([A,B,C,D]),
        goo(A).

fdtest([A,B,C,D]) :-
        fd([A], B),
        fd([A,B], C),
        fd([B,C], D).

fd([], 1).
fd([A | As], B)
        :-!,
        freeze(A, fd(As, B)).




