/*=====================================================================*
 |			freeze.pro
 |		Copyright (c) 1995 Applied Logic Systems, Inc.
 |
 |		Freeze/delay - related tests
 |
 |	Author: Ken Bowen
 |	Date begun: May 7, 1995
 *=====================================================================*/

	/*-------------------------------------
	 | Simple interactive producer-consumer
	 |	-prompts at each cycle:
	 |	- gc - performs a gc, showing stats
	 |	- quit - exits this program
	 |	- any other term: next step in cycle
	 |  Goal: 
	 |	?- int_pc.
	 |	n = 0
	 |	>a.
	 |	n = 1
	 |	>a.
	 |	n = 2
	 |	>gc.
	 |	Before: Tr_b= 419d1c1c  B= 419d19dc  TR= 419d19dc  H= 40e1ecd8  
	 |		HB= 40e1ec84  H_b= 40e19ca0
	 |	After: Tr_b= 419d1c1c  B= 419d1a04  TR= 419d1a04  H= 40e1e8cc  
	 |		HB= 40e1e884  H_b= 40e19ca0
	 |	n = 3
	 |	>quit.
	 *------------------------------------*/

int_pc :- freeze(TS,produce(0,TS)), consume(TS).

produce(N, [N | T])
	:-
write(p(N)),nl,flush_outout,
	M is N+1,
	freeze(T, produce(M,T)).

consume([N | T])
	:-
	write(c(n)=N),nl,flush_output,
	write('>'),read(X),
	disp(X,T).

disp(quit, _) :-!.
disp(gc, T) 
	:-!,
	printf('Before: '), flush_output,cptx,
	gc,
	printf('After: '), flush_output,cptx,
	consume(T).
	
disp(_,T) 
	:-
	consume(T).

	/*-------------------------------------
	 | Simple non-interactive producer-
	 | consumer, with cutoff; displays some info
	 | Goal:
	 |	?- pc2.
	 |	-p-n = 0
	 |	Tr_b= 419d1c1c  B= 419d1b0c  TR= 419d1b0c  H= 40e1e4d0  HB= 40e1e4c4  
	 |			H_b= 40e19ca0
	 |	-p-n = 1
	 |	-p-n = 2
	 |	-p-n = 3
	 |	-p-n = 4
	 |	-p-n = 5
	 |	-p-n = 6
	 |	-p-n = 7
	 |	-p-n = 8
	 |	-p-n = 9
	 |	-p-n = 10
	 |	-p-n = 11
	 |	-p-n = 12
	 |	-p-n = 13
	 |	.....
	 |	-p-n = 297
	 |	-p-n = 298
	 |	-p-n = 299
	 |	-p-n = 300
	 |	Tr_b= 419d1c1c  B= 419d1b1c  TR= 419d1b1c  H= 40e1dd50  HB= 40e1dd50  
	 |			H_b= 40e19ca0
	 |	yes.
	 *------------------------------------*/

pc2 :- 
	freeze(S, produce2(0,S)), consume2(S).

produce2(N, [N | T])
	:-
	M is N+1,
	write('-p-'),
	freeze(T, produce2(M,T)).

consume2([N | T])
	:-
	write(n=N),nl,
	(0 is N mod 3 -> gc; true),
	(N < 300 ->
		(0 is N mod 25 -> cptx; true),
		consume2(T) ; cptx).


	/*-------------------------------------
	 | Some elementary tests
	 *------------------------------------*/

	/*-------------------------------------
	 | Elementary test:
	 | goal: ?- j(X).
	 | result:
	 | frump_running(def,yum)
	 | Got to clause #2 for j
	 |
	 | X[_4614]-> frump(_4614,yum)
	 *------------------------------------*/
j :-
	freeze(W1, frump(W1,yum)),
	s0(W1).

j :-
	write('Got to clause #2 for j'),nl,flush_output.
	

s0(def).

frump(A,B)
	:-
	write(frump_running('-',B)),nl,flush_output,
	fail.

	/*-------------------------------------
	 | Elementary test:
	 | goal: ?- k(X).
	 | result:
	 | aaa+
	 | grump_running(foo,ym)
	 | silly_running(bar,zippo)
	 | bbb+
	 | Tr_b= 419d1c1c  B= 419d1ae4  TR= 419d1ae4  H= 40e1e558  HB= 40e1e558  
	 |		H_b= 40e19ca0
	 | trail:wm_TR=419d1ae4 -> BStop (=B) =419d1ae4
	 | 
	 | X = jj(foo,bar)
	 *------------------------------------*/

k0(jj(W1,W2)) :-
	freeze(W1,grump(W1,ym)),
	freeze(W2,silly(W2,zippo)),
	t01(W1,W2).

t01(W1,W2)
	:-
	write('aaa+'),nl,flush_output,
	[W1,W2]=[foo,bar],
	write('bbb+'),nl,flush_output,
	cptx,
	swp_tr.

flop(def,flameout).

grump(A,B)
	:-
	write(grump_running(A,B)),nl,flush_output.

silly(A,B)
	:-
	write(silly_running(A,B)),nl,flush_output.

	/*-------------------------------------
	 | Elementary test:
	 *------------------------------------*/

u :-
	freeze(W1, silly(W1,yellow)),
	freeze(W2, grump(W2,blue)),
	W2=W1,
	W2 = igloo.

u1 :-
	freeze(W1, silly(W1,yellow)),
	u11(W1).

u11(W1)
	:-
	freeze(W2, grump(W2,blue)),
	W2=W1,
	u111(W2).

u111(W2)
	:-
	freeze(W3, grump(W3,purple)),
	W3 = W2,
	u1_4(W3).

u1_4(W3)
	:-
	W3 = igloo.

u11(W1).
u111(W1).
u1_4(W3).

	/*-------------------------------------
	 | Elementary test:
	 *------------------------------------*/

u2 :-
	freeze(W1, silly(W1,yellow)),
	W1 = foaming,
	freeze(W2, grump(W2,blue)),
	W2=W1.

u3 :-
	freeze(W1, silly(W1,yellow)),
	freeze(W2, grump(W2,blue)),
	W2=nonsense.

u4 :-
	freeze(W1, silly(W1,yellow)),
	freeze(W2, grump(W2,blue)),
	W1=nonsense.

u5 :-
	freeze(W1, silly(W1,yellow)),
	freeze(W2, grump(W2,blue)),
	ub(W1,W2).

ub(nonsense,silliness).

	/*-------------------------------------
	 | Elementary test:
	 *------------------------------------*/

v1 :-
	freeze(W1, silly(W1,yellow)),
	f(g(h(W1))) = f(g(h(hi88))).

	/*-------------------------------------
	 | Elementary test:
	 *------------------------------------*/

f2 :-
	freeze(X, freeze(Y, foobar(X,Y))),
	g2(X,Y).

g2(X,Y)
	:-
	Y = icecream,
	write(y=icecream),nl,flush_output,
	g3(X).

g3(X) :-
	X = coke,
	write(x=X),nl,flush_output.

	/*-------------------------------------
	 | Elementary test:
	 *------------------------------------*/

foobar(X,Y) :-
	write(running_foobar(X,Y)),nl,flush_output.

f2a :-
	freeze(X, freeze(Y, foobar(X,Y))),
	g2a(X,Y).

g2a(X,Y)
	:-
	X = igloo,
	write(x=igloo),nl,flush_output,
	g3a(Y).

g3a(Y)
	:-
	Y = inuit,
	write(y=inuit),nl,flush_output.
	
	/*-------------------------------------
	 | Elementary test:
	 *------------------------------------*/

g1 :- {foobar(X,Y)}, X=hi, write(bong),nl,flush_output,Y=there.

g2 :- {foobar(X,Y)}, X=hi, write(bong),nl,flush_output,fail.
g2 :- write(g2_cl2),nl.

g3 :-
	{foobar(X,Y)}, 
	X=hi(Boy), 
	write(bing),nl,flush_output,
	Y = there,
	write(bong),nl,flush_output,
	Boy = silly.
	
g4 :-
	{foobar(X,Y)}, 
	X=hi(Boy), 
	write(bing),nl,flush_output,
	Y = there,
	write(bong),nl,flush_output.
	
	/*-------------------------------------
	 | Elementary test:
	 *------------------------------------*/
goo(777).

b2 :-
	freeze(A, foo1(A)),
	freeze(A, foo2(A)),
	goo(A).

foo1(X) :- write(foo1(X)),nl,flush_output.
foo2(X) :- write(foo2(X)),nl,flush_output.

c2 :-
	freeze(A, foo1(A)),
	freeze(B, foo2(B)),
	A=B,
	goo(A).

c3 :-
	freeze(A, foo1(A)),
	hh(A).

	/*-------------------------------------
	 | Non-elementary tests:
	 *------------------------------------*/

	/*-------------------------------------
	 | Non-elementary test:
	 |   Freeze with backtracking:
	 | Result:
	 |	?- freeze_backtrack.
	 |	thaw(2)fred(2)
	 |	thaw(3)fred(3)
	 |	thaw(4)fred(4)
	 |	no.
	 *------------------------------------*/

fred(2) :- write(fred(2)),nl.
fred(3) :- write(fred(3)),nl.
fred(4) :- write(fred(4)),nl.

freeze_backtrack
	:-
	freeze(X, write(thaw(X))), fred(X), fail.

	/*-------------------------------------
	 | Non-elementary test:
	 |   Cascading freezes:
	 | Goal: (output for B needs fixing):
	 |	?- fdtest([A,B,C,D]).
	 |	A[_4635]-> fd([],
	 |	B[_4742]-> fd(['%lettervar%'('C')],'%lettervar%'('D'))
	 |	C = C
	 |	D = D
	 |
	 |  Goal:
	 |	?- fdtest([A,B,C,D]), A = 5.
	 |	A = 5
	 |	B = 1
	 |	C = 1
	 |	D = 1
	 *------------------------------------*/

fd([], 1).
fd([A | As], B) 
	:-!,
	freeze(A, fd(As, B)).

fdtest([A,B,C,D]) :-
	fd([A], B),
	fd([A,B], C),
	fd([B,C], D).

tfd :-
	fdtest([A,B,C,D]),
	goo(A).

	/*-------------------------------------
	 | Non-elementary test:
	 |   Resettable mangle under backtracking:
	 | Goal:
	 |	?- tm.
	 |	tm_start = ice_cream_cone(mary_smith)
	 |	tm_after_trailed_mangle = ice_cream_cone(john_jones)
	 |	tm_after_failure = ice_cream_cone(mary_smith)
	 *------------------------------------*/

tm :-
	tm(X).

tm(X) :-
	X = ice_cream_cone(Y),
	Y = mary_smith,
	write(tm_start=X),nl,flush_output,
	m(X).


m(X) :-
	
	trailed_mangle(1,X,john_jones),
	write(tm_after_trailed_mangle=X),nl,flush_output,
	fail.
m(X) :-
	write(tm_after_failure=X),nl,flush_output.
