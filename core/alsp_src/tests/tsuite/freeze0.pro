j(W1) :-
	freeze(W1, frump(W1,yum)),
	s0(W1).

j(W1) :-
	pbi_write('Got to clause #2 for j'),pbi_nl,pbi_ttyflush.
	

s0(W1)
	:-
	W1 =def.

frump(A,B)
	:-
	pbi_write(frump_running(A,B)),pbi_nl,pbi_ttyflush,
	fail.

%%%%%-------------------------

k0(jj(W1,W2)) :-
	freeze(W1,grump(W1,ym)),
	freeze(W2,silly(W2,zippo)),
	t01(W1,W2).

t01(W1,W2)
	:-
	pbi_write('aaa+'),pbi_nl,pbi_ttyflush,
%	flop(W1,W2),
	[W1,W2]=[foo,bar],
	pbi_write('bbb+'),pbi_nl,pbi_ttyflush,
	cptx,
	swp_tr.

flop(def,flameout).

grump(A,B)
	:-
	pbi_write(grump_running(A,B)),pbi_nl,pbi_ttyflush.

silly(A,B)
	:-
	pbi_write(silly_running(A,B)),pbi_nl,pbi_ttyflush.









/*
t01(W1,W2)
	:-
pbi_write('aaa+'),pbi_nl,pbi_ttyflush,
	flop(W1,W2),
	flip(W1),
/*
	W1 =def,
%	W1 = W2,
	W2 =flameout,
	dong(bong),
*/
%	display_heap(0,-140),
pbi_write('bbb+'),pbi_nl,pbi_ttyflush,
	cptx,
	swp_tr.

	
t01(_,_,_).

flop(def,flameout).
flop(X,X).
flip(flounce).
*/





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pc :- freeze(TS,produce(0,TS)), consume(TS).

produce(N, [N | T])
	:-
	M is N+1,
	freeze(T, produce(M,T)).

consume([N | T])
	:-
	write(n=N),nl,
	write('>'),read(X),
	!,
	disp(X,T).

disp(quit, _) :-!.
disp(gc,T) 
	:-!,
	gc,
	consume(T).
disp(_,T) 
	:-
	consume(T).

pc2 :- 
	cptx,
	freeze(S, produce2(0,S)), consume2(S).

produce2(N, [N | T])
	:-
	M is N+1,
	time(Time),
	printf('Produce[%t]: N=%t\n',[Time,N]),
	freeze(T, produce2(M,T)).

consume2([N | T])
	:-
	write('consume:N'=N),nl,
	gc,
	cptx,
%	!,
	consume2(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

u :-
	freeze(W1, silly(W1,yellow)),
	freeze(W2, grump(W2,blue)),
gc,
	W2=W1,
	W1 = igloo.

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

%%%---------------------------------------

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

v1 :-
	freeze(W1, silly(W1,yellow)),
	f(g(h(W1))) = f(g(h(hi88))).


	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

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
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

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
	
foobar(X,Y) :-
	write(running_foobar(X,Y)),nl,flush_output.




