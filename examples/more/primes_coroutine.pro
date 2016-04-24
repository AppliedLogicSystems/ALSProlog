/*=====================================================================*
 |			primes_coroutine.pro
 |		Copyright (c) 1993-2016 Applied Logic Systems, Inc.
 |
 |	Producers and consumers (for primes) with added buffering to improve
 |	throughput. tp(6) or tp(7) seems to be optimal for SPARC.  Time slices
 |	were removed since it appeared that the heap was being consumed by the
 |	early producers.  Giving each process a time slice might work if a
 |	decent scheduler is written.  For this example, this means that once
 |	ints_from runs for a while initially, it should be suspended until
 |	more integers are needed -- not when its turn comes up again by being
 |	in the queue.
 |
 | Author: Kevin Buettner
 *=====================================================================*/

:-['core_concurrent.pro'].

:- op(990,xfy,&), op(990,xfy,&&).

:- make_gv('GoalQueue').


% Set the argument here to false to suppress diagnostic display of the queue and other program parts:
display_queue(true).

% Number of primes to filter for:
num_primes(200).

main :-	
	num_primes(NP),
	tp1(NP, _).
%	tp2(NP, _).
%	tp3(NP, _).
%	tp4(NP, _).
%	primes_list(NP, _).

mainall :-	
	num_primes(NP),
	tp1(NP, ET1),
	tp2(NP, ET2),
	tp3(NP, ET3),
	tp4(NP, ET4),
	primes_list(NP, ETL),
	dispOut([tp1, tp2, tp3, tp4, primes_list], [ET1, ET2, ET3, ET4, ETL]).


tp1(NP, Y) :-	
	gc, X is cputime, primes1(NP), Y is cputime-X,
	write('tp1 Elapsed Time'=Y),nl.

tp2(NP, Y) :-	
	gc, X is cputime, primes2(NP), Y is cputime-X,
	write('Elapsed Time'=Y),nl.

tp3(NP, Y) :-	
	gc, X is cputime, primes3(NP), Y is cputime-X,
	write('Elapsed Time'=Y),nl.

tp4(NP, Y) :-	
	gc, X is cputime, primes4(NP), Y is cputime-X,
	write('Elapsed Time'=Y),nl.

tp(NP, BufSize) :-	
	gc, X is cputime, primes(NP,BufSize), Y is cputime-X,
	write('Elapsed Time'=Y),nl.

dispOut([], _).
dispOut([TP | TPTail], [ET | ETTail])
	:-
	write(TP : ' Elapsed Time' = ET),nl,
	dispOut(TPTail, ETTail).

primes1(NP) :-
	consume(NP,P1,PR) & sieve(IF2,[P1|PR]) & ints_from(2,IF2),
		nl,write([P1|PR]),nl,ttyflush.

primes2(NP) :-
	buffer(L1,L2) & ints_from(2,L1) &
	sieve(L2,[C1|L3]) &
	consume(NP,C1,L3),
		nl,write([C1|L3]),nl,ttyflush.

primes3(NP) :-
	buffer(L1,L2) & ints_from(2,L1) &
	buffer(L3,[C1|L4]) & sieve(L2,L3) &
	consume(NP,C1,L4),
		nl,write([C1|L4]),nl,ttyflush.

primes4(NP) :-
	buffer(L1,L2) & ints_from(2,L1) &
	buffer(L3,[C1|L4]) & sieve2(L2,L3) &
	consume(NP,C1,L4),
		nl,write([C1|L4]),nl,ttyflush.

primes(NP,BufSize) :-
	buffer(BufSize,L1,L2) & ints_from(2,L1) &
	sieve(L2,[C1|L3]) &
	consume(NP,C1,L3),
		nl,write([C1|L3]),nl,ttyflush.

primes_list(NP, Y) :-
	X is cputime,
	    buffer(7,L1,L2) & ints_from(2,L1) &
	    sieve(L2,L3) &
	    firstN(NP,L3,L4),
	Y is cputime-X,
	nl,write(L4),nl,
	write('Time'=Y),nl.
	

filter(P,[H|T],OL) :-
	var(H),
	!,
	suspend(filter(P,[H|T],OL)).
filter(P,[H|T],OL) :-
	0 =:= H mod P, 
	!,
	filter(P,T,OL).
filter(P,[H|T],[H|OT]) :-
	filter(P,T,OT).

sieve([H|T],OL) :-
	var(H),
	!,
	suspend(sieve([H|T],OL)).
sieve([H|T],[H|OT]) :- 
	filter(H,T,SL) && sieve(SL,OT).

sieve2([H|T],OL) :-
	var(H),
	!,
	suspend(sieve2([H|T],OL)).
sieve2([H|T],[H|OT]) :- 
	buffer(5,SL1,SL2) && filter(H,T,SL1) && sieve2(SL2,OT).

buffer(In,Out) :- buffer(10,In,Out).

	
/*
 * ---
test1 :-	
	consume(20,First,Rest) & ints_from(0,[First|Rest]).

test2 :-
	ints_from(0,[First|Rest]) & consume(20,First,Rest).

test3 :-
	ints_from(0,L)	&
	buffer(5,L,[First|Rest]) &
	consume(100,First,Rest).

test4 :-
	consume(100,First,Rest) &
	buffer(5,L,[First|Rest]) &
	ints_from(0,L).

test5 :- 
	consume(100,First,Rest) &
	ints_from(0,L) &
	buffer(5,L,[First|Rest]).
 */

consume(0,_,_) :-
	!,
	kill_processes.
consume(N,Elem,Rest) :-
	var(Elem),
	!,
	suspend(consume(N,Elem,Rest)).
consume(N,Elem,[Next|Rest]) :-
	NPrev is N-1,
		write(c:Elem),write(' '), ttyflush, %  nl,
	consume(NPrev,Next,Rest).

firstN(0,_,[]) :-
	!,
	kill_processes.
firstN(N,[Elem|Rest],Out) :-
	var(Elem),
	!,
	suspend(firstN(N,[Elem|Rest],Out)).
firstN(N,[Elem|Rest],[Elem|Out]) :-
	NPrev is N-1,
	firstN(NPrev,Rest,Out).


ints_from(N,L) :-
	var(L),
	!,
	suspend(ints_from(N,L)).
ints_from(N,[N|T]) :-
	NN is N+1,
	ints_from(NN,T).


%%
%% buffer(N,In,Out)
%%

buffer(N,In,Out) :-
	mk_buf(N,In,InEnd),
	mk_buf(N,Out,_),
	In = [InH|InT],
	buffer(InH,InT,InEnd,Out).

mk_buf(0,L,L) :- 
	!.
mk_buf(N,[_|T],Z) :-
	NP is N-1,
	mk_buf(NP,T,Z).

buffer(InH,InT,InEnd,Out) :-
	var(Out),				%% suspend if consumer is
	!,					%% behind (no template made)
	suspend(buffer(InH,InT,InEnd,Out)).
buffer(InH,InT,InEnd,Out) :-
	buffer0(InH,InT,InEnd,Out).

buffer0(InH,InT,InEnd,Out) :-
	var(InH),				%% suspend if producer is
	!,					%% behind
	suspend(buffer(InH,InT,InEnd,Out)).
buffer0(E,[InH|InT],[_|InEnd],[E|Out]) :-
	buffer0(InH,InT,InEnd,Out).

/*
 * Process Management
 */


:- compiletime, module_closure(suspend,1).

suspend(M,G) :-
	pushQueue(M:G),
	popQueue(NG),
	NG.

:- compiletime, module_closure('&&',2,parallelize2).
parallelize2(M,G1,G2) :-
	psetup(M,G1),
	psetup(M,G2),
	popQueue(G),
	G.

:- compiletime, module_closure('&',2,parallelize).

parallelize(M,G1,G2) :-
	initQueue,
	psetup(M,G1),
	psetup(M,G2),
		dumpQ1(initQueue),
	run_processes.

psetup(M, G1 & G2) :-
    	psetup(M,G1),
    	psetup(M,G2).
psetup(M, G) :-
	pushQueue(M:G).

