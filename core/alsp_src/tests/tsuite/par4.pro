/*=====================================================================*
 |			par4.pro
 |		Copyright (c) 1993-96 Applied Logic Systems, Inc.
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

main :-
get_cwd(D), write(cwd=D),nl,
%	fail. /* What tests should be run, and what constitutes succes? */
	tp3.

:- make_gv('GoalQueue4').
:- op(990,xfy,&), op(990,xfy,&&).


tp1 :-	gc, X is cputime, primes1(200), Y is cputime-X,
	write('Elapsed Time'=Y),nl.

tp2 :-	gc, X is cputime, primes2(200), Y is cputime-X,
	write('Elapsed Time'=Y),nl.

tp3 :-	gc, X is cputime, primes3(200), Y is cputime-X,
	write('Elapsed Time'=Y),nl.

tp4 :-	gc, X is cputime, primes4(200), Y is cputime-X,
	write('Elapsed Time'=Y),nl.

tp(BufSize) :-	
	gc, X is cputime, primes(200,BufSize), Y is cputime-X,
	write('Elapsed Time'=Y),nl.


primes1(N) :-
	consume(N,P1,PR) & sieve(IF2,[P1|PR]) & ints_from(2,IF2).

primes2(N) :-
	buffer(L1,L2) & ints_from(2,L1) &
	sieve(L2,[C1|L3]) &
	consume(N,C1,L3).

primes3(N) :-
	buffer(L1,L2) & ints_from(2,L1) &
	buffer(L3,[C1|L4]) & sieve(L2,L3) &
	consume(N,C1,L4).

primes4(N) :-
	buffer(L1,L2) & ints_from(2,L1) &
	buffer(L3,[C1|L4]) & sieve2(L2,L3) &
	consume(N,C1,L4).

primes(N,BufSize) :-
	buffer(BufSize,L1,L2) & ints_from(2,L1) &
	sieve(L2,[C1|L3]) &
	consume(N,C1,L3).

primes_list :-
	X is cputime,
	    buffer(7,L1,L2) & ints_from(2,L1) &
	    sieve(L2,L3) &
	    firstN(200,L3,L4),
	Y is cputime-X,
	write(L4),nl,
	write('Time'=Y),nl.
	



filter(P,[H|T],OL) :-
%	var2(H,OL),
	var(H),
	!,
	suspend(filter(P,[H|T],OL)).
filter(P,[H|T],OL) :-
    %tab,write(f:P:H),nl,
	0 =:= H mod P, 
	!,
	filter(P,T,OL).
filter(P,[H|T],[H|OT]) :-
	filter(P,T,OT).


sieve([H|T],OL) :-
%	var2(H,OL),
	var(H),
	!,
	suspend(sieve([H|T],OL)).
sieve([H|T],[H|OT]) :- 
	filter(H,T,SL) && sieve(SL,OT).

sieve2([H|T],OL) :-
%	var2(H,OL),
	var(H),
	!,
	suspend(sieve2([H|T],OL)).
sieve2([H|T],[H|OT]) :- 
    %tab,write(s:H),nl,
	buffer(5,SL1,SL2) && filter(H,T,SL1) && sieve2(SL2,OT).

var2(V1,V2) :- var(V1).
var2(V1,V2) :- var(V2).

buffer(In,Out) :- buffer(10,In,Out).

tab :- write('	').
	
	
/*
 * ---
 */

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

	


consume(0,_,_) :-
	!,
	kill_processes.
consume(N,Elem,Rest) :-
	var(Elem),
	!,
	suspend(consume(N,Elem,Rest)).
consume(N,Elem,[Next|Rest]) :-
	NPrev is N-1,
	write(c:Elem), nl,
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
    %tab, write(p:N),nl,
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
 * Queue Management:
 *
 *	initQueue/0		-- initializes the goal queue to empty
 *	remQueue/1		-- removes an element to the queue
 *	addQueue/1		-- adds an element to the queue
 */


initQueue :- setGoalQueue4(gq([],[])).

remQueue(Item) :- 
	getGoalQueue4(GQ),
	arg(1,GQ,[Item|QT]),		%% unify Item, QT, and test for nonempty
	remQueue(QT,GQ).		%% fix queue so front is gone

remQueue([],GQ) :-			%% Queue is empty 
	!,
	mangle(1,GQ,[]),		%% adjust front to be empty
	mangle(2,GQ,[]).		%% adjust rear to be empty also

remQueue(QT,GQ) :-			%% Queue is not empty
	mangle(1,GQ,QT).		%% adjust front to point at tail


addQueue(Item) :-
	getGoalQueue4(Q),
	arg(2,Q,Rear),			%% get Rear of Queue
	addQueue(Rear,Q,[Item]).

addQueue([],Q,NewRear) :-
	!,
	mangle(1,Q,NewRear),
	mangle(2,Q,NewRear).
addQueue(Rear,Q,NewRear) :-
	mangle(2,Rear,NewRear),		%% add the new rear
	mangle(2,Q,NewRear).		%% new rear is now rear of queue



/*
 * Process Management
 */


:- compiletime, module_closure(suspend,1).

suspend(M,G) :-
	addQueue(M:G),
	remQueue(NG),
	NG.

:- compiletime, module_closure('&&',2,parallelize2).
parallelize2(M,G1,G2) :-
	psetup(M,G1),
	psetup(M,G2),
	remQueue(G),
	G.

:- compiletime, module_closure('&',2,parallelize).

parallelize(M,G1,G2) :-
	initQueue,
	psetup(M,G1),
	psetup(M,G2),
    getGoalQueue4(GQ), write(GQ),nl,
	run_processes.


psetup(M, G1 & G2) :-
    	psetup(M,G1),
    	psetup(M,G2).
psetup(M, G) :-
	addQueue(M:G).

run_processes :-
	remQueue(G),
	!,
	G,
	run_processes.
run_processes.

kill_processes :-
	remQueue(_),
	!,
	kill_processes.
kill_processes.
