/*==================================================================================*
 |			simple_coroutine.pro
 |		Copyright (c) 1993-2016 Applied Logic Systems, Inc.
 |
 |	Cooperative producers and consumers example without the timer or interrupts.
 |	The producer is ints_from/2.  On recursing, ints_from/2 and consume/2 each 
 |	explicitly suspend themselves using suspend with suspend using mangle to 
 |	manage what is 	essentially a one-element queue, pushing things in at the
 |	end and popping them from the front.
 |		--KAB
 |
 | Author: Kevin Buettner
 *==================================================================================*/

:-['core_concurrent.pro'].

:- op(990,xfy,&).

:- make_gv('GoalQueue').

/*-------------------------------------------------------*
 | main is called to start the consumer / producer off.
 *-------------------------------------------------------*/

% Set the argument here to false to suppress diagnostic display of the queue and other program parts:
display_queue(true).

% How many integers to generate & consume (= how many produce/consume cycles to traverse):
n_consume(3).

main :-	
	n_consume(K),
	consume(K,L) & ints_from(0,L).

consume(N,NV) :-
	nonvar(NV),
		dump(consume(N,NV)),
	!,
	consume0(N,NV).

consume(N,NV) :-
	suspend(consume(N,NV)).

consume0(0,_) :- 
	kill_processes.

consume0(N,[H|T]) :-
	!,
	NP is N-1,
%		dump(consume0:N-[H|T]),
	consume(NP,T).

ints_from(N,[N|T]) :-
	NN is N+1,
		dump([ints_from(N,[N|T]), suspend(ints_from(NN,T))]),
	suspend(ints_from(NN,T)).

:- compiletime, module_closure(suspend,1).

suspend(M,G) :-
		dumpQ('  >suspend_queue'),
	pushQueue(M:G),
		dump(['  >suspend_add'=(M:G), dumpQ('  >suspend_queue')]),
	popQueue(NG),
		dump(['  >remq_ng'=NG, dumpQ('  >suspend_queue')]),
	NG.

:- compiletime, module_closure('&',2,parallelize).

parallelize(M,G1,G2) :-
	initQueue,
	psetup(M,G1),
	psetup(M,G2),
getGoalQueue(GQ), dump(parallelize_queue=GQ),
	run_processes.

psetup(M, G1 & G2) :-
    	psetup(M,G1),
    	psetup(M,G2).
psetup(M, G) :-
	pushQueue(M:G).

