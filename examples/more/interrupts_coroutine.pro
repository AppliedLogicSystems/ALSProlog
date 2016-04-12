/*==================================================================================*
 |			interrupts_coroutine.pro
 |		Copyright (c) 1993-2016 Applied Logic Systems, Inc.
 |
 |	Consumer / producer example with interrupt-based time slices.
 |	Like concurrent_interrupts.pro, the producer (ints_from/2)  is creating a 
 |	list of integers having an uninstantiated tail, and the consumer is eating 
 | 	the list from the head. However, in this case, the operational roles of
 |	consume/2 and ints_from are inverted. When consume/2 encounters an
 |	uninstatiated list tail, it calls suspend/1 on itself; this will push the
 |	consume call onto the queue, and will pop the waiting ints_from/2 call
 |	off the queue, and invoke it using run_process/1; this sets an alarm
 |	and then calls its argument (ints_from(N, T)).  ints_from/1 has only
 |	one clause:
 |	   ints_from(N,[N|T]) :- NN is N+1, ints_from(NN,T).
 |	This executes without stopping until the sigalrm is raised, adding as many
 |	integers to the list as it can before the interrupt occurs.
 |
 | Author: Kevin Buettner
 *==================================================================================*/
:-['core_concurrent.pro'].

:- op(990,xfy,&).

:- make_gv('GoalQueue'), make_gv('CurrentProc').

/*-------------------------------------------------------*
 | main is called to start the consumer / producer off.
 *-------------------------------------------------------*/

% Set the argument here to false to suppress diagnostic display of the queue and other program parts:
display_queue(false).

% How many integers to generate and consume (= how many produce/consume cycles to traverse):
n_consume(10000).

main 	:-	
	n_consume(K),
	consume(K,L) & ints_from(0,L).

consume(N,NV) :-
	nonvar(NV),
	!,
	consume0(N,NV).

consume(N,NV) :- 
	suspend(consume(N,NV)).

consume0(0,_) :- 
		dump('  -->> kill_processes from consume0(0,...) <<--'),
	kill_processes.
consume0(N,[H|T]) :-
	!,
	NP is N-1,
		dump(consume:N-[H|T]),
write(c:H),nl,
	consume(NP,T).

ints_from(N,[N|T]) :-
	NN is N+1,
		dump( ints_from(NN,T) ),
	ints_from(NN,T).


/*----------------------------------------*
 * Process Management
 *----------------------------------------*/

set_alarm :-
	alarm(0.02,0).
%	alarm(0.001,0).

clear_alarm :-
	alarm(0,0).

:- compiletime, module_closure(suspend,1).

suspend(M,G) :-
		dump('  >> suspend:'),
		dumpQ1(queue_in),
	pushQueue(M:G),
	popQueue(NG),
		dumpQ1(queue_out),
%gc,
	run_process(NG).

:- compiletime, module_closure('&',2,parallelize).

parallelize(M,G1,G2) :-
	initQueue,
	psetup(M,G1),
	psetup(M,G2),
		dumpQ(parallelize_queue),
	trap(run_processes,context_switcher),
	clear_alarm.

psetup(M, G1 & G2) :-
    	psetup(M,G1),
    	psetup(M,G2).

psetup(M, G) :-
	pushQueue(M:G).

run_process(M:G) :-
	functor(G,P,A),
	setCurrentProc(M:P/A),
	set_alarm,
		dump('Running'=(M:G)),
	M:G.

context_switcher(EventId, Goal, Context) :-
	EventId \= sigalrm,
	EventId \= application_interrupt,
	!,
	propagate_event(EventId,Goal,Context).

context_switcher(sigalrm,Goal,_) :- !,
		dump(['  >> context_switch_sigalrm:', dumpQ1(queue)]),
	getCurrentProc(M:P/A),
	break_on(M,P,A),
    		%% Uncomment this, & comment out the following [functor....] to see it
    		%% vomiting interrupts to lower-level stuff:
	nl,dump(newgoal=Goal),
%	functor(Goal,FF,_), ((FF==consume;FF==ints_from) -> nl,dump(newgoal=Goal); true),
%trace,
%set_alarm,
	Goal.

context_switcher(application_interrupt,M:G,_) :-
	getCurrentProc(M1:P/A),
	break_off(M1,P,A),
	suspend(M,G).

