/*
 * par3.pro
 *
 *	Consumers and producers with time slices.
 */

:- make_gv('GoalQueue'), make_gv('CurrentProc').
:- op(990,xfy,&), op(990,xfy,&&).


/*
 * main is called to start the consumer / producer off.
 */


main :-	
	als_system(SysVars),
	dmember(os=OS,SysVars),
	not(OS = macos), not(OS = msw95),
	consume(100,L) & ints_from(0,L).

primes(N) :-
	consume(N,P) & sieve(IF2,P) & ints_from(2,IF2).

filter(P,IL,OL) :-
	nonvar(IL),
	filter0(P,IL,OL).
filter(P,IL,OL) :-
	suspend(filter(P,IL,OL)).

filter0(P,[H|T],OL) :-
	0 =:= H mod P, 
	!,
	filter(P,T,OL).
filter0(P,[H|T],[H|OT]) :-
	filter(P,T,OT).

sieve(IL,OL) :-
	nonvar(IL),
	sieve0(IL,OL).
sieve(IL,OL) :-
	suspend(sieve(IL,OL)).

sieve0([H|T],[H|OT]) :- sieve(SL,OT) && filter(H,T,SL).

	
	
/*
 * ---
 */

consume(N,NV) :-
	nonvar(NV),
	!,
	consume0(N,NV).
consume(N,NV) :- suspend(consume(N,NV)).

consume0(0,_) :- kill_processes.
consume0(N,[H|T]) :-
	!,
	NP is N-1,
	write(c:H),nl,
	consume(NP,T).


ints_from(N,[N|T]) :-
%	write(p:N),nl,
	NN is N+1,
	ints_from(NN,T).


/*
 * Queue Management:
 *
 *	initQueue/0		-- initializes the goal queue to empty
 *	remQueue/1		-- removes an element to the queue
 *	addQueue/1		-- adds an element to the queue
 */


initQueue :- setGoalQueue(gq([],[])).

remQueue(Item) :- 
	getGoalQueue(GQ),
	arg(1,GQ,[Item|QT]),		%% unify Item, QT, and test for nonempty
	remQueue(QT,GQ).		%% fix queue so front is gone

remQueue([],GQ) :-			%% Queue is empty 
	!,
	mangle(1,GQ,[]),		%% adjust front to be empty
	mangle(2,GQ,[]).		%% adjust rear to be empty also

remQueue(QT,GQ) :-			%% Queue is not empty
	mangle(1,GQ,QT).		%% adjust front to point at tail


addQueue(Item) :-
	getGoalQueue(Q),
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


set_alarm :-
	alarm(0.02,0).

clear_alarm :-
	alarm(0,0).

:- compiletime, module_closure(suspend,1).

suspend(M,G) :-
	addQueue(M:G),
	remQueue(NG),
	run_process(NG).

:- compiletime, module_closure('&&',2,parallelize2).
parallelize2(M,G1,G2) :-
	psetup(M,G1),
	psetup(M,G2),
	remQueue(G),
	run_process(G).

:- compiletime, module_closure('&',2,parallelize).

parallelize(M,G1,G2) :-
	initQueue,
	psetup(M,G1),
	psetup(M,G2),
    getGoalQueue(GQ), write(GQ),nl,
	trap(run_processes,context_switcher),
	clear_alarm.


psetup(M, G1 & G2) :-
    	psetup(M,G1),
    	psetup(M,G2).
psetup(M, G) :-
	addQueue(M:G).

run_processes :-
	remQueue(G),
	!,
	run_process(G),
	run_processes.
run_processes.

kill_processes :-
	remQueue(_),
	!,
	kill_processes.
kill_processes.

run_process(M:G) :-
	functor(G,P,A),
	setCurrentProc(M:P/A),
	set_alarm,
%   write('Running '),write(M:P/A),nl,
	M:G.


context_switcher(EventId, Goal, Context) :-
	EventId \= sigalrm,
	EventId \= application_interrupt,
	!,
	propagate_event(EventId,Goal,Context).
context_switcher(sigalrm,Goal,_) :-
	!,
	getCurrentProc(M:P/A),
	break_on(M,P,A),
	Goal.
context_switcher(application_interrupt,M:G,_) :-
	getCurrentProc(M1:P/A),
	break_off(M1,P,A),
	suspend(M,G).

