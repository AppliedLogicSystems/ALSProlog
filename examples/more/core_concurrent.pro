
dump([H | T]) :- 
	display_queue(true), !,
	dump0([H | T]).

dump(X) :- 
	display_queue(true), !,
	write(X),nl,ttyflush.

dump(_).

dump0([]) :-!,
	nl,ttyflush.

dump0([dumpQ(Tag) | T]) :-!,
	dumpQ(Tag).

dump0([dumpQ1(Tag) | T]) :-!,
	dumpQ1(Tag).

dump0([H | T]) :-!,
	write(H), write('   '),
	dump0(T).

dump0(dumpQ(Tag)) :-!,
	dumpQ(Tag).

dump0(X) :-
	write(X).

dumpQ(Tag) :- 
	   display_queue(true), !,
	   getGoalQueue(GQ), 
	   write(Tag=GQ),nl,ttyflush.
dumpQ(_).

dumpQ1(Tag) :- 
	   display_queue(true), !,
	   getGoalQueue(GQ), 
	   arg(1, GQ, Q1), 
	   write(Tag=Q1),nl,ttyflush.
dumpQ1(_).

/*-----------------------------------------------------------*
 | Queue Management:
 |
 |	initQueue/0	-- initializes the goal queue to empty
 |	popQueue/1	-- removes an element to the queue
 |	pushQueue/1	-- adds an element to the queue
 *-----------------------------------------------------------*/

initQueue :- 
	setGoalQueue(gq([],[])).

popQueue(Item) :- 
	getGoalQueue(GQ),
	arg(1,GQ,[Item|QT]),		%% unify Item, QT, and test for nonempty
	popQueue(QT,GQ).		%% fix queue so front is gone

popQueue([],GQ) :-			%% Queue is empty 
	!,
	mangle(1,GQ,[]),		%% adjust front to be empty
	mangle(2,GQ,[]).		%% adjust rear to be empty also

popQueue(QT,GQ) :-			%% Queue is not empty
	mangle(1,GQ,QT).		%% adjust front to point at tail

pushQueue(Item) :-
	getGoalQueue(Q),
	arg(2,Q,Rear),			%% get Rear of Queue
	pushQueue(Rear,Q,[Item]).

pushQueue([],Q,NewRear) :-
	!,
	mangle(1,Q,NewRear),
	mangle(2,Q,NewRear).
pushQueue(Rear,Q,NewRear) :-
	mangle(2,Rear,NewRear),		%% add the new rear
	mangle(2,Q,NewRear).		%% new rear is now rear of queue


/* --- Not used in par1, but used in par2 -- par4 */

run_processes :-
	popQueue(G),
		dump(run_processes_goal=G),
	!,
	G,
	run_processes.
run_processes.

kill_processes :-
	popQueue(_),
	!,
	kill_processes.
kill_processes.
