/*
 * par1.pro
 *		-- test interaction of global variables, trap mechanism, and
 *		   alarm.  Also tests out interrupts.
 */

:- make_gv('GoalQueue'), make_gv('SavedGoal'), setSavedGoal(true).


/*
 * main is called to start the consumer / producer off.
 */


main :-
	trap(main0,alarm_handler).	%% call main0/0 and establish handler

main0 :-
	initQueue,
	produce(0,L),
	consume(L).
	
consume(NV) :-
	nonvar(NV),
	consume0(NV).
consume(NV) :- consume(NV).

consume0([H|T]) :-
	!,
	write(H),nl,
	consume(T).
consume0([]).


produce(100,[]) :- !.
produce(N,[N|T]) :-
%	write(p:N),nl,
	NN is N+1,
	sleep(produce(NN,T)).


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


alarm_handler(EventId, Goal, Context) :-
	EventId \== sigalrm,
	!,
	propagate_event(EventId,Goal,Context).
alarm_handler(_,Goal,_) :-
%	write('Goal'=Goal), nl,
	setSavedGoal(Goal),
	remQueue(NewGoal),
	NewGoal.

/*
 * sleep/1	-- puts a goal to sleep to wait for the next alarm.
 */

:- compiletime, module_closure(sleep,1).

sleep(M,G) :- 
	addQueue(M:G),
	getSavedGoal(SG),
	set_alarm,
	SG.

set_alarm :-
	alarm(1.05,0).
