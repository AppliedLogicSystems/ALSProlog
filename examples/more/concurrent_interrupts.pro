/*===============================================================================*
 | 			concurrent_interrupts.pro
 |		Copyright (c) 1993-2016 Applied Logic Systems, Inc.
 |
 |  -- test interaction of global variables, trap mechanism, and alarm interrupt.
 |	
 |	This a producer/consumer example. The producer is creating a list of
 |	integers having an uninstantiated tail, and the consumer is eating the list 
 |	from the head. Each time the producer adds an integer to the list head, 
 |	it goes to sleep:
 |     	   produce(N,[N|T]) :- NN is N+1, sleep(produce(NN,T)).
 |	The consumer eats integers off the list head until it encounters an
 |	uninstantiated variable; then it goes into an infinite loop:
 |	   consume(NV) :- nonvar(NV), consume0(NV).
 |	   consume(NV) :- consume(NV).
 |	What keeps this from being a stupid unending program is that at the very
 |	beginning,  an alarm handler was established:
 |	   main :- trap(main0,alarm_handler).
 |	And each time sleep is called, and a sleeping goal is woken, an alarm signal
 |	is set (via set_alarm) which will interrupt the consumer busy loop.
 |	Note that the only goals placed on the queue are calls to the producer.
 |	Diagnostic display is controlled by the presence of the fact: display_queue.
 |		--KAB	
 |
 | Author: Kevin Buettner
 *===============================================================================*/

:-['core_concurrent.pro'].  %% Load queue manipulation and display predicates.

:- make_gv('GoalQueue'), make_gv('SavedGoal'), setSavedGoal(true).

/*-------------------------------------------------------*
 | main is called to start the consumer / producer off.
 *-------------------------------------------------------*/

% Set the argument here to false to suppress diagnostic display of the queue and other program parts:
display_queue(true).

% How many integers to consume (= how many produce/consume cycles to traverse):
n_consume(5).

main :-
	trap(main0,alarm_handler).	%% call main0/0 and establish handler

main0 :-
	initQueue,
		dumpQ(initQueue),
	produce(0,L),     
		dump('L'=L),
	consume(L).
	
consume(NV) 
	:-
	nonvar(NV),
		dump(consume_clause_1),
	consume0(NV).

consume(NV) 
	:- 
		%% Uncomment this to spew dots by the consume/1, clause#2 loop:
%		write('.'),
	consume(NV).

consume0([H|T]) 
	:-!,
		dump(consume0:H),
	consume(T).

consume0([]).

produce(N,Tail) 
	:- 
	n_consume(N),
	Tail = [].

produce(N,[N|T]) 
	:-
		dump(produce:N),
	NN is N+1,
	sleep(produce(NN,T)).

/*-----------------------------*
 | Process Management
 *-----------------------------*/

alarm_handler(EventId, Goal, Context) 
	:-
		dump(['  >alarm_handler1_eid'=EventId, goal=Goal]),
	EventId \== sigalrm,
	!,
	propagate_event(EventId,Goal,Context).

alarm_handler(_,Goal,_) 
	:-
		dump(['  >alarm_handler_2_eid_sigalrm_g'=Goal, dumpQ(q)]),      %%getGoalQueue(Q), dump(q=Q),
	setSavedGoal(Goal),
	popQueue(NewGoal),
		dump('  >alarm_handler_2_eid_sigalrm_ng'=NewGoal),
	NewGoal.

/*--------------------------------------------------------*
 | sleep/1	
 |	-- puts a goal to sleep to wait for the next alarm.
 *--------------------------------------------------------*/

:- compiletime, module_closure(sleep,1).

sleep(M,G) 
	:- 
	pushQueue(M:G),
		dump([sleep_add2q=(M:G)]),
	getSavedGoal(SG), 
		dump(sleep_saved_goal=SG),
	set_alarm,
	SG.

set_alarm 
	:-
		dump(setting_alarm=1.05),
	alarm(1.05,0).
