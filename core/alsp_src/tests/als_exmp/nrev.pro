/*--------------------------------------------------------------------------*
 |			nrev.pro	
 |
 |		-- Naive reverse benchmark.
 |
 |	This is the standard benchmark used to compute LIPS for a Prolog system.
 |	{ Lists should have length of at least 30 for accurate measurements. }
 |  30 is now (9/6/95) way too small.  On a 90MHz Pentium, even with lists
 |  of length 90 and 100 iterations, we get 0 average elapsed time. Our
 |  old standard test (100 x 100) just barely gives meaningful results.
 |  Now modified so that if average time is 0, it doubles the length of
 |  the list, and tries again.
 |
 | Usage:
 |	?- reconsult(nrev).
 |	?- nrev.
 *--------------------------------------------------------------------------*/

nrev :-
	write('Length of List: '),
	read(Length),

	write('Number of Iterations: '),
	read(Iters),

	do_nrev0(Length, Iters).

do_nrev0(Length, Iters)
	:-
	nrev0(Length, Iters),
	!.

do_nrev0(Length, Iters)
	:-
	NextSize is 2*Length,
	printf('\nRunning too fast for clock [length = %d,iters=%d]!\n',
			[Length,Iters]),
	printf('Trying list length = %d [%d iterations]\n',[NextSize, Iters]),
	do_nrev0(NextSize, Iters).

nrev0(Length, Iters)
	:-
	genlst(Length, List),

	time(Time1),
	runtest(Iters, List),
	time(Time2),
	runcontrol(Iters, List),
	time(Time3),

	compute_average_time(Time3, Time2, Time1, Iters, Avgtime),
	LI is ((Length+1) * (Length+2)) / 2 + 1,
	LIPS is LI/Avgtime,
	write('LIPS' = LIPS),
	nl.


runtest(N, List) :-
	for(1, N, _),
	runtestonce(List),
	fail.
runtest(_, _).

runcontrol(N, List) :-
	for(1, N, _),
	runcontrolonce(List),
	fail.
runcontrol(_, _).	

runtestonce(List) :- nrev(List, _), !.

runcontrolonce(List) :- control(List, _), !.

nrev([], []).
nrev([H|T], L) :- nrev(T, L1), append(L1, [H], L).

append([], X, X).
append([H|T], X, [H|T1]) :- append(T, X, T1).


control(_, _).

compute_average_time(T, T, T, _, _) 
	:-!,
	fail.
compute_average_time(EndTime, MidTime, StartTime, Iters, AverageTime) 
	:-
	Top is ((MidTime-StartTime) - (EndTime-MidTime)),
	(Top > 0 ->
		AverageTime is ((MidTime-StartTime) - (EndTime-MidTime)) / Iters
		;
		AverageTime is 1/Iters
	).

for(I, I, I) :- !.
for(I, J, I).
for(I, J, K) :- I1 is I+1, for(I1, J, K).

genlst(0, []) :- !.
genlst(N, [N|X]) :- N1 is N-1, genlst(N1, X).

time(Time) :- Time is cputime.
