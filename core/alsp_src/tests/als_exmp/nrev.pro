/*
 * nrev.pro	-- Naive reverse benchmark.
 *
 *	This is the standard benchmark used to compute LIPS for a Prolog system.
 *	Lists should have length of at least 30 for accurate measurements.  The
 *	number of iterations should be such that the benchmark runs for at least
 *	a second (the longer it runs, the more accurate the number that comes
 *	out).  This program should work with all Prologs compatible with 
 *	C-Prolog.
 *
 * Usage:
 *	?- reconsult(nrev).
 *	?- nrev.
 */



nrev :-
	write('Length of List: '),
	read(Length),

	write('Number of Iterations: '),
	read(Iters),

	nrev0(Length, Iters).

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

compute_average_time(EndTime, MidTime, StartTime, Iters, AverageTime) :-
	AverageTime is ((MidTime-StartTime) - (EndTime-MidTime)) / Iters.

for(I, I, I) :- !.
for(I, J, I).
for(I, J, K) :- I1 is I+1, for(I1, J, K).

genlst(0, []) :- !.
genlst(N, [N|X]) :- N1 is N-1, genlst(N1, X).

time(Time) :- Time is cputime.
