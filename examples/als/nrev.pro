/*===============================================================*
 |		nrev.pro  
 |	Copyright (c) 1986, 1988 by Applied Logic Systems
 |	Distribution rights per Copying ALS
 |
 |		Standard LIPS benchmark
 *===============================================================*/
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

	computeavgtime(Time3, Time2, Time1, Iters, Avgtime),
	LI is ((Length+1) * (Length+2)) / 2 + 1,
	LIPS is LI/Avgtime,
	write('LIPS' = LIPS).


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

runtestonce(List) :- nrev(List, Z), !.

runcontrolonce(List) :- control(List, _), !.

nrev([], []).
nrev([H|T], L) :- nrev(T, L1), append(L1, [H], L).

append([], X, X).
append([H|T], X, [H|T1]) :- append(T, X, T1).

/*
reverse(X, Y) :- reverseaux(X, Y, []).

reverseaux([], Y, Y) :- !.
reverseaux([H|T], Out, Temp) :- reverseaux(T, Out, [H|Temp]).
*/

control(_, _).

computeavgtime(EndTime, MidTime, StartTime, Iters, AvgTime) :-
	AvgTime is ((MidTime-StartTime) - (EndTime-MidTime)) / Iters.

for(I, I, I) :- !.
for(I, J, I).
for(I, J, K) :- inc(I, I1), for(I1, J, K).

genlst(0, []) :- !.
genlst(N, [N|X]) :- dec(N, N1), genlst(N1, X).

dec(N,NP) :- NP is N-1.
inc(N,NS) :- NS is N+1.

time(X) :- X is cputime.
