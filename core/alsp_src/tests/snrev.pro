/*--------------------------------------------------------------------*
 |			snrev.pro	
 |
 |		-- Standard naive reverse benchmark.
 |
 |	This is the standard benchmark used to compute LIPS.
 |	Lists have length 100, and the inner number of iterations is 100.
 |	This (100x) iteration is run 3 times and the results averaged.
 |	Now modified to check that it accumulates a non-zero average
 |  time for any run of nrev. If the average time = 0, then the list 
 |  length is doubled, and the test is retried.
 |
 |	Usage: *	?- snrev.
 *--------------------------------------------------------------------*/

snrev 
	:-
	als_system(Info),
	dmember(os=OS,Info),
	dmember(os_variation=OS_Var,Info),
	dmember(processor=Proc,Info),
	dmember(prologVersion=ProVer,Info),
	builtins:system_name(Info, ProName),
	printf('Test running %t: Version = %t\n',[ProName,ProVer]),
	printf('    Processor=%t OS = %t %t\n',[Proc,OS, OS_Var]),
	date(Date), printf('    Date: %t \n',[Date]),
	do_nrev_run(100, AVE_LIPS),

	printf('-------------------\n',[]),
	printf('LIPS Ave   = %d\n\n\n',[AVE_LIPS]).

do_nrev_run(Size1, AVE_LIPS)
	:-
	nrev0(Size1, 100, LIPS1),
	!,
	printf('LIPS Run 1 = %d\n',[LIPS1]),
	nrev0(Size1, 100, LIPS2),
	printf('LIPS Run 2 = %d\n',[LIPS2]),
	nrev0(Size1, 100, LIPS3),
	printf('LIPS Run 3 = %d\n',[LIPS3]),

	AVE_LIPS is (LIPS1 + LIPS2 + LIPS3)/3.

do_nrev_run(Size1, AVE_LIPS)
	:-
	NextSize is 2*Size1,
	printf('Running too fast for clock!\n',[]),
	printf('Trying list length = %d [100 iterations]\n',[NextSize]),
	do_nrev_run(NextSize, AVE_LIPS).

nrev0(Length, Iters, LIPS)
	:-
	genlst(Length, List),

	time(Time1),
	runtest(Iters, List),
	time(Time2),
	runcontrol(Iters, List),
	time(Time3),

	compute_average_time(Time3, Time2, Time1, Iters, Avgtime),
	LI is ((Length+1) * (Length+2)) / 2 + 1,
	LIPS is LI/Avgtime.


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
	AverageTime is ((MidTime-StartTime) - (EndTime-MidTime)) / Iters.

for(I, I, I) :- !.
for(I, J, I).
for(I, J, K) :- I1 is I+1, for(I1, J, K).

genlst(0, []) :- !.
genlst(N, [N|X]) :- N1 is N-1, genlst(N1, X).

time(Time) :- Time is cputime.
