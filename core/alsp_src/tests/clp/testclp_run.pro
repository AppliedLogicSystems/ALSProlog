/*=====================================================================*
 |			testclp_run.pro
 |		Copyright (c) 1996 Applied Logic Systems, Inc.
 |
 |		Automated test run driver for clp tests
 *=====================================================================*/

r :- run_clp_tests.

t0 :- 
	run_clp_tests('clpt', 1, 12).

:- [-testclp_db].

module testclp.

export run_clp_tests/0.
export run_clp_tests/3.
run_clp_tests 
	:-
	default_range(Start, Stop),
	defult_outfile(OutFile),
	run_clp_tests(OutFile, Start, Stop).
	
run_clp_tests(OutFile, Start, Stop)
	:-
	open(OutFile, write, OS, []),
	clp_testing(Start, Stop, Failures, OS),
	printf(OS, '\n\n===================================\n',[]),
	printf(OS, 'The following tests failed:\n\t%t\n\n',[Failures]),
	flush_output(OS),
	close(OS).

defult_outfile('clptests.log').

default_range(1, Stop)
	:-
	bagof(K, [Args,Body]^clause(b(K,Args),Body), TestNums),
	max(TestNums, Stop).

max([N | ListOfNums], Max)
	:-
	max_of(ListOfNums, N, Max).

max_of([], Max, Max).
max_of([N | ListOfNums], CurMax, Max)
	:-
	max(N, CurMax, NextMax),
	max_of(ListOfNums, NextMax, Max).


clp_testing(CurNum, Stop, [], OS)
	:-
	CurNum > Stop,
	!.

clp_testing(CurNum, Stop, Failures, OS)
	:-
	clause(b(CurNum,ArgsCopy), BodyCopy),
	length(ArgsCopy, NumArgs),
	test_var_names(VNameList),
	at_most_n(VNameList, NumArgs, VNames),
	ArgsCopy = VNames,
	run_clp_test(CurNum, VNames, BodyCopy, Failures, NewFailures, OS),

	NextNum is CurNum + 1,
	clp_testing(NextNum, Stop, NewFailures, OS).


test_var_names(['X','Y','Z','U','V','W']).

run_clp_test(CurNum, VNames, BodyCopy, F, F, OS)
	:-
	call(b(CurNum,Args)),
	!,
	clp_test_success(CurNum, Args, VNames, BodyCopy, OS).
	
run_clp_test(CurNum, VNames, BodyCopy, [CurNum | NF], NF, OS)
	:-
	start_test_out(CurNum, BodyCopy, OS),
	printf(OS, '\n     -->> FAILED\n',[]).

clp_test_success(CurNum, Args, VNames, BodyCopy, OS)
	:-
	start_test_out(CurNum, BodyCopy, OS),
	show_substs_ext(VNames, Args, OS),
	fin_clp_test_success(CurNum, OS).

start_test_out(CurNum, BodyCopy, OS)
	:-
	printf(OS, '\n---------- Test #%t -------------',[CurNum]),
	printf(OS, '\n\t:- %t',[BodyCopy]).

fin_clp_test_success(CurNum, OS)
	:-
	orig(CurNum, OrigProblem),
	orig_expect(CurNum, OrigResultsList),
	!,
	printf(OS, '\n----- Original Version:\n\t?- %t\n',[OrigProblem]),
	write_lines_opt(OS, OrigResultsList, [quoted(false)]).

fin_clp_test_success(_, _).

endmod.
