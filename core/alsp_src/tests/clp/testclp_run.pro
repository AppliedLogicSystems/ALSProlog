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
export run_clp_tests/4.

run_clp_tests 
	:-
	default_range(Start, Stop),
	bagof(K, [Args,Body]^clause(b(K,Args),Body), TestIDs),
	defult_outfile(OutFile),
	run_clp_tests(OutFile, TestIDs, Start, Stop).
	
run_clp_tests(OutFile, Start, Stop)
	:-
	bagof(K, [Args,Body]^clause(b(K,Args),Body), TestIDs),
	run_clp_tests(OutFile, TestIDs, Start, Stop).

run_clp_tests(OutFile, TestIDs, Start, Stop)
	:-
	adjust_id_list(Start, Stop, TestIDs, AdjList),
	run_clp_tests(AdjList, OutFile).

adjust_id_list(Start, Stop, TestIDs, TestIDs)
	:-
	default_range(Start, Stop),
	!.

adjust_id_list(Start, Stop, TestIDs, AdjList)
	:-
	default_range(Start, _),
	!,
	asplit0(TestIDs, Stop, AdjList, _).

adjust_id_list(Start, Stop, TestIDs, AdjList)
	:-
	default_range(_, Stop),
	!,
	asplit0(TestIDs, Start, _, AdjList).

adjust_id_list(Start, Stop, TestIDs, AdjList)
	:-
	asplit0(TestIDs, Start, _, Inter),
	asplit0(Inter, Stop, AdjList, _).


run_clp_tests(AdjList, OutFile)
	:-
	open(OutFile, write, OS, []),
	clp_testing(AdjList, Failures, OS),
	printf(OS, '\n\n===================================\n',[]),
	printf(OS, 'The following tests failed:\n\t%t\n\n',[Failures]),
	flush_output(OS),
	close(OS).

defult_outfile('clptests.log').


default_range(first, last).
/*
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
*/


clp_testing([], [], OS).

clp_testing([CurID | RestIDs], Failures, OS)
	:-
	clause(b(CurID,ArgsCopy), BodyCopy),
	(expect(CurID, VNames, _) ->
		true
		;
		length(Args, NumArgs),
		test_var_names(VNameList),
		at_most_n(VNameList, NumArgs, VNames)
	),
	VNames = ArgsCopy,
	run_clp_test(CurID, ArgsCopy, BodyCopy, Failures, NewFailures, OS),
	clp_testing(RestIDs, NewFailures, OS).


test_var_names(['X','Y','Z','U','V','W']).

run_clp_test(CurNum, ArgsCopy, BodyCopy, FIn, FOut, OS)
	:-
	expect(CurNum, _, mult(VValuesList)),
	length(VValuesList, NumSols),
	!,
	mult_run_clp_test(CurNum, NumSols, VValuesList, ArgsCopy, BodyCopy, FIn, FOut, OS).

mult_run_clp_test(CurNum, NumSols, VValuesList, ArgsCopy, BodyCopy, FIn, FOut, OS)
	:-
	clause(b(CurNum, Args),_),
	b_findall(Args, b(CurNum, Args), Solns, NumSols),
	!,
	mult_clp_test_success(CurNum, Solns, NumSols, 
							VValuesList, ArgsCopy, BodyCopy, OS).

mult_run_clp_test(CurNum, NumSols, VValuesList, ArgsCopy, BodyCopy, FIn, FOut, OS)
	:-
	cpt_test_fail(CurNum, BodyCopy, OS).





run_clp_test(CurNum, VNames, ArgsCopy, BodyCopy, F, F, OS)
	:-
	call(b(CurNum,Args)),
	!,
	clp_test_success(CurNum, Args, VNames, BodyCopy, OS).
	
run_clp_test(CurNum, VNames, ArgsCopy, BodyCopy, [CurNum | NF], NF, OS)
	:-
	cpt_test_fail(CurNum, BodyCopy, OS).

cpt_test_fail(CurNum, BodyCopy, OS)
	:-
	start_test_out(CurNum, BodyCopy, OS),
	printf(OS, '\n     -->> FAILED\n',[]).


clp_test_success(CurNum, Args, VNames, BodyCopy, OS)
	:-
	start_test_out(CurNum, BodyCopy, OS),
	show_substs_ext(VNames, Args, OS),
	fin_clp_test_success(CurNum, OS).

start_test_out(CurNum, Body, OS)
	:-
	printf(OS, '\n=============== Test #%t ===============',[CurNum]),
	printf(OS, '\n\t:- %t',[Body]).

fin_clp_test_success(CurNum, OS)
	:-
	orig(CurNum, OrigProblem),
	orig_expect(CurNum, OrigResultsList),
	!,
	printf(OS, '\n------------- Original Version:\n\t?- %t\n',[OrigProblem]),
	write_lines_opt(OS, OrigResultsList, [quoted(false)]).

fin_clp_test_success(_, _).

mult_clp_test_success(CurNum, Solns, NumSols, VValuesList, VNames, Body, OS)
	:-
	start_test_out(CurNum, Body, OS),
	show_mult_solns(Solns, VNames, VValuesList, OS),
	fin_clp_test_success(CurNum, OS).

show_mult_solns([], VNames, VValuesList, OS).
show_mult_solns([Soln | Solns], VNames, [ExpSol | VValuesList], OS)
	:-
	printf(OS, '\n+--Solution:', []),
	show_substs_ext(VNames, Soln, OS),
	printf(OS, '\n-expected:\n', []),
	show_substs_ext(VNames, ExpSol, OS),
	show_mult_solns(Solns, VNames, VValuesList, OS).

endmod.

