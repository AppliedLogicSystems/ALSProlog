/*=====================================================================*
 |			testclp_run.pro
 |		Copyright (c) 1996 Applied Logic Systems, Inc.
 |
 |		Automated test run driver for clp tests
 *=====================================================================*/

r :- run_clp_tests.

t0 :- run_clp_tests('clpt', 5, 12).
t1 :- run_clp_tests('clpt', 11, 12).
t2 :- run_clp_tests('clpt', 1004, 1005).
t3 :- run_clp_tests('clpt', 20, 21).
t4 :- run_clp_tests('clpt', 1006, 1008).
t5 :- run_clp_tests('clpt', 3010, 3011).


	%% Definition of spec_b_findall:
:- [specbf].

	%% Database of tests:
:- [-testclp_db].

module testclp.

export run_clp_tests/0.
export run_clp_tests/3.
export run_clp_tests/4.

run_clp_tests 
	:-
	default_range(Start, Stop),
	bagof(K, [Args,Body]^clause(b(K,Args),Body), TestIDs),
	default_outfile(OutFile),
	run_clp_tests(OutFile, TestIDs, Start, Stop).
	
run_clp_tests(OutFile, Start, Stop)
	:-
	bagof(K, [Args,Body]^clause(b(K,Args),Body), TestIDs),
	run_clp_tests(OutFile, TestIDs, Start, Stop).

run_clp_tests(OutFile, TestIDs, Start, Stop)
	:-
	printf(user_output, 'Running from %t to %t\n',[Start,Stop]),
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
	asplit0([Start | Inter], Stop, AdjList0, _),
	append(AdjList0, [Stop], AdjList).

run_clp_tests(AdjList, OutFile)
	:-
	AdjList = [TheFirst | _],
	last(AdjList, TheLast),
	builtins:epsilon_show(Eps),
	open(OutFile, write, OS, []),
	testing_header(OS,TheFirst,TheLast,Eps),
	clp_testing(AdjList, Failures, OS),
	printf(OS, '\n\n===================================\n',[]),
	printf(OS, 'The following tests failed:\n\t%t\n\n',[Failures]),
	flush_output(OS),
	close(OS).

default_outfile('clptests.log').

default_range(first, last).

test_var_names(['X','Y','Z','U','V','W']).


clp_testing([], [], OS).

clp_testing([CurID | RestIDs], Failures, OS)
	:-
	clause(b(CurID,ArgsCopy), BodyCopy),
	(expect(CurID, VNames, _) ->
		true
		;
		length(ArgsCopy, NumArgs),
		test_var_names(VNameList),
		at_most_n(VNameList, NumArgs, VNames)
	),
	VNames = ArgsCopy,
	run_clp_test(CurID, ArgsCopy, BodyCopy, Failures, NewFailures, OS),
	write(user_output,CurID), nl(user_output),
	clp_testing(RestIDs, NewFailures, OS).

run_clp_test(CurNum, ArgsCopy, BodyCopy, FIn, FOut, OS)
	:-
	expect(CurNum, _, mult(VValuesList)),
	length(VValuesList, NumSols),
	!,
	mult_run_clp_test(CurNum, NumSols, VValuesList, ArgsCopy, BodyCopy, FIn, FOut, OS).

run_clp_test(CurNum, ArgsCopy, BodyCopy, F, F, OS)
	:-
	call(b(CurNum,Args)),
	!,
	clp_test_success(CurNum, Args, ArgsCopy, BodyCopy, OS).
	
run_clp_test(CurNum, ArgsCopy, BodyCopy, [CurNum | NF], NF, OS)
	:-
	cpt_test_fail(CurNum, BodyCopy, OS).

mult_run_clp_test(CurNum, NumSols, VValuesList, ArgsCopy, BodyCopy, F, F, OS)
	:-
	clause(b(CurNum, Args),_),
	spec_b_findall(Args, b(CurNum, Args), Solns, NumSols),
	!,
	mult_clp_test_success(CurNum, Solns, NumSols, 
							VValuesList, ArgsCopy, BodyCopy, OS).

mult_run_clp_test(CurNum, _, _, _, BodyCopy, [CurNum | NF], NF, OS)
	:-
	cpt_test_fail(CurNum, BodyCopy, OS).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%% OUTPUT ROUTINES
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_test_out(CurNum, Body, OS)
	:-
	printf(OS, '\n\n=============== Test #%t ===============',[CurNum]),
	printf(OS, '\n\t:- %t',[Body]).

show_orig(CurNum, OS)
	:-
	orig(CurNum, OrigProblem),
	orig_expect(CurNum, OrigResultsList),
	!,
	printf(OS, '\n------------------------------ Original Version:\n\t?- %t\n',
				[OrigProblem]),
	write_lines_opt(OS, OrigResultsList, [quoted(false)]).

show_orig(_, OS)
	:-
	printf(OS, '\n------------------------------\n',[]).

cpt_test_fail(CurNum, BodyCopy, OS)
	:-
	start_test_out(CurNum, BodyCopy, OS),
	printf(OS, '\n     -->> FAILED\n',[]).

clp_test_success(CurID, Args, VNames, BodyCopy, OS)
	:-
	start_test_out(CurID, BodyCopy, OS),
	show_orig(CurID, OS),
	printf(OS, '\n\n+--Solution:', []),
	show_substs_ext(VNames, Args, OS),
	extract_interval_list(Args, IArgs),
	(expect(CurID, XpVNames, XpSoln) ->
		(match_ints_list(XpSoln, IArgs) -> 
			printf(OS, '   OK Soln!\n', [])
			;
			printf(OS, '   BAD Soln!! Expected %t\n', [XpSoln])
		)
		;
		true
	).

mult_clp_test_success(CurNum, Solns, NumSols, VValuesList, VNames, Body, OS)
	:-
	start_test_out(CurNum, Body, OS),
	show_orig(CurNum, OS),
	show_mult_solns(Solns, VNames, VValuesList, OS).

show_mult_solns([], VNames, VValuesList, OS).
show_mult_solns([Soln | Solns], VNames, [ExpSol | VValuesList], OS)
	:-
	printf(OS, '\n\n+--Solution:', []),
	show_substs_ext(VNames, Soln, OS),
	printf(OS, '\n-expected:', []),
	show_substs_ext(VNames, ExpSol, OS),
		(match_ints_list(ExpSol, Soln) -> 
			printf(OS, '   OK Soln!\n', [])
			;
			printf(OS, '   BAD Soln!! \n', [])
		),
	show_mult_solns(Solns, VNames, VValuesList, OS).

testing_header(OutS,TheFirst,TheLast,Eps)
	:-
	als_system(Info),
	printf(OutS,
		'-------------->>> Test running: <<<--------------\n',[]),
	printf(OutS, '   ',[]),
	builtins:print_banner(OutS, Info),
	date(Date), time(Time),
	printf(OutS,'    Date: %t   Time: %t\n',[Date,Time]),
	get_cwd(WorkingDir),
	printf(OutS,'    Working directory: %t\n\n',[WorkingDir]),
	printf(OutS,'    Running tests from %t to %t\n\n',[TheFirst,TheLast]),
	printf(OutS,'    Checking solutions with epsilon = %t\n\n',[Eps]),
	header_note(Note),
	printf(OutS, Note,[]),
	printf(OutS,'+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n\n',[]).

header_note(
'\n\t"Original values" refer to values taken from BNR Prolog V3.5\n\
\tin the paper `Programming in CLP(BNR)`, Benhamou & Older PPCP \'93.\n').

match_ints_list([], []).
match_ints_list([LItem | Left], [RItem | Right])
	:-
	match_intrvs(LItem, RItem),
	match_ints_list(Left, Right).

match_intrvs([Lower, Upper], [Lower, Upper]) 
	:-
	number(Lower),
	number(Upper),
	!.
match_intrvs([LHead | LTail], [RHead | RTail]) 
	:-!,
	match_intrvs(LHead, RHead), 
	match_ints_list(LTail, RTail). 

match_intrvs([Lower, Upper], RItem)
	:-
	number(RItem),
	!,
	builtins:epsilon_show(Eps),
	abs(Upper - Lower) < Eps,
	(Lower - Eps) =< RItem, RItem =< (Upper + Eps).

match_intrvs(LItem, [Lower, Upper])
	:-
	number(LItem),
	!,
	match_intrvs([Lower, Upper], LItem).

match_intrvs(Item, Item) :-!.

match_intrvs(ItemL, ItemR)
	:-
	number(ItemL),
	number(ItemR),
	builtins:epsilon_show(Eps),
	abs(ItemL - ItemR) < Eps.

endmod.

