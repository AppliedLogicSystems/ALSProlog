/*=====================================================================
 |				autotest.pro
 |		Copyright (c) 1993 Applied Logic Systems, Inc.
 |
 |		Automatic test control top-level
 *====================================================================*/

:-[-autotest_db].

module als_testing.

export run_tests/0.


run_tests :-
	open('autotest.log',write,LOGStream,[alias(autotestlog)]),
	date(Date),time(Time),
	als_system(SysL),
	dmember(os=OS,SysL),
	dmember(os_variation=OSVar,SysL),
	dmember(processor=Proc,SysL),
	dmember(prologVersion=PVer,SysL),
	printf(autotestlog,'*********************************************\n',[]),
	printf(autotestlog,'AUTOTest Log: Date: %t Time: %t\n',[Date,Time]),
	printf(autotestlog,'    OS=%t-%t  Proc=%t  Ver=%t\n',[OS,OSVar,Proc,PVer]),
	printf(autotestlog,'*********************************************\n',[]),
	configure_testing,
	!,
	tell(autotestlog),
	run_tests0,
	bagOf(TestID, failed(TestID), FailedTests),
	final_message(FailedTests,autotestlog),
	close(LOGStream),
	final_message(FailedTests,user_output),
	told.

run_tests :-
	printf(autotestlog,'!!!! Configuration Failure !!!!!\n',[]),
	close(autotestlog).

configure_testing
	:-
	get_cmdline_vals(CmdLineVs),
	bagOf(TD, member(['-td',TD], CmdLineVs), TDs),
	configure_testing(TDs, CmdLineVs).
	
configure_testing(TDs, CmdLineVs)
	:-
	TDs \= [],
	!,
	add_search_dirs(TDs).

configure_testing(TDs, CmdLineVs)
	:-
	dmember(['-srcdir',SrcDir], CmdLineVs),
	extendPath(SrcDir, 'tests/als_exmp',ALSEXMP_Path),
	extendPath(SrcDir, 'tests/tsuite',TSUITE_Path),
	add_search_dirs([ALSEXMP_Path,TSUITE_Path]).
	
add_search_dirs([]).
add_search_dirs([D | Ds])
	:-
	builtins:assert(searchdir(D)),
	add_search_dirs(Ds).

run_tests0 :-
	test_info(TestID, TestFile, TestMod, TestStartCall, TestDescrip),
	conduct_test(TestID, TestFile, TestMod, TestStartCall, TestDescrip),
	flush_output(autotestlog),
	fail.
run_tests0.

conduct_test(TestID, TestFile, TestMod, TestStartCall, TestDescrip)
	:-
	reconsult(TestFile),
	printf(user_output,'--%t\n',[TestID]),
	record_test_info(TestID, TestDescrip),
	TestMod:call(TestStartCall),
	!,
	record_success(TestID),
	remove_test_code(TestMod).

conduct_test(TestID, TestFile, TestMod, TestStartCall, TestDescrip)
	:-
	record_failure(TestID),
	remove_test_code(TestMod).

record_test_info(TestID, TestDescrip)
	:-
	printf(autotestlog,'\n#############################\n',[]),
	printf(autotestlog,'\n--Test = %t:   %t\n\n',[TestID,TestDescrip]).
	
record_success(TestID)
	:-
	printf(autotestlog,'\n--Test = %t: Successful.\n\n',[TestID]).

record_failure(TestID)
	:-
	assert(failed(TestID)),
	printf(autotestlog,'\n--Test = %t: >>>FAILURE!!!.\n\n',[TestID]).

remove_test_code(TestMod)
	:-
	bagof((P,A),R^all_procedures(TestMod,P,A,R),PL),
	kill_off(PL,TestMod).

kill_off([],_).
kill_off([(P,A) | PL],Mod)
	:-
	Mod:abolish(P,A),
	kill_off(PL,Mod).

final_message([], OutStream)
	:-!,
	printf(OutStream,'All Tests Were Successful !!\n').
final_message(FailedTests, OutStream)
	:-
	printf(OutStream, 'The following tests failed:\n\t%t\n', FailedTests).
endmod.
