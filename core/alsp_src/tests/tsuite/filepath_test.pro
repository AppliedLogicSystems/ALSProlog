test(subPath([], X), X = '').

do_test(test(Goal, Test)) :-
	catch(call(Goal),
		Error,
		throw(test_error(unexpected_exception(Error), test(Goal, Test)))),
	call(Test).
do_test(test(Goal, Test)) :-
	throw(test_error(test_failed, test(Goal, Test))).



/* rootPathFile test for MacOS */

subpath_generate_test(macos, [], '').
subpath_generate_test(unix, [], '').
subpath_generate_test(mswin32, [], '').

%%subpath_generate_test(macos, [''], ''). should test this as error condition
subpath_generate_test(unix, [''], '/').
subpath_generate_test(mswin32, [''], '\\').

subpath_generate_test(macos, [a], 'a').
subpath_generate_test(unix, [a], 'a').
subpath_generate_test(mswin32, [a], 'a').

subpath_generate_test(macos, [a, b], ':a:b').
subpath_generate_test(unix, [a, b], 'a/b').
subpath_generate_test(mswin32, [a, b], 'a\\b').

subpath_generate_test(macos, [a, b, c], ':a:b:c').
subpath_generate_test(unix, [a, b, c], 'a/b/c').
subpath_generate_test(mswin32, [a, b, c], 'a\\b\\c').

subpath_generate_test(macos, ['', a], 'a:').
subpath_generate_test(unix, ['', a], '/a').
subpath_generate_test(mswin32, ['', a], '\\a').

subpath_generate_test(macos, ['', a, b], 'a:b').
subpath_generate_test(unix, ['', a, b], '/a/b').
subpath_generate_test(mswin32, ['', a, b], '\\a\\b').

subpath_generate_test(macos, ['', a, b, c], 'a:b:c').
subpath_generate_test(unix, ['', a, b, c], '/a/b/c').
subpath_generate_test(mswin32, ['', a, b, c], '\\a\\b\\c').

/* Test parent directory marker for MacOS only, because the unix/win32 ".." is
   treated just like a directory name. */
subpath_generate_test(macos, ['::'], '::').
subpath_generate_test(macos, ['', '::'], ':::'). /* This is meaningless */
subpath_generate_test(macos, ['', a, '::'], 'a::').  /* This is meaningless */
subpath_generate_test(macos, ['', a, b, '::'], 'a:b::').
subpath_generate_test(macos, [a, b, '::'], ':a:b::').
subpath_generate_test(macos, ['::', a, b], '::a:b').
subpath_generate_test(macos, [a, '::'], ':a::').
subpath_generate_test(macos, [a, '::', '::'], ':a:::').
subpath_generate_test(macos, [a, '::', '::', '::'], ':a::::').
subpath_generate_test(macos, [a, '::', b], ':a::b').
subpath_generate_test(macos, [a, '::', '::', b], ':a:::b').
subpath_generate_test(macos, [a, '::', '::', '::', b], ':a::::b').
subpath_generate_test(macos, ['::', a], '::a').
subpath_generate_test(macos, ['::', '::', a], ':::a').
subpath_generate_test(macos, ['::', '::', '::', a], '::::a').

subpath_parse_test(macos, [], '').
subpath_parse_test(macos, [], '').
subpath_parse_test(unix, [], '').
subpath_parse_test(mswin32, [], '').

subpath_parse_test(unix, [''], '/').
subpath_parse_test(mswin32, [''], '\\').

subpath_parse_test(macos, [a], 'a').
subpath_parse_test(macos, [a], ':a').
subpath_parse_test(macos, [a], ':a:').
subpath_parse_test(unix, [a], 'a').
subpath_parse_test(unix, [a], 'a/').
subpath_parse_test(mswin32, [a], 'a').
subpath_parse_test(mswin32, [a], 'a\\').

subpath_parse_test(macos, [a, b], ':a:b').
subpath_parse_test(macos, [a, b], ':a:b:').
subpath_parse_test(unix, [a, b], 'a/b').
subpath_parse_test(unix, [a, b], 'a/b/').
subpath_parse_test(mswin32, [a, b], 'a\\b').
subpath_parse_test(mswin32, [a, b], 'a\\b\\').

subpath_parse_test(macos, ['', a], 'a:').
subpath_parse_test(unix, ['', a], '/a').
subpath_parse_test(unix, ['', a], '/a/').
subpath_parse_test(mswin32, ['', a], '\\a').
subpath_parse_test(mswin32, ['', a], '\\a\\').

subpath_parse_test(macos, ['', a, b], 'a:b').
subpath_parse_test(macos, ['', a, b], 'a:b:').
subpath_parse_test(unix, ['', a, b], '/a/b').
subpath_parse_test(unix, ['', a, b], '/a/b/').
subpath_parse_test(mswin32, ['', a, b], '\\a\\b').
subpath_parse_test(mswin32, ['', a, b], '\\a\\b\\').

/* Test parent directory marker for MacOS only, because the unix/win32 ".." is
   treated just like a directory name. */
subpath_parse_test(macos, ['::'], '::').
subpath_parse_test(macos, ['::', '::'], ':::').
subpath_parse_test(macos, ['', a, '::'], 'a::').  /* This is meaningless */
subpath_parse_test(macos, ['', a, b, '::'], 'a:b::').
subpath_parse_test(macos, [a, b, '::'], ':a:b::').
subpath_parse_test(macos, ['::', a, b], '::a:b').
subpath_parse_test(macos, [a, '::'], ':a::').
subpath_parse_test(macos, [a, '::', '::'], ':a:::').
subpath_parse_test(macos, [a, '::', '::', '::'], ':a::::').
subpath_parse_test(macos, [a, '::', b], ':a::b').
subpath_parse_test(macos, [a, '::', '::', b], ':a:::b').
subpath_parse_test(macos, [a, '::', '::', '::', b], ':a::::b').
subpath_parse_test(macos, ['::', a], '::a').
subpath_parse_test(macos, ['::', '::', a], ':::a').
subpath_parse_test(macos, ['::', '::', '::', a], '::::a').

check(_, X, X).
check(L, X, Y) :-
	printf("Problem with %t, the result %t isn't %t\n", [L, X, Y]),
	throw(test_failed).

test_subpath_generate :-
	findall(subpath_generate_test(A, B, C), subpath_generate_test(A, B, C), L),
	test_subpath_generate(L).
	
test_subpath_generate([]).
test_subpath_generate([subpath_generate_test(OS, List, ResultPath) | Ts]) :-
	test_subpath_generate(OS, List, ResultPath),
	test_subpath_generate(Ts).

test_subpath_generate(OS, List, ResultPath) :-
	printf("testing %t\n", [subpath_generate_test(OS, List, ResultPath)]),
	subPath(List, X, OS),
	check(List, X, ResultPath).

test_subpath_parse :-
	findall(subpath_parse_test(A, B, C), subpath_parse_test(A, B, C), L),
	test_subpath_parse(L).
	
test_subpath_parse([]).
test_subpath_parse([subpath_parse_test(OS, List, Path) | Ts]) :-
	test_subpath_parse(OS, List, Path),
	test_subpath_parse(Ts).

test_subpath_parse(OS, List, Path) :-
	printf("testing %t\n", [subpath_parse_test(OS, List, Path)]),
	subPath(X, Path, OS),
	check(Path, X, List).

test_subpath :-
	test_subpath_generate,
	test_subpath_parse.
/*
test(Predicate)
	:-
	call(Predicate).
test(FailedPredicate)
	:-
	printf(user_output, 'The test %t failed!\n',
		[FailedPredicate], [quoted(true)]).

subpath_construct_test(macos) :-
	test((subPath([], P1), P1 = '')),
	test((subPath([a], P2), P2 = ':a')),
	test((subPath([a, b], P3), P3 = ':a:b')).

subpath_deconstruct_test(macos) :-
	test(subPath([], '')),
	test(subPath([a], ':a')),
	test(subPath([a, b], ':a:b')).

subpath_test :-
	subpath_construct_test(macos),
	subpath_deconstruct_test(macos).

construct_test(macos) :-
	/* Basic combinations. */
	test((rootPathFile('', [], '', P1), P1 = '')),
	test((rootPathFile(a, [], '', P2), P2 = 'a:')),
	test((rootPathFile('', [a], '', P3), P3 = 'a')), /* maybe :a:? */
	test((rootPathFile('', [], a, P4), P4 = 'a')),
	test((rootPathFile(a, [b], '', P5), P5 = 'a:b')),
	test((rootPathFile(a, [], b, P6), P6 = 'a:b')),
	test((rootPathFile('', [a], b, P7), P7 = ':a:b')),
	test((rootPathFile(a, [b], c, P8), P8 = 'a:b:c')),
	
	/* Test multi-part paths */
	
	test((rootPathFile('', [a, b], '', P9), P9 = ':a:b')),
	test((rootPathFile('', [a, b, c], '', P10), P10 = ':a:b:c')),
	test((rootPathFile('', [a, b, c, d], '', P11), P11 = ':a:b:c:d')),

	test((rootPathFile(v, [a, b], '', P12), P12 = 'v:a:b')),
	test((rootPathFile(v, [a, b, c], '', P13), P13 = 'v:a:b:c')),
	test((rootPathFile(v, [a, b, c, d], '', P14), P14 = 'v:a:b:c:d')),
	
	test((rootPathFile('', [a, b], f, P15), P15 = ':a:b:f')),
	test((rootPathFile('', [a, b, c], f, P16), P16 = ':a:b:c:f')),
	test((rootPathFile('', [a, b, c, d], f, P17), P17 = ':a:b:c:d:f')),

	test((rootPathFile(v, [a, b], f, P18), P18 = 'v:a:b:f')),
	test((rootPathFile(v, [a, b, c], f, P19), P19 = 'v:a:b:c:f')),
	test((rootPathFile(v, [a, b, c, d], f, P20), P20 = 'v:a:b:c:d:f')),

	/* Test parent directory path elements
	   (not v::, v:::, etc are nonsense paths in real life) */

	test((rootPathFile('', ['::'], '', P21), P21 = '::')),
	test((rootPathFile('', ['::', '::'], '', P22), P22 = ':::')),
	test((rootPathFile('', ['::', '::', '::'], '', P23), P23 = '::::')),

	test((rootPathFile(v, ['::'], '', P24), P24 = 'v::')),
	test((rootPathFile(v, ['::', '::'], '', P25), P25 = 'v:::')),
	test((rootPathFile(v, ['::', '::', '::'], '', P26), P26 = 'v::::')),

	test((rootPathFile('', ['::'], f, P27), P27 = '::f')),
	test((rootPathFile('', ['::', '::'], f, P28), P28 = ':::f')),
	test((rootPathFile('', ['::', '::', '::'], f, P29), P29 = '::::f')),

	test((rootPathFile(v, ['::'], f, P30), P30 = 'v::f')),
	test((rootPathFile(v, ['::', '::'], f, P31), P31 = 'v:::f')),
	test((rootPathFile(v, ['::', '::', '::'], f, P32), P32 = 'v::::f')),

	test((rootPathFile('', [a, '::'], '', P33), P33 = ':a::')),
	test((rootPathFile('', ['::', a], '', P34), P34 = '::a')),

	test((rootPathFile('', [a, '::', '::'], '', P35), P35 = ':a:::')),
	test((rootPathFile('', ['::', a, '::'], '', P36), P36 = '::a::')),
	test((rootPathFile('', ['::', '::', a], '', P37), P37 = ':::a')),

	test((rootPathFile('', [a, '::', b, '::'], '', P38), P38 = ':a::b::')),
	test((rootPathFile('', ['::', a, '::', b], '', P39), P39 = '::a::b')),

	test((rootPathFile('', [a, '::', b, '::', c], '', P40), P40 = ':a::b::c')),

	/* Test current directory path elements 
	   (note: there is no way in MacOS to say ./././. Its always just : */
	
	test((rootPathFile('', [':'], '', P41), P41 = ':')),
	test((rootPathFile('', [':', ':'], '', P42), P42 = ':')),
	test((rootPathFile('', [':', ':', ':'], '', P43), P43 = ':')),

	test((rootPathFile(v, [':'], '', P44), P44 = 'v:')),
	test((rootPathFile(v, [':', ':'], '', P45), P45 = 'v:')),
	test((rootPathFile(v, [':', ':', ':'], '', P46), P46 = 'v:')),

	test((rootPathFile('', [':'], f, P47), P47 = ':f')),
	test((rootPathFile('', [':', ':'], f, P48), P48 = ':f')),
	test((rootPathFile('', [':', ':', ':'], f, P49), P49 = ':f')),

	test((rootPathFile(v, [':'], f, P50), P50 = 'v:f')),
	test((rootPathFile(v, [':', ':'], f, P51), P51 = 'v:f')),
	test((rootPathFile(v, [':', ':', ':'], f, P52), P52 = 'v:f')),

	test((rootPathFile('', [a, ':'], '', P53), P53 = 'a')),
	test((rootPathFile('', [':', a], '', P54), P54 = ':a')),

	test((rootPathFile('', [a, ':', ':'], '', P55), P55 = 'a')),
	test((rootPathFile('', [':', a, ':'], '', P56), P56 = 'a')),
	test((rootPathFile('', [':', ':', a], '', P57), P57 = 'a')),

	test((rootPathFile('', [a, ':', b, ':'], '', P58), P58 = ':a:b')),
	test((rootPathFile('', [':', a, ':', b], '', P59), P59 = ':a:b')),

	test((rootPathFile('', [a, ':', b, ':', c], '', P60), P60 = ':a:b:c')).

deconstruct_test(macos) :-
	/* Basic combinations. */
	test(rootPathFile('', [], '', '')),
	test(rootPathFile(a, [], '', 'a:')),
	test(rootPathFile('', [a], '', ':a:')),
	test(rootPathFile('', [], a, 'a')),
	test(rootPathFile(a, [b], '', 'a:b:')),
	test(rootPathFile(a, [], b, 'a:b')),
	test(rootPathFile('', [a], b, ':a:b')),
	test(rootPathFile(a, [b], c, 'a:b:c')),
	
	/* Test multi-part paths */
	test(rootPathFile('', [a, b], '', ':a:b:')),
	test(rootPathFile('', [a, b, c], '', ':a:b:c:')),
	test(rootPathFile('', [a, b, c, d], '', ':a:b:c:d:')),

	test(rootPathFile(v, [a, b], '', 'v:a:b:')),
	test(rootPathFile(v, [a, b, c], '', 'v:a:b:c:')),
	test(rootPathFile(v, [a, b, c, d], '', 'v:a:b:c:d:')),
	
	test(rootPathFile('', [a, b], f, ':a:b:f')),
	test(rootPathFile('', [a, b, c], f, ':a:b:c:f')),
	test(rootPathFile('', [a, b, c, d], f, ':a:b:c:d:f')),

	test(rootPathFile(v, [a, b], f, 'v:a:b:f')),
	test(rootPathFile(v, [a, b, c], f, 'v:a:b:c:f')),
	test(rootPathFile(v, [a, b, c, d], f, 'v:a:b:c:d:f')),

	/* Test parent directory path elements
	   (not v::, v:::, etc are nonsense paths in real life) */

	test(rootPathFile('', ['::'], '', '::')),
	test(rootPathFile('', ['::', '::'], '', ':::')),
	test(rootPathFile('', ['::', '::', '::'], '', '::::')),

	test(rootPathFile(v, ['::'], '', 'v::')),
	test(rootPathFile(v, ['::', '::'], '', 'v:::')),
	test(rootPathFile(v, ['::', '::', '::'], '', 'v::::')),

	test(rootPathFile('', ['::'], f, '::f')),
	test(rootPathFile('', ['::', '::'], f, ':::f')),
	test(rootPathFile('', ['::', '::', '::'], f, '::::f')),
	test(rootPathFile(v, ['::'], f, 'v::f')),
	test(rootPathFile(v, ['::', '::'], f, 'v:::f')),
	test(rootPathFile(v, ['::', '::', '::'], f, 'v::::f')),

	test(rootPathFile('', [a, '::'], '', ':a::')),
	test(rootPathFile('', ['::', a], '', '::a:')),

	test(rootPathFile('', [a, '::', '::'], '', ':a:::')),
	test(rootPathFile('', ['::', a, '::'], '', '::a::')),
	test(rootPathFile('', ['::', '::', a], '', ':::a:')),

	test(rootPathFile('', [a, '::', b, '::'], '', ':a::b::')),
	test(rootPathFile('', ['::', a, '::', b], '', '::a::b:')),
	test(rootPathFile('', [a, '::', b, '::', c], '', ':a::b::c:')),

	/* Test current directory path elements 
	   (note: there is no way in MacOS to say ./././. Its always just : */
	
	test(rootPathFile('', [':'], '', ':')),

	test((rootPathFile('', [':'], f, P47), P47 = ':f')).

test :-
	subPath_test,
	rootPathFile_test,
	construct_test(macos), deconstruct_test(macos).
*/