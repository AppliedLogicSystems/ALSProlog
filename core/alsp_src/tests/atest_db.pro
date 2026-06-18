			/*-------------------------------------
			 |		Test Info Database
			 *------------------------------------*/

%%----------------------------------------------------------------------
%%	test_info(TestID, TestFile, TestMod, TestStartCall, TestDescrip),
%%----------------------------------------------------------------------

module als_testing.

	%% Standard ALS examples:


test_info(stack_overflow_test, stack_overflow_test, user, test,
	'test correct handling of stack-overflows').

test_info(freeze_test, freeze, user, test_freeze, 'tests for freeze.').

test_info(bench, bench, user, main, 'The benchpress example.').

test_info(jobs, jobs, user, go, 'The jobs example.').

test_info(nrev, nrev, user, nrev0(200,100), 'The nrev example.').

test_info(queens, queens, user, all_queens, 'The 8 queens example.').

	%% The tsuite directory:

test_info(blt_atom, atoms, user, test_blt_atom, 'blt_atom testing.').

test_info(join_split_path, join_split_path_test, user, test,
		'join_path and split_path tests').


test_info(compare, compare, user, test,
			'compare/3 testing.').

test_info(copylnes, copylnes, user, test,
			'get_line and put_line testing.').

test_info(counter, counter, user, bench,
			'counter tests and benchmarks.').

test_info(ident, ident, user, test,
			'tests for == and \==.').

test_info(put_test, put_test, user, test, 'test for put/get_*').

 test_info(numio, numio, user, test,
			'tests for get_number/3 and put_number/3.').

test_info(retract1, retract1, user, doit1,
			'  test out retract.').

test_info(retract2, retract2, user, main,
			'  test interaction of gvs with retract & gc.').

test_info(retract3, retract3, user, main,
			'another test of retract.').

test_info(rtrctall, rtrctall, user, (test,listing(p/2)),
			'another test of retract.').

test_info(syms, syms, user, test,
			'another test of retract.').

test_info(testcopy, testcopy, user, testnew,
			'test of I/O.').

test_info(testmath, testmath, user, autotest,
			'test of math functions.').


test_info(par1, par1, user, main,
			'tests for global variables, trap mechanism, alarm & interrupts.').

test_info(par2, par2, user, main,
			' Producers and consumers without the timer.').

test_info(par3, par3, user, main,
			' Consumers and producers with time slices.').

test_info(par4, par4, user, main,
			'  Producers and consumers (for primes) with added buffering.').

test_info(filepath_test, filepath_test, user, test_filepath, 'tests for file system paths.').

test_info(fsunix_mswin32_test, fsunix_mswin32_test, user, test_fsunix_mswin32, 'tests for misc filesystem functions.').

% test_info Format:
% test_info(TestID, TestFile, TestMod, TestStartCall, TestDescrip),

% Library tests:

test_info(read_html_tokens_test, read_examp_toks, user, read_toks, 
	'read html tokens from file versions of example.com and sample_awstats.html').

test_info(parse_examples_html_test, parse_examples_html, user, parse_html, 
	'parse pxml terms from file versions of example.com and sample_awstats.html').

test_info(library_strings_test, strings_test, user, test_strings_lib,
	'tests of string utility functions.').

test_info(library_miscatom_test, miscatom_test, user, test_miscatom_lib,
	'test of miscellaneous atom-related predicates').

test_info(library_arithx1_test, arithx1_tests, user, test_arithx1,
	'test of miscellaneous arithmetic predicates').

test_info(library_rows_cols_test, rows_cols_test, user, test_cols,
	'test of column formatting predicates').

% end--Library tests

% TODO LP64: Restore eoln test once GC is fixed and it no longer core dumps.
%test_info(testeoln, testeoln, user, test,
%'test of all eoln modes for all builtin predicates\n\
% involved with eolns and a test of stream buffer sizes.').
test_info(testeoln, testeoln, user,
	(getenv('LP64_PARTIAL_TEST', _)
	 -> printf(error_stream, 'TODO: restore testeolnx\n', [])
	; test),
'test of all eoln modes for all builtin predicates\n\
 involved with eolns and a test of stream buffer sizes.').

endmod.
