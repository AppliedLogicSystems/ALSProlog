			/*-------------------------------------
			 |		Test Info Database
			 *------------------------------------*/

%%----------------------------------------------------------------------
%%	test_info(TestID, TestFile, TestMod, TestStartCall, TestDescrip),
%%----------------------------------------------------------------------

module als_testing.

	%% Standard ALS examples:

test_info(bench, bench, user, main, 'The benchpress example.').

test_info(jobs, jobs, user, go, 'The jobs example.').

test_info(nrev, nrev, user, nrev0(100,30), 'The nrev example.').

test_info(queens, queens, user, all_queens, 'The 8 queens example.').

	%% The tsuite directory:

test_info(compare, compare, user, test,
			'compare/3 testing.').

test_info(copylnes, copylnes, user, test,
			'get_line and put_line testing.').

test_info(counter, counter, user, bench,
			'counter tests and benchmarks.').

test_info(ident, ident, user, test,
			'tests for == and \==.').

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

test_info(testmath, testmath, user, test,
			'test of math functions.').


test_info(par1, par1, user, main,
			'tests for global variables, trap mechanism, alarm & interrupts.').

test_info(par2, par2, user, main,
			' Producers and consumers without the timer.').

test_info(par3, par3, user, main,
			' Consumers and producers with time slices.').

test_info(par4, par4, user, main,
			'  Producers and consumers (for primes) with added buffering.').

test_info(testeoln, testeoln, user, test,
'test of all eoln modes for all builtin predicates\n\
 involved with eolns and a test of stream buffer sizes.').

endmod.
