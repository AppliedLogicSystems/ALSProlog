%#!alspro -r test

%load tcl package

test :- load_slib(tclintf),
		tcl_new(i),
		%%tcl_eval(i, 'source "prolog test.tcl"', ''),
		run_tests,
		%% tcl_eval(i, 'test', 1),
		tcl_delete_all.

run_tests :-
	test(G, ER),
	printf('Testing %t\n', [G]),
	run_test(G, R),
	check_result(R, ER),
	fail.
run_tests :-
	printf('All tests completed.\n').		 

run_test(G, R) :-
	catch((call(G), R = success), Exception, R = exception(Exception)), !.
run_test(G, failure).

check_result(success, success) :- !.
check_result(failure, failure) :- !.
check_result(Exp, Exp) :- !.
check_result(R, ER) :- !,
	printf('test error: expection %t but got %t\n', [ER, R]).

/* tcl_new/1 tests */

/* Test tcl_new error conditions */
test(tcl_new(1), exception(error(type_error(atom_or_variable,1),_))).
test(tcl_new(1.0), exception(error(type_error(atom_or_variable,1.0),_))).
test(tcl_new(foo(1)), exception(error(type_error(atom_or_variable,foo(1)),_))).
test(tcl_new([1]), exception(error(type_error(atom_or_variable,[1]),_))).
test((tcl_new(a), tcl_new(a)),
	 exception(error(permission_error(create,tcl_interpreter,a),_))).

/*

This currently crashes the system.

overflow_tcl_new :-
	tcl_new(X),
	overflow_tcl_new.

test(overflow_tcl_new, exception(error(resource_error(tcl_memory), _))).
*/

/* Test that automatically generated names are different */
test((tcl_new(A), tcl_new(B), A \= B), success).

/* tcl_delete/1 tests */

test(tcl_delete(V), exception(error(type_error(atom,V),_))).
test(tcl_delete(1), exception(error(type_error(atom,1),_))).
test(tcl_delete(1.0), exception(error(type_error(atom,1.0),_))).
test(tcl_delete(foo(1)), exception(error(type_error(atom,foo(1)),_))).
test(tcl_delete([1]), exception(error(type_error(atom,[1]),_))).
test(tcl_delete(not_tcl_name),
	exception(error(domain_error(tcl_interpreter, not_tcl_name), _))).

/* Test that tcl_delete really deletes the interpretor name */
test((tcl_new(b), tcl_delete(b), tcl_new(b)), success).

/* tcl_eval/3 tests */

/* check interger and floating point conversion. */

test((tcl_eval(i, 'set x foo', X), X = foo), success).

test((tcl_eval(i, 'expr 1', X), X = 1), success).
test((tcl_eval(i, 'expr -1', X), X = -1), success).

/* test prolog min/max integer boundry */
test((tcl_eval(i, 'expr 134217727', X), X = 134217727), success).
test((tcl_eval(i, 'expr 134217728', X), X = 134217728.0), success).
test((tcl_eval(i, 'expr -134217727', X), X = -134217727), success).
test((tcl_eval(i, 'expr -134217728', X), X = -134217728.0), success).

/* test 32-bit min/max integer */
test((tcl_eval(i, 'expr 2147483647', X), X = 2147483647.0), success).
test((tcl_eval(i, 'expr -2147483648', X), X = -2147483647.0), success).

/* test floating point */
test((tcl_eval(i, 'expr 1.0', X), X = 1.0), success).
test((tcl_eval(i, 'expr -1.0', X), X = -1.0), success).
test((tcl_eval(i, 'expr 1.23456789e110', X), X = 1.23456789e110), success).
test((tcl_eval(i, 'expr 1.23456789e-110', X), X = 1.23456789e-110), success).

/* nan? infinity? */

/* check list conversion */

test((tcl_eval(i, 'list a b c', X), X = [a, b, c]), success).
test((tcl_eval(i, 'list 1 2 3', X), X = [1, 2, 3]), success).
/* note how doubles are just strings below. */
test((tcl_eval(i, 'list 1.0 2.0 3.0', X), X = ['1.0', '2.0', '3.0']), success).
/* note that sublist below is a string */
test((tcl_eval(i, 'list 1 {a b} 3', X), X = [1, 'a b', 3]), success).
test((tcl_eval(i, 'list 1 [list a b] 3', X), X = [1, [a, b], 3]), success).

/* check struct conversion */

test((tcl_eval(i, 'struct a b c', X), X = a(b,c)), success).

/* test variable conversion */

test((tcl_eval(i, 'var', X), var(X)), success).
test((tcl_eval(i, 'var x', X), var(X)), success).

/* test evaluation of lists */

test((tcl_eval(i, [expr, 1], X), X = 1), success).
/* note that sublist below is a string */
test((tcl_eval(i, [list, 1, [a, b], 3], X), X = [1, 'a b', 3]), success).


/* out of memory tests memory

- a zillion tcl interpreters
- zillions of tcl_objects
*/

/*	
test(tcl_new(foo(3)), exception(error(
test(R, (tcl_new(X), tcl_delete(X)), [atom(X), 
tcl_new(i), tcl_new(i2), tcl_delete(i), tcl_delete(i2)
tcl_new(i), tcl_new(i),
tcl_new(A), tcl_new(B) -> exception
tcl_new(A), tcl_new(B), A /= B

test(exception(R), tcl_delete(X), ...).
test(exception(R), tcl_delete(1), ...).
test(exception(R), tcl_delete(1.0), ...).
test(exception(R), tcl_delete([1, 2, 3]), ...).
test(exception(R), tcl_delete(struct(a, b)), ...).

test('tcl_delete of non-existant interpretor', exception(R),
	tcl_delete(foobar), range_error(...)). 
	
test(fail, faulure).
test(true, success).
test(throw(foo), exception(foo)).
test(append([1], [2], X), success(X = [1, 2])).
test(append(X, Y, [1, 2]),
	success(3, [X = [[], [1], [1, 2]], Y = [[1, 2], [2], []])).
*/
