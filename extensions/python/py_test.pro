:- ['python.psl'].

int_test :-
	py_eval('1', X1), nonvar(X1), X1 = 1.
	
float_test :-
	py_eval('1.0', X1), nonvar(X1), X1 = 1.0.
	
string_test :-
	py_eval('\'abc\'', X1), nonvar(X1), X1 = abc.

tuple_test :-
	py_eval('(1,2,3)', X1), nonvar(X1), X1 = (1, 2, 3).

list_test :-
	py_eval('[1,2,3]', X1), nonvar(X1), X1 = [1, 2, 3].

eval_test :-
	py_exec('a = 1'),
	py_eval('a', X), nonvar(X), X = 1.

exec_test :-
	py_exec('b = 1').

call_test :-
	py_call('__builtin__', abs(-3), X1), nonvar(X1), X1 = 3.

arg_test :-
	py_call(test_util, arg_test(1), X1), nonvar(X1), X1 = (1, 0, '()', []),
	py_call(test_util, arg_test(1, 2), X2), nonvar(X2), X2 = (1, 2, '()', []),
	py_call(test_util, arg_test(a=1), X3), nonvar(X3), X3 = (1, 0, '()', []),
	py_call(test_util, arg_test(a=1,b=2), X4), nonvar(X4), X4 = (1, 2, '()', []),
	py_call(test_util, arg_test(b=1,a=2), X5), nonvar(X5), X5 = (2, 1, '()', []),
	py_call(test_util, arg_test(1,2,3), X6), nonvar(X6), X6 = (1, 2, (3), []),
	py_call(test_util, arg_test(1,2,3,4), X7), nonvar(X7), X7 = (1, 2, (3,4), []),
	py_call(test_util, arg_test(1,x=2), X8), nonvar(X8), X8 = (1, 0, '()', [(x,2)]),
	py_call(test_util, arg_test(1,x=2,y=3), X9), nonvar(X9), X9 = (1, 0, '()', [(x,2),(y,3)]),
	py_call(test_util, arg_test(1,x=2,y=3,z=4), X10), nonvar(X10), X10 = (1, 0, '()', [(z,4),(x,2),(y,3)]).
	
t :-
	py_exec('import sys'),
	py_exec('sys.path.insert(0,\'.\')'),
	py_exec('import test_util'),
	int_test,
	float_test,
	string_test,
	string_test,
	tuple_test,
	list_test,
	eval_test,
	exec_test,
	call_test,
	arg_test.

