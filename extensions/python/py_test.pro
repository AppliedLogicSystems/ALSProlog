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

t :-
	int_test,
	float_test,
	string_test,
	string_test,
	tuple_test,
	list_test,
	eval_test,
	exec_test,
	call_test.
