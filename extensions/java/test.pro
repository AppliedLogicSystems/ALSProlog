:- consult('java.psl'), java_new('.').

t:- t1, t2, t3.

t1 :-
	java_call(test, hello, '()V', [], _),
	java_call(test, return_int, '()I', [], I), write(I), nl,
	java_call(test, return_double, '()D', [], D), write(D), nl,
	java_call(test, return_string, '()Ljava/lang/String;', [], S), write(S), nl.

t2 :-
	java_call(test, int_arg, '(I)V', [34], _),
	java_call(test, double_arg, '(D)V', [12.3432], _),
	java_call(test, string_arg, '(Ljava/lang/String;)V', ['prolog'], _),
	java_call(test, all_args, '(IDLjava/lang/String;)V', [-23, 3.14, 'a string'], _).

t3 :-
	java_call(test, return_list, '()[Ljava/lang/Object;', [], L), write(L), nl,
	java_call(test, return_sublist, '()[Ljava/lang/Object;', [], L2), write(L2), nl,
	java_call(test, list_arg, '([Ljava/lang/Object;)V', [[5, 3.14, 'an atom']], _).

e1 :-
	java_call(test, divzero, '()V', [], _).

e2 :-
	java_call(test, null_list, '()[Ljava/lang/Object;', [], _).

arg_error :-
	catch(java_call(X, hello, '()V', [], _), error(type_error(atom,X),[]), true),
	catch(java_call(1, hello, '()V', [], _), error(type_error(atom,1),[]), true),
	catch(java_call(1.0, hello, '()V', [], _), error(type_error(atom,1.0),[]), true)
	.