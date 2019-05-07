/*

Test - a simple, but colorful, test harness

Example Usage:

:- [test].

mytest :- test([
	(1 == 1),
	(X is 1 + 1, X == 2),
	(X = 1, X == 2)
]).

*/

test(List) :-
	test(List, Result),
	Result.

test([], true) :- !.
test([], fail).
test([true | Tail], Result) :- !, test(Tail, Result).
test([Goal | Tail], Result) :-
	O = user_output,
	%% (atom(Goal) -> printf(O, '>>>> BEGIN %t <<<<\n', [Goal]) ; true ),
	(
		copy_term(Goal, RGoal),
		catch(RGoal, Error, (write(O, 'Uncaught Error: '), write(O, Error), nl(O), fail)),
		write(O, '\033[32m  OK: ')
		;
		Result=fail,
		write(O, '\033[31mFAIL: ')
	),
	write_term(O, Goal, [quoted(true), line_length(180)]
	),
	write(O, '\033[0m'), nl(O),
	!, test(Tail, Result).
