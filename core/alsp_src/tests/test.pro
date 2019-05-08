/*

Test - a simple, but colorful, test harness

Example Usage:

:- [test].

mytest :- test([
	(1 == 1),
	(X is 1 + 1, X == 2),
	(X = 1, X == 2)
]).

Also, allows
	.... test([Note1/Goal1, Note2/Goal2, ....])
where Note1[2] will be printed out instead of Goal1[2] --
	-- simplifies output in cases like: libtests/read_examp_toks.pro.
*/

test(List) :-
	test(List, Result),
	Result.

test([], true) :- !.
test([], fail).
test([true | Tail], Result) :- !, test(Tail, Result).

test([Notation/Goal | Tail], Result) :-
	manage_test(Goal, Notation, Result, Tail),
	!, test(Tail, Result).

test([Goal | Tail], Result) :-
	manage_test(Goal, Result, Tail),
	!, test(Tail, Result).

manage_test(Goal, Result, Tail) :-
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
	write(O, '\033[0m'), nl(O).

manage_test(Goal, Notation, Result, Tail) :-
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
	write_term(O, Notation, [quoted(true), line_length(180)]
	),
	write(O, '\033[0m'), nl(O).

