/*

Test - a simple, but colorful, test harness

test/1
test(list_of_subtests)

list_of_subtests is a list consisting of individual tests of the form:
	(subtest_goal, <list_of_results>)
Typically, <list_of_results> is a list of variable equality statements of the form
	[X == T1, X == T2, ....]
where these equalities have been created by running subtest_goal.

1.  test/1 uses copy_term/2 to make a copy copy_subtest_goal of subtest_goal and then executes copy_subtest_goal inside a catch.
    The use of copy_term/2 disconnects the variables occurring in copy_subtest_goal from those in any other subtest in the list_of_subtests.  This allows one to reuse the same variables in the different subtests.  This is recommended, since attempting to manually differentiate the variables from the various subtests is usually error-prone.

2.  test/1 writes reports for each subtest (on separate lines) to user_output, as follows:
a.  If subtest_goal fails, the subtest is said to fail, and:
FAIL: subtest_goal           is written in red;
b.  If subtest_goal succeeds, the <list_of_results> is executed in the environment created by the success of subtest_goal, and
b1. If all results on <list_of_results> succeed, the subtest is said to succeed, and:
OK: subtest_goal, <list_of_results>     is written in green;
b2. If at least one result on <list_of_results> fails, the subtest is said to fail, and:
FAIL: subtest_goal, <list_of_results>           is written in red;
3.  test(list_of_subtests) succeeds if and only if every subtest succeeds.

Simple Example Usage:

:- [test].

mytest :- test([
	(1 == 1),
	(X is 1 + 1, X == 2),
	(X = 1, X == 2),
	(test_call(X, Y), X == 2, Y == 6)
]).

test_call(X, Y) :- X is 1 + 1, Y is 2 + 4.

%Alternate version (recommended):
mytest :- test([
	(1 == 1),
	(X is 1 + 1, X == 2),
	(X = 1, X == 2),
	(test_call(X, Y), X == 2, Y == 6),
true]).

% The alternate version is convenient during test development, allowing addition of new subtests
% at the end of the subtests proper, before the 'true', without having to think about adjusting commas.

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
