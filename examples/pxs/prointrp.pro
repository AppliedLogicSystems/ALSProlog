/*---------------------------------------------------------------*
 *	prointrp.pro
 *	Copyright (c) 1989-90 Applied Logic Systems, Inc.
 *
 *	Prolog interpreter in Prolog
 *---------------------------------------------------------------*/

solvable(Goal) 
	:- 
	empty(Goal).
solvable(Goal) 
	:-
	select(Goal, SubGoal, Remaining),
	in_database(Rule),
	parts(Rule, Head, Body),
	match(SubGoal, Head),
	combine(Remaining, Body, NewGoal),
	solvable(NewGoal). 


match(X, Y) :- X = Y.


select( (SubGoal, Remaining), SubGoal, Remaining)
	:- !. 
select( SubGoal, SubGoal, true). 


combine( (A, B), Remaining, (A, Rest) ) 
	:- !,
	combine(B, Remaining, Rest).
combine(A, Remaining, (A, Remaining)).


parts((Head :- Body), Head, Body) 
	:- !.
parts(Fact, Fact, true).


in_database((Head :- Body)) 
	:- !, 
	clause(Head, Body).
in_database(Fact)
	:- clause(Fact, true).

