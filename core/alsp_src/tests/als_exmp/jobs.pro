
/*
 * jobs.pro  -- a constraint solution shell
 *
 * Copyright (c) 1986, 1988 by Applied Logic Systems
 *
 * Author:  Kenneth A. Bowen
 * Revision History: 
 *      mm/dd/yy,       Name	--	Reason
 *      mm/dd/yy,       Name	--	Reason
 */


/*
 *	Description:
 *
 * 	Full "jobs" puzzle from Wos, Overbeek, Lusk, & Boyle, p. 55
 *
 */


\+(X) :- X,!,fail.
\+(X).

go :- jobs(L), write(L), nl.


/* Knowledge */

/*
Times for this ordering: bps: 1.99min;  C-Prolog: 3.7minutes; (factor=1.8)
job(guard).
job(nurse).			File load times: this file + constr
job(telephone_operator).	bps: 1.0sec
job(police_officer).		C-Prolog: 1.25 sec
job(teacher).			  (factor=1.25)
job(actor).
job(boxer).
job(chef).
*/

% Times for this ordering: bps: 27.6sec; C-Prolog: 52.2sec (factor = 1.9)
job(guard).
job(teacher).
job(chef).
job(police_officer).
job(nurse).
job(telephone_operator).
job(actor).
job(boxer).

person(roberta).
person(thelma).
person(steve).
person(pete).

female(roberta).
female(thelma).
male(steve).
male(pete).

not_college_educated(pete).

at_most_2(A, B, C) :-
    A=B; A=C; B=C.


can_be_husband(X, Y) :- male(X), female(Y).

college_educated(X) :- \+(not_college_educated(X)).

can_golf_together(roberta, Person2, Person3)
    :-
    diff(roberta, Person2), diff(roberta, Person3),
    diff(Person2, Person3).

:- dynamic(has_job/2).

/* Local Constraints */

local_constraint((
diff(Job1,Job2) :-
has_job(Person1, Job1), has_job(Person2, Job2),diff(Person1,Person2))).

local_constraint((
at_most_2(Job1, Job2, Job3) :- 
	has_job(Person, Job1), has_job(Person, Job2), has_job(Person, Job3),
	at_most_2(Job1,Job2,Job3)
)).

local_constraint((
can_golf_together(roberta, Person2, Person3) :-
    has_job(Person2, chef), has_job(Person3, police_officer))).

local_constraint((
male(Person) :- has_job(Person, nurse))).

local_constraint((
male(Person) :- has_job(Person, actor))).

local_constraint((
can_be_husband(Person1, Person2) :-
	has_job(Person1, telephone_operator), has_job(Person2, chef))).

?- dynamic(husband/2).

local_constraint((
male(Person) :- husband(Person, Someone))).

local_constraint((
false :- has_job(roberta, boxer))).

local_constraint((
college_educated(Person) :- has_job(Person, teacher))).

local_constraint((
college_educated(Person) :- has_job(Person, police_officer))).

local_constraint((
college_educated(Person) :- has_job(Person, nurse))).

/* Global Constraints */

global_constraint((
has_job(Person, Position) :- job(Position))).


/* Conjecture Method */

conjecture(has_job(X, Y), Established) 
    :- 
    person(X), job(Y),
    \+(member(has_job(X, Y), Established)).


jobs(Result) :-
	solve([
	  has_job(roberta, Roberta_Job1), has_job(roberta, Roberta_Job2),
	  has_job(pete, Pete_Job1), has_job(pete, Pete_Job2),
	  has_job(thelma, Thelma_Job1), has_job(thelma, Thelma_Job2),
	  has_job(steve, Steve_Job1), has_job(steve, Steve_Job2)
	  ], 
		Result).

/*   solve(Goals, Established) */

solve(Goals, Result)
	:-
	InitialTime is cputime,
	solve(Goals, [], Result),
	DeltaTime is cputime-InitialTime,
	write('Time='),write(DeltaTime), nl.

solve([], Established, Established)
    :-
%    write('glc>'),write(Established),nl,
    satisfy_global_constraints(Established).

solve([Goal | Goals], Established, Output)
	:-
%	write('>'),write(Established),nl,
%	write('>'),write(Established),nl,
	conjecture(Goal, Established),
	satisfy_local_constraints([Goal | Established]),
	solve(Goals, [Goal | Established], Output).
	
satisfy_local_constraints(Established)
	:-
	\+(exist_failed_local_constraint_for(Established)).

exist_failed_local_constraint_for(Established)
	:-
	local_constraint((Constraint_Head :- Constraint_Body)),
	satisfy(Constraint_Body, Established),
	\+(satisfy(Constraint_Head, Established)).
%	write('failed: '),
%	write(Constraint_Head),write('<--'),write(Constraint_Body),nl,
%	write('::'),write(Established),nl.

satisfy_global_constraints(Established)
    :-
    \+(exist_failed_global_constraint_for(Established)).

exist_failed_global_constraint_for(Established)
	:-
	global_constraint((Constraint_Head :- Constraint_Body)),
	satisfy(Constraint_Body, Established),
	\+(satisfy(Constraint_Head, Established)).


satisfy(true, Established).	

satisfy((Condition, Rest), Established)
	:- 
	member(Condition, Established), 
	satisfy(Rest, Established).

satisfy((Condition, Rest), Established)
	:-
	call(Condition), 
	satisfy(Rest, Established).

satisfy(Condition, Established) :-
 	\+(Condition = (_,_) ),
	member(Condition, Established).

satisfy(Condition, Established)
	:-
	\+(Condition = (_,_) ),
	call(Condition).


fails(Condition)
	:-
	\+(call(Condition)).

	
member(Item, [Item | _]).

member(Item, [_ | Tail])
	:-
	member(Item, Tail).

same(X,X).

diff(X, Y) :- X \= Y.
