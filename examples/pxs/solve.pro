/*---------------------------------------------------------------*
 *	solve.pro
 *	Copyright (c) 1986-90 Applied Logic Systems, Inc.
 *	Author: Kenneth A. Bowen
 *
 *	Simple Expert System Shell
 *---------------------------------------------------------------*/

:-op(1200, yfx, '::').
:-op(700, yfx, if).
:-[-asking].

sweep_in(File)
	:-
	see(File),
	process,
	seen,
	write('Swept in '),write(File),nl.
sweep_in(File)
	:-
	seen,
	write('Possible errors sweeping in '),write(File),nl.

process 
	:-
	read(Clause),
	dispatch(Clause).

dispatch(end_of_file) :- !.

dispatch(QualifiedClause) 
	:-
	QualifiedClause = (Clause :: Confidence),  !,
	assertz(Clause),
	Clause =.. [Op, Head, Body],
	work_on(Op, Head, Body, Clause, Confidence),
	process.

dispatch(UnQualifiedClause) 
	:-
	assertz(UnQualifiedClause),
	process.

dispatch(UnknownItem) 
	:-
	write(unknownItem=UnknownItem),nl,
	process.

work_on(':-', Head, Body, Clause, Confidence) 
	:-
	clause(Head, Body, Ptr),
	assertz(conf(Ptr, Confidence)).

work_on(Op, Head, Body, Clause, Confidence) 
	:-
	clause(Clause, true, Ptr),
	assert(conf(Ptr, Confidence)).

solve(true, 1.0, []) :-!.

solve( (A,B), Conf, Proof) 
	:-
	solve(A, C1, P1),
	solve(B, C2, P2),
	conj_conf(C1, C2, Conf),	% confidence calculation
	append(P1, P2, Proof).

solve(A, C, [(A if B) | P2]) 
	:-
	clause(A, B, Ptr),
	conf(Ptr, C1),
	solve(B, C2, P2),
	rule_conf(C1, C2, C).	% confidence calculation

solve(A, C, [asked(A)]) 
	:-
	ask_about(A),
	obtain_conf(C).	% determine confidence

solve(not(A), C, [negate(A,P1)] ) 
	:-
	solve(A, C1, P1),
	!,
	C1 < 0.3,
	neg_conf(C1, C).		%confidence calculation

solve(not(A), 1.0,  [not(A)-exhaustion]) :-!.

solve(A, 1.0, [A-builtin]) 
	:-
	clause(A, B, Ptr),
	not(conf(Ptr, C1)),
	A.

decide_neg([], 0).
decide_neg([sp(A, CA, _) | RestSubProofs], CurConf)
	:-
	CA < 0.3,
	max(CurConf, CA, NewConf),
	decide_neg(RestSubProofs, NewConf).

conj_conf(X, Y, Z) 
	:- 
	min(X, Y, Z).
rule_conf(X, Y, Z) 
	:- 
	max(X, Y, Z).
neg_conf(X, Y) 
	:- 
	Y is 1.0 - X.
obtain_conf(C) 
	:-
	write('What is your confidence in that answer?'),
	read(C).

min(X, Y, X) 
	:- 
	X =< Y.
min(X, Y, Y) 
	:- 
	X > Y.

max(X,  Y,  X) 
	:- 
	Y =< X.
max(X,  Y,  Y) 
	:- 
	Y > X.

act_on(yes).
act_on(y). 

