/*
 * retract1.pro		-- program to test out retract
 *	Copyright (c) 1990 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 2/18/90
 *
 *
 * This program should be run both "straight up" and under the debugger.
 * We should get the same solutions in both cases.  When stepping through
 * the debugger, note any strange interaction with any of the builtins.
 *
 */





/*
 * doit1 builds p/1 consisting of 3 facts: p(a). p(b).  and p(c).
 * It calls p, but then removes the b clause.  Upon bactracking, we will
 * have to backtrack through a deleted clause.  Under ALS-Prolog at this time,
 * the list written out should be
 *	[a,c].
 */

doit1 :-
	buildp([a,b,c]),
	findall(X,(p(X), remb), L),
	write('L'=L),nl.

/*
 * doit2 builds p/1 consisting of just p(a) and p(b).
 * It will also have to backtrack through a deleted clause, but this
 * time there is no next clause.  Under ALS-Prolog, the list written out
 * should be
 *	[a].
 */

doit2 :-
	buildp([a,b]),
	findall(X,(p(X), remb), L),
	write('L'=L),nl.

remb :- retract(p(b)), !.
remb.

buildp(L) :- abolish(p,1), member(E,L), assertz(p(E)), fail.
buildp(_).
