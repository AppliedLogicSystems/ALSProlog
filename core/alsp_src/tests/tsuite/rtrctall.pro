/*
 * retractall.pro
 *
 *	Craig Thornley called me about a bug in our system which manifested
 *	itself in the following code.  He was defining a retractall
 *	procedure which removes all clauses in a certain procedure whose
 *	heads unify with the argument passed in to retractall.  The problem,
 *	apparently, was not really with retract, but with the
 *	module closure mechanism working properly in xconsult.
 *
 *	The bug has been fixed, but I am retaining this code because
 *	it is a small test which is probably worth running every now
 *	and then to make sure it still works properly.
 *
 *	To run this program, get into ALS-Prolog, consult this file
 *	and run the goal test.  It should not fail.  After running
 *	this goal do a listing on p/2.  There should only be
 *		p(2,y)
 *	remaining.
 *
 */

module foobar.
:- module_closure(retractall,1).

retractall(Mod,Fact) :-
	Mod:retract(Fact),
	fail.
retractall(Mod,Head) :-
	Mod:retract((Head:-_)),
	fail.
retractall(_,_).
endmod.

module barfoo.
use foobar.

export test/0.

test :-
	assert(p(1,x)), assert(p(2,y)),
	assert((p(1,X) :- q(X))),
	retractall(p(1,_)).

endmod.
