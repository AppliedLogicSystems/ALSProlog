/*
 * gvtest1.pro
 *	Copyright (c) 1993 Applied Logic Systems, Inc.
 *	Copyright (c) 1993 Motorola, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 6/21/93
 *
 * This program was created during a discussion with Alan Newman.  It was
 * supposed to demonstrate that the determinate portion of a global variable
 * stayed instantiated, but that the non-determinate portion did not.  It
 * does in fact do this.  But it also demonstrated that there was a bug with
 * the ALS-Prolog global variable mechanism.  The bug was located in gv.c.
 * There is a loop in gv.c which fixes up the HB pointers on the trail in
 * order to make the structures pointed at by the global variables safe (i.e.,
 * we don't want dangling references after a failure).  The loops which
 * performed this operation did indeed do what they were supposed to do
 * except that they skipped fixing up the first (top most) choice point
 * which needed fixing up.  Many times this was not a problem. For example,
 * the addition of the single clause
 *	testit.
 * after the first clause for testit would result in the program behaving
 * properly.
 *
 * Although the bug has been fixed, this little test should be run every now
 * and then to make sure that the bug has not been reintroduced.  Also it
 * does test to make sure that the portion of the structure which should
 * be uninstantiated after failure is in fact uninstantiated.
 *
 * To run this test, run the goal
 *	test
 * at the prolog prompt.  It will respond with ok or not_ok.  If the reponse
 * was not_ok, the goal
 *	get_g(X)
 * might be useful in determining what went wrong. You might be surprised to
 * see that X is bound to zebra(1).  If everything works properly, X should
 * be bound to f(a,_).
 * 
 */

%%
%% Make global variable access predicates (set_g/1 and get_g/1).
%%

:- make_gv('_g').

%%
%% test/0 is the top level goal to run this test
%%

test :-
	testit.		%% testit should fail causing the next clause to run.
test :-
    	garb,		%% create garbage
	(get_g(f(a,X)), var(X), write(ok),nl ; write(not_ok),nl).


%%
%% garb/0 creates a little bit of garbage on the heap.  It creates the term
%% 	zebra(1)
%% on the heap.  If things are not working right, the global variable might
%% well point at this garbage after the execution of test/0 (above).
%%

garb :-
	X=zebra(1).

%%
%% testit/0 will call foo (thus setting the global variable) and then cut and
%% fail.  This will cause testit to fail in addition to untrailing those
%% portions of the term pointed at by the global variable which were created
%% in a non-determinate manner.
%%

testit :-
	foo,
	!,
	fail.

%%
%% foo/0 creates a structure f/2 whose second argument is instantiated by
%% a call to bar/1.  (Its first argument is set in a determinate fashion).
%% We set the global variable to point at this structure.  Note that the call
%% to bar will leave a choice point which is not cut prior to setting the
%% global variable.  This means that this portion of the of the structure
%% will become uninstantiated upon failure.
%%

foo :- 
	X=f(Y,Z),
	Y = a,
	bar(Z),
	set_g(X).

%%
%% bar/1 contains two clauses and no cuts.  If called with a variable, it will
%% leave a choice point.
%%

bar(1).
bar(2).
