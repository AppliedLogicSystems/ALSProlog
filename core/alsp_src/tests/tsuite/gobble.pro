/*
 * gobble.pro		-- test A/E stack compaction performed in cut
 *	Copyright (c) 1992 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 *
 * Description:
 *	This test was written when it was discovered that stack space was
 * not being reclaimed upon the last call of a determinate semicolon (on
 * the left of the semicolon). Skeletally, the problem code looks like
 * this:
 *
 *	head :- guard, !, goals ; othergoals.
 *
 * The problem is (or was) that ';'/2 is essentially interpreted.  When the
 * left hand side of the semicolon is performed, a copy of the argument frame
 * to semicolon is made.  When the cut is performed, the original frame is/was
 * garbaged, but not collected.  The problem still exists (to an even larger
 * degree) when the code is transformed via xconsult (or something similar).
 * In fact the problem will exist whenever it is possible to cut back to
 * a parent environment of the current one.
 *
 * This problem has been fixed by calling a stack compacting version of
 * cut.
 *
 * The following code will test the stack compaction code to a limited
 * degree.  It should be extended to further exercise cut.
 *
 * gobble/1 will create a bunch of choice points, thus gobbling up both
 * the choice point/trail stack and the argument/environment stack.
 *
 * test/1, test3/1, and test4/0 call statistics, gobble, and statistics.
 * The value of E in the last statistics should be greater than or equal
 * to the value of E in the first statistics.  If it is less than, then
 * cut is not properly compacting the stack.  The single parameter to
 * test/1 and test3/1 should be a positive integer specifying the number
 * of stack frames to gobble up.
 *
 * test2(N) takes a positive integer N which is the number of times
 * to tail recurse on the left of the semicolon.  The value of E should
 * be constant in the statistics printed throughout.  Procedures in real
 * applications resembling this test are worrisome in that if no compaction
 * is done, not only does the Prolog process consume stack space, but it
 * also consumes heap space which can not be collected since the unreferenced
 * stack frames point at portions of the heap.
 *
 *
 */


test(N) :- statistics, (gobble(N),statistics, !, statistics ; true) .

gobble(N) :- N > 0, NN is N-1, gobble(NN).	%% No cut!!
gobble(_).


test2(N) :- N>0, !, NN is N-1, statistics, test2(NN) ; write(done),nl.


test3(N) :- test3(N,a,b,c,d,e,f,g,h,i,j,k).
test3(N,A,B,C,D,E,F,G,H,I,J,K) :-
	statistics, (gobble(N), !, statistics ; true).


test4 :-
	statistics, (gobble(100), !, statistics; true).
