/*
 * retract3.pro		-- another test of retract
 *	Copyright (c) 1991 Applied Logic Systems, Inc.
 * Created: 6/10/91
 * Author: Kevin A. Buettner
 * Revision History:
 *
 * Description:
 *
 *	At the time that this program was written, this program caused core
 *	dumps on both the SPARC and M68k implementations.  I suspect that it
 *	will cause problems on other implementations as well (until the
 *	bug is fixed).  The portable system ran properly.
 *
 *	This program has a small database, p/2, the last clause of which is
 *	has one goal (this is very important).  p/2 will be indexed.  Running
 *	main will call p/2 with the first argument instantiated.  p/2 should
 *	succeed and remove the first clause (the one just matched).  After
 *	printing out the value of Y (it should be 1), failure will force the
 *	second (and last) clause of p/2 to be run.  It is at this point that
 *	the above mentioned systems core dump, but what should happen is that
 *	the message "In second p" will print out and the instantiation for
 *	X (which should be a variable).
 *
 *	Why are we core dumping?  The last clause for p/2 has one goal and
 *	so the compiler performs a special optimization for times it knows
 *	that the clause is determinate.  Execution will begin not at the
 *	start of the clause (where there is a little piece of code for
 *	dealing with the nondeterminate case), but a little ways into it.
 *
 *	When the retract is performed, the indexing for p/2 is removed and
 *	all choice points which point into the indexing are fixed up.
 *	Because the we don't consider the above mentioned optimization when
 *	fixing up the choice points, we fix up the choice point incorrectly
 *	and so when failure occurs, it fails to not quite the right spot
 *	which causes us problems.
 *	
 *	When this problem has been fixed, I will add another paragraph here
 *	stating that it has been fixed.
 *
 *	A (correct) sample run of this program appears as follows:
 *
 *		?- main.
 *		Y=1
 *		In second p
 *		Y=_180			(variable may be different)
 *
 *		no.
 *
 *	Note that this program ultimately fails.
 *
 *	On 6/12/91, wintcode.c was modified so this test will work.  Both
 *	the SPARC and 68k implementations were tested.
 */

p(a,1).
p(a,X) :- foo(X).

foo(X) :- write('In second p'),nl,write('X'=X),nl.

main :- p(a,Y), nukefirst, write('Y'=Y),nl, fail.

nukefirst :- retract(p(a,1)),!.
