/*
 * heapover.pro
 *	Copyright (c) 1993 Applied Logic Systems, Inc.
 *	Copyright (c) 1993 Motorola, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 5/21/93
 *
 * Description:
 *
 *	This file tests out the heap overflow catching mechanisms in
 *	ALS-Prolog.  The first test h1 should generate a heap overflow
 *	and return to the shell (the default handler).  The second test
 *	h2 generates a heap overflow, but catches it and writes out a
 *	message. The third test h3 generates an overflow, but attempts
<<<<<<< 1.3
 *	to keep going.  It should exit to the OS shell gracefully.
=======
 *	to keep going.  It should exit to the OS shell gracefully.  On
 *	certain machines, h3 does not exit prolog.  h4 will do what h3
 *	didn't.  It WILL exit to the OS shell (gracefully if everything
 *	is working right).
>>>>>>> /tmp/T4a000ZJ
 *
 *	Also included is h0.  h0 does not generate a heap overflow because
 *	the variable X in the call to h appears only once in the clause.
 *	Since it is not used elsewhere, the garbage compactor is free to
 *	collect all of the space associated with it.  h0 will perform
 *	many garbage collections, but never terminate.
 */

h0 :-	h(X).
h1 :-	h(X),
	i(X).
h2 :-	catch(	h(X),
		heap_overflow(_),
		(reset_wm_normal,write('Overflow caught'),nl)),
	!,
	i(X).
h3 :-	catch(	h(X),
		heap_overflow(_),
		h(X)),
	!,
	i(X).

h4 :-	hh(X), !, i(X).



h([X]) :- h(X).
i(X).
hh(X) :-
	catch(	h(X),
		heap_overflow(Goal),
		(   %reset_wm_normal,
		    write('Heap overflow caught on '),
		    write(Goal),
		    nl,
		    Goal = (user:h(Y)),
		    hh(Y)
		)).
