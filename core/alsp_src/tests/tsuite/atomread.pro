/*
 * atomread.pro
 *
 * Test stream atoms.  The small buffer size on open stresses the read_upper
 * calls in sio.pro.
 */

test :-
    test('[here,is,a,rather,longish,list,in,an,atom,to,be,read,in,due,course,\'...\',a(structured(term))]',
	 T),
    writeq(T),
    nl.

test(A,T) :-
	atom(A),
	open(atom(A),read,[bufsize(16)],S),
	read_term(S,T,[attach_fullstop(true)]),
	close(S).
