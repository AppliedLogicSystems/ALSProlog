/*
 * rd_test.pro		-- test include and conditional read facilities
 *
 * Author: Kevin A. Buettner
 * Creation: 5-10-93
 * Revision History:
 */

test1 :-
	open(rd_t1,read,T1),
	read_write_terms(T1),
	close(T1).

test1p :-
	assert(p),
	open(rd_t1,read,T1),
	read_write_terms(T1),
	close(T1),
	retract(p),
	!.

test1q :-
	assert(q),
	open(rd_t1,read,T1),
	read_write_terms(T1),
	close(T1),
	retract(q),
	!.

test1qa :-
	assert(q),
	assert(a),
	open(rd_t1,read,T1),
	read_write_terms(T1),
	close(T1),
	retract(q),
	retract(a),
	!.

test1r :-
	assert(r),
	open(rd_t1,read,T1),
	read_write_terms(T1),
	close(T1),
	retract(r),
	!.

test2 :-
	open(rd_t2,read,T2),
	read_write_terms(T2),
	close(T2),
	!.

read_write_terms(Stream) :-
	read(Stream,Term),
	read_write_terms(Term,Stream).

read_write_terms(EOF,Stream) :-
	nonvar(EOF),
	EOF = end_of_file,
	!.
read_write_terms(Term,Stream) :-
	writeq(Term),
	nl,
	read_write_terms(Stream).
