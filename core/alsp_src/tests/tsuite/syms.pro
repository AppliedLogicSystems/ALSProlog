/*
 * syms.pro	-- make lots of uias and intern them as symbols
 *
 *	Copyright (c) 1991 Applied Logic Systems, Inc.
 *
 *
 * Author: Kevin A. Buettner
 * Creation: 6/3/91
 * Description:
 *	This program creates lots of symbols.  It is designed to stress the
 *	routines which allocate new symbol table space.  It can be run by
 *	consulting this file and then entering 'test.' at the query prompt.
 *
 * Note:
 *	The maximum symbol table size on the 88k is 65521.  test1 should be
 *	run for the 88k.  On other machines, running test should be sufficient.
 *	testout will test the behavior of ALS-Prolog running out of symbol
 *	space.
 *
 * Timings:
 *
 *			| test	| test again	| test1	| test1 again	| out at
 *			+=======+===============+=======+===============+=======
 *    SPARCstation IPC  |  38.6	|  28.3		|  24.8	|  17.3		| 522000
 *			+-------+---------------+-------+---------------+-------
 *    Sun 3/50		|   209	| 158		| 133	| 98.3		|
 *			+-------+---------------+-------+---------------+-------
 *    NeXT		|  22.5	| 17.0		| 14.5	| 10.3		|
 *			+-------+---------------+-------+---------------+-------
 *
 */


test :-
	test(100000).

test1 :- 
	test(64000).

testout :-
	test(4000000).

test(N) :-
	X is cputime,
	make_syms(0,N),
	Y is cputime-X,
	write('Elapsed time for '), write(N),
	write(' symbols = '), write(Y), nl.


make_syms(N,N) :- !.
make_syms(I,N) :-
	make_new_sym(I,NewSym),
	write_sometimes(I,NewSym),
	NI is I+1,
	make_syms(NI,N).

make_new_sym(0,sym0) :- !.
make_new_sym(N,Sym) :-
	int_to_list(N,[],SymList),
	name(SymUIA,[0's,0'y,0'm | SymList]),
	functor(Sym,SymUIA,0).

int_to_list(0,L,L) :- !.
int_to_list(N,A,L) :-
	Digit is (N mod 10) + 0'0,
	NRest is N div 10,
	int_to_list(NRest,[Digit | A],L).

write_sometimes(N,Sym) :-
	N mod 2000 =:= 0,
	!,
	writeq(Sym),
	nl.
write_sometimes(_,_).
