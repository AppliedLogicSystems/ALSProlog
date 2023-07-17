/*
 * atoms.pro	-- tests predicates from blt_atom.pro
 *	Copyright (c) 2023 Applied Logic Systems, Inc.
 *
 * Created:	7/16/2023
 * Author:	Ken Bowen
 *
 * Consult this file and run test/0.
 */
:-[test].

test_blt_atom :-
	test([
	test_atom_concat,
	test_sub_atom,
	test_split_atom_4,
	test_split_atom_3,
	test_number_chars,
	test_number_codes,
	test_term_chars,
	test_term_codes,
true]).

test_atom_concat :-
	atom_concat(abc, def, H0),
	H0=abcdef,
	atom_concat(abc, H1, abcdef),
	H1=def,
	atom_concat(K, def, abcdef),
	K=abc,
true.


test_sub_atom :-
	sub_atom(abcdefgh, F, H, J, cde),
	F=2, H=3, J=3,
	sub_atom(abcdefgh, 0, 4, _, S),
	S = abcd,
	sub_atom(abcdefgh, 3, 4, _, S2),
	S2=defg,
	sub_atom(abcdefgh, _, 4, 0, S3),
	S3=efgh,
true.

test_split_atom_4 :-
	atom_split('ar**bnghk', '**', L0, R0),
	L0=ar,  R0=bnghk,
	atom_split('ar**bnghk', S1, ar, bnghk),
	S1 = '**',
	atom_split('ar**bnghk', S2, L2, bnghk),
	S2='ar**', L2 = '',
	atom_split('ar**bnghk', S3, L3, R3),
	S3='', L3='', R3='ar**bnghk'.

test_split_atom_3 :-
	atom_split('ar**bng**hk', '**', L0),
	L0=[ar,bng,hk].

test_number_chars :-
	number_chars(234, CL0),
	CL0=['2','3','4'],
	number_chars(N0, CL0),
	N0 = 234,
	number_chars(35.7, CL1),
	CL1=['3','5','.','7'],
	number_chars(N1, CL1),
	N1 = 35.7,
	number_chars(35.7e14, CL2),
	CL2=['3','.','5','7',e,+,'1','5'] ,
	number_chars(N2, CL2),
	N2=3.57e+15,
true.

test_number_codes :-
        number_codes(234, CL0),
        CL0="234",
        number_codes(N0, CL0),
        N0 = 234,
        number_codes(35.7, CL1),
        CL1="35.7",
        number_codes(N1, CL1),
        N1 = 35.7,
        number_codes(35.7e14, CL2),
        CL2="3.57e+15",
        number_codes(N2, CL2),
        N2=3.57e+15,
true.

test_term_chars :-
	term_chars(f(x,2,P), TCL0),
	TCL0=[f,'(',x,',','2',',','_','A',')'],
	term_chars(T0, TCL0),
	T0=f(x,2,_A),
	term_chars([r(9), 3.4, [f,g,h]], TCL1),
	TCL1=['\'','.','\'','(',r,'(','9',')',',','\'','.','\'','(','3','.','4',',','\'','.','\'','(','\'','.','\'','(',f,',','\'','.','\'','(',g,',','\'','.','\'','(',h,',','[',']',')',')',')',',','[',']',')',')',')'],
	term_chars(T1, TCL1),
	T1=[r(9),3.4,[f,g,h]],
true.

test_term_codes :-
true.
