/*
 * compare.pro		-- tests out compare/3
 *	Copyright (c) 1991 Applied Logic Systems, Inc.
 *
 * Created:	2/13/91
 * Author:	Kevin A. Buettner
 *
 * This program is run by consulting this file and then running 'test'.
 *
 */

test :- write('Testing compare/3'),nl,
	test(1),
	write('Done testing compare/3'), nl.

test(116) :- !.
test(N) :-
	test(N,A,B,ERes), compare(ARes,A,B), check(N,ARes,ERes),
	NN is N+1, !, test(NN).

check(N,Res,Res) :- !.
check(N,_,_) :- write('Test '), write(N), write(' gave unexpected results.'),nl.

test(1,1,1,=).			%% integers, positive
test(2,1,2,<).
test(3,2,1,>).

test(4,-10,12,<).		%% integers, mixed
test(5,14,-9,>).

test(6,-4,-4,=).
test(7,-10,-5,<).		%% integers, negative
test(8,-6,-128,>).
test(9,0,-9,>).

uia1('hello').
uia2('hello').
uia3('howdy').
uia4('hello!').

test(10,hello,hello,=).		%% symbols
test(11,hello,howdy,<).
test(12,howdy,hello,>).
test(13,hello,U1,=) :- uia1(U1).
test(14,hello,U3,<) :- uia3(U3).
test(15,U3,hello,>) :- uia3(U3).
test(16,U1,U2,=) :- uia1(U1), uia2(U2).
test(17,U1,U3,<) :- uia1(U1), uia3(U3).
test(18,U3,U1,>) :- uia3(U3), uia1(U1).
test(19,U1,U4,<) :- uia1(U1), uia4(U4).

test(20,1,hello,<).		%% symbols and integers
test(21,hello,0,>).
test(22,12,U1,<) :- uia1(U1).
test(23,U3,-12,>) :- uia3(U3).

test(24,12.1,12.2,<).		%% doubles
test(25,12.2,12.1,>).
test(26,25,25.1,<).
test(27,25.1,25,>).
test(28,-12.3,-15,>).
test(29,-12.3,-15.99,>).
test(30,-15.99,-12.3,<).
test(31,-123.456,-123.456,=).
test(32,1.0,1,=).
test(33,1e14,1e14,=).

test(34,12.3,hello,<).
test(35,hello,99.13,>).
test(36,U1,14.93,>) :- uia1(U1).
test(37,-453.993,U3,<) :- uia3(U3).

test(38,X,X,=).			%% variables and other types
test(39,X,an_atom,<).
test(40,an_atom,X,>).
test(41,X,U1,<) :- uia1(U1).
test(42,U3,X,>) :- uia3(U3).
test(43,X,1,<).
test(44,1,X,>).
test(45,X,1.234,<).
test(46,1.2345,X,>).
test(47,[a],X,>).
test(48,X,[a],<).
test(49,f(a,b,c),X,>).
test(50,X,g(a,b,c),<).
test(51,X,Y,<) :- _=f(X,Y).
test(52,Y,X,>) :- _=f(X,Y).
test(53,X,Y,=) :- _=f(X,Y), X=Y.

test(54,f(2),f(1,2),<).		%% structures and lists
test(55,f(1,2),f(1),>).
test(56,f(1,2),f(1,2),=).
test(57,f(1,2),f(1,1),>).
test(58,f(1,1),f(1,2),<).
test(59,f(1,2,3),f(3,2,1),<).
test(60,f(3,2,1),f(1,2,3),>).
test(61,[a,b,c],[a,b,c],=).
test(62,[a,b],[a,b,c],<).
test(63,[a,b,c],[a,b],>).
test(64,[a],f(a,[]),<).
test(65,f(a,[]),[a],>).
test(66,f([a,b,c,d],[e],f,[g,h],[i,j,k,l,m]),
	f([a,b,c,d],[e],f,[g,h],[i,j,k,l,m]), =).
test(67,f([a,b,c,d],[e],f,[g,h],[i,j,k,l]),
	f([a,b,c,d],[e],f,[g,h],[i,j,k,l,m]), <).
test(68,f([a,b,c,d],[e],f,[g,h],[i,j,k,l,m]),
	f([a,b,c,d],[e],f,[g,h],[i,j,k,l]), >).
test(69,f(a,b,c),g(a,b,c),<).
test(70,g(a,b,c),f(a,b,c),>).
test(71,g(a,b),f(a,b,c),<).
test(72,f(a,b,c),g(a,b),>).
test(73,f(X,Y,Z),f(X,Y,Z),=).
test(74,f(X,Y,Z),f(X,X,Z),>).
test(75,[f(a,b,c),g(a,b,c)],[f(a,b,c),g(a,b,c)],=).
test(76,[f(a,b,c),g(a,b,c)],[f(a,b,c),g(a,b)],>).
test(77,[f(a,b,c),g(a,b)],[f(a,b,c),g(a,b,c)],<).
test(78,[f(a,b),g(a,b,c)],[f(a,b,c),g(a,b)],<).
test(79,[f(a,b,c),g(a,b)],[f(a,b),g(a,b,c)],>).
test(80,[A,B,C,D,E],[a,b,c,d,e],<).

test(81,1,f(x),<).		%% structures, lists, and other types
test(82,f(x),2,>).
test(83,hello,f(x),<).
test(84,f(x),hello,>).
test(85,U1,f(x,y),<) :- uia1(U1).
test(86,f(x,y),U1,>) :- uia1(U1).
test(87,2.3,f(a,b,c),<).
test(88,f(a,b,c),4.9,>).
test(89,1,[a,b],<).
test(90,[a,b],-9,>).
test(91,hello,[hello],<).
test(92,[hello],hello,>).
test(93,U3,[a,b,c],<) :- uia3(U3).
test(94,[a,b,c],U2,>) :- uia2(U2).
test(95,-45.2,[1,2],<).
test(96,[2,1],-993.12,>).
test(97,f(x),g(x),<).
test(98,f(x,y),g(x),>).
test(99,f(x),g(x,y),<).

big_struct1(S) :-
	functor(S,f,5000),
	fillStruct(4999,S),
	arg(5000,S,a).
big_struct2(S) :-
	functor(S,f,5000),
	fillStruct(4999,S),
	arg(5000,S,b).
big_struct3(S) :-
	functor(S,f,4999),
	fillStruct(4999,S).
big_struct4(S) :-
	functor(S,f,5001),
	fillStruct(4999,S),
	arg(5000,S,a),
	arg(5001,S,z).
big_struct5(S) :-
	functor(S,g,5000),
	fillStruct(4999,S),
	arg(5000,S,a).

fillStruct(N,S) :-
	N > 0,
	!,
	arg(N,S,z),
	NN is N-1,
	fillStruct(NN,S).
fillStruct(_,_).

					%% Big stuctures
test(100,S1,S2,=) :- big_struct1(S1), big_struct1(S2).
test(101,S1,S2,<) :- big_struct1(S1), big_struct2(S2).
test(102,S1,S2,>) :- big_struct1(S2), big_struct2(S1).
test(103,S1,S2,>) :- big_struct1(S1), big_struct3(S2).
test(104,S1,S2,<) :- big_struct1(S2), big_struct3(S1).
test(105,S1,S2,<) :- big_struct1(S1), big_struct4(S2).
test(106,S1,S2,<) :- big_struct1(S1), big_struct5(S2).
test(107,S1,S2,>) :- big_struct1(S2), big_struct5(S1).
test(108,S1,S2,<) :- big_struct3(S1), big_struct5(S2).
test(109,S1,S2,>) :- big_struct4(S1), big_struct5(S2).
test(110,f(a,b,c),S2,<) :- big_struct1(S2).
test(111,S1,g(a,b,c),>) :- big_struct2(S1).
test(112,[hello],S1,<) :- big_struct1(S1).
test(113,S1,[there],>) :- big_struct1(S1).
test(114,12.3,S2,<) :- big_struct1(S2).
test(115,S1,9,>) :- big_struct1(S1).
