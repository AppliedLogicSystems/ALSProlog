/*
 * ident.pro		-- tests for == and \==
 *	Copyright (c) 1991 Applied Logic Systems, Inc.
 */


test :-
	ntests(N),
	stests(1,N),
	ftests(1,N),
	write('ident test completed.'),nl,nl.

stests(I,N) :-	I>N, !.
stests(I,N) :-	stest(I), !, II is I+1, stests(II,N).
stests(I,N) :-	write(stest(I)), write(' failed when it should not have.'), nl,
		II is I+1, stests(II,N).

ftests(I,N) :-	I>N, !.
ftests(I,N) :-	ftest(I), !, write(ftest(I)), 
		write(' succeeded when it should not have.'), nl,
		II is I+1, ftests(II,N).
ftests(I,N) :- 	II is I+1, ftests(II,N).


ntests(34).

%%
%% stest(N) should succeed.
%% ftest(N) should fail.
%%

stest(1) :-	X == X.			ftest(1) :-	X == Y.
stest(2) :-	a == a.			ftest(2) :-	a == b.
stest(3) :-	1 == 1.			ftest(3) :-	1 == 2.
stest(4) :-	[a] == [a].		ftest(4) :-	[a] == X.
stest(5) :-	[X] == [X].		ftest(5) :-	X == [a].
stest(6) :-	[a,b,c] == [a,b,c].	ftest(6) :-	[a,b,c] == [a,b,X].
stest(7) :-	f(x) == f(x).		ftest(7) :-	f(x) == g(x).
stest(8) :-	f(X) == f(X).		ftest(8) :-	f(X) == f(Y).
stest(9) :-	f(a,b,c) == f(a,b,c).	ftest(9) :-	f(a,b) == f(a,b,c).
stest(10) :-	f(1,X,Y) == f(1,X,Y).	ftest(10) :-	f(1,X,Y) == f(1,Y,X).

stest(11) :-	f([a,b,X],g(h(x),Y,Z,[X,Y,Z]),2.48) ==
			f([a,b,X],g(h(x),Y,Z,[X,Y,Z]),2.48).
ftest(11) :-	f([a,b,X],g(h(x),Y,Z,[X,Y,Z]),2.49) ==
			f([a,b,X],g(h(x),Y,Z,[X,Y,Z]),2.48).

uia1('hello_there').
uia2('hello_there').
uia3('howdy_there').

%% uia - sym
stest(12) :-	uia1(X),
		X == hello_there.

ftest(12) :-	uia1(X),
		X == howdy_there.


%% uia - uia
stest(13) :-	uia1(X), uia2(Y),
		X == Y.

ftest(13) :-	uia1(X), uia3(Y),
		X == Y.

%% sym - uia
stest(14) :-	uia1(X),
		hello_there == X.

ftest(14) :-	uia1(X),
		howdy_there == X.

%% uia's and symbols in a list
stest(15) :-	uia1(X), uia2(Y), uia3(Z),
		[X,Y,Z,hello_there,howdy_there] ==
		[X,X,howdy_there,Y,Z].

ftest(15) :-	uia1(X), uia2(Y), uia3(Z),
		[X,Y,Z,hello_there,howdy_there] ==
		[X,X,howdy_there,Y].	%% last elem missing

%% 16 through 30 are the above, but with \== instead

ftest(16) :-	X \== X.		stest(16) :-	X \== Y.
ftest(17) :-	a \== a.		stest(17) :-	a \== b.
ftest(18) :-	1 \== 1.		stest(18) :-	1 \== 2.
ftest(19) :-	[a] \== [a].		stest(19) :-	[a] \== X.
ftest(20) :-	[X] \== [X].		stest(20) :-	X \== [a].
ftest(21) :-	[a,b,c] \== [a,b,c].	stest(21) :-	[a,b,c] \== [a,b,X].
ftest(22) :-	f(x) \== f(x).		stest(22) :-	f(x) \== g(x).
ftest(23) :-	f(X) \== f(X).		stest(23) :-	f(X) \== f(Y).
ftest(24) :-	f(a,b,c) \== f(a,b,c).	stest(24) :-	f(a,b) \== f(a,b,c).
ftest(25) :-	f(1,X,Y) \== f(1,X,Y).	stest(25) :-	f(1,X,Y) \== f(1,Y,X).

ftest(26) :-	f([a,b,X],g(h(x),Y,Z,[X,Y,Z]),2.48) \==
			f([a,b,X],g(h(x),Y,Z,[X,Y,Z]),2.48).
stest(26) :-	f([a,b,X],g(h(x),Y,Z,[X,Y,Z]),2.49) \==
			f([a,b,X],g(h(x),Y,Z,[X,Y,Z]),2.48).

%% uia - sym
ftest(27) :-	uia1(X),
		X \== hello_there.

stest(27) :-	uia1(X),
		X \== howdy_there.


%% uia - uia
ftest(28) :-	uia1(X), uia2(Y),
		X \== Y.

stest(28) :-	uia1(X), uia3(Y),
		X \== Y.

%% sym - uia
ftest(29) :-	uia1(X),
		hello_there \== X.

stest(29) :-	uia1(X),
		howdy_there \== X.

%% uia's and symbols in a list
ftest(30) :-	uia1(X), uia2(Y), uia3(Z),
		[X,Y,Z,hello_there,howdy_there] \==
		[X,X,howdy_there,Y,Z].

stest(30) :-	uia1(X), uia2(Y), uia3(Z),
		[X,Y,Z,hello_there,howdy_there] \==
		[X,X,howdy_there,Y].	%% last elem missing


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
big_struct6(S) :-
	functor(S,f,5001),
	fillStruct(4999,S),
	arg(5000,S,b),
	arg(5001,S,z).

fillStruct(N,S) :-
	N > 0,
	!,
	arg(N,S,z),
	NN is N-1,
	fillStruct(NN,S).
fillStruct(_,_).

%% big structures
ftest(31) :-	big_struct1(X), big_struct2(Y),
		X == Y.
stest(31) :-	big_struct1(X), big_struct1(Y),
		X == Y.

ftest(32) :-	big_struct1(X), big_struct5(Y),
		X == Y.
stest(32) :-	big_struct1(X), big_struct4(Y),
		X \== Y.

ftest(33) :-	big_struct1(X), big_struct3(Y),
		X == Y.
stest(33) :-	big_struct3(X), big_struct4(Y),
		X \== Y.

ftest(34) :-	big_struct4(X), big_struct6(Y),
		X == Y.
stest(34) :-	big_struct4(X), big_struct6(Y),
		X \== Y.
