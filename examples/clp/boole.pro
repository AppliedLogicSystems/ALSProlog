/*===================================================================*
 |		boole.pro
 |
 |	Boolean problems with CLP(BNR) interval constraints.
 *===================================================================*/

%module boole.

:- op(350, xfy, or).
:- op(345, xfy, and).

y1 :- X::boolean, {X == 0}.
y2(X) :- X::boolean, {X == (0 xor 1)}.
y3(X) :- X::boolean, {1 == (X xor 1)}.
y4(X,Y) :- [X,Y]::boolean, { 1 == X and Y }.
y5(X,Y) :- [X,Y]::boolean, { 0 == X or Y }.
y6(X,Y,M) :- [X,Y,M]::boolean, {X == 1, Y == 0, M == (X or Y) }.
y7(X,Y,M) :- [X,Y,M]::boolean, {X == 0, Y == 0, M == (X or Y) }.
y8(X,Y,M) :- [X,Y,M]::boolean, {X == 0, Y == 0, M == (X and Y) }.
y9(X,Y,M) :- [X,Y,M]::boolean, {X == 1, Y == 1, M == (X and Y) }.
y10(X,Y,M) :- [X,Y,M]::boolean, {M == (X and Y), M ==1 }.

	/*-------------------------------------------------------------*
	 |	Digital representation
	 *-------------------------------------------------------------*/

tdr(Cout) 
	:- 
	add1(0, 1, 0, Z, Cout).

add1( X, Y, Cin, Z, Cout)
	:-
	[X,Y,Cin,Z,Cout]::boolean,
	{Z is Cin xor (X xor Y)},
	{Cout is (X and Cin) or (Y and Cin) or (X and Y) }.

	/*-------------------------------------------------------------*
	 |	Pigeon-hole problems
	 *-------------------------------------------------------------*/

pigeons_in_holes(M, N, Hs)
	:-
	pigeons(M, N, Hs),
	holes_used_once(Hs),
	enum_list(Hs).

pigeons(0, N, []).
pigeons(M, N, [H | Hs])
	:-
	M > 0, 
	place_pigeon(N, H),
	M1 is M-1,
	pigeons(M1, N, Hs).

holes_used_once([ [] | _ ]) :-!.
holes_used_once(List_of_Lists)
	:-
	column(List_of_Lists, First_Column, Rest),
	at_most_one(First_Column),
	holes_used_once(Rest).

column([], [], []).
column([ [X|Xs] | Ys ], [X|Cs], [Xs|Rs])
	:-
	column(Ys, Cs, Rs).


place_pigeon(N, Holes)		%% Holes -> list of holes of length N
	:-
	is_length(Holes, N),
	Holes :: boolean,
	or_reduce(Holes, B),
	{ 1==B },
	at_most_one(Holes).

or_reduce([], 0).
or_reduce([X | Xs], (X or S))
	:-
	or_reduce(Xs, S).

at_most_one( [] ).		% each pigeon in just one hole
at_most_one( [X | Xs] )
	:-
	not_both(Xs, X),
	at_most_one(Xs).

not_both([], _).
not_both([X | Xs], Y)
	:-
	{ 1== (~X or ~Y) },
	not_both(Xs, Y).


enum_list([]).
enum_list([ X | Xs ])
	:-
	enumerate(X),
	enum_list(Xs).


%endmod.
