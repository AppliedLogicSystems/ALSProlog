/*=====================================================================*
 |   testclp_db.pro
 |  Copyright (c) 1996 Applied Logic Systems, Inc.
 |
 |  Automated test run data/examples for clp tests
 *=====================================================================*/

module testclp.
use rel_arith.

export b/2.

%------ Group A--------------------------------------------------------
% Various real-valued constraints;
% Correspond to file: basicclp.pro
%
% THESE ARE EXAMPLES FROM THE PAPER:
%           Programming in CLP(BNR) 
%               Benhamou & Older
%           presented at PPCP '93
%  
%  Only those involving real intervals are included here.
%  This file uses the new V3.5 syntax.
%----------------------------------------------------------------------

%----- 1:

b(1,[X,Y]) :- X::real(1,3), {Y**2==X}.

orig(1, 'X::real(1,3), {Y**2==X}.').
orig_expect(1,[ '_X = [1.0000, 3.0001]',
                '_Y = [-1.7321, 1.7321]']).
expect(1, ['X','Y'], [[1.0, 3.0],[-1.732050808, 1.732050808]]).

%----- 2:

b(2,[X,Y]) :- [X,Y]::real, {1==X + 2*Y, Y - 3*X==0}.  % explicit declarations

orig(2, '[X,Y]::real, {1==X + 2*Y, Y - 3*X==0}.').
orig_expect(2,[ '_Y = [0.42857, 0.42858]',
                '_X = [0.14285, 0.14286]' ]).

expect(2, ['X','Y'], [0.1428571429,0.4285714286]).

%----- 3:

b(3,[X,Y]) :- {1==X + 2*Y, Y - 3*X==0}.       % implicit declarations

orig(3, '{1==X + 2*Y, Y - 3*X==0}.').
orig_expect(3,[ '_Y = [0.42857, 0.42858]',
                '_X = [0.14285, 0.14286]']).

expect(3, ['X','Y'], [0.1428571429,0.4285714286]).

%----- 4:

b(4,[X,Y]) :- [X,Y]::real(1,3),{X>=0,Y>=0, tan(X)==Y, X**2 + Y**2 == 5 },solve(X).

orig(4, ' {X>=0,Y>=0, tan(X)==Y, X**2 + Y**2 == 5 }.').
orig_expect(4,[ '_X = [1.0966, 1.0967]',
                '_Y = [1.9486, 1.9487]' ]).

expect(4, ['X','Y'], mult([[1.096668129,1.94867109], 
					[[1.570793152,1.570800781],[1.591409723,1.591417253]]])).

%----- 5:

b(5,[X]) :- X::real(0,1), {0==35*X**256 -14*X**17 + X}, solve(X).

orig(5, 'X:real(0,1), {0==35*X**256 -14*X**17 + X}, solve(X).').
orig_expect(5,[ '_X = [0.0000, 2.1020e-44]',
                ';',
                '_X = [0.84794, 0.84795]',
                ';',
                '_X = [0.99584, 0.99585]' ]).
expect(5, ['X'], mult([[[0.0000, 2.1020e-44]],
                      [[0.84794, 0.84795]],
                      [[0.99584, 0.99585]]
                     ])).

%----- 6:

b(6,[X,Y]) :- { X**3 + Y**3 ==2*X*Y, X**2 + Y**2==1, X>=0}, solve(X).

orig(6, '{ X**3 + Y**3 ==2*X*Y, X**2 + Y**2==1, X>=0}, solve(X).').
orig_expect(6,[ '_X = [0.39104, 0.39106]',
                '_Y = [-0.92038, -0.92036]',
                ';',
                '_X = [0.44977, 0.44980]',
                '_Y = [0.89313, 0.89315]',
                ';',
                '_X = [0.89309, 0.89316]',
                '_Y = [0.44974, 0.44987]' ]).

expect(6, ['X','Y'], mult(
		[ [[0.3910497758,0.3910541883],[-0.9203695306,-0.9203676558]],
		  [[0.449785464,0.4497907715],[0.8931339552,0.8931366281]],
		  [[0.8931316465,0.8931380193],[0.4497827014,0.4497953557]]
		] )).

%----- 7:

b(7,[X,Y]) :- X::real(1,3), {Y**2==X}. 

orig(7, 'X::real(1,3), {Y**2==X}.').
orig_expect(7,[ '?- [_H527 :: real(1, 3), {_H541 ** 2 == _H527}]',
                '   where [_H527 : real(1.0, 3.0),',
                '   _H541 : real(-1.73205080756888, 1.73205080756888)]. '] ).

expect(7, ['X','Y'], [[1.0,3.0],[-1.732050808,1.732050808]]).

%----- 8:

b(8,[X,Y]) :-  [X,Y]::real, {1==X + 2*Y, Y - 3*X==0}. % explicit declarations

orig(8, '[X,Y]::real, {1==X + 2*Y, Y - 3*X==0}.').
orig_expect(8,[ '?- [[_H950, _H945] :: real,',
                '    {1 == _H950 + 2 * _H945, _H945 - 3 * _H950 == 0}]',
                '    where [_H945 : real(0.428571428571429, 0.428571428571429),',
                '    _H950 : real(0.142857142857143, 0.142857142857143)]. ' ]).

expect(8, ['X','Y'], [0.1428571429, 0.4285714286] ).

%----- 9:

b(9,[X,Y]) :- {X>=0,Y>=0, tan(X)==Y, X**2 + Y**2 == 5 }.

orig(9, '{X>=0,Y>=0, tan(X)==Y, X**2 + Y**2 == 5 }.').
orig_expect(9,[ '?- {_H813 >= 0, _H818 >= 0, tan(_H813) == _H818,',
                '    ((_H813 ** 2) + (_H818 ** 2)) == 5}',
                '    where [_H813 : real(0.0, 2.23606797749979),',
                '        _H818 : real(0.0, 2.23606797749979)]. ' ]).

expect(9, ['X','Y'], mult([[0.1428571429, 0.4285714286],
					[[1.57079374,1.57080227],[1.591408254,1.591416673]]])).

%-----10:

b(10,[Y]) :- {Y is tan(1.09)}.

orig(10, '{Y is tan(1.09)}.').
orig_expect(10,[ ' ?- 1.91709182160686 is tan(1.09). ']).

expect(10, ['Y'], [ 1.91709182160686 ] ).

%-----11:

b(11,[X,Y]) :- {X>=0,Y>=0,  X**2 + Y**2 == 5 }. 

orig(11, '{X>=0,Y>=0,  X**2 + Y**2 == 5 }. ').
orig_expect(11,[ '?- {_H567 >= 0, _H572 >= 0, _H567 ** 2 + _H572 ** 2 == 5}',
                 '    where [_H567 : real(0.0, 2.23606797749979),',
                 '    _H572 : real(0.0, 2.23606797749979)]. ' ]).

expect(11, ['X','Y'], [[0.0,2.236067977],[0.0,2.236067977]] ).

%-----12:

b(12,[X]) :- X::real(0,1), {0==35*X**256 -14*X**17 + X}, solve(X).

orig(12, 'X:real(0,1), {0==35*X**256 -14*X**17 + X}, solve(X).').
orig_expect(12,[ 
	'?- [0.0 : real(0, 1),',
         '{0 == (35 * 0.0 ** 256 - 14 * 0.0 ** 17) + 0.0}, solve(0.0)].',
    ';',
    '?- [_H890 : real(0, 1),',
         '{0 == (35 * _H890 ** 256 - 14 * _H890 ** 17) + _H890}, solve(_H890)]',
         '    where [_H890 : real(0.847943660827315, 0.847943660827315)].',
     ';',
     '?- [_H634 : real(0, 1),',
          '{0 == (35 * _H634 ** 256 - 14 * _H634 ** 17) + _H634}, solve(_H634)]',
          '     where [_H634 : real(0.995842494200498, 0.995842494200498)].']).

expect(12, ['X'], mult([[0.0],[0.847943660827315],[0.995842494200498]]) ).

%-----13:

b(13,[X,Y]) :- { X**3 + Y**3 ==2*X*Y, X**2 + Y**2==1, X>=0}, solve(X).

orig(13, '{ X**3 + Y**3 ==2*X*Y, X**2 + Y**2==1, X>=0}, solve(X).').
orig_expect(13,[ 
	 '?- [{((_H1534 ** 3) + (_H1543 ** 3)) == ((2 * _H1534) * _H1543),',
          '    ((_H1534 ** 2) + (_H1543 ** 2)) == 1, _H1534 >= 0},solve(_H1534)]',
          '    where [_H1534 : real(0.391018886096038, 0.391085781049752),',
          '           _H1543 : real(-0.920382654506382, -0.92035423172858)]. ',
     ';',
     '?- [{((_H1567 ** 3) + (_H1576 ** 3)) == ((2 * _H1567) * _H1576),',
          '    ((_H1567 ** 2) + (_H1576 ** 2)) == 1,_H1567 >= 0},solve(_H1567)]',
          '    where [_H1567 : real(0.449060394395367, 0.450226789190836),',
          '           _H1576 : real(0.892914239048135, 0.893501405810577)]. ',
     ';',
     '?- [{((_H1474 ** 3) + (_H1483 ** 3)) == ((2 * _H1474) * _H1483),',
          '    ((_H1474 ** 2) + (_H1483 ** 2)) == 1,_H1474 >= 0},solve(_H1474)]',
          '    where [_H1474 : real(0.892906985150622, 0.893513338800849),',
          '           _H1483 : real(0.449036650380523, 0.450241175226374)].' ]).

expect(13, ['X','Y'], mult(
		[ [[0.3910497758,0.3910541883],[-0.9203695306,-0.9203676558]],
		  [[0.449785464,0.4497907715],[0.8931339552,0.8931366281]],
		  [[0.8931316465,0.8931380193],[0.4497827014,0.4497953557]]
		] )).

%-----14:

b(14,[X,Y]) :- [X,Y]::real(0,_), {5>=X**2, 5>=Y**2,Y==tan(X)}.

orig(14, '[X,Y]:real(0,_), {5>=X**2, 5>=Y**2,Y==tan(X)}.').
orig_expect(14,[
		 '?- [[_H922, _H917] : real(0, _H931),',
         '    {5 >= _H922 ** 2, 5 >= _H917 ** 2, _H917 == tan(_H922)}]',
         '    where [_H917 : real(0.0, 2.23606797749979),',
         '           _H922 : real(0.0, 2.23606797749979)]. ' ]).

expect(14, ['X','Y'], [[0.0,2.236067977], [0.0,2.236067977]]).
%-----15:

b(15,[Y]) :-  { tan(2.236)==Y }.

orig(15, '{ tan(2.236)==Y}.').
orig_expect(15,['?- {tan(2.236) == _H502}',
       			' where [_H502 : real(-1.27473488083765, -1.27473488083765)]. ' ]).

expect(15, ['X'], [-1.27473488083765]).

%-----16:

b(16,[Y]) :-  { tan(X)== 1.274734881 }.

%-----17:

b(17, [X]) :- [X]::real(0.0,0.5), {X == X**2 }.

expect(17, ['X'], [0]).

%-----18:

b(18, [X]) :- [X]::real(0.0,0.5), {X == X**2 }, solve(X).

expect(18, ['X'], [0.0]).

%%%%%%%--------- For continuous interest:

cfv(FV, PV, R, N) 
	:-
	[FV,PV,R,N]::real,
	{ FV == PV * exp( R * N) }.


%-----20:

b(20, [X]) :- cfv(X, 1000.0, 0.06, 7.0).

expect(20, ['X'], [1521.961556]).

%-----21:

b(21, [X]) :- cfv(1521.961556, X, 0.06, 7.0).

expect(21, ['X'], [1000]).

%-----22:

b(22, [X]) :- cfv(1521.961556, 1000.0, X, 7.0).

expect(22, ['X'], [0.06000000004]).

%-----23:

b(23, [X]) :-  cfv(1521.961556, 1000.0, 0.06, X).

expect(23, ['X'], [7.000000004]).

%%%%%%%--------- For discrete interest:
%%%%

export dfv/5.
dfv(FV, PV, R, K, N) 
	:-
	[FV,PV,R,K,N,Z]::real,
	{FV == PV * exp( Z * K * N), exp(Z) == (1+ R/K) }.

%-----24:

b(24, [PV]) :-  dfv(1000.0, PV, 0.08, 1.0, 1.0).

expect(24, ['PV'], [925.9259259]).

%-----25:

b(25, [PV]) :-  dfv(1000.0, PV, 0.08, 4.0, 1.0).

expect(25, ['PV'], [923.845426]).

%-----26:

b(26, [FV]) :-  dfv(FV, 925.9259259, 0.08, 1.0, 1.0).

expect(26, ['FV'], [1000]).

%-----27:

b(27, [FV]) :-  dfv(FV, 923.845426, 0.08, 4.0, 1.0).

expect(27, ['FV'], [1000]).


%-----28:

b(28, [R]) :-  dfv(1000.0, 923.845426, R, 1.0, 1.0).

expect(28, ['R'], [0.08243216003]).

/******
%-----29:

b(29, [K]) :-  dfv(1000.0, 923.845426, 0.08, K, 1.0).

expect(29, ['K'], [[]]).
******/


%------ Group B--------------------------------------------------------
%                INTEGER CONSTRAINTS
%----------------------------------------------------------------------

%-----1001:

b(1001, [N,M]) :- [N,M] :: integer(1,5), { N < 4, N + M == 8 }.

expect(1001, ['N','M'], [3,5]).

%-----1002:

b(1002, [N,M]) :- [N,M] :: integer(1,15),  { 2*N + M == 11, N + 2*M ==13 }.

expect(1002, ['N','M'], [3,5]).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Send More Money
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-----1003:

b(1003, [L]) :- sendmoremoney_x(L).

expect(1003, ['L'], [[9,5,6,7,1,0,8,2]]).

sendmoremoney([S,E,N,D,M,O,R,Y])
	:-
	[S,M] :: integer(1,9),
	[E,N,D,O,R,Y] :: integer(0,9),
	distinct( [S,E,N,D,M,O,R,Y] ),
	{ 1000*(S+M) + 100*(E+O) + 10*(N+R) + (D+E)
		== 10000*M + 1000*O + 100*N + 10*E + Y}.

sendmoremoney_x(L)
	:-
	sendmoremoney(L),
	enumerate(L).

distinct([]).
distinct([X | Xs])
	:-
	distinct_from(Xs, X),
	distinct(Xs).

distinct_from([], _).
distinct_from([X | Xs], Y)
	:-
	{X <> Y},
	distinct_from(Xs, Y).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% N Queens
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export how_many_queens/2.
how_many_queens(N, NumQs)
	:-
	q_list(N, PList),
	length(PList, NumQs).


queens_bound(4, 2).
queens_bound(5, 10).
queens_bound(6, 4).
queens_bound(7, 40).
queens_bound(8, 92).

export q_list/2.
q_list(N, PList)
	:-
	queens_bound(N, BN),
	spec_b_findall(Pos, (n_queens(N, Pos), enumerate(Pos)), PList,BN).

n_queens(N, Position)
	:-
	is_length(Position, N),
	Position :: integer(1, N),
	queens(N, Position).
				 
queens(0, []) :-!.
				  
queens(N, [P | Positions])
	:-
	N1 is N - 1,
	c_noattack(Positions, P, 1),
	queens(N1, Positions).
								   
c_noattack([], _, _).
c_noattack([P | Positions], Pos, N)
	:-
	N1 is N + 1,
	{ Pos <> P, Pos <> P-N, Pos <> P+N },
	c_noattack(Positions, Pos, N1).

%-----1004:

b(1004, [PList]) :- q_list(4, PList).

expect(1004, ['PList'], [[[2,4,1,3],[3,1,4,2]]] ).

%-----1005:

b(1005, [PList]) :- q_list(5, PList).

expect(1005, ['PList'], 
	[ 
	 [[1,3,5,2,4],[1,4,2,5,3],[2,4,1,3,5],[2,5,3,1,4],[3,1,4,2,5],[3,5,2,4,1],
			[4,1,3,5,2],[4,2,5,3,1],[5,2,4,1,3],[5,3,1,4,2] ]
	] ).

%-----1006:

b(1006, [NumQ]) :- how_many_queens(6, NumQ).

expect(1006, ['NumQ'], [4]).

%-----1007:

b(1007, [NumQ]) :- how_many_queens(7, NumQ).

expect(1007, ['NumQ'], [40]).

%-----1008:

b(1008, [NumQ]) :- how_many_queens(8, NumQ).

expect(1008, ['NumQ'], [92]).



%------ Group C--------------------------------------------------------
%                BOOLEAN CONSTRAINTS
%----------------------------------------------------------------------

%-----3001:

b(3001,[X]) :- X::boolean, {X == (0 xor 1)}.

expect(3001, ['X'], [1]).

%-----3002:

b(3002,[X]) :- X::boolean, {1 == (X xor 1)}.

expect(3002, ['X'], [0]).

%-----3003:

b(3003,[X,Y]) :- [X,Y]::boolean, { 1 == X and Y }.

expect(3003, ['X','Y'], [1,1]).

%-----3004:

b(3004,[X,Y]) :- [X,Y]::boolean, { 0 == X or Y }.

expect(3004, ['X','Y'], [1,1]).

%-----3005:

b(3005,[X,Y,M]) :- [X,Y,M]::boolean, {X == 1, Y == 0, M == (X or Y) }.

expect(3005, ['X','Y','M'], [1,0,1]).

%-----3006:

b(3006,[X,Y,M]) :- [X,Y,M]::boolean, {X == 0, Y == 0, M == (X or Y) }.

expect(3006, ['X','Y','M'], [0,0,0]).

%-----3007:

b(3007,[X,Y,M]) :- [X,Y,M]::boolean, {X == 0, Y == 0, M == (X and Y) }.

expect(3007, ['X','Y','M'], [0,0,0]).

%-----3008:

b(3008,[X,Y,M]) :- [X,Y,M]::boolean, {X == 1, Y == 1, M == (X and Y) }.

expect(3008, ['X','Y','M'], [1,1,1]).

%-----3009:

b(3009,[X,Y,M]) :- [X,Y,M]::boolean, {M == (X and Y), M ==1 }.

expect(3009, ['X','Y','M'], [1,1,1]).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Pigeon Holes
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export pigeons_in_holes/3.
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

%-----3010:

b(3010,[HList]) 
	:- 
	Bd is 2^2,
	spec_b_findall(Hs, pigeons_in_holes(2, 2, Hs), HList, Bd).

expect(3010, ['HList'], 
		[[[[0,1],[1,0]],[[1,0],[0,1]]]]
		 ).

%-----3011:

b(3011,[HList]) 
	:- 
	Bd is 3^3,
	spec_b_findall(Hs, pigeons_in_holes(3, 3, Hs), HList, Bd).

expect(3011, ['HList'], [
		[[[0,0,1],[0,1,0],[1,0,0]],[[0,0,1],[1,0,0],[0,1,0]],
		[[0,1,0],[0,0,1],[1,0,0]],[[0,1,0],[1,0,0],[0,0,1]],
		[[1,0,0],[0,0,1],[0,1,0]],[[1,0,0],[0,1,0],[0,0,1]]]
		 ]).

endmod.
