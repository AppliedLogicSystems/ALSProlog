/*-----------------------------------------------------------------------*
 | 		queens.pro  
 | 	Copyright (c) 1987-2015 Applied Logic Systems, Inc.
 |
 |		Placing queens on a chessboard benchmark
 |
 |	Description:
 |
 |	The classic problem of placing 8 queens on a chessboard
 |	so that none is attacking any other.
 |
 | 	This is a benchmark which tests the speed of bagof (among 
 |	other things). It provides a different mix of operations than 
 |	the nrev benchmark.  This benchmark is invoked by entering the 
 |	following goal
 |
 |		?- all_queens.
 |
 | 	It will print out the number of solutions found and the time 
 |	it took to find these solutions.  There are 92 solutions 
 |	and ALS-Prolog takes about 78 seconds on a standard IBM-PC.
 |	It takes about 2 seconds on a Sun 3/260.   (about 1999)
 | 	(11/8/2015): on a 2.6GHz IntelCore i7 MacPro:  Time = 0.12
 *-----------------------------------------------------------------------*/

all_queens :- 
	InitTime is cputime,
        bagof(X,get_solutions(X),L),
	length(L,N),
	DeltaTime is cputime-InitTime,
	write('Number of Solutions'=N), nl,
	write('Time'=DeltaTime), nl.


size(8).
int(1).
int(2).
int(3).
int(4).
int(5).
int(6).
int(7).
int(8).


get_solutions(Soln) :- solve([], Soln).

%	newsquare generates legal positions for next queen

newsquare([], square(1, X)) :- int(X).
newsquare([square(I, J) | Rest], square(X, Y)) :-
	X is I + 1,
	int(Y),
	not_threatened(I, J, X, Y),
	safe(Rest, X, Y).


%	safe checks whether square(X, Y) is threatened by any
%	existing queens

safe([], X, Y).
safe([square(I,J) | L], X, Y) :-
	not_threatened(I, J, X, Y),
	safe(L, X, Y).


not_threatened(I, J, X, Y) :-
	I =\= X,
	J =\= Y,
	I-J =\= X-Y,
	I+J =\= X+Y.

%	solve accumulates the positions of occupied squares

solve([square(Bs, Y) | L], [square(Bs, Y) | L]) :- size(Bs).
solve(Initial, Final) :-
	newsquare(Initial, Next),
	solve([Next | Initial], Final).
