/*========================================================================*
 |			arithx1.pro
 |	Copyright (c) 1991-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |				Miscellaneous arithmetic predicates
 |
 |	Authors:	Various
 |	Date:		1991--96
 *========================================================================*/

module builtins.

export max/3.
export min/3.
export greater/3.

/*!-----------------------------------------------------------------------
 |	max/3
 |	max(A,B,C)
 |	max(A,B,C)
 |
 |	- C is maximum of results of evaluation of A,B with is/2
 *-----------------------------------------------------------------------*/
max(A,B,C) 
	:- 
	A0 is A, 
	B0 is B, 
	max0(A0,B0,C).
max0(A,B,A) 
	:- 
	A >= B,!.
max0(A,B,B).

/*!-----------------------------------------------------------------------
 |	min/3
 |	min(A,B,C)
 |	min(A,B,C)
 |
 |	- C is minimum of results of evaluation of A,B with is/2
 *-----------------------------------------------------------------------*/
min(A,B,C) 
	:- 
	A0 is A, 
	B0 is B, 
	min0(A0,B0,C).
min0(A,B,A) 
	:- 
	A =< B,!.
min0(A,B,B).

/*!-----------------------------------------------------------------------
 |	greater/3
 |	greater(X, Y, Result)
 |	greater(+, +, -)
 |
 |	- returns the greater of two numbers
 *-----------------------------------------------------------------------*/
greater(X,Y,X) 
	:- 
	X >= Y,
	!.

greater(_,Y,Y).

/*!-----------------------------------------------------------------------
 |	least/3
 |	least(X, Y, Result)
 |	least(+, +, -)
 |
 |	- returns the smaller of two numbers
 *-----------------------------------------------------------------------*/
least(X,Y,X) 
	:- 
	X =< Y,
	!.

least(_,Y,Y).

/*!-----------------------------------------------------------------------
 |	maximum/2
 |	maximum(NumList, Result)
 |	maximum(+, -)
 |
 |	- Returns the max of the numbers on NumList; -0inf if NumList = []. 
 *-----------------------------------------------------------------------*/

maximum(NumList, Result)
	:-
	maximum(NumList, -0inf, Result).

maximum([], Cur, Cur).

maximum([Num | Tail], Cur, Result)
	:-
	Num > Cur,
	!,
	maximum(Tail, Num, Result).

maximum([_ | Tail], Cur, Result)
	:-
	maximum(Tail, Cur, Result).

/*!-----------------------------------------------------------------------
 |	minimum/2
 |	minimum(NumList, Result)
 |	minimum(+, -)
 |
 |	- Returns the min of the numbers on NumList; 0inf if NumList = []. 
 *-----------------------------------------------------------------------*/

minimum(NumList, Result)
	:-
	minimum(NumList, 0inf, Result).

minimum([], Cur, Cur).

minimum([Num | Tail], Cur, Result)
	:-
	Num < Cur,
	!,
	minimum(Tail, Num, Result).

minimum([_ | Tail], Cur, Result)
	:-
	minimum(Tail, Cur, Result).





/*!-----------------------------------------------------------------------
 |	sumlist/2
 |	sumlist(NumList, Result)
 |	sumlist(+, -)
 |
 |	- Returns the sum of the numbers on NumList; 0 if NumList = []. 
 *-----------------------------------------------------------------------*/

sumlist([], 0).

sumlist( [ Num | NumListTail ], Result) 
	:-
	sumlist(NumListTail, InterResult),
	Result is InterResult + Num.

/*!-----------------------------------------------------------------------
 |	prodlist/2
 |	prodlist(NumList, Result)
 |	prodlist(+, -)
 |
 |	- Returns the prod of the numbers on NumList; 1 if NumList = []. 
 *-----------------------------------------------------------------------*/

prodlist([], 1).

prodlist( [ Num | NumListTail ], Result) 
	:-
	prodlist(NumListTail, InterResult),
	Result is InterResult * Num.

/*!-----------------------------------------------------------------------
 |	sum_squares/2
 |	sum_squares(NumList, Result)
 |	sum_squares(+, -)
 |
 |	- Returns the sum of the squares of the numbers on NumList; 
 |	0 if NumList = []. 
 *-----------------------------------------------------------------------*/

sum_squares([], 0).

sum_squares( [ Num | NumListTail ], Result) 
	:-
	sum_squares(NumListTail, InterResult),
	Result is InterResult + (Num * Num).

/*!-----------------------------------------------------------------------
 |	sum_square_diffs/3
 |	sum_square_diffs(XList, YList, Result)
 |	sum_square_diffs(+, +, -)
 |
 |	Must have: len(XList) = len(YList); 
 |	Returns 0 if NumList = [];
 |	Else, if XList = X1,X2,... and YList = Y1,Y2,..., returns
 |		(X1-Y1)^2 + (X2-Y2)^2 + ...
 *-----------------------------------------------------------------------*/

sum_square_diffs([], [], 0).

sum_square_diffs( [ X | XListTail ],  [ Y | YListTail ], Result) 
	:-
	sum_square_diffs(XListTail, YListTail, InterResult),
	Result is InterResult + (X - Y) * (X - Y).

endmod.
