/*========================================================================*
 |			arithx1.pro
 |	Copyright (c) 1991-2019 Applied Logic Systems, Inc.
 |		Group: Arithmetic
 |		DocTitle: max/3
 |		-- Miscellaneous arithmetic predicates
 *========================================================================*/
module builtins.

export max/3.
export min/3.
export maximum/2.
export minimum/2.
export sumlist/2.
export prodlist/2.
export sum_squares/2.
export sum_square_diffs/3.
export max_vector/3.
export min_vector/3.

/*!-----------------------------------------------------------------------
 |	max/3
 |	max(A,B,C)
 |	max(A,B,C)
 |
 |	- C is the maximum of results of evaluation of A,B 
 | 
 |	If A and B are arithmetic expressions which are ground at the
 |	time of evaluation, then C is the maximum of the results of 
 |	evaluation of A and B with is/2.
 *!-----------------------------------------------------------------------*/
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
 |	- C is the minimum of results of evaluation of A,B 
 | 
 |	If A and B are arithmetic expressions which are ground at the
 |	time of evaluation, then C is the minimum of the results of 
 |	evaluation of A and B with is/2.
 *!-----------------------------------------------------------------------*/
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
 |	maximum/2
 |	maximum(NumList, Result)
 |	maximum(+, -)
 |
 |	- Returns the maximum of the evaluations of the numbers on NumList
 |
 |	If NumList is a list of arithmetic expressions which are all
 |	ground at the time of evaluation, then Result is the maximum
 |	of the evaluations of all the numbers on NumList.  If 
 |	NumList = [], then Result = -0inf.
 *!-----------------------------------------------------------------------*/
maximum(NumList, Result)
	:-
	maximum(NumList, -0inf, Result).

maximum([], Cur, Cur).

maximum([Num | Tail], Cur, Result)
	:-
	Num > Cur,
	!,
	Num0 is Num,
	maximum(Tail, Num0, Result).

maximum([_ | Tail], Cur, Result)
	:-
	maximum(Tail, Cur, Result).

/*!-----------------------------------------------------------------------
 |	minimum/2
 |	minimum(NumList, Result)
 |	minimum(+, -)
 |
 |	- Returns the minimum of the evaluations of the numbers on NumList
 |
 |	If NumList is a list of arithmetic expressions which are all
 |	ground at the time of evaluation, then Result is the minimum
 |	of the evaluations of all the numbers on NumList.  If 
 |	NumList = [], then Result = 0inf.
 *!-----------------------------------------------------------------------*/
minimum(NumList, Result)
	:-
	minimum(NumList, 0inf, Result).

minimum([], Cur, Cur).

minimum([Num | Tail], Cur, Result)
	:-
	Num < Cur,
	!,
	Num0 is Num,
	minimum(Tail, Num0, Result).

minimum([_ | Tail], Cur, Result)
	:-
	minimum(Tail, Cur, Result).

/*!-----------------------------------------------------------------------
 |	sumlist/2
 |	sumlist(NumList, Result)
 |	sumlist(+, -)
 |
 |	- Returns the sum of the evaluations of the numbers on NumList
 |
 |	If NumList is a list of arithmetic expressions which are all
 |	ground at the time of evaluation, then Result is the sum
 |	of the evaluations of all the numbers on NumList.  
 |	If NumList = [], then Result = 0.
 *!-----------------------------------------------------------------------*/
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
 |	- Returns the product of the evaluations of the numbers on NumList
 |
 |	If NumList is a list of arithmetic expressions which are all
 |	ground at the time of evaluation, then Result is the product
 |	of the evaluations of all the numbers on NumList.  
 |	If NumList = [], then Result = 1.
 *!-----------------------------------------------------------------------*/
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
 |	- Returns the sum of the squares of the evaluations of the numbers on NumList
 |
 |	If NumList is a list of arithmetic expressions which are all
 |	ground at the time of evaluation, then Result is the sum
 |	of the squares of the evaluations of all the numbers on NumList.  
 |	If NumList = [], then Result = 0.
 *!-----------------------------------------------------------------------*/
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
 |	- Returns the sum of squares of differences of XList,YList
 |
 |	If XList and YList are both lists of numbers and are of the
 |	same length, then Result is the sum of the squares of the
 |	pairwise differences of XList and YList; that is
 |		Result = (X1-Y1)^2 + (X2-Y2)^2 + ...
 |	If XList = YList = [], then Result = 0.
 *!-----------------------------------------------------------------------*/
sum_square_diffs([], [], 0).

sum_square_diffs( [ X | XListTail ],  [ Y | YListTail ], Result) 
	:-
	sum_square_diffs(XListTail, YListTail, InterResult),
	Result is InterResult + (X - Y) * (X - Y).
 
/*!-----------------------------------------------------------------------
 |	max_vector/3
 |	max_vector(Left, Right, Result)
 |	max_vector(+, +, -)
 |
 |	- Returns the result of iteratively taking the max of Left,Right
 |
 |	If Left and Right are both lists of numbers which are the same
 |	length and are ground at the time evaluation, then Result is the
 |	list of numbers of the same length as Left,Right, and such that
 |	the nth element of Result is the max of the nth elements of
 |	Left and Right, respectively.
 *!-----------------------------------------------------------------------*/
max_vector([], [], []).

max_vector([Left | LeftTail], [Right | RightTail], [Max | ResultTail])
        :-
        max(Left, Right, Max),
        max_vector(LeftTail, RightTail, ResultTail).

/*!-----------------------------------------------------------------------
 |	min_vector/3
 |	min_vector(Left, Right, Result)
 |	min_vector(+, +, -)
 |
 |	- Returns the result of iteratively taking the min of Left,Right
 |
 |	If Left and Right are both lists of numbers which are the same
 |	length and are ground at the time evaluation, then Result is the
 |	list of numbers of the same length as Left,Right, and such that
 |	the nth element of Result is the min of the nth elements of
 |	Left and Right, respectively.
 *!-----------------------------------------------------------------------*/
min_vector([], [], []).

min_vector([Left | LeftTail], [Right | RightTail], [Max | ResultTail])
        :-
        min(Left, Right, Max),
        min_vector(LeftTail, RightTail, ResultTail).

endmod.
