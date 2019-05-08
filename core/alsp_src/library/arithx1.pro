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
 |	max(Expr1,Expr2,NumResult)
 |	max(+,+,-)
 |
 |	- Returns the maximum of results of evaluating two arithmetic Exprs
 | 
 |	If Expr1 and Expr2 are arithmetic expressions which are ground at the
 |	time of evaluation, then NumResult is the maximum of the results of 
 |	evaluation of Expr1 and Expr2 with is/2.
 *!-----------------------------------------------------------------------*/
max(Expr1,Expr2,NumResult) 
	:- 
	A0 is Expr1, 
	B0 is Expr2, 
	max0(A0,B0,NumResult).

max0(A,B,A) 
	:- 
	A >= B,!.

max0(A,B,B).

/*!-----------------------------------------------------------------------
 |	min/3
 |	min(Expr1,Expr2,NumResult)
 |	min(+,+,-)
 |
 |	- Returns the minimum of results of evaluating two arithmetic Exprs
 | 
 |	If Expr1 and Expr2 are arithmetic expressions which are ground at the
 |	time of evaluation, then NumResult is the minimum of the results of 
 |	evaluation of Expr1 and Expr2 with is/2.
 *!-----------------------------------------------------------------------*/
min(Expr1,Expr2,NumResult) 
	:- 
	A0 is Expr1, 
	B0 is Expr2, 
	min0(A0,B0,NumResult).

min0(A,B,A) 
	:- 
	A =< B,!.

min0(A,B,B).

/*!-----------------------------------------------------------------------
 |	maximum/2
 |	maximum(ExprList, NumResult)
 |	maximum(+, -)
 |
 |	- Returns the maximum of the evaluations of the expressions on ExprList
 |
 |	If ExprList is a list of arithmetic expressions which are all
 |	ground at the time of evaluation, then NumResult is the maximum
 |	of the evaluations of all the expressions on ExprList.  If 
 |	ExprList = [], then NumResult = -0inf.
 *!-----------------------------------------------------------------------*/
maximum(ExprList, NumResult)
	:-
	maximum(ExprList, -0inf, NumResult).

maximum([], Cur, Cur).

maximum([Expr | Tail], Cur, NumResult)
	:-
	max(Expr, Cur, NextMax),
	maximum(Tail, NextMax, NumResult).

/*!-----------------------------------------------------------------------
 |	minimum/2
 |	minimum(ExprList, NumResult)
 |	minimum(+, -)
 |
 |	- Returns the minimum of the evaluations of the expressions on ExprList
 |
 |	If ExprList is a list of arithmetic expressions which are all
 |	ground at the time of evaluation, then NumResult is the minimum
 |	of the evaluations of all the expressions on ExprList.  If 
 |	ExprList = [], then NumResult = 0inf.
 *!-----------------------------------------------------------------------*/
minimum(ExprList, NumResult)
	:-
	minimum(ExprList, 0inf, NumResult).

minimum([], Cur, Cur).

minimum([Expr | Tail], Cur, NumResult)
	:-
	min(Expr, Cur, NextMin),
	minimum(Tail, NextMin, NumResult).

/*!-----------------------------------------------------------------------
 |	sumlist/2
 |	sumlist(ExprList, NumResult)
 |	sumlist(+, -)
 |
 |	- Returns the sum of the evaluations of the numbers on ExprList
 |
 |	If ExprList is a list of arithmetic expressions which are all
 |	ground at the time of evaluation, then NumResult is the sum
 |	of the evaluations of all the numbers on ExprList.  
 |	If ExprList = [], then NumResult = 0.
 *!-----------------------------------------------------------------------*/
sumlist([], 0).

sumlist( [ Num | ExprListTail ], NumResult) 
	:-
	sumlist(ExprListTail, InterResult),
	NumResult is InterResult + Num.

/*!-----------------------------------------------------------------------
 |	prodlist/2
 |	prodlist(ExprList, NumResult)
 |	prodlist(+, -)
 |
 |	- Returns the product of the evaluations of the numbers on ExprList
 |
 |	If ExprList is a list of arithmetic expressions which are all
 |	ground at the time of evaluation, then NumResult is the product
 |	of the evaluations of all the numbers on ExprList.  
 |	If ExprList = [], then NumResult = 1.
 *!-----------------------------------------------------------------------*/
prodlist([], 1).

prodlist( [ Num | ExprListTail ], NumResult) 
	:-
	prodlist(ExprListTail, InterResult),
	NumResult is InterResult * Num.

/*!-----------------------------------------------------------------------
 |	sum_squares/2
 |	sum_squares(ExprList, NumResult)
 |	sum_squares(+, -)
 |
 |	- Returns the sum of the squares of the evaluations of the numbers on ExprList
 |
 |	If ExprList is a list of arithmetic expressions which are all
 |	ground at the time of evaluation, then NumResult is the sum
 |	of the squares of the evaluations of all the numbers on ExprList.  
 |	If ExprList = [], then NumResult = 0.
 *!-----------------------------------------------------------------------*/
sum_squares([], 0).

sum_squares( [ Num | ExprListTail ], NumResult) 
	:-
	sum_squares(ExprListTail, InterResult),
	NumResult is InterResult + (Num * Num).

/*!-----------------------------------------------------------------------
 |	sum_square_diffs/3
 |	sum_square_diffs(XList, YList, NumResult)
 |	sum_square_diffs(+, +, -)
 |
 |	- Returns the sum of squares of differences of XList,YList
 |
 |	If XList and YList are both lists of numbers and are of the
 |	same length, then NumResult is the sum of the squares of the
 |	pairwise differences of XList and YList; that is
 |		NumResult = (X1-Y1)^2 + (X2-Y2)^2 + ...
 |	If XList = YList = [], then NumResult = 0.
 *!-----------------------------------------------------------------------*/
sum_square_diffs([], [], 0).

sum_square_diffs( [ X | XListTail ],  [ Y | YListTail ], NumResult) 
	:-
	sum_square_diffs(XListTail, YListTail, InterResult),
	NumResult is InterResult + (X - Y) * (X - Y).
 
/*!-----------------------------------------------------------------------
 |	max_vector/3
 |	max_vector(Left, Right, VectorResult)
 |	max_vector(+, +, -)
 |
 |	- Returns the result of iteratively taking the max of Left,Right
 |
 |	If Left and Right are both lists of numbers which are the same
 |	length and are ground at the time evaluation, then VectorResult is the
 |	list of numbers of the same length as Left,Right, and such that
 |	the nth element of VectorResult is the max of the nth elements of
 |	Left and Right, respectively.
 *!-----------------------------------------------------------------------*/
max_vector([], [], []).

max_vector([Left | LeftTail], [Right | RightTail], [Max | VectorResultTail])
        :-
        max(Left, Right, Max),
        max_vector(LeftTail, RightTail, VectorResultTail).

/*!-----------------------------------------------------------------------
 |	min_vector/3
 |	min_vector(Left, Right, VectorResult)
 |	min_vector(+, +, -)
 |
 |	- Returns the result of iteratively taking the min of Left,Right
 |
 |	If Left and Right are both lists of numbers which are the same
 |	length and are ground at the time evaluation, then VectorResult is the
 |	list of numbers of the same length as Left,Right, and such that
 |	the nth element of VectorResult is the min of the nth elements of
 |	Left and Right, respectively.
 *!-----------------------------------------------------------------------*/
min_vector([], [], []).

min_vector([Left | LeftTail], [Right | RightTail], [Max | VectorResultTail])
        :-
        min(Left, Right, Max),
        min_vector(LeftTail, RightTail, VectorResultTail).

endmod.
