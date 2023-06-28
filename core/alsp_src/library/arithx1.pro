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
 |
 | Examples: 
 |
 |	?- max(3,92,M1), max(3,50-92,M2), max(-6,-7*9,M3), max(23.89, 23.87,M4).
 |	
 |	M1=92
 |	M2=3
 |	M3=-6
 |	M4=23.89
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
 |
 | Examples: 
 |
 |	?- min(3,92,M1), min(3*1,-0-92,M2), min(-6*(-5),-9,M3), min(23.89, 23.87,M4).
 |	
 |	M1=3
 |	M2=-92
 |	M3=-9
 |	M4=23.87
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
 |
 | Examples: 
 |
 |	?- maximum([3.4, 2+3, -6*1.78, 123.4456], M1),
 |	?_  maximum([-3.4, -5, -6.78, -123.4456], M2).
 |	
 |	M1=123.4456
 |	M2=-3.4
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
 |
 | Examples: 
 |
 |	?-  minimum([3.4, 5*5, -6*1.78, 9+123.4456], M1),
 |	?_ minimum([-3.4+9, -5*6, -6.78, -123.4456], M2).
 |	
 |	M1=-6*1.78
 |	M2=-123.4456
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
 |
 | Examples: 
 |
 |	?- sumlist([3.4, 5*5, -6*1.78, 9+123.4456], M1),
 |	?_ sumlist([-3.4+9, -5*6, -6.78, -123.4456], M2).
 |	
 |	M1=150.1656
 |	M2=-154.6256
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
 |
 | Examples: 
 |
 |	?-  prodlist([3.4, 5*5, -6*1.78, 9+123.4456], M1),
 |	?_ prodlist([-3.4+9, -5*6, -6.78, -123.4456], M2).
 |	
 |	M1=-120234.1157
 |	M2=-140609.4762
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
 |
 | Examples: 
 |
 |	?- sum_squares([3,4], M1), sum_squares([88 + 0.98, 4.3*6.77], M2).
 |	
 |	M1=25
 |	M2=8764.890721
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
 |
 | Examples: 
 |
 |	?- XList = [4.3*1, 4+20.88], YList = [5.12+2.3, 3*20.44],
 |	?_ sum_square_diffs(XList, YList, Result).
 |	
 |	XList=[4.3*1,4+20.88]
 |	YList=[5.12+2.3,3*20.44]
 |	Result=1337.608
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
 |
 | Examples: 
 |
 |	?- Left = [3, 4.1 + 0.4, 88.9], Right = [0.6 * 2, 8.44, 100.9],
 |	?_ max_vector(Left, Right, Result).
 |	
 |	Left=[3,4.1+0.4,88.9] 
 |	Right=[0.6*2,8.44,100.9] 
 |	Result=[3,8.44,100.9]
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
 |
 | Examples: 
 |
 |	?- Left = [3, 4.1 + 0.4, 88.9], Right = [0.6 * 2, 8.44, 100.9],
 |	?_ min_vector(Left, Right, Result).
 |	
 |	Left=[3,4.1+0.4,88.9]
 |	Right=[0.6*2,8.44,100.9]
 |	Result=[1.2,4.5,88.9]
 *!-----------------------------------------------------------------------*/
min_vector([], [], []).

min_vector([Left | LeftTail], [Right | RightTail], [Max | VectorResultTail])
        :-
        min(Left, Right, Max),
        min_vector(LeftTail, RightTail, VectorResultTail).

endmod.
