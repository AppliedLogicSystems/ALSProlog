---
title: 'max/3'
package: ALS Library
group: Arithmetic
predicates:
- {sig: 'max/3', desc: 'Returns the maximum of results of evaluating two arithmetic Exprs'}
- {sig: 'min/3', desc: 'Returns the minimum of results of evaluating two arithmetic Exprs'}
- {sig: 'maximum/2', desc: 'Returns the maximum of the evaluations of the expressions on ExprList'}
- {sig: 'minimum/2', desc: 'Returns the minimum of the evaluations of the expressions on ExprList'}
- {sig: 'sumlist/2', desc: 'Returns the sum of the evaluations of the numbers on ExprList'}
- {sig: 'prodlist/2', desc: 'Returns the product of the evaluations of the numbers on ExprList'}
- {sig: 'sum_squares/2', desc: 'Returns the sum of the squares of the evaluations of the numbers on ExprList'}
- {sig: 'sum_square_diffs/3', desc: 'Returns the sum of squares of differences of XList,YList'}
- {sig: 'max_vector/3', desc: 'Returns the result of iteratively taking the max of Left,Right'}
- {sig: 'min_vector/3', desc: 'Returns the result of iteratively taking the min of Left,Right'}
---
## FORMS

`max(Expr1,Expr2,NumResult)`

`min(Expr1,Expr2,NumResult)`

`maximum(ExprList, NumResult)`

`minimum(ExprList, NumResult)`

`sumlist(ExprList, NumResult)`

`prodlist(ExprList, NumResult)`

`sum_squares(ExprList, NumResult)`

`sum_square_diffs(XList, YList, NumResult)`

`max_vector(Left, Right, VectorResult)`

`min_vector(Left, Right, VectorResult)`

## DESCRIPTION

**`max/3`** If Expr1 and Expr2 are arithmetic expressions which are ground at the
    time of evaluation, then NumResult is the maximum of the results of
    evaluation of Expr1 and Expr2 with is/2.

**`min/3`** If Expr1 and Expr2 are arithmetic expressions which are ground at the
    time of evaluation, then NumResult is the minimum of the results of
    evaluation of Expr1 and Expr2 with is/2.

**`maximum/2`** If ExprList is a list of arithmetic expressions which are all
    ground at the time of evaluation, then NumResult is the maximum
    of the evaluations of all the expressions on ExprList.  If
    ExprList = [], then NumResult = -0inf.

**`minimum/2`** If ExprList is a list of arithmetic expressions which are all
    ground at the time of evaluation, then NumResult is the minimum
    of the evaluations of all the expressions on ExprList.  If
    ExprList = [], then NumResult = 0inf.

**`sumlist/2`** If ExprList is a list of arithmetic expressions which are all
    ground at the time of evaluation, then NumResult is the sum
    of the evaluations of all the numbers on ExprList.
    If ExprList = [], then NumResult = 0.

**`prodlist/2`** If ExprList is a list of arithmetic expressions which are all
    ground at the time of evaluation, then NumResult is the product
    of the evaluations of all the numbers on ExprList.
    If ExprList = [], then NumResult = 1.

**`sum_squares/2`** If ExprList is a list of arithmetic expressions which are all
    ground at the time of evaluation, then NumResult is the sum
    of the squares of the evaluations of all the numbers on ExprList.
    If ExprList = [], then NumResult = 0.

**`sum_square_diffs/3`** If XList and YList are both lists of numbers and are of the
    same length, then NumResult is the sum of the squares of the
    pairwise differences of XList and YList; that is
    NumResult = (X1-Y1)^2 + (X2-Y2)^2 + ...
    If XList = YList = [], then NumResult = 0.

**`max_vector/3`** If Left and Right are both lists of numbers which are the same
    length and are ground at the time evaluation, then VectorResult is the
    list of numbers of the same length as Left,Right, and such that
    the nth element of VectorResult is the max of the nth elements of
    Left and Right, respectively.

**`min_vector/3`** If Left and Right are both lists of numbers which are the same
    length and are ground at the time evaluation, then VectorResult is the
    list of numbers of the same length as Left,Right, and such that
    the nth element of VectorResult is the min of the nth elements of
    Left and Right, respectively.

## EXAMPLES

