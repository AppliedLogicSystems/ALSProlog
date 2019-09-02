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

**`max/3`**
```

?- max(3,92,M1), max(3,50-92,M2), max(-6,-7*9,M3), max(23.89, 23.87,M4).

M1=92
M2=3
M3=-6
M4=23.89
```

**`min/3`**
```

?- min(3,92,M1), min(3*1,-0-92,M2), min(-6*(-5),-9,M3), min(23.89, 23.87,M4).

M1=3
M2=-92
M3=-9
M4=23.87
```

**`maximum/2`**
```

?- maximum([3.4, 2+3, -6*1.78, 123.4456], M1),
   maximum([-3.4, -5, -6.78, -123.4456], M2).

M1=123.4456
M2=-3.4
```

**`minimum/2`**
```

?- minimum([3.4, 5*5, -6*1.78, 9+123.4456], M1),
   minimum([-3.4+9, -5*6, -6.78, -123.4456], M2).

M1=-6*1.78
M2=-123.4456
```

**`sumlist/2`**
```

?- sumlist([3.4, 5*5, -6*1.78, 9+123.4456], M1),
   sumlist([-3.4+9, -5*6, -6.78, -123.4456], M2).

M1=150.1656
M2=-154.6256
```

**`prodlist/2`**
```

?- prodlist([3.4, 5*5, -6*1.78, 9+123.4456], M1),
   prodlist([-3.4+9, -5*6, -6.78, -123.4456], M2).

M1=-120234.1157
M2=-140609.4762
```

**`sum_squares/2`**
```

?- sum_squares([3,4], M1), sum_squares([88 + 0.98, 4.3*6.77], M2).

M1=25
M2=8764.890721
```

**`sum_square_diffs/3`**
```

?- XList = [4.3*1, 4+20.88], YList = [5.12+2.3, 3*20.44],
   sum_square_diffs(XList, YList, Result).

XList=[4.3*1,4+20.88]
YList=[5.12+2.3,3*20.44]
Result=1337.608
```

**`max_vector/3`**
```

?- Left = [3, 4.1 + 0.4, 88.9], Right = [0.6 * 2, 8.44, 100.9],
   max_vector(Left, Right, Result).

Left=[3,4.1+0.4,88.9] 
Right=[0.6*2,8.44,100.9] 
Result=[3,8.44,100.9]
```

**`min_vector/3`**
```

?- Left = [3, 4.1 + 0.4, 88.9], Right = [0.6 * 2, 8.44, 100.9],
   min_vector(Left, Right, Result).

Left=[3,4.1+0.4,88.9]
Right=[0.6*2,8.44,100.9]
Result=[1.2,4.5,88.9]
```

