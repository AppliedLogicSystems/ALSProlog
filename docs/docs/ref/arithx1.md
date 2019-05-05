---
title: 'max/3'
package: ALS Library
group: Arithmetic
predicates:
- {sig: 'max/3', desc: 'C is the maximum of results of evaluation of A,B '}
- {sig: 'max_vector/3', desc: 'Returns the result of iteratively taking the max of Left,Right'}
- {sig: 'maximum/2', desc: 'Returns the maximum of the evaluations of the numbers on NumList'}
- {sig: 'min/3', desc: 'C is the minimum of results of evaluation of A,B '}
- {sig: 'min_vector/3', desc: 'Returns the result of iteratively taking the min of Left,Right'}
- {sig: 'minimum/2', desc: 'Returns the minimum of the evaluations of the numbers on NumList'}
- {sig: 'prodlist/2', desc: 'Returns the product of the evaluations of the numbers on NumList'}
- {sig: 'sum_square_diffs/3', desc: 'Returns the sum of squares of differences of XList,YList'}
- {sig: 'sum_squares/2', desc: 'Returns the sum of the squares of the evaluations of the numbers on NumList'}
- {sig: 'sumlist/2', desc: 'Returns the sum of the evaluations of the numbers on NumList'}
---
## FORMS

`max(A,B,C)`

`max_vector(Left, Right, Result)`

`maximum(NumList, Result)`

`min(A,B,C)`

`min_vector(Left, Right, Result)`

`minimum(NumList, Result)`

`prodlist(NumList, Result)`

`sum_square_diffs(XList, YList, Result)`

`sum_squares(NumList, Result)`

`sumlist(NumList, Result)`

## DESCRIPTION

**`max/3`** If A and B are arithmetic expressions which are ground at the  
    time of evaluation, then C is the maximum of the results of  
    evaluation of A and B with is/2.  

**`max_vector/3`** If Left and Right are both lists of numbers which are the same  
    length and are ground at the time evaluation, then Result is the  
    list of numbers of the same length as Left,Right, and such that  
    the nth element of Result is the max of the nth elements of  
    Left and Right, respectively.  

**`maximum/2`** If NumList is a list of arithmetic expressions which are all  
    ground at the time of evaluation, then Result is the maximum  
    of the evaluations of all the numbers on NumList.  If  
    NumList = [], then Result = -0inf.  

**`min/3`** If A and B are arithmetic expressions which are ground at the  
    time of evaluation, then C is the minimum of the results of  
    evaluation of A and B with is/2.  

**`min_vector/3`** If Left and Right are both lists of numbers which are the same  
    length and are ground at the time evaluation, then Result is the  
    list of numbers of the same length as Left,Right, and such that  
    the nth element of Result is the min of the nth elements of  
    Left and Right, respectively.  

**`minimum/2`** If NumList is a list of arithmetic expressions which are all  
    ground at the time of evaluation, then Result is the minimum  
    of the evaluations of all the numbers on NumList.  If  
    NumList = [], then Result = 0inf.  

**`prodlist/2`** If NumList is a list of arithmetic expressions which are all  
    ground at the time of evaluation, then Result is the product  
    of the evaluations of all the numbers on NumList.  
    If NumList = [], then Result = 1.  

**`sum_square_diffs/3`** If XList and YList are both lists of numbers and are of the  
    same length, then Result is the sum of the squares of the  
    pairwise differences of XList and YList; that is  
    Result = (X1-Y1)^2 + (X2-Y2)^2 + ...  
    If XList = YList = [], then Result = 0.  

**`sum_squares/2`** If NumList is a list of arithmetic expressions which are all  
    ground at the time of evaluation, then Result is the sum  
    of the squares of the evaluations of all the numbers on NumList.  
    If NumList = [], then Result = 0.  

**`sumlist/2`** If NumList is a list of arithmetic expressions which are all  
    ground at the time of evaluation, then Result is the sum  
    of the evaluations of all the numbers on NumList.  
    If NumList = [], then Result = 0.  

## EXAMPLES

