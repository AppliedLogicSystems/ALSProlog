---
title: 'is/2'
predicates:
- {sig: 'is/2', desc: 'evaluates an arithmetic expression'}
---

## FORMS
```
Result is Expression
```
## DESCRIPTION

`Expression` should be a ground term that can be evaluated. Numbers evaluate as themselves, and a list evaluates as the first element of the list. The operators listed in _Arithmetic Operators_ and
_Arithmetic Functions_ can also be evaluated when their arguments can be evaluated. If `Result` is an unbound variable, then it will be bound to the numeric value of `Expression`. If `Result` is not unbound, then it will be evaluated, and the value of the `Result` will be unified with the value of `Expression`.




<hr>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**Arithmetic Operators**

|Operator|Description|
|---------|------------|
| -X | unary minus | 
| X div Y | integer division | 
| X mod Y | X(integer) | 
| X xor Y | X | 
| X * Y | multiplication | 
| X + Y | addition | 
| X-Y | subtraction | 
| X/Y | division | 
| X//Y | integer division | 
| X/\Y | integer bitwise | 
| X &lt; &lt; Y | integer bitwise left | 
| X &gt; &gt; Y | integer bitwise right shift of X by Y places | 
| X\/Y | integer bitwise | 
| X^Y | X to the | 
| \X not(X) | integer bitwise | 
| 0 ' Char | the | 



<hr>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**Arithmetic Functions**

|Function|Description|
|---------|------------|
| abs(X) | absolute value | 
| acos(X) | arc cosine | 
| asin(X) | arc sine | 
| atan(X) | arc tangent | 
| cos(X) | cosine | 
| cputime | CPU time in seconds since ALS Prolog started. | 
| exp(X) | natural | 
| exp10(X) | base 10 exponential function | 
| floor(X) | the largest integer not greater than X | 
| heapused | heap space in use, in bytes | 
| j0(X) | Bessel function of order 0 | 
| j1(X) | Bessel function of order 1 | 
| log(X) | natural | 
| log10(X) | base 10 logarithm | 
| random | returns a | 
| realtime | actual | 
| round(X) | integer rounding of X | 
| sin(X) | sine | 
| sqrt(X) | square root | 
| tan(X) | tangent | 
| trunc(X) | the largest integer not greater than X | 
| y0(X) | Bessel function of second kind of order 0 | 
| y1(X) | Bessel function of second kind of order 1 | 




## EXAMPLES
```
?- 2 is 3-1.

yes.

?- X is 6*7

X=42

yes.

?- X is 2.5+3.5.

X=6

yes.

?- Result is sqrt(2).

Result=1.414213562 

yes.
```

## ERRORS

`is/2` fails when it attempts to evaluate an unknown operator, or if `Expression` is not ground. Failure also occurs if there are any arithmetic faults, such as overflow, underflow, or division by zero.


## NOTES

ALS Prolog complies to the ISO Prolog Standard regarding errors. A calculation error will be thrown on overflow, underflow, division by zero, or use of an unrecognized arithmetic operator.


## SEE ALSO

- [Bowen 91, 7.7]
- [Clocksin 81, 6.11]
- [Bratko 86, 3.4]
