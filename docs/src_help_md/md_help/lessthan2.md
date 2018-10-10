---
title: '</2'
predicates:
 - '</2' : The left expression is less than the right expression
 - '>/2' : The left expression is greater than the right expression
 - '=:=/2' : The left and right expressions are equal
 - '=\=/2' : The left and right expressions are not equal
 - '=</2' : The left expression is less than or equal to the right
 - '>=/2' : The left expression is greater than or equal to the right
---
`</2` — The left expression is less than the right expression

`>/2` — The left expression is greater than the right expression

`=:=/2` — The left and right expressions are equal

`=\=/2` — The left and right expressions are not equal

`=</2` — The left expression is less than or equal to the right

`>=/2` — The left expression is greater than or equal to the right


## FORMS

Expression1 < Expression2

Expression1 > Expression2

Expression1 = : = Expression2

Expression1 = \ = Expression2

Expression1 = < Expression2

Expression1 > = Expression2



## DESCRIPTION

Both arguments to each relational operator should be instantiated to expressions which can be evaluated by is/2. The relational operator succeeds if the relation holds for the value of the two arguments, and fails otherwise. A relational operator will fail if one or both of its arguments cannot be evaluated.


## EXAMPLES

```
?- -7*0=<1+1
yes.
```

```
?- 1+1=<7*0
no.
```


## ERRORS

The ISO Prolog Standard requires that a calculation error be thrown when the arguments cannot be evaluated a for these operators. At this time, ALS Prolog does not conform to this requirement.

