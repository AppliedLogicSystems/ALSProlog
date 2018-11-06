---
title: 'repeat/0'
predicates:
- {sig: 'repeat/0', desc: 'always succeed upon backtracking'}
---
`repeat/0` — always succeed upon backtracking

## FORMS
```
repeat
```
## DESCRIPTION

`repeat/0` always succeeds, even during backtracking. This behavior is useful for implementing loops which repeatedly perform some side-effect. `repeat/0` is defined by the following clauses:
```
repeat.

repeat :- repeat.
```
## EXAMPLES

The following procedure will repeat forever, reading in an expression and printing out its value.
```
loop 
    :-
    repeat,
    read(Expression),
    Value is Expression,
    write(' Value = '), write(Value), nl,
    fail.
```
## SEE ALSO

- `fail/0`

- [Sterling 86, 12.5]
- [Bratko 86, 7.5]
- [Clocksin 81, 6.6]
