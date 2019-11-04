---
title: 'repeat/0'
iso: repeat
predicates:
- {sig: 'repeat/0', desc: 'always succeed upon backtracking'}
---

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

- [`fail/0`](fail.html)

- {% include book.md id="sterling86" sec="12.5" %}
- {% include book.md id="bratko86"   sec="7.5" %}
- {% include book.md id="clocksin81" sec="6.6" %}
