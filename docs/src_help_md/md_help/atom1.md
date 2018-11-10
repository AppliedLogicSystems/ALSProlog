---
title: 'atom/1'
group: Terms
predicates:
- {sig: 'atom/1', desc: 'the term is an atom'}
- {sig: 'atomic/1', desc: 'the term is an atom or a number'}
- {sig: 'float/1', desc: 'the term is a floating point number'}
- {sig: 'integer/1', desc: 'the term is an integer'}
- {sig: 'number/1', desc: 'the term is an integer or a floating point'}
---

## FORMS
```
atom(Term)

atomic(Term)

float(Term)

integer(Term)

number(Term)

```
## DESCRIPTION

Each of these predicates will succeed when its argument is of the proper type, and fail otherwise. `integer/1` and `float/1` examine the only the representation of a number. For instance, the call `integer(2)` will succeed because `2` is represented internally as an integer. In addition, `float(4294967296)` will succeed because `4294967296` is represented by a floating point value since it is outside the range of the integer representation.


## EXAMPLES

The following are examples of the use of the type predicates :

```
?- atom(bomb).
yes.
```

```
?- integer(2001).
yes.
```

```
?- float(cement).
no.
```


## SEE ALSO

- `ar/1`  
`nonvar/1`
- [Bowen 91, 7.6]
- [Sterling 86, 9.1]
- [Bratko 86, 7.1]
- [Clocksin 81, 6.3]
