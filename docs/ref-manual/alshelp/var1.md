---
title: 'var/1'
group: Prolog Database
predicates:
- {sig: 'var/1', desc: 'the variable is unbound'}
- {sig: 'nonvar/1', desc: 'the variable is instantiated'}
---

## FORMS

```
var(Term)
nonvar(Term)
```

## DESCRIPTION

`var/1` succeeds if `Term` is an unbound variable, and fails otherwise.
`nonvar/1` succeeds when `Term` is a constant or structured term.


## EXAMPLES

```
?- var(constant).
no.
```

```
?- nonvar(constant).
yes.
```

```
?- X=Y, Y=Z, Z=doughnut, var(X).
no.
```


## SEE ALSO

- [Bowen 91, 7.6]
- [Bratko 86, 7.1.1]
- [Sterling 86, 10.1]

