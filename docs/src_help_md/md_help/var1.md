---
title: 'var/1'
predicates:
 - 'var/1' : the variable is unbound
 - 'nonvar/1' : the variable is instantiated
---
`var/1` — the variable is unbound  
`nonvar/1` — the variable is instantiated


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

