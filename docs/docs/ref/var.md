---
title: 'var/1'
group: Terms
iso: var
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

- {% include book.md id="bowen91"    sec="7.6" %}
- {% include book.md id="bratko86"   sec="7.1.1" %}
- {% include book.md id="sterling86" sec="10.1" %}

