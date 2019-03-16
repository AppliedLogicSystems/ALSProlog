---
title: 'setPrologInterrupt/1'
predicates:
- {sig: 'setPrologInterrupt/1', desc: 'establish the type of a Prolog interrupt'}
- {sig: 'getPrologInterrupt/1', desc: 'determine the type of a Prolog interrupt'}
---

## FORMS

```
setPrologInterrupt(Term)

getPrologInterrupt(Term)
```

## DESCRIPTION

`Term` is an arbitrary Prolog term.

`setPrologInterrupt(Term)` sets the value of the global interrupt variable to be `Term`.

`getPrologInterrupt(Term)` fetches the value of the global interrupt variable and unifies it with `Term`.


## SEE ALSO

- [`forcePrologInterrupt/0`](forcePrologInterrupt0.html)
- [`callWithDelayedInterrupt/1`](forcePrologInterrupt0.html)
- [User Guide (Prolog Interrupts)](../guide/9-Freeze,-Exceptions,-Events,-Interrupts,-Signals.md)

