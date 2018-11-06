---
title: 'setPrologInterrupt/1'
predicates:
- {sig: 'setPrologInterrupt/1', desc: 'establish the type of a Prolog interrupt'}
- {sig: 'getPrologInterrupt/1', desc: 'determine the type of a Prolog interrupt'}
---
`setPrologInterrupt/1` — establish the type of a Prolog interrupt

`getPrologInterrupt/1` — determine the type of a Prolog interrupt


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

- `forcePrologInterrupt/1`
- `callWithDelayedInterrupt/1`
- User Guide (Prolog Interrupts)

