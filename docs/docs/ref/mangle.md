---
title: 'mangle/3'
group: Terms
predicates:
- {sig: 'mangle/3', desc: 'destructively modify a structure'}
---

## FORMS
```
mangle(Nth, Structure, NewArg)
```
## DESCRIPTION

`mangle/3` destructively modifies an argument of a compound term in a spirit similar to Lisp's rplaca and rplacd. `Structure` must be instantiated to a compound term with at least `Nth` arguments. The `Nth` argument of Structure will become `NewArg`. Lists are considered to be structures of arity two in the usual way.

Modifications made to a structure by `mangle/3` will survive failure and backtracking.

Even though `mangle/3` implements destructive assignment in Prolog, it is not necessarily more efficient than copying a term. This is due to the extensive cleanup operation which ensures that the effects of a `mangle/3` persist across failure.

## EXAMPLES
```
?- Victim = doNot(fold,staple,mutilate), mangle(2,Victim,spindle).

Victim = doNot(fold,spindle,mutilate)

yes.
```
## SEE ALSO

- [`arg/3`](arg.html)
