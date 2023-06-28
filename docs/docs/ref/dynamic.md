---
title: 'dynamic/1'
group: Prolog Database
iso: dynamic
predicates:
- {sig: 'dynamic/1', desc: 'declare a procedure to be dynamic'}
---

## FORMS
```
dynamic(Pred/Arity)

dynamic(Module:Pred/Arity)
```
## DESCRIPTION

`dynamic/1` is a procedure intended to be used in directives in source code. It will declare a procedure given by the form `Pred/Arity` or `Module:Pred/Arity` to be dynamic. Such a procedure will be considered to be defined even if it contains no clauses. Non-dynamic procedures which have no clauses are considered to be undefined and if called as such will generate a warning or error(depending on the value of the `undefined_predicate flag`). In the future, procedures declared to be dynamic will also be subject to the so called &quot;logical database&quot; semantics where the database will appear to be frozen once a procedure is called. Only calls that occur (temporally) after the database modification will be affected by that modification.

## EXAMPLES
```
:- dynamic(foo/1) .
```
## NOTES

Calling [`assert/1`](assert.html) or one of its variants for an undefined procedure will also effectively declare the procedure to be dynamic.

## SEE ALSO

- [`consult/1`](consult.html)
- [`assert/[1,2]`](assert.html)
