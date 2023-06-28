---
title: 'functor/3'
group: Terms
iso: functor
predicates:
- {sig: 'functor/3', desc: 'builds structures and retrieves information about them'}
---

## FORMS
```
functor(Structure, Functor, Arity)
```
## DESCRIPTION

The principal functor of term `Structure` has name `Functor` and arity `Arity`, where `Functor` is an atom. Either `Structure` must be instantiated to a term or an atom, or `Functor` and `Arity` must be instantiated to an atom and a non-negative integer, respectively.

In the case where `Structure` is initially unbound, `functor/3` will unify `Structure` with a structured term of `Arity` arguments, where the principal functor of the term is `Functor`. Each argument of the new structure will be a new uninstantiated variable.

When `Structure` is instantiated to a structured term, `Functor` will be unified with the principal functor of `Structure` and `Arity` will be unified with the arity. `functor/3` treats atoms as structured terms with arity 0. The principal functor of a list is ' . ' with arity 2.

## EXAMPLES
```
?- functor(Structure,fish,2).

Structure=fish(_123,_124)

yes.

?- functor(city('SantaMonica','CA','USA'), Functor, Arity).

Functor=city
Arity=3

yes.
```


## SEE ALSO

- [`arg/3`](arg.html)
- [`mangle/3`](mangle.html)

- {% include book.md id="bowen91"    sec="7.6" %}
- {% include book.md id="clocksin81" sec="6.5" %}
- {% include book.md id="bratko86"   sec="7.2" %}
- {% include book.md id="sterling86" sec="9.2" %}
