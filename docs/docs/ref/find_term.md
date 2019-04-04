---
title: '$findterm/5'
predicates:
- {sig: '$findterm/5', desc: 'locates the given term on the heap'}
---

## FORMS
```
' $findterm '(Functor, Arity, HeapPos, Term, NewHeapPos)
```
## DESCRIPTION

The term whose `Functor` and `Arity` are given is searched for on the heap starting from the given heap position `HeapPos`. If the term is located, the fourth argument, `Term`, is unified with the term found in the heap, and the fifth argument, `NewHeapPos`, is unified with a pointer to the next heap location after the term found. Heap positions are offsets with respect to the heap base. If the term cannot be found, this predicate fails.

## EXAMPLES
```
?- X = f(a,b), '$findterm'(f, 2, 0, T, NHP).

X = f(a,b)

T = f(a,b)

NHP = 3

yes.
```

