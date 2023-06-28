---
title: 'sort/2'
group: Terms
predicates:
- {sig: 'sort/2', desc: 'sorts a list of terms'}
- {sig: 'keysort/2', desc: 'sorts a list of Key-Data pairs'}
---

## FORMS

```
sort(List, SortedList)

keysort(List, SortedList)
```

## DESCRIPTION

`sort/2` sorts the `List` according to the standard order. Identical elements, as defined by [`==/2`](identity.html), are merged, so that each element appears only once in `SortedList`.

`keysort/2` expects `List` to be a list of terms of the form: `Key-Data`. Each pair is sorted by the `Key` alone. Pairs with duplicate `Keys` will not be removed from `SortedList`.

A merge sort is used internally by these predicates at a cost of at most N(log N) where N is the number of elements in `List`.


## EXAMPLES

The following examples illustrate the use of `sort/2` and `keysort/2`:

```
?- sort([orange,apple,orange,tangelo,grape],X).
X = [apple,grape,orange,tangelo]
yes.

?- keysort([warren-davidh, bowen-kenneth,
            warren-davids, bowen-david,
            burger-warren], X).
X = [bowen-kenneth,bowen-david,burger-warren,
     warren-davidh,warren-davids]
yes.
```

The following example shows the way structures with the same principal functor are sorted:

```
?- sort([and(a,b,c), and(a,b,a,b), and(a,a), and(b)],Sorted).
Sorted = [and(a,a),and(a,b,a,b),and(a,b,c),and(b)]
yes.
```

## SEE ALSO

- [`compare/3`](compare.html)
- {% include book.md id="bowen91"    sec="7.4" %}
