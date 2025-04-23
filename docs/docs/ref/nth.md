---
title: 'nth/3'
group: Terms
predicates:
- {sig: 'nth/3', desc: 'returns the nth element of a list'}
---

## FORMS
```
nth(N, List, X)
```
## DESCRIPTION

If `List` is a list and `N` is a non-negative integer, then `X` is the nth element of List.

## EXAMPLES

```
?- nth(1,[larry,moe,curly],X).
X=moe
yes.
```

## ERRORS

If `N` is not a non-negative integer and less than the length of `List`, `nth/3` will fail. `List` must be instantiated to a list.

## SEE ALSO

- [ALS List Library](./#lists)
- [12.3 Lists: Positional List Predicates](../guide/12-The-ALS-Library-Mechanism.html#123-lists-positional-list-predicates-listutl2pro)