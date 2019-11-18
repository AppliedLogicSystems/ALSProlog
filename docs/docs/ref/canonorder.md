---
title: '@=</2'
group: Terms
iso: termcomp
predicates:
- {sig: '@=</2', desc: 'The left argument is not after the right argument'}
- {sig: '@>=/2', desc: 'The left argument is not before the right argument'}
- {sig: '@</2', desc: 'The left argument is before the right argument'}
- {sig: '@>/2', desc: 'The left argument is after the right argument'}
---

## FORMS
```
Arg1 @=< Arg2

Arg1 @>= Arg2

Arg1 @< Arg2

Arg1 @> Arg2
```
## DESCRIPTION

These predicates compare two terms according to the standard order as defined by [`compare/3`](compare.html). The terms are compared as-is without any transformation or interpretation. The order of a partially instantiated term may change as the level of instantiation changes.

## EXAMPLES

```
?- foobar(something)@>foobar.
yes.
```

```
?- cement@>mortar.
no.
```

```
?- inflation_today@=<inflation_tomorrow.
yes.
```

```
?- rain_in(newYork)@>=rain_in(arizona).
yes.
```
## SEE ALSO

- [`compare/3`](compare.html)
