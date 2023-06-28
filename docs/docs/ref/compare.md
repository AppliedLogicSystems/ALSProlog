---
title: 'compare/3'
group: Terms
predicates:
- {sig: 'compare/3', desc: 'compares two terms in the standard order'}
---

## FORMS
```
compare(Relation, TermL, TermR)
```
## DESCRIPTION

`TermL` and `TermR` are compared according to the
**_[standard,order]_**
defined below. `Relation` is unified with an atom representing the result of the comparison. `Relation` is unified with:

`=` when `TermL` is identical to `TermR`

`<` when `TermL` is before `TermR`

`>` when `TermL` is after `TermR`

The
**_[standard,order]_**
provides a means to compare and sort general Prolog terms. The order is somewhat arbitrary in how it sorts terms of different types. For example, an atom is always " less than " a structure. Here ' s the entire order :

Variables &lt; Numbers &lt; Atoms &lt; Structured Terms

**_[Variables]_**
are compared according to their relative locations in the Prolog data areas. Usually a recently created variable will be greater than an older variable. However, the apparent age of a variable can change without notice during a computation; this makes using the comparison of uninstantiated (but not instantiated) variables extremely tricky.

**_[Numbers]_**
are ordered according to their signed magnitude. Integers and floating point values are ordered correctly, so `compare/3` can be used to sort numbers.

**_[Atoms]_**
are sorted by the ASCII order of their print names. If one atom is an initial substring of another, the longer atom will appear later in the standard order.

**_[Structured,terms]_**
are ordered first by arity, then by the ASCII order of their principal functor. If two terms have the same functor and arity, then `compare/3` will recursively compare their arguments to determine the order of the two.

More precisely, if `TermL` and `TermR` are structured terms, then

`TermL @< TermR` holds if and only if :

the arity of `TermL` is less than the arity of `TermR`, or

`TermL` and `TermR` have the same arity, and the functor name of `TermL` precedes
the functor name of `TermR` in the standard order, or

`TermL` and `TermR` have the same arity and functor name, 
and there is an integer N less than or equal to the arity of `TermL` such that for all i less than N,
the ith arguments of `TermL` and `TermR` are identical, and
the Nth argument of `TermL` precedes the Nth argument of `TermR` in the standard order.

## EXAMPLES

The following examples show the use of `compare/3` :

```
?- Myself=I, compare(=,Myself,I).

Myself=_4
I=_4

yes.

?- compare(>,100,99).
yes.

?- compare(<,boy,big(boy)).
yes.
```

The following example shows the way structures are compared :

```
?- compare(Order, and(a,b,c), and(a,b,a,b)).

Order='<'

yes.
```

This says that the structure
```
    and(a, b, c)
```
comes after the structure
```
    and(a, b, a, b)
```
in the standard order, because the second structure has a greater arity than the first.


## SEE ALSO

- [`==/2`](identity.html)
- [`@</2`](canonorder.html)
- [`sort/2`](sort.html)

- {% include book.md id="bowen91"    sec="7.4" %}
