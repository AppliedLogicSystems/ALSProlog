---
title: '=/2'
group: Terms
iso: termcomp
predicates:
- {sig: '=/2', desc: 'unify two terms'}
- {sig: '\=/2', desc: 'test if two items are non-unifiable'}
---

## FORMS

```
Arg1 = Arg2

Arg1 \= Arg2
```

## DESCRIPTION

The procedure `=/2` calls the Prolog unifier to unify `Arg1` and `Arg2`, binding variables if necessary. The occurs check is not performed. `=/2` is defined as if by the clause :

`Term = Term.`

If the two terms cannot be unified, `=` fails. The procedure `\=` succeeds if the two terms cannot be unified. This is different than the `=\=` and the `\==` procedures.


## EXAMPLES

The following examples illustrate the use of `=`.

```
?- f(A,A)=f(a,B).
A=a
B=a
yes.
```

```
?- f(A,A)=f(a,b).
no.
```

```
?- X=f(X).
X=f(f(f(f(f(f(f(f(f(f(f(...)))))))))))
?- X\=1
no.
```

```
?- X\==1
X=_3
yes.
```

Note that in the next to last example, the depth of the printing is much deeper than shown here.


## SEE ALSO

- [`==/2`](identity.html)
- [`\==/2`](identity.html)
- `eq` {%- comment %} TODO: missing {% endcomment %}
- `noneq` {%- comment %} TODO: missing {% endcomment %}
- {% include book.md id="bowen91"    sec="4.6" %}
- {% include book.md id="bratko86"   sec="2.7" %}
- {% include book.md id="clocksin81" sec="6.8" %}
- {% include book.md id="sterling86" sec="4.1" %}
