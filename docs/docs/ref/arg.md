---
title: 'arg/3'
group: Terms
iso: arg
predicates:
- {sig: 'arg/3', desc: 'access the arguments of a structured term'}
---

## FORMS
```
arg(Nth, Structure, Argument)
```
## DESCRIPTION

`Argument` will be unified with the `Nth` argument of `Structure`. `Nth` must be a positive integer. `Structure` should be a compound term whose arity is greater than or equal to `Nth`. When these conditions hold, `Argument` will be unified with the `Nth` argument of `Structure`.


## EXAMPLES

```
?- arg(2,stooges(larry,moe,curly),X).
X=moe
yes.

?- arg(2,[a,b,c],X).
X=[b,c]
yes.
```

## ERRORS

If `Nth` is not an integer greater than `0` and less than or equal to the arity of `Structure`, `arg/3` will fail. `Structure` must be instantiated to a structured term.


## SEE ALSO

- [`functor/3`](functor.html)
- `=../2` {%- comment %} TODO: missing {% endcomment %}

- {% include book.md id="bowen91"    sec='9.1' %}
- {% include book.md id="sterling86" sec='9.2' %}
- {% include book.md id="bratko86"   sec='7.2' %}
- {% include book.md id="clocksin81" sec='6.5' %}
