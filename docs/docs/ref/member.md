---
title: 'member/2'
group: Terms
predicates:
- {sig: 'member/2', desc: 'list membership'}
- {sig: 'dmember/2', desc: 'list membership'}
---

## FORMS
```
member(Element, List)

dmember(Element, List)
```
## DESCRIPTION

`member/2` succeeds when `Element` can be unified with one of the elements in `List`. `dmember/2` is the determinate version of `member/2`.

## EXAMPLES
```
?- member(b, [a,b,c]).

yes.

?- member(foo, [bar | T]).

T=[foo|_A] 

yes.

?- member(X,[1,2,3]).

X=1;

X=2;

X=3;

no.

?- dmember(X,[1,2,3]).

X=1;

no.
```

## NOTES

`member/2` and `dmember/2` are defined by the following clauses :

```
member(Item, [Item | _ ]) .

member(Item, [_ | Rest ]) :- member(Item, Rest) .


dmember(Item, [Item | _ ]) :- !.

dmember(Item, [_ | Rest ]) :- dmember(Item, Rest) .
```
