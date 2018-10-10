---
title: 'member/2'
predicates:
 - 'member/2' : list membership
 - 'dmember/2' : list membership
---
`member/2` — list membership

`dmember/2` — list membership


## FORMS

member(Element, List)

dmember(Element, List)


## DESCRIPTION

member/2 succeeds when Element can be unified with one of the elements in the list, List. dmember/2 is the determinate version of member/2.


## EXAMPLES

```
?- member(a,[a,b,c]).
yes.
```

```
?- member(X,[1,2,3]).
X=1;
X=2;
X=3;
no.
```


## NOTES

member/2 and dmember/2 are defined by the following clauses :


member(Item, [Item | _ ]) .

member(Item, [_ | Rest ]) :- member(Item, Rest) .


dmember(Item, [Item | _ ]) :- !.

dmember(Item, [_ | Rest ]) :- dmember(Item, Rest) .

