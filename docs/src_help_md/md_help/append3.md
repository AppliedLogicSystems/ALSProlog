---
title: 'append/3'
group: Terms
predicates:
- {sig: 'append/3', desc: 'append two lists'}
- {sig: 'dappend/3', desc: 'append two lists'}
---

## FORMS

```
append(List1, List2, List3)

dappend(List1, List2, List3)
```


## DESCRIPTION

`List3` is the result of appending `List2` to the end of `List1`. `dappend` is the determinate version of `append`.


## EXAMPLES

```
?- append([a,b],[c,d],E).
E=[a,b,c,d]
yes.
```

```
?- append([a,b],[C,D],[a,b,c,d]).
C=c
D=d
yes.
```


## NOTES

append and dappend are defined by :

```
append([ ], L, L) .

append([H | T ], L, [H | TL ]) :- append(T, L, TL) .


dappend([ ], L, L) :- !.

dappend([H | T ], L, [H | TL ]) :- dappend(T, L, TL) .
```
