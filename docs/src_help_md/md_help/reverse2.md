---
title: 'reverse/2'
predicates:
 - 'reverse/2' : list reversal
 - 'dreverse/2' : determinate list reversal
---
`reverse/2` `--` list reversal

`dreverse/2` `--` determinate list reversal


## FORMS

reverse(List1, List2)

dreverse(List1, List2)


## DESCRIPTION

reverse/2 succeeds when List2 can be unified with the result of reversing List1. dreverse/2 is the determinate version of reverse/2.


## EXAMPLES

```
?- reverse([a,b,c],List2).
List2=[c,b,a]
yes.
```


## NOTES

Defined by the following clauses :

reverse(List, Rev) :-

reverse(List, [ ], Rev) .


reverse([ ], Rev, Rev) .

reverse([A | Rest ], SoFar, Rev) :-

reverse(Rest, [A | SoFar ], Rev) .


dreverse(List, Rev) :-

dreverse(List, [ ], Rev) .


dreverse([ ], Rev, Rev) :- !.

dreverse([A | Rest ], SoFar, Rev)

:- dreverse(Rest, [A | SoFar ], Rev) .

