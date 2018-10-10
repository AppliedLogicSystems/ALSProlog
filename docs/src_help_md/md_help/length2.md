---
title: 'length/2'
predicates:
 - 'length/2' : count the number of elements in a list
---
`length/2` — count the number of elements in a list


## FORMS

length(List, Size)


## DESCRIPTION

Size is unified with the number of elements in List.


## EXAMPLES

```
?- length([a,b,c],X).
X=3
yes.
```


## NOTES

length/2 is defined by :


length(List, Length) :- length(List, 0, Length) .

length([ ], Length, Length) :- !.

length([_ | Rest ], Old, Length) :-

New is Old + 1,

length(Rest, New, Length) .

