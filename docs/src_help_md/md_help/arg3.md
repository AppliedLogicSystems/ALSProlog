---
title: 'arg/3'
predicates:
 - 'arg/3' : access the arguments of a structured term
---
`arg/3` — access the arguments of a structured term


## FORMS

arg(Nth, Structure, Argument)


## DESCRIPTION

Argument will be unified with the Nth argument of Structure. Nth must be a positive integer. Structure should be a compound term whose arity is greater than or equal to Nth. When these conditions hold, Argument will be unified with the Nth argument of Structure.


## EXAMPLES

```
?- arg(2,stooges(larry,moe,curly),X).
X=moe
yes.
```

```
?- arg(2,[a,b,c],X).
X=[b,c]
yes.
```


## ERRORS

If Nth is not an integer greater than 0 and less than or equal to the arity of Structure, arg/3 will fail. Structure must be instantiated to a structured term.


## SEE ALSO

- `functor/3`  
`=../2`

- [Bowen 91, 7.6]
- [Sterling 86, 9.2]
- [Bratko 86, 7.2]
- [Clocksin 81, 6.5]
