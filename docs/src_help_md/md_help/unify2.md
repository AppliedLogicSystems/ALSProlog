---
title: '=/2'
predicates:
 - '=/2' : unify two terms
 - '\=/2' : test if two items are non-unifiable
---
`=/2` `--` unify two terms

`\=/2` `--` test if two items are non-unifiable


## FORMS

Arg1 = Arg2

Arg1 \ = Arg2


## DESCRIPTION

The procedure = / 2 calls the Prolog unifier to unify Arg1 and Arg2, binding variables if necessary. The occurs check is not performed. = / 2 is defined as if by the clause :

Term = Term.

If the two terms cannot be unified, = fails. The procedure \ = succeeds if the two terms cannot be unified. This is different than the = \ = and the \ = = procedures.


## EXAMPLES

The following examples illustrate the use of = .

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

- `==/2`  
`\==/2`  
`eq`  
`noneq`

- [Bowen 91, 4.6 ]
- [Bratko 86, 2.7 ]
- [Clocksin 81, 6.8 ]
- [Sterling 86, 4.1 ]. 
