---
title: 'copy_term/1'
predicates:
 - 'copy_term/1' : make copy of a term
---
`copy_term/1` `--` make copy of a term


## FORMS

copy_term(Term)


## DESCRIPTION

copy_term/1 will copy the term Term and unify this copy with Copy. Unbound variables in Term and Copy will not be shared between the two terms.


## EXAMPLES

```
?- copy_term(f(X,g(Y,X)),Z).
X=X
Y=Y
Z=f(_A,g(_B,_A))
yes.
```


## NOTES

copy_term/1 is useful in situations involving destructive assignment. It is useful not only for the obvious situation of making a copy which is then destructively modified, but also for avoiding certain problems regarding structures becoming uninstantiated upon backtracking when using access predicates created with either make_gv/1 or make_hashtable/1. See make_gv/1 for further discussion.


## SEE ALSO

- `make_gv/1`  
`make_hash_table/1`  
`mangle/3.
