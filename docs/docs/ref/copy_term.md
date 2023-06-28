---
title: 'copy_term/2'
group: Terms
iso: copyterm
predicates:
- {sig: 'copy_term/2', desc: 'make copy of a term'}
---

## FORMS
```
copy_term(TermIn,TermOut)
```
## DESCRIPTION

`copy_term/2` will copy the term `TermIn` and unify this copy with `TermOut`. Unbound variables in `TermIn` and `TermOut` will not be shared between the two terms; i.e., uninstantiated variables occurring in `TermIn` will be replaced in `TermOut` by new uninstantiated variables not occurring in `TermIn`.

## EXAMPLES
```
?- copy_term(f(X,g(Y,X)),Z).
X=X
Y=Y
Z=f(_A,g(_B,_A))
yes.

?- Y=f(9,rr), copy_term(Y, X), X==Y.

Y=f(9,rr) 
X=f(9,rr) 

yes.

?-  Y=f(9,r, H), copy_term(Y, X), write(x=X),nl, X==Y.
x = f(9,r,_6203)

no.
```


## NOTES

`copy_term/2` is useful in situations involving destructive assignment. It is useful not only for the obvious situation of making a copy which is then destructively modified, but also for avoiding certain problems regarding structures becoming uninstantiated upon backtracking, as in `findall`, `bagof`, etc.  It is also useful when using access predicates created with either [`make_gv/1`](make_gv.html) or [`make_hashtable/1`](make_hash_table.html). See [`make_gv/1`](make_gv.html) for further discussion.


## SEE ALSO

- [`make_gv/1`](make_gv.html)
- [`make_hash_table/1`](make_hash_table.html)
- [`mangle/3`](mangle.html)
