---
title: 'current_op/3'
module: sio
iso: operators
predicates:
- {sig: 'current_op/3', desc: 'retrieve current operator definitions'}
---

## FORMS
```
current_op(Priority, Specifier, Operator)
```
## DESCRIPTION

`current_op/3` is used to retrieve operator definitions. Each parameter to current_op may be used as either an input or output argument. Logically, `current_op(Priority, Specifier, Operator)` is true if and only if `Operator` is an operator with properties defined by `Specifier` and `Priority`.

## EXAMPLES
```
?- current_op(P,S,-).

P=500 
S=yfx ;

P=200 
S=fy ;

no.
?- current_op(500,S,Op).

S=yfx 
Op=+  ;

S=yfx 
Op=-  ;

S=yfx 
Op=/\ 

yes.
```
Above, we hit return after `Op=/\` to skip further answers

## SEE ALSO

- [`op/3`](op.html)
- [`read_term/[2,3]`](read.html)

- [User Guide (Prolog I/O)](../guide/10-Prolog-I-O.html)
