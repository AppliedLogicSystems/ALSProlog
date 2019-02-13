---
title: 'curmod/1'
predicates:
- {sig: 'curmod/1', desc: 'get the current module'}
- {sig: 'modules/2', desc: 'get the use list of a module'}
---

## FORMS
```
curmod(Module)

modules(Module, Uselist)
```

## DESCRIPTION

`curmod/1` instantiates Module to the current module.

`modules(Module, Uselist)` instantiates `Uselist` to the list of modules declared to be used by `Module`, provided Module is a valid module, and fails otherwise. Note that the list of modules explicitly used by `Module` is extended by the list of implicitly used modules, which include:  `xconsult,debugger,sio,builtins,user`.

## EXAMPLES
```
?- curmod(Module).

Module = user

yes.

?- consult(cm), listing.

% foobar:p/1--exported
p(_A) :- curmod(_B), write(m = _B), nl, q(_A).

% foobar:q/1
q(a).

yes.

? - modules(foobar, X) .

X=[m2,m1,xconsult,debugger,sio,builtins,user] 

yes.

?- p(Y).
m = foobar

Y=a 

yes.
```
