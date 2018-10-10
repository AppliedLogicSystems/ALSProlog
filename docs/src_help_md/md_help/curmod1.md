—-
title: 'curmod/1'
predicates:
 - 'curmod/1' : get the current module
 - 'modules/2' : get the use list of a module
—-
`curmod/1` `—` get the current module

`modules/2` `—` get the use list of a module


## FORMS

curmod(Module)

modules(Module, Uselist)


## DESCRIPTION

curmod/1 instantiates Module to the current module.

modules(Module, Uselist) instantiates Uselist to the list of modules declared to be used by Module, provided Module is a valid module, and fails otherwise.


## EXAMPLES

? - curmod(Module) .

Module = user


yes.

? - [user ].

Consulting user ...

module foobar.

use m1.

use m2.

p(a) .

endmod.

user consulted


yes.

? - modules(foobar, X) .

X = [m2, m1, builtins, user ]


yes.

