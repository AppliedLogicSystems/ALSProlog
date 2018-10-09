---
title: 'compiletime/0'
predicates:
 - 'compiletime/0' : Runs the goal only at compile time
---
`compiletime/0` `--` Runs the goal only at compile time


## FORMS

:- compiletime, Goal1, Goal2 ...


## DESCRIPTION

compiletime/0 is intended for use in compile-time commands within files. It will prevent any side-effects caused by


Goal1, Goal2, ...

from occurring when the
.obp

version of the file is loaded. compiletime/0 always succeeds.

