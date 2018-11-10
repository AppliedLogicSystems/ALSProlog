---
title: 'compiletime/0'
predicates:
- {sig: 'compiletime/0', desc: 'Runs goals only at compile time'}
---

## FORMS
```
:- compiletime, Goal1, Goal2 ...
```
## DESCRIPTION

`compiletime/0` is intended for use in executing commands within files at compile-time, as shown above in FORMS.
It will prevent any side-effects caused by running
```
Goal1, Goal2, ...
```
from occurring when the
.obp version of the file is loaded. `compiletime/0` always succeeds.

