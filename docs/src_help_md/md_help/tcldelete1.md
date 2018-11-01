---
title: 'tcl_delete/1'
predicates:
 - 'tcl_delete/1' : delete a Tcl interpreter
 - 'tcl_delete_all/0' : delete all Tcl interpreters
---
`tcl_delete/1` — delete a Tcl interpreter

`tcl_delete_all/0` — delete all Tcl interpreters


## FORMS

```
tcl_delete(+Interpreter)

tcl_delete_all
```

## DESCRIPTION

`tcl_delete(Interpreter)` deletes the interpreter name `Interpreter`. `tcl_delete_all` deletes all
Tcl interpreters created by `tcl_new/1`.


