---
title: 'tcl_delete/1'
package: ALSDev
group: TclTk Interface
module: alsdev
predicates:
- {sig: 'tcl_delete/1', desc: 'delete a Tcl interpreter'}
- {sig: 'tcl_delete_all/0', desc: 'delete all Tcl interpreters'}
---

## FORMS

```
tcl_delete(+Interpreter)

tcl_delete_all
```

## DESCRIPTION

`tcl_delete(Interpreter)` deletes the interpreter name `Interpreter`. `tcl_delete_all` deletes all
Tcl interpreters created by [`tcl_new/1`](tcl_new.html).


