---
title: 'tcl_new/1'
package: ALSDev
group: TclTk Interface
module: alsdev
predicates:
- {sig: 'tcl_new/1', desc: 'create a Tcl interpreter'}
- {sig: 'tk_new/1', desc: 'create a Tcl interpreter initialized for Tk'}
---

## FORMS

```
tcl_new(X)

tk_new(X)
```

## DESCRIPTION

`tcl_new/1` creates a new
Tcl interpreter. If the Interpreter argument is an uninstantiated (Prolog) variable, then a unique name is generated for the interpreter. If Interpreter is a atom, the new Tcl interpreter is given that name.

`tk_new/1` functions in the same manner as `tcl_new/1`, except that the newly created Tcl interpreter is initialized with the Tk package.


## EXAMPLES

```
?- tcl_new(i).
%% Succeeds, creating a Tcl interpreter named 'i'.

?- tcl_new(X).
%% Succeeds, unifying X with the atom 'interp1'.
```

## ERRORS

Interpreter is not an atom or variable.

-- -- -- -- > `type_error(atom_or_variable)`

The atom Interpreter has already been use as the name of a Tcl interpreter.

-- -- -- -- > `permission_error(create, tcl_interpreter, Interpreter)`

Tcl is unable to create the interpreter.

-- -- -- -- > `tcl_error(message)`

