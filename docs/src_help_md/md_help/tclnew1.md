---
title: 'tcl_new/1'
predicates:
 - 'tcl_new/1' : create a Tcl interpreter
 - 'tk_new/1' : create a Tcl interpreter initialized for Tk
---
`tcl_new/1` — create a Tcl interpreter

`tk_new/1` — create a Tcl interpreter initialized for Tk


## FORMS

tcl_new(X)

tcl_new(X)


## DESCRIPTION

tcl_new/1 creates a new
Tcl interpreter. If the Interpreter argument is an uninstantiated(Prolog) variable, then a unique name is generated for the interpreter. If Interpreter is a atom, the new Tcl interpreter is given that name.

tk_new/1 functions in the same manner as tcl_new/1, except that the newly- created Tcl interpreter is initialized with theTk package.


## EXAMPLES

```
?- tcl_new(i).
Succeeds,creatingaTclinterpreternamedi.
?- tcl_new(X).
Succeeds,unifyingXwiththeatom'interp1'.
```

## ERRORS

Interpreter is not an atom or variable.

-- -- -- -- &gt; type_error(atom_or_variable) .

The atom Interpreter has already been use as the name of a Tcl interpreter.

-- -- -- -- &gt; permission_error(create, tcl_interpreter, Interpreter)

Tcl is unable to create the interpreter.

-- -- -- --
tcl_error(message)

