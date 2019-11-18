---
title: 'tcl_call/3'
package: ALSDev
group: TclTk Interface
module: alsdev
predicates:
- {sig: 'tcl_call/3', desc: 'execute Tcl script'}
- {sig: 'tcl_eval/3', desc: 'evaluate Tcl script'}
---

## FORMS

```
tcl_call(+ Interpreter, + Script, ? Result)
tcl_eval(+ Interpreter, + ArgList, ? Result)
```

## DESCRIPTION

`tcl_call/3` and `tcl_eval/3` both execute a
script using the Tcl interpreter and return the Tcl result in `Result`. `Tcl_call` passes the `Script` argument as a single argument toTcl's `eval` command. `Tcl_eval` passes the elements of `ArgList` as arguments to Tcl's `eval` command, which concatenates the arguments before evaluating them.

Tcl_call's script can take the following forms:

List - The list is converted to a Tcl list and evaluated by the Tcl interpreter. The list may contain, atoms, numbers and lists.

Atom - The atom is converted to a string and evaluated by the Tcl interpreter.

Tcl_eval's `ArgList` may contain atoms, numbers or lists.


## EXAMPLES

```
?- tcl_call(i, [puts, abc], R).
R = ''
%% Prints 'abc' to standard output, and binds R to ''.

?- tcl_call(i, [set, x, 3.4], R).
R = 3.4
%% Sets the Tcl variable x to 3.4 and binds R to 3.4.

?- tcl_call(i, 'set x', R).
R = 3.4
%% Binds R to 3.4.

?- tcl_eval(i, ['if [file exists ', Name, '] puts file-found'], R).
```

## ERRORS

Interpreter is not an atom.

Script is not an atom or list.

Script generates a Tcl error.

