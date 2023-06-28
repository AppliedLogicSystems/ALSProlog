---
title: 'tcl_coerce_number/3'
package: ALSDev
group: TclTk Interface
module: alsdev
predicates:
- {sig: 'tcl_coerce_number/3', desc: 'convert Tcl entity to Prolog number'}
- {sig: 'tcl_coerce_atom/3', desc: 'convert Tcl entity to Prolog atom'}
- {sig: 'tcl_coerce_list/3', desc: 'convert Tcl entity to Prolog list'}
---

## FORMS

```
tcl_coerce_number(+ Interpreter, + Object, ? Number)

tcl_coerce_atom(+ Interpreter, + Object, ? Atom)

tcl_coerce_list(+ interpreter, + Object, ? List)
```

## DESCRIPTION

These three predicates convert the object `Object` to a specific Prolog type using the Tcl interpreter `Interpreter`. `Object` can be an number, atom or list. If `Object` is already the correct type, then it is simply bound to the output argument. If `Object` cannot be converted, an error is generated.


## EXAMPLES

```
tcl_coerce_number(i, '1.3', N) - Succeeds, binding N to the float 1.3

tcl_coerce_number(i, 1.3, N) - Succeeds, binding N to the float 1.3

tcl_coerce_number(i, 'abc', N) - Generates an error.

tcl_coerce_atom(i, [a, b, c], A) - Succeeds, binding A to 'a b c'

tcl_coerce_atom(i, 1.4, A) - Succeeds, binding A to '1.4'

tcl_coerce_list(i, 'a b c', L) - Succeeds, binding L to [a, b, c]

tcl_coerce_list(i, 1.4, L) - Succeeds, binding L to [1.4]

tcl_coerce_list(i, '', L) - Succeeds, binding L to []
```

## ERRORS

Interpreter is not an atom.

Object is not a number, atom or list.

Object cannot be converted to the type.
