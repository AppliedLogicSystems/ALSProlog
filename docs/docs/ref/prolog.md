---
title: 'prolog'
package: ALSDev
group: TclTk Interface
module: alsdev
predicates:
- {sig: 'prolog', desc: 'call a prolog term from Tcl'}
---

## FORMS
```
prolog option ? arg arg... ?
```
## DESCRIPTION

The `prolog` command provides methods for executing a prolog query in ALS Prolog from within an executing Tcl script. `Option` indicates how the query is expressed. The valid values for `Option` are :
```
**prolog call** module predicate ? -type arg ... ?
```
This version directly calls a predicate in a module with type-converted arguments. The command returns 1 if the query succeeds, or 0 if it fails. The arguments can take the following forms :

`-number arg` Passes arg as an integer or floating point number.

`-atom arg` Passes arg as an atom.

`-list arg` Passes arg as a list.

`-var varName` Passes an unbound Prolog variable. When the Prolog variable is bound, the Tcl variable with the name `varName` is set to the binding.
```
**prolog read_call** termString ? varName ... ?
```
The string `termString` is first read as a prolog term and then called. The command returns 1 if the query succeeds, or 0 if it fails. The optional variables named by the `varName` arguments are set when a Prolog variable in the query string is bound. The prolog variables are matched to `varNames` in left-to-right depth first order.

## EXAMPLES
```
prolog call builtins append -atom a -atom b -var x
```
Returns 1, and the Tcl variable `x` is set to `{}(a,b)`.
```
prolog read_call "append(a, b, X)" x
```
Returns 1, and the Tcl variable `x` is set to `{}(a,b)`.

