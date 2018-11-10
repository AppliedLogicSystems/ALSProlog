---
title: '$procinfo/5'
predicates:
- {sig: '$procinfo/5', desc: 'retrieves information about the given procedure'}
- {sig: '$nextproc/3', desc: 'retrieves the next procedure in the name table'}
- {sig: '$exported_proc/3', desc: 'checks whether the given procedure is exported'}
- {sig: '$resolve_module/4', desc: 'finds the module which exports the given procedure'}
---

## FORMS
```
'$procinfo'(N,M,P,A,DBRef,ProcType)

'$nextproc'(N,F,NN)

'$exported_proc'(Module,Pred,Arity)

'$resolve_module'(Mod, Pred, Arity, ResMod)


```
## DESCRIPTION

 `'$procinfo'(N,M,P,A,DBRef,ProcType)`
If `N` is instantiated to a valid name_table index, the module, procedure name, arity, and first clause are obtained and unified appropriately; the address of the first clause is used to create the `DBRef`. 

Otherwise, if `N` is not instantiated, `M`, `P`, and `A` must be instantiated.  The name_table index and first clause are obtained from this information and (if valid) are unified.

The integer values coding the procedure types (`ProcType`) are:

|Procedure Type|Code|
|----------|---------------|
|Prolog-defined |   0 |
|Builtin        |   1 |
|Imported       |   2 |
|Undefined      |   3 |
|LibBreak       |   4 | 
|Unknown |   -1|

LibBreak entries in the name_table are used to auto load library procedures.

## EXAMPLES
```
?- '$procinfo'(N, builtins, nl, 0, DBR, PT).

N=13924 
DBR=0 
PT=3 

yes.

?- N=13924, '$procinfo'(N, M, P, A, DBR, PT).

N=13924 
M=builtins 
P=nl 
A=0 
DBR=0 
PT=3 

yes.
```
