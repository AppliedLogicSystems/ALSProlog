---
title: '$procinfo/5'
module: builtins
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
If `N` is instantiated to a valid name_table index, the entry in that name_table slot is retrieved and decomposed, and the elements are unified with `M,P,A,DBRef,ProcType` as follows:  
`M:` the module of the entry  
`P:` the name of the predicate  
`A:` the arity of the predicate  
`DBRef:` a representation of the database index of the first clause for the predicate (or 0, if none)  
`ProcType:` a small integer representing the type of the predicate, according to the table below.

Otherwise, if `N` is not instantiated, at least `M`, `P`, and `A` must be instantiated to valid values.  The name_table index(`N`) and the database reference (`DBRef`) are obtained from this information and these values are unified with `N, DBRef`.

The integer values coding the procedure types (`ProcType`) are:

|Procedure Type|Code|
|----------|---------------|
|Prolog-defined |   0 |
|Builtin (C or other foreign language)        |   1 |
|Imported (from another module)       |   2 |
|Undefined (entry exists, but no clauses entered)      |   3 |
|LibBreak (like Undefined, but dynamically loads a library predicate)     |   4 | 
|Unknown |   -1|

The principal use of Undefined is for indicating module_closure entries in all modules other than the actual defining module of the predicate.  A LibBreak entry contains information on how to dynamically load a library predicate M:P/A. After loading, that nametable entry will look just like any other Prolog-defined nametable entry.

`'$nextproc'(N,F,NN)`  
If `N` is instantiated to a valid name_table index, and if F is instantiated to a valid procedure type code as given in the table above, then `'$nextproc'(N,F,NN)` will succeed if and only if there is a nametable entry index KK > N containing a procedure entry of type F, and NN is the first such index.

`'$exported_proc'(Module,Pred,Arity)`  
succeeds if and only if `Module` is instantiated to an existing module, `Pred/Arity` is a predicate defined in `Module`, and `Pred/Arity` is exported from `Module`.

`'$resolve_module'(Mod, Pred, Arity, ResMod)`
If Pred/Arity is called(used) in module Mod, but is not defined in Mod, but instead is defined in some other module, then `'$resolve_module'(Mod, Pred, Arity, ResMod)` succeeds if and only if ResMod is a module containing a definition of `Pred/Arity`, `Pred/Arity` is exported from `ResMod`, and `ResMod` is on the *uselist* of `Mod`.  Note that if `Pred/Arity` is defined in `Mod`, then `'$resolve_module'(Mod, Pred, Arity, ResMod)` will succeed with `ResMod = Mod`.

## EXAMPLES
```
?- '$procinfo'(N, builtins, nl, 0, DBR, PT).

N=13956 
DBR=0 
PT=3 

yes.

?- N=13956, '$procinfo'(N, M, P, A, DBR, PT).

N=13956 
M=builtins 
P=nl 
A=0 
DBR=0 
PT=3 

yes.
```
Load this code:  
```
p :- N=3, pt(N).

pt(N) :- ns(N,NN), pt(NN).

ns(N,NN) :-
        '$nextproc'(N,1,NN),
        '$procinfo'(N,M,P,A,DBRef,ProcType),
        printf('N=%t NN=%t M=%t P=%t A=%t PT=%t\n', [N,NN,M,P,A,ProcType]).
```
```
?- p.  
N=3 NN=4 M=builtins P=dbg_spy A=3 PT=1  
N=4 NN=27 M=builtins P=set_debugging_state A=1 PT=0  
N=27 NN=28 M=sio P=rt_err A=7 PT=0  
N=28 NN=35 M=sio P=stream_error A=1 PT=0  
N=35 NN=59 M=builtins P=dbg_nospy A=3 PT=1  
N=59 NN=60 M=sio P=output_stream_or_alias_ok A=2 PT=0  
N=60 NN=61 M=sio P=write_term A=2 PT=0  
N=61 NN=67 M=sio P=write_term A=3 PT=0  
N=67 NN=68 M=builtins P=$c_examine A=2 PT=1  
N=68 NN=69 M=builtins P=?semi_1559612662_99 A=3 PT=0  
N=69 NN=91 M=builtins P=make_prompts A=4 PT=0  
N=91 NN=92 M=sio P=rt_err_print A=4 PT=0  
N=92 NN=99 M=sio P=assign_alias A=2 PT=0  
N=99 NN=100 M=builtins P=$c_set A=2 PT=1  
............. etc .............  
N=16347 NN=16355 M=sio P=stream_or_alias_error A=2 PT=0  
N=16355 NN=16356 M=builtins P=dbg_spyoff A=0 PT=1  
N=16356 NN=16357 M=builtins P=send A=2 PT=1  
N=16357 NN=16358 M=builtins P=send A=3 PT=0  
N=16358 NN=16379 M=builtins P=get_debugging_state A=1 PT=0  
N=16379 NN=16380 M=sio P=ide_rt_err A=7 PT=0  
  
no.  
```
```
Load this code:  
```
```
module m1.  
export p/1.  
p(X) :- X = a.  
endmod.  

module m2.  
use m1.  
export q/1.  
q(X) :- p(X).  
endmod.  
```
```
?- '$exported_proc'(m1,p,1).

yes.
?- '$exported_proc'(m1,h,1).

no.

?- '$resolve_module'(m2, p, 1, ResMod).

ResMod=m1 

yes.
```

