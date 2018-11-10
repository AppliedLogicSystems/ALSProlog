---
title: 'procedures/4'
predicates:
- {sig: 'procedures/4', desc: 'retrieves all Prolog-defined procedures'}
- {sig: 'all_procedures/4', desc: 'retrieves all Prolog- or C-defined procedures'}
- {sig: 'all_ntbl_entries/4', desc: 'retrieves all name table entries'}
---

## FORMS
```
procedures(Module, Pred, Arity, DBRef)

all_procedures(Module, Pred, Arity, DBRef)

all_ntbl_entries(Module, Pred, Arity, DBRef)
```
## DESCRIPTION

For all three of these 4-argument predicates, the system name table is searched for an entry corresponding to the triple `(Module, Pred, Arity)`. If such an entry is found, the name table entry is accessed and `DBRef` is unified with the database reference of the procedure's first clause.

`procedures/4` only considers procedures defined in Prolog; `all_procedures/4` considers just procedures defined in either Prolog or C; `all_ntbl_entries/4` considers all name table entries. If the triple `(Module, Pred, Arity)` is not completely specified, all matching name table entries of the appropriate sort are successively returned.

## EXAMPLES
```
?- procedures(M,P,A,DB).

M=builtins 
P=subClassOf 
A=2 
DB='$dbref'(26348,530,4,2139095040.0) ;

M=builtins 
P=succeed_or_fail 
A=2 
DB='$dbref'(12416,529,5,-8388608) ;

M=sio 
P=output_stream_or_alias_ok 
A=2 
DB='$dbref'(49208,516,23,-8388608) 

yes.

?- all_procedures(builtins,P,A,DB).

P=dbg_spy 
A=3 
DB=0 ;

P=subClassOf 
A=2 
DB='$dbref'(26348,114,4,2139095040.0) ;

P=succeed_or_fail 
A=2 
DB='$dbref'(12416,113,5,-8388608) ;

P=dbg_nospy 
A=3 
DB=0 

yes.

?- all_ntbl_entries(debugger, Pred, Arity, DBRef).

Pred=nl 
Arity=1 
DBRef=0 ;

Pred=toggle_mod_show 
Arity=1 
DBRef='$dbref'(42744,295,911,-8388608) ;

Pred=leash 
Arity=1 
DBRef='$dbref'(40096,295,943,-8388608) 

yes.
```
