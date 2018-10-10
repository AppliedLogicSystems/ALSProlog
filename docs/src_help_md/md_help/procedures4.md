---
title: 'procedures/4'
predicates:
 - 'procedures/4' : retrieves all Prolog-defined procedures
 - 'all_procedures/4' : retrieves all Prolog- or C-defined procedures
 - 'all_ntbl_entries/4' : retrieves all name table entries
---
`procedures/4` — retrieves all Prolog-defined procedures

`all_procedures/4` — retrieves all Prolog- or C-defined procedures

`all_ntbl_entries/4` — retrieves all name table entries


## FORMS

procedures(Module, Pred, Arity, DBRef)

all_procedures(Module, Pred, Arity, DBRef)

all_ntbl_entries(Module, Pred, Arity, DBRef)


## DESCRIPTION

For all three of these 4-argument predicates, the system name table is searched for an entry corresponding to the triple(Module, Pred, Arity) . If such an entry is found, the name table entry is accessed and DBRef is unified with the database reference of the procedure ' s first clause.

procedures/4 only considers procedures defined in Prolog; all_procedures/4 considers just procedures defined in either Prolog or C; all_ntbl_entries/4 considers all name table entries. If the triple(Module, Pred, Arity) is not completely specified, all matching name table entries of the appropriate sort are successively returned.


## EXAMPLES

? - all_procedures(builtins, P, A, DB) .

P = nonvar

A = 1

DB = 0;


P = edit2

A = 1

DB = ' $dbref '(404, 21, 24, 0) ;


P = see

A = 1

DB = 0;


P = gc

A = 0

DB = 0


yes.



