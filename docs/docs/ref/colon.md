---
title: ':/2 (colon)'
predicates:
- {sig: ':/2', desc: 'calls a goal in the specified module'}
---

## FORMS
```
Module : Goal
```
## DESCRIPTION

'Module' should be instantiated to a name of a an existing, loaded module, and `Goal` should be instantiated to a non-variable term. The `Goal` will be executed as if the call to `Goal` appeared in the module named `Module`. Any module-dependent procedures such as [`assert/1`](assert.html) or [`retract/1`](retract.html) will operate on the procedures defined in `Module`. Any cuts appearing in `Goal` will cut back through the head of the clause that contained the call to `:/2`.


## EXAMPLES
```
?- listing.

% behavior:eats_linguini/1
eats_linguini(mike).
eats_linguini(jane).

% behavior:eats_salad/1
eats_salad(phil).

yes.

?- eats_linguini(mike).
Error: Operation attempted on object of type procedure which does not exist.
Operation attempted on: user:eats_linguini(mike).
- Goal:          user:eats_linguini(mike)
- Throw pattern: error(existence_error(procedure,user:eats_linguini(mike)),
                     [user:eats_linguini(mike)])

?- behavior:eats_linguini(mike).

yes.

?- builtins:setof(Person, behavior:eats_linguini(Person), Persons).

Person=Person 
Persons=[jane,mike] 

yes.

?- behavior:setof(Person, eats_linguini(Person), Persons).

Person=Person 
Persons=[jane,mike] 

yes.
```
## ERRORS

If `Module` is unbound or not bound to a valid module name, the call to `:/2` will fail.


## SEE ALSO

- [`call/1`](call.html)

- [User Guide (Modules)](../guide/3-Modules.html)
