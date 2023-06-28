---
title: 'listing/0'
group: Prolog Database
predicates:
- {sig: 'listing', args: {
    0: 'Prints all clauses',
    1: 'Prints clauses matching the specified template'
   }}
---

## FORMS
```
listing

listing(Pred)

listing(Pred/Arity)

listing(Mod:Pred/Arity)
```
## DESCRIPTION

`listing/0` lists on the the current output stream all the clauses in the database except those in the `builtins module` and several other system modules.

If `Pred` is the name of a predicate, `listing(Pred)` causes all the clauses for `Pred` of any arity and residing in any module other than builtins or other system modules to be listed to the current output stream. The argument `Pred` may also be a predicate specification of the form `Name/Arity`, in which case only the clauses for the specified predicate of arity `Arity` are listed. If `P` is the name of a predicate of arity `A` which is defined in module `M`, then
```
?- listing(M:P/A).
```
will cause only the clauses of the definition of P/A in M to be listed to the current output stream. A form of wildcards can be used if some of the arguments are left as uninstantiated variables. The following example lists all the eggs clauses in module dairy regardless of what their arity is :
```
?- listing(dairy:eggs/_).
```
To list all of the clauses in the `module dairy`, you could submit the goal :
```
?- listing(dairy:_).
```
## SEE ALSO

- {% include book.md id="clocksin81" sec="6.4" %}
