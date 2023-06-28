---
title: 'abolish/2'
iso: abolish
predicates:
- {sig: 'abolish', args: {
    1: 'remove a procedure from the database',
    2: 'remove a procedure from the database',
    3: 'remove a procedure from the database'
  }}
---

## FORMS

abolish(Name/Arity)

abolish(Name, Arity)

abolish(Module, Name, Arity)

## DESCRIPTION

For the first two forms, all the clauses for the specified procedure `Name/Arity` in the current module are removed from the database. The module from which to abolish clauses can be specified by prefixing one of the first two forms with a `M:`, where `M` is the module name,  or by using the third form (recall that `user` is the name of the default module).  The first two forms are defined in terms of the third using [`module_closures`](module_closure.html).  The first form is synonymous with the second.

If the module `M` does not exist, then both M:abolish(Name/Arity) and M:abolish(Name, Arity) throw a procedure existence error.  But abolish(M, Name, Arity) succeeds, even though it obviously could not have removed a procedure. (See the last three examples below.)

If module `M` does exist, but predicate `Name/Arity` does not exist in `M`, then all three calls 
```
M:abolish(Name/Arity),  M:abolish(Name, Arity), abolish(M, Name, Arity) 
```
succeed, even though there are no clauses in module `M` to be removed.

## EXAMPLES
```
?- listing.

% goods:value/2
value(gold,high).
value(mink_coat,high).
value(t_shirt,low).

% goods:electric/1
electric(heater).
electric(iron).

% user:solid/1
solid(door).
solid(floor).

% user:fluid/1
fluid(sand).

yes.

?- user:abolish(solid/1).

yes.
?- listing.

% goods:value/2
value(gold,high).
value(mink_coat,high).
value(t_shirt,low).

% goods:electric/1
electric(heater).
electric(iron).

% user:fluid/1
fluid(sand).

yes.

?- abolish(goods, value, 2).

yes.
?- listing.

% goods:electric/1
electric(heater).
electric(iron).

% user:fluid/1
fluid(sand).

yes.

?- goods:abolish(victim,7).

yes.
?- abolish(goods,victim,7).

yes.
?- goods:abolish(victim/7).

yes.

?- othermodule:abolish(victim,7).
Error: Operation attempted on object of type procedure which does not exist.
Operation attempted on: othermodule:abolish(victim,7).
- Goal:          othermodule:abolish(victim,7)
- Throw pattern: error(
                     existence_error(procedure,
                         othermodule:abolish(victim,7)),
                     [othermodule:abolish(victim,7)])

?- othermodule:abolish(victim/7).
Error: Operation attempted on object of type procedure which does not exist.
Operation attempted on: othermodule:abolish(victim/7).
- Goal:          othermodule:abolish(victim/7)
- Throw pattern: error(existence_error(procedure,othermodule:abolish(*)),
                     [othermodule:abolish(*)])

?- abolish(othermodule,victim,7).
yes.
```
## ERRORS

If `Name` is not an atom or `Arity` is not an integer, `abolish/2` fails.


## SEE ALSO

- [`:/2`](colon.html)
