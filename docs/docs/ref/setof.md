---
title: 'setof/3'
iso: setof
predicates:
- {sig: 'setof/3', desc: 'all unique solutions for a goal, sorted'}
- {sig: 'bagof/3', desc: 'all solutions for a goal, not sorted'}
- {sig: 'findall/3', desc: 'all solutions for a goal, not sorted'}
- {sig: 'b_findall/4', desc: 'bound list of solutions for a goal, not sorted'}
---

## FORMS

```
setof(Template, Goal, Collection)

bagof(Template, Goal, Collection)

findall(Template, Goal, Collection)

b_findall(Template, Goal, Collection, Bound)
```

## DESCRIPTION

These predicates collect in the list `Collection`, the set of all instances of `Template` such that the goal, `Goal`, is provable. `Template` is a term that usually shares variables with `Goal`.

`setof/3` produces a `Collection` which is sorted according to the standard order with all duplicate elements removed. Both `bagof/3` and `findall/3` produce `Collections` that are not sorted.

If there are no solutions to `Goal`, then `setof/3` and `bagof/3` will fail, whereas, `findall/3` unifies `Collection` with `[]`.

Variables that occur in `Goal` and not within `Template` are known as __*free, variables*__. `setof/3` and `bagof/3` will generate alternative bindings for free variables upon backtracking.

Within a call to `setof/3` or `bagof/3`, free variables can be existentially quantified in `Goal` by using the notation `Variable^Query`. This means that there exists a `Variable` such that `Query` is true.

The collection to be enumerated should be finite, and should be enumerable by Prolog in finite time. It is possible for the provable instances of `Template` to contain variables, but in this case `Collection` will only provide an imperfect representation of what is actually an infinite collection.

`setof/3` calls upon [`sort/2`](sort.html) to eliminate duplicate solutions from `Collection`, which seriously impacts its efficiency. In addition, even though `bagof/3` leaves duplicate solutions, it still calls [`keysort/2`](sort.html).

`findall/3` neither removes duplicates nor generates alternative bindings for free variables; it assumes that all variables occurring within `Goal` are existentially quantified. As a result, `findall/3` is much more efficient than either `setof/3` or `bagof/3`.

When `Bound` is a positive integer, `b_findall/4` operates similarly to `findall/3`, except that it returns at most `Bound` number of solutions on the list `Collection`. It fails if `Bound` is anything other than a positive integer.


## EXAMPLES

```
?- listing.
% user:likes/2
likes(kev,running).
likes(kev,lifting).
likes(keith,running).
likes(keith,lifting).
likes(ken,swimming).
likes(sally,swimming).
likes(andy,bicycling).
likes(chris,lifting).
likes(chris,running).
yes.
```
```
?- setof(Person, likes(Person, Sport), SetOfPeople).
Person = _1
Sport = bicycling
SetOfPeople = [andy];
Person = _1
Sport = lifting
SetOfPeople = [chris,keith,kev];
Person = _1
Sport = running
SetOfPeople = [chris,keith,kev];
Person = _1
Sport = swimming
SetOfPeople = [sally,ken];
no.
```
```
?- setof((Sport,People),
setof(Person, likes(Person, Sport), People),
Set).
Sport = _1
People = _2
Person = _4
Set = [(bicycling,[andy]),(lifting,[chris,keith,kev]),
(running,[chris,keith,kev]),(swimming,[sally,ken])]
yes.
```
```
?- setof(Person, Sport^(likes(Person, Sport)), SetOfPeople).
Person = _1
Sport = _2
SetOfPeople = [andy,chris,sally,keith,ken,kev]
yes.
```

## SEE ALSO

- {% include book.md id="bowen91"    sec="7.5" %}
- {% include book.md id="clocksin81" sec="7.8" %}
- {% include book.md id="bratko86"   sec="7.6" %}
- {% include book.md id="sterling86" sec="17.1" %}
