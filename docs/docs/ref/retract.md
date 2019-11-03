---
title: 'retract/[1,2]'
group: Prolog Database
iso: retract
predicates:
- {sig: 'retract', args: {
    1: 'removes a clause from the database',
    2: 'removes a clause specified by a database reference'
  }}
- {sig: 'erase/1', desc: 'removes a clause from the database'}
---

## FORMS
```
retract(Clause)

retract(Clause, DBRef)

erase(DBRef)
```
## DESCRIPTION

When `Clause` is bound to an atom or a structured term, `retract/1` searches the current module for a clause that will unify with `Clause`. When a matching clause is found in the database, `Clause` is unified with the structure corresponding to the clause. The clause is then removed from the database.

`retract/2` additionally unifies `DBRef` with the database reference of the clause.

`erase/1` removes the clause associated with `DBRef` from the database. Note that `erase(DBRef)` should never be called following `retract(Clause, DBRef)` since at that point `DBRef` is no longer a valid database reference.

`retract/1` and `retract/2` will repeatedly generate and remove clauses upon backtracking. `:/2` can be used to specify which module should be searched.


## EXAMPLES

The following example shows how `retract/1` and `retract/2` can be used to get rid of all the comic book heroes that live in our modules. First we create all the heroes by consulting `user`, typing in facts from the console.  Then we get rid of `hero(spiderman)` by using a simple call to `retract/1`:

```
?- [user].
hero(spiderman).
hero(superman).
hero(batman).
module women.
hero(superwoman).
endmod.
^D
yes.

?- retract(hero(spiderman)).

yes.
```
Continuing the example, we show what heroes are left by using the [`listing/1`](listing.html) procedure. After that, we remove `hero(superman)` with a `retract/2` call. The old database reference to the man of steel is instantiated to `Ref`.

```
?- listing(hero/1).
%user:hero/1
hero(superman).
hero(batman).
%women:hero/1
hero(superwoman).
yes.

?- retract(hero(superman),Ref).

Ref='$dbref'(5208,15,2384,1)

yes.
```
Next, we use [`clause/3`](clause.html) to find the database reference of hero(batman). With this, we use the database reference in a `retract/2` call to remove `hero(batman)` from the database. Note that when the call to  `retract/2` is made,
the `Clause` argument is uninstantiated. After the call to `retract/2` has succeeded, `Clause` is instantiated to 
the clause that was removed.
```
?- clause(hero(batman),Body,Ref).
Body=true
Ref='$dbref'(5052,15,2384,2)

yes.

?- retract(Clause,'$dbref'(5052,15,2384,2)).
Clause=hero(batman)

yes.
```
Now, we list the heroes left in the database. Only `hero(superwoman)` is left, but she's in a different module. However, using the Mod:Goal construct, we can remove her too:
```
?- listing(hero/1).
%women:hero/1
hero(superwoman).

yes.

?- women:retract(hero(X)).
X=superwoman
yes.

?- listing(hero/1).
yes.
```
As the last call to [`listing/1`](listing.html) shows, there are no more heroes left in the database. (Who knows what evil may be lurking in the garbage collector though!)

## SEE ALSO

- [`abolish/2`](abolish.html)
- [`clause/2`](clause.html)

- {% include book.md id="bowen91"    sec="7.3" %}
- {% include book.md id="clocksin81" sec="6.4" %}
- {% include book.md id="bratko86"   sec="7.4" %}
- {% include book.md id="sterling86" sec="12.2" %}

