---
title: 'make_hash_table/1'
group: Terms
predicates:
- {sig: 'make_hash_table/1', desc: 'create a hash table and access predicates'}
---

## FORMS
```
make_hash_table(Name)
```
## DESCRIPTION

**`make_hash_table/1`** will create a hash table and a set of access methods with the atom 	`Name` as the suffix. Suppose for the sake of the following discussion that `Name` is bound to the atom `'_table'`. Then the access predicates created will be as follows:

**`reset_table`** -- throw away the old hash table associated with the `'_table'` hash table and create a brand new one.

**`set_table(Key, Value)`** -- associate 'Key' with 'Value' in the hash table. `Key` should be bound to a ground term. Any former associations that `Key` had in the hash table are replaced.

**`get_table(Key, Value)`** -- get the value associated with the ground term bound to `Key` and unify it with `Value`.

**`del_table(Key, Value)`** -- delete the `Key/Value` association from the hash table. `Key` must be bound to a ground term. `Value. will be unified against the associated value in the table. If the unification is not successful, the table will not be modified.

**`pget_table(KeyPattern, ValPattern)`** -- The &quot;`p`&quot; in `pget` and `pdel`, below, stands for pattern. `pget_table` permits `KeyPattern` and `ValPattern` to have any desired instantiation. It will backtrack through the table and locate associations matching the &quot;`pattern`&quot; as specified by `KeyPattern` and `ValPattern`.

**`pdel_table(KeyPattern, ValPattern)`** -- This functions the same as `pget_table` except that the association is deleted from the table once it is retrieved.

## EXAMPLES
```
?- make_hash_table('_assoc').

yes.

?- set_assoc(a,f(1)).

yes.

?- set_assoc(b,f(2)).

yes.

?- set_assoc(c,f(3)).

yes.

?- get_assoc(X,Y).

no.

?- get_assoc(c,Y).

Y=f(3)

yes.

?- pget_assoc(X,Y).

X=c
Y=f(3);

X=b
Y=f(2);

X=a
Y=f(1);

no.

?- del_assoc(b,Y).

Y=f(2)

yes.

?- pdel_assoc(X,f(3)).

X=c

yes.

?- pget_assoc(X,Y).

X=a
Y=f(1);

no.

?- reset_assoc.

yes.

?- pget_assoc(X,Y).

no.
```
## NOTES

Unlike `assert` and `retract`, the methods created by `make_hash_table/1` do not access the database. The associations between keys and values is stored on the heap. Thus elements of either keys or values may be modified in a destructive fashion. This will probably not have desirable consequences if a key is modified.

These predicates have an advantage over `assert` and `retract` in that no copies are made. In fact structure may be shared between hash table entries.

See the discussion in [`make_gv/1`](make_gv.html) concerning global variable modification and backtracking.

## SEE ALSO

- [`make_gv/1`](make_gv.html)
