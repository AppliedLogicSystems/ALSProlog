—-
title: 'clause/[2,3]'
predicates:
 - 'clause/2' : retrieve a clause
 - 'clause/3' : retrieve a clause with a database reference
 - 'instance/2' : retrieve a clause from the database reference
—-
`clause/2` `—` retrieve a clause

`clause/3` `—` retrieve a clause with a database reference

`instance/2` `—` retrieve a clause from the database reference


## FORMS

clause(Head, Body)

clause(Head, Body, Ref)

instance(Ref, Clause)


## DESCRIPTION

When Head is bound to a non-variable term, the current module is searched for a clause whose head matches Head and whose body matches Body. If there is more than one clause that matches, then successive Heads and Bodys will be generated upon backtracking.

When a fact is found, Body will be unified with the atom true.

clause/3 unifies its third argument with the database reference that corresponds to the clause that was found. When Ref is instantiated in a call to clause/3, the other two arguments can be uninstantiated.

: / 2 can be used to specify which module should be searched. If Ref is a valid database reference, instance(Ref, Clause) retrieves the Prolog clause referenced by Ref and unifies it with Clause.


## EXAMPLES

The following examples show the use of clause/2 :

```
?- listing(fruit/1).
%user:fruit/1
fruit(apple).
fruit(_34):-
product(_34,plantGrowth)
;product(_34,plantFertilization).
fruit(orange).
%unusual:fruit/1
fruit(tomato).
fruit(kiwi).
yes.
```

```
?- clause(fruit(apple),true).
yes.
```

```
?- clause(fruit(X),Body).
X=apple
Body=true;
X=_1
Body=(product(_1,plantGrowth);
product(_1,plantFertilization));
X=orange
Body=true;
no.
```

```
?- unusual:clause(fruit(X),Body).
X=tomato
Body=true;
X=kiwi
Body=true;
no.
```


## ERRORS

If Ref is not instantiated to a database reference and Head is uninstantiated, the call to clause/3 fails.


## SEE ALSO

- [Bowen 91, 7.3]
- [Sterling 86, 12.2]
- [Clocksin 81, 6.4]. 
