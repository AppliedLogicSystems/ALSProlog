---
title: 'abolish/2'
predicates:
 - 'abolish/2' : remove a procedure from the database
---
`abolish/2` `--` remove a procedure from the database


## FORMS

abolish(Name, Arity)


## DESCRIPTION

All the clauses for the specified procedure Name/Arity in the current module are removed from the database. The module from which to abolish clauses can be specified with a ' : ' . Given appropriate arguments, abolish/2 will succeed whether or not it actually removed any clauses.


## EXAMPLES


```
?- abolish(zip,3).
yes.
```

```
?- othermodule:abolish(victim,7).
yes.
```


## ERRORS

If Name is not an atom or Arity is not an integer, abolish/2 fails.


## SEE ALSO

- `:/2
