—-
title: ':/2(colon)'
predicates:
 - ':/2' : calls a goal in the specified module
—-
`:/2` `—` calls a goal in the specified module


## FORMS

Module : Goal


## DESCRIPTION

Module should be instantiated to a name of a module and Goal should be instantiated to a non-variable term. The Goal will be executed as if the call to Goal appeared in the module named Module. Any module dependent procedures such as assert/1 or retract/1 will operate on the procedures defined in Module. Any cuts appearing in Goal will cut back through the head of the clause that contained the call to : / 2


## EXAMPLES

builtins : write(foobar)

behavior : setof(Person, eats_linguini(Person), Persons)


## ERRORS

If Module is unbound or not bound to a valid module name, the call to : / 2 will fail.


## SEE ALSO

- `call/1`  
`UserGuide(Modules).
