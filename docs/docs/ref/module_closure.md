---
title: 'module_closure/[2,3]'
group: Prolog Database
predicates:
- {sig: 'module_closure', args: {
    2: 'creates a module closure',
    3: 'creates a module closure for the specified procedure'
  }}
---

## FORMS
```
:- module_closure(Name, Arity, Procedure) .

:- module_closure(Name, Arity) .
```
## DESCRIPTION

For some Prolog procedures, it is essential to know the module within which they are invoked. For example, [`setof/3`](setof.html) must invoke the goal in its second argument relative to the correct module. The problem is that `setof/3` is defined in `module builtins`, while it may invoked in some other module which is where the code defining the goal in the second argument should be run. In reality, `setof/3` is defined as the module closure of another predicate `setof/4` (whose definition appears in the `builtins module`). The extra argument to `setof/4` is the module in which the goal in the second argument of `setof/3` is to be run. Declaring `setof/3` to be a module closure of `setof/4` means that goals of the form
```
..., setof(X, G, L), ...
```
are automatically expanded to goals of the form
```
..., setof(M, X, G, L), ...
```
where `M` is the current module; i.e., the module in which the original call took place. Thus `setof/4` is supplied with the correct module `M` in which to run the goal in the second argument of the original call to `setof/3`.

The actual predicate that you write should expect to receive the calling module as its first argument. Then one ' closes ' the predicate with a module closure declaration which suppresses the first(module) argument. The arguments to `module_closure/3` are as follows :

- `Name` is the name of the procedure the user will call.

- `Arity` is the number of arguments of the user procedure; that is, the number of arguments in the 'closed' procedure which the user procedure will call.

- `Procedure` is the name of the(unclosed) procedure to call with the additional module argument. Note that `Procedure` can be different than `Name`, although they are often the same.

The procedure that the user will call should be exported if it is contained within a module. The actual (unclosed) procedure does not need to be exported. 

`module_closure/2` simply identifies the first and third arguments of `module_closure/3`. That is, the command
```
:- module_closure(foo, 5) .
```
is equivalent to
```
:- module_closure(foo, 5, foo) .
```
## EXAMPLES

The following example illustrates the use of `module_closure/3`. First assume that the following three modules and code have been created and loaded :
```
module m1.
use m3.
export testA/1.

testA(X) :- leading(X) .

p(tom).
p(dick).
p(harry).

endmod. % m1

module m2.
use m3.
export testB/1.

testB(X) :- leading(X) .

p(sally).
p(jane).
p(martha).

endmod. % m2

module m3.

leading(X) :- p(X) .

endmod. % m3
```
Attempting to run either testA or testB in default module `user` fails :

```
?- testA(X).

no.

?- testB(X).

no.
```
This is because the call to p(X) runs in module m3 which has no clauses defining `p/1`. Now let us change module m3 to read as follows :
```
module m3.

first(M, X) :- M:p(X).

export leading/1.

:- module_closure(leading, 1, first) .

endmod.
```
We have defined a new predicate `first/2` which carries a module as its first argument and which makes the call to `p(X)` in that module. And we have specified that `leading/1` is the module closure of `first/2`. Now the calls succeed :

```
?- testA(X).

X=tom

yes.

?- testB(X).

X=sally

yes.
```
Note that we exported `leading/1` from `module m3`, and both `module m1` and `module m2` were declared to use `module m3`.

## SEE ALSO

- [`:/2`](colon.html)

- [User Guide (Modules)](../guide/3-Modules.md)

