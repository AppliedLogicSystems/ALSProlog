---
title: 'assert/[1,2]'
group: Prolog Database
iso: asserta
predicates:
- {sig: 'assert', args: { 
    1: 'adds a clause to a procedure',
    2: 'adds a clause to a procedure, returning a DB Ref'
  }}
- {sig: 'asserta', args: { 
    1: 'adds a clause to the beginning of a procedure',
    2: 'adds a clause to the beginning of a procedure, returning a DB Ref'
  }}
- {sig: 'assertz', args: { 
    1: 'adds a clause to the end of a procedure',
    2: 'adds a clause to the end of a procedure, returning a DB Ref'
  }}
---

## FORMS
```
assert(Clause)

assert(Clause, Ref)

asserta(Clause)

asserta(Clause, Ref)

assertz(Clause)

assertz(Clause, Ref)
```
## DESCRIPTION

The `Clause` is added to the procedure with the same name and arity in the module that assert is called from. All uninstantiated variables are re-quantified in the clause before it is added to the database, thus breaking any connection between the original variables and those occurring in the clause in the database. Because of this behavior, the order of calls to assert is important. For example, assuming no clauses already exist for `p/1`, the first one of the following goals will fail, while the second succeeds.

```
?- X = a, assert(p(X)), p(b) .

no.

?- assert(p(X)), X = a, p(b) .

X = a

yes.
```
The placement of a clause by `assert/1` is defined by the implementation. `asserta/1` always adds its clause before any other clauses in the same procedure, while `assertz/1` always adds its clause at the end. Each form of assert can take an optional second argument(normally an uninstantiated variable) which is the database reference corresponding to the clause that was added. `:/2` can be used to specify in which module the assert should take place. The database reference argument is normally passed in as an uninstantiated variable.


## EXAMPLES

The following example shows how the different forms of assert work :

```
?- assert(p(a)),asserta(p(c)),assertz(p(b)).
yes.
```

```
?- listing(p/1).
%user:p/1
p(c).
p(a).
p(b).
yes.
```

Notice that the order of the clauses in the database is different than the order in which they were asserted. This is because the second assert was done with `asserta/1`, and the third assert was done with `assertz/1`. The `asserta/1` call put the `p(c)` clause ahead of `p(a)` in the database. The `assertz/1` call put `p(b)` at the end of the `p/1` procedure, which happens to be after the `p(a)` clause. The next example demonstrates the use of parentheses in asserting a rule into the Prolog database :

```
?- assertz((magic(X):-wizard(X);pointGuard(X,lakers))).
X=_1
yes.
```

```
?- listing(magic/1).
%user:magic/1
magic(_24):-
wizard(_24)
;pointGuard(_24,lakers).
yes.
```

If the extra parentheses were not present, the Prolog parser would print the following error message :

```
assertz(magic(X) :- wizard(X) ; pointGuard(X, lakers)) .
^
Syntax Error : Comma or right paren expected in argument list.
```

The next example shows how the assert predicates can be used with modules. The first goal fails because there is no module named animals. After the module is created, the assertion is successful as you can see by looking at the listing of the animals module.

```
?- animals:assert(beast(prolog)).
no.
```

```
?- [user].
Consulting user.
module animals.
endmod.
user consulted.
yes.
```

```
?- animals:assert(beast(prolog)).
yes.
```

```
?- listing(animals:_).
%animals:beast/1
beast(prolog).
yes.
```

The following example shows the effects of adding clauses to procedures which are part of the current goal:

```
?- assert(movie(jaws)),movie(X),assert(movie(jaws2)).
X=jaws;
no.
```

The reason this didn't work is an implementation issue. The following is the sequence of events illustrating what happened:

- First the `assert(movie(jaws))` subgoal was run, causing a new procedure to be placed in the Prolog database.

- When the subgoal `movie(X)` was run, no choicepoint was created because there were no other clauses to try if failure occurred.

- After `movie(X)` succeeded, the second clause of `movie/1` was asserted, and the initial goal succeeded, binding `X` to `jaws`.

- Backtracking was initiated by the `';'` response to the solution, but no second solution was found for `movie/1`, even though there was a solution to be found. This was because there was no choicepoint to return to in `movie/1`.

One of the interesting(and possibly bad) parts to this phenomenon is that the second time this goal is run it will backtrack through the clauses of `movie/1`. This is shown below:

```
?- listing(movie/1).
%user:movie/1
movie(jaws).
movie(jaws2).
yes.
```

```
?- assert(movie(jaws)),movie(X),assert(movie(jaws2)).
X=jaws;
X=jaws2;
X=jaws;
X=jaws2;
X=jaws2;
X=jaws2
yes.
```

The reason for this is that there was more than one clause for `movie/1` in the database this time, so a choicepoint was created for the `movie(X)` subgoal. Incidentally, this goal would continue finding the

```
X = jaws2
```

solution. This is because every time the `movie(X)` finds a new solution, it succeeds, thus causing the

```
assert(movie(jaws2))
```

subgoal to run. This adds another clause to the database to be tried when the user causes backtracking by pressing semicolon(`;`) . If you look at the conversation with the Prolog shell shown above, you will notice that the last solution was accepted because no `';'` was typed after it.


## ERRORS

Clauses must be either structured terms or atoms. If clause is a rule, with a principal functor of `:-/2`, then the head and all the subgoals of the clause must either be atoms or structured terms.


## NOTES

ALS Prolog provides a global variable mechanism separate from the Prolog database. Using global variables is much more efficient than using assert and retract.


## SEE ALSO

- [`:/2`](colon.html)

- {% include book.md id="bowen91"    sec="7.3" %}
- {% include book.md id="clocksin81" sec="6.4" %}
- {% include book.md id="bratko86"   sec="7.4" %}
- {% include book.md id="sterling86" sec="12.2" %}
