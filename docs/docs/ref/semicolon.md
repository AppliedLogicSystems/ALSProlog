---
title: ';/2 (semi-colon)'
iso: or
predicates:
- {sig: ';/2', desc: 'disjunction of two goals'}
---

## FORMS

```
FirstGoal ; SecondGoal
```

## DESCRIPTION

The `FirstGoal` is called. Later, upon backtracking, `SecondGoal` will be called. Cuts appearing in either `FirstGoal` or `SecondGoal` will cut back to the head of the clause which contained the call to `;/2`.

## EXAMPLES

The following example shows the use of ';' as the boolean or connective:

```
?- [user].
Consulting user.
language(postscript).
language(pascal).
food(burrito).
food(crab).
food(steak).
user consulted.
yes.

?- language(postscript) ; food(postscript).
yes.
```

Notice that although postscript isn't a food, the goal succeeds. This is because only one of the two subgoals has to succeed for `;` to succeed. In the next example, we add a few more facts to the database. This example shows that `;` goal also succeeds if both of its arguments can succeed.

```
?- [user].
Consulting user.
food(prolog).
language(prolog).
user consulted.
yes.

?- language(prolog) ; food(prolog).
yes.
```

Note that the food (prolog) goal is never run, even though it is true. The next example shows that `;` will fail if neither of the subgoals succeed.

```
?- language(fortran) ; food(fortran).
no.
```

The next example illustrates the behavior of `;` upon backtracking. The semicolons after the shown answers are typed in by the user interactively:

```
?- language(X) ; food(X).
X=postscript;
X=pascal;
X=prolog;
X=burrito;
X=crab;
X=steak;
X=prolog;
no.
```


## SEE ALSO

- [`!/0`](cut.html)
- [`->/2`](arrow.html)
- {% include book.md id="bowen91"    sec="7.1" %}
- {% include book.md id="sterling86" sec="10.4" %}
- {% include book.md id="bratko86"   sec="2.3" %}
- {% include book.md id="clocksin81" sec="6.7" %}
