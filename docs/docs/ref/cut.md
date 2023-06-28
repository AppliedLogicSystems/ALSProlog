---
title: '!/0'
iso: cut
predicates:
- {sig: '!/0', desc: '(cut) removes choicepoints'}
---

## FORMS
```
FirstGoal, !, SecondGoal

FirstGoal, !; SecondGoal
```
## DESCRIPTION

Discards all choicepoints made since the parent goal started execution, including the choicepoints, if any, created by calling the parent goal. In the following two cases, a cut in `Condition` will remove all choicepoints created by the `Condition`, any subgoals to the left of the `Condition`, and the choicepoint for the parent goal.
```
Condition = (Things, !, MoreThings)

Condition -> TrueGoal; FalseGoal

call(Condition)
```
In other words,
<br>&nbsp;&nbsp;&nbsp;&nbsp;`->`
<br>&nbsp;&nbsp;&nbsp;&nbsp;`call/1`
<br>&nbsp;&nbsp;&nbsp;&nbsp;`;`
<br>&nbsp;&nbsp;&nbsp;&nbsp;`:`
<br>&nbsp;&nbsp;&nbsp;&nbsp;`,`
<br>are all transparent to cut. The ISO Prolog Standard requires that [`call/1`](call.html) be opaque to cut. At this time, ALS Prolog deviates from the standard.


## EXAMPLES

In the following example, the solution eats(chris, pizza) causes a cut to be executed. This removes the choicepoint for the goal `eats/2`. As a result, the solution eats(mick, pizza) is not found, even though Mick will eat anything.

```
?- listing.

eats(chris,pizza):-!.
eats(mick,Anything).

yes.

?- eats(Person,pizza).

Person=chris;

no.
```

The next example shows that [`not/1`](not.html) is opaque to cut. This means that a '!' inside the call to `not/1` will not cut out the choicepoint for `not/1`, or any other choicepoints created by goals to the left of `not/1`.
```
?- not((!,fail)).

yes.
```

Notice the extra pair of parentheses above. This is to prevent the parser from creating a goal to `not/2` instead of `not/1`. In the next example, the transparency of [`call/1`](call.html) with respect to cut is shown:

```
?- listing.

cool(peewee):-call((!,fail)).

cool(X).

yes.

?- cool(peewee).

no.

?- cool(bugsbunny).

yes.
```

`peewee` is not cool because the `'!'` removed the choicepoint for `cool/1`. The `fail` after the `'!'` prevented `cool/1` from succeeding. The rationale for having cut behave this way is so that:

&nbsp;&nbsp;&nbsp;&nbsp;`cool(peewee) :- call(( !,fail)).`

will be equivalent to

&nbsp;&nbsp;&nbsp;&nbsp;`cool(peewee) :- !, fail.`

The next example shows the transparency of -> with respect to cut.

```
?- listing.

cool(X):-(X=peewee,!)->fail.

cool(X).

yes.

?- cool(peewee).

no.
```
Again, peewee is not considered cool. In the goal

```
?- cool(peewee).

no.
```

the `'!'` after `X = peewee` cuts the choicepoint for `cool/1`. The condition succeeds, causing fail to be executed. However, the second clause is never reached because the choicepoint has been cut away. Consequently, the goal fails. The goal

```
?- cool(daffyduck).

yes.
```
succeeds because the '!' is never reached in the condition of ->. The -> fails because there is no else subgoal. This causes the next clause for `cool/1` to be executed. This clause always succeeds, therefore `daffyduck` is considered cool.


## SEE ALSO

- [`->/2`](arrow.html)
- [`not/1`](not.html)

- {% include book.md id="bowen91"    sec="7.1" %}
- {% include book.md id="sterling86" sec="11" %}
- {% include book.md id="bratko86"   sec="5.1" %}
- {% include book.md id="clocksin81" sec="4.2" %}

