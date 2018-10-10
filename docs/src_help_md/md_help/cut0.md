---
title: '!/0'
predicates:
 - '!/0' :(cut) removes choice points
---
`!/0` —(cut) removes choice points


## FORMS

FirstGoal, !, SecondGoal

FirstGoal, !; SecondGoal


## DESCRIPTION


Discards all choice points made since the parent goal started execution, including the choice points, if any, created by calling the parent goal. In the following two cases, a cut in Condition will remove all choice points created by the Condition, any subgoals to the left of the Condition, and the choice point for the parent goal.


Condition =(Things, !, MoreThings)


Condition- &gt; TrueGoal; FalseGoal

call(Condition)

In other words,

- &gt;

call/1

;

:

,

are transparent to cut. The ISO Prolog Standard requires that call/1 be opaque to cut. At this time, ALS Prolog deviates from the standard.


## EXAMPLES

In the following example, the solution eats(chris, pizza) causes a cut to be executed. This removes the choice point for the goal eats/2. As a result, the solution eats(mick, pizza) is not found, even though Mick will eat anything.

```
?- [user].
Consultinguser.
eats(chris,pizza):-!.
eats(mick,Anything).
userconsulted.
yes.
```

```
?- eats(Person,pizza).
Person=chris;
no.
```

The next example shows that not/1 is opaque to cut. This means that a ' ! ' inside the call to not/1 will not cut out the choicepoint for not/2, or any other choicepoints created by goals to the left of not/2.

```
?- not((!,fail)).
yes.
```

Notice the extra pair of parentheses above. This is to prevent the parser from creating a goal to not/2 instead of not/1. In the next example, the transparency of call/1 with respect to cut is shown :

```
?- [user].
Consultinguser.
cool(peewee):-call((!,fail)).
cool(X).
userconsulted.
yes.
```

```
?- cool(peewee).
no.
```

```
?- cool(bugsbunny).
yes.
```

peewee is not cool because the ' ! ' removed the choicepoint for cool/1. The fail after the ' ! ' prevented cool/1 from succeeding. The rationale for having cut behave this way is so that :

cool(peewee) :- call(( !,fail)) .

will be equivalent to

cool(peewee) :- !, fail.

The next example shows the transparency of - &gt; with respect to cut.

```
?- [-user].
Reconsultinguser.
cool(X):-(X=peewee,!)->fail.
cool(X).
userreconsulted.
yes.
```

```
?- cool(peewee).
no.
```

Again, peewee is not considered cool. In the goal

```
?- cool(peewee).
no.
```

the ' ! ' after X = peewee cuts the choicepoint for cool/1. The condition succeeds, causing fail to be executed. However, the second clause is never reached because the choicepoint has been cut away. Consequently, the goal fails. The goal

```
?- cool(daffyduck).
yes.
```

succeeds because the ' ! ' is never reached in the condition of - &gt; . The - &gt; fails because there is no else subgoal. This causes the next clause for cool/1 to be executed. This clause always succeeds, therefore daffyduck is considered cool.


## SEE ALSO

- `->/2`  
`not/1`

- [Bowen 91, 7.1]
- [Sterling 86, 11]
- [Bratko 86, 5.1]
- [Clocksin 81, 4.2]. 
