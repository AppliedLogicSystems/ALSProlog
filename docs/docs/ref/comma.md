---
title: ',/2 (comma)'
iso: and
predicates:
- {sig: ',/2', desc: 'conjunction of two goals'}
---

## FORMS
```
FirstGoal, SecondGoal
```

## DESCRIPTION

The first argument, `FirstGoal`, is called as a goal. If it succeeds, then the `SecondGoal` will be run. If either goal fails, the most recent alternative will be attempted after backtracking.

## EXAMPLES

The following example shows the use of the ', ' connector :

```
?- listing.
lucky(mick,love).
boss(mick,jerri).
yes.

?- lucky(Who,What),boss(Who,Boss).
Who=mick
What=love
Boss=jerri
yes.
```

The goal submitted to the Prolog shell consists of two subgoals

- `lucky(Who, What)`

- `boss(Who, Boss)`

The subgoals are connected together by using the `','` connective. In the next example, the first subgoal fails, so the second subgoal is not executed :

```
?- fail,write('Help, I am stuck in an example'),nl.
no.
```

The following shows that ', ' works the same in [`call/1`](call.html) :

```
?- call((fail,write('Help, I am stuck in an example'),nl)).
no.
```

Note that the parentheses around the argument to [`call/1`](call.html) are to keep the parser from creating a call to `call/3`.


## SEE ALSO

- [`call/1`](call.html)
- [`:/2`](colon.html)
- [`;/2`](semicolon.html)

- {% include book.md id="bratko86"   sec="2.3" %}
- {% include book.md id="clocksin81" sec="6.7" %}
