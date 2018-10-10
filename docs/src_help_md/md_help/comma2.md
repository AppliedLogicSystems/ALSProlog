---
title: ',/2(comma)'
predicates:
 - ',/2' : conjunction of two goals
---
`,/2` — conjunction of two goals


## FORMS

FirstGoal, SecondGoal


## DESCRIPTION

The first argument is called as a goal. If it succeeds, then the SecondGoal will be run. If either goal fails, the most recent alternative will be attempted after backtracking.


## EXAMPLES

The following example shows the use of the ', ' connector :

```
?- [user].
Consultinguser.
lucky(mick,love).
boss(mick,jerri).
userconsulted.
yes.
```

```
?- lucky(Who,What),boss(Who,Boss).
Who=mick
What=love
Boss=jerri
yes.
```

The goal submitted to the Prolog shell consists of two subgoals

- lucky(Who, What)

- boss(Who, Boss)

The subgoals are connected together by using the ', ' predicate. In the next example, the first subgoal fails, so the second subgoal is not executed :

```
?- fail,write('Help,I''mstuckinanexample'),nl.
no.
```

The following shows that ', ' works the same in call/1 :

```
?- call((fail,write('Help,I''mstuckinanexample'),nl)).
no.
```

Note that the parentheses around the argument to call/1 are to keep the parser from creating a call to call/3.


## SEE ALSO

- `call/1`  
`:/2`  
`;/2`

- [Bratko 86, 2.3]
- [Clocksin 81, 6.7]. 
