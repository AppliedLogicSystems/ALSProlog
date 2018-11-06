---
title: '->/2(arrow)'
predicates:
- {sig: '->/2', desc: 'if-then, and if-then-else'}
---
`->/2` — if-then, and if-then-else


## FORMS

```
Condition -> TrueGoal

Condition -> TrueGoal ; FalseGoal
```
## DESCRIPTION


If `Condition` succeeds, then `TrueGoal` will be executed. `If-then` implicitly cuts the Condition. Cuts that occur within Condition or TrueGoal will cut back to the head of the parent clause. If Condition fails, then the call to -&gt;/2 fails. The second form, results from the interaction between ;/2 and
-&gt;/2 because

```
Condition -> TrueGoal ; FalseGoal
```

is actually executed as :

```
( Condition -> TrueGoal) ; FalseGoal
```

In this case, FalseGoal will be executed instead of TrueGoal when Condition fails.

Cuts occurring in

- `Condition`

- `TrueGoal`

- `FalseGoal`

cut back to the head of the parent clause.


## EXAMPLES

```
?- true->write(a).
a
yes.
```

```
?- fail->write(a).
no.
```

```
?- fail->write(a);write(b).
b
yes.
```


## SEE ALSO

- `!/0`  
`not/1`

- [Bowen 91, 7.1]
- [Sterling 86, 11]
- [Bratko 86, 5.1]
- [Clocksin 81, 4.2]
