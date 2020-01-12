---
title: '->/2(arrow)'
iso: ifthen
predicates:
- {sig: '->/2', desc: 'if-then, and if-then-else'}
---

## FORMS

```
Condition -> TrueGoal

Condition -> TrueGoal ; FalseGoal
```
## DESCRIPTION


If `Condition` succeeds, then `TrueGoal` will be executed. `If-then` implicitly cuts the Condition. Cuts that occur within Condition or TrueGoal will cut back to the head of the parent clause. If Condition fails, then the call to `->/2` fails. The second form, results from the interaction between [`;/2`](semicolon.html) and
`->/2` because

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

- [`!/0`](cut.html)
- [`not/1`](not.html)

- {% include book.md id="bowen91"    sec="7.1" %}
- {% include book.md id="sterling86" sec="11" %}
- {% include book.md id="bratko86"   sec="5.1" %}
- {% include book.md id="clocksin81" sec="4.2" %}
