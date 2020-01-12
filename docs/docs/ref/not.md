---
title: 'not/1'
iso: notprovable
predicates:
- {sig: 'not/1', desc: 'tests whether a goal fails'}
- {sig: '\+/1', desc: 'tests whether a goal fails'}
---

## FORMS
```
not Goal

not(Goal)

\+ Goal

\+(Goal)
```
## DESCRIPTION

`not/1` and `\+/1` implement negation by failure. If the `Goal` fails, then `not(Goal)` succeeds. If `Goal` succeeds, then `not(Goal)` fails. When `not/1` succeeds it doesn't bind any variables. Cuts occurring within Goal will be restricted to cutting choices created within the execution of `Goal`.

## EXAMPLES
```
?- not(true).

no.

?- not(a=b).

yes.

?- not(not(f(A)=f(b))).

A=_2

yes.
```
## SEE ALSO

- {% include book.md id="bowen91"    sec="7.1" %}
- {% include book.md id="sterling86" sec="11.3" %}
- {% include book.md id="bratko86"   sec="5.3" %}
- {% include book.md id="clocksin81" sec="6.7" %}
