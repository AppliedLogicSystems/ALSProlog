---
title: 'statistics/[0,2]'
predicates:
- {sig: 'statistics/0', desc: 'display memory allocation information'}
- {sig: 'statistics/2', desc: 'display runtime statistics'}
---

## FORMS

```
statistics

statistics(runtime, X)
```

## DESCRIPTION

`statistics/0` shows the current allocations and amounts used for the working areas of ALS Prolog. `statistics(runtime, X)` unifies `X` with a two-element list

`[Total, SinceLast]`

where `Total` is the elapsed cpu time since the start of Prolog execution, and `SinceLast` is the cpu time which has elapsed since the last call to `statistics/2`.


## EXAMPLES

```
?- statistics.
Machine State:
E=ef7fd94 B=efffe1c H=ef7fe44 HB=ef7fe44 TR=efffe1c
MSP=ef7fd8c SPB=ef7fd94
Clause Space: 55540/262144 (bytes used/total bytes)
yes.
```

```
?- statistics(runtime, [Total, SinceLast]).
Total = 1.95
SinceLast = 0.033333333
yes.
```


## NOTES

E, B, H, HB, TR, MSP, SPB are registers in the Warren Abstract Machine(WAM) ; cf. [Warren 83], [Warren 86], [Maier 88], Chapter 11.8, [Tick 88].


