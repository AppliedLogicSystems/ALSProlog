---
title: 'statistics/[0,2]'
predicates:
 - 'statistics/0' : display memory allocation information
 - 'statistics/2' : display runtime statistics
---
`statistics/0` `--` display memory allocation information

`statistics/2` `--` display runtime statistics


## FORMS

statistics

statistics(runtime, X)


## DESCRIPTION

statistics/0 shows the current allocations and amounts used for the working areas of ALS Prolog. statistics(runtime, X) unifies X with a two-element list

[Total, SinceLast ],

where Total is the elapsed cpu time since the start of Prolog execution, and SinceLast is the cpu time which has elapsed since the last call to statistics/2.


## EXAMPLES

```
?- statistics.
MachineState:
E=ef7fd94B=efffe1cH=ef7fe44HB=ef7fe44TR=efffe1c
MSP=ef7fd8cSPB=ef7fd94
ClauseSpace:55540/262144(bytesused/totalbytes)
yes.
```

```
?- statistics(runtime,[Total,SinceLast]).
Total=1.95
SinceLast=0.033333333
yes.
```


## NOTES

E, B, H, HB, TR, MSP, SPB are registers in the Warren Abstract Machine(WAM) ; cf. [Warren 83 ], [Warren 86 ], [Maier 88 ], Chapter 11.8, [Tick 88 ].


