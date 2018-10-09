---
title: 'ttyflush/0'
predicates:
 - 'ttyflush/0' : forces all buffered output to the screen
---
`ttyflush/0` `--` forces all buffered output to the screen


## FORMS

ttyflush


## DESCRIPTION

ttyflush/0 is used to make sure that output from tty I/O predicates appears on the screen when you want it to. Screen output is normally flushed whenever Prolog is waiting on some I/O operation. Typically, the I/O operation that is waited on is some form of tty read. In this case, you don ' t have to use ttyflush.

A long computation, while causing you to wait for a response, does not qualify as an I/O operation, so any output predicates that were run before the CPU hog started, might not show their output on the screen until the long computation is finished. To combat this problem, ttyflush/0 can be used before entering into the long computation section of your program.


## EXAMPLES

If we define the following useless predicate :

infiniteLoop :- infiniteLoop.

and then try to write something to the screen before running it :

```
?- printf(&quot;AprilFool'sDay&quot;),infiniteLoop.
AprilFool'sDayBreakHandler
------------------------
a-AbortComputation
b-Breakshell
c-Continue
d-Debug
e-ExitProlog
f-Fail
p-ReturntoPreviousBreakLevel
s-Showgoalbrokenat
t-Stacktrace
?- Thismessage
Break(1)>a
Warning:AbortingfromControl-CorControl-Break.
Error:Executionaborted.
wefindthattheoutputdoesn'tappearuntiltheControl-Cispressed.Wecanavoidthisproblembyputtingacalltottyflush/0afterthemessageprintingpredicate.Thefollowingexampleshowstheresult:
?- printf(&quot;AprilFool'sDay&quot;),ttyflush,infiniteLoop.
AprilFool'sDayBreakHandler
------------------------
a-AbortComputation
b-Breakshell
c-Continue
d-Debug
e-ExitProlog
f-Fail
p-ReturntoPreviousBreakLevel
s-Showgoalbrokenat
t-Stacktrace
?- Thismessage
Break(1)>a
Warning:AbortingfromControl-CorControl-Break.
Error:Executionaborted.
```

## NOTES

The ISO Standard mandates that flush_output/ [0, 1 ] is preferred over ttyflush/0.


## SEE ALSO

- `printf/1`  
`write/1`  
`put/1`  
`flush_output/[0`  
`1].
