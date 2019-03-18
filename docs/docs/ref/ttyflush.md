---
title: 'ttyflush/0'
group: Input Output
module: sio
predicates:
- {sig: 'ttyflush/0', desc: 'forces all buffered output to the screen'}
---

## FORMS

`ttyflush`


## DESCRIPTION

`ttyflush/0` is used to make sure that output from tty I/O predicates appears on the screen when you want it to. Screen output is normally flushed whenever Prolog is waiting on some I/O operation. Typically, the I/O operation that is waited on is some form of tty read. In this case, you don't have to use `ttyflush`.

A long computation, while causing you to wait for a response, does not qualify as an I/O operation, so any output predicates that were run before the CPU hog started, might not show their output on the screen until the long computation is finished. To combat this problem, `ttyflush/0` can be used before entering into the long computation section of your program.


## EXAMPLES

If we define the following useless predicate:

`infiniteLoop :- infiniteLoop.`

and then try to write something to the screen before running it:

```
?- printf("AprilFool'sDay"), infiniteLoop.
AprilFool's Day 
BreakHandler
------------------------
a-Abort Computation
b-Break shell
c-Continue
d-Debug
e-Exit Prolog
f-Fail
p-Return to Previous Break Level
s-Show goal broken at
t-Stack trace
?- This message
Break(1)> a
Warning: Aborting from Control-C or Control-Break.
Error: Execution aborted.
```
we find that the output doesn't appear until the Control-C is pressed. We can avoid this problem by putting a call to `ttyflush/0` after the message printing predicate.

The following example shows the result:

```
?- printf("AprilFool'sDay"), ttyflush, infiniteLoop.
AprilFool's Day
BreakHandler
------------------------
a-Abort Computation
b-Break shell
c-Continue
d-Debug
e-Exit Prolog
f-Fail
p-Return to Previous Break Level
s-Show goal broken at
t-Stack trace
?- This message
Break(1)> a
Warning: Aborting from Control-C or Control-Break.
Error: Execution aborted.
```

## NOTES

The ISO Standard mandates that [`flush_output/[0,1]`](flush_output.html) is preferred over `ttyflush/0`.


## SEE ALSO

- [`printf/1`](printf.html)
- [`write/1`](write.html)
- [`put/1`](put.html)
- [`flush_output/[0,1]`](flush_output.html)
