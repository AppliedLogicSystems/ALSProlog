—-
title: 'forcePrologInterrupt/0'
predicates:
 - 'forcePrologInterrupt/0' : force interrupt on next call
 - 'callWithDelayedInterrupt/1' : call goal, setting delayed interrupt
 - 'callWithDelayedInterrupt/2' : call goal, setting delayed interrupt
—-
`forcePrologInterrupt/0` `—` force interrupt on next call

`callWithDelayedInterrupt/1` `—` call goal, setting delayed interrupt

`callWithDelayedInterrupt/2` `—` call goal, setting delayed interrupt


## FORMS

forcePrologInterrupt

callWithDelayedInterrupt(Call)

callWithDelayedInterrupt(Module, Call)


## DESCRIPTION

forcePrologInterrupt forces an interrupt on the next call by setting the heap safety margin to a sufficiently large number which is guaranteed to be larger than the actual heap margin.

callWithDelayedInterrupt(Call) acts like call/1, invoking Call. However, it arranges the system so that an interrupt will take place on the first call occurring after the invocation of Call.

callWithDelayedInterrupt(Module, Call)

acts like

callWithDelayedInterrupt/1, except that it invokes Call within module Module.


## SEE ALSO

- `setPrologInterrupt/1`  
`getPrologInterrupt/1`
- `UserGuide(PrologInterrupts).
