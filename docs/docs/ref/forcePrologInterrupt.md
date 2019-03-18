---
title: 'forcePrologInterrupt/0'
predicates:
- {sig: 'forcePrologInterrupt/0', desc: 'force interrupt on next call'}
- {sig: 'callWithDelayedInterrupt/[1,2]', desc: 'call goal, setting delayed interrupt'}
---

## FORMS
```
forcePrologInterrupt

callWithDelayedInterrupt(Call)

callWithDelayedInterrupt(Module, Call)
```
## DESCRIPTION

`forcePrologInterrupt` forces an interrupt on the next call by setting the heap safety margin to a sufficiently large number which is guaranteed to be larger than the actual heap margin.

`callWithDelayedInterrupt(Call)` acts like [`call/1`](call.html), invoking `Call`. However, it arranges the system so that an interrupt will take place on the first call occurring after the invocation of `Call`.

`callWithDelayedInterrupt(Module, Call)` acts like `callWithDelayedInterrupt/1`, except that it invokes `Call` within module `Module`.


## SEE ALSO

- [`setPrologInterrupt/1`](setPrologInterrupt.html)
- [`getPrologInterrupt/1`](setPrologInterrupt.html)

- [User Guide (Prolog Interrupts)](../guide/9-Freeze,-Exceptions,-Events,-Interrupts,-Signals.md)
