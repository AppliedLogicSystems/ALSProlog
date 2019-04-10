---
title: 'trace/[0,1]'
module: debugger
predicates:
- {sig: 'trace', args: {
    0: 'turn on tracing',
    1: 'trace the execution of a goal'
   }}
- {sig: 'notrace/0', desc: 'turn off tracing'}
---

## FORMS

```
trace Goal

trace(Goal)

trace

notrace
```

## DESCRIPTION

In the `trace/1` case, the `Goal` will be single stepped according to the debugger's leash mode. In the `trace/0` case, tracing will be turned on for all items until a call to `notrace/0` is encountered.


## EXAMPLES

```
?- trace(append([a,b,c],[d],X)).
(1)1call:append([a,b,c],[d],_11)?
(2)2call:append([b,c],[d],_94)?
(3)3call:append([c],[d],_170)?
(4)4call:append([],[d],_246)?
(4)4exit:append([],[d],[d])?
(3)3exit:append([c],[d],[c,d])?
(2)2exit:append([b,c],[d],[b,c,d])?
(1)1exit:append([a,b,c],[d],[a,b,c,d])?
```

## SEE ALSO

- [`spy/1`](spy.html)
- [`nospy/0`](spy.html)
- [`leash/0`](leash.html)
- {% include book.md id="bowen91"    sec="4.5" %}
- {% include book.md id="bratko86"   sec="8.4" %}
- {% include book.md id="clocksin81" sec="8.3" %}
