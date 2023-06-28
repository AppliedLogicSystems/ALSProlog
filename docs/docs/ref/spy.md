---
title: 'spy/[0,1]'
module: debugger
predicates:
- {sig: 'spy', args: {
    0: 'enable spy points',
    1: 'sets a spy point'
   }}
- {sig: 'nospy', args: {
    0: 'removes all spy points',
    1: 'removes a spy point'
   }}
---

## FORMS

```
spy Name/Arity

nospy Name/Arity

nospy

spy(Module : Name/Arity)

nospy(Module : Name/Arity)
```

## DESCRIPTION

`spy/1` places a spy point on the procedure name `Name/Arity`. `nospy/1` removes a specific spy point, while `nospy/0` removes all spy points that are currently set. During consult or reconsult, all spy points are disabled, and spy points on predicates which are consulted or reconsulted are removed. `spy/0` re-enables spy points which have been disabled (e.g., those which were not removed, but were disabled during a reconsult).


## SEE ALSO

- [Tools (Using the Debugger)](../guide/14-Using-the-Four-Port-Debugger.md)
- {% include book.md id="bowen91"    sec="6.5" %}
- {% include book.md id="clocksin81" sec="6.13" %}
- {% include book.md id="bratko86"   sec="8.4" %}
