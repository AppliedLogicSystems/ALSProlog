---
title: 'spy/[0,1]'
predicates:
 - 'spy/0' : enable spy points
 - 'spy/1' : sets a spy point
 - 'nospy/0' : removes all spy points
 - 'nospy/1' : removes a spy point
---
`spy/0` — enable spy points

`spy/1` — sets a spy point

`nospy/0` — removes all spy points

`nospy/1` — removes a spy point


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

- Tools (Using the Debugger)
- [Bowen 91, 6.5]
- [Clocksin 81, 6.13]
- [Bratko 86, 8.4]
