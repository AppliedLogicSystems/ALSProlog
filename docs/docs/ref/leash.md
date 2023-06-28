---
title: 'leash/1'
module: debugger
predicates:
- {sig: 'leash/1', desc: 'set which ports are leashed for the debugger'}
---

## FORMS
```
leash(Mode)

leash([Mode | Modes ])
```
## DESCRIPTION

For each of the `Modes` on the argument, the leashing mode of the debugger is set for that `Mode`, where `Mode` is one of the atoms shown in *Leashing Modes Table* below.
Note that it does not make sense to use the list argument format for specifying modes if the `all` mode is included with other modes.

**Leashing Modes Table**

|Mode|Function|
|-----|---------|
| all | Prompt at all ports | 
| call | Prompt at call ports | 
| exit | Prompt at redo ports | 
| fail | Prompt at fail ports | 
| redo | Prompt at redo ports | 

## EXAMPLES

The following examples illustrate the use of `leash/1`:

```
?- leash([call,redo]).
yes.

?- leash([]).
yes.
```

Note that using an empty list as the argument to `leash/1`, as shown in the example above, results in no ports being leashed.

## SEE ALSO

- [`trace/1`](trace.html)
- [`spy/1`](spy.html)

- [Tools (Using the Debugger)](../guide/14-Using-the-Four-Port-Debugger.md)
- {% include book.md id="clocksin81" sec="8.4" %}

