---
title: 'leash/1'
predicates:
 - 'leash/1' : set which ports are leashed for the debugger
---
`leash/1` `--` set which ports are leashed for the debugger


## FORMS

leash(Mode)

leash([Mode | Modes ])


## DESCRIPTION

The leashing mode of the debugger is set to Mode where Mode is one of the atoms shown in
[Table 7(_[Leashing,Modes.]_)](href = start.htm)
. Note that it does not make sense to use the list format for specifying modes if the all mode is included with other modes.





|**Mode|Function**|
|-----|---------|
| all | Prompt at all ports | 
| call | Prompt at call ports | 
| exit | Prompt at redo ports | 
| fail | Prompt at fail ports | 
| redo | Prompt at redo ports | 


Table 7 Leashing Modes.


## EXAMPLES

The following examples illustrate the use of leash/1 :

```
?- leash([call,redo]).
yes.
```

```
?- leash([]).
yes.
```

Note that using an empty list as the argument to leash/1, as shown in the example above, results in no ports being leashed.


## SEE ALSO

- `trace/1`  
`spy/1`
- `Tools(UsingtheDebugger)`  
`[Clocksin81`  
`8.4]

