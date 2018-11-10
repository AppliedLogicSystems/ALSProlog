---
title: 'abort/0'
predicates:
 - 'abort/0' : return execution immediately to the Prolog shell
---

## FORMS

abort


## DESCRIPTION

The current computation is discarded and control returns to the Prolog shell.


## EXAMPLES

```
error(Message) :-
    write(Message), nl,
    abort.
```

## SEE ALSO

- `see/1`  
`tell/1`

## NOTES

If ALS Prolog was started with the -b command line option, the system exits when abort is executed.

