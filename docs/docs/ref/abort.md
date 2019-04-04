---
title: 'abort/0'
predicates:
- {sig: 'abort/0', desc: 'return execution immediately to the Prolog shell'}
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

- [`see/1`](see.html)
- [`tell/1`](tell.html)

## NOTES

If ALS Prolog was started with the -b command line option, the system exits when abort is executed.

