---
title: 'gc/0'
predicates:
 - 'gc/0' : invokes the garbage compactor
---
`gc/0` — invokes the garbage compactor


## FORMS

gc


## DESCRIPTION

Invokes the garbage compactor to reclaim unused space on the Prolog heap. Normally compaction is carried out automatically, so explicit calls to this predicate are not necessary.


## EXAMPLES

```
?- gc.
yes.
```


## SEE ALSO

- [Sterling 86, 13]. 
