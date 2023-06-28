---
title: 'gc/0'
predicates:
- {sig: 'gc/0', desc: 'invokes the garbage compactor'}
---

## FORMS
```
gc
```
## DESCRIPTION

Invokes the garbage compactor to reclaim unused space on the Prolog heap. Normally compaction is carried out automatically, so explicit calls to this predicate are not necessary.

## EXAMPLES

```
?- gc.

yes.
```
## SEE ALSO

- {% include book.md id="sterling86" sec="13" %}
