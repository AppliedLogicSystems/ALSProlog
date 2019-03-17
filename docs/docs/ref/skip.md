---
title: 'skip/1'
group: Input Output
module: sio
predicates:
- {sig: 'skip/1', desc: 'discard all input characters until specified character'}
---

## FORMS

```
skip(Char)
```

## DESCRIPTION

All characters on the current input stream are discarded up to and including the next character whose ASCII code is `Char`. Skipping past the end-of-file causes `skip/1` to fail.

## EXAMPLES

```
?- skip(0'*), get(Next).
Journey To The *s
Next = 115
yes.
```

## SEE ALSO

- {% include book.md id="clocksin81" sec="6.9" %}
