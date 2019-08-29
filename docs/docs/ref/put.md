---
title: 'put/1'
group: Input Output
module: sio
predicates:
- {sig: 'put/1', desc: 'write out a character'}
- {sig: 'tab/1', desc: 'prints out a specified number of spaces'}
---

## FORMS
```
put(Char)

tab(N)
```
## DESCRIPTION

If `Char` is bound to an integer within the range 0 -- 255, `put/1` will write out the character whose ASCII code is `Char` to the current output stream.

`tab/1` will write out `N` space characters (ASCII 32) to the standard output stream.


## EXAMPLES

```text
?- put(0'(),tab(15),put(0')).
(               )
```

## SEE ALSO

- `nl/0` {%- comment %} TODO: missing {% endcomment %}

- [User Guide (Prolog I/O)](../guide/10-Prolog-I-O.html)
- {% include book.md id="bowen91"    sec="7.8" %}
- {% include book.md id="clocksin81" sec="5.2" %}
- {% include book.md id="bratko86"   sec="6.3" %}
