---
title: 'get0/1'
group: Input Output
module: sio
predicates:
- {sig: 'get0/1', desc: 'read the next character'}
- {sig: 'get/1', desc: 'read the next printable character'}
---

## FORMS
```
get0(Char)

get(Char)
```
## DESCRIPTION

`get0/1` unifies `Char` with the ASCII code of the next character from the current input stream. If there are no more characters left in the stream, `Char` will be unified with -1

`get/1` discards all non-printing characters from the current input stream. It unifies `Char` with the ASCII code of the first non-blank printable character. Char is unified with -1 on end of file.

## EXAMPLES
```
?- get(First),get(Second),get(Third).
ABCDEFGHI<newline>

First=65,
Second=66,
Third=67

yes.
```
## SEE ALSO

- [`skip/1`](skip.html)
- [`get_char/2`](get_char.html)
- [`get_code/2`](get_code.html)

- {% include book.md id="bowen91"    sec="7.8" %}
- {% include book.md id="clocksin81" sec="6.9" %}
- {% include book.md id="bratko86"   sec="6.3" %}
