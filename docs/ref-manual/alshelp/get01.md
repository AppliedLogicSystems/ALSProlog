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

- `skip/1`  
`get_char/2`  
`get_code/2`

- [Bowen 91, 7.8]
- [Clocksin 81, 6.9]
- [Bratko 86, 6.3]
