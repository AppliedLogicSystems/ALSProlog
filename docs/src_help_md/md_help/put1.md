---
title: 'put/1'
predicates:
 - 'put/1' : write out a character
 - 'tab/1' : prints out a specified number of spaces
---
`put/1` `--` write out a character

`tab/1` `--` prints out a specified number of spaces


## FORMS

put(Char)

tab(N)


## DESCRIPTION

If Char is bound to an integer within the range 0 -- 255, put/1 will write out the character whose ASCII code is Char to the current output stream.

tab/1 will write out N space characters(ASCII 32) to the standard output stream.


## EXAMPLES

```
?- put(~(),tab(15),put(~)).
()
yes.
```


## SEE ALSO

- `nl/0`  
`UserGuide(PrologI/O)`  
`[Bowen91`  
`7.8]`  
`[Clocksin81`  
`5.2]`  
`[Bratko86`  
`6.3].
