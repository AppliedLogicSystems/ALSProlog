---
title: 'skip/1'
predicates:
 - 'skip/1' : discard all input characters until specified character
---
`skip/1` `--` discard all input characters until specified character


## FORMS

skip(Char)


## DESCRIPTION

All characters on the current input stream are discarded up to and including the next character whose ASCII code is Char. Skipping past the end-of-file causes skip/1 to fail.


## EXAMPLES

```
?- skip(0'*),get(Next).
JourneyToThe*s
Next=115
yes.
```


## SEE ALSO

- [Clocksin 81, 6.9 ]. 
