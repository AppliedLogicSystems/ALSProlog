---
title: 'char_code/2'
group: Terms
iso: charcode
predicates:
- {sig: 'char_code/2', desc: 'convert between characters and codes'}
---

## FORMS
```
char_code(Char, Code)
```
## DESCRIPTION

`char_code(Char, Code)` is true if the character `Char` has character code `Code`. At least one of `Char` or `Code` must be instantiated.

## EXAMPLES

```
?- char_code(a,C).
C=97
yes.
```

```
?- char_code(C,98).
C=b
yes.
```

```
?- char_code(foo,C).
Error: Argument of type character expected instead of foo.
- Goal:          builtins:char_code(foo,_A)
- Throw pattern: error(type_error(character,foo),[builtins:char_code(foo,_A)])
```
## ERRORS

Char and Code are both variables

-- -- -- -- &gt; instantiation_error.

Char is neither a variable nor a character

-- -- -- -- &gt; type_error(character) .

Code is neither a variable nor an integer

-- -- -- -- &gt; type_error(integer) .

Code is an integer but is not a character code

-- -- -- -- &gt; representation_error(character_code) .

## SEE ALSO

- [`atom_chars/2`](atom_chars.html)
- [`number_chars/2`](number_chars.html)
- [`term_chars/2`](term_chars.html)
