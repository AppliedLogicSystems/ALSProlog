---
title: 'name/2'
group: Terms
predicates:
- {sig: 'name/2', desc: 'converts strings to atoms and atoms to strings'}
---
`name/2` — converts strings to atoms and atoms to strings

## FORMS
```
name(Constant, PrintName)
```
## DESCRIPTION

When `Constant` is instantiated to an atom or a number, `PrintName` is unified with a list of ASCII codes that correspond to the printed representation of `Constant`. When `PrintName` is a list of ASCII codes, `Constant` will be unified with the atom or number whose printed representation is the string `PrintName`.

## EXAMPLES
```
?- name( Symbol, [0'a,0'l,0'i,0'e,0'n,0's] ).

Symbol = aliens

yes.

? - name(aliens, "aliens" ).

yes.

? - name([], "[]" ).

yes.

? - name(2018, X).

X = [50,48,49,56]

yes.

? - name(X, "2018").

X = 2018 	% Note that 2018 is an integer, not a symbol

yes.
```
## NOTES

We recommend the use of `atom_chars/2` and `number_chars/2` instead of `name/2`.


## SEE ALSO

- `atom_chars/2`  
`atom_codes/2`  
`number_chars/2`  
`number_codes/2`  
`term_chars/2`  
`term_codes/2`

- `User Guide (Syntax of ALS Prolog)`  
- [Bowen 91, 7.8]  
- [Clocksin 81, 6.5]  
- [Bratko 86, 6.4]  
- [Sterling 86, 12.1]

