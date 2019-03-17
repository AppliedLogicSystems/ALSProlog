---
title: 'name/2'
group: Terms
predicates:
- {sig: 'name/2', desc: 'converts strings to atoms and atoms to strings'}
---

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

We recommend the use of [`atom_chars/2`](atom_chars.html) and [`number_chars/2`](number_chars.html) instead of `name/2`.


## SEE ALSO

- [`atom_chars/2`](atom_chars.html)
- [`atom_codes/2`](atom_chars.html)
- [`number_chars/2`](number_chars.html)
- [`number_codes/2`](number_chars.html)
- [`term_chars/2`](term_chars.html)
- [`term_codes/2`](term_chars.html)

- [User Guide (Syntax of ALS Prolog)](../guide/1-The-Syntax-of-ALS-Prolog.md)
- {% include book.md id="bowen91"    sec="7.8" %}
- {% include book.md id="clocksin81" sec="6.5" %}
- {% include book.md id="bratko86"   sec="6.4" %}
- {% include book.md id="sterling86" sec="12.1" %}

