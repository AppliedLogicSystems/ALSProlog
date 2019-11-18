---
title: 'sub_atom/5'
group: Terms
iso: subatom
predicates:
- {sig: 'sub_atom/5', desc: 'dissect an atom'}
---

## FORMS

```
sub_atom(Atom, Before, Length, After, SubAtom)
```

## DESCRIPTION

`sub_atom/5` is used to take apart an atom. The only instantiation requirement is that `Atom` be instantiated to an atom. If any of `Before`, `Length`, or `After` are instantiated, they must be instantiated to integers. If `SubAtom` is instantiated, it must be instantiated to an atom.

The `Before` parameter gives the number of characters in `Atom` before the start of the atom `SubAtom`. `Length` is the length of this `SubAtom`. `After` is the number of characters of `Atom` following the end of `SubAtom`. The first character of any atom is considered to begin at position 1.

`sub_atom/5` is resatisfiable. Upon backtracking all possible values of `Before`, `Length`, `After`, and `SubAtom` are generated subject to the initial instantiations of these parameters.

## EXAMPLES

```
?- sub_atom(abcdefg, 2, 3, X, Y).
X=2
Y=cde
yes.
```
```
?- sub_atom(abcdefg, B, L, A, cde).
B=2
L=3
A=2
yes.
```
```
?- sub_atom(abcdefg, B, 4, A, Y).
B=0
A=3
Y=abcd ;
B=1
A=2
Y=bcde ;
B=2
A=1
Y=cdef ;
B=3
A=0
Y=defg ;
no.
```

## ERRORS

`Atom` is a variable

-- -- -- -- > `instantiation_error`

`Atom` is neither a variable nor an atom

-- -- -- -- > `type_error(atom, Atom)`

`SubAtom` is neither a variable nor an atom

-- -- -- -- > `type_error(atom, SubAtom)`

`Start` is neither a variable nor an integer

-- -- -- -- > `type_error(integer, Start)`

`Length` is neither a variable nor an integer

-- -- -- -- > `type_error(integer, Length)`


## SEE ALSO

- `atom_length/2` {%- comment %} TODO: missing {% endcomment %}
- `atom_concat/3` {%- comment %} TODO: missing {% endcomment %}
- [`atom_chars/2`](atom_chars.html)
- [`atom_codes/2`](atom_chars.html)
- [User Guide (Prolog I/O)](../guide/10-Prolog-I-O.html)
