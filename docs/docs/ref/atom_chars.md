---
title: 'atom_chars/2'
group: Terms
iso: atomcodes
predicates:
- {sig: 'atom_chars/2', desc: 'convert between atoms and the list of characters representing the atom'}
- {sig: 'atom_codes/2', desc: 'convert between atoms and the list of character codes representing the atom'}
---

## FORMS
```
atom_chars(Atom, CharList)

atom_codes(Atom, CodeList)
```
## DESCRIPTION

`atom_chars(Atom, CharList)` is true if and only if `CharList` is a character list whose elements correspond to the characters of the atom `Atom`.

`atom_codes(Atom, CodeList)` is true if and only if `CodeList` is a character code list whose elements correspond to the character codes of the atom `Atom`.


## EXAMPLES

```
?- atom_chars('the cat in',L).
L=[t,h,e,'',c,a,t,'',i,n]
yes.
```

```
?- atom_chars(A,[t,h,e,' ',h,a,t,'\n']).
A='the hat\n'
yes.
```

```
?- atom_codes(A,[65,66,67]).
A='ABC'
yes.
```

```
?- atom_codes(holiday,L).
L="holiday"
yes.
```



## ERRORS

Atom and CharList are both variables(`atom_chars/2`)

-- -- -- -- &gt; instantiation_error.

Atom and CodeList are both variables(`atom_codes/2`)

-- -- -- -- &gt; instantiation_error.

Atom is neither a variable nor an atom

-- -- -- -- &gt; type_error(atom, Atom).

CharList is neither a variable nor a list nor a partial list

-- -- -- -- &gt; type_error(list, CharList).

CodeList is neither a variable nor a list nor a partial list

-- -- -- -- &gt; type_error(list, CodeList).

CharList is a list but there is a sublist L of CharList whose first element is neither a variable nor a character

-- -- -- -- &gt; domain_error(character_list, L).

CodeList is a list but there is a sublist L of CodeList whose first element is neither a variable nor a character code

-- -- -- -- &gt; domain_error(character_code_list, L).


## SEE ALSO

- [`number_chars/2`](number_chars.html)
- [`number_codes/2`](number_chars.html)
- [`term_chars/2`](term_chars.html)
- [`term_codes/2`](term_chars.html)
