---
title: 'name/2'
predicates:
 - 'name/2' : converts strings to atoms and atoms to strings
---
`name/2` `--` converts strings to atoms and atoms to strings


## FORMS

name(Constant, PrintName)


## DESCRIPTION

When Constant is instantiated to an atom or a number, PrintName is unified with a list of ASCII codes that correspond to the printed representation of Constant. When PrintName is a list of ASCII codes, Constant will be unified with the atom or number whose printed representation is the string PrintName.

? - name(Symbol, [0 ' a, 0 ' l, 0 ' i, 0 ' e, 0 ' n, 0 ' s ]) .

Symbol = aliens

yes.

? - name(aliens, &quot; aliens &quot;) .


yes.

? - name([ ], &quot; [ ] &quot;) .


yes.

? - name(2018, X) .

X = [50, 48, 49, 56 ]

yes.

? - name(X, &quot; 2018 &quot;) .

X = 2018 % 2018 is an integer, not a symbol

yes.


## NOTES

We recommend the use of atom_chars/2 and number_chars/2 over name/2.


## SEE ALSO

- `atom_chars/2`  
`atom_codes/2`  
`number_chars/2`  
`number_codes/2`  
`term_chars/2`  
`term_codes/2`
- `UserGuide(SyntaxofALSProlog)`  
``  
`[Bowen91`  
`7.8]`  
`[Clocksin81`  
`6.5]`  
`[Bratko86`  
`6.4]`  
`[Sterling86`  
`12.1].

