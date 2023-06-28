---
title: 'catenate/3'
package: ALS Library
group: Atoms
predicates:
- {sig: 'catenate', args: {
    2: 'catenates a list of atoms',
    3: 'catenates two atoms, or an atom and number, to produce a third atom'
  }}
- {sig: 'trim_atoms/3', desc: 'truncates a list of atoms'}
- {sig: 'cat_together_seplines/2', desc: 'convert list of atoms to single atom with eoln separating atom entries'}
- {sig: 'cat_together_spaced/2', desc: 'convert list of atoms to single atom with space separating atom entries'}
- {sig: 'prefix_to/3', desc: 'catenate Atom to the front of each element on a List of atoms'}
- {sig: 'prefix_dir/3', desc: 'prefix Dir to each (atomic) item on List'}
- {sig: 'strip_prefix/3', desc: 'strip a fixed-length prefix from a list of atoms'}
---
## FORMS

`catenate(Atom1, Item2, Atom3)`

`catenate(ListOfAtoms, Result)`

`trim_atoms(InAtoms,Sizes,OutNames)`

`cat_together_seplines(List, Result)`

`cat_together_spaced(List, Result)`

`prefix_to(List, Atom, XList)`

`prefix_dir(List, Dir, XList)`

`strip_prefix(List, Prefix, Result)`

## DESCRIPTION

**`catenate/3`** If Atom1 is an atom, and Item2 is an atom or an integer, then Atom3
    is that atom whose characters consist of those of Atom1 followed
    by those of Item2.

**`catenate/2`** If ListOfAtoms is a list of atoms, then Result is that atom
    whose characters consist of the characters of the atoms on
    ListOfAtoms, in order.

**`trim_atoms/3`** If InAtoms is a list of atoms and Sizes is a list of integers (of
    the same length as list InAtoms), then OutNames is a list of atoms
    whose nth element is the truncation of the nth element of InAtoms
    to be at most length K, where K is the nth element of Sizes.

**`cat_together_seplines/2`** If List is a list of atoms, then Result is obtained by interspersing
    end-of-line character(s) [\n for Linux,MacOS and \r\n for Windows]
    between each pair of atoms, and concatentating the result to
    obtain a single atom.

**`cat_together_spaced/2`** If List is a list of atoms, then Result is obtained by interspersing
    a blank space between each pair of atoms, and concatentating the
    result to obtain a single atom.

**`prefix_to/3`** If List is a list of atoms, and Atom is also an atom, then
    XList will be that list made up by concatenating Atom to the
    beginning of every atom on List.

**`prefix_dir/3`** If List is a list of atoms representing file names, and if
    Dir is an atom representing a path in the filesystem, then
    XList is that list obtained from list by combining Dir
    successively with each element of List to create a path
    terminating in that List element.

**`strip_prefix/3`** List is a list of atoms, NN is an natural number (the length
    of the prefix to strip), and Result is that list
    of atoms which is obtained by obtained by removing the initial
    NN characters of each element of List.

## EXAMPLES

**`catenate/3`**
```
?- catenate(abc, def, X).
X=abcdef
?- catenate(abc, 49, X).
X=abc49
```

**`catenate/2`**
```
?- catenate([abc, def, ghty], Result).
Result == abcdefghty
```

**`trim_atoms/3`**
```
?- InAtoms = ['Abcd', gh768, bkdjfng, fr4], Sizes = [2,3,4,5],
   trim_atoms(InAtoms, Sizes, Results).
% note that the truncation of gh768 is an atom:
Results == [cd,'68',fng,'']
```

**`cat_together_seplines/2`**
```
?- List = [a,b,c,d], als_system(SystemList),
   dmember(os = OS, SystemList), dmember(os_variation = OSVar, SystemList),
   ((OS = mswin32 ; OSVar = cygwin32) ->
        TgtResult = 'a\r\nb\r\nc\r\nd\r'
        ;
        TgtResult = 'a\nb\nc\nd\n'
   ),
   cat_together_seplines(List, Result).
Result == TgtResult
```

**`cat_together_spaced/2`**
```
?- List = [a,b,c,d], cat_together_spaced(List, Result).
Result == 'a b c d '
```

**`prefix_to/3`**
```
?- List = [a1,b2,c3], Atom = 'Zip_',
   prefix_to(List, Atom, XList).
XList == ['Zip_a1','Zip_b2','Zip_c3']
```

**`prefix_dir/3`**
```
?- List = [foo, file3, bar], Dir = zipper,
   prefix_dir(List, Dir, XList).
XList == ['zipper/foo','zipper/file3','zipper/bar']
```

**`strip_prefix/3`**
```
?- List = [abcd, foobar, pop, f, zeroes], NN = 3,
   strip_prefix(List, NN, Result).
Result == [d,bar,'','',oes]
```

