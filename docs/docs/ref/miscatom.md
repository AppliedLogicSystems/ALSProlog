---
title: 'catenate/3'
package: ALS Library
group: Atoms
predicates:
- {sig: 'cat_together_seplines/2', desc: 'convert list of atoms to single atom with eoln separating atom entries'}
- {sig: 'cat_together_spaced/2', desc: 'convert list of atoms to single atom with space separating atom entries'}
- {sig: 'catenate', args: {
    2: 'catenates a list of atoms',
    3: 'catenates two atoms to produce a third'
  }}
- {sig: 'prefix_dir/3', desc: 'prefix Dir to each (atomic) item on List'}
- {sig: 'prefix_to/3', desc: 'catenate Atom to the front of each element on a List of atoms'}
- {sig: 'strip_prefix/3', desc: 'strip a fixed-length prefix from a list of atoms'}
- {sig: 'trim_atoms/3', desc: 'truncates a list of atoms'}
---
## FORMS

`cat_together_seplines(List, Result)`

`cat_together_spaced(List, Result)`

`catenate(ListOfAtoms, Result)`

`catenate(Atom1, Atom2, Atom3)`

`prefix_dir(List, Dir, XList)`

`prefix_to(List, Atom, XList)`

`strip_prefix(List, Prefix, Result)`

`trim_atoms(InAtoms,Sizes,OutNames)`

## DESCRIPTION

**`cat_together_seplines/2`** If List is a list of atoms, then Result is obtained by interspersing  
    end-of-line character(s) [\n for Linux,MacOS and \r\n for Windows]  
    between each pair of atoms, and concatentating the result to  
    obtain a single atom.  

**`cat_together_spaced/2`** If List is a list of atoms, then Result is obtained by interspersing  
    a blank space between each pair of atoms, and concatentating the  
    result to obtain a single atom.  

**`catenate/2`** If ListOfAtoms is a list of atoms, then Result is that atom  
    whose characters consist of the characters of the atoms on  
    ListOfAtoms, in order.  

**`catenate/3`** If Atom1 and Atom2 are atoms, then Atom3 is that atom whose  
    characters consist of those of Atom1 followed by Atom2.  

**`prefix_dir/3`** If List is a list of atoms representing file names, and if  
    Dir is an atom representing a path in the filesystem, then  
    XList is that list obtained from list by combining Dir  
    successively with each element of List to create a path  
    terminating in that List element.  

**`prefix_to/3`** If List is a list of atoms, and Atom is also an atom, then  
    XList will be that list made up by concatenating Atom to the  
    beginning of every atom on List.  

**`strip_prefix/3`** List is a list of atoms, NN is an natural number (the length  
    of the prefix to strip), and Result is that list  
    of atoms which is obtained by obtained by removing the initial  
    NN characters of each element of List.  

**`trim_atoms/3`** If InAtoms is a list of atoms and Sizes is a list of integers (of  
    the same length as list InAtoms), then OutNames is a list of atoms  
    whose nth element is the truncation of the nth element of InAtoms  
    to be at most length K, where K is the nth element of Sizes.  

## EXAMPLES

**`cat_together_seplines/2`**
```
?- List = [a,b,c,d], als_system(SystemList),  
?_ dmember(os = OS, SystemList), dmember(os_variation = OSVar, SystemList),  
?_ ((OS = mswin32 ; OSVar = cygwin32) ->  
?_	TgtResult = 'a\r\nb\r\nc\r\nd\r'  
?_	;  
?_	TgtResult = 'a\nb\nc\nd\n'  
?_ ),  
?_ cat_together_seplines(List, Result).  
Result == TgtResult  
```

**`cat_together_spaced/2`**
```
?- List = [a,b,c,d], cat_together_spaced(List, Result).  
Result == 'a b c d '  
```

**`catenate/2`**
```
?- catenate([abc, def, ghty], Result).  
Result == abcdefghty  
```

**`catenate/3`**
```
?- catenate(abc, def, X).  
X=abcdef  
```

**`prefix_dir/3`**
```
?- List = [foo, file3, bar], Dir = zipper,  
?_ prefix_dir(List, Dir, XList).  
XList == ['zipper/foo','zipper/file3','zipper/bar']  
```

**`prefix_to/3`**
```
?- List = [a1,b2,c3], Atom = 'Zip_',  
?_ prefix_to(List, Atom, XList).  
XList == ['Zip_a1','Zip_b2','Zip_c3']  
```

**`strip_prefix/3`**
```
?- List = [abcd, foobar, pop, f, zeroes], NN = 3,  
?_ strip_prefix(List, NN, Result).  
Result == [d,bar,'','',oes]  
```

**`trim_atoms/3`**
```
?- InAtoms = ['Abcd', gh768, bkdjfng, fr4], Sizes = [2,3,4,5],  
?_ trim_atoms(InAtoms, Sizes, Results).  
% note that the truncation of gh768 is an atom:  
Results == [cd,'68',fng,'']  
```

