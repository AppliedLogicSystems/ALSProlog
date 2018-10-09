---
title: 'chdir/1'
predicates:
 - 'chdir/1' : changes the current directory to the specified directory
---
`chdir/1` `--` changes the current directory to the specified directory


## FORMS

chdir(DirName)


## DESCRIPTION

DirName can be a symbol, a UIA, or a list of ASCII characters describing a valid directory. The current directory is changed to the specified directory. If the current directory cannot be changed to the given directory for any reason, this predicate fails.


## EXAMPLES

? - chdir(' ../foobar ') .


yes.

