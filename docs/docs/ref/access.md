---
title: '$access/2'
group: Input Output
predicates:
- {sig: '$access/2', desc: 'determine accessibility of a file'}
---

## FORMS

`'$access'(FileName, AccessMode)`

## DESCRIPTION

FileName can be a symbol, a UIA, or a list of ASCII characters. '$access'/2 checks whether or not the given file is accessible with the given mode, where AccessMode should be one of the following :

```
    4 : write
    2 : read
    0 : existence
```

These access modes are used as indicated on Unix systems and the Macintosh. However, they are ignored on DOS systems. On DOS systems, the file is only checked as to whether or not it is read accessible.


## EXAMPLES

```
?- '$access'('../foo/foobar.pl', 2) .

yes.
```
