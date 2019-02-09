---
title: 'exists_file/1'
predicates:
- {sig: 'exists_file/1', desc: 'tests whether a file exists'}
---

## FORMS
```
exists_file(Filename)
```
## DESCRIPTION

`Filename` is an atom representing a file name, or a path name (possibly to a directory). `exists_file/1` succeeds if the indicated file or directory actually exists, and fails otherwise.

## EXAMPLES
```
?-exists_file('foo.pro').

yes.

?-exists_file('../mydir/test.pro').

yes.
```

