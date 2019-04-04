---
title: 'chdir/1'
group: Input Output
module: user
predicates:
- {sig: 'chdir/1', desc: 'changes the current directory to the specified directory'}
---

## FORMS
```
chdir(DirName)
```
## DESCRIPTION

`DirName` can be a symbol, a `UIA`, or a list of ASCII characters describing a valid directory. The current directory is changed to the specified directory. If the current directory cannot be changed to the given directory for any reason, this predicate fails.

## EXAMPLES
```
?- chdir('../foobar').

yes.
```
