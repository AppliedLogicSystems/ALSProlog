---
title: 'system/1'
predicates:
- {sig: 'system/1', desc: 'Executes the specified OS shell command'}
---

## FORMS

```
system(Command)
```

## DESCRIPTION

`system(Command)` calls the operating system shell with the string `Command` as its argument, where `Command` is either an atom or a string. For example, on Unix,

```
? - system('rm myfile.pro').
```
will delete the file `myfile.pro` from the current directory.


## EXAMPLES

```
?- system('pwd').
/usr/elvis/u/chris
yes.

?- system('ls').
RandomNotes calendar junkbox mbox public
amber doc kermrc papers test
bin graphics mail prolog tools
yes.

?- system('alspro').
ALS-Prolog Version 1.0
Copyright (c) 1987, 1988 Applied Logic Systems
?- ^D
yes.
%% The last 'yes' was printed by the original ALS Prolog image.
```
