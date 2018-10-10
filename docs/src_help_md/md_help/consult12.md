---
title: 'consult/[1,2]'
predicates:
 - 'consult/1' : load a Prolog file
 - 'consult/2' : load a Prolog file, with options
 - 'consultq/1' : load a Prolog file, without messages
 - 'reconsult/1' : load a Prolog file, updating database
---
`consult/1` `--` load a Prolog file

`consult/2` `--` load a Prolog file, with options

`consultq/1` `--` load a Prolog file, without messages

`reconsult/1` `--` load a Prolog file, updating database


## FORMS

consult(FileSpec)

consult(FileSpec, Options)

[File | Files ]

consultq(File)

reconsult(File)

[-File | Files ]


## DESCRIPTION

FileSpec should be instantiated either to an atom that is the name of a file, or to a list of atoms which are names of files. For each File occurring on FileSpec, File is opened and read, any clauses currently in memory which were read and asserted previously from this same File are erased, and the clauses occurring in File are asserted into the database, and the directives occurring in File are executed immediately when it is encountered.

consultq/1 behaves exactly like consult/1, except that printing of normal messages on the terminal is suppressed. [Note, however, that if the File does not exist, an error message will still be printed. ]

reconsult/1 is identical to consult/1 except in the file in question makes it possible to amend a program without having to restart from scratch and consult all the files which make up the program. Both are included for historical reasons, but the former behavior of consult/1(asserting clauses read with no erasures) can only be obtained by using options in consult/2.

consult/1, reconsult/1, and consultq/1 are defined by :

consult(File) :- consult(File, [ ]) .

reconsult(File :- consult(File, [ ]) .

consultq(File) :- consult(File,

[verbosity(quiet) ]) .

If File is the atom user, then clauses and commands will be read in from the keyboard until an end of file is read.

The Options argument of consult/2 is a list of
consult options,

and their effects, are as follows :

erbosity(Value)
Value = quiet/noisy; turns on/off the printing of messages during consulting.

tgtmod(TgtMod)

Any clauses in File which are not explicitly enclosed in module begin/end directives will be asserted to module TgtMod. The default value of TgtMod is user.

search_path(DirPathList)

DirPathList is a list of directory path names. The File(s) to be consulted are searched for on this path. If the current directory is not listed on DirPathList, it is added at the beginning.

strict_search_path(DirPathList)

Like search_path(DirPathList), but the current directory is not added even if not present on DirPathList.

consult(Value)

Value = true / false. If Value = true, previous clauses from the File being consulted are
not

erased, so that the net effect is additive.

ensure_loaded(Value)

Value = true / false. This option has effect only when operating under one of the ALS development shells(the ALS IDE, or the old TTY shell) . If Value = true, and if File has previously been consulted, no action is taken for File. If Value = false, File is consulted.


## EXAMPLES

The following example illustrates the practice of putting calls to consult/1 inside of files to be(re) consulted. We have the three files with contents as follows :

letters.pro

symbol(a) .

symbol(b) .

symbol(c) .

numbers.pro

symbol(' 1 ') .

symbol(' 2 ') .

symbol(' 3 ') .

topfile.pro

symbol(x) .

symbol(y) .

symbol(z) .

:- consult(letters) .

:- consult(numbers) .

The following conversation illustrates the effects of consult and reconsult.

```
?- consult(letters).
Consultingletters...lettersconsulted
yes.
```

```
?- listing.
%user:symbol/1
symbol(a).
symbol(b).
symbol(c).
yes.
```

```
?- reconsult(topfile).
Reconsultingtopfile...
Consultingletters...consulted
Consultingnumbers...consulted
consulted
yes.
```

```
?- listing.
%user:symbol/1
symbol(a).
symbol(b).
symbol(c).
symbol(x).
symbol(y).
symbol(z).
symbol('1').
symbol('2').
symbol('3').
```

## NOTES

reconsult/1 is not compatible with the DEC-10 notion of reconsult. DEC-10 reconsult would, upon seeing a procedure not already seen in a file, wipe out or abolish all clauses for that predicate before adding the new clause in question.

The present semantics of reconsult/1 are that all clauses which were previously defined in a file are wiped out with new clauses replacing(positionally in the procedure) any old clauses. Thus, a procedure that is defined by several files will not be entirely wiped out when reconsult/1 is invoked on just one of the files -- only those clauses defined by the one file are wiped out and subsequently replaced.

The file
user

is special. When
user

is consulted or reconsulted, the input clauses will be taken from the user ' s terminal. The end-of-file character(often Control-D or Contol-Z) should be used to terminate the consultation and return to the shell.


## SEE ALSO

- [Bratko 86, 6.5 ]
- [Clocksin 81, 6.1 ]

