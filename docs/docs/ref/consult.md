---
title: 'consult/[1,2]'
group: Input Output
predicates:
- {sig: 'consult', args: {
     1: 'load a Prolog file',
     2: 'load a Prolog file, with options'
   }}
- {sig: 'consultq/1', desc: 'load a Prolog file, without messages'}
- {sig: 'reconsult/1', desc: 'load a Prolog file, updating the prolog database'}
---

## FORMS
```
consult(FileSpec)

consult(FileSpec, Options)
```
`[File | Files ]`  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; _Synonym for `consult(FileSpec)` when `FileSpec` is `[File | Files ]`_
```
consultq(File)

reconsult(File)
```
`[-File | Files ]`  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; _Equivalent to a sequence of reconsult/consult calls, corresponding to the appearance of - or no -._

## DESCRIPTION

`FileSpec` should be instantiated either to an atom that is the name of a file, or to a list of atoms which are names of files. For each `File` occurring on `FileSpec`, 

1. `File` is opened and read, 
2. Any clauses currently in memory which were read and asserted previously _from this same `File`_ are erased, 
3. The clauses occurring in `File` are asserted into the database, 
4. Any directives occurring in `File` are executed immediately when encountered.

`consultq/1` behaves exactly like `consult/1`, except that printing of normal `consult/1` messages on the terminal is suppressed. [Note, however, that if the `File` does not exist, an error message will still be printed.]

`reconsult/1` is identical to `consult/1` and is preserved for historical consistency.

`consult/1`, `reconsult/1`, and `consultq/1` are defined by :

```
consult(File) :- consult(File, [ ]) .

reconsult(File :- consult(File, [ ]) .

consultq(File) :- consult(File, [verbosity(quiet) ]) .
```
If File is the atom user, then clauses and commands will be read in from the keyboard until an end of file is read.

The Options argument of `consult/2` is a list of consult options, and their effects, are as follows :

`verbosity(Value)`
<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`Value = noisy/quiet`; turns on/off the printing of messages during consulting.

`tgtmod(TgtMod)`
<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Any clauses in `File` which are not explicitly enclosed in module begin/end directives will be asserted to module `TgtMod`. The default value of `TgtMod` is `user`.

`search_path(DirPathList)`
<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`DirPathList` is a list of directory path names. The `File(s)` to be consulted are searched for on this path. If the current directory is not listed on `DirPathList`, it is added at the beginning.

`strict_search_path(DirPathList)`
<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Like `search_path(DirPathList)`, but the current directory is not added even if not present on `DirPathList`.

`consult(Value)`
<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`Value = true/false`. If `Value` = true, previous clauses from the `File` being consulted are not erased, so that the net effect is additive.

`ensure_loaded(Value)`
<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`Value = true/false`. This option has effect only when operating under one of the ALS development shells(ALS IDE or the TTY shell). If `Value = true`, and if `File` has previously been consulted, no action is taken for `File`. If `Value = false`, `File` is consulted.


## EXAMPLES
Let the file `fruit.pro` contain:
```
fruit(apple).
fruit(X) :- nut(X).
fruit(orange).
nut(hazelnut).
```
Then:
```
?- consult(fruit).

yes.
?- listing.

% user:fruit/1
fruit(apple).
fruit(_A) :- nut(_A).
fruit(orange).

% user:nut/1
nut(hazelnut).

yes.
```
Now, using a separate editor process, edit `fruit.pro` so that it contains:
```
fruit(apple).
fruit(X) :- nut(X).
fruit(raspberry).
nut(hazelnut).
nut(acorn).
```
Now, consult `fruit.pro` again:
```
?- consult(fruit).

yes.
?- listing.

% user:fruit/1
fruit(apple).
fruit(_A) :- nut(_A).
fruit(raspberry).

% user:nut/1
nut(hazelnut).
nut(acorn).

yes.
```
The following example illustrates the practice of putting calls to `consult/1` inside of files to be(re) consulted. We have the three files with contents as follows :
```
letters.pro
-----------
symbol(a).
symbol(b).
symbol(c).

numbers.pro
-----------
symbol('1').
symbol('2').
symbol('3').

topfile.pro
-----------
symbol(x).
symbol(y).
symbol(z).

:- consult(letters).
:- consult(numbers).
```
The following conversation illustrates the effect of repetitive consult (or reconsult) with embedded consults:

```
?- consult(letters).

yes.

?- listing.
%user:symbol/1
symbol(a).
symbol(b).
symbol(c).
yes.

?- consult(topfile).

yes.

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

`reconsult/1` is not compatible with the DEC-10 notion of reconsult. DEC-10 reconsult would, upon seeing a procedure not already seen in a file, wipe out or abolish all clauses for that predicate before adding the new clause in question.

The present semantics of `reconsult/1` are that all clauses which were previously defined in a `File` being loaded are wiped out, with new clauses replacing (positionally in the procedure) any old clauses. Thus, a procedure that is defined by several files will not be entirely wiped out when `reconsult/1` is invoked on just one of the procedure's files -- only those clauses defined by the one given `File` are wiped out and subsequently replaced.

The file `user` is special. When `user` is consulted or reconsulted, the input clauses will be taken from the user's terminal. The end-of-file character (often Control-D or Control-Z) should be used to terminate the consultation and return to the shell.


## SEE ALSO

- {% include book.md id="bratko86"   sec="6.5" %}
- {% include book.md id="clocksin81" sec="6.1" %}
