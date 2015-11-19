Like most symbolic programming langauges, ALS Prolog implements atoms in
two different ways:
* Atoms can be interned which means that they have been installed in the Prolog symbol table. Atoms which have been interned are called symbols.
* Atoms can also be uninterned which means that they have not been installed
in the symbol table. These atoms are called UIAs (UnInterned Atoms).
Because UIAs are stored on the heap, they are efficiently garbage collectable. Ordinary Prolog programs cannot distinguish between interned and uninterned atoms,
except for possible differences in efficiency. However, programs which must interface to other external programs can sometimes find UIAs very useful.

##7.1 The Efficiency of UIAs

Symbols are entered in the symbol table only once, so comparison between atoms
which are symbols is very fast. This is because only the symbol table indices need
to be compared. In contrast, UIAs are stored on the heap as the sequence of characters in the atom’s print name (together with header/footer information). Comparison of a symbol with a UIA, or a UIA with a UIA, is somewhat slower because
the two atoms must be compared by comparing the characters in their print names.
Thus, it is desirable to store atoms as symbols if they are likely to often be compared with other atoms. This includes the functors of structures and the distinguished program constants such as ’:-’ or ’+’, etc. On the other hand, many programs contain atoms which are seldom or never compared with other atoms. Prompt messages and other output strings are good examples, as are atoms read when
searching a file. These objects should usually be stored as UIAs to avoid clogging
up the symbol table.

###7.1.1 When is a UIA created?

ALS Prolog uses the following rules to decide whether a given occurrence of an
atom should be a symbol or a UIA.

1. All functors, operators, and predicate names are put into the symbol table.
2. Atoms appearing in the text without single quotes are put in the symbol table.
3. Atoms appearing in the text enclosed in single quotes are stored as UIAs unless
the string which forms the atom is already in the symbol table, or unless the first
rule applies.
4. Atoms created by name/2 are UIAs unless the string which forms the atom is
already in the symbol table.

Consider the following clauses:
````
p(’x’,y) :- q(’f’,’x’).
p(f(y),’wombat’).
p(x,’wombat’).
````
Let us assume that none of p, q, x, y, f, or wombat are initially in the symbol table
when these clauses are first read. Both p and q will be put into the symbol table
because they are predicate names. y will also be put into the symbol table because
it does not appear between single quotes. On the other hand, wombat will be
stored as a UIA becasue it is surrounded by single quotes. Similarly, x and f will
initially start out as UIAs because they appear in single quotes. But both of them
will eventually be entered into the symbol table because f appears as a functor in
the second clause and x appears unquoted in the third clause.

The rationale behind making atoms which are enclosed in single quotes into UIAs
is that these sort of atoms most often appear as filenames or messages to write out.
As such, they are rarely compared with other atoms.

##7.2 Interning UIAs

It is sometimes desirable, under direct program control, to intern an atom which
was originally stored as a UIA. This will cause all future occurrences of the atom,
whether read by the parser or processed by name/2, to be turned into symbols.
This is accomplished by using functor/3. Suppose that the constant ’ProgramConstant’ should be interned. This atom cannot be written in a program text without enclosing it in single quotes, because otherwise it would be read as a
variable. The way to turn this into a constant is to issue the goal:

    functor(_,’ProgramConstant’,0).

If a large number of constants need to be interned, it may be desirable to write an
intern predicate which might take the following form.
````
intern(X) :- atom(X), !, functor(_,X,0).
intern([H|T]) :- intern(H), intern(T).
````
This could then be called in the following manner:

intern([ ’ProgramConstant’, ’AnotherConstant’, ’YetAnotherConstant’ ]).

All three quoted strings will be interned so that later occurrences will be stored as
symbols. The PI_forceuia() function can also be used to intern UIAs. See
PI_forceui in the Foreign Interface Reference.


