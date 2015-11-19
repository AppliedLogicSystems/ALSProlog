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