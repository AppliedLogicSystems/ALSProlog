/*========================================================================*
 |			miscatom.pro
 |	Copyright (c) 1995-2019 Applied Logic Systems, Inc.
 |		Group: Terms
 |		DocTitle: catenate/3
 |		-- Various atom utilities
 *========================================================================*/
module builtins.

export catenate/3.
export catenate/2.
export trim_atoms/3.
export cat_together_seplines/2.
export cat_together_spaced/2.
export strip_prefix/3.
export prefix_dir/3.
export prefix_to/3.


/*!---------------------------------------------------------------------
 |	catenate/3
 |	catenate(Atom1, Atom2, Atom3)
 |	catenate(+, +, -)
 |
 | 	- catenates two atoms to produce a third
 |
 |	If Atom1 and Atom2 are atoms, then Atom3 is that atom whose
 |	characters consist of those of Atom1 followed by Atom2.
 *!--------------------------------------------------------------------*/
catenate(Atom1, Atom2, Atom3)
	:-
	atom_codes(Atom1, A1Cs),
	atom_codes(Atom2, A2Cs),
	append(A1Cs, A2Cs, A3Cs),
	atom_codes(Atom3, A3Cs).

/*!---------------------------------------------------------------------
 |	catenate/2
 |	catenate(ListOfAtoms, Result)
 |	catenate(+, -)
 |
 | 	- catenates a list of atoms 
 |
 |	If ListOfAtoms is a list of atoms, then Result is that atom
 |	whose characters consist of the characters of the atoms on
 |	ListOfAtoms, in order.
 *!--------------------------------------------------------------------*/
catenate([], '').
catenate(ListOfAtoms, Result)
	:-
	list2strings(ListOfAtoms, ListOfAtomStrings),
	append(ListOfAtomStrings, ResultString),
	atom_codes(Result, ResultString).

list2strings([], []).
list2strings([Atom | ListOfAtoms], [AtomString | ListOfAtomStrings])
	:-
	atom_codes(Atom, AtomString),
	list2strings(ListOfAtoms, ListOfAtomStrings).

/*!---------------------------------------------------------------------
 |	trim_atoms/3
 |	trim_atoms(InAtoms,Sizes,OutNames)
 |	trim_atoms(+,+,-)
 |
 |	-truncates a list of atoms
 |
 |	If InAtoms is a list of atoms and Sizes is a list of integers (of
 |	the same length as list InAtoms), then OutNames is a list of atoms
 |	whose nth element is the truncation of the nth element of InAtoms
 |	to be at most length K, where K is the nth element of Sizes.
 *!--------------------------------------------------------------------*/
trim_atoms([],[],[]).
trim_atoms([InAtom | RestInAtoms],[TruncSize | RestSizes],[OutAtom | RestOutAtoms])
	:-
	atom_length(InAtom,Len),
	(TruncSize > Len ->
		OutAtom = ''
		;
		sub_atom(InAtom,TruncSize,_,0,OutAtom)
	),
	trim_atoms(RestInAtoms,RestSizes,RestOutAtoms).

/*!-----------------------------------------------------------------------
 |	cat_together_seplines/2
 |	cat_together_seplines(List, Result)
 |	cat_together_seplines(+, -)
 |
 |	- convert list of atoms to single atom with eoln separating atom entries
 |
 |	If List is a list of atoms, then Result is obtained by interspersing
 |	end-of-line character(s) [\n for Linux,MacOS and \r\n for Windows]
 |	between each pair of atoms, and concatentating the result to
 |	obtain a single atom.
 *-----------------------------------------------------------------------*/
cat_together_seplines(List, Result)
	:-
	als_system(SystemList),
	dmember(os = OS, SystemList),
        dmember(os_variation = OSVar, SystemList),
	((OS = mswin32 ; OSVar = cygwin32) ->
		EOLN = '\r\n'
		;
		EOLN = '\n'
	),
	insert_item_in_list(List, EOLN, L2),
	catenate(L2, Result).

/*!-----------------------------------------------------------------------
 |	cat_together_spaced/2
 |	cat_together_spaced(List, Result)
 |	cat_together_spaced(+, -),
 |
 |	- convert list of atoms to single atom with space separating atom entries
 |
 |	If List is a list of atoms, then Result is obtained by interspersing
 |	a blank space between each pair of atoms, and concatentating the 
 |	result to obtain a single atom.
 *-----------------------------------------------------------------------*/
cat_together_spaced(List, Result)
	:-
	insert_item_in_list(List, ' ', L2),
	catenate(L2, Result).

/*!-----------------------------------------------------------------------
 |	prefix_to/3
 |	prefix_to(List, Atom, XList)
 |	prefix_to(+, +, -)
 |
 |	- catenate Atom to the front of each element on a List of atoms
 |
 |	If List is a list of atoms, and Atom is also an atom, then
 |	XList will be that list made up by concatenating Atom to the
 |	beginning of every atom on List.
 *!-----------------------------------------------------------------------*/
prefix_to([], _, []).
prefix_to([Item | List], Atom, [XItem | XList])
	:-
	catenate(Atom, Item, XItem),
	prefix_to(List, Atom, XList).

/*!-----------------------------------------------------------------------
 |	prefix_dir/3
 |	prefix_dir(List, Dir, XList)
 |	prefix_dir(+, +, -)
 |
 |	- prefix Dir to each (atomic) item on List
 |
 |	If List is a list of atoms representing file names, and if
 |	Dir is an atom representing a path in the filesystem, then
 |	XList is that list obtained from list by combining Dir 
 |	successively with each element of List to create a path
 |	terminating in that List element.
 *-----------------------------------------------------------------------*/
prefix_dir([], _, []).
prefix_dir([Item | RestList], Dir, [XItem | RestXList])
	:-
	path_directory_tail(XItem, Dir, Item),
	prefix_dir(RestList, Dir, RestXList).

/*!---------------------------------------------------------------------
 |	strip_prefix/3
 |	strip_prefix(List, Prefix, Result)
 |	strip_prefix(+, +, -)
 |
 |	- strip a fixed-length prefix from a list of atoms
 |
 |	List is a list of atoms, NN is an natural number (the length
 |	of the prefix to strip), and Result is that list
 |	of atoms which is obtained by obtained by removing the initial
 |	NN characters of each element of List.
 *!--------------------------------------------------------------------*/
strip_prefix([], _, []).
strip_prefix([Item | RestList], NN, [Stripped | RestResult])
	:-
	atom_length(Item, Len),
	Len >= NN,
	!,
	sub_atom(Item, NN,_,0, Stripped),
	strip_prefix(RestList, NN, RestResult).
strip_prefix([Item | RestList], NN, ['' | RestResult])
	:-
	strip_prefix(RestList, NN, RestResult).

endmod.
