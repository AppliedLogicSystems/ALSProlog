/*========================================================================*
 |			miscatom.pro
 |	Copyright (c) 1995-96 Applied Logic Systems, Inc.
 |		Group: Terms
 |		DocTitle: list_diff/3
 |
 |		Prolog Atom utilities
 |
 |	Author:	Ken Bowen
 |	Date:		1995
 *========================================================================*/
module builtins.

export atm_tail/3.
export trim_atoms/3.

/*!---------------------------------------------------------------------
 |	atm_tail/3
 |	atm_tail(Atom,N,AtmTail)
 |	atm_tail(+,+,-)
 |
 |	- returns the tail of an atom following a given position
 |
 |	If Atom is an atom of length > N, then AtmTail is the subatom of Atom
 |	beginning at postion N+1 and extending to the end of Atom; if the
 |	length of Atom is =< N, AtmTail = ''.
 *!--------------------------------------------------------------------*/

atm_tail(Atom,N,AtmTail)
	:-
	atom_length(Atom,Len),
	Len > N,
	!,
	sub_atom(Atom,N,_,0, AtmTail).
							 
atm_tail(_,_,'').


/*!---------------------------------------------------------------------
 |	trim_atoms/3
 |	trim_atoms(InNames,Sizes,OutNames)
 |	trim_atoms(+,+,-)
 |
 |	-truncates a list of atoms
 |
 |	If InNames is a list of atoms and Sizes is a list of integers (of
 |	the same length as list InNames), then OutNames is a list of atoms
 |	whose nth element is the truncation of the nth element of InNames
 |	to be at most length K, where K is the nth element of Sizes.
 *!--------------------------------------------------------------------*/

trim_atoms([],[],[]).
trim_atoms([InName | InNames],[NameSize | Sizes],[OutName | OutNames])
	:-
	atom_length(InName,Len),
	(Len =< NameSize ->
	 	OutName = InName
		;
		name(InName, InCs),
		at_most_n(InCs, NameSize, OutCs),
		name(OutName, OutCs)
	),
	trim_atoms(InNames,Sizes,OutNames).

/*!-----------------------------------------------------------------------
 |	cat_together_seplines/2
 |	cat_together_seplines(List, Result)
 |	cat_together_seplines(+, -)
 |
 |	- convert list of atoms to single atom with nl separating atom entries
 *-----------------------------------------------------------------------*/
cat_together_seplines([], '').
cat_together_seplines([Item | Rest], Result)
	:-
	cat_together_seplines(Rest, RestResult),
	catenate([Item, '\n', RestResult], Result).

/*!-----------------------------------------------------------------------
 |	cat_together_spaced/2
 |	cat_together_spaced(Rest, RestResult),
 |	cat_together_spaced(+, -),
 |
 |	- convert list of atoms to single atom with space separating atom entries
 *-----------------------------------------------------------------------*/
cat_together_spaced([], '').
cat_together_spaced([Item | Rest], Result)
	:-
	cat_together_spaced(Rest, RestResult),
	catenate([Item, ' ', RestResult], Result).

endmod.
