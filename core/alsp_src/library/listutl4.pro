/*======================================================================
 |			listutl4.pro
 |	Copyright (c) 1996-2019 Applied Logic Systems, Inc.
 |		Group: Lists
 |		DocTitle: split_ll_by_blank/3
 |		-- Miscellaneous list predicates
 *=====================================================================*/
module builtins.

export split_ll_at_start/5.
export split_ll_by_blank/3.

/*!---------------------------------------------------------------------
 |	split_ll_at_start/5
 |	split_ll_at_start(Lines, SplitInitSeg, Head, Tail, SplitterLine)
 |	split_ll_at_start(+, +, -, -, -)
 |
 |	- splits a list of atoms according to an initial subatom
 |
 |	If Lines is a list of atoms (or UIAs), and if SplitInitSeg is an atom, 
 |	then, if there is any element of Lines with SplitInitSeg at its initial segment, 
 |	then:<br>
 |		SplitterLine is the first such line;<br>
 |		Head is the initial sublist of Lines up to but not including SplitterLine;<br>
 |		Tail is the sublist of Lines following SplitterLine<br>
 |	If there is no such line, then:<br>
 |		SplitterLine = ''<br>
 |		Head = Lines<br>
 |		Tail = []
 *!--------------------------------------------------------------------*/

split_ll_at_start([], _, [], [], '').

split_ll_at_start([Line | Tail], Init, [], Tail, Line)
	:-
	sub_atom(Line, 0, _, _, Init),
	!.

split_ll_at_start([Line | Lines], Init, [Line | Head], Tail, Splitter)
	:-
	split_ll_at_start(Lines, Init, Head, Tail, Splitter).

/*!---------------------------------------------------------------------
 |	split_ll_by_blank/3
 |	split_ll_by_blank(Lines, Head, Tail)
 |	split_ll_by_blank(+, -, -)
 |
 |	- splits a list of atoms by the first null atom
 |
 |	If Lines is a list of atoms (or UIAs), then, if '' belongs to Lines, 
 |	then:<br>
 |		Head is the initial sublist of Lines up to the first occurrence of '';<br>
 |		Tail is the sublist of Lines following Head<br>
 |	If there is no such line, then:<br>
 |		Head = Lines<br>
 |		Tail = []
 *!--------------------------------------------------------------------*/

split_ll_by_blank([], [], []).

split_ll_by_blank(['' | Tail], [], Tail)
	:-!.

split_ll_by_blank([Line | Lines], [Line | Head], Tail)
	:-
	split_ll_by_blank(Lines, Head, Tail).

endmod.
