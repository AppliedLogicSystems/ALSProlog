/*========================================================================*
 |						strings.pro
 |		Copyright (c) 1988-91 Applied Logic Systems, Inc.
 |
 |					Prolog String and UIA utilities
 |
 |	Authors:	Keith Hughes, Ken Bowen, Kevin Buettner
 |	Date:		1986-91
 *========================================================================*/
%! category(terms).
%! group(list).
%! group(atom).
%! subgroup(string).

module builtins.

export asplit/4.
export head/4.
export alower/2.
export asub/4.
export asplit0/4.
export asplit00/4.
export head0/4.
export alower0/2.
export insert_spaces/2.

export catenate/2.
export catenate/3.
export string_to_uia/2.
export cnvrt_to_UIA/2.
export string_to_uia/3.
export sized_string_to_uia/3.
export copy_to_uia/3.
export atomic_to_uia/2.
export make_uc/2.
export make_lc/2.
export make_uc_sym/2.
export make_lc_sym/2.
export change_case_sym/2.
export convert_to_uc/2.
export same_uc/2.
export truncate/3.
export strip_tail_white/2.
export strip_white/2.

export read_to/5.
export read_to_blank/3.

/*!---------------------------------------------------------------------
 |	asplit/4
 |	asplit(Atom,Splitter,LeftPart,RightPart) 
 |	asplit(+,+,-,-) 
 |
 |	- divides an atom as determined by a character
 |
 |	If Atom is any atom or UIA, and if Splitter is the character code of
 |	of a character, then, if the character with code Splitter occurs in
 |	Atom, LeftPart is an atom consisting of that part of Atom from the
 |	left up to and including the leftmost occurrence of the character
 |	with code Splitter, and RightPart is the atom consisting of that
 |	part of Atom extending from immediately after the end of LeftPart
 |	to the end of Atom.
 *!--------------------------------------------------------------------*/
asplit(Atom,Splitter,Part1,Part2) 
	:-
	name(Atom,List),
	asplit0(List,Splitter,String1,String2),
	name(Part1,String1),
	name(Part2,String2).

/*!---------------------------------------------------------------------
 |	head/4
 |	head(Atom,Splitter,Head,Tail)
 |	head(+,+,-,-)
 |
 |	- splits an list into segments determined by a character code
 |
 |	If Atom is a list of character codes, splits Atom into Head and
 |	tail the way asplit would, using the first occurrence of Splitter;
 |	on successive retrys, usings the succeeding occurrences of Spliter
 |	as the split point.
 *!--------------------------------------------------------------------*/
head(Atom,Splitter,Head,Tail)
	:-
	name(Atom,List),
	head0(List,Splitter,H0,T0),
	name(Head,H0),
	name(Tail,T0).


/*!---------------------------------------------------------------------
 |	asplit0/4
 |	asplit0(AtomCs,Splitter,LeftPartCs,RightPartCs) 
 |	asplit0(+,+,-,-) 
 |
 |	- divides a list of character codes as determined by a character code
 |
 |	If AtomCs is a list of character codes, and if Splitter is the character 
 |	code of of a character, then, if the character with code Splitter occurs in
 |	AtomCs, LeftPart is the list consisting of that part of AtomCs from the
 |	left up to and including the leftmost occurrence of Splitter,
 |	and RightPart is the atom consisting of that part of AtomCs extending 
 |	from immediately after the end of LeftPart to the end of AtomCs.
 *!--------------------------------------------------------------------*/
asplit0([Char|Rest],Splitter,[Char|R1],String2) 
	:-
	Char \= Splitter,!,
	asplit0(Rest,Splitter,R1,String2).

asplit0([Splitter|Rest],Splitter,[],Rest).

/*!---------------------------------------------------------------------
 |	asplit00/4
 |	asplit00(AtomCs,SplitList,LeftPartCs,RightPartCs) 
 |	asplit00(+,+,-,-) 
 |
 |	- divides a list of character codes as det. by a list of char codes
 |
 |	If AtomCs is a list of character codes, and if Splitter is the character 
 |	code of of a character, then, if the character with code Splitter occurs in
 |	AtomCs, LeftPart is the list consisting of that part of AtomCs from the
 |	left up to and including the leftmost occurrence of Splitter,
 |	and RightPart is the atom consisting of that part of AtomCs extending 
 |	from immediately after the end of LeftPart to the end of AtomCs.
 *!--------------------------------------------------------------------*/
asplit00([Char|Rest],SplitterList,[],Rest) 
	:-
	dmember(Char, SplitterList), !.

asplit00([Char|Rest],SplitterList,[Char|R1],String2) 
	:-
	asplit00(Rest,SplitterList,R1,String2).

/*!---------------------------------------------------------------------
 |	head0/4
 |	head0(List,Splitter,Head,Tail)
 |	head0(+,+,-,-)
 |
 |	- splits a character code list into segments determined by a code
 |
 |	If List is a list of character codes, splits List into Head and
 |	tail the way asplit0 would, using the first occurrence of Splitter;
 |	on successive retrys, usings the succeeding occurrences of Spliter
 |	as the split point.
 *!--------------------------------------------------------------------*/
head0(List,Splitter,Head,Tail)
	:-
	Flag = -([]),
	( (asplit0(List,Splitter,Head,Tail),mangle(1,Flag,Tail)
	  )
	  ;
	  (arg(1,Flag,NewList), NewList \== [],!,head0(NewList,Splitter,Head,Tail)
	  )
	).
head0(List,Splitter,List,[]).

/*!---------------------------------------------------------------------
 *!--------------------------------------------------------------------*/
alower(Atom,Lower) 
	:-
	name(Atom,String),
	alower0(String,LowString),
	name(Lower,LowString).

/*!---------------------------------------------------------------------
 | alower0/2
 | alower0(List,Result)
 | alower0(+,-)
 |
 | - changes upper case codes to lower case codes
 |
 |	If List is a list of ASCII character codes, Result is identical
 |	to List except that all occurrences of codes of upper case characters
 |	have been changed to the corresponding lower case code.
 *!--------------------------------------------------------------------*/
alower0([],[]) 
	:- !.
alower0([Char|Rest],[Fixed|SRest]) 
	:-
	0'A =< Char, Char =< 0'Z,!,
	Fixed is Char+32,
	alower0(Rest,SRest).
alower0([Char|Rest],[Char|SRest]) 
	:-
	alower0(Rest,SRest).


/*!---------------------------------------------------------------------
 | asub/4
 | asub(Atom,Start,Length,Result) 
 | asub(+,+,+,_) 
 |
 |	- extracts a substring from an atom
 |
 |	Result is the sub-string (atom) of Atom of length Length beginning 
 |	at position Start.
 *!--------------------------------------------------------------------*/
asub(Atom,Start,Length,Result) 
	:-
	name(Atom,String),
	sublist(String,Start,Length,RString),
	name(Result,RString).



/*!---------------------------------------------------------------------
 *!--------------------------------------------------------------------*/
insert_spaces([], []).
insert_spaces([Item | RestIn_List], [Item, ' ' | RestOut_List])
	:-
	insert_spaces(RestIn_List, RestOut_List).


/*!---------------------------------------------------------------------
 *!--------------------------------------------------------------------*/
catenate(Atom1, Atom2, Atom3)
	:-
	name(Atom1, A1Cs),
	name(Atom2, A2Cs),
	append(A1Cs, A2Cs, A3Cs),
	string_to_uia(A3Cs, Atom3).
%	bufread(A3Cs,[Atom3 | _]).


/*!---------------------------------------------------------------------
 *!--------------------------------------------------------------------*/
catenate([], '').
catenate([Atom | Atoms], Result)
	:-
	catenate(Atoms, InterResult),
	catenate(Atom, InterResult, Result).

/*!---------------------------------------------------------------------
 *!--------------------------------------------------------------------*/
string_to_uia([], '') :-!.
string_to_uia(String, UIA)
	:-
	length(String,Size),
	'$uia_alloc'(Size,UIA),
	string_to_uia(String,0,UIA).

/*!---------------------------------------------------------------------
 *!--------------------------------------------------------------------*/
string_to_uia([], _, _).
string_to_uia([Char | Chars], CurPos, Symbol)
   :-
   '$uia_pokeb'(Symbol, CurPos, Char),
   NextPos is CurPos+1,
   string_to_uia(Chars, NextPos, Symbol).

/*!---------------------------------------------------------------------
 *!--------------------------------------------------------------------*/
sized_string_to_uia(Size, Chars, Symbol)
   :-
   '$uia_alloc'(Size, Symbol),
   string_to_uia(Chars, 0, Symbol).

/*!---------------------------------------------------------------------
 *!--------------------------------------------------------------------*/
copy_to_uia([],_,_).
copy_to_uia([H|T],N,Buf) 
	:-
	'$uia_pokeb'(Buf,N,H),
	NN is N+1,
	copy_to_uia(T,NN,Buf).

/*!---------------------------------------------------------------------
 *!--------------------------------------------------------------------*/
atomic_to_uia(Atom, UIABuf)
	:-
	atomic(Atom),  	
	name(Atom,LName),
	length([_|LName],BufLen),
	'$uia_alloc'(BufLen,UIABuf),
	copy_to_uia(LName,0,UIABuf).

/*!---------------------------------------------------------------------
 *!--------------------------------------------------------------------*/
cnvrt_to_UIA(In,Out)
	:-
	bufwrite(Chars,In),
	length(Chars,Size),
	'$uia_alloc'(Size,Out),
	string_to_uia(Chars,0,Out).


/*!---------------------------------------------------------------------
 *!--------------------------------------------------------------------*/
make_uc([], []).
make_uc([C | Cs], [UC | UCs])
	:-
	0'a =< C, C =< 0'z, !, UC is C - 32,
	make_uc(Cs, UCs).
make_uc([C | Cs], [C | UCs])
	:-
	make_uc(Cs, UCs).

/*!---------------------------------------------------------------------
 *!--------------------------------------------------------------------*/
make_lc([], []).
make_lc([C | Cs], [LC | LCs])
	:-
	0'A =< C, C =< 0'Z, !, LC is C + 32,
	make_lc(Cs, LCs).
make_lc([C | Cs], [C | LCs])
	:-
	make_lc(Cs, LCs).

/*!---------------------------------------------------------------------
 *!--------------------------------------------------------------------*/
make_uc_sym(InSym, UCSym)
	:-
	name(InSym, ISCs),
	make_uc(ISCs, UCCs),
	name(UCSym, UCCs).

/*!---------------------------------------------------------------------
 *!--------------------------------------------------------------------*/
make_lc_sym(InSym, LCSym)
	:-
	name(InSym, ISCs),
	make_lc(ISCs, LCCs),
	name(LCSym, LCCs).

/*!---------------------------------------------------------------------
 *!--------------------------------------------------------------------*/
change_case_sym(InSym, OutSym)
	:-
	name(InSym, ISCs),
	ISCs = [C1 | _],
	(C1 < 0'a ->
		make_lc(ISCs, OutCs);
		make_uc(ISCs, OutCs)
	),
	name(OutSym, OutCs).

/*!---------------------------------------------------------------------
 *!--------------------------------------------------------------------*/

convert_to_uc([], []).
convert_to_uc([Item | Items], [UCItem | UCItems])
	:-!,
	convert_to_uc(Item, UCItem),
	convert_to_uc(Items, UCItems).
convert_to_uc(Item, UCItem)
	:-
	atom(Item),!,
	make_uc_sym(Item,UCItem).
convert_to_uc(Item, UCItem)
	:-
	Item =..[Functor | Args],!,
	convert_to_uc(Args, UCArgs),
	UCItem =.. [Functor | UCArgs].
convert_to_uc(Item, Item).

/*!---------------------------------------------------------------------
 *!--------------------------------------------------------------------*/
same_uc(Item1, Item2)
	:-
	convert_to_uc(Item1, UCItem),
	convert_to_uc(Item2, UCItem).

/*!---------------------------------------------------------------------
 *!--------------------------------------------------------------------*/
truncate(InField, MaxSize, OutField)
	:-
	name(InField, IFCs),
	at_most_n(IFCs, MaxSize, TIFCs),
	(number(TIFCs) ->
		name(OutField, TIFCs);
		string_to_uia(TIFCs, OutField)
	).


/*!---------------------------------------------------------------------
 |	strip_white/2
 |	strip_white(String, Result)
 |	strip_white(+, -)
 |
 |	- strips leading white space chars from a prolog string
 *!--------------------------------------------------------------------*/
strip_white([], []).
strip_white([C | Tail], Result)
	:-
	dmember(C, [0' , 0'	]),			% space, tab
	!,
	strip_white(Tail, Result).
strip_white(Tail, Tail).

/*!---------------------------------------------------------------------
 |	strip_tail_white/2
 |	strip_tail_white(String, Result)
 |	strip_tail_white(+, -)
 |
 |	- strips trailing white space chars from a prolog string
 *!--------------------------------------------------------------------*/
strip_tail_white(String, Result)
	:-
	dreverse(String, RString),
	strip_white(RString, RResult),
	dreverse(RResult, Result).


/*!-----------------------------------------------------------------------------
 |	read_to/5.
 |	read_to(Chars, Stoppers, Head, Tail,Stopper)
 |	read_to(+, +, -, -,-)
 |
 |	-splits a string according to one of several possible chars
 |
 |	If:
 |	-Chars is a prolog string;
 |	-Stoppers is a list of codes of chars
 |		and if intersect(Chars,Stoppers) \= [], 
 |	Then:
 |		Stopper is the element of Stopper with leftmost occurrence in Chars,
 |		Head is the portion of Chars to the left of the occurrence of Stopper,
 |	and
 |		Tail is the portion of Chars to the right of the occurrence of Stopper.
 *!----------------------------------------------------------------------------*/
read_to([], _, [], [],-1).

read_to([C | Chars], Stoppers, [], Chars,C)
	:-
	dmember(C, Stoppers),
	!.
			 
read_to([C | Chars], Stoppers, [C | Head], Tail,Stopper)
	:-
	read_to(Chars, Stoppers, Head, Tail,Stopper).

/*!---------------------------------------------------------------------
 |	read_to_blank/3
 |	read_to_blank(Chars, Head, Tail)
 |	read_to_blank(+, -, -)
 |
 |	- splits a string around the leftmost occurrence of blank
 |
 |	If:
 |	-Chars is a prolog string containing at least one blank,
 |	Then:
 |		Head is the portion of Chars to the left of the first occurrence
 |			 of a blank, and
 |		Tail is the portion of Chars to the right of the first occurrence 
 |		of a blank
 *!--------------------------------------------------------------------*/

read_to_blank([], [], []).
 
read_to_blank([0'  | Chars], [], Chars)
	:-!.
	  
read_to_blank([C | Chars], [C | Head], Tail)
	:-
	read_to_blank(Chars, Head, Tail).


endmod.
