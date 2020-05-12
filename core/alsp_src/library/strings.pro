/*========================================================================*
 |			strings.pro
 |	Copyright (c) 1988-2019 Applied Logic Systems, Inc.
 |		Group: Strings
 |		DocTitle: make_lc/2
 |		-- String utilities, with related atom/UIA predicates
 *========================================================================*/
module builtins.

export asplit/4.
export head/4.
export head0/4.
export asplit00/4.
export make_lc/2.
export make_uc/2.
export make_lc_sym/2.
export make_uc_sym/2.
export convert_to_uc/2.
export same_uc/2.
export change_case_sym/2.
export string_to_uia/2.
export string_to_uia/3.
export string_to_sized_uia/3.
export atomic_to_uia/2.
export cnvrt_to_UIA/2.
export truncate/3.
export strip_tail_white/2.
export strip_white/2.
export strip_both_white/2.
export strip_both_white_atom/2.
export read_to/5.
export read_to_blank/3.
export char_in/3.
export replace_char_atom/4.
export replace_char_string/4.

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
 |	left up to but not including the leftmost occurrence of the character
 |	with code Splitter, and RightPart is the atom consisting of that
 |	part of Atom extending from immediately after the occurrence of 
 |	the character with code Splitter, to the end of Atom.
 *!--------------------------------------------------------------------*/
asplit(Atom,Splitter,Part1,Part2) 
	:-
	atom_codes(Atom,List),
	asplit0(List,Splitter,String1,String2),
	atom_codes(Part1,String1),
	atom_codes(Part2,String2).

/*!---------------------------------------------------------------------
 |	head/4
 |	head(Atom,Splitter,Head,Tail)
 |	head(+,+,-,-)
 |
 |	- splits an list into segments determined by a character code
 |
 |	If Atom is a list of character codes, splits Atom into Head and
 |	Tail similar to the way asplit would for the list of chars,, using 
 |	the leftmost occurrence of Splitter. On successive retrys, uses the 
 |	succeeding occurrences of Splitter in Tail as the split point.
 *!--------------------------------------------------------------------*/
head(Atom,Splitter,Head,Tail)
	:-
	atom_codes(Atom,List),
	head0(List,Splitter,H0,T0),
	atom_codes(Head,H0),
	atom_codes(Tail,T0).

/*!---------------------------------------------------------------------
 |	head0/4
 |	head0(List,Splitter,Head,Tail)
 |	head0(+,+,-,-)
 |
 |	- splits a character code list into segments determined by a code
 |
 |	If List is a list of character codes, splits List into Head and
 |	tail the way asplit0 would, using the leftmost occurrence of 
 |	Splitter. On successive retrys, uses the succeeding occurrences 
 |	of Spliter in Tail as the split point, effectively breaking
 |	List into segments determined by Splitter.
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
 |	asplit00/4
 |	asplit00(String,SplitList,LeftPartCs,RightPartCs) 
 |	asplit00(+,+,-,-) 
 |
 |	- divides a list of character codes as det. by a list of char codes
 |
 |	If String and SplitList are both lists of character codes, 
 |	and if any code on SplitList also occurs on String, then LeftPartCs 
 |	consists of all those codes of String up to, but not including, 
 |	the leftmost occurrence of any code CS on SplitList, and RightPartCs 
 |	consist of all codes on String following CS.
 *!--------------------------------------------------------------------*/
asplit00([Char|Rest],SplitterList,[],Rest) 
	:-
	dmember(Char, SplitterList), !.

asplit00([Char|Rest],SplitterList,[Char|R1],String2) 
	:-
	asplit00(Rest,SplitterList,R1,String2).

/*!---------------------------------------------------------------------
 |	make_lc/2
 |	make_lc(Cs, LCs)
 |	make_lc(+, -)
 |
 | 	- converts a Prolog string to all lowercase character (codes)
 |
 |	If Cs is a list of character codes, then LCs is the list of
 |	codes corresponding to Cs, with every uppercase code converted
 |	to the corresponding lowercase code.
 *!--------------------------------------------------------------------*/
make_lc([], []).
make_lc([C | Cs], [LC | LCs])
	:-
	0'A =< C, C =< 0'Z, 
	!, 
	LC is C + 32,
	make_lc(Cs, LCs).
make_lc([C | Cs], [C | LCs])
	:-
	make_lc(Cs, LCs).

/*!---------------------------------------------------------------------
 |	make_uc/2
 |	make_uc(Cs, UCs)
 |	make_uc(+, -)
 |
 | 	- converts a Prolog string to all uppercase character (codes)
 |
 |	If Cs is a list of character codes, then UCs is the list of
 |	codes corresponding to Cs, with every lowercase code converted
 |	to the corresponding uppercase code.
 *!--------------------------------------------------------------------*/
make_uc([], []).
make_uc([C | Cs], [UC | UCs])
	:-
	0'a =< C, C =< 0'z, 
	!, 
	UC is C - 32,
	make_uc(Cs, UCs).
make_uc([C | Cs], [C | UCs])
	:-
	make_uc(Cs, UCs).

/*!---------------------------------------------------------------------
 |	make_lc_sym/2
 |	make_lc_sym(InSym, LCSym)
 |	make_lc_sym(+, -)
 |
 | 	- converts an atom or UIA to all lowercase characters
 |
 |	If InSym is a symbol, then LCSym is that symbol consisting of
 |	the same characters, in order, as InSym, except that all 
 |	uppercase symbols will have been converted to lowercase.
 *!--------------------------------------------------------------------*/
make_lc_sym(InSym, LCSym)
	:-
	atom_codes(InSym, ISCs),
	make_lc(ISCs, LCCs),
	atom_codes(LCSym, LCCs).

/*!---------------------------------------------------------------------
 |	make_uc_sym/2
 |	make_uc_sym(InSym, UCSym)
 |	make_uc_sym(+, -)
 |
 | 	- converts an atom or UIA to all uppercase characters
 |
 |	If InSym is a symbol, then UCSym is that symbol consisting of
 |	the same characters, in order, as InSym, except that all 
 |	lowercase symbols will have been converted to uppercase.
 *!--------------------------------------------------------------------*/
make_uc_sym(InSym, UCSym)
	:-
	atom_codes(InSym, ISCs),
	make_uc(ISCs, UCCs),
	atom_codes(UCSym, UCCs).

/*!---------------------------------------------------------------------
 |	convert_to_uc/2
 |	convert_to_uc(Items, UCItems)
 |	convert_to_uc(+, -)
 |
 | 	- converts all items in a list of terms to uppercase
 |
 |	If Items is a list of terms including atoms, numbers and
 |	compound terms, then UCItems will be the corresponding list,
 |	in order, of terms where atoms are converted to uppercase 
 |	(make_uc is applied), numbers and variables are left unchanged,
 |	and for any compound term, it's functor and args are are 
 |	converted by recursively applying convert_to_uc.
 *!--------------------------------------------------------------------*/
convert_to_uc(Item, Item)
	:-
	var(Item),!.
convert_to_uc([], [])
	:-!.
convert_to_uc(Item, Item)
	:-
	number(Item),!.
convert_to_uc(Item, UCItem)
	:-
	atom(Item),!,
	make_uc_sym(Item,UCItem).
convert_to_uc([Item | Items], [UCItem | UCItems])
	:-!,
	convert_to_uc(Item, UCItem),
	convert_to_uc(Items, UCItems).
convert_to_uc(Item, UCItem)
	:-
	Item =..[Functor | Args],!,
	convert_to_uc(Args, UCArgs),
	convert_to_uc(Functor, UCFunctor),
	UCItem =.. [UCFunctor | UCArgs].
convert_to_uc(Item, Item).

/*!---------------------------------------------------------------------
 |	same_uc/2
 |	same_uc(Term1, Term2)
 |	same_uc(+, +)
 |
 | 	- Term1,Term2 unify after converting all characters to upper case
 |
 |	Applies convert_to_uc/2 to both Term1,Term2, and tests
 |	whether the results unify.
 *!--------------------------------------------------------------------*/
same_uc(Term1, Term2)
	:-
	convert_to_uc(Term1, UCTerm),
	convert_to_uc(Term2, UCTerm).

/*!---------------------------------------------------------------------
 |	change_case_sym/2
 |	change_case_sym(InSym, OutSym)
 |	change_case_sym(+, -)
 |
 | 	- converts the case of characters in a symbol, based on the first
 |
 |	Changes the case of characters in a symbol InSym as follows:<br>
 |	a) If the first character of InSym is lowercase, applies make_uc 
 |	   to change all characters of InSym to uppercase;<br>
 |	b) If the first character of InSym is uppercase, applies make_lc 
 |	   to change all characters of InSym to lowercase.
 *!--------------------------------------------------------------------*/
change_case_sym(InSym, OutSym)
	:-
	atom_codes(InSym, ISCs),
	ISCs = [C1 | _],
	(C1 < 0'a ->
		make_lc(ISCs, OutCs);
		make_uc(ISCs, OutCs)
	),
	atom_codes(OutSym, OutCs).

/*!---------------------------------------------------------------------
 |	string_to_uia/2
 |	string_to_uia(String, UIA)
 |	string_to_uia(+, -)
 |
 | 	- creates a UIA corresponding to an arbitrary string
 |
 |	Given a String of arbitrary characters, UIA will be a uia
 |	containing exactly the same characters, in order.
 *!--------------------------------------------------------------------*/
string_to_uia([], '') :-!.
string_to_uia(String, UIA)
	:-
	length(String,Size),
	'$uia_alloc'(Size,UIA),
	string_to_uia(String,0,UIA).

/*!---------------------------------------------------------------------
 |	string_to_uia/3
 |	string_to_uia(Chars, Pos, UIA)
 |	string_to_uia(+, +, +)
 |
 | 	- insert list of char (codes) into a UIA 
 |
 |	If Chars is a list of charater codes of length L, if UIA is a
 | 	uia, if Pos is a positive integer =< length(UIA), then:
 |	if length(Chars) = N, this predicate attempts to replace the
 |	N characters of UIA beginning at position Pos (counting from 0 
 |	at the beginning of UIA), replacing each character of UIA,
 |	in order, by the corresponding character of Chars.  This succeeds 
 |	if length(UIA) - Pos >= L.  If length(UIA) - Pos < L, this fails; 
 |	however, note that this call will have descructively modified 
 |	UIA to insert the first length(UIA) - Pos Chars.
 *!--------------------------------------------------------------------*/
string_to_uia([], _, _).
string_to_uia([Char | Chars], CurPos, UIA)
   :-
   '$uia_pokeb'(UIA, CurPos, Char),
   NextPos is CurPos+1,
   string_to_uia(Chars, NextPos, UIA).


/*!---------------------------------------------------------------------
 |	string_to_sized_uia/3
 |	string_to_sized_uia(Size, Chars, UIA)
 |	string_to_sized_uia(+, +, -)
 |
 | 	- creates a UIA containg chars corresponding to a string
 |
 |	If Size is an integer, and if Chars is a list of character codes, 
 |	then:<br>
 |	a) if Size >= length(Chars), allocates a (new) UIA of length
 |	   Size, and copies Chars into UIA starting at position 0.
 |	b) if Size < length(Chars), fails.  
 *!--------------------------------------------------------------------*/
string_to_sized_uia(Size, Chars, UIA)
   :-
   '$uia_alloc'(Size, UIA),
   string_to_uia(Chars, 0, UIA).

/*!---------------------------------------------------------------------
 |	atomic_to_uia/2
 |	atomic_to_uia(Atom, UIABuf)
 |	atomic_to_uia(+, -)
 |
 | 	- create a UIA corresponding to an atomic item
 |
 |	Allocates a new UIA buffer UIABuf of smallest allowed size
 |	greater than length(Atom), and copies Atom into UIABuf
 |	beginning at position 0.
 *!--------------------------------------------------------------------*/
atomic_to_uia(Atom, UIABuf)
	:-
	atomic(Atom),  	
	atom_codes(Atom, AtomCodes),
	length(AtomCodes, BufLen),
	'$uia_alloc'(BufLen,UIABuf),
	string_to_uia(AtomCodes,0,UIABuf).

/*!---------------------------------------------------------------------
 |	cnvrt_to_UIA/2
 |	cnvrt_to_UIA(Term, UIABuf)
 |	cnvrt_to_UIA(+, -)
 |
 | 	- create a UIA corresponding to an arbitrary term
 |
 |	If term_codes(Term, Codes) holds, then allocates
 |	allocates a new UIA buffer UIABuf of smallest allowed size
 |	>= length(Codes) and copies Codes into UIABuf begninning
 |	at position 0.
 *!--------------------------------------------------------------------*/
cnvrt_to_UIA(In,Out)
	:-
	term_codes(In, Chars),
	length(Chars,Size),
	'$uia_alloc'(Size,Out),
	string_to_uia(Chars,0,Out).


/*!---------------------------------------------------------------------
 |	truncate/3
 |	truncate(InField, MaxSize, OutField)
 |	truncate(+, +, -)
 |
 | 	- creates a UIA truncating the input expression
 |
 |	Obtains the list of character codes corresponding to the
 |	input expression, truncates that list at MaxSize, allocates
 |	a UIA of that size, and copies the list of character codes
 |	into that UIA.
 *!--------------------------------------------------------------------*/
truncate(InField, MaxSize, OutField)
	:-
	(atom(InField) -> atom_codes(InField, IFCs)
		;
		(number(InField) -> number_codes(InField, IFCs)
			;
			term_codes(InField, IFCs)
		)
	),
	at_most_n(IFCs, MaxSize, TIFCs),
	string_to_uia(TIFCs, OutField).


/*!---------------------------------------------------------------------
 |	strip_white/2
 |	strip_white(String, Result)
 |	strip_white(+, -)
 |
 |	- strips leading white space from a string
 |
 |	Removes all leading whitespace chars (space, tab) from the
 |	input String to produce Result.
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
 |	- strips trailing white space  from a string
 |
 |	Removes all trailing whitespace chars (space, tab) from the
 |	input String to produce Result.
 *!--------------------------------------------------------------------*/
strip_tail_white(String, Result)
	:-
	dreverse(String, RString),
	strip_white(RString, RResult),
	dreverse(RResult, Result).

/*!---------------------------------------------------------------------
 |	strip_both_white/2
 |	strip_both_white(String, Result)
 |	strip_both_white(+, -)
 |
 |	- strips leading and trailing white space chars from a string
 |
 |	Removes all leading and trailing whitespace chars (space, tab) 
 |	from the input String to produce Result.
 *!--------------------------------------------------------------------*/
strip_both_white(String, Result)
	:-
	strip_white(String, Result0),
	dreverse(Result0, RString),
	strip_white(RString, RResult),
	dreverse(RResult, Result).

/*!---------------------------------------------------------------------
 |	strip_both_white_atom/2
 |	strip_both_white_atom(Atom, ResultAtom)
 |	strip_both_white_atom(+, -)
 |
 |	- strips leading and trailing white space chars from a prolog atom
 |
 |	Removes all leading and trailing whitespace chars (space, tab) 
 |	that occur in the input symbol.
 *!--------------------------------------------------------------------*/
strip_both_white_atom(Atom, ResultAtom)
	:-
	atom_codes(Atom, AtomCodes),
	strip_both_white(AtomCodes, ResultString),
	atom_codes(ResultAtom, ResultString).

/*!-----------------------------------------------------------------------------
 |	read_to/5.
 |	read_to(Chars, Stoppers, Head, Tail,Stopper)
 |	read_to(+, +, -, -,-)
 |
 |	-splits a string according to one of several possible chars
 |
 |	If:<br>
 |	Chars is a prolog string;<br>
 |	Stoppers is a list of codes of chars
 |		and if intersect(Chars,Stoppers) \= [],<br>
 |	Then:<br>
 |		Stopper is the element of Stopper with leftmost occurrence in Chars,<br>
 |		Head is the portion of Chars to the left of the occurrence of Stopper,<br>
 |	and<br>
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
 |	If:<br>
 |	Chars is a prolog string containing at least one blank,<br>
 |	Then:<br>
 |	Head is the portion of Chars to the left of the first occurrence
 |		 of a blank, and<br>
 |	Tail is the portion of Chars to the right of the first occurrence 
 |		of a blank
 *!--------------------------------------------------------------------*/

read_to_blank([], [], []).
 
read_to_blank([0'  | Chars], [], Chars)
	:-!.
	  
read_to_blank([C | Chars], [C | Head], Tail)
	:-
	read_to_blank(Chars, Head, Tail).

/*!---------------------------------------------------------------------
 |	char_in/3
 |	char_in(Atom, Char, Pos)
 |	char_in(+, +, -)
 |
 |	- Locates the position of a character in an atom.
 |
 |	If Atom is an atom, and Char is a quoted character, then:<br>
 |	a) If Char occurs in atom, then Pos is the position of the 
 |	   leftmost occurrence of Char, counting from 1.<br>
 |	b) Fails if Char does not occur in Atom.
 *!--------------------------------------------------------------------*/

char_in(Atom, Char, Pos)
	:-
	sub_atom(Atom,Pos0,1,_,Char),
	Pos is Pos0 + 1.

/*!-----------------------------------------------------------------------
 |	replace_char_string/4
 |	replace_char_string(InString, OrigCharNum, NewCharNum, OutString)
 |	replace_char_string(+, +, -)
 |
 |	- Replace occurrences of a char in a string by another char
 |
 |	Creates OutString by replacing all occurrences (possibly zero) of 
 |	OrigCharNum in InString by NewCharNum.
 *!-----------------------------------------------------------------------*/

replace_char_string([], _, _, []).
replace_char_string([OrigCharNum | InString], OrigCharNum, 
					NewCharNum, [NewCharNum | OutString])
	:-!,
	replace_char_string(InString, OrigCharNum, NewCharNum, OutString).
replace_char_string([C | InString], OrigCharNum, NewCharNum, [C | OutString])
	:-
	replace_char_string(InString, OrigCharNum, NewCharNum, OutString).

/*!-----------------------------------------------------------------------
 |	replace_char_atom/4
 |	replace_char_atom(AtomIn, OrigCharNum, NewCharNum, AtomOut)
 |	replace_char_atom(+, +, -)
 |
 |	- Replace occurrences of a char in an atom by another char
 |
 |	Creates AtomOut by replacing all occurrences (possibly zero) of 
 |	OrigCharNum in AtomIn by NewCharNum.
 *!-----------------------------------------------------------------------*/

replace_char_atom(AtomIn, OrigCharNum, NewCharNum, AtomOut)
	:-
	atom_codes(AtomIn,  AICs),
	replace_char_string(AICs, OrigCharNum, NewCharNum, AOCs),
	atom_codes(AtomOut, AOCs).

endmod.
