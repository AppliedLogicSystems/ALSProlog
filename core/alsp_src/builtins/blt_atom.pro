/*========================================================================*
 |			blt_atom.pro		
 |		Copyright (c) 1986-1996 Applied Logic Systems, Inc.
 |
 |			-- atom / constant processing predicates
 |
 | Author:  Kevin A. Buettner
 | Created: 10-26-93
 |
 | Procedure descriptions in this file are from the March, 1993
 | Draft ISO Standard.
 *========================================================================*/

module builtins.
/*
 * atom_concat/3
 *
 * atom_concat(Atom_1, Atom_2, Atom_12) is true iff the atom Atom_12 is the
 * atom formed by concatenating the characters of the atom Atom_2 to the
 * characters of atom Atom_1.
 *
 * Procedurally, atom_concat(Atom_1, Atom_2, Atom_12) unifies Atom_12 with
 * the concatenation of Atom_1 and Atom_2.
 *
 * atom_concat(Atom_1, Atom_2, Atom_12) is re-executable when only Atom_12
 * is instantiated.  On re-execution successive value for Atom_1 and Atom_2
 * are generated.
 */


export atom_concat/3.

atom_concat(A1,A2,A12) :-
	nonvar(A1),
	nonvar(A2),
	!,
	atom_ok(A1),
	atom_ok(A2),
	var_or_atom_ok(A12),
	'$atom_concat'(A1,A2,A12).
atom_concat(A1,A2,A12) :-
	nonvar(A1),
	nonvar(A12),
	!,
	atom_ok(A1),
	var_or_atom_ok(A2),
	atom_ok(A12),
	atom_length(A12,LT),
	atom_length(A1,L1),
	L2 is LT-L1,
	S2 is 1+L1,
	'$sub_atom'(A12,1,L1,A1),
	'$sub_atom'(A12,S2,L2,A2).
atom_concat(A1,A2,A12) :-
	nonvar(A2),
	nonvar(A12),
	!,
	var_or_atom_ok(A1),
	atom_ok(A2),
	atom_ok(A12),
	atom_length(A12,LT),
	atom_length(A2,L2),
	L1 is LT-L2,
	S2 is 1+L1,
	'$sub_atom'(A12,S2,L2,A2),
	'$sub_atom'(A12,1,L1,A1).
atom_concat(A1,A2,A12) :-
	var_or_atom_ok(A1),
	var_or_atom_ok(A2),
	atom_ok(A12),
	!,
	atom_length(A12,A12Len),
	StopPos is A12Len+1,
	enumerate_ints(S2,1,StopPos),
	L1 is S2 - 1,
	L2 is A12Len - S2 + 1,
	'$sub_atom'(A12,1,L1,A1),
	'$sub_atom'(A12,S2,L2,A2).

/*
 * enumerate_ints(I,Start,Stop)
 * Generates integers I from Start to Stop inclusive.
 */

enumerate_ints(I,Start,Stop) :-
	integer(I),
	Start =< I,
	I =< Stop,
	!.
enumerate_ints(I,Start,Stop) :-
	Start > Stop,
	!,
	fail.
enumerate_ints(Start,Start,Stop).	%% Do not want to cut here
enumerate_ints(I,Start,Stop) :-
	NStart is Start+1,
	enumerate_ints(I,NStart,Stop).

/*******************************************************************
/*
 * sub_atom/4
 *
 * sub_atom(Atom, Start, Length, Sub_atom) is true iff atom Sub_atom
 * is the atom with Length characters starting at the Start-th character
 * of atom Atom.
 *
 * Procedurally, sub_atom(Atom, Start, Length, Sub_atom) unifies
 * Sub_atom with an atom Atom which has Length characters identical with
 * the Length characters of atom Atom that start with the Start-th
 * character of Atom.
 *
 * sub_atom(Atom, Start, Length, Sub_atom) is re-executable.  On re-
 * execution, all possible values for Start, Length, and Sub_atom are
 * generated.
 *
 * sub_atom(+atom, ?integer, ?integer, ?atom)
 */

export sub_atom/4.
sub_atom(Atom, Start, Length, SubAtom) :-
	nonvar(Atom),
	nonvar(Start),
	nonvar(Length),
	!,
	atom_ok(Atom),
	integer_ok(Start),
	integer_ok(Length),
	var_or_atom_ok(SubAtom),
	'$sub_atom'(Atom, Start, Length, SubAtom).
sub_atom(Atom, Start, Length, SubAtom) :-
	atom_ok(Atom),
	var_or_integer_ok(Start),
	var_or_integer_ok(Length),
	var_or_atom_ok(SubAtom),
	atom_length(Atom, LT),
	MaxStart is LT+1,
	enumerate_ints(Start,1,MaxStart),
	MaxLen is MaxStart-Start,
	enumerate_ints(Length, 0, MaxLen),
	'$sub_atom'(Atom, Start, Length, SubAtom).
*******************************************************************/


/*

TO DO: make a new version of '$sub_atom' that takes a 0-indexed start
       parameter.  This will eliminate all the "Start is Before+1"
       terms.
*/

export sub_atom/5.

/* The main clause simple does error checking. */
sub_atom(Atom, Before, Length, After, SubAtom) :-
	atom_ok(Atom),
	var_or_nonneg_integer_ok(Before),
	var_or_nonneg_integer_ok(Length),
	var_or_nonneg_integer_ok(After),
	var_or_atom_ok(SubAtom),
	sub_atom0(Atom, Before, Length, After, SubAtom).

/* The next three clauses handle single solution cases. */
sub_atom0(Atom, Before, Length, After, SubAtom) :-
	integer(Before),
	integer(Length),
	!,
	atom_length(Atom, LT),
	After is LT - Before - Length,
	Start is Before+1,
	'$sub_atom'(Atom, Start, Length, SubAtom).

sub_atom0(Atom, Before, Length, After, SubAtom) :-
	integer(Length),
	integer(After),
	!,
	atom_length(Atom, LT),
	Before is LT - After - Length,
	Start is Before+1,
	'$sub_atom'(Atom, Start, Length, SubAtom).

sub_atom0(Atom, Before, Length, After, SubAtom) :-
	integer(Before),
	integer(After),
	!,
	atom_length(Atom, LT),
	Length is LT - Before - After,
	Start is Before+1,
	'$sub_atom'(Atom, Start, Length, SubAtom).

/* A clause to effieciently enumerate fixed-After cases. */
sub_atom0(Atom, Before, Length, After, SubAtom) :-
	integer(After),
	!,
	atom_length(Atom, LT),
	MaxBefore is LT - After,
	enumerate_ints(Before, 0, MaxBefore),
	Length is LT - After - Before,
	Start is Before+1,
	'$sub_atom'(Atom, Start, Length, SubAtom).

/* The completely general case. */
sub_atom0(Atom, Before, Length, After, SubAtom) :-
	atom_length(Atom, LT),
	MaxBefore is LT,
	enumerate_ints(Before,0,MaxBefore),
	MaxLen is MaxBefore-Before,
	enumerate_ints(Length, 0, MaxLen),
	After is LT - Before - Length,
	Start is Before+1,
	'$sub_atom'(Atom, Start, Length, SubAtom).
	
/*
 * atom_split/4
 *
 * atom_split(Atom, Splitter, Left, Right) is true iff either
 * Atom and Splitter are atoms, and either:
 * a) there is no occurrence of Splitter as a subatom of Atom, Left equals
 *    Atom, and Right is the empty atom '', or
 * b) there is at least one occurrence of Splitter as a subatom of Atom, 
 *    Left is the subatom of Atom extending from the leftmost character of
 *    Atom up to but not including the leftmost character of the leftmost
 *    occurence of Splitter in Atom, and Right is the subatom of Atom 
 *    extending from the first character to the right of the rightmost
 *   character of the leftmost occurrence of Splitter in Atom.
 *
 *  atom_split(+atom, ?splitter, ?left, ?right)
 */

export atom_split/4.
atom_split(Atom, SplitterAtom, LeftSubAtom, RightSubAtom)
        :-
        sub_atom(Atom, Begin, SALen, After,  SplitterAtom),
        sub_atom(Atom, 0, Begin, _, LeftSubAtom),
        sub_atom(Atom, _, After, 0, RightSubAtom).

/*
 * atom_split/3
 *
 * atom_split(Atom, Splitter, ListOfSubAtoms) is true iff either
 * Atom and Splitter are atoms, and either:
 * a) there is no occurrence of Splitter as a subatom of Atom, and 
 *    ListOfSubAtoms equals the list consisting of Atom alone, or
 * b) there is at least one occurrence of Splitter as a subatom of Atom, 
 *    and ListOfSubAtoms is the list consisting of all of the subatoms of
 *    Atom, in left-to-right order, which satisfy one of:
 * i) extends from the leftmost (beginning) character of Atom to the
 *    leftmost character of the leftmost occurrence of Splitter in Atom;
 * ii) extend from the rightmost character of one occurrence of Splitter
 *    in Atom to the leftmost character of the next occurrence of Splitter
 *    to the right;
 * iii) extends from the leftmost character of the rightmost occurrence
 *    of Splitter in Atom to the right end of Atom.
 *
 *  atom_split(+atom, ?splitter, ?list_of_atoms)
 */

export atom_split/3.

atom_split(Atom, SplitterAtom, [InterLeft | RestCList])
        :-
        atom_split(Atom, SplitterAtom, InterLeft, InterRight),
        !,
        atom_split(InterRight, SplitterAtom, RestCList).

atom_split(Atom, SplitterAtom, [Atom]).


/*
 * number_chars/2
 *
 * number_chars(Number,List) is true iff List is a list whose elements are
 * the characters corresponding to a character sequence of Number which
 * could be output.
 *
 * number_chars(+number,+list)
 * number_chars(+number,-list)
 * number_chars(-number,+list)
 */

export number_chars/2.
number_chars(Number,List) :-
	char_list_ok(List),		%% will *fail* when any portion is var
	!,
	var_or_number_ok(Number),
	open(char_list(List),read,S),
	catch( read_term(S,N,[attach_fullstop(true),syntax_errors(error)]), 
	       E, N=not_number ),
	close(S),
	!,
	number_chars_check_number(N,number_chars,Number,List).
number_chars(Number,List) :-
	number_ok(Number),
	open(char_list(NList),write,S),
	write_canonical(S,Number),
	close(S),
	!,
	List = NList.

number_chars_check_number(N,_,Number,List) :-
	number(N),
	!,
	N = Number.
number_chars_check_number(N,What,Number,List) :- 
	Goal =.. [What,Number,List],
	setPrologError(error(syntax_error,
			     [builtins:Goal])),
	forcePrologError.

/*
 * number_codes/2
 *
 * number_codes(Number,List) is true iff List is a list whose elements are
 * the character codes corresponding to a character sequence of Number
 * which could be output.
 *
 * number_codes(+number,+list)
 * number_codes(+number,-list)
 * number_codes(-number,+list)
 */

export number_codes/2.
number_codes(Number,List) :-
	code_list_ok(List),		%% will *fail* when any portion is var
	!,
	var_or_number_ok(Number),
	open(code_list(List),read,S),
	catch( read_term(S,N,[attach_fullstop(true),syntax_errors(error)]),
	       E, N=not_number ),
	close(S),
	!,
	number_chars_check_number(N,number_codes,Number,List).
number_codes(Number,List) :-
	number_ok(Number),
	open(code_list(NList),write,S),
	write_canonical(S,Number),
	close(S),
	!,
	List = NList.

/*
 * term_chars/2
 * term_codes/2
 *
 * Not part of the standard, but included for orthogonality.  These
 * should replace our old buf_read and buf_write
 */

export term_chars/2.

term_chars(Term, List) :-
	char_list_ok(List),		%% will fail when any portion is var
	!,
	open(char_list(List),read,S),
	catch( read_term(S,RTerm,[attach_fullstop(true),syntax_errors(error)]),
	       E, true ),
	close(S),
	!,
	term_C_check_syntax_error(E,term_chars(Term,List)),
	RTerm = Term.
term_chars(Term, List) :-
	open(char_list(WList),write,S),
	write_canonical(S,Term),
	close(S),
	!,
	List = WList.

term_C_check_syntax_error(E,_) :-
	var(E),
	!.
term_C_check_syntax_error(error(syntax_error,L),G) :-
	list_delete(L,_:_,DL),
	!,
	throw(error(syntax_error,[builtins:G | DL])).
term_C_check_syntax_error(E,_) :-
	throw(E).

export term_codes/2.

term_codes(Term, List) :-
	code_list_ok(List),		%% will fail when any portion is var
	!,
	open(code_list(List),read,S),
	catch( read_term(S,RTerm,[attach_fullstop(true),syntax_errors(error)]),
	       E, true ),
	close(S),
	!,
	term_C_check_syntax_error(E,term_codes(Term,List)),
	RTerm = Term.
term_codes(Term, List) :-
	open(code_list(WList),write,S),
	write_canonical(S,Term),
	close(S),
	!,
	List = WList.

endmod.
