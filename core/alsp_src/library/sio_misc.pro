/*========================================================================*
 |		sio_misc.pro
 |	Copyright (c) 1996 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		Miscellaneous stream io-related predicates
 |
 |	Authors: Ken Bowen, Kevin Buettner
 |	Date:	 
 *========================================================================*/

module sio.

/*
 * get_nonblank_char(Char)
 *
 *	Unifies Char with the next non-whitespace character obtained from the 
 *	default input stream, if that occurs before the next end of line, and
 *	unifies Char with the atom
 *				end_of_line
 *	otherwise.
 */

export get_nonblank_char/1.

get_nonblank_char(Char) :-
	get_current_input_stream(Stream),
	get_nonblank_char(Stream,Char).

/*
 * get_nonblank_char(Stream_or_alias, Char)
 *
 *	Unifies Char with the next non-whitespace character obtained from the 
 *	input stream associated with Stream_or_alias, if that occurs before the 
 *	next end of line, and unifies Char with the atom
 *				end_of_line
 *	otherwise.
 */

export get_nonblank_char/2.

get_nonblank_char(Stream, Char) :-
	get_code(Stream, CChar),
	check_char_input(CChar, Stream, Char).

check_char_input(CC,Stream, end_of_line) :-
    iseoln(CC),
    !.

check_char_input(CChar,Stream,Char) :-
    isspace(CChar),
    !,
	get_nonblank_char(Stream, Char).

check_char_input(CChar,Stream,CChar) :-
	get_code(Stream, C),
    consume_til_end(C, Stream).

consume_til_end(C, Stream) :-
    iseoln(C),
    !.

consume_til_end(_, Stream) :-
	get_code(Stream, C),
    consume_til_end(C, Stream).

iseoln(0'\n).
iseoln(0'\r).

isspace(S) :- S =< 32, not(iseoln(S)).

/*********************************************************
/*
 * get_atomic_nonblank_char(Char)
 *
 *	Unifies Char with the atomic form of the next non-whitespace character 
 *  obtained from the default input stream, if that occurs before the next 
 *	end of line, and unifies Char with the atom
 *				end_of_line
 *	otherwise.
 */

export get_atomic_nonblank_char/1.

get_atomic_nonblank_char(Char) :-
	get_current_input_stream(Stream),
	get_atomic_nonblank_char(Stream,Char).

/*
 * get_atomic_nonblank_char(Stream_or_alias, Char)
 *
 *	Unifies Char with the atomic form of the next non-whitespace character 
 *	obtained from the input stream associated with Stream_or_alias, if that 
 *	occurs before the next end of line, and unifies Char with the atom
 *				end_of_line
 *	otherwise.
 */

export get_atomic_nonblank_char/2.

/*
get_atomic_nonblank_char(Stream,Char)
	:-
	get_nonblank_char(Stream,Char0),
	(Char0 = end_of_line ->
		Char0 = Char
		;
		name(Char, [Char0])
	).
*/

get_atomic_nonblank_char(Stream,Char)
	:-
	get_line(Stream, Line),
	atom_codes(Line, CharList),
	first_nonblank_char(CharList, Char0),
	atom_codes(Char, [Char0]).
get_atomic_nonblank_char(Stream, end_of_line).

first_nonblank_char([Char | Rest], Char) :-
	Char > 32.
first_nonblank_char([_ | Rest], Char) :-
	first_nonblank_char(Rest, Char).

*********************************************************/

/*
 * put_byte(Byte)
 *
 *	Outputs the byte Byte to the current default output stream.
 *
 */

export put_byte/1.

put_byte(Byte) :- put_code(Byte).

/*
 * put_byte(Stream_or_alias,Byte)
 *
 *	Outputs the byte Byte to the stream defined by Stream_or_alias.
 *
 */

export put_byte/2.

put_byte(Stream_or_alias, Byte) :- put_code(Stream_or_alias, Byte).


/*
 * get_byte(Byte)
 *
 *	Unifies Byte with the next byte obtained from the default input stream.
 *
 * FIXME: Move to library.
 */

export get_byte/1.

get_byte(Byte) :- get_code(Byte).


/*
 * get_byte(Stream_or_alias, Byte)
 *
 *	Unifies Byte with the next byte obtained from the stream associated
 *	with Stream_or_alias.
 *
 * FIXME: Move to library.
 */

export get_byte/2.

get_byte(Stream_or_alias,Byte) :- 
	get_code(Stream_or_alias,Byte).

/*
 * read_chars(Stream_or_alias, List, Num)
 *
 *	Num must be a nonnegative integer;
 *	Unifies List with the atom corresponding to the list of characters 
 *	obtained by reading up to Num characters from the stream associated 
 *	with Stream_or_alias.
 */

export read_chars/3.

read_chars(SS, CharAtom, Num)
    :-
    get_codes(Num, Chars, SS),
	name(CharAtom, Chars).

get_codes(0, [], _) :- !.
get_codes(Num, [CC | Chars], SS)
    :-
    get_code(SS, CC),
    !,
    NextNum is Num-1,
    get_codes(NextNum, Chars, SS).
get_codes(_, [], SS).

/*
 * consume_whitespace(Stream)
 *
 *	Consumes any whitespace remaining in the stream's buffer;
 *  Returns successfully, even when the stream is snr_action(snr_code)
 */

export consume_whitespace/1.

consume_whitespace(Stream)
	:-
	peek_code(Stream, CC),
	disp_consume_whitespace(CC,Stream).

disp_consume_whitespace(-2,Stream) :-!.
disp_consume_whitespace(CC,Stream)
	:-
	is_white_space(CC),
	get_code(Stream, _),
	!,
	consume_whitespace(Stream).
	disp_consume_whitespace(_,_).

is_white_space(0' ).        %% space
is_white_space(0'   ).      %% tab
is_white_space(13).
is_white_space(10).

endmod.
