/*--------------------------------------------------------------------*
 |		sio_d10.pro		
 |	Copyright (c) 1992 Applied Logic Systems, Inc.
 |		Distribution rights: Unrestriced
 |
 |		-- DEC-10 compatiblity
 |
 | Author: Kevin A. Buettner
 | Creation: 1-17-92
 *--------------------------------------------------------------------*/

module sio.

/*
 * see(File_or_alias_or_stream)
 *
 * see/1 will take as its only argument a filename, alias, or a stream
 * descriptor. 
 *
 * If its argument is a stream descriptor, then the current
 * input stream is set to this stream.
 *
 * If its argument is an alias, then the current input stream is set
 * to the stream assigned to this alias.
 *
 * Otherwise its argument is assumed to be the name of the file to open.  In
 * this case, the file is opened and the file name is assigned to the
 * newly opened stream as its alias.   The current input stream is then
 * set to this newly opened stream.
 *
 */

export see/1.

see(Alias) :-
	%is_input_alias(Alias, Stream),
	is_stream(Alias, Stream),
	is_input_stream(Stream),
	!,
	set_input(Stream).
see(FileName) :-
	catch(open(FileName,read,Stream,[alias(FileName)]),_,fail),
	!,
	set_input(Stream).

/*
 * seeing(Alias_or_stream)
 *
 * seeing/1 will unify its only argument with an alias of the current input
 * stream.  Failing that (because there is no alias), it will use the stream
 * descriptor.  This permits programs written with the old style I/O to coexist
 * with programs written with the new style.
 */

export seeing/1.

seeing(Alias) :-
	current_input(Stream),
	current_alias(Alias,Stream),
	!.
seeing(Stream) :-
	current_input(Stream).


/*
 * seen
 *
 * seen will close the current input stream
 */

export seen/0.

seen :-
	current_input(Stream),
	close(Stream).


/*
 * tell(File_or_alias_or_stream)
 *
 * tell/1 will take as its only argument a filename, alias, or a stream
 * descriptor. 
 *
 * If its argument is a stream descriptor, then the current
 * output stream is set to this stream.
 *
 * If its argument is an alias, then the current output stream is set
 * to the stream assigned to this alias.
 *
 * Otherwise its argument is assumed to be the name of the file to open.  In
 * this case, the file is opened and the file name is assigned to the
 * newly opened stream as its alias.   The current output stream is then
 * set to this newly opened stream.
 *
 */

export tell/1.

tell(Alias) :-
	%is_output_alias(Alias, Stream),
	is_stream(Alias,Stream),
	is_output_stream(Stream),
	!,
	set_output(Stream).
tell(FileName) :-
	catch(open(FileName,write,Stream,[alias(FileName)]),_,fail),
	!,
	set_output(Stream).


/*
 * telling(Alias_or_stream)
 *
 * telling/1 will unify its only argument with an alias of the current output
 * stream.  Failing that (because there is no alias), it will use the stream
 * descriptor.  This permits programs written with the old style I/O to coexist
 * with programs written with the new style.
 */

export telling/1.

telling(Alias) :-
	current_output(Stream),
	current_alias(Alias,Stream),
	!.
telling(Stream) :-
	current_output(Stream).


/*
 * told
 *
 * told will close the current output stream.
 */

export told/0.

told :-
	current_output(Stream),
	close(Stream).


/*
 * get0(Char)
 *
 *	Same functionality as get_code(Char)
 */


export get0/1.
export get0/2.

get0(Code) :-
	get_code(Code).

get0(Stream_or_alias,Code) :-
	get_code(Stream_or_alias,Code).

/*
 * get(Char)
 *
 *	Calls get0 repeatedly until non-control character is found.
 */

export get/1.
export get/2.

get(Byte) :-
	current_input(Stream),
	get0(Stream,Byte0),
	get_more(Byte0,Byte,Stream).

get(Stream,Byte) :-
	get0(Stream,Byte0),
	get_more(Byte0,Byte,Stream).

get_more(Byte0,Byte,Stream) :-
	Byte0 =< 0' ,		%% space character
	Byte0 \= -1,
	!,
	get0(Stream,Byte1),
	get_more(Byte1,Byte,Stream).
get_more(Byte,Byte,Stream).


/*
 * skip(Char)
 *
 *	reads characters out of current input stream until Char is found.
 */

export skip/1, skip/2.

skip(Char) :-
	current_input(Stream),
	skip(Stream,Char).

skip(Stream,Byte) :-
	get0(Stream,Byte0),
	skip_more(Byte0,Byte,Stream).

skip_more(-1, _, Stream) :- !, fail.	%% fail when end of file is reached
skip_more(Byte,Byte, Stream) :- !.	%% stop when specified char is reached
skip_more(_,Byte, Stream) :-		%% else loop around
	get0(Stream,Byte0),
	skip_more(Byte0,Byte,Stream).


/*
 * put(Char)
 *
 *	This predicate has the functionality as put_code.
 */

export put/1.
export put/2.

put(Byte) :-
	put_code(Byte).

put(Stream_or_alias,Code) :-
	put_code(Stream_or_alias,Code).


/*
 * tab(N)
 *
 *	Puts out N space characters
 */

export tab/1.

tab(N) :-
	N =< 0,
	!.
tab(N) :-
	NN is N-1,
	put(0' ),
	tab(NN).

/*
 * ttyflush
 */

export ttyflush/0.
ttyflush :- flush_output.

/*
 * display/1
 */

export display/1.
display(X) :- 
	write_term(X,[quoted(false),ignore_ops(true),numbervars(true)]).

endmod.
