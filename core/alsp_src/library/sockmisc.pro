/*==========================================================================*
 |   			sockmisc.pro
 |	Copyright (c) 1994 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		Socket-oriented utilities
 |
 |	Authors: Kevin Buettner, Ken Bowen
 |	Date:	 May 1994
 *==========================================================================*/

module sio.

export bread/1.
export bread/2.

/*!---------------------------------------------------
 | bread/1
 | bread/2
 | bread(ReadStream, Next)
 | bread(-)
 | bread(+, -)
 |
 |	-- blocking read from a (non-blocking) stream
 |
 |	Arguments:
 |	ReadStream:	A stream opened in read mode, normally
 |				also in non-blocking mode;
 |	Next:		A term read from ReadStream
 |
 |	This predicate is used for performing blocking reads
 |	from a non-blocking stream(e.g., socket, window).
 |	It is just syntactic sugar for calls to read_term/3
 |	with blocking(true) on the options list.
 *!--------------------------------------------------*/

bread(Next)
	:-
	current_input(Stream),
	bread(Stream, Next).

bread(ReadStream, Next)
	:-
	read_term(ReadStream, Next, [blocking(true)]).

/*!---------------------------------------------------
 | bread_term/3
 | bread_term(ReadStream, Next, Options)
 | bread_term(+, -, +)
 |
 |	-- blocking read, with options, from a (non-blocking) stream
 |
 |	Arguments:
 |	ReadStream:	A stream opened in read mode, normally
 |				also in non-blocking mode;
 |	Next:		A term read from ReadStream
 |	Options:	An options list as for read_term/3
 |
 |	This predicate is used for performing blocking reads
 |	from a non-blocking stream(e.g., socket, window).
 |	This is just syntactic sugar, since all it does is
 |	add blocking(true) to the Options list and call
 |	read_term/3.
 *!--------------------------------------------------*/

bread_term(ReadStream, Next, Options)
	:-
	read_term(ReadStream, Next, [blocking(true) | Options]).

endmod.
