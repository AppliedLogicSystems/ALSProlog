/*----------------------------------------------------------------*
 |	An Internet Finger Client
 |
 |	Copyright (c) 1997 by Applied Logic Systems, Inc.
 |	Distribution rights per Copying ALS
 |
 | Example:
 |
 | ?- finger(ken, 'www.als.com').
 |
 *----------------------------------------------------------------*/

finger(Who, Where) :- 
	open(socket(inet_stream, Where, 79), read, RFinger,
	       [write_eoln_type(crlf), read_eoln_type(crlf)]),
	open(socket(clone,RFinger),write,WFinger,[]),
	put_line(WFinger, Who),
	read_and_print_lines(RFinger),
	close(WFinger),
	close(RFinger).

read_and_print_lines(Stream) :-
	get_line(Stream, Line),
	put_line(Line),
	!,
	read_and_print_lines(Stream).
read_and_print_lines(_).
