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


test :- finger(choupt,'world.std.com').

nfinger(Who, Where) :-
	open(nsocket(inet, stream, 0), read_write, S),
	nsocket_connect(S, Where, 79),
	put_line(S, Who),
	read_and_print_lines(S),
	close(S).
