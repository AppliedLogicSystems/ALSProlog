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


test :- finger(choupt,herbrand).
ntest :- nfinger(choupt,herbrand).

nfinger(Who, Where) :-
	nsocket(internet, stream, 0, SD),
	write(SD), nl,
	nsocket_connect(SD, Where, 79),
	open(nsocket(SD), write, WS, []),
	open(nsocket(SD), read, RS, []),
	put_line(WS, Who),
	read_and_print_lines(RS),
	close(WS), close(RS),
	nsocket_close(SD).
