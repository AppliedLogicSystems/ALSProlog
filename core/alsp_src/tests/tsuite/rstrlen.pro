/*
 * A slow way to determine the length of an atom
 */

rstrlen(A,Len) :-
	rexec('wc -c',[rstream(RS,[]),wstream(WS,[])]),
	write(WS,A),
	close(WS),
	read_term(RS,Len,[attach_fullstop(true)]),
	close(RS).

rstrlen(Host,A,Len) :-
	rexec('wc -c',[host(Host),rstream(RS,[]),wstream(WS,[])]),
	write(WS,A),
	close(WS),
	read_term(RS,Len,[attach_fullstop(true)]),
	close(RS).
