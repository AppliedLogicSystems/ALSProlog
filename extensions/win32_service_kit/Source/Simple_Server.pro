/*======================================================================*
 |	Simple Server.pro
 |	Copyright (c) 1997 Applied Logic Systems, Inc.
 |
 |	-- demonstrate client/server computing in ALS Prolog.
 |
 *======================================================================*/

/* Win32 Service */

:- ensure_loaded('Win32 Service.pro').

use win32_service.

win32_service(prolog_server, 'Prolog Server', start_win32_service).

start_win32_service(_) :- start_server.

/* Utilities */

port_number(4321).

unwind_protect(A, B) :-
	catch(A, Exception, (catch(B, _, true), throw(Exception))),
	B.

/* Server */

start_server :-
	port_number(Port),
	nsocket(internet, stream, 0, ServerSocket),
	unwind_protect((
	    nsocket_bind(ServerSocket, Port),
	    nsocket_listen(ServerSocket, 5),
	    service_connection(ServerSocket)
	), nsocket_close(ServerSocket)).

service_connection(ServerSocket) :-
	nsocket_accept(ServerSocket, Info, ClientSocket),
	catch(open(nsocket(ClientSocket), read, R),
	      Exception, (nsocket_close(ClientSocket), throw(Exception))),
	unwind_protect((
	    open(nsocket(ClientSocket), write, W),
	    unwind_protect((
	        service(R, W)
	    ), close(W))
	), close(R)),
	service_connection(ServerSocket).

service(R, W) :-
	read(R, Query),
	catch((
	    findall(Query, Query, Result)
	), Exception, Result = Exception),
	writeq(W, Result),
	write(W, '.').

/* Client */

ask_server(ServerHost, Query, Result) :-
	port_number(Port),
	nsocket(internet, stream, 0, Socket),
	catch((
	    nsocket_connect(Socket, ServerHost, Port),
	    open(nsocket(Socket), read, R)),
	    Exception, (nsocket_close(ClientSocket), throw(Exception))),
	unwind_protect((
	    open(nsocket(Socket), write, W),
	    unwind_protect((
	        ask(R, W, Query, Result)
	    ), close(W))
	), close(R)).

ask(R, W, Query, Result) :-
	write_clause(W, Query),
	read(R, Result).




