/*
 * mathserv2.pro -- demonstrate client/server computing in ALS-Prolog
 *
 * Author: Kevin A. Buettner
 * Creation: 12/14/93
 *
 * This file contains code for both the client and the server.  It uses
 * datagram sockets for communication.  Datagram sockets are connectionless
 * and so the design of the server is greatly simplified.
 */

/*
 * server framework
 */

start_server :-
	portnum(Port),
	catch(open(socket(inet_dgram,Host,Port), read, SR),
	      _,
	      fail),
	(is_server_socket(SR) -> true ; close(SW), fail),
	!,
	open(socket(clone,SR),write,SW),
	info('Server started on host %s, port %d\n',[Host,Port]),
	service_loop(SR,SW).

info(M,PL) :- printf(user_output,M,PL).

portnum(54321).

service_loop(SR,SW) :-
	read(SR,Request),
	info('Request: %t\n',[Request]),
	service_request(Request,SW),
	flush_input(SR),
	service_loop(SR,SW).

service_request(Request,SW) :-
	Result is Request,
	!,
	write(SW,Result),
	write(SW,.),
	nl(SW),
	flush_output(SW).
service_request(Request,SW) :-
	write(SW,malformed_request(Request)),
	write(SW,.),
	nl(SW),
	flush_output(SW).


/*
 * Client code
 */

start_client(Host) :-
	portnum(Port),
	catch(open(socket(inet_dgram,Host,Port),read,_,[alias(rsock)]),
	      _,
	      fail),
	(is_server_socket(rsock) -> 
	    close(rsock),
	    fail
	;
	    open(socket(clone,rsock),write,_,[alias(wsock)])
	).
start_client(Host) :-
	info('Can\'t find server\n',[]).

send_request(Exp) :-
	write(wsock,Exp),
	write(wsock,.),
	nl(wsock),
	flush_output(wsock),
	read(rsock,Ans),
	flush_input(rsock),
	info('Answer is %t\n',[Ans]).

stop_client :-
	close(rsock),
	close(wsock).
