/*
 * mathserv1.pro -- demonstrate client/server computing in ALS-Prolog.
 *
 * Author: Kevin A. Buettner
 * Creation: 12/13/93
 *
 * This file contains both the client and the server.  It uses inet stream
 * sockets for the communication.  This type of socket is connected, and
 * as such, requires a queue to maintain contact with the clients.
 */

/*
 * server framework  -- should be adaptable to other servers
 */

start_server :-
	portnum(Port),
	catch(open(socket(inet_stream,Host,Port),read,S),
	      _,
	      fail),
	(is_server_socket(S) -> true ; close(S), fail),
	!,
	info('Server started on host %s, port %d\n',[Host,Port]),
	service_loop([s(S) | T],T).
start_server :-
	info('Can\'t start server!',[]).

info(M,PL) :- printf(user_output,M,PL).

portnum(4321).

service_loop([],_) :- !.
service_loop([S | T], H1) :-
	serve_stream(S, H1, H2),
	service_loop(T,H2).

serve_stream(S, H1, H2) :-
	%info('examining %t\n',[S]),
	get_stream_to_poll(S,PollStream),
	poll(PollStream,10000/*00*/),	/* timeout is .01 sec */
	!,
	serve_ready_stream(S, H1, H2).
serve_stream(S, [S | H], H).

serve_ready_stream(s(S),[c(SR,SW,initial_state), s(S) | H], H) :-
	info('Accepting a new connection.\n',[]),
	!,
	open(socket(clone,S),read,SR),
	accept_socket_connection(SR),
	open(socket(clone,SR),write,SW).
serve_ready_stream(c(SR,SW,CurState), [c(SR,SW,NewState) | H], H) :-
	info('Servicing a request.\n',[]),
	service_request(SR,SW,CurState,NewState),
	!.
serve_ready_stream(c(SR,SW,_), H, H) :-
	info('Closing a connection\n',[]),
	close(SR),
	close(SW).

get_stream_to_poll(S,SP) :-
    arg(1,S,SP).

/*
 * server specific code
 */

service_request(SR,SW,State,State) :-
	read(SR, Request),
	info('Request: %t\n',[Request]),
	sr(Request,SW),
	skip(SR, 0'\n).

sr(done,SW) :- !, fail.
sr(end_of_file,SW) :- !, fail.
sr(Expression, SW) :-
	Result is Expression,
	!,
	write(SW, Result),
	write(SW,.),
	nl(SW),
	flush_output(SW).
sr(Expression, SW) :-
	write(SW,malformed_expression(Expression)),
	write(SW,.),
	nl(SW),
	flush_output(SW).

/*
 * Client code
 */

start_client(Host) :-
	portnum(Port),
	catch(open(socket(inet_stream,Host,Port),read,_,[alias(rsock)]),
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
	skip(rsock,0'\n),
	info('Answer is %t\n',[Ans]).

stop_client :-
	close(rsock),
	close(wsock).
