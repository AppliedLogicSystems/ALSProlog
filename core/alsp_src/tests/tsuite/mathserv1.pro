/*======================================================================*
 | 			mathserv1.pro 
 |		Copyright (c) 1993-5 Applied Logic Systems, Inc.
 |
 |		-- demonstrate client/server computing in ALS-Prolog.
 |
 | Author: Kevin A. Buettner
 | Creation: 12/13/93
 |
 | This file contains both the client and the server.  It uses inet stream
 | sockets for the communication.  This type of socket is connected, and
 | as such, requires a queue to maintain contact with the clients.
 *======================================================================*/

portnum(4321).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% server framework  -- should be adaptable to other servers
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_server 
	:-
	portnum(Port),
	catch(open(socket(inet_stream,Host,Port),read,S),
	      _,
	      fail),
	(is_server_socket(S) -> 
		true 
		; 
		close(S), fail
	),
	!,
	info('Server started on host %s, port %d\n',[Host,Port]),
	service_loop([s(S) | T],T).

start_server 
	:-
	info('Can\'t start server!',[]).


/*--------------------------------------------------------
 |	service_loop/2
 |	service_loop(QueueHead,QueueTail)
 |	service_loop(+,+)
 |
 |	The pair (QueueHead,QueueTail) constitutes an
 |	extensible list (QueueTail is uninstantiated);
 *-------------------------------------------------------*/

	%% should never occur, but is here in case the queue
	%% somehow ever becomes empty:
service_loop([],_) :- !.

service_loop([S | RestQ], QT)
	:-
	serve_stream(S, QT, NewQT, Flag),
	disp_service_loop(Flag, S, QT, NewQT, RestQ).

/*--------------------------------------------------------
 |	disp_service_loop/5
 |	disp_service_loop(Flag, S, QT, NewQT, RestQ)
 |	disp_service_loop(+, +, +, +, +)
 |
 |	Flag  -  communicates information from a transaction
 |				directly to the server;
 |	S	  -  stream processed in this transaction
 |	QT	  -  the (uninstantiated) tail of the queue
 |	NewQT -  the new queue head produced by serve_stream/4
 |	RestQ - remainder of the Queue following S
 *-------------------------------------------------------*/
disp_service_loop(Flag, S, QT, NewQT, RestQ)
	:-
	var(Flag),
	!,
	service_loop(RestQ,NewQT).

disp_service_loop(stop_yourself, S, QT, NewQT, RestQ)
	:-!,
	close_all(H1).

disp_service_loop(_, S, QT, NewQT, RestQ)
	:-
	service_loop(RestQ,NewQT).

/*--------------------------------------------------------
 |	serve_stream/4
 |	serve_stream(S, QT, NewQT, Flag) 
 |	serve_stream(+, +, -, -) 
 *-------------------------------------------------------*/
	%% Poll stream to see if anything is ready:
serve_stream(S, QT, NewQT, Flag) 
	:-
			%info('examining %t\n',[S]),
	get_stream_to_poll(S,PollStream),
	poll(PollStream,10000/*00*/),	/* timeout is .01 sec */
	!,
	serve_ready_stream(S, QT, NewQT, Flag).

	%% Nothing ready on S; in this case, push S on
	%% the _END_ of the queue by instantiating QT to
	%% [S | NewQT], and returning NewQT as the new
	%% uninstantiated queue tail:
serve_stream(S, [S | NewQT], NewQT, _).

/*--------------------------------------------------------
 |	serve_ready_stream/4
 |	serve_ready_stream(S, QT, NewQT, Flag).
 |	serve_ready_stream(+, +, -, -).
 *-------------------------------------------------------*/
		%% accept a connection on socket s(S), and push
		%% the group c(...),s(S) on at the _END_ of the queue:
serve_ready_stream(s(S),[c(SR,SW,initial_state), s(S) | H], H,Flag) 
	:-!,
	info('Accepting a new connection.\n',[]),
	open(socket(clone,S),read,SR),
	accept_socket_connection(SR),
	open(socket(clone,SR),write,SW).

		%% Try to service a request on a socket:
serve_ready_stream(c(SR,SW,CurState), [c(SR,SW,NewState) | H], H,Flag) 
	:-
	info('Servicing a request.\n',[]),
	service_request(SR,SW,CurState,NewState,Flag),
	!.

		%% Couldn't service the request on a socket,
		%% so respond to that by closing the socket:
serve_ready_stream(c(SR,SW,_), H, H,Flag) 
	:-
	info('Closing a connection\n',[]),
	close(SR),
	close(SW).

/*--------------------------------------------------------
 *-------------------------------------------------------*/
close_all(H1)
	:- 
	var(H1),!.

close_all([]).

close_all([s(S) | RH])
	:-
	close(S),
	close_all(RH).
close_all([c(SR,SW,_) | RH])
	:-
	close(SR),
	close(SW),
	close_all(RH).

/*--------------------------------------------------------
 *-------------------------------------------------------*/
get_stream_to_poll(S,SP) :-
    arg(1,S,SP).



	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% server specific code
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

service_request(SR,SW,State,State,Flag) 
	:-
	read(SR, Request),
	info('Request: %t\n',[Request]),
	handle_request(Request,SR,SW,Flag),
			%% can cause hang (e.g. mac <-> unix); fix uniform io:
	(dmember(Flag, [done,stop_yourself])  -> 
		true 
		; 
		skip(SR, 0'\n) 
	).

handle_request(end_of_file,SR,SW,Flag) 
	:- !, 
	handle_request(done,SR,SW,Flag).

	%% Word on this socket is finished:
handle_request(done,SR,SW,done) 
	:- !, 
	close(SR),
	close(SW).

handle_request(stop_yourself,SR,SW,stop_yourself) 
	:- !.

handle_request(Expression, SR,SW,Flag) 
	:-
	Result is Expression,
	!,
	printf(SW,'%t.\n',[Result]),
	flush_output(SW).

handle_request(Expression, SR,SW,Flag) 
	:-
	printf(SW,'malformed_expression(%t).\n',[Expression]),
	flush_output(SW).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Client code
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start_client(Host) 
	:-
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

start_client(Host) 
	:-
	info('Can\'t find server\n',[]).

sr(Exp)
	:-
	send_request(Exp).

send_request(Exp) 
	:-
	printf(wsock,'%t.\n',[Exp]),
	flush_output(wsock),
	read(rsock,Ans),
			%% can cause hang (e.g. mac <-> unix); fix uniform io:
	skip(rsock,0'\n),
	info('Answer is %t\n',[Ans]).

stop_client 
	:-
	close(rsock),
	close(wsock).

stop_server 
	:-
	printf(wsock,'stop_yourself.\n',[]),
	flush_output(wsock).

info(M,PL) 
	:- 
	printf(user_output,M,PL).

