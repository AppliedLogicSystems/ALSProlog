/*======================================================================*
 | 			sktserve.pro 
 |		Copyright (c) 1995-96 Applied Logic Systems, Inc.
 |      Distribution rights per Copying ALS
 |
 |		Common basic predicates for socket-based 
 |		server manager.
 |
 | Author: Ken Bowen
 | Started: November, 1995
 |
 | Based on server portion of mathserv1.pro example by Kevin Buettner
 *======================================================================*/

module socket_comms.
use usradmn.

export start_server/0.
export start_server/1.
export server_info_out/4.
export log_server_notify/4.
export date_time_seg/4.

	/*!------------------------------------------------------------*
	 |	start_server/0.
	 |	start_server 
	 |	start_server 
	 |
	 |	- starts the server process
	 *-------------------------------------------------------------*/

start_server 
	:-
	init_server_info_and_queue(Queue, QueueTail, SInfo),
	!,
	catch(service_loop(Queue, QueueTail, SInfo),
			ServiceBall,
				%	'!ERROR: %t %t - Exiting due to service_error:\n\t%t\n',
			panic_exit(service_error,ServiceBall, SInfo) 
		 ).
start_server 
	:-
	write(total_panic_exit),nl, halt.

/*
start_server 
	:-
	server_warning(startup_error, [], SInfo),
	panic_exit(startup_error, '?', SInfo).
*/

	/*!------------------------------------------------------------*
	 |	start_server/5
	 |	start_server(Ports, Queue, QueueTail, StartedPorts, Sinfo)
	 |	start_server(+, +, -, -, +)
	 |
	 |	- starts the server on each port on the list Ports
	 *-------------------------------------------------------------*/

start_server([], [m(0) | QueueTail], QueueTail, [], _).
start_server([Port | Ports], QueueIn, QueueTail, [Port | Started], SInfo)
	:-
	start_server_on_port(Port, QueueIn, QueueInter, SInfo),
	!,
	start_server(Ports, QueueInter, QueueTail, Started, SInfo).

start_server([Port | Ports], QueueIn, QueueTail, Started, SInfo)
	:-
	start_server(Ports, QueueIn, QueueTail, Started, SInfo).

	/*!------------------------------------------------------------*
	 |	start_server_on_port/4
	 |	start_server_on_port(Port, Queue, QueueTail, SInfo)
	 |	start_server_on_port(+, +, -, +)
	 |
	 |	- starts the server on a given port
	 *-------------------------------------------------------------*/

start_server_on_port(Port, [SS | QueueOut], QueueOut, SInfo)
	:-
	(access_server_info(non_login_ports, SInfo, NonLoginPorts) ->
		true
		; 
		NonLoginPorts = []
	),
	(dmember(Port, NonLoginPorts) ->
		SS = n(Socket)
		;
		SS = s(Socket)
	),
	catch(open(socket(inet_stream,Host,Port),read,Socket,
							[read_eoln_type(lf),snr_action(snr_code)]),
			_,
			(server_error(open_socket_fail,[Port],SInfo), fail)
		  ),
	(is_server_socket(Socket) -> 
		server_warning(socket_started,[Port],SInfo)
		; 
		server_error(non_server_socket,[Port],SInfo),
		fail
	),
	!.

/*
	%% Failed to start it - skip on:
start_server_on_port(_, QueueTail, QueueTail, _).
*/

	/*!-------------------------------------------------------------
	 |	service_loop/3
	 |	service_loop(QueueHead,QueueTail,SInfo)
	 |	service_loop(+,+,+)
	 |
	 |	- the primary loop down the queue by the server
	 |
	 |	The pair (QueueHead,QueueTail) constitutes an extensible 
	 |	list (QueueTail is uninstantiated).  Items are pushed in
	 |	at the tail end of the queue by binding QueueTail to
	 |	[Item | NewQueueTail].  Items are popped off the queue
	 |	when QueueHead = [Item | RestQ] by replacing QueueHead
	 |	by RestQ in the next (recursive) call, and utilizing Item.
	 |
	 |	 -->service_loop([S | RestQ], QT, SInfo)
	 |	 |		|
	 |	 |		v
	 |	 |		serve_stream(S, QT, NewQT, SInfo)
	 |	 |		|
	 |	 |		v
	 |	 |		disp_service_loop(Flag, S, QT, NewQT, RestQ, SInfo)
	 |	 |		 |		|
	 |	 |		 |      |---------> [halt]
	 |	 |		 |
	 |	 ^		 v
	 |	 |--<-=	service_loop(RestQ, NewQT, SInfo)
	 |
	 |	The markers m(0), m(1) are used by the queue manager to
	 |	determine when it has made a pass through the entire queue
	 |	without locating any ready sockets; in that case, it enters
	 |	a simpile select on the list of all the open sockets & clones
	 *-------------------------------------------------------------*/

	%% In case queue ever becomes empty:
	%% [Remote requests for closing should only close clone sockets;
	%%  original basic sockets { s(S) or n(S) } should remain on queue ]

service_loop(Queue,_,_) :- 
	var(Queue),
	!.

service_loop([],_,_) :- !.

	%% First loop-thru marker; change & keep going:
service_loop([m(0) | RestQ], QT, SInfo)
	:-!,
	QT = [m(1) | NewQT],
	service_loop(RestQ, NewQT, SInfo).

	%% Second loop-thru marker; change & go to sleep
	%% until something comes in to wake us up:
service_loop([m(1) | RestQ], QT, SInfo)
	:-!,
	(streams_from(RestQ, QS) ->
		simple_select(QS, 0)
		;
		true
	),
	QT = [m(0) | NewQT],
	service_loop(RestQ, NewQT, SInfo).

	%% Local job to handle:
service_loop([local_job(Job) | RestQ], QT, SInfo)
	:-!,
	handle_local_job(Job, Flag, SInfo),
	disp_service_loop(Flag, S, QT, QT, RestQ, SInfo).

	%% Send back the current queue:
service_loop([current_queue(RestQ) | RestQ], QT, SInfo)
	:-!,
	disp_service_loop(Flag, S, QT, QT, RestQ, SInfo).

	%% Try to serve a stream:
service_loop([S | RestQ], QT, SInfo)
	:-
	serve_stream(S, QT, NewQT, Flag, SInfo),
	disp_service_loop(Flag, S, QT, NewQT, RestQ, SInfo).

	/*!-------------------------------------------------------------
	 |	streams_from/2
	 |	streams_from(Q, List)
	 |	streams_from(+, -)
	 |
	 |	- extract the underlying streams from the queue elements
	 |	- fails if any of the records on the queue has a fully
	 |	  delayed goal ("expect(V,G)") where V has been instantiated.
	 *-------------------------------------------------------------*/
streams_from(Q, [])
	:-
	var(Q),
	!.
			 
streams_from([], []).
			  
streams_from([m(_) | Queue], QS)
	:-!,
	streams_from(Queue, QS).
					   
streams_from([Item | Queue], [Stream | QS])
	:-
	not (Item = c(_,_,State,_),
			(arg(2, State, expect(Var, _)) ;
			 	arg(2, State, list_expect(Var, _))
			),
			nonvar(Var)
		),
	get_stream_to_poll(Item, Stream),
	streams_from(Queue, QS).

	/*!-------------------------------------------------------------
	 |	disp_service_loop/6
	 |	disp_service_loop(Flag, S, QT, NewQT, RestQ, SInfo)
	 |	disp_service_loop(+, +, +, +, +, +)
	 |
	 |	- dispatch predicate for main service loop
	 |
	 |	Flag  -  communicates information from a transaction
	 |			 directly to the server;
	 |	S	  -  stream just processed
	 |	QT	  -  incoming (original) queue tail
	 |	NewQT -  the new queue head produced by serve_stream/4
	 |	RestQ -  remainder of the Queue following S
	 *-------------------------------------------------------------*/
disp_service_loop(done, _, _, NewQT, RestQ, SInfo)
	:-!,
	service_loop(RestQ,NewQT, SInfo).

disp_service_loop(stop_yourself, S, _, _, RestQ, SInfo)
	:-!,
	set_server_info(server_warnings, SInfo, true),
	server_warning(queue_shutdown, [], SInfo),
	shutdown_server([S | RestQ], SInfo).

	%% fake the second clause of serve_stream here:
disp_service_loop(bad_request, S, QT, NewQT, RestQ, SInfo)
	:-!,
	QT = [S | NewQT],
	service_loop(RestQ, NewQT, SInfo).

	%% (Re)-submitted jobs:
disp_service_loop(add_to_queue(Job), _, QT, NewQT, RestQ, SInfo)
	:-!,
	QT = [Job | NewQT],
	service_loop(RestQ, NewQT, SInfo).

disp_service_loop(_, _, _, NewQT, RestQ, SInfo)
	:-
	service_loop(RestQ,NewQT, SInfo).

	/*!-------------------------------------------------------------
	 |	serve_stream/5
	 |	serve_stream(S, QT, NewQT, Flag, SInfo) 
	 |	serve_stream(+, +, -, -, +) 
	 |	
	 |	- services the stream S
	 |
	 |	1. Polls S to see if ready; if so, acts on that stream
	 |	2. If S not ready, moves S to end of queue and continues
	 *-------------------------------------------------------------*/

	%% Poll stream to see if anything is ready:
serve_stream(S, QT, NewQT, Flag, SInfo) 
	:-
	polling_timeout(Timeout, SInfo),
	get_stream_to_poll(S, PollStream),
	poll(PollStream, Timeout),	
	catch(serve_ready_stream(S, QT, NewQT, Flag, SInfo),
			Anything, 
			fail),
	!.

	%% See if there is a delayed job on S ready to run:
serve_stream(S, QT, NewQT, Flag, SInfo) 
	:-
	S = c(SR,SW,State,ConnType),
	( (arg(2, State, expect(Var, Goal)), nonvar(Var) ) ;
	 	(arg(2, State, list_expect(ListOfVars, Goal)), all_instantiated(ListOfVars) )
	),
	catch(serve_ready_stream(wc(SR,SW,State,ConnType), QT, NewQT, Flag, SInfo),
			Anything, 
			fail),
	!.

	%% Nothing is ready on S; in this case, push S onto the _END_ of the 
	%% queue by instantiating QT to [S | NewQT], and returning NewQT 
	%% as the new uninstantiated queue tail:

serve_stream(S, [S | NewQT], NewQT, continue, SInfo).

	/*!-------------------------------------------------------------
	 |	serve_ready_stream/5
	 |	serve_ready_stream(S, QT, NewQT, Flag, SInfo).
	 |	serve_ready_stream(+, +, -, -, +).
	 |
	 |	- serves sockets which are ready for reading
	 |
	 |	Basic sockets (set up when starting) are of the form s(S);
	 |
	 |	Clone sockets are of the form 
	 |
	 |		c(ReadSockec,WriteSocket,SocketState,ConnType)
	 |
	 |	ConnType = {login/non_login}
	 |
	 |	SocketState is for the use of the server application; it's
	 |	initial value is set to 
	 |
	 |			initial_state
	 |
	 |	Later, service_request/8 will recognize this as a special case,
	 |	and pass this to the application, which should take whatever
	 |	intialization steps are necessary for individual sockets.
	 |
	 |	If the ready socket is a basic socket on a port, accepts
	 |	the new connection requested on that socket, and sets up
	 |	the appropriate clone socket, push the group (clone and basic)
	 |	on the end of the queue;
	 |
	 |	If the ready socket is a clone socket, reads the message
	 |	from the socket, and acts on it by calling service_request/7.
	 *-------------------------------------------------------------*/
		%% accept a new connection on a basic socket s(S), and push
		%% the group c(SR,SW,InitState,ConnType) on at the _END_ 
		%% of the queue:
serve_ready_stream(SS,[c(SR,SW,InitState,ConnType), SS | NewQT], NewQT, Flag, SInfo) 
	:-
	functor(SS, CType, 1),
	arg(1, SS, S),
	!,
	server_info_out(new_connection, S, [], SInfo),
	open(socket(clone,S),read,SR,[read_eoln_type(lf),snr_action(snr_code)]),
	accept_socket_connection(SR, ConnectAddr),
	open(socket(clone,SR),write,SW,[write_eoln_type(lf)]),
	server_info_out(continue(clone), SR, [ConnectAddr], SInfo),
	(CType = n ->
		InitTag = non_login,
		ConnType = non_login
		; 
		InitTag = initial_state(ConnectAddr),
		ConnType = login
	),
	initial_state(ConnType,ConnectAddr,SR,SW,InitTag,InitState,Flag,SInfo).

		%% Try to service a reawakened job:
serve_ready_stream(wc(SR,SW,State,ConnType), QT, NewQT, Flag, SInfo) 
	:-
	disp_service_request(wake_job,SR,SW,State,ConnType,QT,NewQT,SInfo),
	!,
	arg(1, State, Flag).

		%% Try to service a request on a client clone socket:
serve_ready_stream(c(SR,SW,State,ConnType), QT, NewQT, Flag, SInfo) 
	:-
	service_request(SR,SW, State, ConnType, QT,NewQT, SInfo),
	!,
	arg(1, State, Flag).

		%% Try to service a request on a worker socket:
serve_ready_stream(QueueRec, QT, NewQT, Flag, SInfo) 
	:-
	functor(QueueRec, wkr_rec, _),
	!,
	service_worker(QueueRec, QT, NewQT, Flag, SInfo).

		%% Couldn't service the request on a socket,
		%% so respond to that by closing the socket:
serve_ready_stream(c(SR,SW,State,ConnType), QT, QT,done, SInfo) 
	:-
	socket_stream_info(SR, Host,Port,Domain,Type),
	server_warning(service_failure, [Host,Port], SInfo),
	(ConnType = login,
		access_login_connection_info(logged_in, State, ID),
		ID \= nil,
		do_logout(SR, SW, State, SInfo, ID, Date, Time),
		!
		;
		true),
	(stream_open_status(SR, open) -> close(SR) ; true),
	(stream_open_status(SW, open) -> close(SW) ; true).

	/*!-------------------------------------------------------------
	 |	service_request/7
	 |	service_request(SR,SW,State,ConnType,QT,NewQT,SInfo) 
	 |	service_request(+,+,+,-,+,-,+) 
	 |
	 |	- obtains next service request and dispatches it
	 |
	 |	Input args:
	 |	----------
	 |	SR		- the read socket for this (clone) socket stream
	 |	SW		- the write socket for this (clone) socket stream
	 |	State	- the current (input) state of this comm channel
	 |	ConnType- the connection type: login/non_login
	 |	QT		- the tail of the server queue at outset of this transaction
	 |
	 |	Output args:
	 |	-----------
	 |	NewQT		- the tail of the server queue after this transaction
	 |
	 |	State
	 |	--------------------------
	 |	Application defined term; the only restriction is that the
	 |	server code initializes it to initial_state, so act_on_server_request/6
	 |	must be prepared for this value.
	 |
	 |	Flag Values:
	 |		-- treated by disp_service_loop/8:
	 |	-----------
	 |	variable - ignored
	 |	stop_yourself(Password) 
	 |
	 |	Note that the predicate
	 |		add_use/1
	 |	can be called to dynamically add a module to the use list
	 |	for this module (socket_comms), thus allowing one to easily
	 |	make act_on_server_request/6 visible to this server code.
	 *-------------------------------------------------------------*/

	%% General case: there is something in the socket:
service_request(SR,SW,State,ConnType,QT,NewQT,SInfo) 
	:-
	read(SR, Request),
	(Request \= stream_not_ready ->
		server_info_out(request, SR,[], SInfo),
		server_info_out(continue(fin_request), SR, [Request], SInfo)
		;
		true
	),
	!,
	disp_service_request(Request,SR,SW,State,ConnType,QT,NewQT,SInfo).

	/*!-------------------------------------------------------------
	 |	disp_service_request/8
	 |	disp_service_request(Request,SR,SW,State,ConnType,QT,NewQT,SInfo)
	 |	disp_service_request(+,+,+,+,-,+,-,+)
	 |
	 |	- outer dispatch predicate for servicing client requests
	 *-------------------------------------------------------------*/
	%% Somehow got a variable -- close this connection:
disp_service_request(Request,SR,SW,State,ConnType,QT,QT,SInfo)
	:-
	var(Request),
	!,
	break_connection(uninstatiated_request,SR,SW,State,SInfo),
	mangle(1, State, bad_request).

disp_service_request(end_of_file,SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
	disp_service_request(done,SR,SW,State,ConnType,QT,NewQT,SInfo).

	%% Client wants the connection closed NOW:
disp_service_request(done,SR,SW,State,ConnType,QT,QT,SInfo)
	:-!,
	(ConnType = login ->
		usrlog_script(silent_logout, [_,_,SInfo],[_,_,SR,SW,State], Script, _),
		xmake_tsk_env(TaskEnv, [no_worker,_,_,SR,SW,State]),
		run_script(Script,usradmn, TaskEnv,SInfo)
			;
			true
	).

	%% Stream is not ready; push it on the end of the queue:
disp_service_request(stream_not_ready,SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-!,
	QT = [c(SR,SW,State,ConnType) | NewQT].

	%% Explicit delaying of jobs by the queue manager; wake-up here:
	%% This is is the single-variable case:
disp_service_request(wake_job,SR,SW, State,ConnType,QT,NewQT,SInfo) 
	:-
%	(debug_wake_job -> trace ; true),
		%% State is REQUIRED to have slot #2 = the "expect" slot:
	arg(2, State, expect(Var,Goal)),
	nonvar(Var),
	!,
		%% Immediately reset the expect slot:
	mangle(2, State, nil),
		%% should melt the freeze:
	call(Goal),
	finish_server_request(wake_job,SR,SW,State,ConnType,QT,NewQT,SInfo).

	%% Explicit delaying of jobs by the queue manager; wake-up here:
	%% This is is the list-of-variables case:
disp_service_request(wake_job,SR,SW, State,ConnType,QT,NewQT,SInfo) 
	:-
%	(debug_wake_job -> trace ; true),
		%% State is REQUIRED to have slot #2 = the "expect" slot:
	arg(2, State, list_expect(ListOfVars,Goal)),
%	nonvar(Var),
	all_instantiated(ListOfVars),
	!,
		%% Immediately reset the expect slot:
	mangle(2, State, nil),
		%% should melt the freeze:
	call(Goal),
	finish_server_request(wake_job,SR,SW,State,ConnType,QT,NewQT,SInfo).

all_instantiated([]).
all_instantiated([Var | ListOfVars])
	:-
	nonvar(Var),
	all_instantiated(ListOfVars).

	%% Attempt to do our delays using a frozen Var:
disp_service_request(Request,SR,SW, State,ConnType,QT,NewQT,SInfo) 
	:-
		%% State is REQUIRED to have slot #2 = the "expect" slot:
	arg(2, State, expect(Var)),
	!,
		%% Immediately reset the expect slot:
	mangle(2, State, nil),
		%% should melt the freeze:
	Var = Request,
	finish_server_request(Request,SR,SW,State,ConnType,QT,NewQT,SInfo).

	%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% LOGIN/LOGOUT
	%%%%%%%%%%%%%%%%%%%%%%%%%%

disp_service_request(Request,SR,SW, State,ConnType,QT,NewQT,SInfo) 
	:-
	usrlog_script(Request, [ID,PW,SInfo],[_,_,SR,SW,State], Script, _),
	!,
	xmake_tsk_env(TaskEnv, [no_worker,_,_,SR,SW,State]),
	run_script(Script,usradmn, TaskEnv,SInfo),
	finish_server_request(Request,SR,SW,State,login,QT,NewQT,SInfo).

/*********************************
	%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% SUB-JOB CONTROL
	%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% Request for job control:
disp_service_request(job_control(start,Request,Vars),SR,SW,State,
						login,QT,NewQT,Flag,SInfo) 
	:-!,
	access_login_connection_info(logged_in, State, ID),
	ID \= nil,
	job_control(start,Request,Vars,SR,SW,State,QT,NewQT,Flag,SInfo).

disp_service_request(job_finished(Status,Cookie,Vars),SR,SW,State,
						rem_job,QT,NewQT,Flag,SInfo)
	:-!,
	rem_job_finished(Status,Cookie,Vars,SR,SW,State,QT,NewQT,Flag,SInfo).

disp_service_request(request_received,SR,SW,JobInfo, waiting_start_confirm,
						QT,NewQT,Flag,SInfo)
	:-!,
	check_start_confirm(SR,SW,JobInfo,QT,NewQT,Flag,SInfo).
*********************************/

	%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% GENERAL ADMINISTRATION
	%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% Is the Request a general server administration request:
disp_service_request(Request,SR,SW, State,ConnType,QT,NewQT,SInfo) 
	:-
	trusted_administration(Request, State, SInfo),
	administration_task(Request, SR,SW, State,ConnType,QT,NewQT,SInfo),
	!.

	%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% WORKER COMMUNICATION
	%%%%%%%%%%%%%%%%%%%%%%%%%%

		%%+++++++++++++++++++++++++++++++++
		%% Initial connection/handshake
		%%+++++++++++++++++++++++++++++++++

	%% Reqest from worker to server for initial connection:
	%% [request sent by slave worker; response by master server]:
	%% [ master server <-- slave worker ]:
		%% Request = worker_connect_init(ID,WHost,WPort),
disp_service_request(Request,SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
	functor(Request,worker_connect_init,_),
	!,
		%% See wkr_mgmt.pro:
	init_worker_state(Request,SR,SW,State,ConnType,SInfo,NewSR,NewSW,NewState),
	finish_server_request(Request,NewSR,NewSW,NewState,login,QT,NewQT,SInfo).

	%% Acknowledgement of worker install from server back to worker:
	%% [message sent by master server; response by slave worker]:
	%% [ master server --> slave worker ]:
disp_service_request(worker_installed,SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
	mangle(1, State, done),
	retract(waiting_for_handshake),
	access_login_connection_info(port, State, WPort),
	access_login_connection_info(ip_num, State, WIPNum),
	printf(SW, 'worker_handshake_done(\'%t\',%t).\n', [WIPNum,WPort]),
	flush_output(SW),
	finish_server_request(worker_installed,SR,SW,State,login,QT,NewQT,SInfo).

	%% Completion of server/worker handshake:
	%% [message sent by slave worker; no response by master server]:
	%% [ master server <-- slave worker ]:
disp_service_request(worker_handshake_done(WHost,WPort),SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
	mangle(1, State, done),
	server_info_out(worker_connect_done, SR, [WHost,WPort], SInfo),
	finish_server_request(handshake_done,SR,SW,State,login,QT,NewQT,SInfo).

		%%+++++++++++++++++++++++++++++++++
		%% All other worker comms:
		%% 	[See wkr_mgmt.pro]:
		%%+++++++++++++++++++++++++++++++++

disp_service_request(Request,SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
	worker_comm_req(Request),
	!,
	disp_worker_service_request(Request,SR,SW,State,ConnType,QT,NewQT,SInfo),
	finish_server_request(Request,SR,SW,State,ConnType,QT,NewQT,SInfo).


	%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% GENERAL REQUESTS
	%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% General application request - pass to act_on_server_request/7:
disp_service_request(Request,SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
	fin_disp_service_request(ConnType,Request,SR,SW,State,QT,NewQT,SInfo).

	/*!-------------------------------------------------------------
	 | fin_disp_service_request/8
	 | fin_disp_service_request(ConnType,Request,SR,SW,State,QT,NewQT,SInfo)
	 | fin_disp_service_request(+,+,+,+,+,+,-,+)
	 |
	 |	- completion of disp_service_request/8
	 *-------------------------------------------------------------*/
fin_disp_service_request(outbound_attempt,Request,SR,SW,State,QT,NewQT,SInfo)
	:-!,
	check_outb_atmpt(Request,SR,SW,State,QT,NewQT,SInfo).

fin_disp_service_request(non_login,Request,SR,SW,State,QT,QT,SInfo)
	:-!,
	act_on_server_request(Request,SR,SW,State,_,SInfo),
	put_code(SW,0'\n), 
	flush_output(SW),
	close(SR), close(SW).

fin_disp_service_request(login,Request,SR,SW,State,QT,NewQT,SInfo)
	:-!,
	act_on_server_request(Request,SR,SW,State,SInfo),
	finish_server_request(Request,SR,SW,State,login,QT,NewQT,SInfo).

	/*!-------------------------------------------------------------
	 *-------------------------------------------------------------*/

finish_server_request(Request,SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
	arg(1, State, Flag),
	((nonvar(Flag),Flag = continue) -> true ; mangle(1, State, continue) ),
	do_fin_server_req(Flag,Request,SR,SW,State,ConnType,QT,NewQT,SInfo).

	/*!-------------------------------------------------------------
	 *-------------------------------------------------------------*/

do_fin_server_req(Flag,Request,SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
	var(Flag),
	!,
	QT = [c(SR,SW,State,ConnType) | NewQT],
	arg(1, State, continue).

do_fin_server_req(continue,Request,SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
	QT = [c(SR,SW,State,ConnType) | NewQT].

do_fin_server_req(done,Request,SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
	QT = NewQT.

do_fin_server_req(add_to_queue(current_queue(Q)),Request,
					SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
	QT = [c(SR,SW,State,ConnType), current_queue(Q)  | NewQT],
	mangle(1, State, continue).

do_fin_server_req(add_to_queue(local_job(Job)),Request,
					SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
	QT = [c(SR,SW,State,ConnType), local_job(Job)  | NewQT],
	mangle(1, State, continue).

do_fin_server_req(add_to_queue(Rec),Request,SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
	QT = [Rec, c(SR,SW,State,ConnType) | NewQT],
	mangle(1, State, continue).

	/*--------------------------------------------------------------
	 |		CLOSING DOWN THE QUEUE
	 *-------------------------------------------------------------*/
close_all(Queue, _)
	:- 
	var(Queue),!.

close_all([], _).

close_all([c(SR,SW,State,_) | RQ], SInfo)
	:-!,
	break_connection(queue_shutdown,SR,SW,State,SInfo),
	close_all(RQ, SInfo).

close_all([wc(SR,SW,State,_) | RQ], SInfo)
	:-!,
	break_connection(queue_shutdown,SR,SW,State,SInfo),
	close_all(RQ, SInfo).

close_all([QueueRec | RQ], SInfo)
	:-
	functor(QueueRec, wkr_rec, _),
	!,

	close_all(RQ, SInfo).

close_all([SS | RQ], SInfo)
	:-
	arg(1, SS, S0),
	sio:is_stream(S0,S),
	!,
	(stream_open_status(S, open) -> catch(close(S),_,true) ; true),
	close_all(RQ, SInfo).

close_all([_ | RQ], SInfo)
	:-!,
	close_all(RQ, SInfo).

	/*-------------------------------------------------------------*
	 |	default_portnums/1.
	 |	-	Default port numbers clients can connect on.
	 |	Normally, port numbers for connection should be set in the
	 |	initialization file.
	 *-------------------------------------------------------------*/
default_portnums(6000).

	/*!-------------------------------------------------------------
	 |	init_server_info/2
	 |	init_server_info(Ports, SInfo)
	 |	init_server_info(-, -)
	 |
	 |	- initialize server info (SInfo), by reading a config file
	 |
	 |	initialize_server/2 is defined in sktsvlib.pro
	 *-------------------------------------------------------------*/

init_server_info(Ports, SInfo)
	:-
	initialize_server(Ports, SInfo),
	!,
	(nonvar(Ports) ->
		true
		;
		bagOf(Port, default_portnums(Port), Ports)
	).

	%% default:
init_server_info(Ports, nil)
	:-
	bagof(Port, default_portnums(Port), Ports).

	/*!-------------------------------------------------------------
	 |	shutdown_server/2
	 |	shutdown_server(Queue, SInfo)
	 |	shutdown_server(+, +)
	 |
	 |	- close down server completely, exiting to prolog	
	 *-------------------------------------------------------------*/
shutdown_server(Queue, SInfo)
	:-
	close_all(Queue, SInfo),
	access_server_info(log_stream, SInfo, LogStream),
	(LogStream \= nil ->
		date(Date), time(Time),
		printf(LogStream, '\n===========Server log halted: %t  %t ========\n',
				[Date,Time]),
		close(LogStream)
		;
		true
	),
	kill_all_workers,
	halt.

	/*!------------------------------------------------------------*
		%	'!Exiting due to Error during startup:\n\t%t\n'
		panic_exit(startup_error, StartupBall, SInfo) 
	 *-------------------------------------------------------------*/

panic_exit(Which, Ball, SInfo)
	:-
	date(Date),time(Time),
	Message = '!ERROR: %t %t - Exiting due to %t:\n\t%t\n',
	close_down_everything(Message, [Time,Date,Which, Ball], SInfo).
				 
close_down_everything(Message, MsgArgs, SInfo)
	:-
	(access_server_info(log_warnings, SInfo, true) ->
		log_server_notify(Message, MsgArgs, 'Error', SInfo)
		;
		true
	),
	access_server_info(local_write_stream, SInfo, LocalOut),
	printf(LocalOut, Message, MsgArgs), 
	nl(LocalOut), 
	flush_output(LocalOut),

	kill_all_workers,
	close_down_streams(socket),
	close_down_streams(file),
	!,
	halt.

close_down_everything(_, _, _)
	:-
	halt.

endmod.		%% socket_comms

