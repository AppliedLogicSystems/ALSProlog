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
export log_server_notify/4.
export date_time_seg/4.

	%% Should only be used when servicing a sigalarm interrupt
	%% (by a worker process) to process incoming messages during
	%% a deep computation:

	%% set_mrq_channel(qc(SR,SW,State,SInfo))
:-	make_gv('_mrq_channel').
:-	make_gv('_process_status').

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
		catch(simple_select(QS, 0),BALL,grider(BALL))
		;
		true
	),
	QT = [m(0) | NewQT],
	service_loop(RestQ, NewQT, SInfo).

	%% Need to convert this to a server_out_warning message:
grider(BALL) :- pbi_write(grider(BALL)),pbi_nl,pbi_ttyflush.

	%% Local job to handle:
service_loop([local_job(Job) | RestQ], QT, SInfo)
	:-!,
	handle_local_job(Job, Flag, SInfo),
	disp_service_loop(Flag, S, QT, QT, RestQ, SInfo).

	%% Send back the current queue:
service_loop([current_queue(RestQ) | RestQ], QT, SInfo)
	:-!,
	disp_service_loop(done, S, QT, QT, RestQ, SInfo).

service_loop([check_batch_status(UserName, JobID, SR, SW, State, Status) | RestQ], 
				QT, SInfo)
	:-!,
	handle_batch_status_request(UserName, JobID, SR, SW, State, Status,
									RestQ, QT, NewQT, SInfo),
	disp_service_loop(done, S, QT, NewQT, RestQ, SInfo).

	%% Try to serve a stream:
service_loop([S | RestQ], QT, SInfo)
	:-
	serve_stream(S, QT, NewQT, Flag, SInfo),
	disp_service_loop(Flag, S, QT, NewQT, RestQ, SInfo).

	%%--------------------------------------------------------
	%%	Request for checking batch status
	%%--------------------------------------------------------

	%% Case: there is an entry for JobID in JobConnects; query Worker:
handle_batch_status_request(UserName, JobID, SR, SW, State, Status,
									RestQ, QT, NewQT, SInfo)
	:-
	access_login_connection_info(job_connects,State,JobConnects),
	dmember(JobID-Worker, JobConnects),
	!,
	perform_status_query(current_status, Status, UserName, JobID, 
						 Worker, SR,SW,State,login,QT,NewQT,SInfo).

	%% Case: no entry for JobID in JobConnects; see if out file exists:
handle_batch_status_request(UserName, JobID, SR, SW, State, Status,
									RestQ, QT, NewQT, SInfo)
	:-
	batch_outfile_exists(State, UserName, JobID),
	Status = finished,
	!,
	QT = [c(SR,SW,State,ConnType) | NewQT].

	%% Case: no info found for JobID:
handle_batch_status_request(UserName, JobID, SR, SW, State, Status,
									RestQ, QT, NewQT, SInfo)
	:-
	Status = no_info,
	QT = [c(SR,SW,State,ConnType) | NewQT].

perform_status_query(Atom, ResultVar, UserName, JobID, Worker,
						 SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
	Worker = c(WR,WW,WState,WConn),
	add_to_expect(obm(Atom,ResultVar), WState),
	QT = [c(SR,SW,State,ConnType) | NewQT],
	printf(WW, '%t.\n', [Atom],[quoted(true)]),
	flush_output(WW).

	/*!-------------------------------------------------------------
		Routines to manipulate the "expect" slot of State records;
		Note that the "expect" slot is slot #2: clogintp.typ
	 *-------------------------------------------------------------*/

non_empty_expect(State)
	:-
	arg(2, State, [_ |_ ]).

no_ready_expect(State)
	:-
	arg(2, State, Expect),
	no_ready_terms(Expect).

no_ready_terms([]).
no_ready_terms([Head | Tail])
	:-
	arg(1, Head, Var),
	var(Var),
	no_ready_terms(Tail).

some_ready_expect(State)
	:-
	arg(2, State, Expect),
	some_ready_term(Expect).

some_ready_term([Term | _])
	:-
	arg(1, Term, Var),
	nonvar(Var), !.

some_ready_term([_ | Tail])
	:-
	some_ready_term(Tail).

add_to_expect(Item, State)
	:-
	arg(2,    State, PrevExpect),
	mangle(2, State, [Item | PrevExpect]).

remove_expecting(Item, State)
	:-
	arg(2,    State, Expecting),
	start_find_and_delete(Expecting, Item, EH, EH, State).

start_find_and_delete([], _, _, _, _)
	:-!,
	fail.

start_find_and_delete([Item | TailExpecting], Item, ExpHead, ExpHeadTail, State)
	:-!,
	ExpHeadTail = TailExpecting,
	mangle(2, State, ExpHead).

start_find_and_delete(Expecting, Item, [Skip | RestEH], EHT, State)
	:-
	Expecting = [Skip | TailExpecting],
	find_and_delete(TailExpecting, Expecting, Item, RestEH, EHT, State).

find_and_delete([Item | TailExpecting], PrevExpecting, Item, EH, EHT, State)
	:-!,
	EHT = TailExpecting,
	mangle(2, PrevExpecting, EH).

find_and_delete(Expecting, PrevExpecting, Item, [Skip | RestEH], EHT, State)
	:-
	Expecting = [Skip | TailExpecting],
	find_and_delete(TailExpecting, Expecting, Item, RestEH, EHT, State).

%% Note: No case for arg 1 = [] because we want it to fail then.

remove_ready_expecting(State, Item)
	:-
	arg(2, State, Expecting),
	start_find_and_delete_ready(Expecting, Item, EH, EH, State).

start_find_and_delete_ready([], _, _, _, _)
	:-!,
	fail.

start_find_and_delete_ready([Head | TailExpecting], Item, EH, EHT, State)
	:-!,
	arg(1, Head, VV),
	nonvar(VV),
	Item = Head,
	EHT = TailExpecting,
	mangle(2, State, EH).

start_find_and_delete_ready(Expecting, Item, [Skip | RestEH], EHT, State)
	:-
	Expecting = [Skip | TailExpecting],
	find_and_delete_ready(TailExpecting, Expecting, Item, RestEH, EHT, State).

find_and_delete_ready([Head | TailExpecting], PrevExpecting, Item, EH, EHT, State)
	:-!,
	arg(1, Head, VV),
	nonvar(VV),
	Item = Head,
	EHT = TailExpecting,
	mangle(2, PrevExpecting, EH).

find_and_delete_ready(Expecting, PrevExpecting, Item, [Skip | RestEH], EHT, State)
	:-
	Expecting = [Skip | TailExpecting],
	find_and_delete(TailExpecting, Expecting, Item, RestEH, EHT, State).





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
/*
	not (Item = c(_,_,State,_),
			(arg(2, State, expect(Var, _)) ;
			 	arg(2, State, list_expect(Var, _))
			),
			nonvar(Var)
		),
*/
	(Item = c(_,_,State,_) -> no_ready_expect(State) ; true),
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
/*
	( (arg(2, State, expect(Var, Goal)), nonvar(Var) ) ;
	 	(arg(2, State, list_expect(ListOfVars, Goal)), all_instantiated(ListOfVars) )
	),
*/
	some_ready_expect(State),
	
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

/*
		%% receive an out-of-band message response:
serve_ready_stream( obm(SR,SW,Atom,ResultVar,JobID,State), QT, NewQT, Flag, SInfo) 
	:-
	read(SR, Request),
	(Request \= stream_not_ready ->
		server_info_out(obm_response, S, [Response], SInfo),
		ResultVar = Request,
		QT = NewQT
		;
		QT = [ obm(SR,SW,Atom,ResultVar,JobID,State) | NewQT]
	),
	!,
	arg(1, State, Flag).
*/

/*
serve_ready_stream( Rec, QT, NewQT, Flag, SInfo) 
	:-
	try_obm(Rec, QT, NewQT, Flag, SInfo),
	!.

try_obm(Rec, QT, NewQT, Flag, SInfo)
	:-
	Rec = obm(SR,SW,Atom,ResultVar,JobID,State),
	read(SR, Request),
	(Request \= stream_not_ready ->
		server_info_out(obm_response, S, [Response], SInfo),
		ResultVar = Request,
		QT = NewQT
		;
		QT = [ obm(SR,SW,Atom,ResultVar,JobID,State) | NewQT]
	),
	!,
	arg(1, State, Flag).
*/






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
s_nr_break,
	QT = [c(SR,SW,State,ConnType) | NewQT].

s_nr_break.

	%% Receiving return value from job submitted to remote worker:
disp_service_request(Request,SR,SW, State,ConnType,QT,NewQT,SInfo) 
	:-
	functor(Request,worker_done,_),
%	arg(2, State, worker_wait(Value)),
	remove_expecting(worker_wait(V1, Values), State),
	server_info_out(continue(clone), SR, [ConnectAddr], SInfo),
	!,
		%% unify return value with the waiting variable 'Value':
	Request = worker_done([V1 | Values]),
%	mangle(2, State, nil),
	push_idle_worker(c(SR,SW,State,ConnType)),

		%% Get the invoking client info from the worker's State:
	access_login_connection_info(job_connects,State,[JobID-Client]),
	set_login_connection_info(job_connects,State,[]),

	Client = c(CSR,CSW,CState,CConnType),
	access_login_connection_info(job_connects,CState,CJobConnects),
	(dmember(JobID-WRR, CJobConnects) ->
		list_delete(CJobConnects,JobID-WRR,NewCJobConnects),
		set_login_connection_info(job_connects,CState, NewCJobConnects)
		;
		true
	),

		%% Now see if there are waiting jobs:
	(pop_next_job(ClientRec) ->
		ClientRec = c(NSR,NSW,NState,NConnType),
		pop_job_rec(Request, NState),
		remote_proc(Request,NSR,NSW,NState,NConnType,QT,NewQT,SInfo)
		;
		NewQT = QT
	).

	%% Receiving return value from obm message to worker:
disp_service_request(Request,SR,SW, State,ConnType,QT,NewQT,SInfo) 
	:-
	functor(Request,obm,_),
	!,
obm_trace_point,
	Request = obm(Atom,Value),
	remove_expecting(obm(Atom,Value), State),
	server_info_out(continue(clone), SR, [ConnectAddr], SInfo),
	QT = [c(SR,SW,State,ConnType) | NewQT].

obm_trace_point.




	%% Explicit delaying of jobs by the queue manager; wake-up here:
	%% This is is the single-variable case:
disp_service_request(wake_job,SR,SW, State,ConnType,QT,NewQT,SInfo) 
	:-
		%% State is REQUIRED to have slot #2 = the "expect" slot:
%	arg(2, State, expect(Var,Goal)),
%	nonvar(Var),

%	arg(2, State, Exp),
%write(user_output,wake_job_disp=Exp),nl(user_output),flush_output(user_output), 
	functor(Item, expect, 3),
	remove_ready_expecting(State, Item),
	Item = expect(Var,Vs, Goal),
	!,
		%% Immediately reset the expect slot (now done by remove_ready_expecting/2)
%	mangle(2, State, nil),
		%% should melt the freeze:
	call(Goal),
	finish_server_request(wake_job,SR,SW,State,ConnType,QT,NewQT,SInfo).

/*****************
	%% Explicit delaying of jobs by the queue manager; wake-up here:
	%% This is is the list-of-variables case:
disp_service_request(wake_job,SR,SW, State,ConnType,QT,NewQT,SInfo) 
	:-
		%% State is REQUIRED to have slot #2 = the "expect" slot:
	arg(2, State, list_expect(ListOfVars,Goal)),
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
*****************/

/***********
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
***********/

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
	%% LOGIN/LOGOUT (CLIENTS)
	%%%%%%%%%%%%%%%%%%%%%%%%%%

disp_service_request(Request,SR,SW, State,ConnType,QT,NewQT,SInfo) 
	:-
	usrlog_script(Request, [ID,PW,SInfo],[_,_,SR,SW,State], Script, _),
	!,
	xmake_tsk_env(TaskEnv, [no_worker,_,_,SR,SW,State]),
	run_script(Script,usradmn, TaskEnv,SInfo),
	finish_server_request(Request,SR,SW,State,login,QT,NewQT,SInfo).

	%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% WORKER COMMUNICATION
	%%%%%%%%%%%%%%%%%%%%%%%%%%

		%%+++++++++++++++++++++++++++++++++
		%% Initial connection/handshake
		%%+++++++++++++++++++++++++++++++++

	%%--------------------------------------------------------
	%% Reqest from master to independent (free-running) worker
	%% for adoption by the master; this clause implements the
	%% worker response:
	%% [ master server --> independent slave worker ]:
	%% Request = are_you_available(MasterIP,MasterPort,WID):
	%%--------------------------------------------------------
disp_service_request(Request,SR,SW,IState,IConnType,QT,NewQT,SInfo)
	:-
	functor(Request,are_you_available,_),
	!,
	Request = are_you_available(MasterIP,MasterPort,WID),
	set_server_info(master_host,SInfo,MasterIP),
	set_server_info(master_port,SInfo,MasterPort),
	set_server_info(worker_id,SInfo,WID),
	special_connect_job(MasterIP, MasterPort, 0, JobRec, SInfo),
	JobRec = c(MRS,MWS,State,CType),
	getpid(PID0), PID is floor(PID0),
	sio_gethostname(InitHN),
	gethostbyname(InitHN,_,_,[MyIP | _]),
	access_server_info(ports_list, SInfo, [MyPort|_]),
	printf(MWS, 'worker_connect_init(\'%t\',\'%t\',%t,%t).\n',
				[WID,MyIP,MyPort,PID]),
	flush_output(MWS),
	QT = [JobRec | NewQT],
	mangle(1, State, continue).

	%%--------------------------------------------------------
	%% Reqest from worker to server for initial connection:
	%% [request is sent by slave worker; this clause implements
	%% the response by master server; 
	%%		master server <-- slave worker ]:
	%% Request = worker_connect_init(ID,WHost,WPort,PID),
	%%--------------------------------------------------------
disp_service_request(Request,SR,SW,IState,IConnType,QT,NewQT,SInfo)
	:-
	functor(Request,worker_connect_init,_),
	!,
	Request = worker_connect_init(WID,WHost,WPort,PID),
	special_connect_job(WHost, WPort, PID, WkrRec, SInfo),
	WkrRec = c(WRS,WWS,State,CType),
	QT = [WkrRec | NewQT],
	mangle(1, State, continue),
		%% add to master list:
	add_worker(WkrRec),
		%% add to idle list:
	push_idle_worker(WkrRec),
		%% modify task_intf:use_workers status if this is 
		%% the first worker being started:
	(task_intf:use_workers ->
		true
		;
		task_intf:assert(use_workers)
	).

	%%--------------------------------------------------------
	%%	Request from master to slave to do some work;
	%%	reqest is sent by the master; this clause implements 
	%%	the worker response:
	%%	[ master server --> independent slave worker ]:
	%%
	%%	Request = sibling_job(OrigRequest, UserID, JobID),
	%%--------------------------------------------------------
disp_service_request(Request,SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
		%% 'sibling_job(%t,%t,%t).\n', [OrigRequest,UserID,JobID],
	functor(Request,sibling_job,_),
	!,
	set_mrq_channel(qc(SR,SW,State,SInfo)),
	Request = sibling_job(OrigRequest, UserID, JobID),
	set_login_connection_info(logged_in, State, UserID),
	xmake_tsk_env(TaskEnv, [no_worker,JobID,UserID,SR,SW,State]),
	OrigRequest =.. [TaskName | TaskArgs],
	access_server_info(users_area,SInfo,UsersArea),
	(var(UsersArea) -> UsersArea = users ; true),
	get_cwd(CurPath),
		%% 	CfgTerms = [],
		%% do_defaults(CfgTerms),
		%% do_logicals(CfgTerms),
	change_cwd(UsersArea),
	(exists_file(UserID) -> true ; make_subdir(UserID)),
	change_cwd(UserID),
	open(null_stream(foobar),write,LogS, []),
	set_login_connection_info(user_log_stream, State, LogS),

	setup_remote_script(OrigRequest,TaskEnv,ReqScript),

	run_script(ReqScript, Mod, TaskEnv, SInfo),
	change_cwd(CurPath),
	finish_server_request(Request,SR,SW,State,ConnType,QT,NewQT,SInfo).

setup_remote_script(Request,TaskEnv,ReqScript)
	:-
	task_intf:get_csp_script(Request, [], TaskEnv, Mod,  
								Prelude, Compute, Postlude),
	Postlude = [V1 | RestVs]^[_|_],
	!,
	NewTail = 
		[full_delay(V1)^
			respond('worker_done(%t).',[[V1 | RestVs]],[quoted(true)])],
	append(Compute, NewTail, ReqScript).

setup_remote_script(Request,TaskEnv,ReqScript)
	:-
	task_intf:get_csp_script(Request, [], TaskEnv, Mod,  
								Prelude, Compute, Postlude),
	NewTail = 
		[full_delay(V1)^
			respond('worker_done(%t).',[[V1 | RestVs]],[quoted(true)])],
	append(Compute, NewTail, ReqScript).

setup_remote_script(OrigRequest,TaskEnv,ReqScript)
	:-
	task_intf:get_csp_script(OrigRequest, [], TaskEnv, Mod, ReqScript0),
	ReqScript0 = ScriptHead+ScriptTail,
		%% Assumes ResponseScript is of form full_delay(V1)^[....,respond(...)]:
	ScriptTail = [V1 | RestVs]^[_|_],
	NewTail = 
		[full_delay(V1)^
			respond('worker_done(%t).',[[V1 | RestVs]],[quoted(true)])],
	append(ScriptHead, NewTail, ReqScript).


/************
	%%--------------------------------------------------------
	%%	Request for checking batch status
	%%--------------------------------------------------------

	%% Case: there is an entry for JobID in JobConnects; query Worker:
disp_service_request(check_batch_status(UserName, JobID, Status),
								SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
	access_login_connection_info(job_connects,State,JobConnects),
	dmember(JobID-Worker, JobConnects),
	!,
	SQ = current_status,
	perform_status_query(current_status, Status, UserName, JobID, 
						 SR,SW,State,ConnType,QT,NewQT,SInfo).

	%% Case: no entry for JobID in JobConnects; see if out file exists:
disp_service_request(check_batch_status(UserName, JobID, finished),
								SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
	batch_outfile_exists(State, UserName, JobID),
	!,
	QT = [c(SR,SW,State,ConnType) | NewQT].

	%% Case: no info found for JobID:
disp_service_request(check_batch_status(UserName, JobID, no_info),
								SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-!,
	QT = [c(SR,SW,State,ConnType) | NewQT].


perform_status_query(Atom, ResultVar, UserName, JobID, 
						 SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
	OBM = obm(SR,SW,Atom,ResultVar,JobID,State),
	QT = [OBM, c(SR,SW,State,ConnType) | NewQT],
	printf(SW, '%t.\n', [Atom],[quoted(true)]),
	flush_output(SW).
	
**********/




/****************-------**************
	%%--------------------------------------------------------
	%%	Request for checking multiple batch status
	%%--------------------------------------------------------
disp_service_request(check_batch_status(UserName, StatusList),
								SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-!,
	access_login_connection_info(job_connects,State,JobConnects),
	setup_worker_requests(JobConnects, Requests, ListOfResponses),
	mangle(2, State, list_expect(ListOfResponses, 
									bind_equal(ListOfResponses,StatusList)), 
	add_list_to_queue(Requests, QT,NewQT,SInfo).
	
bind_equal(X,X).
setup_worker_requests([], _, [], []).
setup_worker_requests([JobID-Worker | JobConnects], 
						[Req | Requests], [RV | Responses])
	:-
	Worker = c(WR,WW,WState,WConn),
	setup_worker_requests(JobConnects, Requests, Responses).
	
/*
	set_login_connection_info(job_connects,State,[JobID-Worker | PrevCJobConnects]),
	Worker = c(WR,WW,WState,WConn),
*/


/*
	S = c(SR,SW,State,ConnType),
	( (arg(2, State, expect(Var, Goal)), nonvar(Var) ) ;
	 	(arg(2, State, list_expect(ListOfVars, Goal)), all_instantiated(ListOfVars) )
*/
/*
do_fin_server_req(add_to_queue(current_queue(Q)),Request,
					SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
	QT = [c(SR,SW,State,ConnType), current_queue(Q)  | NewQT],
	mangle(1, State, continue).
*/

add_list_to_queue([], NewQT,NewQT,SInfo).
add_list_to_queue([Req | Requests], QT,NewQT,SInfo)
	:-
	QT = [Req | InterQT],
	add_list_to_queue(Requests, InterQT,NewQT,SInfo).

****************-------**************/

		%%+++++++++++++++++++++++++++++++++++++++++++++++
		%% Attempt to send request for remote processing:
		%%+++++++++++++++++++++++++++++++++++++++++++++++

disp_service_request(Request,SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
	remote_proc(Request,SR,SW,State,ConnType,QT,NewQT,SInfo).

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
fin_disp_service_request(non_login,Request,SR,SW,State,QT,QT,SInfo)
	:-!,
	act_on_server_request(Request,SR,SW,State,_,SInfo),
	put_code(SW,0'\n), 
	flush_output(SW),
	close(SR), close(SW).

fin_disp_service_request(login,Request,SR,SW,State,QT,NewQT,SInfo)
	:-!,
%	set_MainQ(q(QT,SInfo)),
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
	 |		SENDING A REQUEST FOR REMOTE PROCESSING
	 *-------------------------------------------------------------*/
remote_proc(Request,SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
	task_intf:use_workers,
	Request =.. [TaskName | TaskArgs],
		%% Policy for remote processing is set by application:
	task_intf:allow_remote(TaskName),
	fin_remote_proc(TaskName,TaskArgs,Request,SR,SW,State,ConnType,QT,NewQT,SInfo).

fin_remote_proc(TaskName,TaskArgs,Request,SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
		%% There is a worker which can perform the request:
	pop_idle_sibling_worker(Worker,SInfo),
	!,
	access_login_connection_info(logged_in,State,UserID),
	assign_job_id(Request, UserID, JobID),
	xmake_tsk_env(TaskEnv, [worker_job,JobID,UserID,SR,SW,State]),

	Client = c(SR,SW,State,ConnType), 
	Worker = c(WR,WW,WState,WConn),
		%% Enter the worker record in the invoking client's State:
	access_login_connection_info(job_connects,State,PrevCJobConnects),
	set_login_connection_info(job_connects,State,[JobID-Worker | PrevCJobConnects]),

		%% Enter the invoking client's record in the worker's State:
	set_login_connection_info(job_connects,WState,[JobID-Client]),

	QT = [Worker,Client | NewQT],

	setup_fin_remote_proc(Request, TaskEnv, V1, Values, State, SInfo, Mod, Prelude),

%	Value = [V1 | Values],
	add_to_expect( worker_wait(V1, Values), WState),
%	mangle(2, WState, worker_wait(Value)),


	run_script(Prelude, Mod, TaskEnv, SInfo),
	printf(WW, 'sibling_job(%t,%t,%t).\n', 
				[Request,UserID,JobID],[quoted(true)]),
	flush_output(WW).

	%% No workers available, but ' task_intf:use_workers' above states that
	%% application policy wants to use workers; so need to queue the
	%% request for later processing when a worker becomes available:
fin_remote_proc(TaskName,TaskArgs,Request,SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-
		%% Remove client (requesting) Rec from main queue:
	NewQT = QT,
		%% add Request to client Rec's waiting_job list:
	add_job_rec(Request, State),
		%% push client Rec onto global queue of rec's with waiting jobs:
	push_next_job(c(SR,SW,State,ConnType)).




/*
setup_fin_remote_proc(Request, TaskEnv, Value, State, SInfo, Mod, Prelude)
	:-
	task_intf:get_csp_script(Request, [], TaskEnv, Mod,  
								Prelude, Compute, Postlude),
	!,
	(Postlude = RespondVars^RespondScript ->
		RespondVars = [V1 | Vs],
		mangle(2, State, 
			expect(Value, 
				(Value=RespondVars,
					run_script(RespondScript, user, TaskEnv,SInfo))
			   ))
		;
		RespondScript=Postlude,
		mangle(2, State, 
			expect(true, run_script(RespondScript, user, TaskEnv,SInfo))
			  )
	).
*/

setup_fin_remote_proc(Request, TaskEnv, V1, Vs, State, SInfo, Mod, [])
	:-
	task_intf:get_csp_script(Request, [], TaskEnv, Mod, ReqScript0),
	ReqScript0 = ScriptHead+ScriptTail,
	ScriptTail = RespondVars^RespondScript,
	RespondVars = [V1 | Vs],

	add_to_expect( expect(V1, Vs,
					([V1 | Vs] = RespondVars,
						run_script(RespondScript, Mod, TaskEnv,SInfo))), State).
/*
	mangle(2, 
		State, 
		expect(Value, 
				(Value=RespondVars,
					run_script(RespondScript, Mod, TaskEnv,SInfo)))
			  ).
*/




	%% Needs real implementation:
pop_idle_sibling_worker(Worker,SInfo)
	:-
	pop_idle_worker(Worker).

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

