/*======================================================================*
 | 			jobserve.pro 
 |		Copyright (c) 1996 ChemSoft, Inc.
 |		Portions Copyright (c) 1993-6 Applied Logic Systems, Inc.
 |
 |		-- server layer for backend SPARC computation server
 |
 |	Start this with:
 |
 |		alspro jobserve -q -giac -g jobserve -p -port <Num> 
 |
 |		or
 |		
 |		image -q -giac -g jobserve -p -port <Num> 
 *======================================================================*/

module jobserve.
use socket_comms.
use user.

export jobserve/0.

jobserve 
	:-
	proc_jobserve_cmdline(CmdLine, CmdlinePort, Rexec, Dir),
		%% Who knows where we might start up when running under 
		%% an rexec?  So we pass the directory in which to actually
		%% run on the command line, and change to it right away:
	(Rexec = true -> change_cwd(Dir) ; true),
    ConfigFile = 'zparc.ini',
	make_server_info(SInfo),
	grab_terms(ConfigFile, ConfigsTerms),
	assert(configs_terms(ConfigsTerms)),
	process_server_config(ConfigsTerms, CmdLine, SInfo, [], _, ConfigsTermsPorts),
	!,
	determine_port(CmdlinePort, ConfigsTermsPorts, Port),
	set_server_info(ports_list, SInfo, [Port]),
	start_js_server(Port, Rexec, ConfigsTerms, SInfo).

proc_jobserve_cmdline(CmdLine,Port, Rexec, Dir)
	:-
	pbi_get_command_line(CmdLine),
	parse_the_cmds(CmdLine, Port, Rexec, Dir).

parse_the_cmds([], Port, Rexec, Dir)
	:-
	(var(Port) -> Port = 0 ; true),
	(var(Rexec) -> Rexec = false ; true).

parse_the_cmds(['-port', NumUIA | CmdLine], Port, Rexec, Dir)
	:-!,
	atomread(NumUIA, Port),
	parse_the_cmds(CmdLine, Port, Rexec, Dir).

parse_the_cmds(['-rexec' | CmdLine], Port, Rexec, Dir)
	:-!,
	Rexec = true,
	parse_the_cmds(CmdLine, Port, Rexec, Dir).

parse_the_cmds(['-dir', Dir | CmdLine], Port, Rexec, Dir)
	:-!,
	parse_the_cmds(CmdLine, Port, Rexec, Dir).


parse_the_cmds(['-localterm' | CmdLine], Port, Rexec, Dir)
	:-!,
	assert(localterm),
	parse_the_cmds(CmdLine, Port, Rexec, Dir).

parse_the_cmds([_ | CmdLine], Port, Rexec, Dir)
	:-
	parse_the_cmds(CmdLine, Port, Rexec, Dir).

determine_port(0, ConfigsTermsPorts, Port)
	:-!,
	ConfigsTermsPorts = [Port | _].

determine_port(Port, ConfigsTerms, Port).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% server 
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_js_server(Port, Rexec, ConfigsTerms, SInfo) 
	:-
	getpid(PID),
	open('jobsrv.log',append,_,[alias(jslog)]),
	printf(jslog,'==================================\n',[]),
	server_info_out(js_starting, 0, [PID,Port,Rexec], SInfo),
	!,
	start_js_server(35, PID, Port, Rexec, ConfigsTerms, SInfo).

start_js_server(Port, _, _, SInfo)
	:-
		%% info('server_failure(%t).\n',[Port]),
	server_info_out(js_failure, 0, [Port], SInfo),
	printf(jslog,'server_failure(%t).\n',[Port]),
	close(jslog),
	throw(quit).

start_js_server(0, PID, Port, Rexec, ConfigsTerms, SInfo)
	:-!,
	printf(jslog,'Unable to open socket on port %t...exiting -- counted to 0\n', [Port]),
	flush_output(jslog),
	close(jslog),
		%% 'Unable to open socket on port %t\n'
	server_info_out(js_no_socket, S, [Port, PID], SInfo),
	halt.

start_js_server(Count, PID, Port, Rexec, ConfigsTerms, SInfo) 
	:-
	Count > 0,
		%% info('job server starting [pid=%t, port=%, rexec=%t]...\n',[PID,Port,Rexec])
	server_info_out(js_starting_try, 0, [Count,PID,Port,Rexec], SInfo),

	catch(open(socket(inet_stream,Host,Port),read,S,
					[read_eoln_type(lf),snr_action(snr_code)]),
		Exception,
		except_start_js_server(Exception,Count,PID,Port, Rexec, ConfigsTerms, SInfo) 
	      
	),
	!,
	cont_start_js_server(S,PID,Host,Port, Rexec, ConfigsTerms, SInfo).

except_start_js_server(Exception,Count, PID, Port, Rexec, ConfigsTerms, SInfo) 
	:-
	server_info_out(skt_open_except, 0, [Exception,Count,PID,Port,Rexec], SInfo),
	printf(jslog,'Unable to open socket on port %t...sleeping - count = %t\n', [Port,Count]),
	system('sleep 5'),
	NextCount is Count -1,
	start_js_server(NextCount, PID, Port, Rexec, ConfigsTerms, SInfo).

cont_start_js_server(S,PID,Host,Port, Rexec, ConfigsTerms, SInfo) 
	:-
	is_server_socket(S),
	!,
	fin_start_js_server(S,PID,Host,Port, Rexec, ConfigsTerms, SInfo) .

cont_start_js_server(S,PID,Host,Port, Rexec, ConfigsTerms, SInfo) 
	:-
		date(Date),time(Time),
		%% info('NOT server socket \n',[PID,Host,Port,Date,Time])
	server_info_out(not_socket, 0, [PID,Host,Port,Date,Time], SInfo),
	close(S), 
	close(jslog),
	!,
	fail.

fin_start_js_server(S,PID,Host,Port, Rexec, ConfigsTerms, SInfo) 
	:-
	date(Date),time(Time),
		%% info('--Server started on host %s, port %d -%t %t\n',[Host,Port,Date,Time])
%	sio_gethostname(HostName),
	server_info_out(js_started, S, [Host,Port,Date,Time], SInfo),
	(Rexec = true ->
		printf(user_output, 'ready_for_business(%t).\n', [PID]),
		flush_output(user_output)
		;
		true
	),
	flush_output(user_output),
	catch(js_service_loop([s(S), m(0) | T],T, SInfo),
			_,
			panic_exit(SInfo)
	).

panic_exit(SInfo)
	:-
	date(Date),time(Time),
		%% info('Unknown error occurred at %t  %t ... exiting\n',[Time,Date]),
	server_info_out(js_panic, 0, [Time,Date], SInfo),
	close(jslog),
%	close_down_streams(socket),
	halt.

/*--------------------------------------------------------
 |	js_service_loop/3
 |	js_service_loop(QueueHead,QueueTail, SInfo)
 |	js_service_loop(+,+,+)
 |
 |	The pair (QueueHead,QueueTail) constitutes an
 |	extensible list (QueueTail is uninstantiated);
 *-------------------------------------------------------*/

	%% should never occur, but is here in case the queue
	%% somehow ever becomes empty:
js_service_loop([],_,_) :- !.

	%% First loop-thru marker; change & keep going:
js_service_loop([m(0) | RestQ], QT, SInfo)
	:-!,
	QT = [m(1) | NewQT],
	js_service_loop(RestQ, NewQT, SInfo).
				 
	%% Second loop-thru marker; change & go to sleep
	%% until something comes in to wake us up:
js_service_loop([m(1) | RestQ], QT, SInfo)
	:-!,
	streams_from(RestQ, QS),
	simple_select(QS, 0),
	QT = [m(0) | NewQT],
	js_service_loop(RestQ, NewQT, SInfo).
											  
js_service_loop([S | RestQ], QT, SInfo)
	:-
	serve_js_stream(S, QT, NewQT, Flag, SInfo),
	disp_js_service_loop(Flag, S, QT, NewQT, RestQ, SInfo).

/*--------------------------------------------------------
 |  streams_from/2
 |  streams_from(Q, List)
 |  streams_from(+, -)
 |
 |  - extract the underlying streams from the queue elements
 *-------------------------------------------------------*/
	   
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
	get_stream_to_poll(Item, Stream),
	streams_from(Queue, QS).

/*--------------------------------------------------------
 |	disp_js_service_loop/6
 |	disp_js_service_loop(Flag, S, QT, NewQT, RestQ, SInfo)
 |	disp_js_service_loop(+, +, +, +, +, +)
 |
 |	Flag  -  communicates information from a transaction
 |				directly to the server;
 |	S	  -  stream processed in this transaction
 |	QT	  -  the (uninstantiated) tail of the queue
 |	NewQT -  the new queue head produced by serve_js_stream/4
 |	RestQ - remainder of the Queue following S
 *-------------------------------------------------------*/
disp_js_service_loop(Flag, S, QT, NewQT, RestQ, SInfo)
	:-
	var(Flag),
	!,
	js_service_loop(RestQ,NewQT, SInfo).

disp_js_service_loop(stop_yourself, S, QT, NewQT, RestQ, SInfo)
	:-!,
		%% info('Closing down due to client command.\n',[]),
	server_info_out(js_client_close, 0, [], SInfo),
	close_all([S | RestQ]),
	throw(quit).


disp_js_service_loop(_, S, QT, NewQT, RestQ, SInfo)
	:-
	js_service_loop(RestQ,NewQT, SInfo).

/*--------------------------------------------------------
 |	serve_js_stream/5
 |	serve_js_stream(S, QT, NewQT, Flag, SInfo) 
 |	serve_js_stream(+, +, -, -, +) 
 *-------------------------------------------------------*/
	%% Poll stream to see if anything is ready:
serve_js_stream(S, QT, NewQT, Flag, SInfo) 
	:-
	polling_timeout(Timeout, SInfo),
	get_stream_to_poll(S,PollStream),
	poll(PollStream, Timeout),
	!,
	serve_ready_js_stream(S, QT, NewQT, Flag, SInfo).

	%% Nothing ready on S; in this case, push S on
	%% the _END_ of the queue by instantiating QT to
	%% [S | NewQT], and returning NewQT as the new
	%% uninstantiated queue tail:
serve_js_stream(S, [S | NewQT], NewQT, continue, SInfo).

/*--------------------------------------------------------
 |	serve_ready_js_stream/5
 |	serve_ready_js_stream(S, QT, NewQT, Flag, SInfo).
 |	serve_ready_js_stream(+, +, -, -, +).
 *-------------------------------------------------------*/
		%% accept a connection on socket s(S), and push
		%% the group c(...),s(S) on at the _END_ of the queue:
serve_ready_js_stream(s(S),[c(SR,SW,initial_state), s(S) | H], H,continue, SInfo) 
	:-!,
		%% info('Accepting a new connection.\n',[]),
	server_info_out(js_new_conn, 0, [], SInfo),
	open(socket(clone,S),read,SR,[read_eoln_type(lf),snr_action(snr_code)]),
	accept_socket_connection(SR),
	open(socket(clone,SR),write,SW,[write_eoln_type(lf)]).

serve_ready_js_stream(QueueRec, QT, NewQT, Flag, SInfo)
	:-
	get_stream_to_poll(QueueRec,PollStream),
	catch(read(PollStream, Term), 
		Ball, 
		abort_srs_read(Ball,SInfo)),
	!,
	disp_serve_ready_js_stream(Term, QueueRec, QT, NewQT, Flag, SInfo).

abort_srs_read(Ball,SInfo)
	:-
		%% info('Failure reading from stream: %t\n', [Ball]),
	server_info_out(js_read_fail, 0, [Ball], SInfo),
	!,
	fail.

		%% Couldn't service the request on a socket,
		%% so respond to that by closing the socket:
serve_ready_js_stream(c(SR,SW,_), H, H,done,SInfo) 
	:-
	date(Date),time(Time),
		%% info('Closing a connection\n',[]),
	server_info_out(break_conn, 0, [Date,Time,stream_service_failure], SInfo),
	(stream_open_status(SR, open) -> close(SR) ; true),
	(stream_open_status(SW, open) -> close(SW) ; true).

	%% Stream polled ok, but can't read a complete term:
disp_serve_ready_js_stream(stream_not_ready, QueueRec, QT, NewQT, Flag,SInfo)
	:-!,
	QT =  [QueueRec | NewQT],
	Flag = continue.

	%% Ok, we read a complete term from the stream:
disp_serve_ready_js_stream(Request, QueueRec, QT, NewQT, Flag,SInfo)
	:-
%info('Servicing request: %t\n',[Request]),
	QueueRec = c(SR,SW,State),
	service_js_request(Request, SR,SW,State,Flag,SInfo),
	((nonvar(Flag),Flag = done) ->
			%% Finished with this record; drop it out:
		QT = NewQT
		;
			%% Queue rec still active; push on end of queue:
		QT = [QueueRec | NewQT]
	).

/*--------------------------------------------------------
 *-------------------------------------------------------*/
close_all(H1)
	:- 
	var(H1),!.

close_all([]).

close_all([s(S) | RH])
	:-!,
	(stream_open_status(S, open) -> close(S) ; true),
	close_all(RH).
close_all([c(SR,SW,_) | RH])
	:-!,
	(stream_open_status(SR, open) -> close(SR) ; true),
	(stream_open_status(SW, open) -> close(SW) ; true),
	close_all(RH).
close_all([_ | RH])
	:-
	close_all(RH).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% server specific code
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

service_js_request(Request, SR,SW,State,Flag,SInfo) 
	:-
		%% info('Request: %t\n',[Request]),
	functor(Request, ReqF, ReqN),
	(ReqN > 0 -> arg(1,Request,ReqT) ; ReqT = ReqN),
	server_info_out(js_request, 0, [ReqT/ReqN], SInfo),
	handle_request(Request,SR,SW,Flag, SInfo),
	server_info_out(fin_request, 0, [Request], SInfo).

handle_request(end_of_file,SR,SW,Flag,SInfo) 
	:- !, 
	handle_request(done,SR,SW,Flag,SInfo).

	%% Work on this socket is finished:
handle_request(done,SR,SW,done,SInfo) 
	:- !, 
	(stream_open_status(SR, open) -> close(SR) ; true),
	(stream_open_status(SW, open) -> close(SW) ; true).


handle_request(stop_yourself,SR,SW,stop_yourself,SInfo) 
	:- !.

handle_request(are_you_ready,SR,SW,continue,SInfo) 
	:-!,
	flush_input(SR),
	getpid(PID),
	printf(SW,'ready_for_business(%t).\n', [PID]),
	flush_output(SW).

handle_request(Job, SR,SW,Flag,SInfo) 
	:-
	functor(Job,job_rec,_),
arg(1,Job,Task),
write(handle_request=Task),nl,
	configs_terms(CfgTerms),
	run_job(Job,SR,SW,Flag,CfgTerms,SInfo).

endmod.

module socket_comms.

	%% server_info_out(js_starting, 0, [PID,Port,Rexec], SInfo),
server_info_msg(js_starting, 'job server starting [pid=%t, port=%t, rexec=%t]...\n').

	%% server_info_out(js_starting_try, 0, [Count,PID,Port,Rexec], SInfo),
server_info_msg(js_starting_try, 
	'job server starting try: count=%t pid=%t port=%t rexec=%t ...\n').

	%% server_info_out(not_socket, S, [PID,Host,Port,Date,Time], SInfo),
server_info_msg(not_socket, 
	'NOT server socket-exiting: pid=%t host=%t port=%t date=%t time=%t\n').

	%% server_info_out(js_started, S, [Port,Date,Time], SInfo),
server_info_msg(js_started, '--Server started on host %s, port %d -%t %t\n').

	%% server_info_out(js_no_socket, S, [Port, PID], SInfo),
server_info_msg(js_no_socket, 'Unable to open socket on port %t [pid=%t]\n').

	%% server_info_out(js_failure, S, [Port], SInfo),
server_info_msg(js_failure, 'server_failure(%t).\n').

	%% server_info_out(js_panic, S, [Time,Date], SInfo),
server_info_msg(js_panic, 'Unknown error occurred at %t  %t ... exiting\n',[Time,Date]).

	%% server_info_out(js_client_close, S, [], SInfo),
server_info_msg(js_client_close, 'Closing down due to client command.\n').

	%% server_info_out(js_new_conn, S, [], SInfo),
server_info_msg(js_new_conn, 'Accepting a new connection.\n').

	%% server_info_out(js_read_fail, S, [Ball], SInfo).
server_info_msg(js_read_fail, 'Failure reading from stream: %t\n').

	%% server_info_out(js_request, S, [Task], SInfo).
server_info_msg(js_request, 'Client request: %t\n').

endmod.
