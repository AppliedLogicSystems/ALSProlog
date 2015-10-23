/*======================================================================*
 | 			sktsvutl.pro 
 |		Copyright (c) 1995-96 Applied Logic Systems, Inc.
 |
 |		Utilities extracted from sktserve.pro, etc.:
 |		Common basic predicates for socket-based 
 |		server manager.
 |
 | Author: Ken Bowen
 | Started: November, 1995
 *======================================================================*/

module socket_comms.

export toggle_server_warnings/0.
export toggle_server_setting/1.

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%% ADMINISTRATION PREDICATES
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

toggle_server_warnings
	:-
	server_warnings_on,
	!,
	abolish(server_warnings_on,0).
						  
toggle_server_warnings
	:-
	assert( server_warnings_on ).

toggle_server_setting(Item)
	:-
	call(Item),
	!,
	abolish(Item,0).

toggle_server_setting(Item)
	:-
	assert(Item).

	/*-------------------------------------------------------------*
	 |	Server Error and Warning Notification
	 *-------------------------------------------------------------*/

	%% default at startup:
server_warnings_on.
	 
	%% default:
verbose_server.

export server_error/3.
server_error(MessageID, MsgArgs, SInfo)
	:-
	server_error_msg(MessageID, MsgFormat),
	(access_server_info(log_warnings, SInfo, true) ->
		log_server_notify(MsgFormat, MsgArgs, 'Error', SInfo)
		;
		true
	),
	access_server_info(local_write_stream, SInfo, LocalOut),
	printf(LocalOut, MsgFormat, MsgArgs), 
	nl(LocalOut), flush_output(LocalOut),
	!,
	fail.
					  
export server_warning/3.
server_warning(MessageID, MsgArgs, SInfo)
	:-
	(access_server_info(server_warnings, SInfo, true) ->
		server_warning_msg(MessageID, MsgFormat),
		access_server_info(local_write_stream, SInfo, LocalOut),
		printf(LocalOut, MsgFormat, MsgArgs),
		nl(LocalOut), flush_output(LocalOut)
		;
		true
	),
	(access_server_info(log_warnings, SInfo, true) ->
		server_warning_msg(MessageID, MsgFormat),
		log_server_notify(MsgFormat, MsgArgs, warning, SInfo)
		;
		true
	).

        %%---------------------
		%% ERROR MESSAGES
        %%---------------------

	%% server_error(no_ports,[]).
server_error_msg(no_ports, 'Error: No ports listing for listening!!').

	%% server_error(open_socket_fail,[Port])
server_error_msg(open_socket_fail, 'Error: Can''t open socket on port %t!').

	%% server_error(non_server_socket,[Port])
server_error_msg(non_server_socket, 'Error: Got non-server socket on port %t!').

	%% server_error(missing_cfg, [CfgFile])
server_error_msg(missing_cfg, 'Error: Can''t find configuration file: %t').

	%% server_error(unknown_exception, [Ball,Port,SID,IPNum])
server_error_msg(unknown_exception, 
	'Error: Unknown exception: ball=%t\n\tPort=%t\tsid=%t\tsrc ip=%t\n\n').

        %%---------------------
		%% WARNING MESSAGES
        %%---------------------

	%% server_warning(service_failure, [CloneID,Port])
server_warning_msg(service_failure, 
					'Service failure: clone %t on port %t!-Closing socket!').

	%% server_warning(queue_shutdown, [])
server_warning_msg(queue_shutdown, '!WARNING: Server shutting down!\n').

	%% server_warning(socket_started,[Port])
server_warning_msg(socket_started, 'Socket started on port %t').

	%% server_warning(remote_termination,[Password])
server_warning_msg(remote_termination,
				'!!Warning! Closing down due to remote termination from %t').

	%% server_warning(unknown_config, [Expr])
server_warning_msg(unknown_config, 'Warning: Unrecognized config file entry: %t').

	%% server_warning(dynamic_load_fail, [File], SInfo).
server_warning_msg(dynamic_load_fail, 'Dynamic load: can\'t find file: %t').

	%% server_warning(done_init, [], SInfo).
server_warning_msg(done_init, 'Init file read, and log file started...').

	%% server_warning(checking_workers, [], SInfo).
server_warning_msg(checking_workers, 'Checking for detached workers startup...').

	%% server_warning(workers_check_done, [], SInfo).
server_warning_msg(workers_check_done, '   ... workers check/startup done.').

        %%------------------------------
		%% Notification/info messages
        %%------------------------------

	%server_info_out('Connect: port=%t fid=%t', S, [], SInfo),
server_info_msg(new_connection, 'Connect: port=%t fid=%t').

	%server_info_out('Connect ok: From %t at %t on %t -port=%t fid=%t\n',
	%			[IPNum,Time,Date,Port,SID]),
server_info_msg(new_connection_fin,'Connect ok: From %t at %t on %t -port=%t fid=%t\n').

	%server_info_out('...clone: port=%t fid=%t\n', SR, [IP], SInfo),
server_info_msg(clone, '...clone: ip=%t port=%t fid=%t\n').

	%server_info_out('Request: port=%t fid=%t', SR,[], SInfo),
server_info_msg(request, 'Request: port=%t fid=%t').

	%% server_info_out(fin_request, SR, [Request], SInfo),
server_info_msg(fin_request, '...%t\n').

	%% server_info_out(break_conn, SR, [Date,Time,Reason], SInfo),
server_info_msg(break_conn, 'Shutdown connection: %t %t %t\n').

	%% server_info_out(spawn_attempt, 0, [HostName,WorkerPort,WID], SInfo),
server_info_msg(spawn_attempt, 'Spawn worker on %t at port %t wid=%t\n').

	%% server_info_out(worker_connect, NewRS, [WHost,WPort], SInfo),
server_info_msg(worker_connect, 'Worker on %t at port %t connecting...\n').

	%% server_info_out(worker_connect_done, NewRS, [WHost,WPort], SInfo),
server_info_msg(worker_connect_done, 'Worker on %t at port %t handshake complete.\n').

	%% server_info_out(obm_response, RS, [Response], SInfo),
server_info_msg(obm_response, 'OBM repsonse = $t.\n').

        %%---------------------
		%% server_info_out/4
        %%---------------------

export server_info_out/4.
server_info_out(InFormatCode, Socket, OtherArgs, SInfo) 
	:-
	(InFormatCode = continue(FormatCode) ->
		true
		;
		FormatCode = InFormatCode),
	access_server_info(log_info, SInfo, LogInfo),
	(verbose_server; LogInfo \= false),
	server_info_msg(FormatCode, MFormat),
	!,
	det_port_sid(Socket, ThePort, TheSID),
	append(OtherArgs, [ThePort,TheSID], AllArgs),
	access_server_info(local_write_stream, SInfo, LocalOutput),
	(verbose_server ->
		getpid(PID0), PID is floor(PID0), 
		access_server_info(host_name, SInfo, Host),
		catenate(['[',Host,'-',PID,']',MFormat],XMFormat),
		printf(LocalOutput,XMFormat, AllArgs),
		flush_output(LocalOutput)
		;
		true
	),
	server_info_logging(LogInfo,InFormatCode,FormatCode,MFormat,AllArgs,SInfo).

server_info_out(_, _, _, _). 

det_port_sid(Socket, 0, 0)
	:-
	var(Socket),!.
det_port_sid(0, 0, 0)
	:-!.
det_port_sid(Socket, ThePort, TheSID)
	:-
	socket_stream_info(Socket, _,ThePort,_,_),
	!,
	sio_fd(Socket, TheSID).
det_port_sid(_, 0, 0).

        %%------------------------------
		%% Log File
        %%------------------------------

log_server_notify(MsgFormat, MsgArgs, Type, SInfo)
	:-
	access_server_info(log_stream, SInfo, LogStream),
	date_time_seg(LogStream, Type,_,_),
	printf(LogStream, MsgFormat, MsgArgs),
	flush_output(LogStream).

server_info_logging(false,_,_,_,_,_) :-!.
server_info_logging(LogInfo,InFormatCode,FormatCode,MFormat,AllArgs,SInfo)
	:-
	access_server_info(log_stream, SInfo, LogStream),
	log_level(FormatCode,CodeLevel),
	(InFormatCode = continue(_) ->
		true
		;
		date_time_seg(LogStream, CodeLevel,_,_) 
	),
	printf(LogStream,MFormat, AllArgs),
	flush_output(LogStream).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% 		>> WARNING! <<
	%% Any new "connect" server_info codes added 
	%% must have an entry in log_level.
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

log_level(new_connection, connect) :-!.
log_level(new_connection_fin, connect) :-!.
log_level(clone, connect) :-!.
log_level(_, request).

date_time_seg(Stream_or_alias, Type, Date, Time)
	:-
	date(Date),
	time(Time),
	(is_stream(Stream_or_alias, Stream) ->
		printf(Stream, '\n[%t-%t] %t- ',[Date,Time,Type])
		;
		true
	).

	/*-------------------------------------------------------------
	 |	Stream polling & timeouts:
	 *-------------------------------------------------------------*/
export get_stream_to_poll/2.
get_stream_to_poll(local_job(_), _) 
	:-!,
	fail.

get_stream_to_poll(S,SP) 
	:-
    arg(1,S,SP).

export polling_timeout/2.
polling_timeout(Timeout, SInfo)
	:-
	access_server_info(polling_timeout, SInfo, Timeout),
	!.

	%%  Default:
	%%  10000 - timeout is .01 sec (on ....)
polling_timeout(10000, SInfo).

endmod.
