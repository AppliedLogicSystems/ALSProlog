

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%% Experimental Remote Execution:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-	make_gv("_rem_x_streams"), 	set_rem_x_streams( 0 ).

	%% Needed to be able to turn off remote execution:
shell_execute(InStream,OutStream,'$switch_remote_to_local',NamesOfVars,Vars,InWins,Status)
	:-!,
	get_rem_x_streams( Prev ),
	set_rem_x_streams( [0 | Prev ] ).

shell_execute(InStream,OutStream,InitGoal,NamesOfVars,Vars,InWins,Status)
	:-
	get_rem_x_streams( [(RemReadStream, RemWriteStream) | _] ),
	!,
	copy_term(Vars^InitGoal, OrigCopy),
	OrigCopy = CopyVars^CopyInitGoal,
	unique_ground(CopyVars),
	remote_exec(RemReadStream, RemWriteStream, OrigCopy, Vars, 
					OutStream, InitGoal, Status).

unique_ground([]).
unique_ground([V | Vars])
	:-
	gensym(sHrX,V),
	unique_ground(Vars).

remote_exec(RemReadStream, RemWriteStream, OrigCopy, Vars, 
				OutStream, InitGoal, Status)
	:-
	write(RemWriteStream, OrigCopy), put_code(RemWriteStream, 0'.),
	nl(RemWriteStream), flush_output(RemWriteStream),
	wait_for_remote(RemReadStream, Return),
	fin_rem_exec(Return,  Vars,  OutStream,  Status).

wait_for_remote(RemReadStream, Return)
	:-
	read(RemReadStream, RRSTerm),
	disp_wait_for_remote(RRSTerm, RemReadStream, Return).

disp_wait_for_remote(stream_not_ready, RemReadStream, Return)
	:-!,
	simple_select(QS, 0),
	wait_for_remote(RemReadStream, Return).

disp_wait_for_remote(Return, RemReadStream, Return).

fin_rem_exec(rem_exec_error(Pattern),  Vars,  OutStream,  Status)
	:-!,
	printf(OutStream, '!>RemEx Error: %t\n', [Pattern]),
	flush_output(OutStream).

fin_rem_exec(ReturnVars,  Vars,  OutStream,  Status)
	:-
	ReturnVars = Vars,
	!.

fin_rem_exec(Return,  Vars,  OutStream,  Status)
	:-!,
	printf(OutStream, '!!!!>BAD RemEx RETURN: %t\n', [Return]),
	flush_output(OutStream).

rem_x_restore
	:-
	get_rem_x_streams( [0 | Prev ] ),
	set_rem_x_streams( Prev ).

connect_to_server(Host,Port,ResultStatus)
	:-
	catch(open(socket(inet_stream,Host,Port),read,ClientReadSock,[]),
			_,
			ResultStatus=client_error(connection,[Host,Port])),
	(is_server_socket(ClientReadSock) ->
		close(ClientReadSock),
		ResultStatus=client_error(server_socket,[Host,Port])
		;
		open(socket(clone,ClientReadSock),write,ClientWriteSock,[)])
	),
	ResultStatus=socket_open([Host,Port],ClientReadSock,ClientWriteSock).

connect_rem_exec(Host,Port)
	:-
	connect_to_server(Host,Port,ResultStatus),
	disp_connect_rem_exec(ResultStatus,Host,Port).

disp_connect_rem_exec(socket_open([Host,Port],ClientReadSock,ClientWriteSock), Host,Port)
	:-!,
	get_rem_x_streams( Prev ),
	set_rem_x_streams( [ (ClientReadSock,ClientWriteSock) | Prev] ).

disp_connect_rem_exec(ResultStatus, Host,Port)
	:-
	printf('Connect Error: %t\n', [ResultStatus]),
	flush_output.


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%% END Experimental Remote Execution END
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
