/*============================================================================*
 |				gui_test_server.pro
 |		Copyright (c) 1994 Applied Logic Systems, Inc.
 |
 |	Server side of GUI/DB system test process
 |
 |	This program is invoked by the mail HITK Prolog process, either 
 |	on the local machine, or on some other remote machine [to be finished].
 |	This program executes the submitted goals coming from the invoking process.
 |
 |	Author: Ken Bowen, based on Kevin Buettner's mathserv?.pro examples.
 |	Date:	10 May 94
 *============================================================================*/

module work.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%% Entry Point for the Server Shell
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export start_sx_server_shell/0.
start_sx_server_shell
	:-
	start_sx_server_shell(true).

export start_sx_server_shell/1.
start_sx_server_shell(Cmd)
	:-
		check_for_debug,
	create_initial_server_socket(SR,Host,Port),
		trace_io('Init read socket: host=%t, port=%t\n',[Host,Port]),

		%% Send handshake info to initiating client:
	printf('handshake(%t,%t).\n',[Host,Port]),flush_output,
	accept_socket_connection(SR),
	open(socket(clone,SR),write,SW),

	Info = info(SR,SW,Host,Port),
		trace_io('Starting tserveloop\n',[]),
	call(Cmd),
	tserveloop(Info,SR,SW).

tserveloop(Info,SR,SW)
	:-
	get_it(SR,Request),
		trace_io('tserveloop_read=%t\n',[Request]),
	tserve_execute(Request,Info,SR,SW,Result),
		trace_io('executed %t --> %t\n',[Request,Result]),
	tserveloop(Info,SR,SW).

tserve_execute(end_of_file,Info,SR,SW,Result)
	:-
	printf(SW,'got_end_of_file.\n',[]),flush_output(SW),
		debug_halt,
	close(SR), close(SW),
	halt.

tserve_execute(halt,Info,SR,SW,Result)
	:-
	printf(SW,'halting.\n',[]),flush_output(SW),
		debug_halt,
	close(SR), close(SW),
	halt.

tserve_execute(Ctr-Goal,Info,SR,SW,success(Goal))
	:-
	call(Goal), 
	send_back(SW,Ctr,Goal).

tserve_execute(Ctr-Goal,Info,SR,SW,fail)
	:-
	printf(SW,'%t-fail.\n',[Ctr]),
	flush_output(SW).

	%% These are just hacks to enforce determinacy; 
	%% Double-check and/or FIX bread/2, printf, and flush_output.
	%% THEN GET RID
get_it(SR,Request)
	:-
	bread(SR, Request),
	!.

send_back(SW,Ctr,Goal)
	:-
	printf(SW,'%t-success((%t)).\n',[Ctr,Goal]),
	flush_output(SW),
	!.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%  Setting up duplex sockts  %%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

portnum(4321).

create_initial_server_socket(S,Host,Port)
	:-
	catch(open(socket(inet_stream,Host,Port),read,S,[snr_action(snr_code)]),
	      _,
	      fail),
	(is_server_socket(S) -> true ; close(S), fail).

create_clone_server_sockets(BaseSocket,SR,SW)
	:-
	open(socket(clone,BaseSocket),read,SR,[snr_action(snr_code)]),
	accept_socket_connection(SR),
	open(socket(clone,SR),write,SW).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%  Debugging:  trace file  %%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% The presence of -p .... -srvdbg [FileName] ...
	%% on the command line invoking this program will cause
	%% debugging to be turned on; debugger output is written
	%% to a file.  The default file name is  dbg.trc .  If
	%% '-srvdbg' is followed by an item which does NOT start
	%% with a '-', that item is taken to be the name of the
	%% debugging file.

check_for_debug
	:-
	builtins:command_line(CL),
		%% Succeeds if '-srvdbg' is on CL; CL_Tail follows 'debug':
	ck_cl_debug(CL, CL_Tail),
	!,
	ck_trace_name(CL_Tail, TraceFileName),
	open(TraceFileName,write,SDB, [alias(sdb)]),
	printf(sdb,'Background_server_started.\n',[]),flush_output(sdb).

check_for_debug.

ck_cl_debug(['-srvdbg' | Tail], Tail) :-!.
ck_cl_debug([_ | ThisTail], Tail)
	:-
	ck_cl_debug(ThisTail, Tail).

ck_trace_name([], 'sdb.trc').
ck_trace_name([Item | _], 'sdb.trc')
	:-
	atom_chars(Item, ['-' | _]),!.
ck_trace_name([Item | _], Item).

trace_io(Pattern,Args)
	:-
	printf(sdb,Pattern,Args),
	flush_output(sdb).

debug_halt
	:-
	printf(sdb,'halting.\n'),flush_output(sdb),
	close(sdb).

endmod.
