/*============================================================================*
 |				rex_client.pro
 |		Copyright (c) 1994 Applied Logic Systems, Inc.
 |
 |	Client side of client-server Prolog execution model		
 |
 |	This program spawns another (server) Prolog process, either on the local
 |	machine, or on a remote machine, and then runs a typical top-level
 |	Prolog shell with execution of submitted goals being carried out
 |	in the server process.
 |
 |	Author: Ken Bowen, based on Kevin Buettner's mathserv?.pro examples.
 |	Date:	10 May 94
 *============================================================================*/


		%% Test with execution process on the local machine:
sts :-
%	ShellCmd = 'alspro rex_server -g start_sx_server_shell -p -srvdbg', 
	ShellCmd = 'test_process -p -srvdbg', 
	start_test_shell(ShellCmd,[]).

		%% Remote test in Newton Centre:
sts2 :-
	rxs(mailbox,prolog,'/home3/work',logical).

		%% Test with execution process on a remote machine:
rxs(Machine,User,Dir)
	:-
	rxs(Machine,User,Dir,0).

rxs(Machine,User,Dir,Password)
	:-
	SC = ' ; alspro rex_server -g start_sx_server_shell -p -srvdbg',
	catenate(['cd ',Dir,SC],ShellCmd),
	(Password = 0 ->
		Opts = [host(Machine),username(User),password(Password)]
		;
		Opts = [host(Machine),username(User)]
	),
	start_test_shell(ShellCmd,Opts).

module work.

	/*---------------------------------
	 |  Start the test server process
	 *--------------------------------*/

export start_test_shell/2.
start_test_shell(ShellCmd,InRExecOpts)
	:-
	RExecOpts0 = [rstream(InitRS,[snr_action(snr_code)]),wstream(InitWS,[])],
	append(InRExecOpts, RExecOpts0, RExecOpts),
	rexec(ShellCmd, RExecOpts),
	setup_client_sockets(InitRS,RS,WS),

		%% Prime the pump & get going:
	printf(WS,'0-true.\n',[]),
	flush_output(WS),
	rxloop(RS,WS,0,[0-(true,[],[])]).

setup_client_sockets(InitRS,RS,WS)
	:-
	bread(InitRS, Next),
	Next = handshake(OtherHost,Port),
		printf('Foreground: Got handshake: host=%t port=%t\n',[OtherHost,Port]), 
		flush_output,
	start_client_sockets(OtherHost, Port, test_rs,RS, test_ws,WS).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%  Setting up duplex sockts  %%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_client_sockets(OtherHost, Port, RA,RS, WA,WS)
	:-
	catch(open(socket(inet_stream,OtherHost,Port),read,
				RS,[alias(RA),snr_action(snr_code)]),
	      _,
	      fail),
	(is_server_socket(RS) -> 
	    close(RS),
	    fail
		;
	    open(socket(clone,RS),write,WS,[alias(WA)])
	).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Outer Loop for Shell:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rxloop(RS, WS, Ctr, Stack)
	:-
write(rxloop),pbi_nl,pbi_ttyflush,
	read_term(RS, Return, [blocking(true)]),
pbi_write(rxloop_GOTTERM),pbi_nl,pbi_ttyflush,
	NextCtr is Ctr+1,
	rxloop_return(Return, RS, WS, NextCtr, Stack).

rxloop_return(stream_not_ready(_), RS, WS, Ctr, Stack)
	:-!,
	rxloop(RS, WS, Ctr, Stack).

rxloop_return(Id-Return, RS, WS, Ctr, Stack)
	:-
	extract_stack(Stack,Id,StackItem,RestStack),
	rx_dispose(Return,Id,StackItem,RS,WS,Ctr,RestStack).

rx_dispose(fail,Id,(Goal,V,N),RS,WS,Ctr,RestStack)
	:-!,
	printf('RX(%t): no.\n',[Id]), 
	flush_output,
	rxloop_cmd(RS, WS, Ctr, RestStack).
	
rx_dispose(success(Return),Id,(Goal,V,N),RS,WS,Ctr,RestStack)
	:-
	Goal = Return,
	printf('RX(%t):',[Id]), 
	builtins:showanswers(N,V,user_input,user_output),
	rxloop_cmd(RS, WS, Ctr, RestStack).
	
rx_dispose(success(Return),Id,(Goal,V,N),RS,WS,Ctr,RestStack)
	:-
	printf(WS,'forceFailure.\n',[]),flush_output(WS),
	rxloop(RS, WS, Ctr, [Id-(Goal,V,N) | RestStack]).

rxloop_cmd(RS, WS, Ctr, Stack)
	:-
	printf('\n(%t): ',[Ctr]),flush_output,
	read_term(user_input,Goal,[blocking(true),vars_and_names(V,N)]),
%	printf('%t\n',[Goal]),flush_output,
	disp_rxloop_cmd(Goal, RS, WS, Ctr, [Ctr-(Goal,V,N) | Stack]).

disp_rxloop_cmd(halt, RS, WS, Ctr, Stack)
	:-!,
	printf(WS,'halt.\n',[]),flush_output(WS),
	bread(RS,Resp),
	printf('Final Background: %t||\n',[Resp]),flush_output,
	close(test_rs), close(test_ws).

disp_rxloop_cmd(Goal, RS, WS, Ctr, Stack)
	:-
	printf(WS,'%t-(%t).\n',[Ctr,Goal]),flush_output(WS),
	rxloop(RS,WS, Ctr, Stack).

extract_stack([Id-StackItem | RestStack],Id,StackItem,RestStack)
	:-!.

extract_stack([Skip | RestStack],Id,StackItem,[Skip | RestStack])
	:-
	extract_stack(RestStack,Id,StackItem,RestStack).


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%  Client-side Utilities %%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wait_running(RS,Target)
	:-
	get_line(RS, Line),
	disp_wait_running(Line,RS,Target).

disp_wait_running(stream_not_ready(''), RS, Target) 
	:-!,
		printf('Waiting-got: %t -- polling\n',['stream_not_ready()']),
		flush_output,
	sio_poll(RS, 5000000),
	wait_running(RS,Target).
disp_wait_running(end_of_file, RS, Target) 
	:-!,
		printf('Waiting over! Got: %t\n',[end_of_file]).
disp_wait_running(Target, RS, Target)
	:-!,
		printf('Waiting over! Got: %t\n',[Target]).
disp_wait_running(Line, RS, Target)
	:-
		printf('Waiting-got: %t\n',[Line]), flush_output,
	wait_running(RS,Target).

endmod.

