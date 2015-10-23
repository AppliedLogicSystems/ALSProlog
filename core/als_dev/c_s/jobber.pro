/*======================================================================*
 | 			jobber.pro 
 |		Copyright (c) 1996 Applied Logic Systems, Inc.
 |
 |		Remote job execution & control predicates for 
 |		the socket-based server manager.
 |
 | Author: Ken Bowen
 | Started: October, 1996
 *======================================================================*/

module socket_comms.

/*-----------------------------------------------------------------------*
 |	job_control(start,Request,Vars,CSR,CSW,State,QT,NewQT,Flag,SInfo).
 |
 |	(CSR,CSW) = sockets to requesting client
 *-----------------------------------------------------------------------*/

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Start a new job
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%
job_control(start,Request,Vars,CSR,CSW,State,QT,NewQT,Flag,SInfo)
	:-
	request_details(Request, OSCmdLine, JobStartCmd, Mode),
	select_machine_tgt(OSCmdLine, SInfo, JobInfo, MSrc),
	!,
	gensym(jbctl,Cookie),
	set_remote_job_info(cookie, JobInfo, Cookie),
	set_remote_job_info(job_start_cmd, JobInfo, JobStartCmd+Vars),
	set_remote_job_info(status, JobInfo, trying),
	set_remote_job_info(parent_struct, JobInfo, State),
	record_job_connect(State, JobInfo),
	cont_job_start(MSrc,JobInfo,JobStartCmd,Vars,Mode,CSR,CSW,State,QT,NewQT,Flag,SInfo).

		%% Job failed:
job_control(start,Request,Vars,CSR,CSW,State,QT,QT,done,SInfo)
	:-
	printf(CSW,'no_machine_available.\n',[]),
	flush_output(CSW).

/*-----------------------------------------------------------------------*
 *-----------------------------------------------------------------------*/

cont_job_start(new,JobInfo,JobStartCmd,Vars,Mode,CSR,CSW,State,QT,NewQT,Flag,SInfo)
	:-
	access_remote_job_info(msor, JobInfo, MSOR),
	access_remote_job_info(msiw, JobInfo, MSIW),
	QT = [c(MSOR,MSIW,JobInfo,outbound_attempt),c(CSR,CSW,State,login) | NewQT],
	Flag = continue.

cont_job_start(idle,JobInfo,JobStartCmd,Vars,Mode,CSR,CSW,State,QT,NewQT,Flag,SInfo)
	:-
	access_remote_job_info(socket_in, JobInfo, RMS),
	access_remote_job_info(socket_out, JobInfo, WMS),

	access_remote_job_info(cookie, JobInfo, Cookie),
	access_remote_job_info(job_start_cmd, JobInfo, Goal+Vars),
	printf(WMS, 'xjob(%t,%t,%t).\n',[Cookie,Goal,Vars]),
	flush_output(WMS),
	set_remote_job_info(status, JobInfo, waiting_start_confirm),

	QT = [c(RMS,WMS,JobInfo,waiting_start_confirm),c(CSR,CSW,State,login) | NewQT],
	Flag = continue.



/*-----------------------------------------------------------------------*
 |	Incoming job req
 |		pj(OSCmdLine, StartCmd) 	- a remote prolog job
 |		j(OSCmdLine,  StartCmd) 	- a remote non-prolog job
 |
 |	pj( 'alspro -b -q jobserve -g jobserve ',  run_job(X) )
 *-----------------------------------------------------------------------*/

request_details(pj(JobRequest, StartCmd), JobRequest, StartCmd, prolog).
request_details(j(JobRequest, StartCmd), JobRequest, StartCmd, non_prolog).

/*-----------------------------------------------------------------------*
 |	select_machine_tgt/4
 |	select_machine_tgt(OSCmdLine, SInfo, JobInfo, Src)
 |	select_machine_tgt(+, +, -, -)
 |
 |
 |	- Gets a connection to a server process on some machine
 |
 |	JobINfo = remote_job defStruct
 |	Src = {idle/new}
 |
 |	1.  Checks in SInfo.idle_machines for any entry indicating a computer
 |		server process already running which doesn't have current work;
 |		Returns connection to this, if available;
 |	2.	Checks in SInfo.machine_tgts for descriptions of a target system
 |		where a computer server process can be run; if any are available,
 |		selects one such description, and uses OSCmdLine combined with
 |		information from the description to (attempt to) start a compute
 |		server process on that system; returns connection to this if
 |		successful; 
 |
 |	(RMS, WMS) -  socket pair to Port on IP, connected to the
 |				  running compute server process;
 |	MStdOutRead - a read socket (here) connected to the compute server's
 |				  standard output (user_output)
 |	MStdInWrite - a write socket (here) connected to the compute server's
 |				  standard input (user_input)
 *-----------------------------------------------------------------------*/
	%% First look for an idle process:
select_machine_tgt(OSCmdLine, SInfo, JobInfo, idle)
	:-
	access_server_info(idle_machines, SInfo, IdleMachines),
	get_list_tail(IdleMachines, JobInfo, [JobInfo | RestIdleMachines]),
	!,
	set_server_info(idle_machines, SInfo, RestIdleMachines).

	%% Otherwise, look for a system on which we can start a process:
select_machine_tgt(OSCmdLine, SInfo, JobInfo, new)
	:-
	access_server_info(machine_tgts, SInfo, MachineDB),
	find_system(MachineDB,  JobInfo, RestMachineDB, MD, RestPorts),
	start_the_server(JobInfo, OSCmdLine),
	!,
	mangle(4, MD, RestPorts),
	set_server_info(machine_tgts, SInfo, RestMachineDB).

find_system([M | RestMachineDB], JobInfo, RestMachineDB, M, RestPorts)
	:-
	M = m(HostName,IP,User,PW,Ports),
	get_list_tail(Ports, Port, [Port | RestPorts]),
	make_remote_job_info(JobInfo),
	set_remote_job_info(ip_num, JobInfo, IP),
	set_remote_job_info(port, JobInfo, Port),
	set_remote_job_info(username, JobInfo, User),
	set_remote_job_info(password, JobInfo, PW).

find_system([M | MachineDBTail], JobInfo, [M | RestMachineDB], MD, RestPorts)
	:-
	find_system(MachineDBTail, JobInfo, RestMachineDB, MD, RestPorts).

/*-----------------------------------------------------------------------*
 *-----------------------------------------------------------------------*/
start_the_server(JobInfo, OSCmdLine)
	:-
	access_remote_job_info(ip_num, JobInfo, IP),
	access_remote_job_info(port, JobInfo, Port),
	access_remote_job_info(username, JobInfo, User),
	access_remote_job_info(password, JobInfo, PW),
	catenate([OSCmdLine, ' -p -port ', Port], FullCmd),
	rexec(FullCmd, [host(IP),username(User),password(PW),
					rstream(MSOR,[snr_action(snr_code)]),wstream(MSIW,[])]),
	set_remote_job_info(msor, JobInfo, MSOR),
	set_remote_job_info(msiw, JobInfo, MSIW).

/*-----------------------------------------------------------------------*
 *-----------------------------------------------------------------------*/

record_job_connect(State, JobInfo)
	:-
	access_login_connection_info(job_connects, State, PrevJobConns),
	set_login_connection_info(job_connects, State, [JobInfo | PrevJobConns]).

remove_job_connect(JobInfo, State)
	:-
	access_login_connection_info(job_connects, State, JobConns),
	access_remote_job_info(cookie, JobInfo, Cookie),
	delete_job_rec(JobConns, Cookie,  RemainJobConns),
	set_login_connection_info(job_connects, State, RemainJobConns).

delete_job_rec([JobInfo | RemainJobConns], Cookie,  RemainJobConns)
	:-
	access_remote_job_info(cookie, JobInfo, Cookie),
	!.
delete_job_rec([JobInfo | TailJobConns], Cookie,  [JobInfo | RemainJobConns])
	:-
	delete_job_rec(TailJobConns, Cookie,  RemainJobConns).

/*-----------------------------------------------------------------------*
 *-----------------------------------------------------------------------*/

check_outb_atmpt(ready_for_business(PID),SR,SW,JobInfo,QT,NewQT,Flag,SInfo)
	:-!,
	set_remote_job_info(pid, JobInfo, PID),
	fin_check_outb_atmpt(JobInfo, SR,SW,QT,NewQT,Flag,SInfo).

check_outb_atmpt(server_failure(Port),SR,SW,JobInfo,QT,NewQT,Flag,SInfo)
	:-!,
	printf(SW, 'halt.\n', []), flush_output(SW),
	close(SR),close(SW),
	QT = NewQT,
	Flag = done.

check_outb_atmpt(Request,SR,SW,JobInfo,QT,NewQT,continue,SInfo)
	:-
	QT = [c(SR,SW,JobInfo,outbound_attempt) | NewQT].

/*-----------------------------------------------------------------------*
 *-----------------------------------------------------------------------*/

fin_check_outb_atmpt(JobInfo, SR,SW,QT,NewQT,Flag,SInfo)
	:-
	access_remote_job_info(ip_num, JobInfo, IP),
	access_remote_job_info(port, JobInfo, Port),
	open_connection(IP,Port, RMS, WMS),
	!,
	set_remote_job_info(socket_in, JobInfo, RMS),
	set_remote_job_info(socket_out, JobInfo, WMS),
	access_remote_job_info(cookie, JobInfo, Cookie),
	access_remote_job_info(job_start_cmd, JobInfo, Goal+Vars),
	printf(WMS, 'xjob(%t,%t,%t).\n',[Cookie,Goal,Vars]),
	flush_output(WMS),
	set_remote_job_info(status, JobInfo, waiting_start_confirm),
	QT = [c(RMS,WMS,JobInfo,waiting_start_confirm) | NewQT],
	Flag = continue.

fin_check_outb_atmpt(JobInfo, SR,SW,QT,NewQT,Flag,SInfo)
	:-
	printf(SW, 'halt.\n', []), flush_output(SW),
	close(SR), close(SW),
	QT = NewQT,
	Flag = done.


open_connection(Host,Port, RMS, WMS)
	:-
	catch(open(socket(inet_stream,Host,Port),read,RMS,[snr_action(snr_code)]),
				  _,
			fail),
	(is_server_socket(RMS) ->
		close(rsock),
		fail
		;
		open(socket(clone,RMS),write,WMS,[])
	).

/*-----------------------------------------------------------------------*
 *-----------------------------------------------------------------------*/

check_start_confirm(RMS,WMS,JobInfo,QT,NewQT,Flag,SInfo)
	:-!,
	set_remote_job_info(status, JobInfo, running),
	QT = [c(RMS,WMS,JobInfo,rem_job) | NewQT],
	Flag = continue.

/*-----------------------------------------------------------------------*
 *-----------------------------------------------------------------------*/

rem_job_finished(Status,Cookie,Vars,SR,SW,JobInfo,QT,NewQT,Flag,SInfo)
	:-
	access_remote_job_info(parent_struct, JobInfo, ParentStruct),
	set_remote_job_info(status, JobInfo, idle),
		%% The following 2 calls must be in this order:
	remove_job_connect(JobInfo, ParentStruct),
	set_remote_job_info(cookie, JobInfo, nil),

	access_server_info(idle_machines, SInfo, CurIdleMachines),
	set_server_info(idle_machines, SInfo, [JobInfo | CurIdleMachines]),

	QT = NewQT,
	Flag = continue,

	access_login_connection_info(socket_out, ParentStruct, PSWS),
	(Status = success ->
		printf(PSWS, 'yes(%t).\n', [Vars])
		;
		printf(PSWS, 'no.\n', [Vars])
	),
	flush_output(PSWS).







endmod.
