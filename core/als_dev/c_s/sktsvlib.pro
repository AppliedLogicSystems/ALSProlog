/*======================================================================*
 | 			sktsvlib.pro 
 |		Copyright (c) 1995-96 Applied Logic Systems, Inc.
 |      Distribution rights per Copying ALS
 |
 |	Library routines for server tools (sktserve.pro)
 |
 | Author: Ken Bowen
 *======================================================================*/

module socket_comms.

export initialize_server/2.
export process_config_item/4.
export initial_state/8.
export outer_sp_err_handler/7.
export break_connection/5.
export break_connection/6.

        %%---------------------
		%% Initialization
        %%---------------------

init_server_info_and_queue(Queue, QueueTail, SInfo)
	:-
	make_server_info(SInfo),
	sio_gethostname(InitHN),
	gethostbyname(InitHN,_,_,[IP | _]),
	set_server_info(host_ip, SInfo, IP),
	gethostbyaddr(IP,_,[HostName|_],_),
	set_server_info(host_name, SInfo, HostName),
	pbi_get_command_line(InitCmdLine),
	parse_cmd_line(InitCmdLine, CmdLine),
	(dmember(dir = MyDir, CmdLine) ->
		change_cwd(MyDir)
		;
		true
	),
	initialize_server(CmdLine, SInfo),
	begin_free_worker(CmdLine, SInfo),
	app_specific_init(CmdLine, SInfo),
	access_server_info(ports_list, SInfo, Ports),
	start_server_ports(Ports,  Queue, InitQueueTail, Started, SInfo),
	Started = [FirstServerPort | _],
	worker_initialize(CmdLine, FirstServerPort, SInfo),
	check_master_connect(CmdLine, InitQueueTail, QueueTail, SInfo).

/*!---------------------------------------------------------------*
 |	parse_cmd_line/2
 |	parse_cmd_line(InitCmdLine, CmdLine)
 |	parse_cmd_line(+, -)
 *----------------------------------------------------------------*/
parse_cmd_line(InitCmdLine, CmdLine)
	:-
	skip_cl_head(InitCmdLine, InitCmdLine0),
	parse_cmd_line0(InitCmdLine0, CmdLine).

skip_cl_head(['-p' |InitCmdLine0], InitCmdLine0) :-!.

skip_cl_head([_ | InitCmdLine], InitCmdLine0)
	:-
	skip_cl_head(InitCmdLine, InitCmdLine0).

skip_cl_head([], []).

parse_cmd_line0([], []).
parse_cmd_line0([Item  | RestInitCmdLine ], OutCmdLine)
	:-
	parse_cmd_line_item(Item, RestInitCmdLine, InitCmdLineTail, 
						OutCmdLine, OutCmdLineTail),
	parse_cmd_line0(InitCmdLineTail, OutCmdLineTail).

parse_cmd_line_item(Item, RestInitCmdLine, InitCmdLineTail, 
					OutCmdLine, OutCmdLineTail)
	:-
	'$uia_peekb'(Item, 0, 0'-),
	!,
	atom_codes(Item, [_ | ItemCs]),
	p_cl_i(ItemCs, RestInitCmdLine, InitCmdLineTail, 
						OutCmdLine, OutCmdLineTail).

parse_cmd_line_item(_, RestInitCmdLine, RestInitCmdLine, 
					OutCmdLine, OutCmdLine).

p_cl_i(ItemCs, InitCmdLineTail, InitCmdLineTail, 
					OutCmdLine, OutCmdLineTail)
	:-
	asplit0(ItemCs, 0'=, LeftCs, RightCs),
	!,
	OutCmdLine = [Tag=Value | OutCmdLineTail],
	atom_codes(Tag, LeftCs),
	bufread(RightCs, Value).

p_cl_i(ItemCs, InitCmdLineTail, InitCmdLineTail, 
					OutCmdLine, OutCmdLineTail)
	:-
	atom_codes(Item, ItemCs),
	single_cmd(Item),
	OutCmdLine = [Item=true | OutCmdLineTail].

p_cl_i(ItemCs, [Item2 | InitCmdLineTail], InitCmdLineTail, 
					OutCmdLine, OutCmdLineTail)
	:-!,
	atom_codes(Item, ItemCs),
	OutCmdLine = [Item = Item2 | OutCmdLineTail].

p_cl_i(_, InitCmdLineTail, InitCmdLineTail, 
					OutCmdLineTail, OutCmdLineTail).

single_cmd(localterm).
single_cmd(rexec).

/*!---------------------------------------------------------------*
 *----------------------------------------------------------------*/
start_server_ports([localhost(Descrip)],  Queue, QueueTail, [], SInfo)
	:-!,
	open_local(Descrip, S, SInfo),
	make_gv('_intf_state'),
	S = c(SR,SW,InitState,ConnType),
	set_intf_state(c(SR,SW,InitState,ConnType,SInfo)),
	Queue = [S | QueueTail].

start_server_ports([],  Queue, QueueTail, [], SInfo)
	:-!,
	server_warning(no_ports, [], SInfo),
	panic_exit(startup_error, no_ports, SInfo).

start_server_ports(Ports,  Queue, QueueTail, StartedPorts, SInfo)
	:-
	start_server(Ports, Queue, QueueTail, StartedPorts, SInfo).

/*!---------------------------------------------------------------*
 |	initialize_server/2
 |	initialize_server(CmdLine, SInfo)
 |	initialize_server(+, +)
 |
 |	- shell for server initialization
 |
 |	Application must supply (and export) the following:
 |
 |	config_file_name_type/2
 |	config_file_name_type(CfgFile,CfgType)
 |		CfgFile = configuration file name (possibly quoted atom)
 |		CfgType = terms - entries are prolog terms, or
 |				  lines - entries are read as line-by-line atoms
 |
 |	process_config_item/3
 |	process_config_item(Item, SInfo, Ports)
 |	process_config_item(+, +, -)
 |
 |	Obviously, this must be coordinated with the SInfo type
 |	definition.  Note that there should be some item whose
 |	effect is to instatiate Ports to a list of port numbers.
 |	Example:
 |
 |		ports_list = [34,45,12345,787] 
 |
 |	Ports gets set when this line is read.
 *----------------------------------------------------------------*/

initialize_server(CmdLine, SInfo)
	:-
	get_config_file_and_type(CmdLine, CfgFile, CfgType),
	(exists_file(CfgFile) ->
		(CfgType = terms ->
			grab_terms(CfgFile, CfgSrcItems)
			;
			grab_lines(CfgFile, CfgSrcItems)
		)
		;
			%% assumed to be CfgType = terms:
		app_default_config(CmdLine, CfgSrcItems)
	),
	getpid(PID0), PID is floor(PID0), 
	(dmember(server_name=SName, CfgSrcItems) -> 
		true
		;
		SName = 'Unknown'
	),
	write(SName), write(' server starting. ProcessID'=PID),nl,
	write('Reading initialization file...'), nl,

	process_server_config(CfgSrcItems, CmdLine, SInfo, [], _, Ports),
	set_server_info(trusted_administration, SInfo, [server_admin]),

	access_server_info(log_file, SInfo, Logfile),
	(Logfile = nil ->
		true
		;
		(file_extension(LogfileBase,_,Logfile) ->
			true ; LogfileBase = Logfile),
		number_chars(PID, PIDCs),
		atom_chars(PIDatom, PIDCs),
		file_extension(LogfileBase, PIDatom, ThisLogFile),
		set_server_info(log_file, SInfo, ThisLogFile),
		open(ThisLogFile, append, LogStream, []),
		set_server_info(log_stream, SInfo, LogStream),
		date(Date), time(Time),
		sio_gethostname(Hostname),
		printf(LogStream, 
				'\n====== Server log started: %t  %t Running on: %t PID: %t ======\n', 
				[Date,Time,Hostname,PID])
	),
	assert(configs_terms(CfgSrcItems)),
	server_warning(done_init, [], SInfo).

	/*!---------------------------------------------------------------*
	 |	process_server_config/6
	 |	process_server_config(CfgSrcItems, CmdLine, ASIn, ASOut, SInfo, Ports)
	 |	process_server_config(+, +, +, +, -, -)
	 |
	 |	- loops thru CfgSrcItems, calling process_config_item/3
	 |
	 |	process_config_item/4 appropriately processes terms or 
	 |	lines (atoms).
	 |
	 |	Note that process_server_config/4 handles the case where
	 |	process_config_item/4 fails on a given Item, giving a
	 |	server_warning in this case.
	 |
	 |	AliasStack (ASIn, ASOut)  is used to accumlate temporary info such as
	 |
	 |			alias(foobar, '/apache/als_dev')
	 |
	 |  for use during subsequent (recursive) calls.
	 *----------------------------------------------------------------*/
export process_server_config/6.

process_server_config([], _, _, AS, AS, _).
process_server_config([case_os(Cases) | CfgSrcItems], CmdLine, SInfo, ASIn,ASOut, Ports)
	:-!,
	als_system(SL),
	dmember(os=OS, SL),
	(dmember(OS-CaseItems, Cases),!;dmember(default-CaseItems,Cases),!;
		CAseItems=[]),
	process_server_config(CaseItems, CmdLine, SInfo, ASIn, IAS, Ports),
	process_server_config(CfgSrcItems, CmdLine, SInfo, IAS, ASOut, Ports).

process_server_config([alias(A,B) | CfgSrcItems], CmdLine, SInfo, ASIn, ASOut, Ports)
	:-
	pci_deref(B,ASIn,SInfo,UnaliasB),
	process_server_config(CfgSrcItems, CmdLine, SInfo,  
							[alias(A,UnaliasB) | ASIn], ASOut, Ports).

process_server_config([Item | CfgSrcItems], CmdLine, SInfo, ASIn, ASOut, Ports)
	:-
	(process_config_item(Item, CmdLine, SInfo, ASIn, Ports) ->
		true
		;
		server_warning(unknown_config, [Item], SInfo)
	),
	process_server_config(CfgSrcItems, CmdLine, SInfo, ASIn, ASOut, Ports).

	/*!---------------------------------------------------------------*
	 |	process_config_item/5
	 |	process_config_item(Item, CmdLine, SInfo, ASIn, Ports)
	 |	process_config_item(+, +, +, +, -)
	 |
	 |	- process an individual server config entry
	 *----------------------------------------------------------------*/
process_config_item(ports_list=FilePorts, CmdLine, SInfo, ASIn, AllPorts)
	:-!,
	(dmember(ports_list=PortsExpr, CmdLine) -> 
		atomread(PortsExpr, Ports)
		; 
		Ports = FilePorts
	),
	sort_out_ports(Ports, NonLoginPorts, AllPorts),
	set_server_info(ports_list, SInfo, AllPorts),
	set_server_info(non_login_ports, SInfo, NonLoginPorts).

process_config_item(dynamic_loads=LoadsList, CmdLine, SInfo, ASIn, AllPorts)
	:-!,
	load_dynamic_list(LoadsList,SInfo).

load_dynamic_list([],_).
load_dynamic_list([File | LoadsList],SInfo)
	:-
	(exists(File) ->
		consult(File)
		;
		server_warning(dynamic_load_fail, [File], SInfo)
	),
	load_dynamic_list(LoadsList,SInfo).

	%% Look for application-specific processing:
:-dynamic(application_config_item/5).

process_config_item(Tag=Value, CmdLine, SInfo,ASIn,  _)
	:-
	application_config_item(Tag, Value, CmdLine, ASIn, SInfo),
	!.

	%% Use command line value if one was provided:
process_config_item(Tag=_, CmdLine, SInfo, ASIn, _)
	:-
	dmember(Tag=Value, CmdLine),
	!,
	set_server_info(Tag, SInfo, Value).

process_config_item(Tag=Value, CmdLine, SInfo, ASIn, _)
	:-
	(atom(Value) ->
		pci_deref(Value,ASIn,SInfo,FinalValue)
		;
		FinalValue = Value
	),
		%% Try to put it in a standard slot first:
	(set_server_info(Tag, SInfo, FinalValue), !
		;
		%% Else put it in the extension slot list:
		access_server_info(extension_slot, SInfo, XTNSlotList),
		set_server_info(extension_slot, SInfo, [Tag=FinalValue | XTNSlotList])
	).

pci_deref(InFileValue,AS,SInfo,FileValue)
	:-
	subPath(InDirPath,InFileValue),
	path_deref(InDirPath, AS, SInfo,DirPath),
	!,
	subPath(DirPath, FileValue).

pci_deref(FileValue,AS,SInfo,FileValue).

path_deref([], _, _,[]).

path_deref([Item | InDirPath], AS, SInfo,FinalDirPath)
	:-
	(dmember(alias(Item, UnaliasItem), AS) 
		; access_server_info(Item, SInfo, UnaliasItem)),
	!,
	subPath(ItemL, UnaliasItem),
	path_deref(ItemL, AS, SInfo, DerefItemL),
	path_deref(InDirPath, AS, SInfo,DirPath),
	append(DerefItemL, DirPath, FinalDirPath).

path_deref([Item | InDirPath], AS, SInfo,[Item | DirPath])
	:-
	path_deref(InDirPath, AS, SInfo,DirPath).


	/*!---------------------------------------------------------------*
	 |	sort_out_ports/3
	 |	sort_out_ports(Ports, NonLoginPorts, AllPorts)
	 |	sort_out_ports(+, -, -)
	 |
	 |	- sort ports spec list into clean lists of non-login and all ports
	 *----------------------------------------------------------------*/
sort_out_ports([], [], []).
 
sort_out_ports([P/n | Ports], [P | NonLoginPorts], [P | AllPorts])
	:-!,
	sort_out_ports(Ports, NonLoginPorts, AllPorts).
	  
sort_out_ports([P | Ports], NonLoginPorts, [P | AllPorts])
	:-
	sort_out_ports(Ports, NonLoginPorts, AllPorts).

	/*!---------------------------------------------------------------*
	 |	get_config_file_and_type/3
	 |	get_config_file_and_type(CmdLine, CfgFile, CfgType)
	 |	get_config_file_and_type(+, -, -)
	 |
	 |	- 
	 *----------------------------------------------------------------*/
get_config_file_and_type(CmdLine, CfgFile, CfgType)
	:-
	dmember(cfgf=CfgFile, CmdLine),
	!,
	fin_config_file_type(CmdLine, CfgType).

get_config_file_and_type(CmdLine, CfgFile, CfgType)
	:-
	config_file_name_type(CfgFile,CfgType),
	!.

get_config_file_and_type(_, 'generic.ini', terms).


fin_config_file_type(CmdLine, CfgType)
	:-
	dmember(cfgt=CfgType, CmdLine),
	!.
fin_config_file_type(_, CfgType)
	:-
	config_file_name_type(_,CfgType),
	!.
fin_config_file_type(_, terms).


        %%---------------------
		%% Closing Down
        %%---------------------

closedown_server(SInfo)
	:-
	access_server_info(log_stream, SInfo, LogStream),
	(LogStream \= nil ->
		date(Date), time(Time),
		printf(LogStream, '\n===========Server log halted: %t  %t ========\n',
				[Date,Time]),
		close(LogStream)
		;
		true
	).

        %%---------------------
		%% Administration
        %%---------------------

/*!-----------------------------------------------------------------------*
 |	trusted_administration/3
 |	trusted_administration(Request, ConnectState, SInfo)
 |	trusted_administration(+, +, +)
 |
 |	- check on acceptablility of admin request from given user connect
 *!-----------------------------------------------------------------------*/

	%% Force this away from administration (so remote execution fails):
trusted_administration(start_job(_), ConnectState, SInfo)
	:-!,
	fail.

trusted_administration(stop_yourself, ConnectState, SInfo)
	:-
	access_login_connection_info(logged_in, ConnectState, admin).

trusted_administration(Request, ConnectState, SInfo)
	:-
	access_server_info(trusted_administration, SInfo, TrustedIDs),
	access_login_connection_info(logged_in, ConnectState, ID),
	dmember(ID, TrustedIDs),
	local_admin_check(Request, ID, TrustedIDs, ConnectState, SInfo).

	%% Default; change for specific security policy:
local_admin_check(Request, ID, TrustedIDs, ConnectState, SInfo).

/*!----------------------------------------------------------------------*
 |	administration_task/8
 |	administration_task(Request, SR,SW, State,ConnType,QT,NewQT,SInfo)
 |	administration_task(+,       +, +, +,      +,       +, -,     +)
 |
 |	- perform admin tasks, assuming security has been previously ok'd
 *-----------------------------------------------------------------------*/

administration_task(stop_yourself(_), _,_,_,_,QT,QT, _)
	:-!,
	mangle(1, State, stop_yourself).

administration_task(stop_yourself, _,_,State,_,QT,QT, _)
	:-!,
	mangle(1, State, stop_yourself).

administration_task(Request, SR,SW, State,login,QT,NewQT,SInfo)
	:-
	do_admin_task(Request,QT,NewQT,Flag,SInfo,AdminResult),
	mangle(1, State, continue),
	!,
	printf(SW, '%t.\n', [AdminResult],[quoted(true)]),
	QT = [c(SR,SW,State,login) | NewQT].

administration_task(Request, SR,SW, State,login,QT,NewQT,SInfo)
	:-
	mangle(1, State, continue),
	printf(SW, 'admin_failed(%t).\n', [Request],[quoted(true)]),
	QT = [c(SR,SW,State,login) | NewQT].

do_admin_task(switch_local(Where),QT,NewQT,Flag,SInfo,admin_ok)
	:-
	change_server_local_streams(Where, State, SInfo).

do_admin_task(spy(What),QT,NewQT,Flag,SInfo,admin_ok)
	:-
	spy(What).

do_admin_task(reconsult(What),QT,NewQT,Flag,SInfo,admin_ok)
	:-
	reconsult(What).

do_admin_task(rex(Goal,Args),QT,NewQT,Flag,SInfo,RexResult)
	:-
	remote_execute(Goal,user,Args,RexResult).

do_admin_task(rex(Goal,Mod,Args),QT,NewQT,Flag,SInfo,RexResult)
	:-
	remote_execute(Goal,Mod,Args,RexResult).

do_admin_task(rst(Slot),QT,NewQT,Flag,SInfo,rst(Val))
	:-
	access_server_info(Slot,SInfo,Val), !.
do_admin_task(rst(Slot),QT,NewQT,Flag,SInfo,bad_slot).


		%%----------------------------%%
		%% REMOTE EXECUTION PREDICATES
		%%----------------------------%%

remote_execute(Goal,Mod,Args,yes(Args))
	:-
	Mod:Goal,
	!.

remote_execute(Goal,Mod,Args,no).

		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%% Connection-level utilities and default procedures
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	/*----------------------------------------------*
	 |	Default Last chance error/exception handler
	 *----------------------------------------------*/

	%% Default clause for outer_sp_err_handler:
outer_sp_err_handler(Ball,SR,SW,State,State,Flag,SInfo)
	:-
	access_login_connection_info(socket_id,State,FID),	%% id of clone socket
	access_login_connection_info(ip_num,State,IPNum),		%% source IP number,
	access_login_connection_info(port,State,Port),		%% port number
	socket_comms:server_error(unknown_exception, [Ball,Port,FID,IPNum], SInfo),
	Flag = done.

/*!----------------------------------------------------------------------*
 |	initial_state/8
 |	initial_state(ConnType,IPNum, SR,SW,InitialState,State,Flag,SInfo)
 |	initial_state(+, +,+,+,-,+,+,+)
 |
 |	- sets up the initial connection state structure
 *-----------------------------------------------------------------------*/
initial_state(ConnType,IPNum, SR,SW,InitialState,State,continue,SInfo)
	:-
	make_login_connection_info(State),
	set_login_connection_info(ip_num, State, IPNum),
	date(Date), time(Time),
	set_login_connection_info(conn_date, State, Date),
	set_login_connection_info(conn_time, State, Time),

	(IPNum \= 0 ->
		socket_stream_info(SR, Host,Port,_,_),
		sio_fd(SR, FID)
		;
		Host = localhost,
		Port = 0,
		FID = 0
	),
	set_login_connection_info(port, State, Port),
	set_login_connection_info(socket_id, State, FID),
%	set_login_connection_info(socket_in, State, SR),
%	set_login_connection_info(socket_out, State, SW),
	server_info_out(new_connection_fin, SR, [IPNum,Time,Date], SInfo).

/*!----------------------------------------------------------------------*
 |	reinit_state/1
 |	reinit_state(State)
 |	reinit_state(+)
 |
 |	- reinitializes a logged-in state struct
 *-----------------------------------------------------------------------*/
reinit_state(State)
	:-
	set_login_connection_info(conn_date, State, (0/0/0)),
	set_login_connection_info(conn_time, State, (0:0:0)),
	set_login_connection_info(conn_date, State, (0/0/0)),
	set_login_connection_info(conn_time, State, (0:0:0)),

	set_login_connection_info(logged_in, State, nil),
	set_login_connection_info(login_time, State, nil),
	set_login_connection_info(login_date, State, nil),
	set_login_connection_info(logout_date, State, nil),
	set_login_connection_info(logout_time, State, nil),
	set_login_connection_info(last_trans_time, State, nil),
	set_login_connection_info(trans_ctr, State, 0),
	set_login_connection_info(succ_trans_ctr, State, 0),

	set_login_connection_info(term_protocol, State, prolog),
	set_login_connection_info(user_options, State, []),
	set_login_connection_info(job_connects, State, []),
	set_login_connection_info(magic_cookie, State, nil).

/*!----------------------------------------------------------------------*
 |	break_connection/[5,6]
 |	break_connection(Reason,Args,SR,SW,State,SInfo)
 |	break_connection(+,+,+,+,+,+)
 |
 |	- handles explicit breaking of connection to client socket
 *-----------------------------------------------------------------------*/
break_connection(Reason,SR,SW,State,SInfo)
	:-
	break_connection(Reason,[],SR,SW,State,SInfo).

break_connection(Reason,Args,SR,SW,State,SInfo)
	:-
	date(Date), time(Time),
	access_login_connection_info(logged_in, State, ID),
	(ID \= nil ->
		do_logout(SR,SW,State,SInfo, ID, Date, Time)
		;
		true
	),
	set_login_connection_info(disconn_date, State, Date),
	set_login_connection_info(disconn_time, State, Time),
	server_info_out(break_conn, SR, [Date,Time,Reason], SInfo),

	access_login_connection_info(logged_in, State, UserID),
	(UserID \= nil -> accounting(logout, State, SInfo) ; true ),
	(stream_open_status(SW, open) -> close(SW) ; true),
	(stream_open_status(SR, open) -> close(SR) ; true).


    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%           WORKER ALARM MANAGEMENT
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


export install_worker_alarm_handler/0.
install_worker_alarm_handler
	:-
	builtins:global_handler(sigalrm,socket_comms,worker_alarm_handler),
	!.
install_worker_alarm_handler
	:-
	builtins:asserta(global_handler(sigalrm,socket_comms,worker_alarm_handler)).

export worker_alarm_handler/3.
worker_alarm_handler(EventId, Goal, Context) 
	:-
	EventId \== sigalrm,
	!,
	propagate_event(EventId,Goal,Context).

worker_alarm_handler(_,ModuleAndGoal,_) 
	:-
	process_some_incoming_messages(WW),
	disp_worker_alarm_handler(WW, ModuleAndGoal).

disp_worker_alarm_handler(-1, ModuleAndGoal)
	:-!,
	trigger_event(sigint, ModuleAndGoal).

disp_worker_alarm_handler(_, ModuleAndGoal)
	:-
	ModuleAndGoal.

export worker_alarm_interval/1.
worker_alarm_interval(5.05).

export set_worker_alarm_interval/1.
set_worker_alarm_interval(I)
	:-
	abolish(worker_alarm_interval,1),
	assert(worker_alarm_interval(I)).

export process_some_incoming_messages/1.
process_some_incoming_messages(WW)
	:-
pbi_write('WAKEUP!'),pbi_nl,pbi_ttyflush,
	get_mrq_channel(qc(SR,SW,State,SInfo)),
	poll(SR, 0),
	!,
	read(SR, Request),
pbi_write(wakeup_got_request=Request),pbi_nl,pbi_ttyflush,    
	dispatch_process_some_incoming_messages(Request,WW,SR,SW,State,SInfo).

process_some_incoming_messages(0).

dispatch_process_some_incoming_messages(stream_not_ready,0,_,_,_,_)
	:-!.

dispatch_process_some_incoming_messages(current_status,0,SR,SW,State,SInfo)
	:-!,
pbi_write(disp_wakeup_request=current_status),pbi_nl,pbi_ttyflush,    
	get_process_status(Status),
pbi_write(responding_current_status=Status),pbi_nl,pbi_ttyflush,    

	(stream_open_status(SW, open) ->
			printf(SW, 'obm(current_status,%t).\n', [Status], [quoted(true),line_length(500000)]),
			flush_output(SW)
			;
				%% replace this with an abort of the whole worker:
			true
	).

/*
dispatch_process_some_incoming_messages(Request,WW,SR,SW,State,SInfo)
	:-
pbi_write(disp_wakeup_request=Request),pbi_nl,pbi_ttyflush,    
*/

dispatch_process_some_incoming_messages(Request,0,SR,SW,State,SInfo)
	:-
pbi_write('SKIPPING_wakeup_request'=Request),pbi_nl,pbi_ttyflush.



endmod.		%% socket_comms





/*!----------------------------------------------------------------------*
 *-----------------------------------------------------------------------*/

