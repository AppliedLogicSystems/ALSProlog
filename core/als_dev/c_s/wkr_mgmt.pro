/*======================================================================*
 | 			wkr_mgmt.pro 
 |		Copyright (c) 1996 Applied Logic Systems, Inc.
 |      Distribution rights per Copying ALS
 |
 |		Management of background "worker" processes from
 |		the server manager.
 |
 | Author: Ken Bowen
 | Started: September, 1996
 *======================================================================*/

module socket_comms.

	/*--------------------------------------------------------------*
	 |	
	 *--------------------------------------------------------------*/
begin_free_worker(CmdLine, Queue, QueueTail, SInfo)
	:-
getpid(XXX), write(begin_free_worker(XXX,CmdLine)),nl,
		%% Get initial necessary info from commandline:
	dmember(chost=MasterHost,CmdLine),
	dmember(cport=ICPortAtom,CmdLine),
	dmember(uport=MyPortAtom,CmdLine),
	dmember(uhost=MyHost,CmdLine),
	dmember(id=MyID,CmdLine),
	atomread(ICPortAtom, ICPort),
		%% Open the (client) socket back to the master server on the
		%% master server's port:
	catch(open(socket(inet_stream,MasterHost,ICPort),read,ICRS,[]),
			_,
			fail),
	(is_server_socket(ICRS) ->
		close(ICRS),
		fail
		;
		open(socket(clone,ICRS),write,ICWS,[])
	),
	cont_free_work(MyPortAtom,MyHost,ICRS,ICWS,MyID,Queue,QueueTail,SInfo).

	%% Now open a server socket on MyPort; we will act as
	%% server on this socket (and the master server will act
	%% as a client on it):
cont_free_work(MyPortAtom,MyHost,ICRS,ICWS,MyID,Queue,QueueTail,SInfo)
	:-
	atomread(MyPortAtom, MyPort),
%	start_server_ports([MyPort], Queue, QueueTail, Res, SInfo),
%	Res \= [],

	MyPortTop is MyPort + 100,
	start_server_port_range(MyPort, MyPortTop, Queue, QueueTail, ThePort, SInfo),
write(cont_free_work(XXX,start_server_ports_done(ThePort,Res))),nl,flush_output,
	!,
	assert(waiting_for_handshake),
		%% send Request: worker_connect_init(_,_,_)
		%% on master server's (listening) socket;
	printf(ICWS, 'worker_connect_init(\'%t\',\'%t\',%t).\n',[MyID,MyHost,ThePort]),
	flush_output(ICWS).

	%% Failed to open our socket; close down & quit:
cont_free_work(MyPortAtom,MyHost,ICRS,ICWS,MyID,Queue,QueueTail,SInfo)
	:-
	printf(ICWS, 'worker_init_failure(%t,%t,%t).',[MyID,MyHost,MyPortAtom]),
	flush_output(ICWS),
	close(ICRS),
	close(ICWS),
	halt.

%worker_init_req(worker_connect_init(_,_,_)).

start_server_port_range(CurPort, TopPort, Queue, QueueTail, CurPort, SInfo)
	:-
write(wkr_sspr(CurPort)),nl,
	catch(start_server_on_port(CurPort, Queue, QueueTail, SInfo),
			_,
			sspr_fail(CurPort)
	),
	!.

start_server_port_range(CurPort, TopPort, Queue, QueueTail, ThePort, SInfo)
	:-
write(wkr_sspr_c2(CurPort,TopPort)),nl,
	CurPort < TopPort,
	NextPort is CurPort + 1,
	start_server_port_range(NextPort, TopPort, Queue, QueueTail, ThePort, SInfo).

sspr_fail(CurPort)
	:-
	write(sspr_fail(CurPort)),nl,
	fail.

	/*--------------------------------------------------------------*
	 |	
	 *--------------------------------------------------------------*/

init_worker_state(worker_connect_init(ID,WHost,WPort),SR,SW,State,ConnType,SInfo,
					NewRS,NewWS,State)
	:-
	(pending_worker_setup(ID, WHost,WPort,WkrNum, WkrCfg); 
		special_worker_setup(ID, WHost,WPort,WkrNum, WkrCfg)),
	catch(open(socket(inet_stream,WHost,WPort),read,NewRS,[]),
			_,
			fail),
	(is_server_socket(NewRS) ->
		close(NewRS),
		fail
		;
		open(socket(clone,NewRS),write,NewWS,[])
	),
		%% Temporary (??):
	socket_stream_info(NewRS, WWHost,WWPort,_,_),
	sio_fd(SR, FID),
	set_login_connection_info(port, State, WWPort),
	set_login_connection_info(socket_id, State, FID),
	set_login_connection_info(socket_in, State, NewRS),
	set_login_connection_info(socket_out, State, NewWS),

%write(init_worker_connect(WHost/WWHost,WPort/WWPort,fid=FID)),nl, flush_output,
	server_info_out(worker_connect, NewRS, [WHost,WPort], SInfo),

	%% Put record in wdbs, and send message back to the worker for handshake:

	InitWorker = m(WWHost,IP,User,PW,WWPort,NewRS,NewWS,PID),
	xmake_wkr_rec(Worker, [NewRS, NewWS, idle, WkrNum, InitWorker]),
	get_worker_dbs(WrkDB),
	mangle(WkrNum, WrkDB, Worker),

	get_idle_workers(PrevIdle),
	set_idle_workers([Worker | PrevIdle]),

	printf(NewWS, 'worker_installed.\n', []),
	flush_output(NewWS).

	/*--------------------------------------------------------------*
	 |	
	 *--------------------------------------------------------------*/

outer_max_num_workers(100).

init_worker_framework(NumWkrs)
	:-
	make_gv('_idle_workers'),
		set_idle_workers([]),
	make_gv('_worker_dbs'),
	make_gv('_job_queue'), 
		set_job_queue((Var,Var)),
	functor(WrkDB, wdb, NumWkrs),
	set_all_args(1,NumWkrs,WrkDB, 0),
		set_worker_dbs(WrkDB).

init_workers(MaxNumWkrs, CmdLine, CfgSrcItems, SInfo, LogFile, FinalNWs)
	:-
	start_workers(MaxNumWkrs, FinalNWs, CfgSrcItems, CmdLine, WrkDB, SInfo, WkrRecsList),
assert(pending_wkr(ken_ok,'hilbert.als.com',30002,
		 m('hilbert.als.com','204.5.42.8',ken,logical,30002,
			'/dakota/ken/zparc/eng/servers/builds/solaris/zsd-solaris',
			'/dakota/ken/zparc/eng/servers/builds/solaris')  )),
	set_idle_workers(WkrRecsList).

special_worker_setup(ken_ok, WHost,WPort,WkrNum, WkrCfg)
	:-
	get_worker_dbs(WrkDB),
	functor(WrkDB, _, NumWkrs),
	first_free(1,NumWkrs,WrkDB,WkrNum).

pending_worker_setup(ID, WHost,WPort,WkrNum, WkrCfg)
	:-
	pending_wkr(ID,WHost,WPort,WkrCfg),
	get_worker_dbs(WrkDB),
	functor(WrkDB, _, NumWkrs),
	first_free(1,NumWkrs,WrkDB,WkrNum),
	retract(pending_wkr(ID,WHost,WPort,WkrCfg)).

	/*--------------------------------------------------------------*
	 |	Locate the index of the first free (= 0) arg in
	 |	the worker_dbs (database) term:
	 *--------------------------------------------------------------*/
first_free(CurArgN, NumWkrs, WrkDB, WkrNum)
	:-
	arg(CurArgN, WrkDB, WRec),
	disp_first_free(WRec, CurArgN, NumWkrs,WrkDB, WkrNum).

disp_first_free(0, WkrNum, NumWkrs, WrkDB, WkrNum).
disp_first_free(WRec, CurArgN, WrkDB, WkrNum)
	:-
	WRec \= 0, 
	CurArgN < NumWkrs,
	!,
	NextArgN is CurArgN + 1,
	first_free(NextArgN, NumWkrs, WrkDB, WkrNum).

	/*--------------------------------------------------------------*
	 |	Build a list of descriptions of individual worker
	 |	processes from the info given in the initialization file:
	 *--------------------------------------------------------------*/
config_worker_descs(ConfigsTerms, WorkerConfigs)
	:-
	findall(BWD, member(worker_info=BWD, ConfigsTerms), BWDL),
	(dmember(dflt_worker_cmd=DfltWorkerCmd, ConfigsTerms),!; DfltWorkerCmd = ''),
	(dmember(dflt_worker_dir=DfltWorkerCmd, ConfigsTerms),!; DfltWorkerDir = ''),
	bld_wrkcfgs(BWDL, DfltWorkerCmd, DfltWorkerDir, WorkerConfigs).

bld_wrkcfgs([], DfltWorkerCmd, DfltWorkerDir, []).
bld_wrkcfgs([BWD | BWDL], DfltWorkerCmd, DfltWorkerDir, WorkerConfigs)
	:-
	bld_some_wcfgs(BWD, DfltWorkerCmd, DfltWorkerDir, WorkerConfigs, InterWorkerConfigs),
	bld_wrkcfgs(BWDL, DfltWorkerCmd, DfltWorkerDir, InterWorkerConfigs).

bld_some_wcfgs(BWD, DfltWorkerCmd, DfltWorkerDir, WrCfigs, IntWrCfigs)
	:-
	(BWD = m(HostName,IP,UserAcct,PW,PortsList) ->
		WorkerCmd = DfltWorkerCmd,
		WorkerDir = DfltWorkerDir
		;
		(BWD = m(HostName,IP,UserAcct,PW,PortsList,WorkerCmd) ->
			WorkerDir = DfltWorkerDir
			;
			BWD = m(HostName,IP,UserAcct,PW,PortsList,WorkerCmd,WorkerDir)
		)
	),
	bldws(PortsList,HostName,IP,UserAcct,PW, WorkerCmd, WorkerDir, WrCfigs, IntWrCfigs).

bldws([],HostName,IP,UserAcct,PW, WorkerCmd, WorkerDir, WrCfigs, WrCfigs).
bldws([Port | PortsList],HostName,IP,UserAcct,PW, 
		WorkerCmd, WorkerDir, WrCfigs, WrCfigsTail)
	:-
	WrCfigs = [WCF | InterWrCfigs],
	WCF = m(HostName,IP,Port,UserAcct,PW, WorkerCmd, WorkerDir),
	bldws(PortsList,HostName,IP,UserAcct,PW, 
			WorkerCmd, WorkerDir, InterWrCfigs, WrCfigsTail).
bldws(Port,HostName,IP,UserAcct,PW, 
		WorkerCmd, WorkerDir, WrCfigs, WrCfigsTail)
	:-
	atomic(Port),
	!,
	WrCfigs = [WCF],
	WCF = m(HostName,IP,Port,UserAcct,PW, WorkerCmd, WorkerDir).

	/*--------------------------------------------------------------*
	 |	If the parsed CommandLine contains "noworker=true", don't 
	 |	really startup anything, just set NumWorkers=1, and make the 
	 |	single worker record a dummy (used for development); ,
	 |	otherwise, build the list of worker descriptions from zparc.ini, 
	 |	and start each of the described worker processes, up to the
	 |	specificied max:
	 *--------------------------------------------------------------*/
start_workers(MaxNWs, 1, ConfigsTerms, CommandLine, WrkDB, SP, SInfo, [Worker])
	:-
	dmember(noworker=true, CommandLine),
	!,
	xmake_wkr_rec(Worker, [nil,nil,nil,0,nil]),
	arg(1, WrkDB, Worker).

start_workers(MaxNWs, FinalNWs, ConfigsTerms, CommandLine, WrkDB, SP, SInfo, WkrRecsList)
	:-
		% 'Starting worker processes. Please wait...\n'
%	server_info_out(start_wkrs, 0, [], SInfo),
	config_worker_descs(ConfigsTerms, WorkerConfigs),
	!,
	sio_gethostname(InitHN),
	gethostbyname(InitHN,_,FHN,_),
	(atomic(FHN) -> ThisHost = FHN ; FHN = [ThisHost | _]),
	start_all_workers(WorkerConfigs, 0, MaxNWs, FinalNWs,ThisHost,WrkDB, SP, SInfo, WkrRecsList),
	fin_start_all_workers(FinalNWs, WorkerConfigs, WrkDB, SInfo, WkrRecsList).

/*
fin_start_all_workers(0, WorkerConfigs, WrkDB, SInfo, WkrRecsList)
	:-
	set_idle_workers([]),
	set_worker_dbs(wdb),
	set_job_queue(nil),
	server_info_out(wkrs_started, 0, [0], SInfo).
*/

fin_start_all_workers(FinalNWs, WorkerConfigs, WrkDB, SInfo, WkrRecsList)
	:-
	server_info_out(wkrs_started, 0, [FinalNWs], SInfo).

	/*--------------------------------------------------------------*
	 |	Main recursion for starting/connecting to the workers:
	 |
	 |	Also creates the list (WkrRecsList) of records concerning
	 |	the started workers; this is later accessed using:
	 |		worker_avail/1   and return_to_idle/1
	 |
	 |	Also sets the WrkDB info.
	 *--------------------------------------------------------------*/
start_all_workers([], FinalNWs, MaxNWs, FinalNWs, ThisHost,WrkDB, SP, SInfo, []).

start_all_workers([WCfg | WorkerConfigs], CurN, MaxNWs, FinalNWs, 
				ThisHost,WrkDB, SP, SInfo, WkrRecsList)
	:-
	CurN < MaxNWs,
	!,
/*
	TryNextN is CurN + 1,
	(start_worker(WCfg, TryNextN, SInfo, Worker) ->
		NextN is CurN + 1,
		mangle(NextN, WrkDB, Worker),
			%	'Wkr#%d started. '
		server_info_out(a_wkr_started, 0, [NextN], SInfo),
		WkrRecsList = [Worker | WkrRecsListTail]
		;
		NextN = CurN,
		WkrRecsList = WkrRecsListTail
	),
	*/
	start_worker(WCfg, ThisHost, NextN, SP, SInfo, Worker),
	NextN is CurN + 1,
	WkrRecsList = [Worker | WkrRecsListTail],
	start_all_workers(WorkerConfigs, NextN, MaxNWs, FinalNWs, 
						ThisHost,WrkDB, SP, SInfo, WkrRecsListTail).

start_all_workers(_, FinalNWs, MaxNWs, FinalNWs, ThisHost, WrkDB, SP, SInfo, []).

	/*--------------------------------------------------------------*
	 |	start_worker/5
	 |	start_worker(WCfg, ThisHost, CurN, SP, SInfo, Worker)
	 |	start_worker(+, +, +, +, -)
	 |
	 |	Try to start/connect to an individual worker:
	 |
	 |	Success: Returns
	 |	Worker = record made with xmake_wkr_rec/2:
	 |		xmake_wkr_rec(Worker, [WkRS, WkWS, idle, CurN, InitWorker])
	 |	
	 *--------------------------------------------------------------*/
	%% Can we connect to the port?
start_worker(WCfg, ThisHost, CurN, SP, SInfo, Worker)
	:-
	WCfg =  m(HostName,IP,Port,UserAcct,PW, WorkerCmd,WorkerDir),
			%% Can we open a read socket on the port?
	WorkerHost = HostName,
	catch(open(socket(inet_stream,WorkerHost,Port),read,WkRS,
			[read_eoln_type(lf),snr_action(snr_code)]),
			_,
		 	fail
	),
			%% Yes; Is there a process & can we connect:
	(proceed_start_worker(WkRS, Port, WCfg, CurN, SInfo, Worker) ->
		true
		;
		server_info_out(running_no_connect, 0, [HostName,Port], SInfo),
		fail
	).

	%% No worker running that we can connect to; Setup start worker process:
start_worker(WCfg, ThisHost, CurN, SP, SInfo, Worker)
	:-
	WCfg =  m(HostName,IP,WorkerPort,User,PW, WorkerCmd,WorkerDir),
	gensym(det_wkr,WID),
	spawn_free_worker(ThisHost,HostName,WID,SP, WCfg, SInfo),
	assert(pending_wkr(WID,HostName,WorkerPort,WkrCfg)).


	%% Worker process is to run on the same machine as this server:
spawn_free_worker(Host,Host,WID,SP, WCfg, SInfo)
	:-
	WCfg =  m(Host,IP,Port,UserAcct,PW, WorkerCmd,WorkerDir),
	sprintf(atom(Cmd),
	'%t -g start_server -p -st fws -chost %t -cport %t -uhost %t -uport %t -id %t &',
		[WorkerCmd,Host,SP,Host,Port,WID]),

	server_info_out(spawn_attempt, 0, [Host,IP,WID], SInfo),
%getpid(XXX), write(pid(XXX)),write('system_call:'),nl, write(Cmd),nl,nl,

	system(Cmd).


/*
	%% Worker process is to run on a different machine than this server:
spawn_free_worker(_,_,WID,SP, WCfg)
	:-
	WCfg =  m(HostName,IP,Port,UserAcct,PW, WorkerCmd,WorkerDir),
*/


/***********
	%% No worker running that we can connect to; Try to start worker process:
start_worker(WCfg, CurN, SInfo, Worker)
	:-
	WCfg =  m(HostName,IP,WorkerPort,User,PW, WorkerCmd,WorkerDir),
	server_info_out(try_start, 0, [HostName,WorkerPort], SInfo),

	sprintf(atom(FullCmd), 
		'%t -gias -g jobserve:jobserve -p -rexec -port %t -dir %t', 
			[WorkerCmd, WorkerPort, WorkerDir]),

printf('Trying rexec: %t\n',[FullCmd]), flush_output,
	rexec(FullCmd, [host(IP),username(User),password(PW),
					rstream(WkIRS,[snr_action(snr_code)]),wstream(WkIWS,[])]),
printf('rexec OK\n',[]), flush_output,
	check_connect(25,WkIRS,WkIWS,1000000,ConnectCheckResultLine),
	atomread(ConnectCheckResultLine, ConnectCheckResult),
	ConnectCheckResult = ready_for_business(PID),
	InitWorker = m(HostName,IP,User,PW,WorkerPort,WkIRS,WkIWS,PID),
	make_worker_conns(InitWorker, CurN, SInfo, Worker),
	!.

start_worker(WCfg, CurN, SInfo, Worker)
	:-
	WCfg = m(HostName,IP,Port,UserAcct,PW, WorkerCmd,WorkerDir),
		%	'Can\'t start service worker on host %t(%t) as %t at port %t...Skipping\n'
	server_info_out(skip_wkr, 0, [HostName,IP,UserAcct,Port], SInfo),
	!,
	fail.
***********/

	/*--------------------------------------------------------------*
	 |	proceed_start_worker/6
	 |	proceed_start_worker(WkRS, Port, WCfg, CurN, SInfo, Worker)
	 |	proceed_start_worker(+, +, +, +, +, -)
	 |
	 |	Try to connect to a running worker process:
	 |
	 |	Success: Returns
	 |	Worker = record made with xmake_wkr_rec/2:
	 |		xmake_wkr_rec(Worker, [WkRS, WkWS, idle, CurN, InitWorker])
	 |	
	 *--------------------------------------------------------------*/
			%% Opened socket; Is it a server socket? - If so, QUIT:
proceed_start_worker(WkRS, Port, WCfg, CurN, SInfo, Worker)
	:-
	is_server_socket(WkRS),
	!,
	close(WkRS),
	fail.

			%% Not a server socket (we're a client); Try to connect to it:
proceed_start_worker(WkRS, WorkerPort, WCfg, CurN, SInfo, Worker)
	:-
	flush_input(WkRS),
	open(socket(clone,WkRS),write,WkWS,[write_eoln_type(lf)]),
	catch( (printf(WkWS, 'are_you_ready.\n', []), flush_output(WkWS)),
		   _,
		   fail),
	check_connect(25,WkRS,WkWS,100000,ConnectCheckResultLine),
	atomread(ConnectCheckResultLine, ConnectCheckResult),
	ConnectCheckResult = ready_for_business(PID),
	!,
	WCfg =  m(HostName,IP,_,User,PW, _,_),
	InitWorker = m(HostName,IP,User,PW,WorkerPort,WkIRS,WkIWS,PID),
	xmake_wkr_rec(Worker, [WkRS, WkWS, idle, CurN, InitWorker]),
	server_info_out(connect_wkr, 0, [HostName,IP,PID,User,WorkerPort], SInfo).


	/*--------------------------------------------------------------*
	 |	Kill all workers (if running)
	 *--------------------------------------------------------------*/
kill_all_workers
	:-
%	num_workers(NWs),
	get_worker_dbs(WrkDB),
	functor(WrkDB, wdb, NumWkrs),
%	WrkDB =.. [wdb | WkrRecList],
	!,
	kill_all_workers(1, NumWkrs, WrkDB).
kill_all_workers.

kill_all_workers(N, NWs, WrkDB)
	:-
	N > NWs,
	!.

kill_all_workers(CurN, NWs, WrkDB)
	:-
	arg(CurN, WrkDB, Worker),
	kill_off_wkr(Worker),
	NextN is CurN + 1,
	kill_all_workers(NextN, NWs, WrkDB).

kill_off_wkr(Worker)
	:-
	access_wkr_rec(write_s, Worker, WkWs),
	stream_open_status(WkWs, open),
	!,
	(catch( ( printf(WkWs, 'stop_yourself.\n', []),
				flush_output(WkWs) ),
			_,
			true
	),
	catch( close(WkWs), _, true),
	access_wkr_rec(read_s, Worker, WkRs),
	catch( close(WkRs), _, true)).

kill_off_wkr(Worker).


	/*--------------------------------------------------------------*
	 |	Job queue handling, etc....
	 *--------------------------------------------------------------*/

	%% remove when any real ones are installed:
worker_comm_req(worker_dummy_request).
/*
worker_comm_req(Request).
disp_worker_service_request(Request,SR,SW,State,ConnType,QT,NewQT,SInfo)
	:-

*/





	/*--------------------------------------------------------------*
	 |	Job queue handling, etc....
	 *--------------------------------------------------------------*/

export appl_exec/4.
appl_exec(Task,TaskArgs,Mod,TaskEnv,SInfo)
	:-
	mk_job_rec(Task,TaskArgs,Mod,TaskEnv,Job),
	configs_terms(CfgTerms),
	run_local_job(Job,Task,TaskArgs,Mod,CfgTerms,TaskEnv, OutFileName,SInfo),

			%% info('Job finished: %t %t file=%t\n',[JobType, Job, OutFileName]),
	server_info_out(job_done, SR, [JobType,UserID,JobID,OutFileName], SInfo).

job_submit(Task,TaskArgs,Mod,TaskEnv,SInfo,WorkerRec)
	:-
		%% WorkerRec was made with:
		%% xmake_wkr_rec(WorkerRec, [WkRS, WkWS, idle, ID, WorkerInfo])
		%% WorkerInfo = m(HostName,IP,User,PW,WorkerPort,WkIRS,WkIWS,PID),

		%% Get top level var/nonvar pattern from TaskArgs to use
		%% for matching up the return value list from the worker;
		%% this puts the computed values (e.g., "Result") back into
		%% the correct variables from the governing csp_script, and
		%% in particular, binds delay/full_delay variables:
	get_var_mask(TaskArgs, TaskArgsVarMask),
	Busy =.. [busy | TaskArgsVarMask],
		%% Mark worker record as busy, including storing the
		%% task var mask; note that the WorkerRec has already
		%% been put in the main server queue;
	set_wkr_rec(state,WorkerRec,Busy),

		%% Transmit job to worker:
	mk_job_rec(Task,TaskArgs,Mod,TaskEnv,Job),
	access_wkr_rec(write_s,WorkerRec,WkWS),
	printf(WkWS, '%t.\n', [Job], [quoted(true)]),

	access_wkr_rec(workerID,WorkerRec,N),
		%	'Submitted to worker %t: %t \n'
	server_info_out(job_sub, 0, [N, Job], SInfo).

get_var_mask([], []).
get_var_mask([TA | TaskArgs], [TA | TaskArgsVarMask])
	:-
	var(TA),
	!,
	get_var_mask(TaskArgs, TaskArgsVarMask).
get_var_mask([TA | TaskArgs], [_ | TaskArgsVarMask])
	:-
	get_var_mask(TaskArgs, TaskArgsVarMask).

pop_next_job(Job)
	:-
	get_job_queue(JobQueue),
	JobQueue = (Head, Tail),
	nonvar(Head),
	Head = [Job | RestHead],
	!,
	set_job_queue(RestHead, Tail).
							 
pop_next_job(job_queue_empty).
							  
push_next_job(Job)
	:-
	get_job_queue(JobQueue),
	JobQueue = (Head, Tail),
	Tail = [Job | NewTail],
	set_job_queue(Head, NewTail).
												   
worker_avail(Worker)
	:-
	get_idle_workers([Worker | RestWkrRecs]),
	set_idle_workers(RestWkrRecs).

return_to_idle(Worker)
	:-
	get_idle_workers(CurWkrRecs),
	set_idle_workers([Worker | CurWkrRecs]).

queue_up_job_req(QJ, State)
	:-
	get_job_queue((GQHead, GQTail)),
	Tail = [QJ | NewGQTail],
	set_job_queue((GQHead, NewGQTail)),
	add_job_rec(QJ, State).

	/*--------------------------------------------------------------*
	 |	Messages codes relating to worker management
	 *--------------------------------------------------------------*/

	%% server_info_out(start_wkrs, 0, [], SInfo),
server_info_msg(start_wkrs, 'Starting worker processes. Please wait...\n').

	%% server_info_out(wkrs_started, 0, [FinalNWs], SInfo),
server_info_msg(wkrs_started, '\n>> Total of %t workers started\n').

	%% server_info_out(a_wkr_started, 0, [NextN], SInfo),
server_info_msg(a_wkr_started, 'Wkr#%d started. ').

	%% server_info_out(skip_wkr, 0, [[HostName,IP,UserAcct,Port], SInfo),
server_info_msg(skip_wkr, 
		'Can\'t start service worker on host %t(%t) as %t at port %t...Skipping\n').

	%% server_info_out(running_no_connect, 0, [[HostName,WorkerPort], SInfo)
server_info_msg(running_no_connect,'Server running on %t/%t, but can\'t connect - skipping\n').

	%% server_info_out(try_start, 0, [[HostName,WorkerPort], SInfo)
server_info_msg(try_start, 'No server running on %t/%t - trying to start\n').

	%% server_info_out(connect_wkr, 0, [[HostName,IP,PID,User,WorkerPort], SInfo).
server_info_msg(connect_wkr,
		'Connected to service worker on host %t(%t) with pid %t as user %t at port %t\n').

	%% server_info_out(attmpt_conn, 0, [HostName,IP,Port,PID,CurN], SInfo),
server_info_msg(attmpt_conn, 'Attempt socket connect to worker: %t %t/%t pid=%t[#%t]\n').

	%% server_info_out(fail_attmpt_conn, 0, [], SInfo),
server_info_msg(fail_attmpt_conn, 'Worker connection attempt failed...\n').

	%% server_info_out(no_worker_info, 0, [], SInfo),
server_info_msg(no_worker_info, '!!Panic: Couldn\'t start any workers! ... exiting\n').

	%% server_info_out(job_sub, 0, [N, Job], SInfo),
server_info_msg(job_sub, 'Submitted to worker %t: %t \n').

	%% server_info_out(break_conn, SR, [Date,Time,Reason], SInfo),
server_info_msg(service_wkr, 'Servicing worker return\n').

	%% server_info_out(job_done, SR, [JobType, UserID, JobID, OutFileName], SInfo).
server_info_msg(job_done, 'Job finished: %t %t %t file=%t\n').

	%% server_info_out(run_job, SR, [JobType, Job], SInfo).
server_info_msg(run_job, 'Running job: %t %t\n').

	%% server_info_out(sent_resp, SR, [Response], SInfo).
server_info_msg(sent_resp, 'Sent response: %t\n').

	%% server_info_out(job_yes, SR, [JobType, JobID], SInfo).
server_info_msg(job_yes, '>job_exec success: %t - %t\n').

	%% server_info_out(job_no, SR, [JobType, JobID], SInfo).
server_info_msg(job_no, '>job_exec FAILURE: %t - %t\n').

    /*--------------------------------------------------------------*
	 |  Task Management
	 *--------------------------------------------------------------*/

export handle_task/4.
export handle_task/5.

handle_task(_,current_queue,[Q],Mod,TaskEnv,SInfo)
	:-!,
	access_tsk_env(state, TaskEnv, State),
	mangle(1, State, add_to_queue(current_queue(Q))).

:-dynamic(localhost_situation/0).
handle_task(no_worker,Task,TaskArgs,Mod,TaskEnv,SInfo)
	:-
	localhost_situation,
	!,
	appl_exec(Task,TaskArgs,Mod,TaskEnv,SInfo).

handle_task(no_worker,Task,TaskArgs,Mod,TaskEnv,SInfo)
	:-!,
	access_tsk_env(state, TaskEnv, State),
	mangle(1, State, 
		add_to_queue(
			local_job( 
				SIV^appl_exec(Task,TaskArgs,Mod,TaskEnv,SIV)
					) ) ).
		 
handle_task(worker_job,Task,TaskArgs,Mod,TaskEnv,SInfo)
	:-

		%% returns Worker record made with:
		%%	xmake_wkr_rec(Worker, [WkRS, WkWS, idle, N, InitWorker])
	worker_avail(Worker),
	!,
		% Put the worker record in the queue first:
		%   Flag = add_to_queue(Worker).
	access_tsk_env(state, TaskEnv, State),
	mangle(1, State, add_to_queue(Worker) ),

		%% Now submit the job:
	job_submit(Task,TaskArgs,Mod,TaskEnv,SInfo,Worker).

handle_task(worker_job,Task,TaskArgs,Mod,TaskEnv,SInfo)
	:-
	QJ = queued_request(Task,TaskArgs,Mod,TaskEnv),
				%% push into both State's queue and the global job queue:
	access_tsk_env(state, TaskEnv, State),
	queue_up_job_req(QJ, State).

    /*--------------------------------------------------------------*
	 |  Manipulating job queues in the Client Record
	 *--------------------------------------------------------------*/

add_job_rec(Job, State)
	:-
	access_login_connection_info(waiting_jobs, State, CurJobList),
	CurJobList = (Head, Tail),
	Tail = [Job | NewTail],
	set_login_connection_info(waiting_jobs, State, (Head, NewTail)).

pop_job_rec(Job, State)
	:-
	access_login_connection_info(waiting_jobs, State, CurJobList),
	CurJobList = (Head, Tail),
	nonvar(Head),
	Head = [Job | NewHead],
	set_login_connection_info(waiting_jobs, State, (NewHead, Tail)).

service_worker(QueueRec, QT, NewQT, Flag, SInfo)
	:-
	access_wkr_rec(read_s,QueueRec,WkRS),
%	access_wkr_rec(write_s,QueueRec,WkWS),
%	access_wkr_rec(state,QueueRec,CurState),
%	access_wkr_rec(workerID,QueueRec,N),
%	access_wkr_rec(worker_info,QueueRec,WorkerInfo),

	read(WkRS, Request),
	(Request \= stream_not_ready ->
		server_info_out(service_wkr, WkRS,[], SInfo)
		;
		true
	),
	!,
	handle_worker(Request, QueueRec, WFlag),
	dispose_worker(WFlag, QueueRec, QT, NewQT, Flag).

    /*--------------------------------------------------------------*
	 |  Deal with worker responses:
	 *--------------------------------------------------------------*/
handle_worker(job_done(JobID, UserID, ReturnTaskArgs), QueueRec, return_to_idle)
	:-!,
	access_wkr_rec(state,QueueRec,CurState),
	CurState =.. [busy | TaskVarArgsMask],
	TaskVarArgsMask = ReturnTaskArgs,
	set_wkr_rec(state,QueueRec,idle).
								   
handle_worker(end_of_file, QueueRec, done)
	:-!.
												
handle_worker(Request, QueueRec, continue).

dispose_worker(Flag, QueueRec, QT, NewQT, continue)
	:-
	var(Flag),
	!,
	QT = [QueueRec | NewQT].
					 
dispose_worker(return_to_idle, QueueRec, QT, NewQT, done)
	:-!,
	return_to_idle(QueueRec),
	QT = NewQT.
								  
dispose_worker(done, QueueRec, QT, NewQT, done)
	:-!,
	QT = NewQT.

dispose_worker(continue, QueueRec, QT, NewQT, continue)
	:-!,
	QT = [QueueRec | NewQT],
	Flag=continue.
														

/*------------------------------------------------------------------------*
 *------------------------------------------------------------------------*/

mk_job_rec(Task,TaskArgs,Mod,TaskEnv,Job)
	:-
	access_tsk_env(state, TaskEnv, State),
	access_tsk_env(jobID, TaskEnv, JobID),
	access_tsk_env(userID, TaskEnv, UserID),
	access_login_connection_info(user_area, State, UserAreaPath),
	xmake_job_rec(Job,[Task,TaskArgs,Mod,State,JobID,UserID,UserAreaPath]).

endmod.

	%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+
	%%     CSP SCRIPTS for QUEUES and WORKERS
	%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+

module task_intf.

export csp_script/4.
csp_script(send_cur_main_queue,
			[],
			TaskEnv,
			[
			 server_info(SInfo),
			 task(current_queue(RawQ)),
			 full_delay(RawQ)^[
			 	queue_representation(RawQ, QEntries),
			 	length(QEntries, NumEntries),
			 	pack_for_client(QEntries, QEntriesExpr),
printf(user_output,'&&&>**admin_resp@cur_main_queue@%t@%t\n',[NumEntries, QEntriesExpr]),
flush_output(user_output),
			  	respond('**admin_resp@cur_main_queue@%t@%t',[NumEntries, QEntriesExpr])
				]
			]
		).

csp_script(send_known_workers,
			[],
			TaskEnv,
			[
			 known_workers(WrkrList),
			 full_delay(WrkrList)^[
			 	length(WrkrList, NumWrkrs),
			 	pack_for_client(WrkrList, WrkrListExpr),
printf(user_output,'&&&>**admin_resp@known_workers@%t@%t\n',[NumWrkrs, WrkrListExpr]),
flush_output(user_output),
			  	respond('**admin_resp@known_workers@%t@%t',[NumWrkrs, WrkrListExpr])
				]
			]
		).

/*
csp_script(send_active_workers,
			[],
			TaskEnv,
			[
			 server_info(SInfo),
			 task(active_workers(WrkrList)),
			 full_delay(WrkrList)^[
			 	length(WrkrList, NumWrkrs),
			 	pack_for_client(WrkrList, WrkrListExpr),
printf(user_output,'&&&>**admin_resp@active_workers@%t@%t\n',[NumWrkrs, WrkrListExpr]),
flush_output(user_output),
			  	respond('**admin_resp@active_workers@%t@%t',[NumWrkrs, WrkrListExpr])
				]
			]
		).
*/

endmod.

module socket_comms.

/*!----------------------------------------------------------------------*
 |	queue_representation/2
 |	queue_representation(QEntries, RepsList)
 |	queue_representation(+,-)
 |
 |	- Input: QEntries = the raw server queue 
 |		(Note: terminates with an uninstantiated variable)
 |	- Output: RepsList= proper list of representations of queue entries
 *-----------------------------------------------------------------------*/

export queue_representation/2.
queue_representation(L, [])
	:-
	var(L),
	!.
queue_representation([], []).
queue_representation([RawItem | RawQ], [QE | QEntries])
	:-
	queue_rep(RawItem, QE),
	queue_representation(RawQ, QEntries).

queue_rep(s(Socket), QE)
	:-!,
	sio:stream_name(Socket, socket(Host,Port,_,_)),
	sprintf(atom(QE),'{skt: port=%t}',[Port]).

queue_rep(n(Socket), QE)
	:-!,
	sio:stream_name(Socket, socket(Host,Port,_,_)),
	sprintf(atom(QE),'{nskt: port=%t}',[Port]).

queue_rep(m(X), QE)
	:-!,
	catenate('m-',X,QE).


queue_rep(RawItem, QE)
	:-
	RawItem = c(SR,SW,State,ConnType),
	!,
	sio:stream_name(SR, socket(Host,Port,_,_)),
	access_login_connection_info(logged_in, State, User),
	access_login_connection_info(ip_num, State, IPNum),
	access_login_connection_info(conn_time, State, ConnTime),
	access_login_connection_info(conn_date, State, ConnDate),
	access_login_connection_info(trans_ctr, State, TransCtr),
	access_login_connection_info(expect, State, Expect),
	sprintf(atom(QE),'{%t,%t: ip=%t ct=%t-%t nt=%t exp=%t}',
				[User,ConnType,IPNum,ConnTime,ConnDate,TransCtr,Expect]).

queue_rep(Item, '?').

/*!----------------------------------------------------------------------*
 |	known_workers/1
 |	known_workers(WrkrList)
 |	known_workers(-)
 *-----------------------------------------------------------------------*/
export known_workers/1.
known_workers(WrkrList, PIDList)
	:-
	configs_terms(CfgSrcItems),
	findall(WI, member(worker_info=WI, CfgSrcItems),  RawWkrs),

	get_worker_dbs(WrkDB),
	functor(WrkDB, wdb, NumWkrs),
	WrkDB =.. [wdb | WkrRecList],
	WrkrList = [Titles | WrkrList0],
	worker_titles(Titles),
	active_wrkr_info(WkrRecList, RawWkrs, WrkrList0,  WrkrListTail, 
					ActiveWkrsInfo),

	inactive_wrkr_info(RawWkrs, ActiveWkrsInfo, WrkrListTail).

active_wrkr_info([], RawWkrs, WrkrListTail,  WrkrListTail, []).

active_wrkr_info([0 | WkrRecList], RawWkrs, WrkrList,  WrkrListTail, RawActiveWkrs)
	:-!,
	active_wrkr_info(WkrRecList, RawWkrs, WrkrList,  WrkrListTail, RawActiveWkrs).

active_wrkr_info([WR | WkrRecList], RawWkrs, [WRInfo | WrkrList0],  WrkrListTail, 
					[AWI | RawActiveWkrs])
	:-
	access_wkr_rec(worker_info,WR,AWI),
	active_inst(WR, AWI, RawWkrs, WRInfo),
	active_wrkr_info(WkrRecList, RawWkrs, WrkrList0,  WrkrListTail, RawActiveWkrs).

active_inst(WR, AWI, RawWkrs, WRInfo)
	:-
	access_wkr_info(host_name,AWI,Name),		%% Host (normal) name
	access_wkr_info(host_ip,AWI,IP),			%% Host IP address
	access_wkr_info(login_name,AWI,Usr),		%% Name of account to login under
	access_wkr_info(port,AWI,Port),				%% Port to connect on
	access_wkr_info(pid,AWI,PID),				%% Port to connect on
	access_wkr_rec(state,WR,State),
	access_wkr_rec(workerID,WR,N),

	member(m(Name,IP,Usr,_,Port,Exe,Dir),RawWkrs),
	wxdir(Exe,Dir,DispDir,DispX),
	ACT = '+ ',
	(State = idle -> DState = '- '; DState = '+ '),
	worker_info_pattern(Pat),
	instantiate_all_entries([Name,IP,N,ACT,DState,PID,Port,Usr,DispDir]),
	sprintf(atom(WRInfo),Pat,
		[Name,IP,N,ACT,DState,PID,Port,Usr,DispDir]).

inactive_wrkr_info([], ActiveWkrsInfo, []).
inactive_wrkr_info([WI | RawWkrs], ActiveWkrsInfo, InactiveWrkrList)
	:-
	WI = m(Name,IP,Usr,_,Port,_,_),
	member(AWI, ActiveWkrsInfo),
	access_wkr_info(host_name,AWI,Name),		%% Host (normal) name
	access_wkr_info(host_ip,AWI,IP),			%% Host IP address
	access_wkr_info(login_name,AWI,Usr),		%% Name of account to login under
	access_wkr_info(port,AWI,Port),				%% Port to connect on
	!,
	inactive_wrkr_info(RawWkrs, ActiveWkrsInfo, InactiveWrkrList).
inactive_wrkr_info([WI | RawWkrs], ActiveWkrsInfo, 
					[InactInst | InactiveWrkrList])
	:-
	inactive_inst(WI, InactInst),
	inactive_wrkr_info(RawWkrs, ActiveWkrsInfo, InactiveWrkrList).

inactive_inst(WI, InactInst)
	:-
	WI = m(WName,IP,Usr,_,[Skt|_],Exe,Dir),
	wxdir(Exe,Dir,DispDir,DispX),
	ACT = '- ',
	DState = '- ',
	N = 0,
	PID = 0,
	worker_info_pattern(Pat),
	sprintf(atom(WInfo),Pat,
		[WName,IP,N,ACT,DState,PID,Skt,Usr,DispDir]).

instantiate_all_entries([]).
instantiate_all_entries([Item | Tail])
	:-
	(nonvar(Item),! ; Item = '-'), 
	instantiate_all_entries(Tail).

/*------------------------------------------------------
 Raw worker info:
 m(calder,'204.5.42.24',ken,logical,[30003],
			'/dakota/chem/eng/spc_skt/zparcwkr-hp9.05',
			'/dakota/chem/eng/server_hp').

    CurWkrRecs: listof WorkerRec, built by:

  Worker Recs:
  -----------
	xmake_wkr_rec(WorkerRec, [WkRS, WkWS, idle, ID, WorkerInfo])
	use:
	access_wkr_rec(workerID,WorkerRec,N)
            state/idle,			%% worker state (idle/...)
			workerID/0,			%% assigned worker id (num)
			worker_info/nil		%% detailed worker info structure

  Worker Info in Worker Recs:
  --------------------------
	defStruct(wkr_info, [
		propertiesList = [
			host_name/nil,		%% Host (normal) name
			host_ip/nil,		%% Host IP address
            login_name/nil,		%% Name of account to login under
			login_pw/nil,		%% Password for login
			port/nil,			%% Port to connect on
			std_in/nil,			%% Write (for server) socket to worker std_in
			std_out/nil,		%% Read (for server) socket to worker std_out
			pid/nil				%% Running process id
		accessPred = access_wkr_info,
 *-------------------------------------------------------*/

worker_titles(Line)
	:-
	sprintf(atom(Line),
		'{%t\t%t\t%t\t%t\t%t\t%t\t%t\t%t\t%t}',
		['MachName','IP','N ','Act','Busy','PID','Socket','UserID','Dir'] ).

worker_info_pattern(
		'{%t\t%t\t%t\t%t\t%t\t%t\t%t\t%t\t%t}'
	).


wxdir(Exe,Dir,DispDir,DispX)
	:-
	pathPlusFile(XPath,XF,Exe),
	(XPath=Dir -> 
		DispDir = Dir,
		DispX = XF
		;
		DispX = Exe,
		DispDir = Dir
	).




endmod.
