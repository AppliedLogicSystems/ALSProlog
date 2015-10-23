/*======================================================================*
 | 			wkr_mgmt.pro 
 |		Copyright (c) 1996 Applied Logic Systems, Inc.
 |
 |		Management of background "worker" processes from
 |		the server manager.
 |
 | Author: Ken Bowen
 | Started: September, 1996
 *======================================================================*/

module socket_comms.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%% GLOBAL WORKER VARIABLES
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% Don't remake them if we are restarting:
make_worker_globals
	:-
	clause(get_worker_dbs(_),_),
	!.

make_worker_globals
	:-
	make_gv('_worker_dbs'), set_worker_dbs([]),
	make_gv('_idle_workers'), set_idle_workers([]),
	make_gv('_waiting_job_queue'), set_waiting_job_queue((Var,Var)).

	%%-----------------------------------------------------
	%% Master (static) list of all workers; this is just
	%% a list pointing at all of the worker records which
	%% have been created; the worker records are just the
	%% Rec's entered on the server's main queue; workers
	%% get put on this list when (and only when) they
	%% are created; this can happen in start_all_workers/_,
	%% or can happen in sktserve.pro at the clause for
	%% disp_service_request/8 which handles the message
	%%		worker_connect_init(_,_,_)
	%%-----------------------------------------------------
%% :- make_gv('_worker_dbs'), set_worker_dbs([]).

add_worker(Worker)
	:-
	get_worker_dbs(CurWkrRecs),
	set_worker_dbs([Worker | CurWkrRecs]).

remove_dead_worker(Worker)
	:-
	get_worker_dbs(WkrRecs),
	list_delete(WkrRecs,Worker,NewWkrRecs),
	set_worker_dbs(NewWkrRecs).

	%%-----------------------------------------------------
	%% List of all currently available (idle) workers:
	%%-----------------------------------------------------
%% :- make_gv('_idle_workers'), set_idle_workers([]).

pop_idle_worker(Worker)
	:-
	get_idle_workers([Worker1 | RestWkrRecs]),
	Worker1 = c(SR,_,_,_),
	(stream_open_status(SR, open) ->
		set_idle_workers(RestWkrRecs),
		Worker = Worker1
		;
		remove_dead_worker(Worker1),
		pop_idle_worker(Worker)
	).

push_idle_worker(Worker)
	:-
	get_idle_workers(CurWkrRecs),
	set_idle_workers([Worker | CurWkrRecs]).

	%%-----------------------------------------------------
	%% Queue of Main Queue records which have submitted jobs 
	%% not yet sent to any worker (and not being performed by 
	%% the master server):
	%%-----------------------------------------------------
%% :- make_gv('_waiting_job_queue'), set_waiting_job_queue((Var,Var)).

pop_next_job(RecWithJob)
	:-
	get_waiting_job_queue(JobQueue),
	JobQueue = (Head, Tail),
	nonvar(Head),
	Head = [RecWithJob | RestHead],
	set_waiting_job_queue(RestHead, Tail).
							 
%pop_next_job(job_queue_empty).
							  
push_next_job(RecWithJob)
	:-
	get_waiting_job_queue(JobQueue),
	JobQueue = (Head, Tail),
	Tail = [RecWithJob | NewTail],
	set_waiting_job_queue(Head, NewTail).
												   
/*
queue_up_job_req(QJ, State)
	:-
	get_job_queue((GQHead, GQTail)),
	Tail = [QJ | NewGQTail],
	set_job_queue((GQHead, NewGQTail)),
	add_job_rec(QJ, State).
*/

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%% STARTING WORKERS
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	/*------------------------------------------------------*
	 |	Try to start any workers which are listed in
	 |	the *.ini file, modulo any qualification on
	 |	the command line:
	 *------------------------------------------------------*/
worker_initialize(CmdLine, ServerPort, SInfo)
	:-
	make_worker_globals,
	access_server_info(log_file, SInfo, LogFile),
	configs_terms(CfgSrcItems),
	server_warning(checking_workers, [], SInfo),
	worker_initialize(CfgSrcItems, CmdLine, ServerPort, SInfo, LogFile),
	server_warning(workers_check_done, [], SInfo).

worker_initialize(CfgSrcItems, CmdLine, SP, SInfo, LogFile)
	:-
	minimum_num_workers(CfgSrcItems, CmdLine, MinNumWorkers),
	(MinNumWorkers = 0 ->
		task_intf:abolish(use_workers,0)
		;
		task_intf:assert(use_workers),
		start_workers(MinNumWorkers, CfgSrcItems, CmdLine, SP, SInfo, NW)
	).

	%% Command-line value overrides anything else:
minimum_num_workers(CfgSrcItems, CmdLine, MinNumWorkers)
	:-
	dmember(min_workers=MinNumWorkersAtom,CmdLine),
	atomread(MinNumWorkersAtom, MinNumWorkers),
	!.

minimum_num_workers(CfgSrcItems, CmdLine, MinNumWorkers)
	:-
	dmember(min_workers=MinNumWorkers, CfgSrcItems),
	!.

	%% Normally need to start at least one, for batch jobs:
minimum_num_workers(CfgSrcItems, CmdLine, 1).

	/*------------------------------------------------------*
	 |	Check to determine whether the server process
	 |	being started is in fact being started as some
	 |	sort of worker process:
	 *------------------------------------------------------*/

begin_free_worker(CmdLine, SInfo)
	:-
	dmember(st=fws, CmdLine),
	!,
%printf(user_output,'ENTER: begin_free_worker\n',[]),flush_output(user_output),
	assert(process_character(worker)),
	getpid(MyPID0), MyPID is floor(MyPID0), 
		%% Get initial necessary info from commandline:
	dmember(uport=MyPortAtom,CmdLine),
	dmember(uhost=MyHost,CmdLine),
	atomread(MyPortAtom, MyPort),
	set_server_info(ports_list, SInfo, [MyPort]),
	master_info_cmd_line(CmdLine, SInfo).

begin_free_worker(CmdLine, SInfo)
	:-
	assert(process_character(main_server)).

master_info_cmd_line(CmdLine, SInfo)
	:-
	dmember(chost=MasterHost,CmdLine),
	dmember(cport=ICPortAtom,CmdLine),
	dmember(id=MyID,CmdLine),
	!,
	atomread(ICPortAtom, ICPort),
	set_server_info(master_host,SInfo,MasterHost),
	set_server_info(master_port,SInfo,ICPort),
	set_server_info(worker_id,SInfo,MyID).

master_info_cmd_line(CmdLine, SInfo).

	/*------------------------------------------------------*
	 |	If we (the server process being started) are being
	 |	started as a worker process (as determined from the 
	 |	command-line driven, check to see whether we are
	 |	already connected to the master process, and make the
	 |	connection if we are not.
	 *------------------------------------------------------*/

check_master_connect(CmdLine,InitQueueTail, QueueTail, SInfo)
	:-
%printf('TRY:check_master_connect:cmdline= ||%t||\n',[CmdLine]),flush_output,
	dmember(chost=MasterHost,CmdLine),
	dmember(cport=MasterPortAtom,CmdLine),
	atomread(MasterPortAtom, MasterPort),
%printf('check_master_connect-call: special_connect_job\n',[]),flush_output,
	getpid(PID0), PID is floor(PID0),
	special_connect_job(MasterHost, MasterPort, PID, JobRec, SInfo),
	!,
	InitQueueTail = QueueTail,
	JobRec = c(_,MWS,_,_),
	access_server_info(worker_id,SInfo,MyID),
	dmember(uport=MyPortAtom,CmdLine),
	dmember(uhost=MyHost,CmdLine),
	dmember(id=MyID,CmdLine),
%printf('check_master_connect-issue: worker_connect_init\n',[]),flush_output,
	printf(MWS, 'worker_connect_init(\'%t\',\'%t\',%t,%t).\n',
						[MyID,MyHost,MyPortAtom,PID]),
	flush_output(MWS).

check_master_connect(CmdLine, QueueTail, QueueTail, SInfo).

	/*------------------------------------------------------*
	 |	Actually open the sockets back to the master server,
	 |	and build the queue Rec we will maintain for this
	 |	connection (remember that the worker process is just
	 |	another server):
	 *------------------------------------------------------*/

	%%------------------------------------------------------
	%% Initial version: running on same machine as master
	%% server, in same user areas:
	%%------------------------------------------------------
special_connect_job(Host, Port, PID, JobRec, SInfo)
	:-
	catch(open(socket(inet_stream,Host,Port),read,MRS,
						[read_eoln_type(lf),snr_action(snr_code)]),
			_,
					%% Make into better error dianostics: 
			fail),
	(is_server_socket(MRS) ->
		close(MRS),
					%% Make into better error dianostics: 
		fail
		;
		open(socket(clone,MRS),write,MWS,[write_eoln_type(lf)])
	),
	JobRec = c(MRS,MWS,InitState,login),
	ConnectAddr = Host,
	initial_state(login,ConnectAddr,MRS,MWS,
					initial_state(ConnectAddr),InitState,Flag,SInfo),
	set_login_connection_info(ip_num, InitState, Host),

	gethostbyaddr(Host,_,[HostName | _],_),
	set_login_connection_info(hostname, InitState, HostName),
	set_login_connection_info(pid, InitState, PID),
	open(null_stream(foobar),write,LogS, []),
	set_login_connection_info(user_log_stream, InitState, LogS).

	/*--------------------------------------------------------------*
	 |	
	 *--------------------------------------------------------------*/

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
bld_wrkcfgs([BWD | BWDL], DfltWorkerCmd, DfltWorkerDir, [WCF | WorkerConfigs])
	:-
	bld_wcfg(BWD, DfltWorkerCmd, DfltWorkerDir, WCF),
	bld_wrkcfgs(BWDL, DfltWorkerCmd, DfltWorkerDir, WorkerConfigs).

bld_wcfg(BWD, DfltWorkerCmd, DfltWorkerDir, WCF)
	:-
	(BWD = m(HostName,IP,UserAcct,PW,Port) ->
		WorkerCmd = DfltWorkerCmd,
		WorkerDir = DfltWorkerDir
		;
		(BWD = m(HostName,IP,UserAcct,PW,Port,WorkerCmd) ->
			WorkerDir = DfltWorkerDir
			;
			BWD = m(HostName,IP,UserAcct,PW,Port,WorkerCmd,WorkerDir)
		)
	),
	WCF = m(HostName,IP,Port,UserAcct,PW, WorkerCmd, WorkerDir).

	/*--------------------------------------------------------------*
	 *--------------------------------------------------------------*/
start_workers(MinNumWorkers, CfgSrcItems, CmdLine, SP, SInfo, NW)
	:-
	config_worker_descs(CfgSrcItems, SrcWorkerConfigs),
	length(SrcWorkerConfigs, NumSrcCfgs),
	Diff is MinNumWorkers - NumSrcCfgs,
	max(Diff, 0, MissingNumCfgs),
	synth_local_clones(MissingNumCfgs, SInfo, ExtraCfgs),
	append(SrcWorkerConfigs, ExtraCfgs, WorkerConfigs),
	access_server_info(host_ip, SInfo, ThisHost),
	start_all_workers(WorkerConfigs, 0, NW, ThisHost, SP, SInfo).

synth_local_clones(Num, SInfo, ExtraCfgs)
	:-
	access_server_info(host_ip, SInfo, ThisHost),
	get_cwd(WDir),
	access_server_info(self_start, SInfo, WCmd),
	access_server_info(ports_list, SInfo, SPorts),
	maximum(SPorts, MaxSPort),
		%% Need to raise exception if this fails:
	MaxSPort + Num < 65536,
	FirstPort is MaxSPort + 1,
	synth_local_clones(Num,FirstPort,ThisHost,WCmd,WDir,SInfo,ExtraCfgs).

synth_local_clones(0, _, _, _,_,_, []) :-!.
synth_local_clones(CurNum,CurPort,ThisHost,WCmd,WDir,SInfo,[Cfg|ExtraCfgs])
	:-
	Cfg = m(ThisHost,ThisHost,CurPort,x,x, WCmd,WDir),
	NxtNum is CurNum - 1,
	NxtPort is CurPort + 1,
	synth_local_clones(NxtNum,NxtPort,ThisHost,WCmd,WDir,SInfo,ExtraCfgs).
	


start_all_workers([], NW, NW, ThisHost,SP, SInfo).

start_all_workers([WCfg | WorkerConfigs], CurN, NW, ThisHost, SP, SInfo)
	:-
	gensym(det_wkr,WID),
	issue_start_worker(WCfg, ThisHost, SP, WID, SInfo),
	NextN is CurN + 1,
	start_all_workers(WorkerConfigs, NextN, NW, ThisHost, SP, SInfo).

	/*--------------------------------------------------------------*
	 |	issue_start_worker/5
	 |	issue_start_worker(WCfg, ThisHost, SP, WID, SInfo)
	 |	issue_start_worker(+, +, +, +, +)
	 |
	 |	Try to start/connect to an individual worker:
	 |
	 |	Success: Issues appropriate messages; real connection
	 |	occurs when the candidate worker responds with
	 |		worker_connect_init     message.
	 *--------------------------------------------------------------*/
	%% Can we connect to the port?
issue_start_worker(WCfg, ThisHost, SP, WID, SInfo)
	:-
	WCfg =  m(WorkerHost,IP,WPort,UserAcct,PW, WorkerCmd,WorkerDir),
			%% Can we open a read socket on the port?
	catch(open(socket(inet_stream,WorkerHost,WPort),read,WkRS,
			[read_eoln_type(lf),snr_action(snr_code)]),
			_,
		 	fail
	),
			%% Yes; Is there a process & can we connect:
	(proceed_start_worker(WkRS, WorkerHost, WPort, WCfg, ThisHost, SP, WID, SInfo) ->
		true
		;
		server_info_out(running_no_connect, 0, [WorkerHost,WPort], SInfo),
		fail
	).

	%% No worker running that we can connect to; Setup start worker process:
issue_start_worker(WCfg, ThisHost, SP, WID, SInfo)
	:-
	WCfg =  m(WHostName,IP,WorkerPort,User,PW, WorkerCmd,WorkerDir),
	spawn_free_worker(ThisHost,SP,WHostName,WID,WCfg, SInfo).

	%% Worker process is to run on the same machine as this server:
spawn_free_worker(SHost,SP,WHost,WID,WCfg, SInfo)
	:-
	WCfg =  m(WHost,WIP,WPort,UserAcct,PW, WorkerCmd,WorkerDir),
	sprintf(atom(Cmd),
	'%t -g start_server -p -st fws -chost %t -cport %t -uhost %t -uport %t -id %t -dir %t -min_workers 0 &',
		[WorkerCmd,SHost,SP,WIP,WPort,WID,WorkerDir]),

	server_info_out(spawn_attempt, 0, [WHost,WPort,WID], SInfo),
	system(Cmd).

	/*--------------------------------------------------------------*
	 |	proceed_start_worker/8
	 |	proceed_start_worker(WkRS, WorkerHost, Port, WCfg, ThisHost, SP, WID, SInfo)
	 |	proceed_start_worker(+, +, +, +, +, +, +, +)
	 |
	 |	Try to connect to a running worker process:
	 *--------------------------------------------------------------*/
			%% Opened socket; Is it a server socket? - If so, QUIT:
proceed_start_worker(WkRS, WorkerHost, Port, WCfg, ThisHost, SP, WID, SInfo)
	:-
	is_server_socket(WkRS),
	!,
	close(WkRS),
	fail.

		%% Not a server socket (we're a client); Try to connect to it:
proceed_start_worker(WkRS, WorkerHost, WorkerPort, WCfg, ThisHost, SP, WID, SInfo)
	:-
	flush_input(WkRS),
	open(socket(clone,WkRS),write,WkWS,[write_eoln_type(lf)]),
	special_connect_job(WorkerHost, WorkerPort, 0, Worker, SInfo),
	catch( (printf(WkWS, 'are_you_available(\'%t\',%t,\'%t\').\n', 
							[ThisHost,SP,WID]), 
			flush_output(WkWS)),
		   _,
		   fail).

	/*--------------------------------------------------------------*
	 |	Kill all workers (if running)
	 *--------------------------------------------------------------*/
kill_all_workers
	:-
	get_worker_dbs(WrkDB),
	functor(WrkDB, wdb, NumWkrs),
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
	WorkerRec = c(_,WkWS,_,_),
	printf(WkWS, '%t.\n', [Job], [quoted(true)]),
	flush_output(WkWS).

get_var_mask([], []).
get_var_mask([TA | TaskArgs], [TA | TaskArgsVarMask])
	:-
	var(TA),
	!,
	get_var_mask(TaskArgs, TaskArgsVarMask).
get_var_mask([TA | TaskArgs], [_ | TaskArgsVarMask])
	:-
	get_var_mask(TaskArgs, TaskArgsVarMask).


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


	%disp_service_request(check_batch_status(UserName, JobID, Status),
	%% serve_ready_stream( obm(SR,SW,Atom,ResultVar,JobID,State), QT, NewQT, Flag, SInfo) 
handle_task(_,check_batch_status,[UserName, JobID, Status],Mod,TaskEnv,SInfo)
	:-!,
	access_tsk_env(state, TaskEnv, State),
	access_tsk_env(read_s, TaskEnv, SR),
	access_tsk_env(write_s, TaskEnv, SW),
	mangle(1, State, 
		add_to_queue(
			check_batch_status(UserName, JobID, SR, SW, State, Status)
		)
		).


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
	pop_idle_worker(Worker),
	!,
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
	access_login_connection_info(waiting_jobs, State, CurJobsList),
	append(CurJobsList, [Job], NewJobsList),
	set_login_connection_info(waiting_jobs, State, NewJobsList).

pop_job_rec(Job, State)
	:-
	access_login_connection_info(waiting_jobs, State, [Job | RestJobsList]),
	set_login_connection_info(waiting_jobs, State, RestJobsList).

/*------------------------------------------------------------------------*
 *------------------------------------------------------------------------*/

mk_job_rec(Task,TaskArgs,Mod,TaskEnv,Job)
	:-
	access_tsk_env(state, TaskEnv, State),
	access_tsk_env(jobID, TaskEnv, JobID),
	access_tsk_env(userID, TaskEnv, UserID),
	xmake_job_rec(Job,[Task,TaskArgs,Mod,State,JobID,UserID,UserAreaPath]).

	%% record_submit_acknowledge(neutral_submit(FileName), JobID, TaskEnv)
export record_submit_acknowledge/3.
record_submit_acknowledge(Msg, JobID, TaskEnv)
	:-
	access_tsk_env(state, TaskEnv, State),
%	access_login_connection_info(job_connects, State, JobConnects),
nl(user_output),
printf(user_output, 'ACK:%t id=%t \n',[Msg, JobID]),
flush_output(user_output).


export job_notify/3.
job_notify(Msg, JobID, TaskEnv)
	:-
	access_tsk_env(state, TaskEnv, State),
	access_login_connection_info(job_connects, State, JobConnects),
	dmember(JobID-Client, JobConnects),
	Client = c(_,SW,_,_),

	cnvrt_msg(Msg, Pattern, Args),

nl(user_output),
write(user_output,'ACK-job_notify: '),
printf(user_output, Pattern, Args, [line_length(500000)]),
flush_output(user_output),

	printf(SW, Pattern, Args, [line_length(500000)]),
	flush_output(SW).


%cnvrt_msg(neutral_submit(FF,JID), '**neutral_submit@%t@%t\n', [FF,JID])
%	:-!.

cnvrt_msg(Msg, '%t', [Msg])
	:-
	atom(Msg), 
	!.

cnvrt_msg(Msg, Pattern, [Functor | Args])
	:-
	functor(Msg, Functor, NArgs),
	Msg =.. [Functor | Args],
%	catenate('**', Functor, InitPat),
	BeginPat = ['**%t' | TailPat],
	setup_respond_args_pat(NArgs, TailPat),
	catenate(BeginPat, Pattern).

setup_respond_args_pat(0, ['\n']) :-!.
setup_respond_args_pat(NArgs, ['@%t' | TailPat])
	:-
	NArgs > 0,
	MArgs is NArgs - 1,
	setup_respond_args_pat(MArgs, TailPat).


batch_outfile_exists(State, UserName, JobID)
	:-
	access_login_connection_info(user_area, State, UserAreaPath),
	file_extension(JobID,bo,BaseBOFile),
	path_elements(UserAreaPath, DirPathElts),
	append(DirPathElts, [BaseBOFile], FilePathElts),
	path_elements(FilePath, FilePathElts),
	exists_file(FilePath).



endmod.

	%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+
	%%     CSP SCRIPTS for QUEUES and WORKERS
	%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+%+

module task_intf.

export csp_script/4.

csp_script(send_cur_main_queue, [], TaskEnv,
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
	 ] ).

csp_script(send_known_workers,
			[],
			TaskEnv,
			[
			 known_workers(WrkrList),
			 full_delay(WrkrList)^[
			 	length(WrkrList, NumWrkrs),
			 	pack_for_client(WrkrList, WrkrListExpr),
%printf(user_output,'&&&>**admin_resp@known_workers@%t@%t\n',[NumWrkrs, WrkrListExpr]), flush_output(user_output),
			  	respond('**admin_resp@known_workers@%t@%t',[NumWrkrs, WrkrListExpr])
				]
			]
		).

csp_script(send_all_info, [], TaskEnv,
	[
	 server_info(SInfo),
	 task(current_queue(RawQ)),
	 full_delay(RawQ)^gather_all_server_info(RawQ,SInfo,InfoList),
	 full_delay(InfoList)^[
		InfoList = [QEntries,WrkrList,Users],
	 	length(QEntries, NumEntries),
		pack_for_client(QEntries, QEntriesExpr),
		length(WrkrList, NumWrkrs),
		pack_for_client(WrkrList, WrkrListExpr),
		length(Users, NumUsers),
		pack_for_client(Users, UsersExpr),
	 	respond('**admin_resp@all_info@%t@%t@%t@%t@%t@%t',
			[NumEntries, QEntriesExpr, NumWrkrs, WrkrListExpr,NumUsers, UsersExpr])
		]
	] ).




endmod.

module socket_comms.

/*!----------------------------------------------------------------------*
 *-----------------------------------------------------------------------*/
export gather_all_server_info/3.
gather_all_server_info(RawQ,SInfo,InfoList)
	:-
	queue_representation(RawQ, QEntries),
	known_workers(WrkrList),
	cur_logged_in_users(Users, SInfo),
	InfoList = [QEntries,WrkrList,Users].


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
/*
queue_representation(L, [])
	:-
	var(L),
	!.
queue_representation([], []).
queue_representation([RawItem | RawQ], [QE | QEntries])
	:-
	queue_rep(RawItem, QE),
	queue_representation(RawQ, QEntries).
*/

queue_representation(L, [])
	:-
	var(L),
	!.

queue_representation([], []).

queue_representation([m(_) | RawQ], QEntries)
	:-!,
	queue_representation2(RawQ, QEntries).

queue_representation([_ | RawQ], QEntries)
	:-
	queue_representation(RawQ, QEntries).

queue_representation2(L, [])
	:-
	var(L),
	!.

queue_representation2([], []).

queue_representation2([m(_) | _], []) :-!.

queue_representation2([RawItem | RawQ], [QE | QEntries])
	:-
	queue_rep(RawItem, QE),
	queue_representation2(RawQ, QEntries).


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
known_workers([Titles | WrkrList])
	:-
	get_worker_dbs(AllWorkers),
	get_idle_workers(IdleWorkers),
	worker_titles(Titles),

	setup_worker_info(AllWorkers, IdleWorkers, WrkrList).

setup_worker_info([], _, []).
setup_worker_info([WkrRec | AllWorkers], IdleWorkers, [WRInfo | WorkersInfo])
	:-
	WkrRec = c(SR,SW,WState,CType),
	access_login_connection_info(ip_num, WState, IP),
	access_login_connection_info(port, WState, Port),
	access_login_connection_info(pid, WState, PID),
	gethostbyaddr(IP,_,[Name|_],_),

	worker_info_pattern(Pat),
	(dmember(WkrRec, IdleWorkers) ->
		DState = ' - ' 
		;
		DState = ' + ' 
	),
	instantiate_all_entries([Name,IP,DState,PID,Port,Usr,DispDir]),
	sprintf(atom(WRInfo),Pat,
		[Name,IP,DState,PID,Port,Usr,DispDir]),
	setup_worker_info(AllWorkers, IdleWorkers, WorkersInfo).

instantiate_all_entries([]).
instantiate_all_entries([Item | Tail])
	:-
	(nonvar(Item),! ; Item = '-'), 
	instantiate_all_entries(Tail).

worker_titles(Line)
	:-
	sprintf(atom(Line),
		'{%t\t%t\t%t\t%t\t%t\t%t\t%t}',
		['MachName','IP','Busy','PID','Socket','UserID','Dir'] ).

worker_info_pattern(
		'{%t\t%t\t%t\t%t\t%t\t%t\t%t}'
	).
/*
worker_titles(Line)
	:-
	sprintf(atom(Line),
		'{%t\t%t\t%t\t%t\t%t\t%t\t%t\t%t\t%t}',
		['MachName','IP','N ','Act','Busy','PID','Socket','UserID','Dir'] ).

worker_info_pattern(
		'{%t\t%t\t%t\t%t\t%t\t%t\t%t\t%t\t%t}'
	).
*/


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
