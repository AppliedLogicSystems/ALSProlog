/*======================================================================*
 | 			job_rectp.typ 
 |		Copyright (c) 1995-96 Applied Logic Systems, Inc.
 |      Distribution rights per Copying ALS
 |
 |		Job and worker record structures
 |
 | Author: Ken Bowen
 | Started: November, 1995
 *======================================================================*/

module socket_comms.

	%%	Fundamental (local/remote) job record: 
defStruct(job_rec, [
		propertiesList = [
			task/nil,			%% task name (functor of call)
			taskArgs/[],		%% 
            mod/user,			%% module in which to run task goals
            state/nil,			%% login state record for user connect
            jobID/0,			%% JobID
			userID/nil,			%% UserID
			userAreaPath/nil	%% Path to user's work area
			],
		accessPred = access_job_rec,
		setPred =    set_job_rec,
		makePred =   make_job_rec,
		structLabel = job_rec
	] ).

	%%	Remote worker record/queue structure:
defStruct(wkr_rec, [
		propertiesList = [
			read_s/nil,			%% read  socket to worker process
			write_s/nil,		%% write socket to worker process
            state/idle,			%% worker state (idle/...)
			workerID/0,			%% assigned worker id (num)
			worker_info/nil		%% detailed worker info structure
		],
		accessPred = access_wkr_rec,
		setPred =    set_wkr_rec,
		makePred =   make_wkr_rec,
		structLabel = wkr_rec
	] ).

	%%	Remote worker information structure:
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
		],
		accessPred = access_wkr_info,
		setPred =    set_wkr_info,
		makePred =   make_wkr_info,
		structLabel = wkr_info
	] ).

	%%	Task global environment structure:
defStruct(tsk_env, [
		propertiesList = [
			wkr_use/no_worker,	%% no_worker/worker_job
            jobID/0,			%% JobID
			userID/nil,			%% UserID
			read_s/nil,			%% read  socket to worker process
			write_s/nil,		%% write socket to worker process
            state/nil			%% login state record for user connect
		],
		accessPred = access_tsk_env,
		setPred =    set_tsk_env,
		makePred =   make_tsk_env,
		structLabel = tsk_env
	] ).


endmod.
