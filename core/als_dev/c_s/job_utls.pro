/*======================================================================*
 | 			job_utls.pro 
 |		Copyright (c) 1996 Applied Logic Systems, Inc.
 |      Distribution rights per Copying ALS
 |
 |		Utilities for:
 |		Management of background "worker" processes from
 |		the server manager, or in-process 0-worker execution
 |
 | Author: Ken Bowen
 | Started: September, 1996
 *======================================================================*/

module socket_comms.

export handle_local_job/3.
handle_local_job(Job, continue, SInfo)
	:-
	Job = SIV^appl_exec(Task,TaskArgs,Mod,TaskEnv,SIV),
	SIV = SInfo,
	call(appl_exec(Task,TaskArgs,Mod,TaskEnv,SIV)).

export run_local_job/8.
run_local_job(Job,Task,TaskArgs,Mod,CfgTerms,TaskEnv, OutFileName,SInfo)
	:-
	short_setup_for_job(Job,CfgTerms,UserID,JobID,CurPath,State),
	cont_run_job(Task, TaskArgs, UserID, JobID, OutFileName, State, SInfo), 
		%	info('Job finished: %t %t file=%t\n',[Task, Job, OutFileName]),
	change_cwd(CurPath).

export run_job/6.
run_job(Job,SR,SW,continue,CfgTerms,SInfo)
	:-
			%% Note SR,SW might not be the same as the pair
			%% embedded in Job --- repair....
	setup_for_job(Job,CfgTerms,Task,TaskArgs,UserID,JobID,CurPath,State),
	cont_run_job(Task, TaskArgs, UserID, JobID, OutFileName, State, SInfo), 
		%% info('Job finished: %t %t file=%t\n',[JobType, Job, OutFileName]),
	server_info_out(job_done, SR, [Task,UserID,JobID,OutFileName], SInfo),

	change_cwd(CurPath),
	Response = job_done(JobID, UserID, TaskArgs),
	printf(SW, '%t.\n', [Response],[quoted(true)]),
	flush_output(SW),
		%% info('Sent response: %t\n',[Response]).
	server_info_out(sent_resp, SR, [Response], SInfo).

cont_run_job(Task,TaskArgs, UserID, JobID, OutFileName, State, SInfo)
	:-
	job_exec(Task, TaskArgs, UserID, JobID, State, OutFileName),
	!,
		%% info('>job_exec success: %t - %t\n',[Task, JobID]).
	server_info_out(job_yes, SR, [Task, JobID], SInfo).

cont_run_job(Task,TaskArgs, UserID, JobID, _, State, SInfo)
	:-
		%% info('>job_exec FAILURE: %t - %t\n',[Task, JobID]).
	server_info_out(job_no, SR, [Task, JobID], SInfo).

short_setup_for_job(Job,CfgTerms,UserID,JobID,CurPath,State)
	:-
	access_job_rec(userID, Job, UserID),
	access_job_rec(jobID, Job, JobID),
	access_job_rec(userAreaPath, Job, UserAreaPath),
	access_job_rec(state, Job, State),
	get_cwd(CurPath),
	do_defaults(CfgTerms),
	do_logicals(CfgTerms),
	change_cwd(UserAreaPath).

setup_for_job(Job,CfgTerms,Task,TaskArgs,UserID,JobID,CurPath,State)
	:-
	access_job_rec(task, Job, Task),
	access_job_rec(taskArgs, Job, TaskArgs),
		%% info('Running job: %t %t\n',[Task, Job]),
	short_setup_for_job(Job,CfgTerms,UserID,JobID,CurPath,State).

endmod.
