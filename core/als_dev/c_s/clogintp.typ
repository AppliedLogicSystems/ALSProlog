/*======================================================================*
 | 			clogin.typ 
 |		Copyright (c) 1995-96 Applied Logic Systems, Inc.
 |      Distribution rights per Copying ALS
 |
 |		Minimal server information structure type defintion
 |		for client login state structure
 |
 | Author: Ken Bowen
 | Started: November, 1995
 *======================================================================*/

module socket_comms.

	%% The base individual socket (clone) information structure:
defStruct(base_connection_info, [
		propertiesList = [

				%% all applications must have the following,
				%% AND, no matter what:
				%% continue MUST be the very first slot, and
				%% expect   MUST be the second slot:
			continue/continue,		%% continue/done
			expect/nil,				%% nil/expect(What)
            ip_num/0,               %% source IP number
			port/0,                 %% port number
			conn_date/(0/0/0),      %% connect date
			conn_time/(0:0:0),      %% connect time
			disconn_date/(0/0/0),   %% disconnect date
			disconn_time/(0:0:0),   %% disconnect time
			socket_in/nil,			%% input read socket
			socket_out/nil,			%% input write socket
			socket_id/nil			%% id of connected clone socket: s(Port,FID)
			],
		accessPred = access_base_connection_info,
		setPred =    set_base_connection_info,
		makePred =   make_base_connection_info,
		structLabel = base_connection_info
	] ).


	%% Pure login -- for inclusion in other structs:
defStruct(pure_login, [
		propertiesList = [
                %% supporting logged-in users:
			logged_in/nil,			%% logged-in user
			login_time/nil,
			login_date/nil,
			logout_time/nil,
			logout_date/nil,
			last_trans_time/nil,
			trans_ctr/0,			%% count of transactions,
			succ_trans_ctr/0		%% count of successful transactions,
			],
		accessPred = access_pure_login,
		setPred =    set_pure_login,
		makePred =   make_pure_login,
		structLabel = pure_login
	] ).


	%% The basic client socket (clone) information structure,
	%% with remote background job support:

defStruct(basic_client, [
		propertiesList = [

			term_protocol/prolog,		%% {prolog/tty}
			user_options/[],

                %% supporting remote batch jobs:
			job_connects/[],
                %% submitted jobs waiting for execution:
			waiting_jobs/(X,X),

				%% "secure" applications might use the following:
			magic_cookie/nil		%% client magic cookie after connect & login

			],
		accessPred = access_basic_client,
		setPred =    set_basic_client,
		makePred =   make_basic_client,
		structLabel = basic_client
	] ).

	%% Remote (outbound) job connection:
defStruct(remote_job, [
		propertiesList = [

			include(base_connection_info),
			
			username, 
			password,
			msor/nil, %%  read socket to stdout (user_output) of rexec'd process
			msiw/nil, %%  write socket to stdin (user_input)  of rexec'd process
			parent_struct/nil,		%% where this job request originated

			pid/0,
			cookie/nil,
			status/no_connect,
			job_start_cmd/nil
			],
		accessPred = access_remote_job_info,
		setPred =    set_remote_job_info,
		makePred =   make_remote_job_info,
		structLabel = remote_job_info
	] ).


endmod.
