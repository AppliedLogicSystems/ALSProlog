/*======================================================================*
 | 			sktsrvtp.typ 
 |		Copyright (c) 1995-96 Applied Logic Systems, Inc.
 |      Distribution rights per Copying ALS
 |
 |		Minimal server information structure type defintion
 |		for server proper info state structure
 |
 | Author: Ken Bowen
 | Started: November, 1995
 *======================================================================*/

module socket_comms.

defStruct(server_info, [
		propertiesList = [

                %% standard slots for supporting remote jobs:

			max_num_wkrs/1,
			machine_tgts/[],        %% 'Machines' which can run a slave process
			working_machines/[],    %% Working 'machines'
			idle_machines/[],       %% Idle processes already running

                %% standard slots for supporting logged-in users:

			trusted_administration/[],  %% list of trusted id's
			admin_pw/'zZLxE.Tg7nmo6',	%% encrypted pw for direct admin login
			cur_logged_in/[],
			user_file/nil,
			users_area/nil,
			accounting_file/'accting.dfl',
			banner/[],
			motd/[],

				%% all applications should have the following:

			local_read_stream/user_input,    %% 
			local_write_stream/user_output,  %% 
			log_warnings/false,     %% {true,false(default)}
			log_info/false,         %% {true,false(default)}
			log_file/nil,			%% file name for logging
			log_stream/nil,         %% stream to log file (when logging)
			server_warnings/false,  %% {true,false}
			polling_timeout/10000,	%% timeout for polling streams
			ports_list/[],			%% list of basic port numbers
			non_login_ports/[], %% list of basic non_login port numbers

				%% this is for supporting app-specific extensions:
			extension_slot/[]		%% list of add'l Tag=Value pairs
			],
		accessPred = access_server_info,
		setPred =    set_server_info,
		makePred =   make_server_info,
		structLabel = server_info
	] ).

/********
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
			socket_id/nil,			%% id of connected clone socket: s(Port,FID)
			expect/nil
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


	%% The individual client socket (clone) information structure:
defStruct(login_connection_info, [
		propertiesList = [

			include(base_connection_info),
			include(pure_login),

			term_protocol/prolog,		%% {prolog/tty}
			user_options/[],

                %% supporting remote batch jobs:
			job_connects/[],
                %% submitted jobs waiting for execution:
			waiting_jobs/(X,X),

				%% "secure" applications might use the following:
			magic_cookie/nil		%% client magic cookie after connect & login

			],
		accessPred = access_login_connection_info,
		setPred =    set_login_connection_info,
		makePred =   make_login_connection_info,
		structLabel = login_connection_info
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
*******/


endmod.
