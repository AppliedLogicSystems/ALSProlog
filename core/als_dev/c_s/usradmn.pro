/*=====================================================================*
 |			usradmn.pro
 |		Copyright (c) 1996 Applied Logic Systems, Inc.
 |      Distribution rights per Copying ALS
 |
 |		Managing user logins, etc -- generic stuff
 |
 | Author: Ken Bowen
 *=====================================================================*/

		%%%%%%%%%%%%%%%%%%%%%%%%%%
		%% LOGIN PREDICATES
		%%%%%%%%%%%%%%%%%%%%%%%%%%

module usradmn.
use socket_comms.

export usrlog_script/5.
export user_login/9.
export lg_cmplt/8.
export do_logout/7.
export accounting/3.
export currently_logged_in/2.
export validate_login/3.

	%% To be supplied by application, if needed:
export name_only_user_entry_allowed/0.
export login_response_msg/2.

		%%%%%%%%%%%%%%%%%%%%%%%%%%
		%% Scripts
		%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%----------------------------------------------------------------
	%% Look for an administrative login script first, if appropriate:
	%%----------------------------------------------------------------
usrlog_script(admin_login(ID,IPAddr,HostName,PW,SCType), 
					Locals, Globals, Script, WkrUse)
	:-
	admin_log_script(ID,IPAddr,HostName,PW,SCType,
						Locals, Globals, Script, WkrUse).
admin_log_script(ID,IPAddr,HostName,PW,SCType,
					[ID,PW,SInfo], 
					[JobID,UserID,SR,SW,State],
					Script, no_worker)
	:-
	Script = [
		admin_login(ID,IPAddr,HostName,PW,SR,SW,State,SInfo,LRes,Date,Time),
		access_server_info(extension_slot, SInfo, ExtensionsList),
		process_extensions(ExtensionsList, State),
		access_login_connection_info(magic_cookie, State, Cookie),
		message_of_the_day(SCType, LRes, ID, IPAddr, HostName, SInfo, TodaysMOTD),
		login_response_msg(LRes, LType, Msg),
		respond(Msg, [Date, Time, Cookie, TodaysMOTD]),
		lg_cmplt(LRes, ID, Date, Time, SR, SW, State, SInfo)
	].

export admin_login/11.
admin_login(ID,IPAddr,HostName,PW,SR,SW,State,SInfo,LRes,Date,Time)
	:-
	access_server_info(admin_pw, SInfo, EncryptedAdminPW),
	crypt(PW, 'zZ', EncryptIncoming),
	date(Date),time(Time),
	fin_admin_login(EncryptIncoming, EncryptedAdminPW, 
					ID, SInfo, LRes, State, Date, Time).

fin_admin_login(PW, PW, ID, SInfo, LRes, State, Date, Time)
	:-!,
	LRes = ok_admin_login,
	record_login(admin, Date, Time, nil, State),
	access_login_connection_info(port, State, Port),
	access_login_connection_info(socket_id, State, SID),
	addto_current_logins(admin,nil,Port,SID,State,SInfo).

fin_admin_login(EncryptIncoming, EncryptedAdminPW, ID, SInfo, LRes, State, Date, Time)
	:-
	LRes = not_allowed.

motd_text(ok_admin_login, admin, SInfo, TextList)
	:-!,
	TextList = [
	'ZPARC Administrative Login.',
	'',
	'ZPARC Administrative Login granted.'
	].

login_response_msg(ok_admin_login, full_logon, '**full_logon@%t@%t@%t@%t')
	:-!.

	%%---------------------------------------------------
	%% Look for an application-defined login script next:
	%%---------------------------------------------------
usrlog_script(Request, Locals, Globals, Script, WkrUse)
	:-
	user_log_script(Request, Locals, Globals, Script, WkrUse),
	!.

	%% If none, then use the default:
usrlog_script(Request, Locals, Globals, Script, WkrUse)
	:-
	dfltlog_script(Request, Locals, Globals, Script, WkrUse).


		%%-----------------
		%% Default Logins:
		%%-----------------

dfltlog_script(login(ID,Password), [ID,Password,SInfo], 
			[JobID,UserID,SR,SW,State], 
			ScriptTail, no_worker)
	:-
	fin_login_script(ID,Password,SInfo,SR,SW,State,ScriptTail).

dfltlog_script(login(ID), [ID,Password,SInfo], 
			[JobID,UserID,SR,SW,State], 
			[read(Password) | ScriptTail], no_worker)
	:-
	fin_login_script(ID,Password,SInfo,SR,SW,State,ScriptTail).

dfltlog_script(login, [ID,Password,SInfo], 
			[JobID,UserID,SR,SW,State], 
			[read(ID), read(Password) | ScriptTail], no_worker)
	:-
	fin_login_script(ID,Password,SInfo,SR,SW,State,ScriptTail).

dfltlog_script(logon(ID,PW), V1, V2, S, W) 
	:-
	dfltlog_script(login(ID,PW), V1, V2, S, W).

dfltlog_script(logon(ID), V1, V2, S, W) 
	:-
	dfltlog_script(login(ID), V1, V2, S, W).

dfltlog_script(logon, V1, V2, S, W) 
	:-
	dfltlog_script(login, V1, V2, S, W).

fin_login_script(ID,Password,SInfo,SR,SW,State,
	[
	 user_login(ID,Password,SR,SW,State,SInfo,LRes,Date,Time),
	 login_response_msg(LRes, Msg),
	 respond(Msg, [ID, Date, Time]),
	 lg_cmplt(LRes, ID, Date, Time, SR, SW, State, SInfo)
	]). 

		%%-----------------
		%% Default Logout
		%%-----------------

dfltlog_script(logout, [_,_,SInfo],
			[_,_,SR,SW,State], 
			[ 
			  date(Date),time(Time),
			  login_response_msg(logout, _, Msg),
			  respond(Msg, [ID, Date, Time]),
			  do_logout(SR, SW, State, SInfo, ID, Date, Time),
			  lg_cmplt(logout, ID, Date, Time, SR, SW, State, SInfo) ], 
			no_worker).

dfltlog_script(logoff, V1, V2, S, W) 
	:-
	dfltlog_script(logout, V1, V2, S, W).

dfltlog_script(silent_logout, [_,_,SInfo],
			[_,_,SR,SW,State], 
			[ do_logout(SR, SW, State, SInfo, ID, Date, Time),
			  lg_cmplt(logout, ID, Date, Time, SR, SW, State, SInfo) ], 
			no_worker).

lg_cmplt(Tag, ID, Date, Time, SR, SW, State, SInfo)
	:-
	dmember(Tag, [not_allowed, logout, missing_area]),
	!,
	break_connection(logout,[ID],SR,SW,State,SInfo),
	mangle(1, State, done).

lg_cmplt(Tag, ID, Date, Time, SR, SW, State, SInfo).

		%%%%%%%%%%%%%%%%%%%%%%%%%%
		%% Login Managmenet
		%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% Is user already logged in??:
user_login(ID,InitPW,SR,SW,State,SInfo, already_logged_in,Date,Time)
	:-
	currently_logged_in(ID, SInfo),
		%% Yes -- already logged on:
		%% And not a trusted administrator:
	access_server_info(trusted_administration, SInfo, TrustedIDs),
	not(member(ID, TrustedIDs)),
	!,
	date(Date), time(Time).

	%% No -- not already logged on; ID UserFile line exists:
user_login(ID,InitPW,SR,SW,State,SInfo, LRes,Date,Time)
	:-
	get_user_list_entry(ID, SInfo, ID_LineEntry, PWEntry, RestEntry),
	!,
	fin_check_user_logon(InitPW, ID, ID_LineEntry, PWEntry, RestEntry, 
							SR,SW,State,SInfo,LRes,Date,Time).

	%% No ID entry in PW file:
user_login(ID,InitPW,SR,SW,State,SInfo, not_allowed,Date,Time)
	:-
	date(Date), time(Time).

currently_logged_in(ID, SInfo)
	:-
	access_server_info(cur_logged_in, SInfo, CurLoggedIn),
	dmember(l(ID,Port,FID,Cookie,State), CurLoggedIn).

validate_login(UserID,SW,State)
	:-
	access_login_connection_info(logged_in,State,UserID),
	!.
validate_login(UserID,SW,State)
	:-
	login_response_msg(bad_validation, Msg),
	printf(SW, Msg, []), nl(SW),
	flush_output(SW),
	!,
	fail.

	
/*!--------------------------------------------------------------------*
 |	fin_check_user_logon/9
 |	fin_check_user_logon(InitPW, ID, ID_LineEntry, PWEntry, 
 |								RestEntry, SR,SW,State,SInfo)
 |	fin_check_user_logon(+, +, +, +, +,+,+,+,+)
 |
 |	- finish login checking, given a password file entry extists
 *!--------------------------------------------------------------------*/

fin_check_user_logon(InitPW,ID,ID_LineEntry,PWEntry,RestE,
					 SR,SW,State,SInfo,LRes,Date,Time)
	:-
	check_passwd(InitPW,PWEntry,ID,SR,SW,State,SInfo),
	check_logon_src(ID,PWEntry,RestE,SR,SW,State,SInfo),
	!,
	end_check_user_logon(ID,SR,SW,RestE,State,SInfo,LRes,Date,Time).

fin_check_user_logon(InitPW, ID, ID_LineEntry, RestE,PWEntry, 
					SR,SW,State,SInfo,not_allowed,Date,Time)
	:-
	fail_login(ID, SR,SW,Date,Time,State,SInfo).

fail_login(ID, SR,SW,Date,Time,State,SInfo)
	:-
	access_login_connection_info(ip_num, SInfo, IPNum),
	access_login_connection_info(port, SInfo, Port),
	access_login_connection_info(conn_date, SInfo, Date),
	access_login_connection_info(conn_time, SInfo, Time),
	log_server_notify('%t %t %t %t\n', [ID,Date,Time,Port], 'Logon Failure: ', SInfo).

/*!--------------------------------------------------------------------*
 |	end_check_user_logon/6
 |	end_check_user_logon(ID,SR,SW,RestEntry,State,SInfo)
 |	end_check_user_logon(+,+,+,+,+)
 |
 |	- complete login, given ID & password are ok
 *!--------------------------------------------------------------------*/
end_check_user_logon(ID,SR,SW,RestEntry,State,SInfo,ok_logged_in,Date,Time)
	:-
	access_server_info(users_area, SInfo, UserDir),
	extendPath(UserDir, ID, ThisUserArea),
	exists_file(ThisUserArea),
	!,
	set_login_connection_info(users_area, State, UserDir),
	set_login_connection_info(user_area, State, ThisUserArea),
	filePlusExt(ID, log, BaseLogFile),
	pathPlusFile(ThisUserArea,BaseLogFile,LogFile),
	catenate(log_file_, ID, UserLogFileAlias),
	catch(open(LogFile, append, UsrLogOutS, [alias(UserLogFileAlias)]),_,fail),
	set_login_connection_info(user_log_file, State, LogFile),
	set_login_connection_info(user_log_stream, State, UsrLogOutS),

	date(Date), time(Time),
	access_login_connection_info(ip_num, State, IPNum),
	printf(UsrLogOutS, 'login(%t,%t,%t).\n',[Date,Time,IPNum]),

	gensym(logsession, InitCookie),
	atom_length(InitCookie,ICLL),
	ICLL1 is ICLL - 1,
	sub_atom(InitCookie, 2, ICLL1, Cookie),

	record_login(ID, Date, Time, Cookie, State),
	accounting(login, State, SInfo),
	log_server_notify('%t %t %t %t\n', [ID,Date,Time,Cookie], 'Login: ', SInfo),
	access_login_connection_info(socket_id, State, SID),
	access_login_connection_info(port, State, Port),
	addto_current_logins(ID,Cookie,Port,SID,State,SInfo).

	%% User dir doesn't exist:
end_check_user_logon(ID,SR,SW,RestEntry,State,SInfo,not_allowed,Date,Time)
	:-
	access_login_connection_info(ip_num, SInfo, IPNum),
	access_login_connection_info(port, SInfo, Port),
	access_login_connection_info(conn_date, SInfo, Date),
	access_login_connection_info(conn_time, SInfo, Time),
	log_server_notify('%t %t %t %t\n', [ID,Date,Time,Port],'Missing User Area: ',SInfo).

/*!--------------------------------------------------------------------*
 |	get_user_list_entry/4
 |	get_user_list_entry(ID, SInfo, ID_LineEntry, PWEntry)
 |	get_user_list_entry(+, +, -, -)
 |
 |	- get entry matching ID from password file
 *!--------------------------------------------------------------------*/
get_user_list_entry(ID, SInfo, ID_LineEntry, PWEntry, RestEntry)
	:-
	access_server_info(user_file, SInfo, UserListFile),
	grab_lines(UserListFile, UserFileLines),
	check_for_entry(UserFileLines, ID, ID_LineEntry, PWEntry, RestEntry).

check_for_entry([Line | _], ID, Line, PWEntry, RestEntry)
	:-
	atom_codes(Line,LineCs),
	check_entry_line(LineCs, ID, PWEntry, RestEntry),
	!.

check_for_entry([_ | UserFileLines],ID, ID_LineEntry, PWEntry, RestEntry)
	:-
	check_for_entry(UserFileLines,ID, ID_LineEntry, PWEntry, RestEntry).


check_entry_line(LineCs, ID, PWEntry, RestEntry)
	:-
	asplit0(LineCs, 0':, NmCs, TailCs),
	atom_codes(LineName, NmCs),
	LineName = ID,
	!,
	extract_pw(TailCs, PWEntry, RestEntry).

		%% Probably should go away: allows name-only entries:
:- dynamic(name_only_user_entry_allowed/0).
check_entry_line(LineCs, ID, '', '')
	:-
	name_only_user_entry_allowed,
	atom_codes(LineName, LineCs),
	LineName = ID.

extract_pw(TailCs, PWEntry, RestEntry)
	:-
	asplit0(TailCs, 0':, PWCs, RestEntryCs),
	!,
	atom_codes(PWEntry, PWCs),
	atom_codes(RestEntry, RestEntryCs).

extract_pw(TailCs, PWEntry, '')
	:-
	atom_codes(PWEntry, TailCs).

/*!--------------------------------------------------------------------*
 |	check_passwd/7
 |	check_passwd(InitPW, PWEntry, ID, SR, SW, State, SInfo)
 |	check_passwd(+, +, +, +, +, +, +)
 |
 |	- check input password against entry from user file
 *!--------------------------------------------------------------------*/
check_passwd(InitPW, PWEntry, ID, SR, SW, State, SInfo)
	:-
	var(InitPW),
	!,
	fail.

check_passwd(InitPW, '', ID, SR, SW, State, SInfo)
	:-!.

:- dynamic(wildcard_pw_allow/1).

check_passwd(InitPW, PWEntry, ID, SR, SW, State, SInfo)
	:-
	wildcard_pw_allow(InitPW),
	!.

check_passwd(InitPW, PWEntry, ID, SR, SW, State, SInfo)
	:-
	crypt(InitPW, zu, EInitPW),
	EInitPW = PWEntry.

/*!--------------------------------------------------------------------*
 *!--------------------------------------------------------------------*/
decrypt(PWLine, PWLine, _).

/*!--------------------------------------------------------------------*
 *!--------------------------------------------------------------------*/
check_logon_src(ID,PWEntry,RestE,SR,SW,State,SInfo)
	:-
	access_login_connection_info(ip_num, State, SourceIPNum),
	in_restricted_domain(SourceIPNum).

in_restricted_domain(SourceIPNum)
	:-
	no_domain_restriction_on_server,
	!.

in_restricted_domain(SourceIPNum)
	:-
	catch(gethostbyaddr(SourceIPNum,HostName),
			_, fail),
	allowed_domains(DomainsList),
	(member(Dom, DomainsList), 
		ends_in(HostName, Dom), !
		;
		try_full_addr(SourceIPNum, DomainsList) 
	).

ends_in(Atom, TailAtom)
	:-
	sub_atom(Atom, Start, TailLength, TailAtom),
	atom_length(Atom, AL),
	AL is Start + TailLength - 1.

try_full_addr(SourceIPNum, DomainsList)
	:-
	catch(gethostbyaddr(SourceIPNum,_,NameList,_),
			_, fail),
	member(HostName,NameList),
	member(Dom, DomainsList), 
	ends_in(HostName, Dom),
	!.


/*!--------------------------------------------------------------------*
 |	addto_current_logins/6
 |	addto_current_logins(ID,Cookie,Port,SID,State,SInfo)
 |	addto_current_logins(+,+,+,+,+,+)
 |
 |	- add new login info to server's list in SInfo structure
 *!--------------------------------------------------------------------*/
addto_current_logins(ID,Cookie,Port,SID,State,SInfo)
	:-
	access_server_info(cur_logged_in, SInfo, PrevLoggedIn),
	set_server_info(cur_logged_in, SInfo, 
		[l(ID,Port,SID,Cookie,State) | PrevLoggedIn]).

rem_from_current_logins(ID,Cookie,Port,SID,State,SInfo)
	:-
	access_server_info(cur_logged_in, SInfo, CurLoggedIn),
	rem_from_list(CurLoggedIn, ID, Port, SID, Cookie, State,NewLoggedIn),
	set_server_info(cur_logged_in, SInfo,  NewLoggedIn).

rem_from_list([], ID, Port, SID, Cookie, []).

rem_from_list([l(ID,Port,SID,Cookie,State) | List],ID,Port,SID,Cookie,State,List)
	:-!.

rem_from_list([Skip | List], ID, Port, SID, Cookie, State, [Skip | NewLoggedIn])
	:-
	rem_from_list(List, ID, Port, SID, Cookie, State, NewLoggedIn).

/*!--------------------------------------------------------------------*
 |	record_login/5
 |	record_login(ID, Date, Time, Cookie, State)
 |	record_login(+, +, +, +, +)
 |
 |	- record login information in connection State structure
 *!--------------------------------------------------------------------*/
record_login(ID, Date, Time, Cookie, State)
	:-
	set_login_connection_info(logged_in, State, ID),
	set_login_connection_info(login_date, State, Date),
	set_login_connection_info(login_time, State, Time),
	set_login_connection_info(last_trans_time, State, Date+Time),
	set_login_connection_info(magic_cookie, State, Cookie).

		%%%%%%%%%%%%%%%%%%%%%%%%%%
		%% LOGOUT
		%%%%%%%%%%%%%%%%%%%%%%%%%%

/*!--------------------------------------------------------------------*
 |	do_logout/6
 |	do_logout(SR,SW,State,SInfo,Date,Time)
 |	do_logout(+,+,+,+,-,-)
 |
 |	- logout a user, recording info, and breaking socket connection
 *!--------------------------------------------------------------------*/

do_logout(SR,SW,State,SInfo, ID, Date, Time)
	:-
	access_login_connection_info(logged_in, State, ID),
	access_login_connection_info(port, State, Port),
	access_login_connection_info(socket_id, State, SID),
	access_login_connection_info(magic_cookie, State, Cookie),

	(rem_from_current_logins(ID,Cookie,Port,SID,_,SInfo),!;true),

	access_login_connection_info(job_connects, State, BatchJobsList),
	access_server_info(users_area, SInfo, UserDir),
	record_batch_jobs(BatchJobsList, ID, Date,Time, State, UserDir),

	(var(Date) -> date(Date) ; true),
	(var(Time) -> time(Time) ; true),
/*
	access_login_connection_info(user_log_stream, State, UsrLogFileS),
	sio:remove_aliases(UsrLogFileS),
	printf(UsrLogFileS,'logout(%t,%t).\n',[Date,Time]),
	flush_output(UsrLogFileS),
	close(UsrLogFileS),
*/

	set_login_connection_info(logout_date, State, Date),
	set_login_connection_info(logout_time, State, Time),
	log_server_notify('%t %t %t %t\n', [ID,Date,Time,Cookie], 'Logout: ', SInfo),
	accounting(logout, State, SInfo).

/*!--------------------------------------------------------------------*
 |	record_batch_jobs/6
 |	record_batch_jobs(BatchJobs, ID, Date,Time, State, UserDir)
 |	record_batch_jobs(+, +, +,+, +, +)
 |
 |	- record current batch jobs on logout
 |
 |	For applications allowing/supporting detatched batch job
 |	processing for logged-in users, records information about
 |	current unfinished batch jobs when the user logs out from
 |	the main login.  Uses a file in the users working directory.
 *!--------------------------------------------------------------------*/
record_batch_jobs([], ID, Date,Time, State, UserDir) :-!.

record_batch_jobs(BatchJobs, ID, Date,Time, State, UserDir)
	:-
	extendPath(UserDir, ID, ThisUserDir),
	batch_record_file_name(BatchFile),
	pathPlusFile(ThisUserDir, BatchFile, BatchRecordFile),
	get_cwd(CurFile),
	change_cwd(BatchRecordFile),
	(exists_file(BatchRecordFile) ->
		open(BatchRecordFile, append, BRFS, [])
		;
		open(BatchRecordFile, write, BRFS, [])
	),
	write_clause(BRFS, batch_running(Date,Time,BatchJobs)),
	close(BRFS).

batch_record_file_name('batch.run').

		%%%%%%%%%%%%%%%%%%%%%%%%%%
		%% User Management
		%%%%%%%%%%%%%%%%%%%%%%%%%%

/*!--------------------------------------------------------------------*
 |	add_new_user/5.
 |	add_new_user(ID, SR,SW,State,SInfo)
 |	add_new_user(+, +,+,+,+)
 |
 |	- administrative: adds a user to the user_file
 *!--------------------------------------------------------------------*/

add_new_user(ID, SR,SW,State,SInfo)
	:-
	access_server_info(user_file, SInfo, UserListFile),
	open(UserListFile, read, UFIS, []),
	read_lines(UFIS, UserFileLines),
	close(UFIS),
	(check_for_entry(UserFileLines,ID,SR,SW, _, _) ->
		printf(SW,'User %t already entered.\n',[ID]),
		flush_output(SW)
		;
		cont_add_new_user(ID,UserListFile,SR,SW,State,SInfo)
	).

cont_add_new_user(ID,UserListFile,SR,SW,State,SInfo)
	:-
	printf(SW, 'password=',[]),
	flush_output(SW),
	get_line(SR, PWLine),
	printf(SW, 'retype password=',[]),
	flush_output(SW),
	get_line(SR, PWLine2),
	(PWLine \= PWLine2 ->
		printf(SW,'Sorry, passwords differ...can\'t add user\n',[]),
		flush_output(SW)
		;
		add_user_dir(ID, SInfo),
		open(UserListFile,append,ULFS,[]),
		printf(ULFS,'%t:%t:\n',[ID,PWLine]),
		close(ULFS),
		printf(SW,'User %t added.\n',[ID])
	).

add_user_dir(ID,SInfo)
	:-
	access_server_info(users_area, SInfo, UserDir),
	get_cwd(CurDir),
	change_cwd(UserDir),
	make_subdir(ID),
	change_cwd(CurDir).

/*!--------------------------------------------------------------------*
 *!--------------------------------------------------------------------*/

export cur_list_of_users/2.
cur_list_of_users(Users, SInfo)
	:-
	access_server_info(user_file, SInfo, UserListFile),
	grab_lines(UserListFile, UserFileLines),
	extract_users(UserFileLines, Users).


extract_users([], []).

extract_users( [Line | UserFileLines], [ID | Users] )
	:-
	atom_codes(Line, LineCs),
	check_entry_line(LineCs, ID, _, _),
	extract_users(UserFileLines, Users).


export cur_logged_in_users/2.
cur_logged_in_users(Users, SInfo)
	:-
	access_server_info(cur_logged_in, SInfo, CurLoggedIn),
	findall(ID, member(l(ID,Port,FID,Cookie,State), CurLoggedIn),  Users).

export add_user/5.
add_user(UID,UPW,UInfo,SInfo,AddResult)
	:-
	access_server_info(user_file, SInfo, UserListFile),
	grab_lines(UserListFile, UserFileLines),
	add_user(UserFileLines,UID,UPW,UInfo,UserListFile,SInfo,AddResult).

add_user(UserFileLines,UID,UPW,UInfo,UserListFile,SInfo,entry_exists)
	:-
	check_for_entry(UserFileLines, UID, _, _, _),
	!.

add_user(UserFileLines,UID,UPW,UInfo,UserListFile,SInfo,added_entry)
	:-
	open(UserListFile,append,OS,[]),
	crypt(UPW,zu,EUPW),
	printf(OS, '%t:%t:%t\n',[UID,EUPW,UInfo]),
	close(OS),
	add_user_dir(UID,SInfo).

export delete_user/4.
delete_user(UID,SInfo,State,DelResult)
	:-
	access_server_info(cur_logged_in, SInfo, CurLoggedIn),
	findall(ID, member(l(ID,Port,FID,Cookie,State), CurLoggedIn),  CurUsers),
	delete_user(UID,CurUsers,SInfo,State,DelResult).

delete_user(UID,CurUsers,SInfo,State,cur_logged_in)
	:-
	dmember(UID, CurUsers),
	!.

delete_user(UID,_,SInfo,State,DelResult)
	:-
	access_server_info(user_file, SInfo, UserListFile),
	grab_lines(UserListFile, UserFileLines),
	del_user(UserFileLines,UID,UserListFile,SInfo,State,DelResult).

del_user(UserFileLines,UID,UserListFile,SInfo,State,user_deleted)
	:-
	remove_user_line(UserFileLines,UID,NewLines),
	access_server_info(user_file, SInfo, UserListFile),
	filePlusExt(UserListFile, 'tmppx',NewTmpFile),
	open(NewTmpFile,write,OS1,[]),
	write_lines(OS1, NewLines),
	close(OS1),
	filePlusExt(UserListFile, 'bak',UserListBackupFile),
	sprintf(atom(Cmd1),'cp %t %t ; mv %t %t', 
		[UserListFile, UserListBackupFile, NewTmpFile, UserListFile]),
	system(Cmd1),
	access_login_connection_info(user_area, State, ThisUserArea),
	sprintf(atom(Cmd2),'rm -r %t', [ThisUserArea]),
	system(Cmd2),
	!,
	date(Date),time(Time),
	log_server_notify('%t %t %t %t\n', [UID,Date,Time,nil], 'UserDeleted: ', SInfo).

del_user(UserFileLines,UID,UserListFile,SInfo,State,delete_user_error)
	:-
	date(Date),time(Time),
	log_server_notify('%t %t %t %t\n', [UID,Date,Time,nil], 'Error-UserDelete ', SInfo).

		%%%%%%%%%%%%%%%%%%%%%%%%%%
		%% Accounting
		%%%%%%%%%%%%%%%%%%%%%%%%%%

/*!--------------------------------------------------------------------*
 |	accounting/3.
 |	accounting(Cmd,State,SInfo)
 |	accounting(+,+,+)
 |
 |	- administrative: records accounting information
 *!--------------------------------------------------------------------*/

accounting(login, State, SInfo)
	:-
	access_login_connection_info(logged_in, State, UserID),
	access_login_connection_info(ip_num, State, IPNum),
	access_login_connection_info(port, State, Port),
	access_login_connection_info(socket_id, State, SID),
	access_login_connection_info(login_time, State, LoginTime),
	access_login_connection_info(login_date, State, LoginDate),

	access_server_info(accounting_file, SInfo, AccountingFile),
	open(AccountingFile, append, AOS, []),
	write_clause(AOS,login(UserID,IPNum,Port,SID,LoginTime,LoginDate)),
	close(AOS).

accounting(logout, State, SInfo)
	:-
	access_login_connection_info(logged_in, State, UserID),
	access_login_connection_info(ip_num, State, IPNum),
	access_login_connection_info(port, State, Port),
	access_login_connection_info(socket_id, State, SID),
	access_login_connection_info(login_time, State, LoginTime),
	access_login_connection_info(login_date, State, LoginDate),
	access_login_connection_info(logout_time, State, LogoutTime),
	access_login_connection_info(logout_date, State, LogoutDate),
	access_login_connection_info(trans_ctr, State, TransNum),
	access_login_connection_info(succ_trans_ctr, State, SuccTransNum),

	access_server_info(accounting_file, SInfo, AccountingFile),
	open(AccountingFile, append, AOS, []),
	write_clause(AOS,logout(UserID,IPNum,Port,SID,LoginTime,LoginDate,
					 LogoutTime,LogoutDate,TransNum, SuccTransNum)),
	close(AOS).

accounting(_, State, SInfo).

		%%%%%%%%%%%%%%%%%%%%%%%%%%
		%% User Level Responses
		%%%%%%%%%%%%%%%%%%%%%%%%%%

/*!--------------------------------------------------------------------*
 |	server_respond/4
 |	server_respond(Code, Args, OutS, State)
 |	server_respond(+, +, +, +)
 |
 |	- provides server-level responses to connected socket
 *!--------------------------------------------------------------------*/

server_respond(Code, Args, OutS, State)
	:-
	access_login_connection_info(term_protocol,State,Protocol),
	cont_server_respond(Protocol,Code, Args, OutS, State).

cont_server_respond(_, Code, Args, OutS, State)
	:-!,
	write_term(OutS, server_respond(Code,Args), [quoted(true)]), 
	put_code(OutS, 0'.), nl(OutS),
	flush_output(OutS).

endmod.


