/*==================================================================*
 |			cmnclint.pro
 |		Copyright (c) 1996 Applied Logic Systems, Inc.
 |
 |		Common code for clients such as the SPARC client
 |		-- both application clients & administration clients
 |
 |	Should be consulted from a file with the following structure:
 |
 |-----------------------------------------------------------------------
 |	module socket_client.
 |	use <detail_mod>
 |
 |	init_file('sp_clint.ini').
 |	default_port(6676).
 |	
 |	job_db(t1(X), pj('alspro -q -b jobserve -giac -g jobserve ', (X is 3+4))).
 |	job_db(t2(X), pj('alspro -q -b jobserve -giac -g jobserve ', (X is 45*5))).
 |		....etc.....
 |	
 |	endmod.
 |
 |	:- reconsult(cmnclint).
 |
 |	module <detail_mod>.
 |
 |	admin_process(Goal,VarNames,Vars,RemRS,RemWS)
 |		:-
 |		....<etc>
 |
 |	and/or:
 |
 |	appl_process(Goal,VarNames,Vars,RemRS,RemWS)
 |		:-
 |		....<etc>
 |
 |	endmod.	%% <detail_mod>
 |-----------------------------------------------------------------------
 |
 |	<detail_mod> could be taken to be 'socket_client', in which case
 |	the use statement should be omitted.
 *==================================================================*/

module socket_client.

	%%%%%%%%%%%%%%%%%%%%%%
	%% STARTUP CODE
	%%%%%%%%%%%%%%%%%%%%%%

export start/0.
export start_client/2.

start 
	:-
	init_file(InitFile),
	grab_terms(InitFile, Terms),
	!,
	fin_start(Terms).

start 
	:-
	init_file(InitFile),
	info('Error: Can\'t find configuration file >> %t <<\n',[InitFile]),
	halt.

fin_start(Terms)
	:-
	dmember(host=Host, Terms),
	!,
	(dmember(port=Port, Terms) -> true ; default_port(Port) ),
	start_client(Host,Port).

fin_start(Terms)
	:-
	init_file(InitFile),
	info('Error: No host in configuration file >> %t <<\n',[InitFile]),
	halt.

:-dynamic(client_shell_prompts/2).

start_client(Host,Port) 
	:-
	catch(open(socket(inet_stream,Host,Port),read,RemRS,[alias(rsock)]),
	      _,
	      fail),
	(is_server_socket(rsock) -> 
	    close(rsock),
	    fail
		;
	    open(socket(clone,rsock),write,RemWS,[alias(wsock)])
	),
	(client_shell_prompts(One, Two) ; (One,Two) = (':-', ':-_') ),
	builtins:get_shell_prompts( PrevPrompts),
	builtins:set_shell_prompts( [(One, Two) | PrevPrompts] ),
	!,
	catch( login_loop(RemRS,RemWS),
		   Ball,
		   login_loop_exception_handler(Ball, RemRS, RemWS) 
		  ).

login_loop_exception_handler(login_loop, RemRS, RemWS) 
	:-!,
	catch( login_loop(RemRS,RemWS),
		   Ball,
		   login_loop_exception_handler(Ball, RemRS, RemWS) 
		  ).

login_loop_exception_handler(shutdown, RemRS, RemWS) 
	:-!,
	login_loop_exception_handler(end_of_file, RemRS, RemWS).

login_loop_exception_handler(end_of_file, RemRS, RemWS) 
	:-!,
	info('Encountered end of stream from server...Exiting...Bye.\n',[]),
	(stream_open_status(RemRS, open) -> close(RemRS) ; true),
	(stream_open_status(RemWS, open) -> close(RemWS) ; true),
	halt.

login_loop_exception_handler(_, RemRS, RemWS) 
	:-
	info('!!! Error: Unknown error occurred...Exiting...Bye.\n',[]),
	(stream_open_status(RemRS, open) -> close(RemRS) ; true),
	(stream_open_status(RemWS, open) -> close(RemWS) ; true),
	halt.

start_client(Host,Port) 
	:-
	info('Can\'t find server on %t at port %t\n',[Host,Port]).

info(M,PL) 
	:- 
	printf(user_output,M,PL).

	%%%%%%%%%%%%%%%%%%%%%%
	%% LOGIN LOOP
	%%%%%%%%%%%%%%%%%%%%%%

:-dynamic(login_banner/1).

login_loop(RemRS,RemWS)
	:-
	(login_banner(LoginBanner) ; LoginBanner = 'Login'),
	printf(user_output, '%t:\n', [LoginBanner]),
	flush_input(RemRS),
	builtins:shell_read(user_input,user_output,InGoal,VarNames,Vars),
	disp_login_loop(InGoal,VarNames,Vars,RemRS,RemWS).

disp_login_loop(end_of_file,_,_,RemRS,RemWS)
	:-!,
	throw(end_of_file).

disp_login_loop(shutdown,_,_,RemRS,RemWS)
	:-!,
	throw(end_of_file).

	%%%%%%%%%%%%%%%%%%%%%%
	%% LOGIN / LOGOUT
	%%%%%%%%%%%%%%%%%%%%%%


	%% login(ID,PW):
disp_login_loop(login(ID,PW),_,_,RemRS,RemWS)
	:-!,
	flush_input(RemRS),
	printf(RemWS, 'login(%t,%t).\n',[ID,PW]), flush_output(RemWS),
		%% blocking get_line:
	get_line(RemRS, Return),
	login_finish(Return,RemRS,RemWS),
	catch( client_loop(RemRS,RemWS),
		   Ball,
		   client_loop_exception_handler(Ball, RemRS, RemWS, ID)
		 ).

	%% login(ID):
disp_login_loop(login(ID),_,_,RemRS,RemWS)
	:-!,
	flush_input(RemRS),
	printf(RemWS, 'login(%t).\n',[ID,PW]), flush_output(RemWS),
		%% blocking get_line:
	get_line(RemRS, Return),
	login_finish(Return,RemRS,RemWS),
	catch( client_loop(RemRS,RemWS),
		   Ball,
		   client_loop_exception_handler(Ball, RemRS, RemWS, ID)
		 ).

	%% login:
disp_login_loop(login,_,_,RemRS,RemWS)
	:-!,
	flush_input(user_input),
	printf(user_output, 'Name = ', []), 
	flush_output(user_output),
	get_line(user_input, InitID),
	cleanup_atom(InitID, ID),
	disp_login_loop(login(ID),_,_,RemRS,RemWS).

	%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% DEFAULT FOR LOGIN LOOP
	%%%%%%%%%%%%%%%%%%%%%%%%%%

disp_login_loop(_,_,_,RemRS,RemWS)
	:-
	throw(login_loop).

	%%%%%%%%%%%%%%%%%%%%%%
	%% MAIN CLIENT LOOP
	%%%%%%%%%%%%%%%%%%%%%%

client_loop(RemRS,RemWS)
	:-
	builtins:shell_read(user_input,user_output,InGoal,VarNames,Vars),
	disp_client_loop(InGoal,VarNames,Vars,RemRS,RemWS).

client_loop_exception_handler(shutdown, RemRS, RemWS, ID)
	:-!,
	throw(shutdown).

client_loop_exception_handler(logout, RemRS, RemWS, ID)
	:-!,
	throw(login_loop).

client_loop_exception_handler(Ball, RemRS, RemWS, ID)
	:-
	info('!!! Error: Unknown error occurred...Logging out...Bye.\n',[]),
	throw(login_loop).

disp_client_loop(end_of_file,VarNames,Vars,RemRS,RemWS)
	:-!,
	disp_client_loop(logout,VarNames,Vars,RemRS,RemWS).

	%%%%%%%%%%%%%%%%%%%%%%
	%% LOGOUT
	%%%%%%%%%%%%%%%%%%%%%%

disp_client_loop(logout,VarNames,Vars,RemRS,RemWS)
	:-!,
	printf(RemWS, 'logout.\n',[]), 
	flush_output(RemWS),
	read(RemRS, Return),
	Return = server_respond(Code,Args),
	server_response(Code, Args),
	throw(logout).

	%%%%%%%%%%%%%%%%%%%%%%
	%% REMOTE JOBS
	%%%%%%%%%%%%%%%%%%%%%%

/*-----------------------------------------------------------------*
 |	job_db(<JobIDTerm>, pj(<OS Process CMD Line>, ServerCmd))
 *-----------------------------------------------------------------*/

	%% sjob(Goal) - submit job [temporary testing construct]:
disp_client_loop(sjob(InitGoal),VarNames,Vars,RemRS,RemWS)
	:-!,
	(job_db(InitGoal, Goal) -> true; Goal = InitGoal),
	printf(RemWS, '%t.\n',[job_control(start,Goal,Vars)],[quoted(true)]), 
	flush_output(RemWS),
	read(RemRS, Return),
	fin_rem_exec(Return,  Vars,  VarNames),
	client_loop(RemRS,RemWS).


	%%%%%%%%%%%%%%%%%%%%%%
	%% ADMINISTRATION
	%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(admin_process/5).

disp_client_loop(Goal,VarNames,Vars,RemRS,RemWS)
	:-
	admin_process(Goal,VarNames,Vars,RemRS,RemWS),
	!,
	client_loop(RemRS,RemWS).


	%%%%%%%%%%%%%%%%%%%%%%
	%% APPLICATION
	%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(appl_process/5).

disp_client_loop(Goal,VarNames,Vars,RemRS,RemWS)
	:-
	appl_process(Goal,VarNames,Vars,RemRS,RemWS),
	!,
	client_loop(RemRS,RemWS).

	%%%%%%%%%%%%%%%%%%%%%%
	%% UNKNOWN REQUEST
	%%%%%%%%%%%%%%%%%%%%%%

	%% Unknown
disp_client_loop(Request,_,_,RemRS,RemWS)
	:-
	printf(user_output, 'Unknown client request: %t\n',[Request]),
	!,
	client_loop(RemRS,RemWS).

	%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% LOGIN / LOGOUT SUPPORT
	%%%%%%%%%%%%%%%%%%%%%%%%%%

login_finish(Return,RemRS,RemWS)
	:-
	atomread(Return, LoginReturn),
	!,
	login_finish_pro(LoginReturn,RemRS,RemWS).

login_finish(Return,RemRS,RemWS)
	:-
	login_finish_tty(LoginReturn,RemRS,RemWS).

login_finish_pro(logged_in(Banner,MOTD),RemRS,RemWS)
	:-!,
	printf_lines(Banner, user_output, [quoted(true)]),
	printf_lines(MOTD, user_output, [quoted(true)]),
	flush_output(user_output).

login_finish_pro(server_respond(not_allowed,Args),RemRS,RemWS)
	:-!,
	server_response(not_allowed, Args),
	throw(login_loop).

login_finish_pro(pw-LoginCookie,RemRS,RemWS)
	:-!,
	flush_input(user_input),
	printf(user_output, 'Password: ', []),
	flush_output(user_output),
	get_line(user_input, InitPW),
	cleanup_atom(InitPW, PW),
	printf(RemWS, 'password(%t,%t).\n', [LoginCookie,PW],[quoted(true)]),
	flush_output(RemWS),
	get_line(RemRS, NextReturn),
	login_finish(NextReturn,RemRS,RemWS).

login_finish_pro(server_respond(Code,Args),RemRS,RemWS)
	:-!,
	server_response(Code, Args).

login_finish_tty(Return,RemRS,RemWS)
	:-
	printf(user_output,'Unknown login response: %t\n',[Return]),
	flush_output(user_output).

printf_lines([], OutStream, Options).
printf_lines([Line | Lines], OutStream, Options)
	:-
	printf(OutStream, Line, Options),
	printf_lines(Lines, OutStream, Options).


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% RESPONSE CODES FROM SERVER
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
server_response(Code, Args)
	:-
	server_respond_code(Code, Pattern),
	!,
	printf(user_output, Pattern, Args),
	flush_output(user_output).


	%respond: '%t already logged in on port %t, sid=%t\n', [ID,Port,SID]
server_respond_code(already_logged_in, '%t already logged in on port %t, sid=%t\n').

	%respond: 'Sorry. Login not allowed for %t.\n', [ID]
server_respond_code(not_allowed, 'Sorry. Login not allowed for %t.\n').

	%respond: 'Sorry. Login attempt failed.\n', []
server_respond_code(bad_login, 'Sorry. Login attempt failed.\n').

	%respond: 'password=', []
server_respond_code(get_password, 'password=').

	%respond: 'logout', [ID,Date,Time]
server_respond_code(logout, 'User %t logged out at %t on %t\n\n').

	%respond: 'Missing work area for %t!\nContact system administrator!\n',[ID]
server_respond_code(missing_user_area, 
			'Missing work area for %t!\nContact system administrator!\n').

	%respond: 'Unknown server request: %t\n', [Expr]
server_respond_code(unknown_request, 'Unknown server request: %t\n').

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% REMOTE EXECUTION SUPPORT
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			 
fin_rem_exec(yes(Vars),  Vars,  VarNames)
	:-!,
	builtins:showanswers(VarNames,Vars,user_input,user_output).
					  
fin_rem_exec(no,  _,  _)
	:-!,
	builtins:print_no(user_output).
							   
fin_rem_exec(Result,  _,  _)
	:-
	printf(user_output, 'remx: %t\n',[Result]),
	flush_output(user_output).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% UTILITIES
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% Strips off any leading white, and 
	%% Strips off any trailing white, and any final period:
cleanup_atom(AtomIn, AtomOut)
	:-
	atom_codes(AtomIn, AtomInCs),
	strip_white(AtomInCs, AtomInCs0),
	dreverse(AtomInCs0, RevInCs),
	strip_white(RevInCs, RevInCs1),
	(RevInCs1 = [0'. | RevInCs2] -> 
		true 
		; 
		RevInCs2 = RevInCs1
	),
	dreverse(RevInCs2, AtomOutCs),
	atom_codes(AtomOut, AtomOutCs).

endmod.
