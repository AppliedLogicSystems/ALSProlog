

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Client code
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% test:
th :- start_client(hilbert, 5788).

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
	builtins:get_shell_prompts( Prev),
	builtins:set_shell_prompts( [(':?-', ':?-_') | Prev] ),
	client_loop(RemRS,RemWS).

start_client(Host,Port) 
	:-
	info('Can\'t find server on %t at port %t\n',[Host,Port]).

client_loop(RemRS,RemWS)
	:-
	builtins:shell_read(user_input,user_output,InGoal,VarNames,Vars),
	disp_client_loop(InGoal,VarNames,Vars,RemRS,RemWS).

	%% login(ID,PW):
disp_client_loop(login(ID,PW),_,_,RemRS,RemWS)
	:-!,
	printf(RemWS, 'login(%t,%t).\n',[ID,PW]), flush_output(RemWS),
		%% blocking read:
	read(RemRS, Return),
	login_finish(Return,RemRS,RemWS),
	client_loop(RemRS,RemWS).

	%% login(ID):
disp_client_loop(login(ID),_,_,RemRS,RemWS)
	:-!,
	printf(RemWS, 'login(%t).\n',[ID,PW]), flush_output(RemWS),
		%% blocking get_line:
	get_line(RemRS, Return),
	login_finish(Return,RemRS,RemWS),
	client_loop(RemRS,RemWS).

	%% logout:
disp_client_loop(logout,VarNames,Vars,RemRS,RemWS)
	:-!,
	printf(RemWS, 'logout.\n',[]), 
	flush_output(RemWS),
	close(RemRS),
	close(RemWS),
	printf('Logged off.\n',[]).


%job_db(<JobIDTerm>, pj(<OS Process CMD Line>, ServerCmd))
%job_db(t1(X), pj('alspro -q -b jobserve -giac -g jobserve ', run_job(X))).
job_db(t1(X), pj('alspro -q -b jobserve -giac -g jobserve ', (X is 3+4))).
job_db(t2(X), pj('alspro -q -b jobserve -giac -g jobserve ', (X is 45*5))).

	%% sjob(Goal) - submit job [temporary testing construct]:
disp_client_loop(sjob(InitGoal),VarNames,Vars,RemRS,RemWS)
	:-!,
	(job_db(InitGoal, Goal) -> true; Goal = InitGoal),
	printf(RemWS, '%t.\n',[job_control(start,Goal,Vars)],[quoted(true)]), 
	flush_output(RemWS),
	read(RemRS, Return),
	fin_rem_exec(Return,  Vars,  VarNames),
	client_loop(RemRS,RemWS).

	%% ssv(Slot) - return server SInfo slot value:
disp_client_loop(ssv(Slot),VarNames,Vars,RemRS,RemWS)
	:-!,
	printf(RemWS, '%t.\n',[rst(Slot)]), flush_output(RemWS),
	read(RemRS, Return),
	fin_rem_exec(Return,  [Val],  ['Value']),
	client_loop(RemRS,RemWS).

	%% Goal: - remote execution request;
	%% 
disp_client_loop(InGoal,VarNames,Vars,RemRS,RemWS)
	:-
	flush_input(RemRS),
	printf(RemWS, '%t.\n',[rex(InGoal,Vars)]), flush_output(RemWS),
	read(RemRS, Return),
	fin_rem_exec(Return,  Vars,  VarNames),
	client_loop(RemRS,RemWS).


login_finish(logged_in(Banner,MOTD),RemRS,RemWS)
	:-!,
	printf_lines(Banner, user_output, [quoted(true)]),
	printf_lines(MOTD, user_output, [quoted(true)]),
	flush_output(user_output).


login_finish(Return,RemRS,RemWS)
	:-!,
	atomread(Return, pw-LoginCookie),
	!,
	flush_input(user_input),
	printf(user_output, 'Password: ', []),
	flush_output(user_output),
	get_line(user_input, InitPW),
	atom_codes(InitPW, InitPWCs),
	dreverse(InitPWCs, RIPWCs),
	strip_white(RIPWCs, RIPWCs1),
	(RIPWCs1 = [0'. | RIPWCs2] -> true ; RIPWCs2 = RIPWCs1),
	dreverse(RIPWCs2, PWCs),
	atom_codes(PW, PWCs),
	printf(RemWS, 'password(%t,%t).\n', [LoginCookie,PW],[quoted(true)]),
	flush_output(RemWS),
	read(RemRS, NextReturn),
	login_finish(NextReturn,RemRS,RemWS).

login_finish(server_respond(Code,Args),RemRS,RemWS)
	:-!,
	server_respond_code(Code, Pattern),
	!,
	printf(user_output, Pattern, Args),
	flush_output(user_output).

login_finish(Return,RemRS,RemWS)
	:-
	printf(user_output,'Unknown login response: %t\n',[Return]),
	flush_output(user_output).




	%repond: '%t already logged in on port %t, sid=%t\n', [ID,Port,SID]
server_respond_code(already_logged_in, '%t already logged in on port %t, sid=%t\n').

	%repond: 'Sorry. Login not allowed for %t.\n', [ID]
server_respond_code(not_allowed, 'Sorry. Login not allowed for %t.\n').

	%respond: 'password=', []
server_respond_code(get_password, 'password=').

	%respond: 'Missing work area for %t!\nContact system administrator!\n',[ID]
server_respond_code(missing_user_area, 
			'Missing work area for %t!\nContact system administrator!\n').

	%respond: 'Unknown request: %t\n', [Expr]
server_respond_code(unknown_request, 'Unknown request: %t\n').



printf_lines([], OutStream, Options).
printf_lines([Line | Lines], OutStream, Options)
	:-
	printf(OutStream, Line, Options),
	printf_lines(Lines, OutStream, Options).








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



















sr(Exp)
	:-
	send_request(Exp).

send_request(Exp) 
	:-
	printf(wsock,'%t.\n',[Exp]),
	flush_output(wsock),
	read(rsock,Ans),
			%% can cause hang (e.g. mac <-> unix); fix uniform io:
	skip(rsock,0'\n),
	info('Answer is %t\n',[Ans]).

stop_client 
	:-
	close(rsock),
	close(wsock).

stop_server 
	:-
	printf(wsock,'stop_yourself.\n',[]),
	flush_output(wsock).

info(M,PL) 
	:- 
	printf(user_output,M,PL).


