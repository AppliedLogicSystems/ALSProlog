/*======================================================================*
 | 			csp_int.pro 
 |		Copyright (c) 1996 Applied Logic Systems, Inc.
 |      Distribution rights per Copying ALS
 |
 |		Simple interpreter for a communicating sequential processes 
 |		sub-language for the client-server tools
 |
 | Author: Ken Bowen
 | Started: September, 1996
 *======================================================================*/

module socket_comms.
export run_script/3.
export run_script/4.

%%%%%%%%%%%%%%%%%%% Experimental Script Code %%%%%%%%%%%%%%%%%%%%%%

/*----------------------------------------------------------------------*
 |	csp scripts govern the interations between two agents,
 |	from the point of view of one of the agents ("me"; the 2nd agent
 |	is "other");  the communication between the two agents is via
 |	a pair of channels (sockets) which are labelled from the
 |	point of view of "me" as SR (the "me" read channel) and SW
 |	(the "me" write channel).
 |
 |	csp scripts are lists of terms of the following forms 
 |	and interpretations:
 |
 |	receive(Item)			- "me" receives (reads) an item from SR;
 |	get_lines_to_end(List)	- "me" reads a list of lines from SR until
 |							  a line is received consisting only of
 |							  the atom '$end$of$list$'
 |	read_terms_to_end(List)	- "me" reads a list of terms from SR until
 |							  the atom '$end$of$list$' received 
 |
 |	Note that most output from "me" on a socket is followed by a system-
 |	supplied newline (nl) followed by a flush_output; for brevity below,
 |	write "flush_output(SW)" as "fo(SW)":
 |
 |	respond(Pattern)		- "me" sends (writes with printf) Pattern on SW;
 |							  i.e., "me" executes 
 |								printf(SW, Pattern, []),nl(SW),fo(SW)
 |	respond(Pattern,Args)	- printf(SW,Pattern,Args),nl(SW),fo(SW)
 |	respond_prolog(Pattern)	- 
 |					printf(SW, Pattern, [], [quoted(true), line_length(500000)]),
 |					nl(SW), flush_output(SW)
 |	respond_prolog(Pattern, Args)	- 
 |					printf(SW, Pattern, Args, [quoted(true), line_length(500000)]),
 |					nl(SW), flush_output(SW)
 |	Note NO nl on the following:
 |	respond_xnl(Pattern,Args)	- 
 |					printf(SW,Pattern,Args,[line_length(500000)]),
 |					flush_output(SW)
 |	send_file_lines(FileName)	- effectively applies the following to
 |								  each line of file FileName:
 |							put_atom(SW, Line), nl(SW), flush_output(SW),
 |
 |	guarded(Cmds)	-	Cmds is a list of expressions of the form
 |							(Guard -> Cmd)
 |						Scans them in order ("left to right"), executing
 |							call(Guard)
 |						until this call succeeds, in which case it 
 |						executes
 |							call(Cmd).
 |						Succeeds or fails according as call(Cmd) succeeds
 |						or fails; fails if no call(Guard) succeeds.
 |						Note that no Guard should contain any I/O calls
 |						on SR,SW.
 |
 |	case(Var,Cases) -	Cmds is a list of expressions of the form
 |							(Value -> Cmd)
 |						Scans them in order ("left to right"), executing
 |							call(Value = Var)
 |						until this call succeeds, in which case it 
 |						executes
 |							call(Cmd).
 |						Succeeds or fails according as call(Cmd) succeeds
 |						or fails; fails if no call(Value = Var) succeeds.
 |
 |	ArbitraryExp	-	executes	call(ArbitraryExp)
 |						Note that ArbitraryExp should not contain I/O calls
 |						on SR,SW.
 *----------------------------------------------------------------------*/

	 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	 %%	Recurse down the interaction script list:
	 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*--------------------------------------------------------------*
 |	interaction/5
 |	interaction(Script,Mod,TaskEnv,Result,SInfo)
 |	interaction(+,+,+,-,+)
 *--------------------------------------------------------------*/

interaction([], _, _, [],_).

interaction([ScriptItem | Script], Mod, TaskEnv, FinalResult,SInfo)
	:-
	interact(ScriptItem, Mod, TaskEnv, Result,SInfo),
	!,
	disp_interaction(Result, ScriptItem, Script, Mod, TaskEnv, FinalResult,SInfo).

disp_interaction(delay_on(Var,Call), ScriptItem, Script, Mod, 
					TaskEnv, [ delay_on(Var,Call) | Script ] ,SInfo)
	:-!.

disp_interaction(full_delay_on(Var,Call), ScriptItem, Script, Mod, 
					TaskEnv, [ full_delay_on(Var,Call) | Script ] ,SInfo)
	:-!.

disp_interaction(_, ScriptItem, Script, Mod, TaskEnv, FinalResult,SInfo)
	:-
	interaction(Script, Mod, TaskEnv, FinalResult,SInfo).
		
	 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	 %%	Execute an individual script item:
	 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*--------------------------------------------------------------*
 |	interact/5
 |	interact(ScriptItem, Mod, TaskEnv, Result,SInfo)
 |	interact(+, +, +, -, +)
 *--------------------------------------------------------------*/

	/*--------------------------------------------------------------*
	 |	read a prolog term from the incoming (SR) stream:
	 *--------------------------------------------------------------*/
interact(read(Var), Mod, TaskEnv, Result, SInfo)
	:-!,
	access_tsk_env(read_s, TaskEnv, SR),
	read_term(SR, InitVar, [blocking(false)]),
	(InitVar == stream_not_ready ->
		Result = delay_on(Var, true)
		;
		Var = InitVar,
		Result = true).

	/*--------------------------------------------------------------*
	 |	Explict delay and full_delay:
	 *--------------------------------------------------------------*/
interact(delay(Var)^Goal, Mod, TaskEnv, Result, SInfo)
	:-
	var(Var),
	!,
	Result = delay_on(Var, Goal).

interact(delay(Var)^Goal, Mod, TaskEnv, Result, SInfo)
	:-!,
	interact(Goal, Mod, TaskEnv, Result, SInfo).

interact(full_delay(Var)^Goal, Mod, TaskEnv, Result, SInfo)
	:-
	var(Var),
	!,
	Result = full_delay_on(Var, Goal).

interact(full_delay(Var)^Goal, Mod, TaskEnv, Result, SInfo)
	:-!,
	interact(Goal, Mod, TaskEnv, Result, SInfo).

	/*--------------------------------------------------------------*
	 |	Tasking
	 *--------------------------------------------------------------*/
interact(local_task(Goal), Mod, TaskEnv, Result, SInfo)
	:-!,
	Goal =.. [Task | TaskArgs],
	interact(Task/no_worker,TaskArgs, Mod, TaskEnv, Result, SInfo).

interact(remote_task(Goal), Mod, TaskEnv, Result, SInfo)
	:-!,
	Goal =.. [Task | TaskArgs],
	interact(Task/worker_job,TaskArgs, Mod, TaskEnv, Result, SInfo).

interact(task(Goal), Mod, TaskEnv, Result, SInfo)
	:-!,
	Goal =.. [Task | TaskArgs],
	interact(task(Task,TaskArgs), Mod, TaskEnv, Result, SInfo).

interact(task(Task/ExplWkr,TaskArgs), Mod, TaskEnv, true, SInfo)
	:-!,
		%% need check that ExplWkr is ok....
	handle_task(ExplWkr,Task,TaskArgs,Mod,TaskEnv,SInfo).

interact(task(Task,TaskArgs), Mod, TaskEnv, true, SInfo)
	:-!,
	access_tsk_env(wkr_use, TaskEnv, WkrUse),
	handle_task(WkrUse,Task,TaskArgs,Mod,TaskEnv,SInfo).

	/*--------------------------------------------------------------*
	 |	input lines from the incoming (SR) stream, to end_of_file:
	 *--------------------------------------------------------------*/
interact(get_lines_to_end(Var), Mod, TaskEnv, Result, SInfo)
	:-!,
	access_tsk_env(read_s, TaskEnv, SR),
	get_lines_from_stream(SR, Var, Result).

	%% Next will always be instantiated:
interact(test_get_lines_to_end(Next, Lines), Mod, TaskEnv, Result, SInfo)
	:-!,
	((Next = '$end$of$list$' ; Next = end_of_file) -> 
		Line = [],
		Result = true
		;
		Line = [Next | RestLine],
		access_tsk_env(read_s, TaskEnv, SR),
		get_lines_to_end(SR, RestLine, Result)
	).

	/*--------------------------------------------------------------*
	 |	read prolog terms from the incoming (SR) stream, to end_of_file:
	 *--------------------------------------------------------------*/
interact(read_terms_to_end(Var), Mod, TaskEnv, Result, SInfo)
	:-!,
	access_tsk_env(read_s, TaskEnv, SR),
	read_terms_from_stream(SR, Var, Result).

	%% Next will always be instantiated:
interact(test_read_terms_to_end(Next, Terms), Mod, TaskEnv, Result, SInfo)
	:-!,
	((Next = '$end$of$list$' ; Next = end_of_file) -> 
		Terms = [],
		Result = true
		;
		Terms = [Next | RestTerms],
		access_tsk_env(read_s, TaskEnv, SR),
		read_terms_from_stream(SR, RestTerms, Result)
	).

	/*--------------------------------------------------------------*
	 |	output to the outgoing (SW) stream:
	 *--------------------------------------------------------------*/
interact(respond(Pattern), Mod, TaskEnv, true, SInfo)
	:-
	access_tsk_env(write_s, TaskEnv, SW),
	stream_open_status(SW, open),
	!,
	(stream_open_status(SW, open) -> 
		printf(SW, Pattern, [], [line_length(500000)]),
		nl(SW)
		; 
		(stream_open_status(SW, open) -> 
			nl(SW)
			;
			(poll(SW, 0) -> 
				catch (flush_output(SW) , _, true)
				; 
				true )
		)
	),
	
nl(user_output),
printf(user_output, Pattern, [], [line_length(500000)]),
nl(user_output), flush_output(user_output).

interact(respond(Pattern), Mod, TaskEnv, true, SInfo)
	:-!.

interact(respond(Pattern, Args), Mod, TaskEnv, true, SInfo)
	:-
	access_tsk_env(write_s, TaskEnv, SW),
	stream_open_status(SW, open),
	!,
	printf(SW, Pattern, Args, [line_length(500000)]),
	nl(SW), flush_output(SW),
nl(user_output),
printf(user_output, Pattern, Args, [line_length(500000)]),
nl(user_output), flush_output(user_output).

interact(respond(Pattern, Args), Mod, TaskEnv, true, SInfo)
	:-!.

interact(respond(Pattern, Args, Options), Mod, TaskEnv, true, SInfo)
	:-
	access_tsk_env(write_s, TaskEnv, SW),
	stream_open_status(SW, open),
	!,
	printf(SW, Pattern, Args, [line_length(500000) | Options]),
	nl(SW), flush_output(SW),
nl(user_output),
printf(user_output, Pattern, Args, [line_length(500000) | Options]),
nl(user_output), flush_output(user_output).

interact(respond(Pattern, Args, Options), Mod, TaskEnv, true, SInfo)
	:-!.

interact(respond_xnl(Pattern, Args), Mod, TaskEnv, true, SInfo)
	:-
	access_tsk_env(write_s, TaskEnv, SW),
	stream_open_status(SW, open),
	!,
	printf(SW, Pattern, Args, [line_length(500000)]),
	flush_output(SW).

interact(respond_xnl(Pattern, Args), Mod, TaskEnv, true, SInfo)
	:-!.

interact(respond_prolog(Pattern), Mod, TaskEnv, true, SInfo)
	:-
	access_tsk_env(write_s, TaskEnv, SW),
	stream_open_status(SW, open),
	!,
	printf(SW, Pattern, [], [quoted(true), line_length(500000)]),
	nl(SW), flush_output(SW).

interact(respond_prolog(Pattern), Mod, TaskEnv, true, SInfo)
	:-!.

interact(respond_prolog(Pattern, Args), Mod, TaskEnv, true, SInfo)
	:-
	access_tsk_env(write_s, TaskEnv, SW),
	stream_open_status(SW, open),
	!,
	printf(SW, Pattern, Args, [quoted(true),line_length(500000)]),
	nl(SW), flush_output(SW).

interact(respond_prolog(Pattern, Args), SR, SW, Mod, TaskEnv, true, SInfo)
	:-!.

	/*--------------------------------------------------------------*
	 |	Output lines from File 
	 *--------------------------------------------------------------*/
interact(send_file_lines(File), Mod, TaskEnv, true, SInfo)
	:-!,
	access_tsk_env(write_s, TaskEnv, SW),
	stream_open_status(SW, open),
	!,
	grab_lines(File, Lines),
	send_lines(Lines, SW).

interact(send_file_lines(File), Mod, TaskEnv, true, SInfo)
	:-!.

	/*--------------------------------------------------------------*
	 |	Guarded commands
	 *--------------------------------------------------------------*/
interact(guarded(CommandList), Mod, TaskEnv, Result, SInfo)
	:-!,
	guarded_interact(CommandList, Mod, TaskEnv, Result, SInfo).

interact(true, _, _, true, SInfo)
	:-!.
interact(fail, _, _, fail, SInfo)
	:-!,
	fail.

	/*--------------------------------------------------------------*
	 |	Case command
	 *--------------------------------------------------------------*/
interact(case(Var,CommandList), Mod, TaskEnv, Result, SInfo)
	:-!,
	case_interact(CommandList, Var, Mod, TaskEnv, Result, SInfo).

	/*--------------------------------------------------------------*
	 |	Access to the server info variable:
	 |		- for administrative commands
	 *--------------------------------------------------------------*/
interact(server_info(SInfo), Mod, TaskEnv, true, SInfo)
	:-!.

	/*--------------------------------------------------------------*
	 |	All other cases - attempt to execute it:
	 *--------------------------------------------------------------*/
interact([Command], Mod, TaskEnv, Result, SInfo)
	:-!,
	interact(Command, Mod, TaskEnv, Result, SInfo).

interact([Command | Commands], Mod, TaskEnv, Result, SInfo)
	:-!,
	interact(Command, Mod, TaskEnv, _, SInfo),
	interact(Commands, Mod, TaskEnv, Result, SInfo).

interact( (Command , Commands), Mod, TaskEnv, Result, SInfo)
	:-!,
	interact(Command, Mod, TaskEnv, _, SInfo),
	interact(Commands, Mod, TaskEnv, Result, SInfo).

interact(Command, Mod, TaskEnv, true, SInfo)
	:-
	call(Mod:Command),
	!.

interact(Command, Mod, TaskEnv, true, SInfo)
	:-
	call(Command).

	/*--------------------------------------------------------------*
	 |	Guarded interact
	 *--------------------------------------------------------------*/
guarded_interact([], Mod, TaskEnv, fail, SInfo).

guarded_interact([(Guard -> GCmdList) | CommandList], Mod, TaskEnv, Result, SInfo)
	:-
	interact(Guard, Mod, TaskEnv, _, SInfo),
	!,
	interact(GCmdList, Mod, TaskEnv, Result, SInfo).

guarded_interact([_ | CommandList], Mod, TaskEnv, Result, SInfo)
	:-
	guarded_interact(CommandList, Mod, TaskEnv, Result, SInfo).

case_interact([], Var, TaskEnv, fail, SInfo).

case_interact([(Value -> Cmd) | CommandList], Var, Mod, TaskEnv, Result, SInfo)
	:-
	Value = Var,
	!,
	(Cmd = [_|_] ->
		interaction(Cmd, Mod, TaskEnv, Result, SInfo)
		;
		interact(Cmd, Mod, TaskEnv, Result, SInfo)
	).

case_interact([ (_ -> _) | CommandList], Var, Mod, TaskEnv, Result, SInfo)
	:-!,
	case_interact(CommandList, Var, Mod, TaskEnv, Result, SInfo).

		%% Note: Default is NOT of the form (_ -> _):
case_interact([ Default ], Var, Mod, TaskEnv, Result, SInfo)
	:-
	(Default = [_|_] ->
		interaction(Default, Mod, TaskEnv, Result, SInfo)
		;
		interact(Default, Mod, TaskEnv, Result)
	).


	 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	 %%	Interaction script support routines
	 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	 %% get lines of input
	 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_lines_from_stream(SrcStrm, LinesFromStream, Result)
	:-
	get_line(SrcStrm, NextLine),
	!,
		%% Necessary because source can artificially send 'end_of_file':
	disp_get_lines_from_stream(NextLine, SrcStrm, LinesFromStream, Result).

get_lines_from_stream(_, [], true).

disp_get_lines_from_stream(end_of_file, SrcStrm, [], true) :-!.

disp_get_lines_from_stream('$end$of$list$', SrcStrm, [], true) :-!.

disp_get_lines_from_stream(stream_not_ready(_), SrcStrm, 
							[NextLine |  LinesFromStream], Result)
	:-!,
	Result = delay_on(NextLine, test_get_lines_to_end(Next, LinesFromStream)).

disp_get_lines_from_stream(NextLine, SrcStrm, [NextLine |  LinesFromStream], Result)
	:-
	get_lines_from_stream(SrcStrm, LinesFromStream, Result).

	 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	 %% get terms from input
	 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_terms_from_stream(SrcStrm, TermsFromStream, Result)
	:-
	read_term(SrcStrm, InitNextLine,[blocking(false)]),
	!,
		%% Necessary because source can artificially send 'end_of_file':
	disp_read_terms_from_stream(InitNextLine, SrcStrm, TermsFromStream, Result).

read_terms_from_stream(_, [], true).

disp_read_terms_from_stream('@@done', SrcStrm, [], true) :-!.

disp_read_terms_from_stream(end_of_file, SrcStrm, [], true) :-!.

disp_read_terms_from_stream('$end$of$list$', SrcStrm, [], true) :-!.

disp_read_terms_from_stream(stream_not_ready, SrcStrm, TermsFromStream, Result)
	:-!,
	Result = delay_on(NextTerm, test_read_terms_to_end(NextTerm, TermsFromStream)).

disp_read_terms_from_stream(NextTerm, SrcStrm, [NextTerm |  TermsFromStream], Result)
	:-
	read_terms_from_stream(SrcStrm, TermsFromStream, Result).


	 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	 %% send list of lines as output
	 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_lines([], SW)
	:-
%	nl(SW),
	flush_output(SW).
send_lines([Line | Lines], SW)
	:-
	stream_open_status(SW, open),
	!,
	put_atom(SW, Line), nl(SW),
	flush_output(SW),
	send_lines(Lines, SW).
send_lines([Line | Lines], SW)
	:-!.

%%%
%%% TaskEnv = [WkrUse,JobID,UserID,SR,SW,State,SInfo]
%%%

run_script(Script,TaskEnv,SInfo)
	:-
	run_script(Script,user,TaskEnv,SInfo).

run_script(Script,Mod,TaskEnv,SInfo)
	:-
	interaction(Script,Mod,TaskEnv,Result,SInfo),
	fin_run_script(Result, Script,Mod,TaskEnv,SInfo).

fin_run_script([], Script,Mod,TaskEnv,SInfo)
	:-!.

fin_run_script(Result, Script,Mod, TaskEnv,SInfo)
	:-
	Result = [full_delay_on(Var, Call) | RestScript],
	NewScript = [Call | RestScript],
	access_tsk_env(state, TaskEnv, State),
%	mangle(2, State, expect(Var, run_script(NewScript,Mod,TaskEnv,SInfo))).
	add_to_expect(expect(Var, [], run_script(NewScript,Mod,TaskEnv,SInfo)), State).

/****
fin_run_script(Result, Script,Mod, TaskEnv,SInfo)
	:-
	Result = [full_delay_on_list(ListOfVars, Call) | RestScript],
	NewScript = [Call | RestScript],
	access_tsk_env(state, TaskEnv, State),
	mangle(2, State, list_expect(ListOfVars, run_script(NewScript,Mod,TaskEnv,SInfo))).
******/


/*****
fin_run_script(Result, Script,Mod,TaskEnv,SInfo)
	:-
	Result = [delay_on(Var, Call) | RestScript],
	!,
	NewScript = [Call | RestScript],
	access_tsk_env(state, TaskEnv, State),
	mangle(2, State, expect(Var)),
	freeze(Var, run_script(NewScript,Mod,TaskEnv,SInfo)).
*****/



foobarzip.

endmod.
