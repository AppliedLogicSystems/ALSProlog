/*===============================================================*
 |	mainwin.pro
 |
 |	Sample code showing how to set up a "main window"
 |	for a Tcl/Tk GUI application.  The Tcl/Tk code is
 |	contained in the file "mainwin.tcl"
 *===============================================================*/

start_mainwin
	:-
		%% This initializes the tcli Tcl interpreter:
	init_tk_alslib,
		%% Avoid raising exceptions which might kill your
		%% program during regular execution:
	set_prolog_flag(unknown, fail),
		%% Load your Tcl code:
	tcl_call(tcli, [source,'mainwin.tcl'], _),
		%% This could be at the end of mainwin.tcl (your preference):
	tcl_call(tcli, ['Window',show,'.mainwin'], _),

		%% Set up the main window as an output stream:
	open(tk_win(tcli,'.mainwin.txtwin.text'), write, OutS, [alias(main_out)]),
	open(tk_win(tcli,'.mainwin.txtwin.text'), read, InS, [alias(main_in)]).

		%% When you run this inside the ALS IDE, there is already
		%% an event loop running.  If you package from the IDE,
		%% that remains true.  If you build a totally event-driven
		%% program (look at how the ALS IDE is built), the code
		%% stops right here, and everything happens in response to
		%% to button clicks and hitting Return on the main win;
		%% However, you could as well have an "in-charge" loop
		%% running from here....


	%% Stubs exhibiting some types of interaction:
	%% These are all attached to menu items on the
	%% main menu bar;  the calls invoking these stubs are
	%% generally of the form:
	%%
	%%	prolog call user load_existing_project
	%%
	%% "user" can be replaced another module name, as appropriate.
	%% If arguments are passed to the prolog goal, they must be
	%% typed, as in the following example:
	%%
	%%	prolog call user start_new_project -number $Ans
	%%
load_existing_project
	:-
	printf(main_out, 'User just clicked "Load Project"\n\n', []),
	flush_output(main_out).

start_new_project(Arg)
	:-
	printf(main_out, 'User just clicked "New Project" and\n', []),
	Choice is Arg + 1,
	printf(main_out, 'made choice #%t\n\n', [Choice]),
	flush_output(main_out).

save_project 
	:-
	file_select_dialog(FileName),
	printf(main_out, 'User just clicked "Save Project" and\n', []),
	printf(main_out, 'chose file \n\t%t\n', [FileName]),
	flush_output(main_out).


save_project_as
	:-
	SourceList = [apples, oranges, 'Kiwi Fruit', 'Bacon & Eggs'],
	popup_select_items(SourceList, [mode=extended], ChoiceList),
	printf(main_out, 'User just clicked "Save Project As" and\n', []),
	printf(main_out, 'chose items \n\t%t\n', [ChoiceList]),
	flush_output(main_out).

delete_project
	:-
	printf(main_out, 'User just clicked "Delete Project"...\n', []),
	printf(main_out, 'Please type something (e.g., your name) plus <Return>:', []),
	flush_output(main_out),
	get_line(main_in, UserInput),
	printf(main_out, 'Received: \n\t%t\n', [UserInput]),
	flush_output(main_out).


whatever_project
	:-
	printf(main_out, 'User just clicked "Whatever Project"...\n', []),
	printf(main_out, 'Please type a Prolog term plus <Return>:', []),
	flush_output(main_out),
	read(main_in, UserInput),
	printf(main_out, 'Received: \n\t%t\n', [UserInput]),
	flush_output(main_out).


activate_main_in(Line)
	:-
	printf(main_out, '\nUser just typed %t\n', [Line]),
	flush_output(main_out).


