ff :-
builtins:first_enter_key(fookey).

module builtins.

export demo_init/0.
demo_init
	:-
	builtins:sys_searchdir(SSD),
	join_path([SSD, 'alspro.key'], KeyPath),
	(exists_file(KeyPath) ->
		has_key_file(KeyPath)
		;
		no_key_file(KeyPath)
	).

qkc :-
	builtins:sys_searchdir(SSD),
	join_path([SSD, 'alspro.key'], KeyPath),
	exists_file(KeyPath),
	!,
	open(KeyPath, read, IS, []),
	get_line(IS, KeyAtom),
	close(IS),
	check_key_type(KeyAtom, KeyType, KeyInfo, KeyPath),
	(KeyType = permanent -> 
		true
		;
		(KeyType = day30-_ ->
			date(Today),
			date_less(Today, KeyInfo)
			;
			printf(user_output, 'Missing valid key: Run ALS Prolog (alsdev) first\n', []),
			halt
		)
	).
qkc :-
	printf(user_output, 'Missing valid key: Run ALS Prolog (alsdev) first\n', []),
	halt.
		
no_key_file(KeyPath)
	:-
	sprintf(atom(Msg),'%t\n%t\n%t',[
		'This copy of ALS Prolog will quit after 15 minutes unless you enter an appropriate registration key.',
		'To obtain a free key which will allow ALS Prolog to run uninterrupted for one month, go to',
		'     www.als.com/demo'
		]),
	yes_no_dialog(shl_tcli, Msg, 'Welcome', 
					'I have a key', 'I\'ll get a key later', Ans),
	no_key_file_cont(Ans, KeyPath).

no_key_file_cont('I have a key', KeyPath)
	:-!,
	first_enter_key(KeyPath).

no_key_file_cont(_, _)
	:-
	demo15_year_check,
	!,
	M15 is 15 * 60 * 1000, 
	tcl_call(shl_tcli, [after,M15,'prolog call builtins demo_shutdown'],_),
	setup_demo_examples,
	sprintf(atom(Msg2),'%t\n%t\n%t',[
		'Click on the menubar > Demo for info about included programs.',
		'Run ALS Prolog Manual.pdf to read the User Guide in Adobe Acrobat.',
		'Load als_help.htm in your browser to view the Reference Help.']),
	tcl_call(shl_tcli,
		[tk_dialog,'.ddtop', 'ALS Prolog Demo',Msg2,'',0,'OK'],_).

no_key_file_cont(_, _)
	:-
	sprintf(atom(Msg),'%t\n%t\n%t',[
		'This demo version of ALS Prolog has exceeded its lifetime.',
		'To obtain a new demo, or to purchase a copy of ALS Prolog, please go to',
		'     www.als.com'
		]),
	tcl_call(shl_tcli,
		[tk_dialog,'.ddtop', 'ALS Prolog Demo',Msg,'',0,'OK'],_),
	tcl_call(shl_tcli, [exit], _).

setup_demo_examples
	:-
	locate_examples_dir(ExamplesDir),
	abolish(examples_dir,1),
	assert(examples_dir(ExamplesDir)),
	menu_demo_setup.

demo15_year_check
	:-
	date(Today),
	date_less(Today, 2001/1/1).

demo_done_msg
	:-
	sprintf(atom(Msg),'%t\n%t\n%t\n%t',[
		'This demo version of ALS Prolog will now quit.',
		'The next screen will let you save your work.',
		'To obtain a free one-month registration key, go to',
		'      www.als.com/demo']),
	tcl_call(shl_tcli,
		[tk_dialog,'.ddtop', 'ALS Prolog Demo',Msg,'',0,'OK'],_).
		
demo_shutdown
	:-
	demo_done_msg,
	tcl_call(shl_tcli, ['document.close_all'], _),
	tcl_call(shl_tcli, [exit], _).	

first_enter_key(KeyPath)
	:-
	Msg = 'Please input your 16-digit registration number:',
	Title = 'Registration Key Input',	
	atomic_input_dialog(shl_tcli, Msg, Title, KeyAtom),
	try_enter_key(KeyAtom, KeyPath).

try_enter_key(KeyAtom, KeyPath)
	:-
	check_key_type(KeyAtom, KeyType, KeyInfo, KeyPath),
	!,
	first_acton_key(KeyType, KeyInfo, KeyAtom, KeyPath).

try_enter_key(KeyAtom, KeyPath)
	:-
	sorry_no_go(KeyAtom).

first_acton_key(permanent-_, PersonalInfo, KeyAtom, KeyPath)
	:-!,
	open(KeyPath, write, OS, []),
	write(OS, KeyAtom),
	close(OS),
	setup_personalization(PersonalInfo),

	sprintf(atom(L1), 
		'Congratulations %t for buying ALS Prolog!',
		[PersonalInfo]),
	sprintf(atom(Msg),'%t\n%t\n%t',[
		L1,
		'Remember that our web site is a growing resource:',
		'      www.als.com'
		]),
	tcl_call(shl_tcli,
		[tk_dialog,'.ddtop', 'ALS Prolog',Msg,'',0,'OK'],_).

first_acton_key(day30-_, QDate, KeyAtom, KeyPath)
	:-
	date(Today),
	date_less(Today, QDate),
	!,
	open(KeyPath, write, OS, []),
	write(OS, KeyAtom),
	close(OS),
	cont_has_key_file(day30-_, QDate, KeyAtom, KeyPath).

first_acton_key(_, _, KeyAtom, _)
	:-
	sorry_no_go(KeyAtom).

sorry_no_go(KeyAtom)
	:-
	sprintf(atom(Msg),'Sorry, the key\n    %t\nis invalid.',[KeyAtom]),
	tcl_call(shl_tcli,
		[tk_dialog,'.ddt2', 'Invalid Key',Msg,'',0,'OK'],_),
	tcl_call(shl_tcli,[exit], _).
	
	%% A stub for now:
setup_personalization(_).

has_key_file(KeyPath)
	:-
	open(KeyPath, read, IS, []),
	get_line(IS, KeyAtom),
	close(IS),
	check_key_type(KeyAtom, KeyType, KeyInfo, KeyPath),
	cont_has_key_file(KeyType, KeyInfo, KeyAtom, KeyPath).

cont_has_key_file(permanent-_, _, _, _)
	:-!.

cont_has_key_file(day30-_, QDate, KeyAtom, KeyPath)
	:-
	date(Today),
	date_less(Today, QDate),
	!,
	setup_demo_examples,
	sprintf(atom(Msg),'%t\n%t\n%t\n%t',[
		'This demo version of ALS Prolog will run until ',
		QDate,
		'To purchase a copy of ALS Prolog, please go to',
		'www.als.com/purchase.html'
		]),
	tcl_call(shl_tcli,
		[tk_dialog,'.ddtop', 'ALS Prolog Demo',Msg,'',0,'OK'],_),
	sprintf(atom(Msg2),'%t\n%t\n%t',[
		'Click on the menubar > Demo for info about included programs.',
		'Run ALS Prolog Manual.pdf to read the User Guide in Adobe Acrobat.',
		'Load als_help.htm in your browser to view the Reference Help.']),
	tcl_call(shl_tcli,
		[tk_dialog,'.ddtop', 'ALS Prolog Demo',Msg2,'',0,'OK'],_).

cont_has_key_file(_, _, _, _)
	:-
	sprintf(atom(Msg),'%t\n%t\n%t',[
		'This demo version of ALS Prolog has exceeded its lifetime.',
		'To purchase a copy of ALS Prolog, please go to',
		'www.als.com/purchase.html'
		]),
	tcl_call(shl_tcli,
		[tk_dialog,'.ddtop', 'ALS Prolog Demo',Msg,'',0,'OK'],_),
	tcl_call(shl_tcli, [exit], _).


:- dynamic(menu_setup_done/0).

menu_demo_setup
	:-
	menu_setup_done,!.

menu_demo_setup
	:-
	MenuEntriesList = [
		('Original Chat 80' + (builtins:demo_chat_80)), 
		('Naive Reverse' + (builtins:demo_nrev)),
		('Prolog 1000 Database' + (builtins:demo_p1k)),
		('Visual Eight Queens' + (builtins:demo_vqueens)),
		('Visual Desk Calculator' + (builtins:demo_vdc)),
		('Drawing Demonstration' + (builtins:demo_drawing)),
		('Hickory Tree Identification' + (builtins:demo_hickid)),
		('NIM Playing Program' + (builtins:demo_vnim)),
		separator,
		('About the Demo Programs' + (builtins:about_demos))
		],
	extend_main_menubar('Demo', MenuEntriesList),
	assert(menu_setup_done).

locate_examples_dir(Dir)
	:-
	builtins:sys_searchdir(SSD),
	split_path(SSD, SSDS),
	dreverse(SSDS, [alsdir | XX]),
	dreverse(SSDS, [alsdir | XX]),
	dreverse(EDS, [examples | XX]),
	join_path(EDS, Dir).

export demo_chat_80/0.
demo_chat_80
	:-
	examples_dir(ED),
	join_path([ED,'Chat80'], C80Path),
	sprintf(atom(Msg),'%t\n%t\n%t',[
		'This is the original Pereira & Warren Chat 80 program.',
		'Start the program by typing the goal:   hi.',
		'Exit the program by typing   bye.  to the Question: prompt.'
		]),
	tcl_call(shl_tcli,
		[tk_dialog,'.ddtop', 'ALS Prolog Demo',Msg,'',0,'OK'],_),
	write('Consulting the Chat 80 files...'),nl,nl,
	join_path([C80Path, 'als_chat'], F1),
	consult(F1),
	join_path([C80Path, 'load'], F2),
	consult(F2),
	user:hi,
	nl, alsdev:clear_workspace, nl, 
	flush_input(user_input), flush_output.

demo_nrev 
	:- 
	examples_dir(ED), 
	join_path([ED,als,nrev], NrevPath), 
	consult(NrevPath), 
	printf('\nThis is the classical Naive Reverse (speed test) example...\n\n', []), 
	user:nrev, 
	nl, alsdev:clear_workspace, nl, 
	flush_input(user_input), flush_output.  
	
demo_p1k 
	:- 
	examples_dir(ED), 
	sprintf(atom(Msg),'%t\n%t\n%t',[ 
		'This example allows you to browse the "Prolog 1000" database using a visual browser.', 
		'The original data for the "Prolog 1000" was processed into a variable-length record database on disk.',
		'That external data file is being accessed during this demo.'
		]),
	tcl_call(shl_tcli,
		[tk_dialog,'.ddtop', 'ALS Prolog Demo',Msg,'',0,'OK'],_),
	join_path([ED,'Prolog1000'], P1000Path),
	get_cwd(CurDir),
	change_cwd(P1000Path),
	join_path([ED,'Prolog1000',p1k_db], P1kdbPath),
	consult(P1kdbPath),
	user:p1k_db,
	printf('Hit RETURN to finish...', []),
	get_code(_),
	change_cwd(CurDir),
	nl, alsdev:clear_workspace, nl, 
	flush_input(user_input), flush_output.  

demo_vqueens 
	:- 
	examples_dir(ED), 
	printf('\nThis is a visual version of the eight queens example...\n\n', []), 
	join_path([ED,visual], VPath),
	get_cwd(CurDir),
	change_cwd(VPath),
	join_path([ED,visual,vqueens], VQPath), 
	consult(VQPath), 
	user:all_queens, 
	change_cwd(CurDir),
	nl, alsdev:clear_workspace, nl, 
	flush_input(user_input), flush_output.  
	
demo_vdc 
	:- 
	examples_dir(ED), 
	printf('\nThis is a visual version of the original \n', []), 
	printf('Desk Calculator example with a complete functional\n', []), 
	printf('programming language (Kevin Buettner).\n\n', []), 
	join_path([ED,visual], VPath),
	get_cwd(CurDir),
	change_cwd(VPath),
	join_path([ED,visual,vdc], VDCPath), 
	consult(VDCPath), 
	user:vdc,
	printf('Hit RETURN to finish...', []),
	change_cwd(CurDir),
	get_code(_),
	nl, alsdev:clear_workspace, nl, 
	flush_input(user_input), flush_output.  
	
demo_drawing 
	:- 
	examples_dir(ED), 
	printf('\nThis example illustrates the ability to draw on Tcl/Tck canvases...\n\n', []), 
	join_path([ED,visual], VPath),
	get_cwd(CurDir),
	change_cwd(VPath),
	join_path([ED,visual,drawing], DPath), 
	consult(DPath), 
	user:sample_draw,
	printf('This drawing was created by the following program:\n\n', []),

	write_out(user_output,
	(sample_draw 
		:-
		start_drawing,
		oval(50,60,230,270,[fill=green]),
		line(50,200,210,60,[fill=blue,width=3]),
		arc(55,260,210,65,30,140,[width=2,fill=red]),
		rect(200,120,300,40, [width=6,outline=brown]),
		poly([55,230,85,150,120,310,190,90],[fill=yellow,width=2,outline=purple])
	 	)),nl,
	 printf('Hit RETURN to finish...', []),
	 get_code(_),
	change_cwd(CurDir),
	nl, alsdev:clear_workspace, nl, 
	flush_input(user_input), flush_output.  
	
demo_hickid 
	:- 
	sprintf(atom(Msg),'%t\n%t\n%t',[
		'This is a visual version the original Hickory Tree',
		'identification expert system demonstration program from',
		'"Prolog and Expert Systems", by Kenneth A. Bowen.' 
		]),
	tcl_call(shl_tcli,
		[tk_dialog,'.ddtop', 'ALS Prolog Demo',Msg,'',0,'OK'],_),
	examples_dir(ED), 
	join_path([ED,visual], VPath),
	get_cwd(CurDir),
	change_cwd(VPath),
	join_path([ED,visual,hickory], HPath), 
	consult(HPath), 
	join_path([ED,visual,id], IPath), 
	consult(IPath), 
	user:start_id,
	printf('Hit RETURN to finish...', []),
	get_code(_),
	change_cwd(CurDir),
	nl, alsdev:clear_workspace, nl, 
	flush_input(user_input), flush_output.  

demo_vnim 
	:- 
	examples_dir(ED), 
	printf('\nThis is a visual version of the original NIM program.\n', []), 
	join_path([ED,visual], VPath),
	get_cwd(CurDir),
	change_cwd(VPath),
	join_path([ED,visual,vnim], VDCPath), 
	consult(VDCPath), 
	user:vnim,
	printf('Hit RETURN to finish...', []),
	change_cwd(CurDir),
	get_code(_),
	nl, alsdev:clear_workspace, nl, 
	flush_input(user_input), flush_output.  
	

about_demos
	:-
	examples_dir(ED), 
	join_path([ED,'about.txt'], AboutPath),
	grab_lines(AboutPath, Lines),
	write_lines(Lines).



endmod.

%:- builtins:demo_init.
