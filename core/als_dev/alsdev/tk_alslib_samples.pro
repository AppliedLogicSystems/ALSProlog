/*=============================================================*
 |		tk_alslib_samples.pro
 |	Copyright (c) 1997 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |	Sample usage for various Tcl/Tk-based GUI library routines
 |	from the library file tk_alslib.pro
 *=============================================================*/


:- init_tk_alslib.

	%%------------------------------------
	%% 		Popup selection lists
	%%------------------------------------

popup_sel(1, Selection) 
	:-
	popup_select_items(
		['The first item','Item #2', 'Item three','The final item'], 
		Selection).

popup_sel(2, Selection) 
	:-
	popup_select_items(
		['The first item','Item #2', 'Item three','The final item'], 
		[mode=extended,title='Extended Mode Selection'],
		Selection).

popup_sel(3, Selection) 
	:-
	popup_select_items(
		['The first item','Item #2', 'Item three','The final item'], 
		[mode=multiple,title='Multiple Mode Selection'],
		Selection).

	%%------------------------------------
	%% 		Information/warning dialogs
	%%------------------------------------

info_d(1)
	:-
	info_dialog('Message for the User').
		 
info_d(2)
	:-
	info_dialog('Message for the User', 'Dialog Box Title').

	%%------------------------------------
	%% 		Yes/no dialogs
	%%------------------------------------

yn(1, Answer)
	:-
	yes_no_dialog('Sample yes-no query for user?', Answer).

yn(2, Answer)
	:-
	yes_no_dialog('Sample yes-no query for user?', 
					'Dialog Box Title',
					Answer).

yn(3, Answer)
	:-
	yes_no_dialog(tcli, 
				'Sample yes-no query for user?',
				'Dialog Box Title',
				'OK', 'Cancel', Answer).

	%%------------------------------------
	%% 		Atom input dialogs
	%%------------------------------------

aind(1, Atom)
	:-
	atom_input_dialog('Please input something:', Atom).

aind(2, Atom)
	:-
	atom_input_dialog('Please input something:', 'Dialog Box Title', Atom).

	%%------------------------------------
	%% 		File selection dialogs
	%%------------------------------------

file_sel(1, File) 
	:- 
	file_select_dialog(File).

file_sel(2, File) 
	:- 
	file_select_dialog(
		[title='Testing File Selection for Open',
		 filetypes= [[zip,[zip]],
					 ['Prolog',['pro']],
					 ['All Files',['*']] ]
		],
		File).
	
file_sel(3, File) 
	:- 
	file_select_dialog(
		[title='Testing File Selection for Save As',
		 mode = save_as,
		 filetypes= [[zip,[zip]],
					 ['All Files',['*']] ],
		 defaultname=fooey,
		 ext = '.zip'
		],
		File).

	%%------------------------------------
	%%    Adding to the main menubar:
	%%------------------------------------

tamb(1)
	:-
	extend_main_menubar('Test Extend', 
			['Test Entry #1', 'Test Entry #2']).

tamb(2)
	:-
	extend_main_menubar('Test Extend', 
			['Test Entry #1' + tcl('Window show .break_choices'), 
			 'Test Entry #2' + test_write
			]).

test_write 
	:-
	printf(user_output, 'This is a test ...\n', []),
	flush_input(user_input).

