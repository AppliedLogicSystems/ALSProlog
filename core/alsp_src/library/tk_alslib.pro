/*=============================================================*
 |		tk_alslib.pro
 |	Copyright (c) 1986-1996 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |	Tcl/Tk-based GUI library routines
 |
 |	Author: Ken Bowen
 |	Creation Date: 1997
 *=============================================================*/


/*
t(X) :-
	popup_select_items([aa,bb,cc,dd,ee], X).

u(X) :-
	popup_select_items([aa,bb,cc,dd,ee], 
		[mode=extended,title='Extended Mode Selection'],X).

v(X) :-
	popup_select_items([aa,bb,cc,dd,ee], 
		[mode=multiple,title='Multiple Mode Selection'],X).
*/

module tk_alslib.
use tcltk.

export init_tk_alslib/0.
export init_tk_alslib/1.
export popup_select_items/2.
export popup_select_items/3.
export extend_main_menubar/2.
export extend_main_menubar/3.
export extend_main_menubar/4.
export extend_menubar_cascade/2.
export extend_menubar_cascade/4.

/*------------------------------------------------------------*
 *------------------------------------------------------------*/

init_tk_alslib
	:-
	init_tk_alslib(tcli,_).

init_tk_alslib(Interp,Shared)
	:-
	(all_procedures(tcltk,read_eval_results,2,_) ->
		true
		;
		consult(tcltk)
	),
	tk_new(Interp),
	tcl_call(Interp, [wm,withdraw,'.'], _),
	builtins:sys_searchdir(ALSDIR),
	extendPath(ALSDIR, shared, Shared),
	tcl_call(Interp, [set,'ALSTCLPATH',Shared], _),

	pathPlusFile(Shared, 'als_tklib.tcl', ALSTKLIB),
	tcl_call(Interp, [source, ALSTKLIB], _).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%% 		POPUP SELECTION LISTS 			%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*------------------------------------------------------------*
 |	popup_select_items/2.
 |	popup_select_items(SourceList, ChoiceList)
 |	popup_select_items(+, -)
 |
 |	Calls popup_select_items(SourceList, Options, ChoiceList),
 |		filling in default Options
 *------------------------------------------------------------*/
popup_select_items(SourceList, ChoiceList)
	:-
	popup_select_items(SourceList, [], ChoiceList).

/*------------------------------------------------------------*
 |	popup_select_items/3
 |	popup_select_items(SourceList, Options, ChoiceList)
 |	popup_select_items(+, +, -)
 |
 |	SourceList	= Prolog list of atoms;
 |	Options		= list option equations (see below); 
 |	ChoiceList	= list of items selected by user
 |
 |	Option equations:   Option = Value
 |	Options:
 |	  -	interp	- the Tcl/Tk interpreter to run under;
 |	  -	mode	- the listbox mode to utilize:
 |		* select at most 1 element; clicking a different element 
 |		  changes the selection:
 |			browse	- can drag thru selections with button 1 down;
 |			single	- no drag;
 |		* select multiple elements (including discontiguous groups):
 |			multiple - clicking button 1 on  an  element toggles its 
 |				selection state without affecting any other elements;
 |			extended - pressing button 1  on  an  element selects  it,  
 |				deselects everything else, and sets the anchor to the 
 |				element under the mouse;   dragging  the  mouse  with
 |				button  1 down extends the selection to include all the 
 |				elements between the anchor and the element  under  the  
 |				mouse, inclusive.
 |	  -	choice_widget_name - base widget name for the choice box.
 |	  -	title - the window title bar entry
 *------------------------------------------------------------*/
popup_select_items(SourceList, Options, ChoiceList)
	:-
	(dmember(choice_widget_name=BaseName, Options) -> true;
		BaseName = '.popup_select_widget'), 
	(dmember(mode=Mode, Options) -> true;
		Mode = browse),
	(dmember(interp=Interp, Options) -> true ;
		Interp = tcli),
	(dmember(title=Title, Options) -> true ;
		Title = 'List Selection'),
	mk_tcl_atom_list(SourceList, TclSourceList),
	tcl_call(Interp, [do_select_items,BaseName,Mode,Title,TclSourceList],ChoiceList).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%% 				DIALOGS 				%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	/*----------------------------------------------------*
	 |	Note: available bitmaps for use in dialogs are:
	 |
	 |	question	?
	 |	questhead	? inside a sunken human head
	 |	warning		!
	 |	info		sunken i
	 |	hourglass	sunken hourglass
	 |	error		sunken european "forbidden" roadsign
	 *----------------------------------------------------*/

/*------------------------------------------------------------*
 |	info_dialog/1
 |	info_dialog(Msg)
 |	info_dialog(+)
 |
 |	info_dialog/2
 |	info_dialog(Msg, Title)
 |	info_dialog(+, +)
 |
 |	info_dialog/3
 |	info_dialog(Interp, Msg, Title)
 |	info_dialog(+, +, +)
 |
 |	Pops up a informational dialog (just an OK button)
 |	displaying the input message (Msg). Title is the dialog
 |	window title; it defaults to "Info"
 *------------------------------------------------------------*/

export info_dialog/1.
export info_dialog/2.
export info_dialog/3.

info_dialog(Msg)
	:-
	info_dialog(Msg, 'Info').

info_dialog(Msg, Title)
	:-
	info_dialog(tcli, Msg, Title).

info_dialog(Interp, Msg, Title)
	:-
	tcl_call(Interp, [tk_dialog, '.quit_dialog', Title, Msg, '', 0, 'OK'], _).

/*------------------------------------------------------------*
 |	yes_no_dialog/2
 |	yes_no_dialog(Msg, Answer)
 |	yes_no_dialog(+, -)
 |
 |	yes_no_dialog/3
 |	yes_no_dialog(Msg, Title, Answer)
 |	yes_no_dialog(+, +, -)
 |
 |	yes_no_dialog/4
 |	yes_no_dialog(Interp, Msg, Title, Answer)
 |	yes_no_dialog(+, +, +, -)
 |
 |	Pops up a yes-no dialog (with Yes/No buttons)
 |	displaying the input message (Msg). Title is the dialog
 |	window title; it defaults to "Info" The extended forms
 |	allow customization of the buttons:
 |
 |	yes_no_dialog/6
 |	yes_no_dialog(Interp, Msg, Title, YesLabel, NoLabel, Answer)
 |	yes_no_dialog(+, +, +, +, +, -)
 |
 |	Yes = label for the left (Yes; default) button
 |	No =  label for the right (No) button
 |
 |	Answer is either YesName or NoName
 *------------------------------------------------------------*/

export yes_no_dialog/2.
export yes_no_dialog/3.
export yes_no_dialog/4.

yes_no_dialog(Msg, Answer)
	:-
	yes_no_dialog(Msg, 'Info', Answer).

yes_no_dialog(Msg, Title, Answer)
	:-
	yes_no_dialog(tcli, Msg, Title, Answer).

yes_no_dialog(Interp, Msg, Title, Answer)
	:-
	yes_no_dialog(Interp, Msg, Title, 'Yes', 'No', Answer).

yes_no_dialog(Interp, Msg, Title, YesLabel, NoLabel, Answer)
	:-
	tcl_call(Interp, [tk_dialog, '.quit_dialog', Title, Msg, 
						question, 0, YesLabel, NoLabel], InitAns),
	(InitAns =  0 -> Answer = YesLabel ; Answer = NoLabel).

/*------------------------------------------------------------*
 |	atom_input_dialog/2
 |	atom_input_dialog(Msg, Atom)
 |	atom_input_dialog(+, -)
 |
 |	atom_input_dialog/3
 |	atom_input_dialog(Msg, Title, Atom)
 |	atom_input_dialog(+, +, -)
 |
 |	atom_input_dialog/4
 |	atom_input_dialog(Interp, Msg, Title, Atom)
 |	atom_input_dialog(+, +, +, -)
 *------------------------------------------------------------*/
export atom_input_dialog/2.
export atom_input_dialog/3.
export atom_input_dialog/4.

atom_input_dialog(Msg, Atom)
	:-
	atom_input_dialog(Msg, 'Input', Atom).

atom_input_dialog(Msg, Title, Atom)
	:-
	atom_input_dialog(tcli, Msg, Title, Atom).

atom_input_dialog(Interp, Msg, Title, Atom)
	:-
	tcl_call(Interp, [do_popup_input, Msg, Title], InitResult),
	(atom(InitResult) ->
		Atom = InitResult
		;
		atomread(InitResult, Atom)
	).

/*------------------------------------------------------------*
 |	file_select_dialog/2
 |	file_select_dialog(Prompt, FileName)
 |	file_select_dialog(+, -)
 |
 |	file_select_dialog/4
 |	file_select_dialog(Interp, Prompt, Options, FileName)
 |	file_select_dialog(+, +, +, -)
 |
 |	Options:
 |		default = <Default file name>
 |		ext		= Ext to either add, or use for selection
 |		mode	= new/select/save_as (default = select)
 |		initialdir = 	initial dir in which to begin...
 *------------------------------------------------------------*/

file_select_dialog(Prompt, FileName)
	:-
	file_select_dialog(tcli, Prompt, FileName, []).

file_select_dialog(Interp, Prompt, FileName, Options)
	:-
	fselect_modes(Options, DefaultName, Ext, Mode, IDir),

		%% Need to extend this to file types broadly, including the
		%% Mac special stuff;
	
	cont_file_select(Mode, DefaultName, Ext, IDir, FileName).

/*
cont_file_select(save_as, DefaultName, Ext, IDir FileName)
	:-!,

cont_file_select(select, DefaultName, Ext, IDir FileName)
	:-!,
	tcl_call(Interp, 
				[tk_getOpenFile,'-title','Select File to Open' | FileTypes], 
				FileName).
*/


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%	  ADDING ITEMS TO THE MENU BAR		%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*------------------------------------------------------------*
 *------------------------------------------------------------*/
extend_main_menubar(Label, MenuEntriesList)
	:-
	extend_menubar('.topals.mmenb', Label, MenuEntriesList).

extend_menubar(MenubarPath, Label, MenuEntriesList)
	:-
	extend_menubar(MenubarPath, Label, MenuEntriesList, shl_tcli).

extend_menubar(MenubarPath, Label, MenuEntriesList, Interp)
	:-
	extend_menubar_cascade(MenubarPath, Label, MenuPath, Interp),
	list_extend_cascade(MenuEntriesList, MenuPath, Interp).

extend_menubar_cascade(Label, MenuPath)
	:-
	extend_menubar_cascade('.topals.mmenb', Label, MenuPath, shl_tcli).

extend_menubar_cascade(MenubarPath, Label, MenuPath, Interp)
	:-
	make_lc_sym(Label, TclLabel),
	catenate([MenubarPath, '.', TclLabel], MenuPath),
	(tcl_call(shl_tcli, [winfo,exists,MenuPath],'1') ->
		true
		;
		tcl_call(Interp, [menu,MenuPath,'-relief',raised],_)
	),
	tcl_call(Interp, [MenubarPath,add,cascade,'-label',Label,'-menu',MenuPath], _).

list_extend_cascade([], MenuPath, Interp).
list_extend_cascade([MenuEntry | MenuEntriesList], MenuPath, Interp)
	:-
	extend_cascade(MenuEntry, MenuPath, Interp),
	list_extend_cascade(MenuEntriesList, MenuPath, Interp).

extend_cascade(Label + tcl(Call), MenuPath, Interp)
	:-!,
	tcl_call(Interp, [MenuPath,add,command,'-label',Label,'-command',Call],_).

extend_cascade(Label + (Mod:Goal), MenuPath, Interp)
	:-!,
	open(atom(Cmd), write, CmdS, []),
	Goal =.. [Functor | Args],
	printf(CmdS, 'prolog call %t %t ', [Mod, Functor]),
	mk_tcl_call_args(Args, CmdS),
	close(CmdS),
	tcl_call(Interp, [MenuPath,add,command,'-label',Label, '-command', Cmd], _).

extend_cascade(Label + Goal, MenuPath, Interp)
	:-!,
	open(atom(Cmd), write, CmdS, []),
	Goal =.. [Functor | Args],
	printf(CmdS, 'prolog call %t ', [Functor]),
	mk_tcl_call_args(Args, CmdS),
	close(CmdS),
	tcl_call(Interp, [MenuPath,add,command,'-label',Label, '-command', Cmd], _).

extend_cascade(separator, MenuPath, Interp)
	:-!,
	tcl_call(Interp, [MenuPath,add,separator], _).

extend_cascade(Label, MenuPath, Interp)
	:-
	tcl_call(Interp, [MenuPath,add,command,'-label',Label], _).

mk_tcl_call_args(Args, CmdS).
mk_tcl_call_args([Arg | Args], CmdS)
	:-
	insert_tcl_call_arg(Arg, CmdS),
	mk_tcl_call_args(Args, CmdS).

insert_tcl_call_arg(Arg, CmdS)
	:-
	atom(Arg),
	printf(CmdS, '-atom %t ',[Arg]).

insert_tcl_call_arg(Arg, CmdS)
	:-
	number(Arg),
	printf(CmdS, '-number %t ',[Arg]).

insert_tcl_call_arg(Arg, CmdS)
	:-
	var(Arg),
	printf(CmdS, '-var %t ',[Arg]).

insert_tcl_call_arg(Arg, CmdS)
	:-
	Arg = [ _ | _ ],
	printf(CmdS, '-list %t ',[Arg]).

/*------------------------------------------------------------*
 *------------------------------------------------------------*/



/*------------------------------------------------------------*
 *------------------------------------------------------------*/
endmod.
