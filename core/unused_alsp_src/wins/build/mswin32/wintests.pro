/*======================================================================*
 |		wintest.pro
 |	Copyright (c) 1996 Applied Logic Systems, Inc.
 |
 |	Primitive Windows Tests for Microsoft Win32
 |
 *======================================================================*/

wintest :- generic.

/*======================================================================*
 |
 |  This program is based on Microsoft's Win32 sample program GENERIC.
 |	
 |	Generic demostrates the basic machinery needed for a Win32 GUI
 |	application.
 |  
 |   The following pieces of the C Generic are not currently implemented:
 |		Previous instance detection.
 |		Accelarator tables.
 |		MyRegisterClass function.
 |		non-Win95 help menu.
 |		About box.
 |		Window Help calls.
 |		Right-button popup menu.
 |		Display change handling.    
 |
 | 	A Note on Resources:
 | 	
 | 	To make generic.pro a complete stand-alone Prolog application, we
 | 	elected to contruct menus and other resources dynamically.
 |	
 |	If desired resources can be linked into the ALS Prolog executable,
 |	and used in the normal fashion.
 | 
 *======================================================================*/


generic :-

	/* Fetch the WinMain arguments. */
	winmain_arguments(Instance, PrevInstance, CommandLine, Show),

	initApplication(Instance),

	c_const('SW_SHOWNORMAL', SW_SHOWNORMAL),
	initInstance(Instance, SW_SHOWNORMAL, Window),
	
	mainloop(AccelTable).


mainloop(AccelTable) :-
	c_alloc_abs('MSG', Msg),
	w_GetMessage(Msg, 0, 0, 0, R),
	R =\= 0,
	!,
	w_DispatchMessage(Msg, _),
	c_free(Msg),
	mainloop(AccelTable).
mainloop(_).

appName('Generic').

initApplication(Instance) :-

	c_const('CS_HREDRAW', CS_HREDRAW),
	c_const('CS_VREDRAW', CS_VREDRAW),
	Style is CS_HREDRAW \/ CS_VREDRAW,
	
	/* Get the universal prolog WindowProc callback. This will be bound to a
	   prolog term below in initInstance. */
	c_rconst('WindowProcCallback', WindowProcCallback),
	
	/* Use the ALS Prolog Icon, since we don't have generic's resources. */
	w_LoadIcon(Instance, 'ALSPRO', Icon),		
	c_const('IDC_ARROW', IDC_ARROW),
	w_LoadCursor(0, IDC_ARROW, Cursor),
	c_const('COLOR_WINDOW', COLOR_WINDOW),
	Background is COLOR_WINDOW + 1,
	appName(Name),
	c_create_abs(str, Name, CName),

	/* Note: we don't include a menu name, because we will dynamically create
	   menus for this example. */

	c_create_abs('WNDCLASS',
		[style,			Style,
		 lpfnWndProc,	WindowProcCallback,
		 cbClsExtra,	0,
		 cbWndExtra,	0,
		 hInstance,		Instance,
		 hIcon,			Icon,
		 hCursor,		Cursor,
		 hbrBackground,	Background,
		 lpszMenuName,  0,
		 lpszClassName, CName
		 ], WC),
		
	w_RegisterClass(WC, Z).


initInstance(Instance, CmdShow, Window) :-

	/* Dynamically create the menu bar. */
	buildMenuBar(Menu),

	appName(Name),
	c_const('WS_OVERLAPPEDWINDOW', WS_OVERLAPPEDWINDOW),
	c_const('CW_USEDEFAULT', CW_USEDEFAULT),

	w_CreateWindowEx(0, Name, 'ALS Prolog Test Window',
		WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, 0, CW_USEDEFAULT, 0, 0, Menu, Instance, 0, Window),
	
	/* Bind the generic WindowProc callback with the WndProc predicate and this particular window. */
	c_rconst('WindowProcCallback', WndProc),
	c_bind_callback(WndProc, Window, wndproc),
	
	w_ShowWindow(Window, CmdShow, _),
	w_UpdateWindow(Window, _).

/* The window callback predicate. */

wndproc(Window, Message, WParam, LParam, Result) :-
	c_const('WM_COMMAND', WM_COMMAND),
	Message = WM_COMMAND,
	do_command(Window, Message, WParam, LParam, Result).
		
wndproc(Window, Message, WParam, LParam, Result) :-
	c_const('WM_DESTROY', WM_DESTROY),
	Message = WM_DESTROY,
	printf('Received the destroy message.\n'),
	w_PostQuitMessage(0),
	Result = 0.
	
wndproc(Window, Message, WParam, LParam, Result) :-
	w_DefWindowProc(Window, Message, WParam, LParam, Result).

/* Command handling predicates. */

do_command(Window, Message, WParam, LParam, Result) :-
	WMID is WParam /\ 0xFFFF,
	menu_item(exit, _, ExitID, _),
	WMID = ExitID,
	printf('Received the exit command\n'),
	w_DestroyWindow(Window, _),
	Result = 0.

/* Menu bar data. */

menu_bar([file, edit, help]).

popup_menu(file, '&File', [new, open, save, save_as, separator, print, print_setup, separator, exit]).
popup_menu(edit, '&Edit', [undo, separator, cut, copy, paste, link, separator, links]).
popup_menu(help, '&Help', [help_topics, about]).

menu_item(new, '&New', 100.0, disabled).
menu_item(open, '&Open...', 101.0, disabled).
menu_item(save, '&Save', 102.0, disabled).
menu_item(save_as, 'Save &As...', 103.0, disabled).
menu_item(print, '&Print...', 104.0, disabled).
menu_item(print_setup, 'P&rint Setup...', 105.0, disabled).
menu_item(exit, 'E&xit', 106.0, enabled).

menu_item(undo, '&Undo\tCtrl+Z', 200.0, disabled).
menu_item(cut, 'Cu&t\tCtrl+X', 201.0, disabled).
menu_item(copy, '&Copy\tCtrl+C', 202.0, disabled).
menu_item(paste, '&Paste\tCtrl+V', 203.0, disabled).
menu_item(link, 'Paste &Link', 204.0, disabled).
menu_item(links, 'Lin&ks...', 205.0, disabled).

menu_item(help_topics, '&Help Topics...', 304.0, enabled).
menu_item(about, '&About Generic...', 303.0, enabled).


/* Menu bar construction predicates. */

buildMenuBar(MenuBar) :-
	w_CreateMenu(MenuBar),
	MenuBar =\= 0,
	menu_bar(PopupList),
	append_popup_menus(MenuBar, PopupList).

append_popup_menus(Menu, []).
append_popup_menus(Menu, [Popup | Rest]) :-
	popup_menu(Popup, Text, ItemList),
	w_CreatePopupMenu(PopupMenu),
	PopupMenu =\= 0,
	append_menu_items(PopupMenu, ItemList),
	c_const('MF_STRING', MF_STRING),	
	c_const('MF_POPUP', MF_POPUP),	
	Flags is MF_STRING \/ MF_POPUP,
	w_AppendMenu(Menu, Flags, PopupMenu, Text, 1.0),
	append_popup_menus(Menu, Rest).

append_menu_items(Menu, []).
append_menu_items(Menu, [Item | Rest]) :-
	append_menu_item(Menu, Item),
	append_menu_items(Menu, Rest).
	
append_menu_item(Menu, separator) :-
	c_const('MF_SEPARATOR', Flags),
	w_AppendMenu(Menu, Flags, 0, 0, 1.0).
	
append_menu_item(Menu, Item) :-
	c_const('MF_STRING', MF_STRING),	
	menu_item(Item, Text, ID, InitState),
	menu_state_flags(InitState, StateFlags),
	Flags is MF_STRING \/ StateFlags,
	w_AppendMenu(Menu, Flags, ID, Text, 1.0).

menu_state_flags(enabled, Flags) :- c_const('MF_ENABLED', Flags).
menu_state_flags(disabled, Flags) :- c_const('MF_GRAYED', Flags).

