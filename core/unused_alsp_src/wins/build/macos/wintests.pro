/*===================================================================================*
 |		wintests.pro
 |	Copyright (c) 1995 Applied Logic Systems, Inc.
 |
 |	Primitive Windows Test for MacOS
 |
 |	Creates Modeless dialog with a few active controls
 *===================================================================================*/


	
winItemNo(quitButton, 1).
winItemNo(okButton, 2).
winItemNo(cancelButton, 3).
winItemNo(helpButton, 4).
winItemNo(checkBox1, 5).
winItemNo(checkBox2, 6).
winItemNo(radioButton1, 7).
winItemNo(radioButton2, 8).
winItemNo(scrollBar, 9).
winItemNo(popupMenu, 10).
winItemNo(editText, 11).
winItemNo(staticText1, 12).
winItemNo(staticText2, 13).

wintest :-
	c_create_abs(str, 'wintests.rsrc', ResFileStr),
	cstr2pstr(ResFileStr), 
	m_OpenResFile(ResFileStr, RefNum),
	RefNum =:= 4294967295 ->
		(
			m_ResError(ErrCode),
			printf('Error (%t) opening resource file "wintests.rsrc".\n', [ErrCode]),
			fail
		); true,
	
	m_GetNewDialog(128, 0, -1, Dialog),
	m_SetPort(Dialog),
	doLoop,
	m_DisposeDialog(Dialog).


doLoop :-
	c_alloc(short, Item),
	m_ModalDialog(0, Item),
	c_examine(Item, short, ItemNo),
	winItemNo(ItemName, ItemNo),
	doItemAction(ItemName),
	ItemName \= quitButton,
	!,
	doLoop.
doLoop.
	

doItemAction(quitButton) :-
	printf('Quiting from wintest.\n').

doItemAction(okButton) :-
	!, printf('OK\n').
doItemAction(cancelButton) :-
	!, printf('cancel\n').
doItemAction(helpButton) :-
	!, printf('Help!\n').
	
doItemAction(AnyItem) :-
	printf('No Action Defined!\n').
	




/*
	c_create( int, 1, Argc),
	c_create_abs( str, 'Motif Windowing Test - ALS Prolog', WidgetAbs ),
	c_createn(int,2,[WidgetAbs, 0], Argv),
	x_XtInitialize(
		'widget',	% Application name
		'widget',	% Application class
		0,		% No options list
		0,		% Number of options
		Argc,		% Pointer to number of args
		Argv,		% Argv = application name
		TopLevel),

	createBulletinBoard('bboard', TopLevel, BBoard),
	  createPushButton('QUIT', BBoard, [5,5],PushButton),
	  createLabel('label', BBoard, [20,120], Label),
	  createRowColumn('rowcolumn', BBoard, [10,50], RowColumn),
	    createToggleButton('toggle1', RowColumn, Toggle1),
	    createToggleButton('toggle2', RowColumn, Toggle2),
	  createMessageBox('messagebox', BBoard, [145,2], MessageBox),
	  createText('text', BBoard, [90,125], Text),

	x_XtRealizeWidget(TopLevel),
	x_XtMainLoop.


createBulletinBoard(Name, Parent, Widget) :-
	x_xmBulletinBoardWidgetClass(WidgetClass),
	c_create_arglist([allowOverlap,1],AL),
	x_XtCreateManagedWidget(
		Name,
		WidgetClass,
		Parent,
		AL,
		1,
		Widget).

createPushButton(Name, Parent,[X,Y], Widget) :-
        x_xmPushButtonWidgetClass(WidgetClass),
	c_create_arglist([x,X,y,Y],AL),
        x_XtCreateManagedWidget(
		Name,
		WidgetClass,
		Parent,
	   	AL,
		2,
		Widget),
	c_rconst('callbackfunc',Callback),	% get the address of callback func
	c_create_abs(str,'pushbuttonCB', Data),
	x_XtAddCallback(
		Widget,
		'activateCallback',
		Callback, 
		Data).

createLabel(Name, Parent, [X,Y],Widget) :-
	x_xmLabelWidgetClass(WidgetClass),
	c_create_arglist([x,X,y,Y],AL),
        x_XtCreateManagedWidget(
		Name,
		WidgetClass,
		Parent,
	   	AL,
		2,
		Widget).

createRowColumn(Name, Parent, [X,Y], Widget) :-
	x_xmRowColumnWidgetClass(WidgetClass),
	c_create_arglist([x,X,y,Y,borderWidth,1],AL),
	x_XtCreateManagedWidget(
		Name,
		WidgetClass,
		Parent,
	   	AL,
		3,
		Widget).

createToggleButton(Name, Parent, Widget) :-
	x_xmToggleButtonWidgetClass(WidgetClass),
	x_XtCreateManagedWidget(
		Name,
		WidgetClass,
		Parent,
		0,
		0,
		Widget),
	c_create_abs(str,'togglebuttonCB', Data),
	c_rconst('callbackfunc',Callback),	% get the address of callback func
	x_XtAddCallback(
		Widget,
		'valueChangedCallback',
		Callback, 
		Data).

createMessageBox(Name, Parent, [X,Y], Widget) :-
	x_xmMessageBoxWidgetClass(WidgetClass),
	c_create_arglist([x,X,y,Y,borderWidth,2],AL),
	x_XtCreateManagedWidget(
		Name,
		WidgetClass,
		Parent,
		AL,
		3,
		Widget),
	c_rconst('callbackfunc',Callback),	% get the address of callback func
	c_create_abs(str,'messageboxCB', Data),
	x_XtAddCallback(
		Widget,
		'okCallback',
		Callback,
		Data).

createText(Name, Parent, [X,Y], Widget) :-
	x_xmTextWidgetClass(WidgetClass),
	c_create_arglist([x,X,y,Y],AL),
	x_XtCreateManagedWidget(
		Name,
		WidgetClass,
		Parent,
		AL,
		2,
		Widget),
	c_rconst('callbackfunc',Callback),	% get the address of callback func
	c_create_abs(str,'textCB', Data),
	x_XtAddCallback(
		Widget,
		'modifyVerifyCallback',
		Callback,
		Data).

'$callback'(Widget,Client_data,Call_data) :-
	c_examine(Client_data,str,Proc),
	Term =.. [Proc,Widget,Call_data],
	call(Term).

pushbuttonCB(Widget,Call_data) :-
	halt.

togglebuttonCB(Widget,Call_data) :-
	printf("Toggle button was activated\n").

messageboxCB(Widget, Call_data) :-
	printf("OK button was activated\n").

textCB(Widget, Call_data) :-
	printf("Text Verify callback invoked\n").

scrolledlistCB(Widget,Call_data) :-
	printf("Scrolled List was activated\n").

	
*/