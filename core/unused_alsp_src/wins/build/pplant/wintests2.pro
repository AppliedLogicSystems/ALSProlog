/*===================================================================================*
 |		wintest.pro
 |	Copyright (c) 1994 Applied Logic Systems, Inc.
 |
 |	Primitive Windows Test for MacOS and PP
 |
 |	Creates a Bulletin Board Widget with a few active children
 *===================================================================================*/

uses power_plant.

wintest :-
	pp_new_LApplication(App),

	c_const('windAttr_Regular', AttrRegular),
	c_const('windAttr_Enabled', AttrEnable),
	Attr is AttrRegular \/ AttrEnable,
	pp_new_LWindow(1, Attr, App, Window),
	
	c_create_abs(str, 'MacOS PP Windowing Test - ALS Prolog', WindowTitle),
	cstr2pstr(WindowTitle),
	pp_SetDescriptor(Window, WindowTitle),
	
	pp_Disable(Window),
	
	createButton('Quit', Window, [5, 5], Button),
	
	
	pp_Enable(Window),
	pp_Show(Window),
	
	pp_Run(App).
	
createButton(Name, SuperView, [X, Y], Button) :-
	
	c_const('pushButProc', ButtonProc),
	c_create_abs(str, Name, Title),
	cstr2pstr(Title),
	cpp_object_cast(SuperView, 'LView', SV),
	c_create_abs('SPaneInfo', [paneID, 0, width, 70, height, 20, visible, 1,
				  enabled, 1, left, X, top, Y, userCon, 0, superView, SV], PaneInfo),
	%%c_examine(PaneInfo, 'SPaneInfo', ['bindings', Bindings]),
	%%c_set(Bindings, 'SBooleanRect', [left, 0, top, 0, right, 0, bottom, 0]),
	
	pp_new_LStdButton(PaneInfo, 1, 0, Title, Button),
	pp_Enable(Button),
	
	c_create_abs(str, 'pushButtonCB', CallBackTerm),
	pp_new_CPrologListener(CallBackTerm, ButtonListener),
	pp_AddListener(Button, ButtonListener).
	

pushButtonCB(Message, IOParam) :- write('I''ve been pressed!'), nl.

/*
wintest :-
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
