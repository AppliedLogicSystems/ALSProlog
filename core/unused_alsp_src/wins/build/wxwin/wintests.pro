wintest :- x_StartwxWindows.

wx_ALSApp_OnInit(Frame) :-
	c_create_abs(str, 'wxWindows Windowing Test - ALS Prolog', FrameTitle),
	c_const('wxDEFAULT_FRAME', FrameType),
	c_create_abs(str, 'frame', FrameName),
	x_wxcFrameCreate(0, FrameTitle, 50, 50, 400, 300, FrameType, FrameName, Frame),

	x_wxcMenuCreate(0, 0, FileMenu),

	c_create_abs(str, '&Quit', QuitLabel),
  	x_wxcMenuAppend(FileMenu, 1, QuitLabel, -1, 0, 0, _),
	x_wxcMenuBarCreate(MenuBar),
	c_create_abs(str, 'File', FileLabel),
	x_wxcMenuBarAppend(MenuBar, FileMenu, FileLabel, _),
	x_wxcFrameSetMenuBar(Frame, MenuBar, _),


	x_wxcPanelCreate(Frame, 0, 0, 700, 700, 0, 0, Panel),
	
	c_create_abs(str, 'Quit', ButtonLabel),
	c_create_abs(str, 'button', ButtonName),
	x_wxcButtonCreate(Panel, ButtonLabel, -1, -1, -1, -1, 0, ButtonName, QuitButton),
	%%x_wxAssociateUserData(QuitButton, 1, _),
	
	x_wxcPanelNewLine(Panel, _),
	
	c_create_abs(str, 'Radio 1', Radio1),
	c_create_abs(str, 'Radio 2', Radio2),
	c_createn_abs(ptr, 2, [Radio1, Radio2], RadioStrings),
	
	c_create_abs(str, 'Radio Box', RadioBoxTitle),
	c_const('wxFLAT', Flat),
	c_const('wxBORDER', Border),
	Options is Flat \/ Border, 
	
	x_wxcRadioBoxCreate(Panel, RadioBoxTitle,
			-1, -1, -1, -1,
			2, RadioStrings, 1, Options, 0, RadioButtons),
	
	x_wxcPanelNewLine(Panel, _),

	c_create_abs(str, 'Label', EditLabel),
	c_create_abs(str, 'Text', InitialText),
	c_const('wxBORDER', Style),

	x_wxcTextCreate(Panel, EditLabel, InitialText, -1, -1, 200, -1, Style, 0, EditText),
	
	x_wxcWindowShow(Frame, 1, _).
	

wx_Button_OnCommand :-
	write('A button was pressed!'), nl.

wx_Frame_OnMenuCommand(Frame, 1) :-
	write('Quiting...'), nl,
	x_wxcObjectDelete(Frame, _).
	

