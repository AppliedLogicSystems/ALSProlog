

module windows.

export menu/3.
export menu/4.

:-dynamic(current_ws/1).

menu(WinName, ChoiceList, Choice)
	:-
	menu(WinName, ChoiceList, Choice, []).

menu(WinName, ChoiceList, Choice, Options)
	:-
	windows:current_ws(WinSys),
	!,
	win_menu(WinSys, WinName, ChoiceList, Choice, Options).
	
menu(_, ChoiceList, Choice, Options)
	:-
	simple_menu(ChoiceList, Choice, [0-'No Choice - Exit menu' | Options]).

export chooseFile/3.
chooseFile(WinName,Pattern,File)
	:-
	files(Pattern,FileList),
	menu(WinName,FileList,File).

%%% Note: simple_menu/3 defined in simplio.pro in Library
endmod.
