/*==================================================================
 *            simplio.pro
 *        Copyright (c) 1991  Applied Logic Systems, Inc.
 *
 *	Elementary TTY-style IO for default user interface
 *
 * Author:	Ken Bowen
 * Date:	November, 1991
 *=================================================================*/

module windows.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% query_user
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export query_user/3.
query_user(FmtString,Args,Answer)
	:-
	sio:get_current_output_stream(OutStream),
	sio:get_current_input_stream(InStream),
	query_user(InStream,OutStream,FmtString,Args,Answer).

export query_user/5.
query_user(InStream,OutStream,FmtString,Args,Answer)
	:-
	catenate(FmtString,' (yes/no)?',XFmtString),
	printf(OutStream,XFmtString,Args),
	get_line(InStream,Line),
	atomread(Line,InitAnswer),
	q_acton(InitAnswer,Answer).

q_acton(yes,yes).
q_acton(y,yes).
q_acton(_,no).


export simple_menu/3.

simple_menu([Choice], Choice, Options) :-!.
simple_menu(List, Choice, Options) 
	:-
	dmember(codes=ListOfCodes,Options),
	!,
	encode_list(List, ListOfCodes, CodedList),
	themenu(CodedList, ChoiceNum, Options),
	fin_simple_menu_num(ChoiceNum, CodedList, Options, Choice).

simple_menu(List, Choice, Options) 
	:-
	dmember(0-ZeroChoice, Options),
	!,
	number_list([ZeroChoice | List], 0, NumberedList),
	themenu(NumberedList, ChoiceNum, Options),
	fin_simple_menu_num(ChoiceNum, NumberedList, Options, Choice).

simple_menu(List, Choice, Options) 
	:-
	number_list(List, 1, NumberedList),
	themenu(NumberedList, ChoiceNum, Options),
	fin_simple_menu_num(ChoiceNum, NumberedList, Options, Choice).

fin_simple_menu_num(default, NumberedList, Options, Choice)
	:-
	dmember(default=Choice,Options),
	!.		
fin_simple_menu_num(default, [_-Choice | NumberedList], Options, Choice)
	:-!.		

fin_simple_menu_num(0, NumberedList, Options, '$noChoice') :-!.
fin_simple_menu_num(ChoiceNum, NumberedList, Options, Choice)
	:-
	dmember(ChoiceNum-Choice, NumberedList),!.
fin_simple_menu_num(ChoiceNum, NumberedList, Options, '$badInput$'(ChoiceNum)).

themenu(List, ChoiceNum, Options)
	:-
	(dmember(default=DefaultContent,Options) ->
		true
		;
		DefaultContent = ''
	),
	(dmember(indent=Indent, Options) ->
		true
		;
		Indent = '    '
	),
	(dmember(terminator=Termin, Options) ->
		true
		;
		Termin = ''
	),
	(dmember(spacer=Spacer, Options) ->
		true
		;
		Spacer = ' - '
	),
	(dmember(title=Title, Options) ->
		printf("%t%t\n%t------------------------\n",[Indent,Title,Indent])
		;
		true
	),
	output_prolog_list(List,Indent,Termin,Spacer,DefaultContent),
	(dmember(prompt=Prompt, Options) ->
		true
		;
		Prompt = ' Choice = '
	),
	write(Prompt),
	get_line(Line),
	((Line = ''; Line = end_of_line) ->
		ChoiceNum = default
		;
		atomread(Line,ChoiceNum,[vars_and_names(Vars,Names)]),
		Vars = Names
	).


/*
export writeWindow/2.
writeWindow(_,Item)
	:-
	write(Item).

export writenwWindow/2.
writenwWindow(_,Item)
	:-
	write(Item).

export writeListWindow/2.
export writeListWindow/3.
writeListWindow(_,List,_)
	:-
	writeListWindow(_,List).
writeListWindow(_,List)
	:-
	writeLine(List).

export writeLine/1.
writeLine([]).
writeLine([Item | Items])
	:-
	writeLineItem(Item),
	writeLine(Items).

writeLineItem(nl)
	:-!,
	nl.
writeLineItem('$at'(Item,_))
	:-!,
	writeLineItem(Item).
writeLineItem(Item)
	:-
	write(Item).

export get_keysym_or_mouseclick/2.
get_keysym_or_mouseclick(_,_)
	:-
	get0(_).

export readGroundWindow/2.
readGroundWindow(_,InterfMod)
	:-
	readvnv(InterfMod,NameList,VarList),
	VarList = NameList.

export clearWindow/1.
clearWindow(_).
*/

endmod.
