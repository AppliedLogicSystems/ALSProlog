/*==================================================================
 |            simplio.pro
 |     Copyright (c) 1991-96  Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |	Elementary TTY-style IO for default user interface
 |
 | Author:	Ken Bowen
 | Date:	November, 1991
 *=================================================================*/

module builtins.

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
simple_menu(List, Choice, Options) 
	:-
	sio:get_current_output_stream(OutS),
	sio:get_current_input_stream(InS),
	simple_menu(List, Choice, Options, InS, OutS).

export simple_menu/5.
simple_menu([Choice], Choice, Options,InS,OutS) :-!.
simple_menu(List, Choice, Options,InS,OutS) 
	:-
	dmember(codes=ListOfCodes,Options),
	!,
	encode_list(List, ListOfCodes, CodedList),
	themenu(CodedList, ChoiceNum, Options,InS,OutS),
	fin_simple_menu_num(ChoiceNum, CodedList, Options, Choice).

simple_menu(List, Choice, Options,InS,OutS) 
	:-
	dmember(0-ZeroChoice, Options),
	!,
	number_list([ZeroChoice | List], 0, NumberedList),
	themenu(NumberedList, ChoiceNum, Options,InS,OutS),
	fin_simple_menu_num(ChoiceNum, NumberedList, Options, Choice).

simple_menu(List, Choice, Options,InS,OutS) 
	:-
	number_list(List, 1, NumberedList),
	themenu(NumberedList, ChoiceNum, Options,InS,OutS),
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

themenu(List, ChoiceNum, Options,InS,OutS)
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
		printf(OutS,'%t%t\n%t------------------------\n',[Indent,Title,Indent])
		;
		true
	),
	output_prolog_list(List,OutS,Indent,Termin,Spacer,DefaultContent),
	(dmember(prompt=Prompt, Options) ->
		true
		;
		Prompt = ' Choice = '
	),
	write(OutS,Prompt),
	get_line(InS,Line),
	((Line = ''; Line = end_of_line) ->
		ChoiceNum = default
		;
		atomread(Line,ChoiceNum,[vars_and_names(Vars,Names)]),
		Vars = Names
	).

/*!---------------------------------------------------------------------
 |	output_prolog_list/[1,5,6]
 |	output_prolog_list(List)
 |	output_prolog_list(+)
 |
 |	- outputs items on a list, one to a line
 |
 |	Outputs (to the current output stream) each item on List, one item
 |	to a line, followed by a period.
 *!--------------------------------------------------------------------*/
export output_prolog_list/1.
output_prolog_list(List)
	:-
	current_output(Stream),
	output_prolog_list(Stream, List,'','.',' - ','').

export output_prolog_list/5.
output_prolog_list(List,Indent,Term,Spacer,DefaultContent)
	:-
	current_output(Stream),
	output_prolog_list(List,Stream,Indent,Term,Spacer,DefaultContent).

export output_prolog_list/6.
output_prolog_list([],Stream,Indent,Term,Spacer,DefaultContent).
output_prolog_list([Item | RestList],Stream,Indent,Term,Spacer,DefaultContent)
	:-
	(Item = (Code-Content) ->
		printf(Stream,"%t%t%t%t%t",[Indent,Code,Spacer,Content,Term]),
		(Content=DefaultContent -> put_code(Stream,0'*);true)
		;
		printf(Stream,"%t%t%t",[Indent,Item,Term]),
		(Iterm=DefaultContent -> put_code(Stream,0'*);true)
	),
	nl(Stream),
	output_prolog_list(RestList,Stream,Indent,Term,Spacer,DefaultContent).


/*!---------------------------------------------------------------------
 |  nth/3
 |  nth(N, List, X)
 |  nth(+, +, -)
 |
 |   - returns the nth element of a list
 |
 |  If List is a list and N is a non-negative integer, then X is
 |  the nth element of List.
 *!--------------------------------------------------------------------*/
export nth/3.
nth(0, [X | _], X) :-!.
nth(M, [_ | T], X) 
	:-
	K is M-1,
	nth(K, T, X).

/*!---------------------------------------------------------------------
 |  position/3
 |  position(List, Item, N)
 |  position(+, +, -)
 |
 |  -   returns the position number of an item in a list
 |
 |  If List is a list and Item occurs in List, N is the number of
 |  the leftmost occurrence of Item in List; fails if Item does not
 |  occur in List.
 *!--------------------------------------------------------------------*/
export position/3.
position(List, Item, Nth)
	:-
	position(List, Item, 0, Nth).

/*!---------------------------------------------------------------------
 |  position/4
 |  position(List, Item, M, N)
 |  position(+, +, +, -)
 |
 |  -   returns the position number of an item in a list
 |
 |  If List is a list and Item occurs in List, N-M is the number of
 |  the leftmost occurrence of Item in List; fails if Item does not
 |  occur in List.
 *!--------------------------------------------------------------------*/
export position/4.
position([Item | _], Item, Nth, Nth) :-!.
position([_ | ListTail], Item, Current, Nth)
	:-
	Next is Current+1,
	position(ListTail, Item, Next, Nth).


endmod.
