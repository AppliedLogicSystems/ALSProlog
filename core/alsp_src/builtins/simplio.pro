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
	query_user(InStream,OutStream,FmtString,Args,[],Answer).

export query_user/4.
query_user(FmtString,Args,Options,Answer)
	:-
	sio:get_current_output_stream(OutStream),
	sio:get_current_input_stream(InStream),
	query_user(InStream,OutStream,FmtString,Args,Options,Answer).

export query_user/5.
query_user(InStream,OutStream,FmtString,Args,Answer)
	:-
	query_user(InStream,OutStream,FmtString,Args,[],Answer).

export query_user/6.
query_user(InStream,OutStream,FmtString,Args,Options,Answer)
	:-
	dmember(io_config=ansi_screen,Options),
	!,
	(dmember(yes_no, Options) ->
		catenate(FmtString,' (y/n):',XFmtString)
		;
		XFmtString = FmtString
	),
	(dmember(screen_locn=dca(N,M),Options) -> true ; N=1,M=1),
	dca(N,M),
	printf(OutStream,XFmtString,Args),
	flush_output(OutStream),
	get_line(InStream,Line),
	atomread(Line,InitAnswer),
	!,
	(dmember(yes_no, Options) ->
		q_acton(InitAnswer,Answer)
		;
		(InitAnswer = end_of_file ->
			(dmember(default=Answer, Options) -> true ; Answer = '')
			;
			Answer = InitAnswer
		)
	).

query_user(InStream,OutStream,FmtString,Args,Options,Answer)
	:-
	catenate(FmtString,' (y/n):',XFmtString),
	printf(OutStream,XFmtString,Args),
	get_line(InStream,Line),
	atomread(Line,InitAnswer),
	!,
	q_acton(InitAnswer,Answer).


q_acton(yes,yes) :-!.
q_acton(y,yes) :-!.
q_acton(end_of_file,yes) :-!.
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
	(dmember(return_code=true, Options) ->
		fin_simple_menu_codes(ChoiceNum, CodedList, Options, Choice)
		;
		fin_simple_menu_num(ChoiceNum, CodedList, Options, Choice)
	).

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

fin_simple_menu_codes(default, CodedList, Options, Choice)
	:-
	dmember(default=DefaultContent,Options),
	dmember(Choice-DefaultContent,CodedList),
	!.		
fin_simple_menu_codes(default, [_-Choice | CodedList], Options, Choice)
	:-!.

fin_simple_menu_codes(Choice, CodedList, Options, Choice).

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
	(dmember(dflt_mark=DfltMark, Options) ->
		true
		;
		DfltMark = ' *'
	),

	output_prolog_list(List,OutS,Indent,Termin,Spacer,DefaultContent,DfltMark),

	(dmember(prompt=Prompt, Options) ->
		true
		;
		Prompt = ' Choice = '
	),
	write(OutS,Prompt),
	flush_output(OutS),
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
export output_prolog_list/6.
export output_prolog_list/7.

output_prolog_list(List)
	:-
	current_output(Stream),
	output_prolog_list(List,Stream,'','.',' - ','',' *').

output_prolog_list(List,Indent,Term,Spacer,DefaultContent,DfltMark)
	:-
	current_output(Stream),
	output_prolog_list(List,Stream,Indent,Term,Spacer,DefaultContent,DfltMark).

output_prolog_list([],Stream,Indent,Term,Spacer,DefaultContent,DfltMark).
output_prolog_list([Item | RestList],Stream,Indent,Term,Spacer,DefaultContent,DfltMark)
	:-
	(Item = (Code-Content) ->
		printf(Stream,"%t%t%t%t%t",[Indent,Code,Spacer,Content,Term]),
		(Content=DefaultContent -> write(Stream,DfltMark);true)
		;
		printf(Stream,"%t%t%t",[Indent,Item,Term]),
		(Iterm=DefaultContent -> write(Stream,DfltMark);true)
	),
	nl(Stream),flush_output(Stream),
	output_prolog_list(RestList,Stream,Indent,Term,Spacer,DefaultContent,DfltMark).


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


export dca/2.
dca(X,Y)
	:-
	put_code(user_output,27),		%% escape
	put_code(user_output,0'[),
	write_term(user_output,X,[]),
	put_code(user_output,0';),
	write_term(user_output,Y,[]),
	put_code(user_output,0'H).

endmod.
