/*=====================================================================
 | 			mscioout.pro		
 |	Copyright (c) 1990-96 Applied Logic Systems, Inc.
 |		Group: Input Output
 |		DocTitle: write_lines/1
 |		-- Various I/O utilities: output 
 *====================================================================*/
module builtins.

export colwrite/4.
export putc_n_of/3.

export write_lines/1.
export write_lines/2.
export write_lines/3.
export write_lines_opt/2.

export write_clause/1.
export write_clause/2.
export write_clause/3.
export write_clauses/1.
export write_clauses/2.
export write_clauses/3.

export write_lines_opt/3.
export write_lines_nl/3.
export write_lines_nl/4.

export printf_multiargs/3.

export codesweep/2.

/*!-------------------------------------------------------------
 |	colwrite/4
 |	colwrite(AtomList,ColPosList,CurPos,Stream)
 |	colwrite(+,+,+,+)
 |
 |	-	writes atoms in AtomList at column positions in ColPosList
 |
 |	If AtomList is a list of atoms (symbols or UIAs), and if 
 |	ColPosList is a list of  monotonically increasing positive
 |	integers of the same length as AtomList, and if CurPos is
 |	a positive integer (normally 1), and Stream is valid output
 |	stream (in text mode), this predicate outputs the items on
 |	AtomList to Stream, starting each element of AtomList at the
 |	postition indicated by the corresponding element of ColPosList.
 |	If a given item would overflow its column, it is truncated.
 |  Normally, CurPos = 1 and ColPosList begins with an integer
 |	greater than 1, so that the first column position is implicit.
 *!------------------------------------------------------------*/

colwrite([], ColStarts, CurPos, Stream).

colwrite([Item | Items], [NextColPos | RestColStarts], CurPos, Stream)
    :-
    CurPos < NextColPos,
    !,
	(atom(Item) -> 
		atom_length(Item, ItemLen)
		;
		name(Item,ICs),length(ICs,ItemLen)
	),
    ColWidth is NextColPos - CurPos + 1,
    (ItemLen =< ColWidth ->
        write(Stream,Item),
        NumBlanks is ColWidth - ItemLen,
        putc_n_of(NumBlanks, 0' , Stream)
        ;
        name(Item, ItemCs),
        at_most_n(ItemCs, ColWidth, TruncItemCs),
        name(TruncItem, TruncItemCs),
        write(Stream, TruncItem)
    ),
    colwrite(Items, RestColStarts, NextColPos, Stream).

colwrite([Item | Items], [NextColPos | RestColStarts], CurPos, Stream)
    :-
    colwrite(Items, RestColStarts, CurPos, Stream).

/*!-------------------------------------------------------------
 |	putc_n_of/3
 |	putc_n_of(Num, Char, Stream)
 |	putc_n_of(+,+,+)
 |
 |	-	output Num copies of the char with code Char to Stream
 |
 |	Num should be a positive integer, and Char should be the code
 |	of a valid character; Stream should be an output stream in
 |	text mode.  Outputs, to Stream,  Num copies of the characater
 |	with code Char.
 *!------------------------------------------------------------*/

putc_n_of(0, Char, Stream) :-!.
putc_n_of(Num, Char, Stream)
    :-
    put_code(Stream,Char),
    NextNum is Num-1,
    putc_n_of(NextNum, Char, Stream).

/*!-------------------------------------------------------------
 |	write_lines/1
 |	write_lines(List)
 |	write_lines(+)
 |
 |	- write a list of lines to the current output stream
 |
 *!------------------------------------------------------------*/
write_lines(List)
	:-
	sio:get_current_output_stream(TgtStream),
	write_lines(TgtStream, List, []).

/*!-------------------------------------------------------------
 |	write_lines/2
 |	write_lines(TgtStream, List)
 |	write_lines(+, +)
 |
 |	- write a list of lines to a stream
 |
 *!------------------------------------------------------------*/
write_lines(TgtStream, List)
	:-
	write_lines(TgtStream, List, []).

/*!-------------------------------------------------------------
 |	write_lines_opt/2
 |	write_lines_opt(List, Opts)
 |	write_lines_opt(+, +)
 |
 |	- write a list of lines to the current output stream
 |
 *!------------------------------------------------------------*/
write_lines_opt(List, Opts)
	:-
	sio:get_current_output_stream(TgtStream),
	write_lines(TgtStream, List, Opts).

/*!-------------------------------------------------------------
 |	write_lines/3
 |	write_lines(Lines, Stream, Options)
 |	write_lines(+, +, +)
 |
 |	- write a List of lines to a Stream, with Options
 |
 *!------------------------------------------------------------*/
write_lines(Stream, Lines, Options)
	:-
	(append(Left, [left_mar(AA) | Right], Options) ->
		append(Left, Right, Options0)
		;
		Options0 = Options, AA = ''
	),
	write_lines0(Lines, AA, Stream, Options0).

write_lines0([], _, _, _) :-!.

write_lines0([Line | Lines], AA, Stream, Options)
	:-!,
	put_atom(Stream, AA),
	write_term(Stream, Line, Options),
	nl(Stream),
	write_lines0(Lines, AA, Stream, Options).

write_lines0(Lines, AA, Stream, Options)
	:-
	put_atom(Stream, AA),
	atomic(Lines),
	put_atom(Stream, Lines),
	nl(Stream).

/*!-------------------------------------------------------------
 |	write_clause/1
 |	write_clause(Clauses) 
 |	write_clause(+) 
 |
 |	- write a clause to the current output stream
 |
 *!------------------------------------------------------------*/
write_clause(Clause) 
	:-
	sio:get_current_output_stream(Stream),
	write_clause(Stream, Clause).

/*!-------------------------------------------------------------
 |	write_clause/2
 |	write_clause(Stream, Clauses) 
 |	write_clause(+, +) 
 |
 |	- write a clause to a stream
 |
 *!------------------------------------------------------------*/
write_clause(Stream, Clause) 
	:-
	write_clause(Stream, Clause, []).

/*!-------------------------------------------------------------
 |	write_clause/3
 |	write_clause(Stream, Clauses, Options) 
 |	write_clause(+, +, +) 
 |
 |	- write a clause to a stream, with options
 |
 *!------------------------------------------------------------*/
write_clause(Stream, Clause, Options) 
	:-
	write_term(Stream,Clause, Options),
	put_code(Stream, 0'.),
	nl(Stream).

/*!-------------------------------------------------------------
 |	write_clauses/1
 |	write_clauses(Clauses) 
 |	write_clauses(+) 
 |
 |	- write list of clauses to the current output stream
 |
 *!------------------------------------------------------------*/
write_clauses(Clauses) :-
	sio:get_current_output_stream(Stream),
	write_clauses(Stream,Clauses).

/*!-------------------------------------------------------------
 |	write_clauses/2
 |	write_clauses(Stream, Clauses) 
 |	write_clauses(+, +) 
 |
 |	- write list of clauses to a stream
 |
 *!------------------------------------------------------------*/
write_clauses(Stream, Clauses) :-
	write_clauses(Stream,Clauses,[]).

/*!-------------------------------------------------------------
 |	write_clauses/3
 |	write_clauses(Stream, Clauses, Options) 
 |	write_clauses(+, +, +) 
 |
 |	- write list of clauses to a stream, with options
 |
 *!------------------------------------------------------------*/
write_clauses(Stream, Clauses, Options) 
	:-
	write_clauses0(Clauses, Stream, Options).

write_clauses0([], Stream, Options).

write_clauses0([nl | Clauses], Stream, Options) 
	:-!,
	nl(Stream),
	write_clauses0(Clauses, Stream, Options).

write_clauses0([Clause | Clauses], Stream, Options) 
	:-
	write_clause(Stream, Clause, Options),
	write_clauses0(Clauses, Stream, Options).

export write_items/1.
export write_items_nl/1.
export write_items/2.
export write_items/3.
export write_items/5.

write_items(List)
	:-
	write_items(List, ' ', none).

write_items_nl(List)
	:-
	write_items(List, '', nl).

write_items(List, Spacer)
	:-
	write_items(List, Spacer, none).

write_items(List, Spacer, EOL)
	:-
	sio:get_current_output_stream(Stream),
	write_items(List, Spacer, EOL, [], Stream).

write_items([], Spacer, EOL, Options, Stream).
write_items([nl | List], Spacer, nl, Options, Stream)
	:-!,
	nl(Stream),
	write_items(List, Spacer, EOL, Options, Stream).
write_items([nl | List], Spacer, EOL, Options, Stream)
	:-!,
	write_items(List, Spacer, EOL, Options, Stream).
write_items([nl(N) | List], Spacer, nl, Options, Stream)
	:-
	N > 0,
	!,
	nl(Stream),
	M is N-1,
	write_items([nl(M) | List], Spacer, nl, Options, Stream).
write_items([nl(_) | List], Spacer, EOL, Options, Stream)
	:-!,
	write_items(List, Spacer, EOL, Options, Stream).

write_items([Item | List], Spacer, EOL, Options, Stream)
	:-
	write_term(Stream, Item, Options),
	put_atom(Stream, Spacer),
	(EOL = nl -> nl(Stream) ; true),
	write_items(List, Spacer, EOL, Options, Stream).

export printf_multiargs/2.
export printf_multiargs/3.
export printf_multiargs/4.
printf_multiargs(ArgsList, Pattern)
	:-
	sio:get_current_output_stream(Stream),
	printf_multiargs(ArgsList, Pattern, [], Stream).

printf_multiargs(ArgsList, Pattern, Stream)
	:-
	printf_multiargs(ArgsList, Pattern, [], Stream).

printf_multiargs([], Pattern, Options, Stream).
printf_multiargs([Arg | ArgsList], Pattern, Options, Stream)
	:-
	printf(Stream, Pattern ,[Arg], Options),
	printf_multiargs(ArgsList, Pattern, Options, Stream).

export printf_list/1.
export printf_list/2.
export printf_list/3.
export printf_list_nl/2.
export printf_list_nl/3.
printf_list(PatternsAndArgs)
	:-
	sio:get_current_output_stream(Stream),
	printf_list(PatternsAndArgs, [], false, Stream).

printf_list(PatternsAndArgs, Stream)
	:-
	printf_list(PatternsAndArgs, [], false, Stream).

printf_list_nl(PatternsAndArgs, Stream)
	:-
	printf_list(PatternsAndArgs, [], true, Stream).

printf_list_nl(PatternsAndArgs, Options, Stream)
	:-
	printf_list(PatternsAndArgs, Options, true, Stream).

printf_list([], _, _, _).
printf_list([PatternPlusArgs | PatternsAndArgs], Options, NL, Stream)
	:-
	printf_list_patargs(PatternPlusArgs, Options, Stream),
	(NL=true -> nl(Stream) ; true),
	printf_list(PatternsAndArgs, Options, NL, Stream).

printf_list_patargs(Pattern+Args, Options, Stream)
	:-!,
	printf(Stream, Pattern, Args, Options).
printf_list_patargs(Pattern, Options, Stream)
	:-
	printf(Stream, Pattern, [], Options).

/*!-------------------------------------------------------------
 |	codesweep/2
 |	codesweep(CodeLineList, OS)
 |	codesweep(+, +)
 |
 |	Treats CodeLineList as a list of items to be written out
 |	to the stream OS, each atom followed by newline;  the
 |	elements of CodeLineList must be of the following forms:
 |
 |    -  the atom nl:   
 |             outputs two newlines;
 |
 |    -  any other atom A: 
 |             outputs A followed by a newline;
 |
 |    -  a term Expr + Args, where Args is a list:
 |             executes printf(OS, Expr, Args)
 *!------------------------------------------------------------*/

codesweep([], _).
codesweep([Item | CL], OS)
	:-
	codeout(Item, OS),
	codesweep(CL, OS).

codeout(nl, OS)
	:-!,
	nl(OS).

codeout(Item + Args, OS)
	:-!,
	printf(OS, Item, Args),
	nl(OS).

codeout(Item, OS)
	:-
	printf(OS, Item, []),
	nl(OS).

endmod.
