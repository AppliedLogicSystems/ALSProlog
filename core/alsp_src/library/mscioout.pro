/*=====================================================================
 | 			mscioout.pro		
 |	Copyright (c) 1990-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		Various I/O utilities: output 
 *====================================================================*/
module builtins.

export colwrite/4.
export putc_n_of/3.

export write_lines/1.
export write_lines/2.
export write_lines_opt/2.
export write_lines_opt/3.
export write_lines_nl/3.
export write_lines_nl/4.
export write_clause/1.
export write_clause/2.
export write_clause/3.
export write_clauses/1.
export write_clauses/2.
export write_clauses/3.

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
		'$strlen'(Item, ItemLen)
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
	write_lines(TgtStream, List).

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
	builtins:sys_env(OS,_,_),
	write_lines_nl(List, TgtStream, OS,[]).

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
	builtins:sys_env(OS,_,_),
	write_lines_nl(List, TgtStream, OS, Opts).

/*!-------------------------------------------------------------
 |	write_lines_opt/3
 |	write_lines_opt(TgtStream, List, Opts)
 |	write_lines_opt(+, +, +)
 |
 |	- write a list of lines to a stream, with options
 |
 *!------------------------------------------------------------*/
write_lines_opt(TgtStream, List, Opts)
	:-
	builtins:sys_env(OS,_,_),
	write_lines_nl(List, TgtStream, OS, Opts).

/*!-------------------------------------------------------------
 |	write_lines_nl/3
 |	write_lines_nl(Lines, TgtS, NL_type)
 |	write_lines_nl(+, +, +)
 |
 |	- write a list of lines to a stream, with nl type specified
 |
 *!------------------------------------------------------------*/
write_lines_nl(Lines, TgtS, NL_type)
	:-
	write_lines_nl(Lines, TgtS, NL_type, []).

/*!-------------------------------------------------------------
 |	write_lines_nl/4
 |	write_lines_nl(Lines, TgtS, NL_type, Options)
 |	write_lines_nl(+, +, +, +)
 |
 |	- write a list of lines to a stream, with options, incl nl
 |
 *!------------------------------------------------------------*/
write_lines_nl([], _, _, _).

write_lines_nl([Line | Lines], TgtS, NL_type, Options)
	:-
	write_term(TgtS, Line, Options),
	output_nl(NL_type, TgtS),
	write_lines_nl(Lines, TgtS, NL_type, Options).

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

endmod.
