/*=====================================================================
 | 			misc_io.pro		
 |	Copyright (c) 1990-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		Various I/O utilities 
 *====================================================================*/
module builtins.

export read_terms/1.
export read_terms/2.
export read_terms_pos/1.
export read_terms_pos/2.
export read_terms_vn/2.
export read_as_list/3.
export read_lines/2.
export colwrite/4.
export putc_n_of/3.
export gen_file_header/3.
export gen_file_header/4.

/*!-------------------------------------------------------------
 |	read_terms/1
 |	read_terms(Term_List)
 |	read_terms(-)
 |
 |	-	reads a list of Prolog terms from the default input stream
 |
 |	Reads a list (Term_List)  of all terms which can be read from
 |	the default input stream
 *!------------------------------------------------------------*/

read_terms(Term_List)
	:-
	current_default_stream(input, Stream),
	read_terms(Term_List, Stream).

/*!-------------------------------------------------------------
 |	read_terms/2
 |	read_terms(Stream,Term_List)
 |	read_terms(+,-)
 |
 |	-	reads a list of Prolog terms from stream Stream
 |
 |	Reads a list (Term_List)  of all terms which can be read from
 |	the stream Stream.
 *!------------------------------------------------------------*/

read_terms(Stream,Term_List)
	:-
	read_terms0(Term_List, Term_List,Stream).

read_terms0(Term_List, List_Tail,Stream)
	:-
	read(Stream, Next_Term),
	!,
	dispatch_read_terms0(Next_Term, Term_List, List_Tail,Stream).

dispatch_read_terms0(Next_Term, Term_List, List_Tail,Stream)
	:-
	var(Next_Term),!,
	List_Tail = [Next_Term | New_List_Tail],
	!,
	read_terms0(Term_List, New_List_Tail,Stream).
dispatch_read_terms0(end_of_file, _, [],_) :-!.
dispatch_read_terms0(Next_Term, Term_List, List_Tail,Stream)
	:-
	List_Tail = [Next_Term | New_List_Tail],
	!,
	read_terms0(Term_List, New_List_Tail,Stream).

/*!-------------------------------------------------------------
 |	read_terms_pos/1
 |	read_terms_pos(Term_List)
 |	read_terms_pos(-)
 |
 |	-	reads list of terms, with positions, from default input stream
 |
 |	Reads a list (Term_List)  of all terms which can be read from
 |	the default input stream, with their positions;
 *!------------------------------------------------------------*/

read_terms_pos(Term_List)
	:-
	current_default_stream(input, Stream),
	read_terms_pos(Term_List, Stream).

/*!-------------------------------------------------------------
 |	read_terms_pos/2
 |	read_terms_pos(Stream,Term_List)
 |	read_terms_pos(+,-)
 |
 |	-	reads list of terms, with positions, from stream Stream
 |
 |	Reads a list (Term_List)  of all terms, with positions  which 
 |	can be read from the stream Stream; the elements of Term_List
 |	are of the form (Term, Start, End), where:
 |	- Start is the offset in Stream of the first character of Term;
 |		if the offset cannot be meaninfully calculated, Start = -1;
 |	- End is the offset of the first character position following
 |		Term in Stream;
 *!------------------------------------------------------------*/

read_terms_pos(Stream,Term_List)
	:-
	sio:skip_layout(Stream),
	sio_getpos(Stream,StartPos),
	read_term(Stream, Next_Term, []),
	sio_getpos(Stream,LastPos),
	dispatch_read_terms_pos(Next_Term, StartPos, LastPos, Term_List, Stream).

dispatch_read_terms_pos(Next_Term, StartPos, LastPos, Term_List,Stream)
	:-
	var(Next_Term),
	!,
	Term_List = [(Next_Term,StartPos,LastPos) | List_Tail],
	read_terms_pos(Stream, List_Tail).

dispatch_read_terms_pos(end_of_file, _, _, [],_) :-!.

dispatch_read_terms_pos(Next_Term, StartPos, LastPos, Term_List, Stream)
	:-
	Term_List = [(Next_Term,StartPos,LastPos) | List_Tail],
	!,
	read_terms_pos(Stream, List_Tail).


/*!-------------------------------------------------------------
 |	read_terms_vn/2
 |	read_terms_vn(Stream,Term_List)
 |	read_terms_vn(+,-)
 |
 |	-	reads list of terms from stream Stream, with vars instatiated
 |
 |	Reads a list (Term_List)  of all terms which can be read from
 |	the stream Stream, with all the variables in each term instatiated
 |	to their names.
 *!------------------------------------------------------------*/

read_terms_vn(Stream,Term_List)
	:-
	read_terms_vn0(Term_List, Term_List,Stream).

read_terms_vn0(Term_List, List_Tail,Stream)
	:-
	read_term(Stream, Next_Term, [ vars_and_names(Vars,Names) ]),
	Vars = Names,
	!,
	dispatch_read_terms_vn0(Next_Term, Term_List, List_Tail,Stream).

dispatch_read_terms_vn0(Next_Term, Term_List, List_Tail,Stream)
	:-
	var(Next_Term),!,
	List_Tail = [Next_Term | New_List_Tail],
	!,
	read_terms_vn0(Term_List, New_List_Tail,Stream).

dispatch_read_terms_vn0(end_of_file, _, [],_) :-!.

dispatch_read_terms_vn0(Next_Term, Term_List, List_Tail,Stream)
	:-
	List_Tail = [Next_Term | New_List_Tail],
	!,
	read_terms_vn0(Term_List, New_List_Tail,Stream).


/*!-------------------------------------------------------------
 |	read_lines/2
 |	read_lines(Stream, Lines)
 |	read_lines(+, -)
 |
 |	- read lines from a stream
 |
 |	Reads list of Lines (as atoms) from Stream until end of 
 |	Stream is encountered.
 *!------------------------------------------------------------*/

read_lines(Stream, [Line | Lines])
	:-
	get_line(Stream, Line),
	!,
	read_lines(Stream, Lines).

read_lines(Stream, []).

/*!-------------------------------------------------------------
 |	read_as_list/3
 |	read_as_list(Atom,List,Options)	
 |	read_as_list(+,-,+)
 |
 |	- process an input atom into a list, if possible	
 |
 |	Atom is a an atom, normally quoted; Options is a list of
 |	equations; currently the only supported element is
 |		delimiter=Cd,
 |	where Cd is the code of a character C.  If Atom actually appears
 |	as a sequence of readable Prolog terms separated by one or
 |	more occurrences of C, then List is the corresponding list
 |	of those terms.  Example:
 |		Atom = 'foo.bar zip.go  silly.jam'
 |		Options = [delimiter = 0' ],
 |		List = ['foo.bar','zip.go','silly.jam']
 *!------------------------------------------------------------*/

read_as_list(FLine,Fs,Options)
	:-
	atom_length(FLine,FLen),
	(dmember(delimiter=DelimCode, Options) -> true ; DelimCode = 0' ),
	atom_codes(Delim,[DelimCode]),
	read_as_list(1,FLen,FLine,Delim,Fs,Options).

read_as_list(Cur,Size,Source,Delim,[],Options)
	:-
	Cur > Size,
	!.

read_as_list(Cur,Size,Source,Delim,[Next | Rest],Options)
	:-
	read_item(Cur,Size,Source,Delim,Next,After),
	AfterAnother is After + 1,
	read_as_list(AfterAnother,Size,Source,Delim,Rest,Options).
	
read_item(Cur,Size,Source,Delim,Next,After)
	:-
	probe_delim(Cur,Size,Source,Delim,After),
	ALen is After - Cur,
	sub_atom(Source,Cur,ALen,Next).

probe_delim(Cur,Size,Source,Delim,Cur)
	:-
	Cur > Size,		%% really: Cur = Size + 1
	!.
probe_delim(Cur,Size,Source,Delim,Cur)
	:-
	sub_atom(Source,Cur,1,Delim),
	!.

probe_delim(Cur,Size,Source,Delim,After)
	:-
	NPos is Cur + 1,
	probe_delim(NPos,Size,Source,Delim,After).


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

putc_n_of(0, Char, Stream).
putc_n_of(Num, Char, Stream)
    :-
    put_char(Stream,Char),
    NextNum is Num-1,
    putc_n_of(NextNum, Char, Stream).

/*!-------------------------------------------------------------
 |	gen_file_header/[3,4]
 |	gen_file_header(OutStream,SourceFile,TargetFile)
 |	gen_file_header(OutStream,SourceFile,TargetFile,ExtraCall)
 |	gen_file_header(+,+,+)
 |	gen_file_header(+,+,+,+)
 |
 |	- output a header suitable for a generated file
 |
 |	OutStream is a write stream, normally to file TargetFile; 
 |	gen_file_header/3 outputs a header of the following format
 |	on OutStream:
 |
 |	/*================================================================
 |	          fooout
 |	          --Generated from: fooin
 |	          Date: 94/4/17   Time: 9:52:53
 |	 *===============================================================*/
 |
 |	In gen_file_header/4, the argument ExtraCall is called just before
 |	the printing of the lower comment line.  Thus, if ExtraCall were
 |
 |			printf(user_output,'          -- by zipper_foo\n',[]),
 |
 |	the output would look like:
 |	
 |	/*================================================================
 |	          fooout
 |	          --Generated from: fooin
 |	          Date: 94/4/17   Time: 9:52:53
 |	          -- by zipper_foo
 |	 *===============================================================*/
 |	
 *!------------------------------------------------------------*/

gen_file_header(OutStrm, SrcFile, TgtFile)
	:-
	gen_file_header(OutStrm, SrcFile, TgtFile, true).

gen_file_header(OutStrm, SrcFile, TgtFile, Extra)
	:-
	comment_start(CS),
	comment_end(CE),

	printf(OutStrm,'%t\n',[CS]),
	printf(OutStrm,'          %t\n',[TgtFile]),
	printf(OutStrm,'          --Generated from: %t\n',[SrcFile]),
	date(Date), time(Time),
	printf(OutStrm,'          Date: %t   Time: %t\n',[Date,Time]),
	(nonvar(Extra) ->
		(call(Extra) -> true ; true)
		; true ),
	printf(OutStrm,'%t\n\n',[CE]).


comment_start(
	'/*================================================================'
		).

comment_end(
	' *===============================================================*/'
		).

endmod.
