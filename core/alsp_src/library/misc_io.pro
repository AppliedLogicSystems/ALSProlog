/*=====================================================================
 * 			misc_io.pro		
 *		Copyright (c) 1990 Applied Logic Systems, Inc.
 *
 *		Various I/O utilities for the library
 *====================================================================*/

module builtins.

%export copyFiles/2.
%export copy_file/2.
export readline/1.
export readline/2.
export read_terms/1.
export read_terms/2.
export writeListNl/1.
export colwrite/4.
export putc_n_of/3.
export gen_file_header/3.
export gen_file_header/4.

/**************************************************
/*!---------------------------------------------------------------
 |	copyFiles/2
 |	copyFiles(SourceFilesList, TargetSubDirPath)
 |	copyFiles(+, +)
 |	
 |	- copies files to a directory	
 |	
 |	If SourceFilesList is a list of file names, and TargetSubDirPath
 |	is either an atom or an internal form naming a directory,
 |	copies all of the indicated files to files with the same 
 |	names in TargetSubDirPath.
 *!--------------------------------------------------------------*/
%%% Needs to be changed to just recursively call copy_file/2.
%%% Also, change to use filepath.pro routines for building the file names
copyFiles(SourceFilesList, TargetSubDirPath)
	:-
	name(TargetSubDirPath, TargetSubDirPathChars),
	copyFiles0(SourceFilesList, TargetSubDirPathChars).

copyFiles0([], _).
copyFiles0([File | SourceFilesList], TargetSubDirPathChars)
	:-
	name(File, FileChars),
	append(TargetSubDirPathChars," > bucket.bit",Tmp1),
	append(FileChars, [0' | Tmp1], Tmp2),
	append("copy ",Tmp2,CmdChars),
	system(CmdChars),
	!,
	copyFiles0(SourceFilesList, TargetSubDirPathChars).

%%% Needs to be changed under Merge3 (new io) to open the
%%% source & sink, and just pass the (C) file descriptors
%%% to a little C routine which just copies chars over until
%%% eof is reached.

/*!---------------------------------------------------------------
 |	copy_file/2
 |	copy_file(SourceFile,TargetFile)
 |	copy_file(+,+)
 |
 |	-	copies one file to another
 |
 |	If SourceFile and TargetFile are names of files, copies
 |	contents of SourceFile to TargetFile, overwriting the latter.
 *!--------------------------------------------------------------*/
copy_file(SourceFile,TargetFile)
	 :-
	 see(SourceFile),
	 tell(TargetFile),
	 pump_chars,
	 !,
	 told, seen.
copy_file(SourceFile,TargetFile)
	 :-
	 told, seen.

pump_chars
	 :-
	 get0(C),
	 disp_pump_chars(C).

disp_pump_chars(C)
	 :-
	 eof_char(C),!.
disp_pump_chars(C)
	 :-
	 put(C),
	 pump_chars.

eof_char(-1).
eoln_char(13).
eoln_char(10).

/*!---------------------------------------------------------------
 |	readline/1
 |	readline(Result)
 |	readline(-)
 |
 |	-	reads a line of characters
 |
 |	Reads a line of characters from the current input; if end
 |	of file is encountered, the "list tail" for the list of
 |	characters is end_of_file.
 *!--------------------------------------------------------------*/
readline(Result)
	 :-
	readline(Result,0).

/*!---------------------------------------------------------------
 |	readline/2
 |	readline(Result,CharConv)
 |	readline(-,+)
 |
 |	-	reads a line of character, with case conversion
 |
 |	Reads a line of characters from the current input, converting
 |	the case of alphabetic characters according to the following:
 |		1	- 	convert lower to upper
 |		0	-	no conversion
 |		-1	-	convert upper to lower
 |	If end of file is encountered, the "list tail" for the list of
 |	characters is end_of_file.
 *!--------------------------------------------------------------*/
readline(Result,CharConv)
	 :-
	dmember(CharConv, [0,1,-1]),
	 get0(C),
	 (eof(C) ->
	     Result = end_of_file;
	     disp_readline_tail(C, Result, CharConv)
	 ).

readline_tail(CurTail, CharConv)
	 :-
	 get0(C),
	 disp_readline_tail(C, CurTail, CharConv).

disp_readline_tail(C, CurTail, CharConv)
	 :-
	 eof_char(C),!, CurTail = end_of_file.

disp_readline_tail(C, CurTail, CharConv)
	 :-
	 eoln_char(C),!, CurTail = [].

disp_readline_tail(C, [TheC | NextTail], CharConv)
	 :-
	conv_case(CharConv,C,TheC),
	 readline_tail(NextTail, CharConv).

conv_case(-1,C,TheC)
	:-!,
	(0'A =< C, C =< 0'Z  ->
		TheC is C+32;
		TheC = C
	).
conv_case(1,C,TheC)
	:-!,
	(0'a =< C, C =< 0'z  ->
		TheC is C-32;
		TheC = C
	).
conv_case(_,C,C).
**************************************************/

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

/*************************************************
/*!-------------------------------------------------------------
 |	writeListNl/1
 |	writeListNl(Term_List)
 |	writeListNl(+)
 |
 |	-	writes out a list, one element per line
 |
 |	If Term_List is a list of Prolog terms, writes out Term_List
 |	to the default output stream, placing one term on each line.
 *!------------------------------------------------------------*/

writeListNl([]).
writeListNl([Term | RestTerm_List])
	:-
	write(Term), nl,
 	writeListNl(RestTerm_List).
***************************************/


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
