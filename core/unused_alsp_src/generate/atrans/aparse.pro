/*
 * aparse.pro		-- parsing phase for assembly language translator
 *	Copyright (c) 1990 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 4/1/90
 * Revision History:
 */

module parser.
    use tokenizer.
    use avl.
    use args.
    use instructions.
    use labels.
    use output.

    export read_file/1.

    read_file(FileName) :-
	newTable(Tab),
	avl_insert(ifdef(level),[],Tab,Tab1),
	output_begin(FileName),
	read_file(FileName,Tab1,OutTab),
	output_end(OutTab).

    read_file(FileName,InTab,OutTab) :-
	openFile(FileName,FD),
	avl_search(ifdef(level),Current,InTab),
	avl_insert(ifdef(level),[[]|Current],InTab,ITab),
	!,
	read_lines(FD,ITab,OutTab).
    read_file(FileName,Tab,Tab) :- !.
	%write('Error opening '),write(FileName),write(' for read access'),nl.
    
    read_lines(FD1,InTab,OutTab) :-
	getLine(Line,FD1,FD2),
	!,
	lineNum(FD1,LineNum),
	process_line(Line,LineNum,FD2,FD3,InTab,Tab1),
	read_lines(FD3,Tab1,OutTab).
    read_lines(FD,InTab,OutTab) :-	%% end-of-file
	avl_search(ifdef(level),[Level|Prev],InTab),
	avl_insert(ifdef(level),Prev,InTab,OutTab),
	lineNum(FD,LN),
	check_level(Level,LN).
    
    check_level([],_) :- !.
    check_level(_,LN) :- error('Unmatched #ifdef',LN).


    newTable(Table) :-
	avl_create(Table).
    
    process_line([],LN,FD,FD,Tab,Tab) :- !.
    process_line([pound, ident(Ident) | Rest ],LN,InFD,OutFD,InTab,OutTab) :-
	!,
	macro_statement(Ident,Rest,LN,InFD,OutFD,InTab,OutTab).
    process_line(Line, LN, FD,FD, InTab,OutTab) :-
	macro_expand(Line, First, Rest, [], LN, InTab),
	parse_lines([First|Rest], LN,InTab,OutTab).
    
    parse_lines([],_,Tab,Tab) :- !.
    parse_lines([First|Rest], LN,InTab,OutTab) :- 
	parse_line(First,LN,InTab,ITab),
	parse_lines(Rest,LN,ITab,OutTab).
    
    parse_line([],LN,Tab,Tab) :- !.	%% ignore empty lines
    parse_line(Line,LN,InTab,OutTab) :-
	parse_statement(Line,Statement,LN,InTab,ITab),
	output(Statement,LN,ITab,OutTab).
    



    /*
     * macro_statement(Token,Rest,LineNum,InFD,OutFD,InTab,OutTab)
     *
     *		Token	-- the token following the pound sign
     *		Rest	-- the rest of the line after Token
     *		LineNum	-- the line number for error reporting purposes
     *		InFD	-- the file stream descriptor (input)
     *		OutFD	-- the file stream descriptor returned
     *		InTab	-- the macro definition symbol table (input)
     *		OutTab	-- the macro definition symbol table to return
     */

    macro_statement(include,Rest,LN,FD,FD,InTab,OutTab) :- !,
	include(Rest,LN,InTab,OutTab).
    macro_statement(define,Rest,LN,FD,FD,InTab,OutTab) :- !,
	define(Rest,LN,InTab,OutTab).
    macro_statement(ifdef,Rest,LN,InFD,OutFD,InTab,OutTab) :- !,
	ifdef(Rest,LN,InFD,OutFD,InTab,OutTab).
    macro_statement(ifndef,Rest,LN,InFD,OutFD,InTab,OutTab) :- !,
	ifndef(Rest,LN,InFD,OutFD,InTab,OutTab).
    macro_statement(endif,_,LN,InFD,OutFD,InTab,OutTab) :- !,
	endif_or_else(endif,LN,InFD,OutFD,InTab,OutTab).
    macro_statement(else,_,LN,InFD,OutFD,InTab,OutTab) :- !,
	endif_or_else(else,LN,InFD,OutFD,InTab,OutTab).
    macro_statement(macro,Rest,LN,InFD,OutFD,InTab,OutTab) :- !,
	macro(Rest,LN,InFD,OutFD,InTab,OutTab).
    macro_statement(undef,Rest,LN,FD,FD,InTab,OutTab) :- !,
	undef(Rest,LN,InTab,OutTab).
    macro_statement(endm,Rest,LN,FD,FD,Tab,Tab) :- !,
	error('Missing #macro statement',LN).
    macro_statement(Tok,Rest,LN,FD,FD,Tab,Tab) :- !,
	error('Unknown directive:',Tok,LN).
    
    /*
     * include/4	-- handle #includes
     */
    
    include([string(FileName)],LN,InTab,OutTab) :-
	find_include_file(FileName,IncFileName),
	!,
	read_file(IncFileName,InTab,OutTab).
    include(Whatever,LN,Tab,Tab) :-
	error('Bad #include',LN).

    :- dynamic(incdir/1).

    find_include_file(FileName, FileName) :-
    	exists(FileName),
	!.
    find_include_file(FileName, IncFileName) :-
	incdir(Path),
	pathPlusFile(Path,FileName,IncFileName),
	exists(IncFileName),
	!.
    
    /*
     * macro/6		-- handle #macro statements
     */
    
    macro([ident(Id) | Formals],LN,InFD,OutFD,InTab,OutTab) :-
	getFormals(ArgList,Formals,[]),
	!,
	getmLines(InFD,OutFD,Lines),
	addToTable(Id,dm(ArgList,Lines),LN,InTab,OutTab).
    macro(_,LN,FD,FD,Tab,Tab) :-
	error('Bad #macro',LN).
    
    getFormals([],[],[]) :- !.
    getFormals([Id|More]) --> [ident(Id)], getMoreFormals(More).

    getMoreFormals([],[],[]) :- !.
    getMoreFormals([Id|More]) --> [comma,ident(Id)], getMoreFormals(More).

    getmLines(InFD,OutFD,Lines) :-
	getLine(Line,InFD,IFD),
	!,
	getmLines(Line,IFD,OutFD,Lines).
    getmLines(FD,FD,[]) :-
	lineNum(FD,LN),
	error('End of file encountered while reading #macro definition',LN).
    
    getmLines([pound,ident(endm)|_],FD,FD,[]) :- !.
    getmLines(Line,InFD,OutFD,[Line|Lines]) :-
	getmLines(InFD,OutFD,Lines).
    
    /*
     * undef/4		-- handle #undef
     */
    
    undef([ident(Id)],LN,InTab,OutTab) :- 
	avl_search(Id,_,InTab),
	avl_insert(Id,unDefined,InTab,OutTab),
	!.
    undef([ident(_)],LN,Tab,Tab) :- !.
    undef(_,LN,Tab,Tab) :-
	error('Illegal #undef argument',LN).


    /*
     * define/4		-- handle #defines
     */
    
    define([ident(Id)|Defn],LN,InTab,OutTab) :- !,
	addToTable(Id,d(Defn),LN,InTab,OutTab).
    define([fident(Id)|ArgsAndDefn],LN,InTab,OutTab) :-
	getArgs(Args,ArgsAndDefn,Defn),
	!,
	addToTable(Id,da(Args,Defn),LN,InTab,OutTab).
    define(Whatever,LN,Tab,Tab) :-
	error('Bad #define',LN).
    
    /*
     * addToTable(Key,Defn,LineNum,InTab,OutTab)
     *
     *		Tests for the presence of Key in the macro defn table.  If
     *		Key is already in the macro definition table, an error is
     *		reported using LineNum.  Otherwise, the macro name and the
     *		definition are added to the macro definition table (InTab)
     *		to produce OutTab.
     */
    
    addToTable(Key,Defn,LN,Tab,Tab) :-
	avl_search(Key,Data,Tab),
	Data \= unDefined,
	!,
	error(Key,'defined again',LN).
    addToTable(Key,Defn,LN,InTab,OutTab) :-
	avl_insert(Key,Defn,InTab,OutTab).
    

    /*
     * getArgs/3	-- dcg rules for getting #define formal parameters
     */
    
    getArgs([Arg|MoreArgs]) --> [lparen,ident(Arg)],moreArgs(MoreArgs).

    moreArgs([]) --> [rparen],!.
    moreArgs([Arg|MoreArgs]) --> [comma,ident(Arg)],moreArgs(MoreArgs).


    /*
     * dappend/3	-- standard append, just determinate.
     */
    
    dappend([],L,L) :- !.
    dappend([H|T],L,[H|TL]) :- dappend(T,L,TL).

    /*
     * ifdef(Rest,LineNum,InFD,OutFD,InTab,OutTab)
     *
     *	-- handle ifdefs
     */		
    
    ifdef([Token],LN,InFD,OutFD,InTab,OutTab) :-
	is_identifier(Token,Id),
	!,
	do_ifdef(Id,LN,InFD,OutFD,InTab,OutTab).
    ifdef(_,LN,FD,FD,Tab,Tab) :-
	error('Ignoring ill formed #ifdef',LN).

    ifndef([Token],LN,InFD,OutFD,InTab,OutTab) :-
	is_identifier(Token,Id),
	!,
	do_ifndef(Id,LN,InFD,OutFD,InTab,OutTab).
    ifndef(_,LN,InFD,OutFD,InTab,OutTab) :-
	error('Ignoring ill formed #ifndef',LN).

    endif_or_else(Which,LN,InFD,OutFD,InTab,OutTab) :-
	skipMore(Which,InFD,OutFD),
	avl_search(ifdef(level),[[Lev|Rest]|Prev],InTab),
	avl_insert(ifdef(level),[Rest|Prev],InTab,OutTab).
    
    do_ifdef(Id,LN,FD,FD,InTab,OutTab) :-
	avl_search(Id,_,InTab),
	!,
	avl_search(ifdef(level),[LevList|Prev],InTab),
	avl_insert(ifdef(level),[[_|LevList]|Prev],InTab,OutTab).
    do_ifdef(Id,LN,InFD,OutFD,InTab,OutTab) :-
	skipLines(InFD,OutFD,What),
	check_else(What,InTab,OutTab).

    do_ifndef(Id,LN,InFD,OutFD,InTab,OutTab) :-
	avl_search(Id,_,InTab),
	!,
	skipLines(InFD,OutFD,What),
	check_else(What,InTab,OutTab).
    do_ifndef(Id,LN,FD,FD,InTab,OutTab) :-
	avl_search(ifdef(level),[LevList|Prev],InTab),
	avl_insert(ifdef(level),[[_|LevList]|Prev],InTab,OutTab).
    
    check_else(else,InTab,OutTab) :-
	!,
	avl_search(ifdef(level),[LevList|Prev],InTab),
	avl_insert(ifdef(level),[[endif|LevList]|Prev],InTab,OutTab).
    check_else(endif,Tab,Tab).

    skipLines(InFD,OutFD,RetTok) :-
	getLine(Line,InFD,TFD),
	!,
	skipLines(Line,TFD,OutFD,RetTok).
    skipLines(FD,FD,endif) :-
	lineNum(FD,LN),
	error('Missing #endif',LN).
    
    skipLines([pound,ident(endif) | _],FD,FD,endif) :- !.
    skipLines([pound,ident(else) | _],FD,FD,else) :- !.
    skipLines([pound,ident(ifdef) | _],FD1,OutFD,RetTok) :-
	!,
	skipLines(FD1,FD2,What),
	skipMore(What,FD2,FD3),
	skipLines(FD3,OutFD,RetTok).
    skipLines([pound,ident(ifndef) | _],FD1,OutFD,RetTok) :-
	!,
	skipLines(FD1,FD2,What),
	skipMore(What,FD2,FD3),
	skipLines(FD3,OutFD,RetTok).
    skipLines(_,InFD,OutFD,RetTok) :-
	skipLines(InFD,OutFD,RetTok).
    
    %% called when we've discovered a nested ifdef or ifndef
    skipMore(endif,FD,FD) :- !.
    skipMore(else,InFD,OutFD) :-
	!,
	skipLines(InFD,TFD,What),
	skipMore(What,TFD,OutFD).	%% we should error check here,
					%% but I'm not sure to do about
					%% recovery so we will go til we
					%% find an endif
	
    

    /*
     * is_identifer(Token,Id)	-- succeeds when Token is an identifier
     *				   and unifies Id accordingly.
     */
    
    is_identifier(ident(Id),Id) :- !.
    is_identifier(fident(Id),Id).


    /*
     * macro_expand(Line,FirstLine,RestLines,Hole, LN,Tab)
     *
     *	Takes a Line as the input and produces FirstLine and RestLines as the
     *	output.  Hole is the final tail at the end of RestLines.  LN is the
     *	line number that Line appeared on for error reporting purposes. Tab
     *	is the macro expansion symbol table.
     */
    

    macro_expand([],[],RL,RL,LN,Tab) :- !.
    macro_expand([Head|Rest],EL,RL,RH,LN,Tab) :-
        macro_expand(Head,Rest,EL,RL,RH,LN,Tab).

    macro_expand(Tok,Rest,EL,RL,RH,LN,Tab) :-
        is_identifier(Tok,Id),
        avl_search(Id,Defn,Tab),
        !,
        do_expand(Defn,Tok,Rest,EL,RL,RH,LN,Tab).
    macro_expand(Token,Rest,[Token|ER],RL,RH,LN,Tab) :-
        macro_expand(Rest,ER,RL,RH,LN,Tab).

    do_expand(d(Repl),_,Rest,EL,RL,RH,LN,Tab) :-
        !,
        dappend(Repl,Rest,Again),
        macro_expand(Again,EL,RL,RH,LN,Tab).
    do_expand(da(FArgs,Defn),_,Rest,EL,RL,RH,LN,Tab) :-
        getMArgs(AArgs,Rest,Rest2),
        length(FArgs,NArgs),
        length(AArgs,NArgs),
        !,
        do_subst(Defn,FArgs,AArgs,OutDefn),
        append(OutDefn,Rest2,Again),
	!,
        macro_expand(Again,EL,RL,RH,LN,Tab).
    do_expand(dm(FArgs,Lines),_,Rest,[],RL,RH,LN,Tab) :-
	getIArgs(AArgs,Rest,[]),
	length(FArgs,NArgs),
	length(AArgs,NArgs),
	!,
	do_substs(Lines,FArgs,AArgs,OutLines),
	macro_expand_lines(OutLines,RL,RH,LN,Tab).
    do_expand(_,Token,Rest,[Token|ER],RH,RH,LN,Tab) :-
        error('Macro expansion screwed up',LN).

    
    macro_expand_lines([],RH,RH,LN,Tab) :- !.
    macro_expand_lines([First|Rest],[EL|RL],RH,LN,Tab) :-
	macro_expand(First,EL,RL,RH1,LN,Tab),
	macro_expand_lines(Rest,RH1,RH,LN,Tab).

    /*
     * getMArgs(Args,InList,RestList)
     *
     *	Called to get the left paren, the actual arguments, and the right paren
     *	for a macro defined with a #define statement.
     */
    
    getMArgs([FA|RA]) --> [lparen], getMArg(FA), moreMArgs(RA).

    getMArg([],[rparen|T],[rparen|T]) :- 
	!.
    getMArg([],[comma|T],[comma|T]) :- 
	!.
    getMArg([lparen|More]) -->	
	[lparen], !,
	consumeThruRight(More,Tail),
	getMArg(Tail).
    getMArg([Any|More]) -->
	[Any],
	getMArg(More).
    
    moreMArgs([]) -->
	[rparen],
	!.
    moreMArgs([FA|RA]) -->
	[comma],
	getMArg(FA),
	moreMArgs(RA).
    
    /*
     * getIArgs(Args,InList,RestList)
     *
     *	Called to get the actual arguments in an instruction macro.  It
     *  may of course be used elsewhere to get the arguments to any
     *	instruction.
     */
    
    getIArgs([],[],[]) :- !.
    getIArgs([FA|RA]) --> getIArg(FA), moreIArgs(RA).

    getIArg([],[],[]) :- !.
    getIArg([],[comma|T],[comma|T]) :- !.
    getIArg([lparen|More]) -->
	[lparen], !,
	consumeThruRight(More,Tail),
	getIArg(Tail).
    getIArg([Any|More]) -->
	[Any],
	getIArg(More).
    
    moreIArgs([],[],[]) :- !.
    moreIArgs([FA|RA]) -->
	[comma],
	getIArg(FA),
	moreIArgs(RA).

    /*
     * consumeThruRight/4 is used to consume all characters up to a right
     *	paren (handling the case of nested left, right paren pairs) in
     *	getMArgs and getIArgs above.
     *
     */

    consumeThruRight([rparen|Tail],Tail) --> 
	[rparen],!.
    consumeThruRight([lparen|More],Tail) -->
	[lparen], !,
	consumeThruRight(More,ITail),
	consumeThruRight(ITail,Tail).
    consumeThruRight([Any|More],Tail) -->
	[Any],
	consumeThruRight(More,Tail).
    
    /*
     * do_substs(InLines,FArgs,AArgs,OutLines)
     *
     *	Called to perform substitions in a list of lines where FArgs is
     *	a list of the formal arguments and AArgs is a list (of same length)
     *	of the actual arguments
     */
    
    do_substs([],_,_,[]) :- !.
    do_substs([FL|RL],FArgs,AArgs,[FR|RR]) :-
	do_subst(FL,FArgs,AArgs,FR),
	do_substs(RL,FArgs,AArgs,RR).

    /*
     * do_subst(InLine,FArgs,AArgs,OutLine)
     *
     *	Called to perform substitions within a single line where FArgs is
     *	a list of formal Arguments and AArgs is a list (of same length) of
     *	the actual arguments.  The input line is InLine; the output line
     *	is OutLine.
     */
    
    do_subst([],_,_,[]) :- !.
    do_subst([Tok|Rest],FArgs,AArgs,Result) :-
	is_identifier(Tok,Id),
	assoc(Id,FArgs,AArgs,Subst),
	append(Subst,ResTail,Result),
	!,
	do_subst(Rest,FArgs,AArgs,ResTail).
    do_subst([Tok|Rest],FArgs,AArgs,[Tok|ResTail]) :-
	do_subst(Rest,FArgs,AArgs,ResTail).

    /*
     * assoc/4			-- associate a value with a key.
     */

    assoc(Key,[Key|_],[RVal|_],RVal) :- 
	!.
    assoc(Key,[_|T1],[_|T2],RVal) :- 
	assoc(Key,T1,T2,RVal).

	
    /*
     * parse_statement(Line,Statement,LN,InTab,OutTab)
     *
     * Regardless of the underlying assembly language, we expect
     * assembly language statements to be of the form:
     *
     *		label:	opcode	op1, op2, ... opN
     *
     * The label field may be ommitted.  The number of op1..opN fields will
     * depend on the actual instruction.  It is possible that these will
     * be empty.
     *
     */

    parse_statement(Line,Statement,LN,InTab,OutTab) :-
	label(Label,InTab,ITab,Line,Line2),
	parse_statement(Line2,Statement,Label,LN,ITab,OutTab).

    parse_statement([],statement(Label,empty,[]),Label,LN,Tab,Tab) :- !.
    parse_statement(Line2,statement(Label,Opcode,Args),Label,LN,InTab,OutTab) :-
	opcode(Opcode1,Line2,Line3),
	check_err(Opcode1,LN),
	sargs(Line3,Args,LN,InTab,ITab),
	check_instruction(Opcode1,Args,LN,Opcode,ITab,OutTab),
	!.
    parse_statement(Line,error,LN,Label,Tab,Tab).
    
    label(Id,Tab,Tab) --> 
		[ident(Id), colon], !.
    label(llab(Id),InTab,OutTab) -->
		[number(Num), colon], !,
		{define_label(Num,Id,InTab,OutTab)}.
    label(empty(_),Tab,Tab) --> [].

    sargs([],[],_,Tab,Tab) :- !.
    sargs(L,[Arg|RestArgs],LN,InTab,OutTab) :-
	aarg(Arg1,L,RL),
	!,
	check_err(Arg1,LN),
	check_arg(Arg1,Arg,InTab,ITab),
	sargs_more(RL,RestArgs,LN,ITab,OutTab).
    
    sargs_more([],[],_,Tab,Tab) :- !.
    sargs_more([comma|Rest],Args,LN,InTab,OutTab) :-
	!,
	sargs_more1(Rest,Args,LN,InTab,OutTab).
    sargs_more(_,_,LN,_,_) :- 
	error('Comma expected in argument list',LN),
	fail.
    
    sargs_more1([],_,LN,_,_) :- !,
	error('Argument expected after comma',LN),
	fail.
    sargs_more1(L,Args,LN,InTab,OutTab) :-
	sargs(L,Args,LN,InTab,OutTab).
    
    check_arg(blab(Label),llab(LID),Tab,Tab) :-
	!,
	backward_label(Label,LID,Tab).
    check_arg(flab(Label),llab(LID),InTab,OutTab) :-
	!,
	forward_label(Label,LID,InTab,OutTab).
    check_arg(Arg,Arg,Tab,Tab).

    check_instruction(InOP,Args,LN,OutOP,InTab,OutTab) :-
	instruction(InOP,Args,OutOP,InTab,OutTab),
	!.
    check_instruction(_,_,LN,_,T,T) :-
	error('Illegal instruction or argument mismatch',LN),
	fail.


    check_err(error(What),LN) :- error(What,LN), !, fail.
    check_err(_,_).	%% ok

    /*
     * error/2 and error/3	-- report errors
     */
    
    error(What,LineNum) :-
	telling(Current),
	tell(user),
	write('Error: '),
	write(What),write(' on line '),write(LineNum),write('.'),nl,
	tell(Current).

    error(What1,What2,LineNum) :-
	telling(Current),
	tell(user),
	write('Error: '),
	write(What1),write(' '),write(What2),
	write(' on line '),write(LineNum),write('.'),nl,
	tell(Current).
endmod.		%% parser
