/*
 * atoken.pro		-- assembly language tokenizer
 *	Copyright (c) 1990 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 3/31/90
 *
 *
 * Exported Procedures:
 *
 *	getlines/1	-- This procedure is used for testing.  Its single
 *			   argument is a file name.  It will read the file,
 *			   tokenize each line and write the tokenized lines
 *			   out to the current output device.
 *
 *	openFile/2	-- This procedure takes a file name as its first
 *			   argument and builds a structure which describes
 *			   certain attributes about the tokenization of the
 *			   file.  This state variable is the second parameter.
 *
 *	getLine/3	-- This procedure returns a tokenized line as its
 *			   first argument.  The second argument is an input
 *			   argument and is the input state.  The third argument
 *			   is the state after getting the line in question.
 *
 *	fileName/2	-- When given a state variable as the first argument,
 *			   unifies the second argument with the file name.
 *
 *	lineNum/2	-- When given a state variable as the first argument,
 *			   unifies the second argument with the present line
 *			   number.  This value may be useful for printing
 *			   out error messages.
 *
 */


module tokenizer.

/*
 * getLines is used for testing.   It tokenizes a file and writes out the
 * tokenized lines.
 */

export getLines/1.

getLines(FileName) :-
    openFile(FileName,State),
    getLines1(State).

getLines1(State) :-
    getLine(TokLine,State,NextState),
    !,
    write(TokLine),nl,
    getLines1(NextState).
getLines1(_).			%% succeed when getLine eventually fails
				%% upon eof

/*
 * openFile builds the starting state
 */

export openFile/2.

openFile(FName,State) :-
    seeing(Current),
    see(FName),
    fileStream(State,InStream),
    lineNum(State,1),
    fileName(State,FName),
    getInput(InStream),
    see(Current).

/*
 * getLine(TokLine,InState,OutState) repeatedly calls getToken until an
 * entire line of input is obtained.  The line of tokenized input is
 * turned into a list an returned via TokLine.
 *
 * getLine will fail after closing the File upon end of file.
 */


export getLine/3.

getLine(TokLine,InState,OutState) :-
    getToken(Tok,InState,IState),
    getLine(Tok,TokLine,IState,OutState).

getLine(eof,_,State,_) :-
    fileName(State,FName),
    seeing(Current),
    see(FName),
    seen,
    see(Current),
    !,
    fail.
getLine(eoln,[],State,State) :- !.
getLine(backslash,Line,InState,OutState) :-
    getToken(Tok,InState,IState),
    getLine_backslash(Tok,Line,IState,OutState).
getLine(Tok,[Tok|More],InState,OutState) :-
    getLine(More,InState,OutState).

%% If backslash comes before end-of-line, then merge lines together
getLine_backslash(eoln,Line,InState,OutState) :- !,
    getLine(Line,InState,OutState).
getLine_backslash(Other,[backslash,Other|More],InState,OutState) :-
    getLine(More,InState,OutState).

/*
 * getToken(Token,InState,OutState)
 *
 * Gets a token from the file stream in the InState variable and unifies
 * it with Token.  OutState is set to the new output state.
 */

getToken(Token,InState,OutState) :-
    fileStream(InState,InStream),
    fileStream(OutState,OutStream),
    lineNum(InState,InLine),
    lineNum(OutState,OutLine),
    fileName(InState,FName),
    fileName(OutState,FName),
    seeing(Current),
    see(FName),
    skipSpace(InStream,IStream,InLine,ILine),
    getToken(Token,IStream,OutStream,InLine,ILine,OutLine),
    see(Current).

getToken(eoln,Stream,Stream,InLine,Line,Line) :-
    InLine \= Line,		%% have end of line when we advance to
    !.				%% a different line while skipping space
getToken(eof,[],[],_,Line,Line) :- 
    !.				%% EOF processing
getToken(Token,[First | Rest], OutStream, _, Line, Line) :-
    getInput(Rest),
    getToken1(First,Token,Rest,OutStream,Line).

getToken1(Char1,Token,[Char2|OutStream],OutStream,Line) :-
    isSpecial2(Char1,Char2,Token),
    !.
getToken1(SpecialChar, SpecialTok, Stream,Stream, Line) :-
    isSpecial(SpecialChar, SpecialTok),
    !.
getToken1(NumChar, number(Num), [NextChar|Rest], OutStream, Line) :-
    isNumeric(NumChar,INum),
    !,
    getInput(Rest),
    getNumber(INum,NextChar,Rest,OutStream,Num).
getToken1(0'", string(StringName), [NextChar|Rest], OutStream, Line) :-
    !,
    getInput(Rest),
    getString(NextChar,String,Rest,OutStream,Line),
    name(StringName,String).
getToken1(IdChar, Token, InStream, OutStream, Line) :-
    isAlpha(IdChar),
    !,
    getIdentifier(IdChar,IdString, InStream,OutStream),
    name(Identifier,IdString),
    classifyIdentifier(Identifier,OutStream,Token).
getToken1(Other, other(Other), Stream, Stream, Line).


/*
 * getNumber(INum,Char,InStream,OutStream,OutNum) 
 *
 * INum is the intermediate result.  Char is the next character.  Produces
 * the result OutNum and binds OutStream as appropriate.
 *
 * Calls the subsidiary procedures getHex/5 and getDecimal/5.
 */

getNumber(0,0'x,[NextChar|Rest],OutStream,Num) :-
    !,
    getHex(0,NextChar,Rest,OutStream,Num).
getNumber(INum,NextChar,InStream,OutStream,Num) :-
    getDecimal(INum,NextChar,InStream,OutStream,Num).

getHex(INum,Char,[NextChar|Rest],OutStream,Num) :-
    isHex(Char,INum1),
    !,
    getInput(Rest),
    INum2 is (INum<<4)+INum1,
    getHex(INum2,NextChar,Rest,OutStream,Num).
getHex(Num,Char,Rest,[Char|Rest],Num).

getDecimal(INum,Char,[NextChar|Rest],OutStream,Num) :-
    isNumeric(Char,INum1),
    !,
    getInput(Rest),
    INum2 is INum*10+INum1,
    getDecimal(INum2,NextChar,Rest,OutStream,Num).
% experimental clause to add floating point numbers
% if it's not numeric, instead of stopping, check for a decimal point
getDecimal(DecPart, 0'. ,[NextChar|[NC|Rest]],OutStream,Num) :-
	!,
	isNumeric(NextChar,INum),
	getDecimal(INum,NC,Rest,OutStream,FracPart),
	% now we have DecPart and FracPart as integers - add them as floats
	name(FracPart,FL),
	length(FL,FLen),
	FracNum is FracPart/10^FLen,
	Num is DecPart+FracNum.
getDecimal(Num,Char,Rest,[Char|Rest],Num).


/*
 * getString(NextChar,String,InStream,OutStream,Line) is used to read the 
 * characters up til the next double quote.
 */

getString(0'",[],Stream,Stream,_) :- !.
getString(0'\n,[],Stream,[0'\n|Stream],Line) :- !,
    writeWarning('Unterminated double string',Line).
getString(Char,[Char|RestChars],[NextChar|Rest],OutStream,Line) :-
    getInput(Rest),
    getString(NextChar,RestChars,Rest,OutStream,Line).

/*
 * getIdentifier(Char,IdString,InStream,OutStream) is used to get an
 *	identifier and put it in IdString.
 */

getIdentifier(Char,IdString,InStream,OutStream) :-
    isAlpha(Char),
    !,
    getIdentifier2(Char,IdString,InStream,OutStream).
getIdentifier(Char,IdString,InStream,OutStream) :-
    isNumeric(Char,_),
    !,
    getIdentifier2(Char,IdString,InStream,OutStream).
getIdentifier(Char,[],InStream,[Char|InStream]).

getIdentifier2(Char,[Char|RestChars],[NextChar|Rest],OutStream) :-
    getInput(Rest),
    getIdentifier(NextChar,RestChars,Rest,OutStream).

/*
 * classifyIdentifier(Identifier, InStream, Token) is used to build a token
 * out of the Identifier based upon the upcoming character in InStream.
 */

classifyIdentifier(Id,[0'( | Rest],fident(Id)) :- !.
classifyIdentifier(Id, InStream, ident(Id)).



/*
 * isSpecial(Char,Tok) is map between characters which are considered
 * to be tokens by themselves and the symbolic name of the token.
 */

isSpecial(0'(, lparen).
isSpecial(0'), rparen).
isSpecial(0'[, lbrac).
isSpecial(0'], rbrac).
isSpecial(0'<, langle).
isSpecial(0'>, rangle).
isSpecial(0'+, plus).
isSpecial(0'-, minus).
isSpecial(0'*, star).
isSpecial(0'|, or).
isSpecial(0'&, and).
isSpecial(0'#, pound).
isSpecial(0'@, at).
isSpecial(0'=, equal).
isSpecial(0':, colon).
isSpecial(0',, comma).
isSpecial(0'., dot).
isSpecial(0'~, not).
isSpecial(0'!, lnot).
isSpecial(0'\\, backslash).
isSpecial(0'/, slash).
isSpecial(0'?, question).
isSpecial(0'%, percent).
isSpecial(0'^, carat).
isSpecial(0'{, lcurly).
isSpecial(0'}, rcurly).

/*
 * isSpecial2(Char1,Char2,Tok) is a map between 2 character sequences and
 * the tokens which they represent.
 */

isSpecial2(0'<,0'<,leftshift).
isSpecial2(0'>,0'>,rightshift).
isSpecial2(0'!,0'=,notequal).
isSpecial2(0'<,0'=,lessOrEqual).
isSpecial2(0'>,0'=,greaterOrEqual).
isSpecial2(0'=,0'=,equalEqual).
isSpecial2(0'&,0'&,land).			% logical and
isSpecial2(0'|,0'|,lor).			% logical or
isSpecial2(0'+,0'+,plusplus).
isSpecial2(0'-,0'-,minusminus).

/*
 * isNumeric is a map between decimal numeric characters and the numbers that
 * they represent.
 *
 * isHex is a map between Hexadecimal characters and the numbers that they
 * represent.
 *
 */

isNumeric(NC, Num) :- 0'0 =< NC, NC =< 0'9, Num is NC - 0'0.

isHex(NC, Num) :- isNumeric(NC,Num), !.
isHex(NC, Num) :- 0'a =< NC, NC =< 0'f, Num is NC-0'a+10.
isHex(NC, Num) :- 0'A =< NC, NC =< 0'F, Num is NC-0'A+10.

/*
 * isAlpha(Char) succeeds when Char is considered to be an alphabetic
 * character.
 */

isAlpha(Char) :- 0'a =< Char, Char =< 0'z, !.
isAlpha(Char) :- 0'A =< Char, Char =< 0'Z, !.
isAlpha(0'$).
isAlpha(0'_).

/*
 * State manipulation predicates
 *
 *	fileStream(State,FileStream)
 *	lineNum(State,LineNum)
 *	fileName(State,FileName)
 */

export lineNum/2, fileName/2.

checkState(State) :- functor(State,tokState,3).
fileStream(State,FileStream) :- checkState(State), arg(1,State,FileStream).
lineNum(State,LineNum) :- checkState(State), arg(2,State,LineNum).
fileName(State,Name) :- checkState(State), arg(3,State,Name).

/*
 * lineCommentChar(Char)
 *
 * Enumerates the characters which denote line comments
 *
 */

lineCommentChar(0';).		%% Semicolon is the line comment character


/*
 * skipSpace(InStream,OutStream,InLine,OutLine)
 *
 * It is assumed that the InputStream has at least K characters in it.
 * InLine is the line number of the line we are on before processing any
 * characters in InputStream.  OutLine is the line number that we get to
 * after the processing.
 * 
 */

skipSpace([],[],Line,Line) :- !.	%% EOF
skipSpace([First | Rest],OutStream, InLine,OutLine) :-
    getInput(Rest),
    skipSpace(First,Rest,OutStream, InLine,OutLine).


skipSpace(0'\n, Rest, OutStream, InLine, OutLine) :-
    !,
    NewLine is InLine + 1,		%% NewLine Character
    skipSpace(Rest,OutStream,NewLine,OutLine).

skipSpace(SpaceChar, Rest, OutStream, InLine, OutLine) :-
    SpaceChar =< 32,
    !,					%% Space Characters
    skipSpace(Rest,OutStream,InLine,OutLine).

skipSpace(LComment, Rest, OutStream, InLine, OutLine) :-
    lineCommentChar(LComment),
    !,					%% Line Comment
    skipToEOL(Rest, Rest2),
    NewLine is InLine+1,
    skipSpace(Rest2,OutStream,NewLine,OutLine).

skipSpace(0'/, [0'* | Rest], OutStream, InLine,OutLine) :-
    !,					%% C-Style Comments
    skipToCommentEnd(Rest,Rest2,InLine,InLine2),
    skipSpace(Rest2,OutStream,InLine2,OutLine).

skipSpace(Any, InStream, [Any|InStream], Line, Line).	%% Non-Space char


skipToEOL([],[]) :- !.			%% EOF
skipToEOL([First | Rest], OutStream) :-
    getInput(Rest),
    skipToEOL(First,Rest,OutStream).

skipToEOL(0'\n,Stream,Stream) :- !.
skipToEOL(Any,Rest,OutStream) :-
    skipToEOL(Rest,OutStream).

skipToCommentEnd([],[],Line,Line) :- 
    !,					%% EOF
    writeWarning('Unclosed C-Style Comment',Line).
skipToCommentEnd([First|Rest], OutStream, InLine, OutLine) :-
    getInput(Rest),
    skipToCommentEnd(First,Rest,OutStream,InLine,OutLine).

skipToCommentEnd(0'\n, Rest, OutStream, InLine,OutLine) :-
    !,					%% NewLine Character
    NewLine is InLine+1,
    skipToCommentEnd(Rest,OutStream,NewLine,OutLine).
skipToCommentEnd(0'/, [0'* | Rest], OutStream, InLine,OutLine) :-
    !,					%% Nested C-Style Comment
    skipToCommentEnd(Rest,Rest2,InLine,InLine2),
    skipToCommentEnd(Rest2,OutStream,InLine2,OutLine).
skipToCommentEnd(0'*, [0'/ | Rest], Rest, Line, Line) :-
    !.					%% End of C-Style Comment
skipToCommentEnd(Any, Rest, OutStream, InLine, OutLine) :-
    skipToCommentEnd(Rest,OutStream,InLine,OutLine).



/*
 * getInput(List)
 *
 * We would like to view the file as one long list of characters.  We can, of
 * course, read in the file all at once into a long list, but I find this
 * aesthetically unpleasing as the file may be very large.  We only need
 * to look ahead K characters anyway, so we call getInput(List) to advance
 * the size of the list if possible.
 */

getInput(List) :-
    getInput(3,List).		%% K=3, List will always have 3 chars in it

getInput(0,List) :- !.		%% Have enough characters in the list
getInput(N,List) :- 		
    var(List), 			%% Don't have enough characters yet
    !,
    get0(C),			%% So get some more
    eofCheck(C,N,List).
getInput(N,[]) :- !.		%% At end of file, don't need any more chars
getInput(N,[_|Tail]) :-
    NP is N-1,
    getInput(NP,Tail).

eofCheck(-1,_,[]) :- !.
eofCheck(C,N,[C|Tail]) :-
    NP is N-1,
    getInput(NP,Tail).


writeWarning(Warning,Line) :-
    telling(Current),
    tell(user),
    write('Warning: '),write(Warning),write(' on line '),write(Line),nl,
    tell(Current).

endmod.				%% tokenizer
