/*=================================================================*
 |			html_tokens.pro
 |		Copyright (c) 1999-2004 Applied Logic Systems, Inc.
 |	
 |		Tokenize html source files suitably for parsing into pxml.
 |
 |	Authors: Ken Bowen & Chuck Houpt
 |
 |	This software is copyrighted by Applied Logic Systems, Inc.,
 |	and other parties.  The following terms apply to all files 
 |	associated with the software unless explicitly disclaimed in 
 |	individual files.
 |
 |	The authors hereby grant permission to use, copy, modify, distribute,
 |	and license this software and its documentation for any purpose, provided
 |	that existing copyright notices are retained in all copies and that this
 |	notice is included verbatim in any distributions. No written agreement,
 |	license, or royalty fee is required for any of the authorized uses.
 |	Modifications to this software may be copyrighted by their authors
 |	and need not follow the licensing terms described here, provided that
 |	the new terms are clearly indicated on the first page of each file where
 |	they apply.
 |	
 |	IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
 |	FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
 |	ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
 |	DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
 |	POSSIBILITY OF SUCH DAMAGE.
 |	
 |	THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
 |	INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
 |	FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
 |	IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
 |	NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 |	MODIFICATIONS.
 *=================================================================*/

module pxml.

export grab_html_tokens_cleaned/2.
export grab_html_tokens/2.
export cut_cmts_js/2.
export rtt/2.
export read_tokens_lines/2.
export read_tokens/2.
export read_tokens/5.

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
grab_html_tokens_cleaned(Path, HTMLPageTokens)
    :-
    grab_lines(Path, RawLines),
    cut_cmts_js(RawLines, ScriptlessLines),
    read_tokens_lines(ScriptlessLines, HTMLPageTokens).


/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
grab_html_tokens(Path, HTMLPageTokens)
    :-
    open(Path, read, S),
    unwind_protect( read_tokens(S, HTMLPageTokens), close(S) ).

	%%------------------------------------------
	%% Strip out comments, java script & styles
	%%------------------------------------------

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
cut_cmts_js(RawLines, ScriptlessLines)
    :-
    cut_cmts_js(RawLines, normal, ScriptlessLines).

cut_cmts_js([], State, []).
cut_cmts_js([Line | RawLines], State, ScriptlessLines)
	:-
	script_cmt_cut(State, Line, ScriptlessLines, NextState, ScriptlessLinesTail),
	cut_cmts_js(RawLines, NextState, ScriptlessLinesTail).

script_cmt_cut(State, Line, ScriptlessLines, State, ScriptlessLines)
	:-
	atom_length(Line, 0),
	!.

script_cmt_cut(comment, Line, ScriptlessLines, NextState, ScriptlessLinesTail)
	:-
	sub_atom(Line, EndStart, 3, _, '-->'),
	!,
	TailStart is EndStart + 3,
	(sub_atom(Line, TailStart, _, 0, LineTail) ->
		script_cmt_cut(normal, LineTail, ScriptlessLines, 
						NextState, ScriptlessLinesTail)
		;
		NextState = normal,
		ScriptlessLinesTail = ScriptlessLines
	).

script_cmt_cut(comment, Line, ScriptlessLines, script, ScriptlessLines).

script_cmt_cut(script, Line, ScriptlessLines, NextState, ScriptlessLinesTail)
	:-
	sub_atom(Line, EndStart, 9, _, '</script>'),
	!,
	TailStart is EndStart + 9,
	(sub_atom(Line, TailStart, _, 0, LineTail) ->
		script_cmt_cut(normal, LineTail, ScriptlessLines, NextState, 
						ScriptlessLinesTail)
		;
		NextState = normal,
		ScriptlessLinesTail = ScriptlessLines
	).

script_cmt_cut(script, Line, ScriptlessLines, script, ScriptlessLines).

script_cmt_cut(script, Line, ScriptlessLines, NextState, ScriptlessLinesTail)
	:-
	sub_atom(Line, EndStart, 8, _, '</style>'),
	!,
	TailStart is EndStart + 8,
	(sub_atom(Line, TailStart, _, 0, LineTail) ->
		script_cmt_cut(normal, LineTail, ScriptlessLines, NextState, 
						ScriptlessLinesTail)
		;
		NextState = normal,
		ScriptlessLinesTail = ScriptlessLines
	).

script_cmt_cut(script, Line, ScriptlessLines, style, ScriptlessLines).



script_cmt_cut(normal, Line, ScriptlessLines, NextState, ScriptlessLinesTail)
	:-
	sub_atom(Line, BeginStart, 4, _, '<!--'),
	!,
	(BeginStart > 0 ->
		sub_atom(Line, 0, BeginStart, _, LineHead),
		ScriptlessLines = [LineHead | InterLines]
		;
		InterLines = ScriptlessLines
	),
	TailStart is BeginStart + 4,
	(sub_atom(Line, TailStart, _, 0, LineTail) ->
		script_cmt_cut(comment, LineTail, InterLines, NextState, 
						ScriptlessLinesTail)
		;
		NextState = comment,
		ScriptlessLinesTail = InterLines
	).

script_cmt_cut(normal, Line, ScriptlessLines, NextState, ScriptlessLinesTail)
	:-
	sub_atom(Line, BeginStart, 7, _, '<script'),
	!,
	(BeginStart > 0 ->
		sub_atom(Line, 0, BeginStart, _, LineHead),
		ScriptlessLines = [LineHead | InterLines]
		;
		InterLines = ScriptlessLines
	),
	TailStart is BeginStart + 7,
	(sub_atom(Line, TailStart, _, 0, LineTail) ->
		script_cmt_cut(script, LineTail, InterLines, NextState, 
						ScriptlessLinesTail)
		;
		NextState = script,
		ScriptlessLinesTail = InterLines
	).

script_cmt_cut(normal, Line, ScriptlessLines, NextState, ScriptlessLinesTail)
	:-
	sub_atom(Line, BeginStart, 6, _, '<style'),
	!,
	(BeginStart > 0 ->
		sub_atom(Line, 0, BeginStart, _, LineHead),
		ScriptlessLines = [LineHead | InterLines]
		;
		InterLines = ScriptlessLines
	),
	TailStart is BeginStart + 6,
	(sub_atom(Line, TailStart, _, 0, LineTail) ->
		script_cmt_cut(script, LineTail, InterLines, NextState, 
						ScriptlessLinesTail)
		;
		NextState = script,
		ScriptlessLinesTail = InterLines
	).

script_cmt_cut(normal, Line, [Line | ScriptlessLinesTail], normal, 
				ScriptlessLinesTail).


	%%------------------------------------------
	%% Tokenize the input
	%%------------------------------------------
/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
rtt(File, Tokens)
	:-
	open(File, read, S),
	read_tokens(S, Tokens),
	close(S).

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
read_tokens_lines(Lines, Tokens)
	:-
	read_tokens_lines(Lines, normal, Tokens).

read_tokens_lines([], _, []).
read_tokens_lines([Line | Lines], Flag, Tokens)
	:-
%write(Line),write('  ||  '),
	open(atom(Line),read,S,[]),
	read_tokens(Flag, S,  Tokens, InterTail, InterFlag),
%write('['), write(Flag), write('] '),
%write(Tokens),nl,
	close(S),
%trace,
	read_tokens_lines(Lines, InterFlag, InterTail).

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
read_tokens(S, Tokens)
	:-
	read_tokens(normal, S,  Tokens, [], _).



/*----------------------------------------------------------------*
 | read_tokens/5
 | read_tokens(FlagIn, S, Tokens, Tail, FlagOut)
 | read_tokens(+, +, -, -, -)
 |
 | Input:
 |   FlagIn  = one of: normal, n1, comment, comment_eol
 |   S       = input stream
 | Output:
 |   Tokens  = [<list of tokens read from S> | Tail]
 |   Tail    = tail of Tokens
 |   FlagOut = one of: normal, n1, comment
 |
 | Reads as many tokens as it can from S, returning them in Tokens,
 | with Tail = the (uninstantiated) tail of Tokens;  
 | if FlagIn = normal, begins reading in "normal" tokenizing mode;
 | if FlagIn = comment, begins reading in mode appropriate for
 |    the interior of a comment;
 | if FlagIn = comment_eol, has encountered end of line while reading
 |    the interior of a comment;
 | if FlagIn = n1, an opening '<' has been read, which __might__
 |    be follwed by '--', starting a comment (or might not);
 | if FlagIn = href(T, Tokens), an opening 'T=' (T=href/src/alt) has been read, 
 |    so we must process the stream following in a manner appropriate 
 |    for the context 'href=...'
 | FlagOut is the mode current when the last token is read.
 *----------------------------------------------------------------*/
read_tokens(comment_eol, S, Tail, Tail, comment)
	:-!.

read_tokens(comment, S, Tokens, Tail, FlagOut)
	:-!,
	get_code(S, C),
	start_get_comment(C, S, Tokens, InterTail, InterFlagOut),
	read_tokens(InterFlagOut, S, InterTail, Tail, FlagOut).

read_tokens(href(T, Tokens), S, Tokens, Tail, FlagOut)
	:-!,
	scoop_href(T, 0'=, S, Tokens, InterTokens, NxtC0, normal, FlagInter0),
	read_tokens(FlagInter0, NxtC0, S, InterTokens, Tail, FlagOut).

read_tokens(FlagIn, S, [T | Tokens], Tail, FlagOut)
	:-
	cross_white(S, NextNonblankChar),
	!,
	next_token(NextNonblankChar, FlagIn, S, T, NxtC, FlagInter),
	    %% if an 'href' token was encountered, process the stream following
	    %% in a manner appropriate for the context 'href=..."
	scoop_href_and_go(T, NxtC, S, Tokens, FlagInter, Tail, FlagOut).
/*
	scoop_href(T, NxtC, S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0),
	read_tokens(FlagInter0, NxtC0, S, InterTokens, Tail, FlagOut).
*/

read_tokens(Flag, S, Tail, Tail, Flag).

scoop_href_and_go(T, NxtC, S, Tokens, FlagInter, Tail, FlagOut)
	:-
	peek_code(S, C),
	(C = -1 ->
		Tokens = Tail,
		FlagOut = href(T, Tail)
		;
		scoop_href(T, NxtC, S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0),
		read_tokens(FlagInter0, NxtC0, S, InterTokens, Tail, FlagOut)
	).


/*
scoop_href(href, 0'=, S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0)
	:-!,
	scoop_0(S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0).

scoop_href(src, 0'=, S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0)
	:-!,
	scoop_0(S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0).

scoop_href(alt, 0'=, S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0)
	:-!,
	scoop_0(S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0).
*/

scoop_href(T, 0'=, S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0)
	:-
	dmember(T, [href, src, alt, content]),
	scoop_0(S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0).



scoop_href(T, NxtC, S, Tokens, Tokens, NxtC, FlagInter, FlagInter).


scoop_0(S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0)
	:-
	Tokens = ['=', StringToken | InterTokens],
	get_code(S, C),
	(C = 0'" ->
		read_string_char_list(S, StringChars, NxtC0)
		;
		StringChars = [C | SCs0],
		read_string_char_list_xtnd(S, SCs0, NxtC0)
	),
	atom_codes(StringToken, StringChars),
	FlagInter0 = FlagInter.

/*
scoop_0(S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0)
	:-
	Tokens = ['=', StringToken | InterTokens],
	get_code(S, C),
	(C = 0'" ->
		read_string_char_list(S, StringChars, NxtC0)
		;
		StringChars = [C | SCs0],
		read_string_char_list_pointy(S, SCs0, NxtC0)
	),
	atom_codes(StringToken, StringChars),
	FlagInter0 = FlagInter.

scoop_1(S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0)
	:-
	Tokens = ['=', StringToken | InterTokens],
	get_code(S, C),
	(C = 0'" ->
		read_string_char_list(S, StringChars, NxtC0)
		;
		StringChars = [C | SCs0],
		read_string_char_list_eol(S, SCs0, NxtC0)
	),
	atom_codes(StringToken, StringChars),
	FlagInter0 = FlagInter.

read_string_char_list_pointy(S, StringChars, NxtC0)
	:-
	get_code(S, C),
	disp_read_string_char_list_pointy(C, S, StringChars, NxtC0).

disp_read_string_char_list_pointy(0'>, S, [], 0'>)
	:-!.
disp_read_string_char_list_pointy(C, S, [C | StringChars], NxtC)
	:-
	read_string_char_list_pointy(S, StringChars, NxtC).

read_string_char_list_eol(S, StringChars, NxtC0)
	:-
	get_code(S, C),
	disp_read_string_char_list_eol(C, S, StringChars, NxtC0).

disp_read_string_char_list_eol(-1, S, [], 0' )
	:-!.
disp_read_string_char_list_eol(C, S, [C | StringChars], NxtC)
	:-
	read_string_char_list_eol(S, StringChars, NxtC).

*/




read_string_char_list_xtnd(S, StringChars, NxtC0)
	:-
	get_code(S, C),
	disp_read_string_char_list_xtnd(C, S, StringChars, NxtC0).

disp_read_string_char_list_xtnd(0'>, S, [], 0'>)
	:-!.
disp_read_string_char_list_xtnd(-1, S, [], 0' )
	:-!.
disp_read_string_char_list_xtnd(C, S, [C | StringChars], NxtC)
	:-
	read_string_char_list_xtnd(S, StringChars, NxtC).




/*----------------------------------------------------------------*
 | read_tokens/6
 | read_tokens(FlagIn, C, S, Tokens, Tail, FlagOut)
 | read_tokens(+, +, +, -, -, -)
 |
 | Input:
 |   FlagIn  = one of: normal, n1, comment
 |   C       = character most recently read from S
 |   S       = input stream
 | Output:
 |   Tokens  = [<list of tokens read from S> | Tail]
 |   Tail    = tail of Tokens
 |   FlagOut = one of: normal, n1, comment
 |
 | Reads as many tokens as it can from S, returning them in Tokens,
 | with Tail = the (uninstantiated) tail of Tokens;  
 | if FlagIn = normal, begins reading in "normal" tokenizing mode;
 | if FlagIn = comment, begins reading in mode appropriate for
 |    the interior of a comment;
 | if FlagIn = n1, an opening '<' has been read, which __might__
 |    be follwed by '--', starting a comment (or might not);
 | FlagOut is the mode current when the last token is read.
 *----------------------------------------------------------------*/
read_tokens(FlagIn, -1, S, Tail, Tail, FlagIn) :-!.

read_tokens(comment, NxtC, S, Tokens, Tail, FlagOut)
	:-
	start_get_comment(NxtC, S, Tokens, InterTail, InterFlagOut),
	read_tokens(InterFlagOut, S, InterTail, Tail, FlagOut).

read_tokens(FlagIn, NxtC, S, Tokens, Tail, FlagOut)
	:-
	NxtC =< 0' ,		%% space (32)
	!,
	read_tokens(FlagIn, S, Tokens, Tail, FlagOut).

read_tokens(FlagIn, NxtC, S, [T | Tokens], Tail, FlagOut)
	:-
	next_token(NxtC, FlagIn, S, T, NxtNxtC, FlagInter),
	scoop_href(T, NxtNxtC, S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0),
	read_tokens(FlagInter0, NxtC0, S, InterTokens, Tail, FlagOut).

cross_white(S, NextNonblankChar)
	:-
	get_code(S, C),
	dispatch_cross_white(C, S, NextNonblankChar).

dispatch_cross_white(-1, S, _) :- !, fail.
dispatch_cross_white(C, S, NextNonblankChar)
	:-
	C =< 0' ,		%% space (32)
	!,
	cross_white(S, NextNonblankChar).
dispatch_cross_white(C, S, C).


next_token(0'<, _, S, '<',NxtC, n1) 
	:-!,
	get_code(S,NxtC).

next_token(0'!, n1, S, T, NxtC, InterFlag)
	:-!,
	try_comment(S, [0'!], T, NxtC, InterFlag).

next_token(C, n1, S, T, NxtC, normal)
	:-!,
	next_token(C, S, T, NxtC).

next_token(C, normal, S, T, NxtC, normal)
	:-!,
	next_token(C, S, T, NxtC).

try_comment(S, Stack, T, NxtC, InterFlag)
	:-
	get_code(S,C0),
	disp_try_comment(C0, S, Stack, T, NxtC, InterFlag).

disp_try_comment(0'-, S, [0'!], T, NxtC, InterFlag)
	:-!,
	try_comment(S, [0'-,0'!], T, NxtC, InterFlag).

disp_try_comment(C, S, [0'!], T, NxtC, normal)
	:-!,
	read_to_terminator(S, Cs, NxtC),
	name(T, [0'!, C | Cs]).

disp_try_comment(0'-, S, [0'-, 0'!], '!--', NxtC, comment)
	:-!,
	get_code(S,NxtC).

disp_try_comment(C, S, [0'-, 0'!], T, NxtC, comment)
	:-!,
	read_to_terminator(S, Cs, NxtC),
	name(T, [0'!, 0'- | Cs]).

next_token(0'<, S, '<',NxtC) 
	:-!,
	get_code(S,NxtC).

next_token(0'>, S, '>',NxtC) 
	:-!,
	get_code(S,NxtC).

next_token(0'/, S, '/',NxtC) 
	:-!,
	get_code(S,NxtC).

next_token(0'=, S, '=',NxtC) 
	:-!,
	get_code(S,NxtC).

/*  ADDITION FOR DTDs */
next_token(0'+, S, '+',NxtC) 
	:-!,
	get_code(S,NxtC).
next_token(0'*, S, '*',NxtC) 
	:-!,
	get_code(S,NxtC).
next_token(0':, S, ':',NxtC) 
	:-!,
	get_code(S,NxtC).
next_token(0'(, S, '(',NxtC) 
	:-!,
	get_code(S,NxtC).
next_token(0'), S, ')',NxtC) 
	:-!,
	get_code(S,NxtC).
next_token(0',, S, ',',NxtC) 
	:-!,
	get_code(S,NxtC).
next_token(0'|, S, '|',NxtC) 
	:-!,
	get_code(S,NxtC).
next_token(0'?, S, '?',NxtC) 
	:-!,
	get_code(S,NxtC).

next_token(C, S, Atm, NxtC)
	:-
	single_char_dtd(C),
	atom_codes(Atm, [C]).
single_char_dtd(0'+).
single_char_dtd(0'*).
single_char_dtd(0':).
single_char_dtd(0'().
single_char_dtd(0')).
single_char_dtd(0',).
single_char_dtd(0'|).
single_char_dtd(0'?).

/*  END ADDITION FOR DTDs */

/*
next_token(0'", S, '"',NxtC) 
	:-!,
	get_code(S,NxtC).
*/
%%% ?? MUST GET STRING AS TOKEN:  ??
next_token(0'", S, StringToken ,NxtC) 
	:-!,
	read_string_char_list(S, StringChars, NxtC),
	atom_codes(StringToken, StringChars).

next_token(0'', S, '\'',NxtC) 
	:-!,
	get_code(S,NxtC).

    %% Read a character reference:
next_token(0'&, S, T, NxtC)
	:-!,
	get_code(S,NNxtC),
	((NNxtC=0' ; NNxtC=0'	; NNxtC=10; NNxtC=13) ->
		Cs = [], NxtC = NNxtC
		;
		dispatch_read_to_semi(NNxtC, S, Cs, NxtC)
	),
	atom_codes(T, [0'& | Cs]).

next_token(C1, S, T,NxtC)
	:-
	read_to_terminator(S, Cs, NxtC),
	name(T, [C1 | Cs]).

read_to_terminator(S, Cs, NxtC)
	:-
	get_code(S, C),
	dispatch_read_to_terminator(C, S, Cs, NxtC).

dispatch_read_to_terminator(C, S, [], C)
	:-
	C =< 0' , !.		%% space (32)
dispatch_read_to_terminator(C, S, [], C)
	:-
	C = 0'>, !.
dispatch_read_to_terminator(C, S, [], C)
	:-
	C = 0'<, !.
dispatch_read_to_terminator(C, S, [], C)
	:-
	C = 0'=, !.
dispatch_read_to_terminator(C, S, [], C)
        :-
        C = 0'&, !.
	

read_to_semi(S, Cs, NxtC)
	:-
	get_code(S, C),
	dispatch_read_to_semi(C, S, Cs, NxtC).

dispatch_read_to_semi(0';, S, [0';], NxtC)
	:-!,
	get_code(S, NxtC).
dispatch_read_to_semi(C, S, [C | Cs], NxtC)
	:-
	read_to_semi(S, Cs, NxtC).






/*  ADDITION FOR DTDs */
dispatch_read_to_terminator(C, S, [], C)
	:-
	single_char_dtd(C),!.

dispatch_read_to_terminator(C, S, [C | Cs], NxtC)
	:-
	read_to_terminator(S, Cs, NxtC).

read_to_q2(S, Cs)
	:-
	get_code(S, C),
	dispatch_read_to_q2(C, S, Cs).

	%% Need to raise exception:
dispatch_read_to_q2(-1, S, Cs) :- fail, !.
dispatch_read_to_q2(0'", S, []) :-!.
dispatch_read_to_q2(C, S, [C | Cs])
	:-
	read_to_q2(S, Cs).

read_string_char_list(S, StringChars, NxtC)
	:-
	get_code(S, C),
	disp_read_string_char_list(C, S, StringChars, NxtC).

disp_read_string_char_list(0'", S, [], NxtC)
	:-!,
	get_code(S, NxtC).
disp_read_string_char_list(C, S, [C | StringChars], NxtC)
	:-
	read_string_char_list(S, StringChars, NxtC).

start_get_comment(0'-, S, Tokens, Tail, Flag)
	:-!,
	try_end_comment(S, [0'-], Tokens, Tail, Flag).

%start_get_comment(-1, S, Tail, Tail, comment).
start_get_comment(-1, S, Tail, Tail, comment_eol).

start_get_comment(C, S, Tokens, Tail, Flag)
	:-
	get_cmt(S, Cs, Terminal, _),
	get_cmt_inter(Terminal,  [C | Cs], S, Tokens, Tail, Flag).

get_cmt(S, Cs, Terminal, Flag)
	:-
	get_code(S, C0),
	disp_get_cmt(C0, S, Cs, Terminal, Flag).

disp_get_cmt(-1, S, [], eol, comment) :-!.
disp_get_cmt(0'-, S, Cs, Terminal, Flag) 
	:-!,
	end_cmt_try(S, [0'-], Cs, Terminal, Flag).
disp_get_cmt(C, S, [C | RCs], Terminal, Flag) 
	:-
	get_cmt(S, RCs, Terminal, Flag).

get_cmt_inter(eol,  Cs, S, Tokens, Tail, comment)
	:-!,
	atom_codes(Tok, Cs),
	Tokens = [Tok | Tail].

%get_cmt_inter('--',  Cs, S, Tokens, Tail, normal)
get_cmt_inter('--',  Cs, S, Tokens, Tail, FlagOut)
	:-!,
	atom_codes(Tok, Cs),
	get_code(S, C),
	(C = 0'> ->
		Tokens = [Tok, '--', '>' | Tail],
		FlagOut = normal
		;
		Tokens = [Tok, '--',  InterTokens],
		Flag = comment,
		read_tokens(comment, C, S, InterTokens, Tail, FlagOut)
	).

		%% if C \= 0'>, raise error...

/*
	%% raise eof error:
get_cmt_inter(end_of_file,  Cs, Tokens, Tail)
	:-

	%% anything else is a big-time error:
get_cmt_inter(,  Cs, Tokens, Tail)
	:-
*/

try_end_comment(S, CCs, Tokens, Tail, Flag)
	:-
	get_code(S, C),
	disp_try_end_comment(C, CCs, S, Tokens, Tail, Flag).

disp_try_end_comment(0'>, [0'-, 0'- | Cs], S, Tokens, Tail, normal)
	:-!,
	atom_codes(Tok, Cs),
	Tokens = [Tok, '--', '>' | Tail].

disp_try_end_comment(0'-, [0'- | Cs], S, Tokens, Tail, Flag)
	:-!,
	CCs = [0'-, 0'- | Cs],
	try_end_comment(S, CCs, Tokens, Tail, Flag).

disp_try_end_comment(-1, Ds, S, Tokens, Tail, comment)
	:-
	atom_codes(Tok, Ds),
	Tokens = [Tok | Tail].

disp_try_end_comment(C, Ds, S, Tokens, Tail, Flag)
	:-
	atom_codes(Tok, Ds),
	Tokens = [Tok | InterTail],
	get_cmt(S, Cs, Terminal, _),
	get_cmt_inter(Terminal,  [0'-, C | Cs], S, InterTail, Tail, Flag).

end_cmt_try(S, [0'-], Cs, Terminal, Flag)
	:-
	get_code(S, C),
	disp_end_cmt_try(C, [0'-], S, Cs, Terminal, Flag).

disp_end_cmt_try(0'-, [0'-], S, [], Terminal, normal)
	:-!,
	Terminal = '--'.

disp_end_cmt_try(C, [0'-], S, [C, 0'- | RCs], Terminal, Flag)
	:-
	get_cmt(S, RCs, Terminal, Flag).

endmod.

