/*=================================================================*
 |			pxml.pro
 |		Copyright (c) 1999 Applied Logic Systems, Inc.
 |	
 |		Conversion between html and abstract pxml representations
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

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Decomposing form input:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export html_form_input/2.
html_form_input('', []) :-!.
html_form_input(Line, [Left | X])
	:-
	sub_atom(Line, Init,Mid,Final, '&'),
	!,
	sub_atom(Line, 0,Init,_,Left0),
	html_eq(Left0, Left),
	Init1 is Init+1,
	sub_atom(Line, Init1,_,0,RestLine),
	html_form_input(RestLine, X).
html_form_input(Line, [Item])
	:-
	html_eq(Line, Item).

html_eq(Line, Left=Right)
	:-
	sub_atom(Line, Init,Mid,Final, '='),
	!,
	sub_atom(Line, 0,Init,_,Left0),
	html_escapes(Left0, Left),
	Init1 is Init+1,
	sub_atom(Line, Init1,_,0,Right0),
	html_escapes(Right0, Right).
html_eq(Line, Line).

html_escapes(Item0, Item)
	:-
	sub_atom(Item0, _,_,_,'+'),
	!,
	atom_codes(Item0, I0Cs),
	process_html_escapes(I0Cs, ICs),
	atom_codes(Item, ICs).
html_escapes(Item0, Item)
	:-
	sub_atom(Item0, _,_,_,'%'),
	!,
	atom_codes(Item0, I0Cs),
	process_html_escapes(I0Cs, ICs),
	atom_codes(Item, ICs).
html_escapes(Item, Item).

process_html_escapes([], []).
process_html_escapes([0'+ | I0Cs], [0'  | ICs])
	:-!,
	process_html_escapes(I0Cs, ICs).
process_html_escapes([0'%,C1,C2 | RestI0Cs], [C | ICs])
	:-!,
	hex_decimal_digit(C1, D1),
	hex_decimal_digit(C2, D2),
	C is D1 * 16 + D2,
	process_html_escapes(RestI0Cs, ICs).
process_html_escapes([C | I0Cs], [C | ICs])
	:-
	process_html_escapes(I0Cs, ICs).

hex_decimal_digit(HexCC, DecCC)
	:-
	HexCC >= 0'A,
	!,
	DecCC is ((HexCC /\ 0xdf) - 0'A) + 10.
hex_decimal_digit(HexCC, DecCC)
	:-
	DecCC is HexCC - 0'0.

export html_get_value/3.
html_get_value(Eqs, Tag, Val)
	:-
	dmember(Tag=Val0, Eqs),
	Val0 \= '',
	!,
	atomread(Val0, Val, [vars_and_names(Vs,Ns)]), Vs=Ns.
html_get_value(_, _, '').



	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Parser:  HTML --> PXML
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export grab_pxml/2.
grab_pxml(Path, PXML)
	:-
	open(Path, read, IS, [write_eoln_type(lf)]),
	unwind_protect(r_pxml(IS, PXML), 
	close(IS)).

export r_pxml/2.
r_pxml(S, L)
	:-
	read_tokens(S, Tokens),
	parse_pxml(Tokens, L).

	%%------------------------------------------
	%% Tokenize the input
	%%------------------------------------------
export read_tokens_lines/2.
read_tokens_lines(Lines, Tokens)
	:-
	read_tokens_lines(Lines, normal, Tokens).

read_tokens_lines([], _, []).
read_tokens_lines([Line | Lines], Flag, Tokens)
	:-
	open(atom(Line),read,S,[]),
	read_tokens(Flag, S,  Tokens, InterTail, InterFlag),
	close(S),
	read_tokens_lines(Lines, InterFlag, InterTail).

export read_tokens/2.
read_tokens(S, Tokens)
	:-
	read_tokens(normal, S,  Tokens, [], _).

/*----------------------------------------------------------------*
 | read_tokens/5
 | read_tokens(FlagIn, S, Tokens, Tail, FlagOut)
 | read_tokens(+, +, -, -, -)
 |
 | Input:
 |   FlagIn  = one of: normal, n1, comment
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
export read_tokens/5.
read_tokens(comment_eol, S, Tail, Tail, comment)
	:-!.

read_tokens(comment, S, Tokens, Tail, FlagOut)
	:-!,
	get_code(S, C),
	start_get_comment(C, S, Tokens, InterTail, InterFlagOut),
	read_tokens(InterFlagOut, S, InterTail, Tail, FlagOut).


read_tokens(FlagIn, S, [T | Tokens], Tail, FlagOut)
	:-
	cross_white(S, NextNonblankChar),
	!,
	next_token(NextNonblankChar, FlagIn, S, T, NxtC, FlagInter),
%write(T),write(' '),
	scoop_href(T, NxtC, S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0),
%(nonvar(Tokens) -> write(Tokens) ; true), nl,
	read_tokens(FlagInter0, NxtC0, S, InterTokens, Tail, FlagOut).
read_tokens(Flag, S, Tail, Tail, Flag).

scoop_href(href, 0'=, S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0)
	:-!,
	scoop_0(S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0).
scoop_href(src, 0'=, S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0)
	:-!,
%	scoop_1(S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0).
	scoop_0(S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0).
scoop_href(alt, 0'=, S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0)
	:-!,
%	scoop_1(S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0).
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
%write(T),write(' '),
	scoop_href(T, NxtNxtC, S, Tokens, InterTokens, NxtC0, FlagInter, FlagInter0),
%(nonvar(Tokens) -> write(Tokens) ; true), nl,
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

get_cmt_inter('--',  Cs, S, Tokens, Tail, normal)
	:-!,
	atom_codes(Tok, Cs),
	get_code(S, C),
		%% if C \= 0'>, raise error...
	Tokens = [Tok, '--', '>' | Tail].

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


	%%------------------------------------------
	%% Parse the token stream
	%%------------------------------------------
export parse_pxml/2.
parse_pxml([], []).
parse_pxml(Tokens, [Term | RestTerms])
	:-
	read_pxml_term(Tokens, Term, RestTokens),
	parse_pxml(RestTokens, RestTerms).

read_pxml_term([string(StringAtom) | RestTokens], StringAtom, RestTokens)
	:-!.

read_pxml_term(['<', InTag,'>','<', InTag,'>' | Tokens], Term, RestTokens)
	:-
	make_lc_sym(InTag, html),
	!,
	read_pxml_term(['<', InTag,'>' | Tokens], Term, RestTokens).


read_pxml_term(['<', InTag | Tokens], Term, RestTokens)
	:-
	make_lc_sym(InTag, Tag),
	unary_tag(Tag),
	!,
	read_pxml_eqs_to(Tokens, '>', Features, RestTokens),
	Term  =.. [Tag, Features, []].

read_pxml_term(['<', '!--' | Tokens], Term, RestTokens)
	:-
	read_pxml_comment(Tokens, Features, RestTokens),
	Term  =.. [comment, Features, []].

read_pxml_term(['<', InTag | Tokens], Term, RestTokens)
	:-!,
	make_lc_sym(InTag, Tag),
	read_pxml_eqs_to(Tokens, '>', Features, InterTokens),
	read_to_close_html(InterTokens, SubTerms, Tag, RestTokens),
	Term  =.. [Tag, Features, SubTerms].

read_pxml_term(Tokens, List, ['<' | RestTokens])
	:-
	read_pxml_terms_to(Tokens, '<', List, RestTokens).

read_pxml_terms_to([], Terminator, [], []).
read_pxml_terms_to([Terminator | Tokens], Terminator, [], Tokens)
	:-!.
read_pxml_terms_to(['/','>' | Tokens], _, [], Tokens)
	:-!.
read_pxml_terms_to([T0 | Tokens], Terminator, [T0 | Terms], RestTokens)
	:-
	read_pxml_terms_to(Tokens, Terminator, Terms, RestTokens).

read_pxml_eqs_to([], Terminator, [], []).
read_pxml_eqs_to([Terminator | Tokens], Terminator, [], Tokens)
	:-!.
read_pxml_eqs_to(['/','>' | Tokens], _, [], ['/','>' | Tokens])
	:-!.
read_pxml_eqs_to(['<' | Tokens], '>', [], ['<' | Tokens])
	:-!.
read_pxml_eqs_to([T0, '=', T1 | Tokens], Terminator, 
				 [(Tag = Value) | Terms], RestTokens)
	:-!,
	make_lc_sym(T0, Tag),
	read_tag_value(T1, Tokens, Value, InterTokens),
	read_pxml_eqs_to(InterTokens, Terminator, Terms, RestTokens).
read_pxml_eqs_to([T0 | Tokens], Terminator, [T0 | Terms], RestTokens)
	:-
	read_pxml_eqs_to(Tokens, Terminator, Terms, RestTokens).

read_tag_value('"', Tokens, Value, InterTokens)
	:-
	consume_tokens_to_q2(Tokens, Head, InterTokens),
	catenate(Head, Value0),
	unescape_quotes(Value0, Value).
read_tag_value(Value, Tokens, Value, Tokens).

consume_tokens_to_q2([], Head, []) :-!, fail.
%consume_tokens_to_q2(['"' | InterTokens], [Tok], InterTokens)
%	:-!.
consume_tokens_to_q2(['"' | InterTokens], ['""'], InterTokens)
	:-!.
consume_tokens_to_q2([Item | Tokens], [Tok], InterTokens)
	:-
	sub_atom(Item, Bef,1,Aft, '"'),
	!,
	sub_atom(Item,0,Bef,_,Tok),
	(Aft = 0 ->
		InterTokens = Tokens
		;
		B1 is Bef+1,
		sub_atom(Item,B1,_,0,RTok),
		InterTokens = [RTok | Tokens]
	).


consume_tokens_to_q2([Item | Tokens], [Item | Head], InterTokens)
	:-
	consume_tokens_to_q2(Tokens, Head, InterTokens).

export read_pxml_comment/3.
read_pxml_comment([], [], []).
read_pxml_comment(['--','>' | Tokens], [], Tokens)
	:-!.
read_pxml_comment(['!--','>' | Tokens], [], Tokens)
	:-!.
read_pxml_comment(['//--','>' | Tokens], [], Tokens)
	:-!.
read_pxml_comment([Token | Tokens], [Token | Features], RestTokens)
	:-
	read_pxml_comment(Tokens, Features, RestTokens).


read_to_close_html([], [], _, []).
read_to_close_html(['/','>' | Tokens], [], Tag, Tokens)
	:-!.
read_to_close_html(['<','/',InTag0,'>','<','/',InTag1,'>' | Tokens], 
                       [], Tag, Tokens)
	:-
	Tag=font,
	make_lc_sym(InTag0, Tag),
	make_lc_sym(InTag1, Tag),
	!.
read_to_close_html(['<','/',InTag0,'>','<','/',InTag,'>' | Tokens], 
                       [], Tag, Tokens)
	:-
	make_lc_sym(InTag0, font),
	member(Tag,[td,tr,table]),
	make_lc_sym(InTag, Tag),
	!.

read_to_close_html(['<','/',InTag,'>' | Tokens], [], Tag, Tokens)
	:-
	make_lc_sym(InTag, Tag),
	!.
read_to_close_html(['<',InTag,'>' | Tokens], [], Tag, 
					['<',ContainingTag,'>' | Tokens])
	:-
	make_lc_sym(InTag, ContainingTag),
	start_can_terminate(ContainingTag, Tag),
	!.


read_to_close_html(['<','/',InTag,'>' | Tokens], [], Tag, 
					['<','/',ContainingTag,'>' | Tokens])
	:-
	make_lc_sym(InTag, ContainingTag),
	end_can_terminate(ContainingTag, Tag),
	!.

%% Text appearance tag which has been close by earlier // structure:  a, b, /a, /b
read_to_close_html(['<','/',InTag,'>' | InterTokens], [], CurTag, Tokens)
	:-
	make_lc_sym(InTag, LCInTag),
	text_appearance_tag(LCInTag),
	end_can_terminate(CurTag, LCInTag),
	!,
	read_to_close_html(InterTokens, [], CurTag, Tokens).

	%% on Yahoo, sometimes:
read_to_close_html(['<','/',InTag,',','>' | Tokens], [], Tag, Tokens)
	:-
	dmember(Tag, [td,a]),
	make_lc_sym(InTag, Tag).

read_to_close_html(Tokens, [Term | SubTerms], Tag, RestTokens)
	:-
	read_pxml_term(Tokens, Term, InterTokens),
	read_to_close_html(InterTokens, SubTerms, Tag, RestTokens).


	%%------------------------------------------
	%% Syntactic roles of tags:
	%%------------------------------------------
unary_tag(hr).
unary_tag(br).
unary_tag(p).
unary_tag('!doctype').
unary_tag(meta).
unary_tag(img).
unary_tag(input).
unary_tag(frame).
unary_tag(link).
%unary_tag(option).

containing_tag(html, X).
containing_tag(body, X)
	:-
	X \= html.
containing_tag(head, X)
	:-
	X \= html,
	X \= body.
containing_tag(table, X)
	:-
	table_tag(X).
containing_tag(table, X)
	:-
	text_appearance_tag(X).
containing_tag(form, X)
	:-
	table_tag(X).
containing_tag(form, X)
	:-
	text_appearance_tag(X).

containing_tag(tr, td).
containing_tag(tr, X)
	:-
	text_appearance_tag(X).
containing_tag(td, X)
	:-
	text_appearance_tag(X).

table_tag(tr).
table_tag(td).

text_appearance_tag(font).
text_appearance_tag(b).
text_appearance_tag(i).

start_can_terminate(td, td).
start_can_terminate(tr, td).
start_can_terminate(tr, tr).

end_can_terminate(tr, td).
end_can_terminate(table, td).
end_can_terminate(form, td).
end_can_terminate(table, tr).
end_can_terminate(form, tr).

end_can_terminate(table, center).
end_can_terminate(tr, center).
end_can_terminate(td, center).

end_can_terminate(table, font).
end_can_terminate(tr, font).
end_can_terminate(td, font).

end_can_terminate(map, area).

end_can_terminate(td, X) :- text_appearance_tag(X).
end_can_terminate(tr, X) :- text_appearance_tag(X).
end_can_terminate(table, X) :- text_appearance_tag(X).
end_can_terminate(a, X) :- text_appearance_tag(X).

end_can_terminate(font, T2) 
	:-
	text_appearance_tag(T2).

end_can_terminate(body, X) :-
	X \= head.
end_can_terminate(html, _).

start_can_terminate(dd, dt).
start_can_terminate(dt, dd).
end_can_terminate(dl, dd).

start_can_terminate(li, li).
start_can_terminate(ul, li).
start_can_terminate(ol, li).
end_can_terminate(ul, li).
end_can_terminate(ol, li).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Input: Parse DTD files
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	/* Very simple DTD parser -- not complete */
export parse_dtd/2.
parse_dtd(DTDTokens, DTDTerm)
	:-
	parse_dtd(DTDTokens, DTDTerm, Remainder).

parse_dtd([], [], []).
parse_dtd(DTDTokens, [DTDTerm | DTDTermList], Remainder)
	:-
	dtde(DTDTerm, DTDTokens, RestDTDTokens),
	!,
	parse_dtd(RestDTDTokens, DTDTermList, Remainder).
parse_dtd(Remainder, [], Remainder).

	%% comment
dtde(comment(List)) --> ['<','!--'], {!},  read_comment(List).

read_comment(List, In, Out)
	:-
	rc(In, List, Out).

rc(['--','>' | Out], [], Out).
rc([Item | RestIn], [Item | List], Out)
	:-
	rc(RestIn, List, Out).

	%% !ELEMENT
dtde(element(Name, Parts)) 
	--> ['<'], keyword('!element'), {!},  [Name], eparts(Parts), ['>'].

eparts(Parts) --> ['('], eparts_list(Parts0, Kind), [')'], {mk_parts_list(Kind, Parts0, Parts)}.
eparts([]) --> ['EMPTY'].
mk_parts_list(Kind, Parts0, Parts)
	:-
	nonvar(Kind),
	!,
	Kind = disj,
	Parts = disj(Parts0).

mk_parts_list(Kind, Parts, Parts).

eparts_list([Part | Parts],Kind) --> 
	epart(Part), [Punct], {Punct=',' ; Punct='|',Kind=disj}, eparts_list(Parts,Kind).

eparts_list([Part],_) --> epart(Part).
eparts_list([], _, X, X).

/*
eparts(disj(Parts)) --> ['('], eparts_disj_list(Parts), [')'].
eparts(Parts) --> ['('], eparts_list(Parts), [')'].
eparts([]) --> ['EMPTY'].

eparts_disj_list([Part | Parts]) --> epart(Part), ['|'], eparts_disj_list(Parts).
eparts_disj_list([Part]) --> epart(Part).
eparts_disj_list([],X,X).

eparts_list([Part | Parts]) --> epart(Part), [','], eparts_list(Parts).
eparts_list([Part]) --> epart(Part).
eparts_list([], X, X).
*/

epart(Parts) --> ['('], eparts_list(Parts0,Kind), [')'], {mk_parts_list(Kind, Parts0, Parts)}.
epart(MPart) --> [Part], part_modifier(Mod), {part_modify(Mod, Part, MPart)}.
epart(Part) --> [Part].

part_modifier(Mod) --> [Mod], {is_part_modifier(Mod)}.
is_part_modifier('+').
is_part_modifier('*').
is_part_modifier('?').

	%% !ATTLIST
dtde(attlist(Name, Tag, Value, Option)) 
	--> ['<'], keyword('!attlist'), {!}, 
		[Name], [Tag], attlist_value(Value), attlist_option(Option), ['>'].

attlist_value(disj(Value)) --> ['('], disjunct_list(Value), [')'], {!}.
attlist_value('#cdata') --> keyword('#cdata').

attlist_option('#required') --> keyword('#required'), {!}.
attlist_option('#implied') --> keyword('#implied'), {!}.
attlist_option(text(Text)) --> [Text].

disjunct_list([Item | Items]) --> [Item], ['|'], disjunct_list(Items).
disjunct_list([Item]) --> [Item].
disjunct_list([], X, X).

keyword(What) --> [Item], {keyword(What, Item)}.

keyword(What, What) :-!. 
keyword('!element', '!ELEMENT') :-!.
keyword('!attlist', '!ATTLIST') :-!.
keyword('#cdata', 'CDATA') :-!.
keyword('#required', '#REQUIRED') :-!.
keyword('#implied', '#IMPLIED') :-!.
keyword(What, Item) 
	:-
	atom_codes(Item, ItemCs),
	make_lc(ItemCs, LCItemCs),
	atom_codes(What, LCItemCs).

part_modify(Mod, Part, MPart)
	:-
	MPart =.. [Mod, Part].

quoted_text(Text, Target, In, Out)
	:-
	read_to(In, Target, Text, Out).

items([Item | Items]) --> [Item], items(Items).
items([Item]) --> [Item].
items([], X, X).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Output:  PXML --> HTML
	%%%% Prolog Markup Language to eXtensible Markup Language
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export writeXml/2.
writeXml(L, S)
	:-
	pml2xml(L, S, '  ', []).
export writeXml/3.
writeXml(L, S, Indent)
	:-
	pml2xml(L, S, Indent, []).

export pml2xml/4.
pml2xml([], S, _, _).
pml2xml([T | RT], S, Ind, IStack) 
	:-
	T = [_|_],
	!,
	pml2xml(T, S, Ind, IStack),
	pml2xml(RT, S, Ind, IStack).

pml2xml([T | RT], S, Ind, IStack) 
	:-!,
	pml2xml(T, S, Ind, IStack),
	nl(S), 
	pml2xml(RT, S, Ind, IStack).
pml2xml(comment(L), S, Ind, IStack) 
	:-!,
	write(S, '<--!'),nl(S),
	pml_cmt(L,S),
	write(S, '-->').
pml2xml(T, S, Ind, IStack) 
	:-
	functor(T, Functor, Arity),
	out_istack(IStack, S),
	pml2xml0(Arity, Functor, T, S, Ind, [Ind | IStack]),
	!.
pml2xml(T, S, Ind, IStack) 
	:-
	copy_term(pml(error(CT)), ThrowTerm),
	throw(ThrowTerm).

pml2xml0(0, newline, _, S, Ind, IStack) 
	:-!,
	nl(S). 
pml2xml0(0, Functor, _, S, Ind, IStack) 
	:-!,
	write(S, Functor).
pml2xml0(1, Functor, comment(L), S, Ind, IStack) 
	:-!,
	write(S, '<!--'),nl(S),
	pml_cmt(L,S),
	write(S, '-->').
pml2xml0(2, Functor, comment(_,L), S, Ind, IStack) 
	:-!,
	write(S, '<!--'),nl(S),
	pml_cmt(L,S),
	write(S, '-->').
pml2xml0(1, Functor, _, S, Ind, IStack) 
	:-!,
	write(S, '<'),write(S, Functor), write(S, '>').
pml2xml0(2, Functor, T, S, Ind, IStack) 
	:-
	unary_tag(Functor),
	!,
	write(S, '<'),write(S, Functor),
	arg(1, T, Options),
	write_options(Options, S),
%	pml2xml([], S),
	write(S, '>'),
	nl(S). 
pml2xml0(2, Functor, T, S, Ind, IStack) 
	:-!,
	write(S, '<'),write(S, Functor),
	arg(1, T, Options),
	write_options(Options, S),
	arg(2, T, B),
	(B = [] ->
		write(S, '/>')
		;
		write(S, '>'),
		nl(S), 
		pml2xml(B, S, Ind, IStack),
		pop_istack(IStack, PoppedIStack),
		out_istack(PoppedIStack, S),
		write(S, '</'), write(S, Functor), write(S, '>')
	).
pml2xml0(N, Functor, T, S, Ind, IStack) 
	:-
	copy_term(pml(bad_arity(CT)), ThrowTerm),
	throw(ThrowTerm).

write_options([], _).
write_options([O | RO], S) 
	:-
	write(S, ' '), 
	write_option(O, S, Ind, IStack), 
	write_options(RO, S).
write_options(R, S)
	:-
	copy_term(pml(improper_options_list(R)),ThrowTerm),
	throw(ThrowTerm).

write_option(T=V, S, Ind, IStack)
	:-!,
	escape_quotes(V, XV),
	printf(S, '%t="%t"',[T,XV]).
write_option(O, S, Ind, IStack)
	:-
	write(S, O). 

	%% Need proper implementation:
escape_quotes(V, V).

unescape_quotes(V, V).


pml_cmt([], _).
pml_cmt([T | L], S)
	:-
	write(S, T), nl(S), 
	pml_cmt(L, S).

out_istack([], _).
out_istack([Item | IStack], S)
	:-
	write(S, Item),
	out_istack(IStack, S).

pop_istack([_ | PoppedIStack], PoppedIStack) :-!.
pop_istack(IStack, IStack).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Output:  Macro PXML --> XML 
	%%%% Prolog Markup Language to eXtensible Markup Language
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export mpml2xml/2.
mpml2xml([], S).
mpml2xml([T | RT], S) 
	:-
	macro_expand(T, NT),
%	pml2xml([NT], S),
	pml2xml([NT], S, '  ', []).
	mpml2xml(RT, S).

export mpml2xml_list/3.
mpml2xml_list([], Tail, Tail).
mpml2xml_list([T | RT], [NT | RR], Tail) 
	:-
	macro_expand(T, NT),
	mpml2xml_list(RT, RR, Tail).  


macro_expand([], []).
macro_expand([T | RT], [FT | RFT]) 
	:-
	macro_expand(T, FT),
	macro_expand(RT, RFT).
macro_expand(T, FT) 
	:-
	mpml:macro(T, NT),
	(NT = [_|_] ->
		macro_expand(NT, FT)
		;
		macro_expand_body(NT, FT)
	).
macro_expand(T, FT) 
	:-
	macro_expand_body(T, FT).

macro_expand_body(T, FT) 
	:-
	functor(T, Functor, Arity),
	macro_expand_body0(Arity, Functor, T, FT).

macro_expand_body0(0, F, T, T).
macro_expand_body0(1, F, T, T).
macro_expand_body0(2, F, T, NT) 
	:-
	arg(1, T, O),
	arg(2, T, B),
	macro_expand(B, NB),
	NT =.. [F, O, NB].

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Input: Fetching Documents
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export fetch_url/2.
fetch_url(RequestDescription, Response)
	:-
	build_http_request(RequestDescription, HTTP_Request),
	dmember(host=Host, RequestDescription),
	(dmember(port=Port, RequestDescription) -> true ; Port = 80),
	(dmember(timeout=Timeout, RequestDescription) -> true ; default_timeout(Timeout)),
	nsocket(internet, stream, 0, Socket),
	open(nsocket(Socket), read, RS, []),
	open(nsocket(Socket), write, WS, []),
	nsocket_connect(Socket, Host, Port),
	write_lines(WS, HTTP_Request),
	nl(WS),flush_output(WS),
%	nsocket_select([RS], [], [], [Mark], _, _, Timeout:0),
%	Mark = set,
	!,
	get_lines(RS, Response).


build_http_request(RequestDescription, HTTP_Request)
	:-
	(dmember(method=Method, RequestDescription) -> true ; Method = 'GET'),
	dmember(docpath=DocPath, RequestDescription),
	http_level(HTTPLev),
	sprintf(atom(HTTP_Request), '%t %t %t',[Method,DocPath,HTTPLev]). 

http_level('HTTP/1.0').


default_timeout(300).  %% time in secs

endmod.

