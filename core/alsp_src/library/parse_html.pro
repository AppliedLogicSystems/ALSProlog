/*=================================================================*
 |			parse_html.pro
 |		Copyright (c) 1999-2004 Applied Logic Systems, Inc.
 |	
 |		Parse tokenized html into pml prolog terms
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

module pml.


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Parser:  HTML --> PXML
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export parse_html_toks_to_pml/6.
export grab_pml/2.
export read_pml_term/3.
export read_pml_comment/3.

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/

grab_pml(Path, PXML)
    :-
    grab_html_tokens(Path, Tokens),
    parse_html_toks_to_pml(Tokens, PXML, [], _, [], _).

	%%------------------------------------------
	%% Parse the token stream
	%%------------------------------------------

/*------------------------------------------------------------------------------
 |	parse_html_toks_to_pml(Tokens, Terms, MTags, RestMTags, MTagVals, ResultMTagVals).
 |
 |  Parses a list of HTML-tokens, as produced by 
 | 		read_tokens/5
 |  in html_tokens.pro, into a collection of Prolog Terms consituting a
 |  PXML representation of the source.
 |
 |  	MTags, RestMTags, MTagVals, ResultMTagVals
 |
 |  provide a means of capturing components of the PXML output.
 |  Below, MTags is a list of non-comment tags.  
 |  Often, MTags = [body].
 |  MTagVals is the list of corresponding PXML terms found, if any.
 |  So if MTags = [body, table], we might have MTagVals = [body=Body], if
 |  there were no table, and 
 |	MTagVals = [body=Body, table=[list of PXML table terms] ]
 |  if there was more than one table.
 |
 |  If MTags and MTagVals was to be big, it could be carried as
 |  an AVL tree.  But I think the examples above will be typical.
 *-----------------------------------------------------------------------------*/
parse_html_toks_to_pml([], [], MTags, MTags, MTagVals, MTagVals).
parse_html_toks_to_pml(Tokens, [Term | RestTerms], MTags, RestMTags, MTagVals, RestMTagVals)
	:-
	read_pml_term(Tokens, Term, RestTokens, MTags, InterMTags, MTagVals, InterMTagVals),
	parse_html_toks_to_pml(RestTokens, RestTerms, InterMTags, RestMTags, InterMTagVals, RestMTagVals).

read_pml_term([string(StringAtom) | RestTokens], StringAtom, RestTokens, 
							MTags, MTags, MTagVals, MTagVals)
	:-!.

/*
%%??: Is this needed??
read_pml_term(['<', InTag,'>','<', InTag,'>' | Tokens], Term, RestTokens)
	:-
	make_lc_sym(InTag, html),
	!,
	read_pml_term(['<', InTag,'>' | Tokens], Term, RestTokens).
*/

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
read_pml_term(['<', InTag | Tokens], Term, RestTokens,
				MTags, InterMTags, MTagVals, InterMTagVals)
	:-
	make_lc_sym(InTag, Tag),
	unary_tag(Tag),
	!,
	read_pml_eqs_to(Tokens, '>', Features, RestTokens),

	Term  =.. [Tag, Features, []],
	handle_tag(Tag, Term, MTags, InterMTags, MTagVals, InterMTagVals).

read_pml_term(['{', InTag | Tokens], Term, RestTokens,
				MTags, MTags, MTagVals, MTagVals)
	:-!,
	read_pml_eqs_to(Tokens, '}', Features, RestTokens),
	Term  =.. ['{}', Features].

read_pml_term(['<', '!--' | Tokens], Term, RestTokens,
				MTags, MTags, MTagVals, MTagVals)
	:-
	read_pml_comment(Tokens, Features, RestTokens),
	!,
	Term  =.. [comment, Features, []].

read_pml_term(['<', script | Tokens], _, RestTokens,
				MTags, MTags, MTagVals, MTagVals)
	:-!,
	cross_toks_to(Tokens, ['<','/',script,'>'], RestTokens).

read_pml_term(['<', InTag | Tokens], Term, RestTokens,
				MTags, InterMTags, MTagVals, InterMTagVals)
	:-!,
	make_lc_sym(InTag, Tag),
	read_pml_eqs_to(Tokens, '>', Features, InterTokens),
	!,
	read_to_close_html(InterTokens, SubTerms, Tag, RestTokens,
				MTags, InterMTagsA, MTagVals, InterMTagValsA),
	Term  =.. [Tag, Features, SubTerms],
	handle_tag(Tag, Term, InterMTagsA, InterMTags, InterMTagValsA, InterMTagVals).

cross_toks_to(Tokens, TerminList, RestTokens)
	:-
	TerminList = [Termin1 | RestTermins],
	do_cross_toks_to(Tokens, Termin1, RestTermins, RestTokens).

do_cross_toks_to([Termin1 | InitRestTokens], Termin1, RestTermins, RestTokens)
	:-
	do_match(RestTermins, InitRestTokens, RestTokens),
	!.

do_cross_toks_to([X | TokensTail], Termin1, RestTermins, RestTokens)
	:-
	do_cross_toks_to(TokensTail, Termin1, RestTermins, RestTokens).

do_match([], RestTokens, RestTokens).

do_match([Termin | RestTermins], [Termin | InitRestTokens], RestTokens)
	:-!,
	do_match(RestTermins, InitRestTokens, RestTokens).

read_pml_term(Tokens, List, ['<' | RestTokens],
				MTags, InterMTags, MTagVals, InterMTagVals)
	:-
	read_pml_terms_to(Tokens, '<', List, RestTokens,
				MTags, InterMTags, MTagVals, InterMTagVals).

read_pml_terms_to([], Terminator, [], [],
				MTags, MTags, MTagVals, MTagVals).

read_pml_terms_to([Terminator | Tokens], Terminator, [], Tokens,
				MTags, MTags, MTagVals, MTagVals).
	:-!.

read_pml_terms_to(['/','>' | Tokens], _, [], Tokens,
				MTags, MTags, MTagVals, MTagVals).
	:-!.

read_pml_terms_to(['{' | Tokens], Terminator, [Grp | Terms], RestTokens,
				MTags, InterMTags, MTagVals, InterMTagVals)
	:-!,
	read_pml_terms_to(Tokens, '}', GrpSubTerms, InterRestTokens,
				MTags, InterMTagsA, MTagVals, InterMTagValsA),
	Grp =..['{}' | GrpSubTerms],
	read_pml_terms_to(InterRestTokens, Terminator, Terms, RestTokens,
				InterMTagsA, InterMTags, InterMTagValsA, InterMTagVals).

read_pml_terms_to([T0 | Tokens], Terminator, [T0 | Terms], RestTokens,
				MTags, InterMTags, MTagVals, InterMTagVals)
	:-
	read_pml_terms_to(Tokens, Terminator, Terms, RestTokens,
				MTags, InterMTags, MTagVals, InterMTagVals).

read_pml_eqs_to([], Terminator, [], []).
read_pml_eqs_to([Terminator | Tokens], Terminator, [], Tokens)
	:-!.
read_pml_eqs_to(['/','>' | Tokens], '>', [], Tokens)
	:-!.
read_pml_eqs_to(['<' | Tokens], '>', [], ['<' | Tokens])
	:-!.
read_pml_eqs_to([T0, '=', T1 | Tokens], Terminator, 
				 [(Tag = Value) | Terms], RestTokens)
	:-!,
	make_lc_sym(T0, Tag),
	read_tag_value(T1, Tokens, Value, InterTokens),
	read_pml_eqs_to(InterTokens, Terminator, Terms, RestTokens).
read_pml_eqs_to([T0 | Tokens], Terminator, [T0 | Terms], RestTokens)
	:-
	read_pml_eqs_to(Tokens, Terminator, Terms, RestTokens).

read_tag_value('"', Tokens, Value, InterTokens)
	:-
	consume_tokens_to_q2(Tokens, Head, InterTokens),
	catenate(Head, Value0),
	unescape_quotes(Value0, Value).
read_tag_value(Value, Tokens, Value, Tokens).

consume_tokens_to_q2([], Head, []) :-!, fail.
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

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
read_pml_comment([], [], []).
read_pml_comment(['--','>' | Tokens], [], Tokens)
	:-!.
read_pml_comment(['!--','>' | Tokens], [], Tokens)
	:-!.
read_pml_comment(['//--','>' | Tokens], [], Tokens)
	:-!.
read_pml_comment([Token | Tokens], [Token | Features], RestTokens)
	:-
	read_pml_comment(Tokens, Features, RestTokens).


read_to_close_html([], [], _, [],
				MTags, MTags, MTagVals, MTagVals).
read_to_close_html(['/','>' | Tokens], [], Tag, Tokens,
				MTags, MTags, MTagVals, MTagVals)
	:-!.
read_to_close_html(['<','/',InTag0,'>','<','/',InTag1,'>' | Tokens], 
                       [], Tag, Tokens,
				MTags, MTags, MTagVals, MTagVals)
	:-
	Tag=font,
	make_lc_sym(InTag0, Tag),
	make_lc_sym(InTag1, Tag),
	!.
read_to_close_html(['<','/',InTag0,'>','<','/',InTag,'>' | Tokens], 
                       [], Tag, Tokens,
				MTags, MTags, MTagVals, MTagVals)
	:-
	make_lc_sym(InTag0, font),
	member(Tag,[td,tr,table]),
	make_lc_sym(InTag, Tag),
	!.

read_to_close_html(['<','/',InTag,'>' | Tokens], [], Tag, Tokens,
				MTags, MTags, MTagVals, MTagVals)
	:-
	make_lc_sym(InTag, Tag),
	!.

read_to_close_html(['<',InTag,'>' | Tokens], [], Tag, 
					['<',ContainingTag,'>' | Tokens],
				MTags, MTags, MTagVals, MTagVals)
	:-
	make_lc_sym(InTag, ContainingTag),
	start_can_terminate(ContainingTag, Tag),
	!.


read_to_close_html(['<','/',InTag,'>' | Tokens], [], Tag, 
					['<','/',ContainingTag,'>' | Tokens],
				MTags, MTags, MTagVals, MTagVals)
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

read_to_close_html(Tokens, [Term | SubTerms], Tag, RestTokens,
				MTags, InterMTags, MTagVals, InterMTagVals)
	:-
	read_pml_term(Tokens, Term, InterTokens,
				MTags, InterMTagsA, MTagVals, InterMTagValsA),
	!,
	read_to_close_html(InterTokens, SubTerms, Tag, RestTokens,
				InterMTagsA, InterMTags, InterMTagValsA, InterMTagVals).


	%%------------------------------------------
	%% Syntactic roles of tags:
	%%------------------------------------------
unary_tag(hr).
unary_tag(br).
%unary_tag(p).
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

end_can_terminate(table, span).
end_can_terminate(tr, span).
end_can_terminate(td, span).

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


	%% assumes unique occurrences of Tag(s) in list:
xtrctl([], _, _)
	:-!,
	fail.

xtrctl([Tag | Tags], Tag, Tags)
	:-!.

xtrctl([OTag | Tags], Tag, [OTag | OTags])
	:-
	xtrctl(Tags, Tag, OTags).

% -------- Tests for xtrctl:

tx1 :-
	Tags = [table, body, head],
write(Tags),nl,
	xtrctl(Tags, body, OTags1),
write(OTags1),nl,
	xtrctl(OTags1, table, OTags2),
write(OTags2),nl.

tx2 :-
	Tags = [table, body, head],
write(Tags),nl,
write('Try extract ''form'':'),nl,
	xtrctl(Tags, form, OTags1).

% -------- End Tests for xtrctl


xtrct_eqn([], _, _, _)
	:-!,
	fail.

xtrct_eqn([Tag=Val | Eqns], Tag, Tag=Val, Eqns)
	:-!.

xtrct_eqn([OTag=OVal | Eqns], Tag, Result, [OTag=OVal | OEqns])
	:-
	xtrct_eqn(Eqns, Tag, Result, OEqns).


handle_tag(Tag, Term, MTags, InterMTags, MTagVals, InterMTagVals)
	:-
	xtrctl(MTags, Tag, InterMTags),
	!,
	do_handle_eqn( MTagVals, Tag, InterMTagVals).  

handle_tag(_, _, MTags, MTags, MTagVals, MTagVals).

do_handle_eqn( MTagVals, Tag, InterMTagVals)
    	:-
	xtrct_eqn(MTagVals, Tag, Tag=InitTagVals, TmpMTagVals), 
	!,
	InterMTagVals = [Tag=[Term | InitTagVals] | TmpMTagVals].

do_handle_eqn( MTagVals, Tag, InterMTagVals)
    	:-
	InterMTagVals = [Tag=[Term] | MTagVals].


% -------- Tests for xtrct_eqn:

txy1 :-
	Eqns = [table=[tblTop,bot], body=[bod], head=[hh]],
	xtrct_eqn(Eqns, body, Result1, OEqns1),
write(Result1), write('  '), write(OEqns1),nl,
	xtrct_eqn(OEqns1, table, Result2, OEqns2),
write(Result2), write('  '), write(OEqns2),nl.

txy2 :-
	Eqns = [table=[tblTop,bot], body=[bod], head=[hh]],
	xtrct_eqn(Eqns, form, Result1, OEqns1).

% -------- End Tests for xtrct_eqn
	
endmod.

