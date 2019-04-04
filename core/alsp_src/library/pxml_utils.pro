	%% REMOVE THESE CONSULTS WHEN library/[html_tokens, parse_html]
	%% ARE UPDATED, AND pxml_utils.pro IS ADDED TO THE LIBRARY.
	%% unary_tag/1 MUST BE A LIBRARY FUNCTION (called below).
%% Force most recent:
%:-['../ALSProlog/core/alsp_src/library/html_tokens.pro'].
%:-['parse_html.pro'].

/*=================================================================*
 |			pxml_utils.pro
 |		Copyright (c) 2019 Applied Logic Systems, Inc.
 |			Group: Web
 |			DocTitle: write_pxml_to_html/3
 |	-- Converts pxml (tokenized & parsed html) back to html
 *=================================================================*/

	
module pxml.

export write_pxml_to_html_list/3.
export write_pxml_to_html/3.


/*!---------------------------------------------------------------------
 |	write_pxml_to_html_list/3
 |	write_pxml_to_html_list(PXML_List, OS, InD)
 |	write_pxml_to_html_list(+, +, +)
 |
 |	- writes a list of PXML terms out as HTML
 |
 |	PXML_List is a list of PXML terms, OS is an Output Stream,
 |	and InD is an integer specifying an (initial/ or current)
 |	amount of indent.  Converts each element of PXML_List
 |	to HTML and writes that out on stream OS, using IndD as the
 |	starting amount of blank spaces indent.  The default amount
 |	of increment of the indent is 2, controlled by
 |		 nxt_ind(2).
 *!--------------------------------------------------------------------*/

write_pxml_to_html_list([], OS, _)
	:-!.

write_pxml_to_html_list([Pxml1 | RestPxml], OS, InD)
	:-!,
	write_pxml_to_html(Pxml1, OS, InD),
	write_pxml_to_html(RestPxml, OS, InD).
	
/*!---------------------------------------------------------------------
 |	write_pxml_to_html/3
 |	write_pxml_to_html(Pxml, OS, InD)
 |	write_pxml_to_html(+, +, +)
 |
 |	- writes a single PXML term out as HTML
 |
 |	Pxml is a single PXML term, OS is an Output Stream,
 |	and InD is an integer specifying an (initial/ or current)
 |	amount of indent.  Converts Pxml to HTML and writes that out 
 |	on stream OS, using IndD as the starting amount of blank spaces 
 |	indent.  The default amount of increment of the indent is 2, 
 |	controlled by
 |		 nxt_ind(2).
 *!--------------------------------------------------------------------*/

write_pxml_to_html([], OS, _)
	:-!.

write_pxml_to_html([Pxml1 | RestPxml], OS, InD)
	:-!,
	write_pxml_to_html_list([Pxml1 | RestPxml], OS, InD).

write_pxml_to_html(Pxml, OS, InD)
	:-
	Pxml =.. [Tag, Features, Content],
	!,
	disp_write_pxml_to_html(Tag, Features, Content, OS, InD).

write_pxml_to_html(Pxml, OS, InD)
	:-
	printf(OS, '%t ', [Pxml]).

disp_write_pxml_to_html(Tag, Features, Content, OS, InD)
	:-
	unary_tag(Tag),
	!,
		%% should be no Features or Content??
	get_indent(InD, PhysInD),
	printf(OS, '%t<%t ',[PhysInD,Tag]),
	write_features(Features, OS),
	printf(OS, '>\n',[]),

	incri(InD, NxtInD),
	get_indent(NxtInD, NxtPhysInD),
	printf(OS, '%t', [NxtPhysInD]),
	write_pxml_to_html_list(Content, OS, NxtInD).
	
disp_write_pxml_to_html(Tag, Features, Content, OS, InD)
	:-
%printf('%t <<disp NON UN TAG>>\n', [Tag]),
	get_indent(InD, PhysInD),
	printf(OS, '%t<%t ',[PhysInD,Tag]),
	write_features(Features, OS),
	Content = [C1 | _],
%(C1==option -> trace ; true),
	(( C1=[D1|_], atomic(D1)) -> Flat=true ; Flat=false),
	(Flat==true -> printf(OS, '> ',[]) ; printf(OS, '>\n',[])),

	incri(InD, NxtInD),
	get_indent(NxtInD, NxtPhysInD),
	(Tag==style ->
		Content = [StylesList],
		printf(OS, '\n', []),
	 	write_style_html_list(StylesList, OS, NxtInD)
		;
	 	write_pxml_to_html_list(Content, OS, NxtInD)
	),

	(Flat==true -> printf(OS, '</%t>\n',[Tag]) 
		;
		printf(OS, '%t</%t>\n',[NxtPhysInD,Tag])
	).

write_features([], OS).
write_features([FF | RestFeatures], OS)
	:-
	write_feat(FF, OS),
	write_features(RestFeatures, OS).

write_feat(Tg=Val, OS)
	:-
	printf(OS, '%t="%t" ',[Tg,Val]).
write_feat(FF, OS)
	:-
	printf(OS, '%t ', [FF]).
	
write_style_html_list([], OS, _).
%write_style_html_list([STag, StyleValsList | StylesList], OS, InD)
write_style_html_list(StylesList, OS, InD)
	:-
	get_sTags_to_braces(StylesList, STagPartsList, BracesTerm, RestStylesList),
	catenate(STagPartsList, STag),
	get_indent(InD, PhysInD),
	printf(OS, '%t%t {', [PhysInD,STag]),
	BracesTerm =.. ['{}' | ValsList],
	break_into_tag_groups(ValsList, Groups),
	write_style_vals(Groups, OS),
	printf(OS, '}\n', [PhysInD,STag]),
	write_style_html_list(RestStylesList, OS, InD).

get_sTags_to_braces([BracesTerm | RestStylesList], [], BracesTerm, RestStylesList)
	:-
	functor(BracesTerm, '{}', _),
	!.
get_sTags_to_braces([PartTag | StylesList], [PartTag | RestPTags], BracesTerm, RestStylesList)
	:-
	get_sTags_to_braces(StylesList,  RestPTags, BracesTerm, RestStylesList).

break_into_tag_groups([], []).
break_into_tag_groups(ValsList, [g(Tag, TagVals) | RestGroups])
	:-
	split_ll_at_start(ValsList, ':', Tag, Tail, _),
        split_atoms_list_at_end(Tail, ';', TagVals, RestValsList, _),
	break_into_tag_groups(RestValsList, RestGroups).

write_style_vals([], OS)
	:-!.
write_style_vals([g([],_)|TT], OS)
	:-!,
	write_style_vals(TT, OS).
	
write_style_vals([g(TagList, TagVals) | StyleVals], OS)
	:-
	TagList = [Tag],
	printf(OS, ' %t: ', [Tag]),
	write_tag_vals(TagVals, OS),
	write_style_vals(StyleVals, OS).

write_tag_vals([], OS).
write_tag_vals([Val | TagVals], OS)
	:-
	printf(OS, '%t ', [Val]),
	write_tag_vals(TagVals, OS).

split_atoms_list_at_end([], _, [], [], '').

split_atoms_list_at_end([Atom | Tail], Init, [], Tail, Atom)
        :-
	atom(Atom),
        sub_atom(Atom, _, 1, 0, Init),
        !.
split_atoms_list_at_end([NonAtom | Tail], Init, [], Tail, Atom)
        :-
	to_atom(NonAtom, XAtom),
        sub_atom(XAtom, _, 1, 0, Init),
        !.

split_atoms_list_at_end([Atom | Atoms], Init, [Atom | Head], Tail, Splitter)
        :-
        split_atoms_list_at_end(Atoms, Init, Head, Tail, Splitter).

to_atom(NonAtom, XAtom)
	:-
	number(NonAtom),
	!,
	number_chars(NonAtom, NACs),
	atom_chars(XAtom, NACs).

to_atom(NonAtom, XAtom)
	:-
	sprintf(Tmp, '%t', [NonAtom]), 
	atom_codes(XAtom, Tmp).

get_indent(-1, '') :-!.
get_indent(NInd, PhysInd)
	:-
	n_of(NInd, ' ', SpList),
	catenate(SpList, PhysInd).
	
nxt_ind(2).

incri(-1,-1) :-!.
incri(N,M) 
	:-
	nxt_ind(K),
	M is N+K.

decri(-1,-1) :-!.
decri(N,M) 
	:-
	nxt_ind(K),
	M is N-K.

endmod.

t1 :- grab_terms('./unix/tfad.pxml', [Pxml|_]), 
	t_p2h(Pxml).

t2 :- grab_terms('./unix/tfad.pxml', [Pxml|_]), 
	nt_p2h(Pxml).

t3 :- grab_terms('./unix/tfad.pxml', [Pxml|_]), 
	t_p2h(Pxml).

t4 :- grab_terms('./unix/tfad.pxml', Pxml_List), 
	OutFile = 'tfad.pxml.html',
	open(OutFile, write, OS),
	write_pxml_to_html_list(Pxml_List, OS, 0),
	close(OS).

t_p2h(Pxml_List)
	:-
	OS = user,
	write_pxml_to_html_list(Pxml_List, OS, 0).
	
nt_p2h(Pxml_List)
	:-
	OS = user,
	write_pxml_to_html_list(Pxml_List, OS, -1).
