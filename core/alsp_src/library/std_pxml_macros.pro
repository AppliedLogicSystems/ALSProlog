/*=================================================================*
 |			std_pxml_macros.pro
 |		Copyright (c) 1999 Applied Logic Systems, Inc.
 |	
 |		Standard macro set for use with pxml output
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


module mpml.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Standard PXML Macros
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%%-----------------------
	%% ---- Text Matters ----
	%%-----------------------

macro(ref(URL0,   Content), Result)
	:-
	macro_ref(URL0,   Content, Result).

macro_ref(URL/Options,   Content, a([href=URL | Options], [Content])) :-!.
macro_ref(URL,   Content, a([href=URL], [Content])) :-!.

macro(img(SRC,   Options), img([src=SRC | Options],[])) :-!.
macro(img(SRC), img([src=SRC],[])) :-!.

macro( br, br([])) :-!.
macro( hr, hr([])) :-!.
macro( p, p([],[])) :-!.

macro(h1(X), h1([], X)).
macro(h2(X), h2([], X)).
macro(h3(X), h3([], X)).
macro(h4(X), h4([], X)).
macro(h5(X), h5([], X)).
macro(h6(X), h6([], X)).
macro(b(X), b([], X)).
macro(i(X), i([], X)).
macro(u(X), u([], X)).
macro(tt(X), tt([], X)).
macro(center(X), center([], X)).
macro(title(X), title([], X)).
macro(blockquote(X), blockquote([], X)).
macro(pre(X), pre([], X0))
	:-
	pml_add_nl(X, X0).
macro(pre(Opts,X), pre(Opts, X0))
	:-
	pml_add_nl(X, X0).

pml_add_nl([], []).
pml_add_nl([T | L], [T, '\n' | L0])
	:-
	pml_add_nl(L, L0).



macro( heading(N, Content), HNTerm)
	:-!,
	integer(N), 1 =< N, N =< 5,
	catenate(h, N, HNF),
	HNTerm =.. [HNF, [], Content].
	
	%%-----------------------
	%% ---- List Matters ----
	%%-----------------------

macro( itemize(List), ul([], LiList) )
	:-!,
	findall(li([],X), member(X, List), LiList).

macro( ul(List), ul([], LiList) )
	:-!,
	findall(li([],X), member(X, List), LiList).

macro( ol(List), ol([], LiList) )
	:-!,
	findall(li([],X), member(X, List), LiList).

macro( bl(List), bl([], LiList) )
	:-!,
	findall(li([],X), member(X, List), LiList).


macro( dl(List), dl([], LiList) )
	:-!,
	findall(dt([],X), member(X, List), LiList).
macro( deflist(List), dl([], LiList) )
	:-!,
	findall(dt([],X), member(X, List), LiList).

	%%-----------------------
	%% ---- Table Matters ----
	%%-----------------------

macro( row(List), tr([], XList) )
	:-!,
	table_row_list(List, XList).

table_row_list([], []).
table_row_list([Cell | List], [XCell | XList])
	:-
	table_cell(Cell, XCell),
	table_row_list(List, XList).

table_cell(td(Options,Content), td(Options,Content)) :-!.
table_cell(td(Content),         td([],Content)) :-!.
table_cell(th(Options,Content), th(Options,Content)) :-!.
table_cell(th(Content),         th([],Content)) :-!.
table_cell(Cell,                td([], [Cell])).

macro( xtable(TblCmds, CellCmds, RowList), 
		table(TblCmds, XRowList) )
	:-!,
	apply_cell_cmds(RowList, CellCmds, XRowList).

apply_cell_cmds([], _, []).
apply_cell_cmds([Row | RowList], CellCmds, [XRow | XRowList])
	:-
	row_apply_cell_cmds(Row, CellCmds, XRow),
	apply_cell_cmds(RowList, CellCmds, XRowList).

row_apply_cell_cmds(row(Row), CellCmds, tr([],XRow))
	:-!,
	row_apply_cell_cmds(Row, CellCmds, XRow).
row_apply_cell_cmds(tr(Row), CellCmds, tr([],XRow))
	:-!,
	row_apply_cell_cmds(Row, CellCmds, XRow).
row_apply_cell_cmds(tr(Options,Row), CellCmds, tr(Options,XRow))
	:-!,
	row_apply_cell_cmds(Row, CellCmds, XRow).
row_apply_cell_cmds([], CellCmds, []).
row_apply_cell_cmds([Cell | Row], CellCmds, [XCell | XRow])
	:-!,
	cell_apply_cell_cmds(Cell, CellCmds, XCell),
	row_apply_cell_cmds(Row, CellCmds, XRow).

cell_apply_cell_cmds(td(Options,Content), CellCmds, td(XOptions,XContent))
	:-!,
	cell_cmds_xtnd(CellCmds,Options,XOptions,Content,XContent).
cell_apply_cell_cmds(th(Options,Content), CellCmds, th(XOptions,XContent))
	:-!,
	cell_cmds_xtnd(CellCmds,Options,XOptions,Content,XContent).

cell_apply_cell_cmds(td(Content), CellCmds, XCell)
	:-!,
	cell_apply_cell_cmds(td([],Content), CellCmds, XCell).
cell_apply_cell_cmds(th(Content), CellCmds, XCell)
	:-!,
	cell_apply_cell_cmds(th([],Content), CellCmds, XCell).
cell_apply_cell_cmds(Content, CellCmds, XCell)
	:-
	functor(Content, FF, _), FF\=td, FF\=th,
	cell_apply_cell_cmds(td([],Content), CellCmds, XCell).

cell_cmds_xtnd([],XOptions,XOptions,XContent,XContent).
cell_cmds_xtnd([T=V | CellCmds],Options,XOptions,Content,XContent)
	:-!,
	cell_cmds_xtnd(CellCmds,[T=V | Options],XOptions,Content,XContent).
cell_cmds_xtnd([MM | CellCmds],Options,XOptions,Content,XContent)
	:-
	functor(MM, FF, 1),
	arg(1, MM, AA),
	functor(MC, FF, 2),
	arg(1, MC, AA),
	arg(2, MC, Content),
	cell_cmds_xtnd(CellCmds,Options,XOptions,MC,XContent).


	%%-----------------------
	%% ---- Form Matters ----
	%%-----------------------

	%% input(submit, [name=clear_result, value='Clear'])])

macro( input(Atom, Options), input([type=Atom | Options], []) )
	:-!,
	atomic(Atom).

macro( select(Atom, Options), select([name=Atom], SelOptions) )
	:-!,
	atomic(Atom),
	(xpand_sel_opts(Options, SelOptions),!;
	 pxml:macro_expand(Options,OOps), xpand_sel_opts(OOps, SelOptions)
	).

macro( select(Atom, Opts, Options), select([name=Atom | Opts], SelOptions) )
	:-!,
	atomic(Atom),
	(xpand_sel_opts(Options, SelOptions),!;
	 pxml:macro_expand(Options,OOps), xpand_sel_opts(OOps, SelOptions)
	).

xpand_sel_opts([], []).
xpand_sel_opts([Opt | Options], [XOpt | SelOptions])
	:-
	x_sel_opt(Opt, XOpt),
	xpand_sel_opts(Options, SelOptions).

x_sel_opt(s(Opt), option([value=Opt, selected],Opt)) 
	:-
	atomic(Opt),
	!.

x_sel_opt(o(Opt), option([value=Opt],Opt)) 
	:-
	atomic(Opt),
	!.

x_sel_opt(Opt, option([value=Opt],Opt)) 
	:-
	atomic(Opt),
	!.
	
x_sel_opt(o(Opts, Opt), option(Opts,Opt)) :-!.
x_sel_opt(option(Opts, Opt), option(Opts,Opt)) :-!.
x_sel_opt(Opt, option([],Opt))
	:-
	atomic(Opt),
	!.
x_sel_opt(Opt, Opt).

macro(  textarea(Name,Options,Text),
		textarea([name=Name | Options], Text) ) :-!.

macro( form_reply(Options, Head, Content),
	['Content-type: text/html',
	 newline, newline,
	  html([], [
	     newline, 
	     head([], Head),
	 	 newline,
	     body(Options,Content) ])
	] ) :-!.

macro( form_reply(Options, Content),
	['Content-type: text/html',
	 newline, newline,
	  html([], [
	     newline, 
	     head([], []),
	 	 newline,
	     body(Options,Content) ])
	] ) :-!.


	%% In application macro file, add facts like:
	%% style_tag(style(level1), [background=white]).
	%% 
macro( body(Tag, Content),
		body(Style, Content) )
	:-
	style_tag(Tag, Style).

	%%-----------------------
	%% ---- Misc Matters ----
	%%-----------------------

macro( javascript(Stmts),
		script([language='JavaScript'],
				[comment(Stmts)]) ) :-!.

macro( '!doctype',
		'<!doctype html public "-//w3c//dtd html 4.0 transitional//en">').

macro( meta_std_1,
		'<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">').

macro( meta_std_2(Arg), MSTD2)
	:-
	sprintf(atom(MSTD2),
			'<meta name="GENERATOR" content="PXML Gen [%t]">', [Arg]).

macro(page(Head, Body), 
	  ['!doctype', html([], [head([], [meta_std_1, meta_std_2('ALS') | Head]), Body ]) ]).

macro( metatag(Name,Value),
		meta([name=Name,value=Value],[]) ).

macro( cookie(NameValuesList, Extras), CookieText)
	:-
	name_value_text(NameValuesList, NameValuesTextList),
	name_value_text(Extras, ExtrasTextList),
	append(ExtrasTextList, [newline,newline],ETL0),
	append(NameValuesTextList, ETL0, TextList),
	CookieText = ['Set-Cookie: ' | TextList]. 

name_value_text([], []).
name_value_text([Tag=Val | Eqs], [Text | TextList])
	:-
	sprintf(atom(Text), '%t=%t; ', [Tag,Val]),
	name_value_text(Eqs, TextList).

	%%-----------------------
	%% --- Generic Matters --
	%%-----------------------

export standard_today/1.
standard_today(Date)
	:-
	date(YY/MM/DD),
	month_name(MM,NMM),
	sprintf(atom(Date), '%t %t %t', [DD, NMM, YY]).

month_name(1,'January').
month_name(2,'February').
month_name(3,'March').
month_name(4,'April').
month_name(5,'May').
month_name(6,'June').
month_name(7,'July').
month_name(8,'August').
month_name(9,'September').
month_name(10,'October').
month_name(11,'November').
month_name(12,'December').

endmod.
