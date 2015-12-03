	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% PrologDoc PXML Macros
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

module mpml.

include('std_pxml_macros.pro').

style_tag(style(level1), [bgcolor=white]).
style_tag(style(stars), 
           [background=path([als_root,als_icons,'stars.jpg'])]).

macro( path(List), Path)
	:-
	pxml:macro_expand(List, XList0),
	((XList0 = ['', Path0], atomic(Path0)) ->
		Path = Path0
		;
		(XList0 = ['' | XList1] ->
			join_path(XList1, Path)
			;
			join_path(XList0, Path)
		)
	).

macro(prolog_doc_page(Head, Body), 
	  ['!doctype', html([], [head([], [meta_std_1, meta_std_2('PrologDoc') | Head]), Body ]) ]).

macro(pix(IDX, Target), [ref(SIDX,TargetAtom),br])
	:-
	catenate('#', IDX, SIDX),
	sprintf(atom(TargetAtom), '%t', [Target]).

macro(page_index(List), Result)
	:-
	append([[p], List, [p]], Result).

macro( pd(PredDesc, IDX, CallPattern, IOPatterns, ShortNote, ExtendedDescrip),
	[p,a([name=IDX],[b(TargetPA)]),br,CallPatternAtom,br,IOPL,blockquote(BQ) ] )

     	:-
	sprintf(atom(TargetPA), '%t', [PredDesc]),
	sprintf(atom(CallPatternAtom), '%t', [CallPattern]),
	sprintf(atom(IOPatternAtom), '%t', [IOPatterns]),
	sprintf(atom(ShortNoteAtom), '%t', [ShortNote]),
	atomizelist(IOPatterns, IOPL),
	(ExtendedDescrip = [] 
		-> 
		BQ = [ShortNote]
		;
		atomizelist(ExtendedDescrip, XD),
		BQ = [ShortNote, br | XD]
	).

atomizelist([], []).
atomizelist([X | L], [XA | LL])
	:-
	sprintf(atom(XA), '%t ', [X]),
	atomizelist(L, LL).


endmod.

