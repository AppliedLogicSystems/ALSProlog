/*==================================================================
 |		prologdoc.pro
 |	Copyright (c) 1991-2003 Applied Logic Systems, Inc.
 |
 |   Doc Tools for maintaining  code documentation.
 |	-- originally part of libmaint.pro, separated in 1993
 |	-- HTML-oriented version replaced original 2003
 |
 |	Date: October, 1991
 |	Author: Ken Bowen
 |	Revised: May 1993; Dec 1995
 *==================================================================*/

:-[pml].

%module prologdoc.

t1 :- prolog_doc_file('testfiles/arithx1.pro', output, outlists).
t2 :- prologdoc('testfiles', output, outlists).


macro_file( 'prologdoc_pxml_macros.pro').

		/*-----------------------------------------
		 | PROLOG DOCUMENTATION PROCESSING - DIRs
		 *----------------------------------------*/
export prologdoc/3.
prologdoc(SourcePath, OutDir, ListsDir)
	:-
	files(SourcePath, '*.pro', Files),
	pdoc_files(Files, SourcePath, OutDir, ListsDir).

pdoc_files([], SourcePath, OutDir, ListsDir).
pdoc_files([File | Files], SourcePath, OutDir, ListsDir)
	:-
	path_directory_tail(FilePath, SourcePath, File),
	prolog_doc_file(FilePath, OutDir, ListsDir),
	pdoc_files(Files, SourcePath, OutDir, ListsDir).


		/*-----------------------------------------
		 | PROLOG DOCUMENTATION PROCESSING - FILES
		 *----------------------------------------*/
export prolog_doc_file/3.
prolog_doc_file(SourcePath, OutDir, ListsDir)
	:-
	open(SourcePath, read, InStream, []),
	unwind_protect(
		assemble_documentation(InStream, SortedDocList, NamesList, FormsList,
			DescriptionsList,DocumentedPredsList, FileDesc, Module),
		close(InStream) ),

	path_directory_tail(SourcePath, _, SourceFile),
	file_extension(SourceFile, SourceRoot, _),
	file_extension(ListFile, SourceRoot, lis),
	path_directory_tail(ListPath, ListsDir, ListFile),

	open(ListPath, write, OutListStream, []),
	unwind_protect(
		write_names_list(NamesList,OutListStream),
		close(OutListStream) ),

	file_extension(PXMLFile, SourceRoot, pxml),
	file_extension(HTMLFile, SourceRoot, html),
	path_directory_tail(PXMLPath, OutDir, PXMLFile),
	path_directory_tail(HTMLPath, OutDir, HTMLFile),
	setup_hyper_desc(FileDesc, SourceFile, SortedDocList, Module,
			 HTMLPath, HyperDocList),

	macro_file( MacroFile ),
	open(PXMLPath, write, PXMLStream, []),
	unwind_protect(
	  ( write_clause(PXMLStream, include=MacroFile, [quoted(true)]),
	    write_clause(PXMLStream, HyperDocList, [quoted(true)]) ),
	  close(PXMLStream) ),
	
	!,
	pml(PXMLPath, ['prologdoc_pxml_macros.pro'] ).
	
	






/*!-----------------------------------------------------------------------------
 *!----------------------------------------------------------------------------*/

eof_char(-1).
eoln_char(13).
eoln_char(10).

/*------------------------------------------
 *-----------------------------------------*/
assemble_documentation(Stream, SortedDocList, NamesList, FormsList, 
                       DescriptionsList, DocumentedPredsList, FileDesc, Module)
	:-
	get_all_doc(Stream, FirstDocList, FileDesc, Module),
	sort(FirstDocList,SortedDocList),
	combine_docs(SortedDocList,
	             NamesList, FormsList, DescriptionsList,
				 DocumentedPredsList).
	
/*------------------------------------------
 *-----------------------------------------*/
get_all_doc(Stream, [Doc | RestDoc], FileDesc, Module)
	:-
	get_file_desc(Stream, FileDesc),
	get_doc(Stream, Doc, Module),
	(Doc = empty_doc ->
		RestDoc = [];
		get_all_doc0(Stream, RestDoc, Module2)
	),
	(var(Module) -> Module = Module1 ; true),
	(var(Module) -> Module = user ; true).

get_all_doc0(Stream, Docs, Module)
	:-
	get_doc(Stream, Doc, Module),
	!,
	disp_get_all_doc0(Doc,Docs, Stream, Module1),
	(var(Module) -> Module = Module1 ; true),
	(var(Module) -> Module = user ; true).

disp_get_all_doc0(empty_doc,[], Stream, Module).
disp_get_all_doc0(Doc,[Doc | RestDoc], Stream, Module)
	:-
	get_all_doc0(Stream, RestDoc, Module).


/*------------------------------------------
 *-----------------------------------------*/
get_doc(Stream,Doc,Module)
	:-
	search_lib_comment(Stream,FirstLine,Module),
	(dmember(FirstLine, [end_of_file,end_of_comment]) ->
		Doc = empty_doc;
		get_pred_doc(Stream, FirstLine, Doc)
	).

/*------------------------------------------
 *-----------------------------------------*/
search_lib_comment(Stream, FirstLine,Module)
	:-
	search_lib_comment_init(Stream,Module),
	!,
	skip_to_next(Stream,NextLineUIA),
	name(NextLineUIA, NextLineChars),
	strip_leader(NextLineChars, PotentialFirstLine, [0' ,9,0'|,0'*]),
	(PotentialFirstLine = [0'! | _] ->
		FirstLine = end_of_comment
		;
		FirstLine = PotentialFirstLine
	).
search_lib_comment(Stream, end_of_comment,Module).

skip_to_next(Stream,NextLine)
	:-
	get_line(Stream,VeryNextLine),
	disp_skip_to_next(VeryNextLine,NextLine,Stream).

disp_skip_to_next(end_of_file,end_of_file,Stream) :-!.
disp_skip_to_next('',NextLine,Stream)
	:-!,
	skip_to_next(Stream,NextLine).
disp_skip_to_next(NextLine,NextLine,Stream).

/*------------------------------------------
 *-----------------------------------------*/
get_file_desc(Stream, FileDesc)
	:-
	get_line(Stream,Line),
	sub_atom(Line, _, 2, _, '/*'),
	!,
	get_file_desc0(Stream, FileDesc).

get_file_desc0(Stream, FileDesc)
	:-
	get_line(Stream,Line),
	get_file_desc00(Line, Stream, FileDesc).

get_file_desc00(Line, Stream, FileDesc)
	:-
	sub_atom(Line, Start, 1, _, '@'),
	!,
	X is Start+1,
	sub_atom(Line,X,_,0,FileDesc).

get_file_desc00(Line, Stream, '')
	:-
	sub_atom(Line, _, 2, _, '*/'),
	!.

get_file_desc00(Line, Stream, FileDesc)
	:-
	get_file_desc0(Stream, FileDesc).

/*------------------------------------------
 *-----------------------------------------*/
search_lib_comment_init(Stream,Module)
	:-
	get_line(Stream,Line),
	disp_search_lib_comment_init(Line,Stream,Module).

disp_search_lib_comment_init(end_of_file,Stream,Module) :-!.
disp_search_lib_comment_init(Buffer,Stream,Module) 
	:-
	sub_atom(Buffer, 0, 3, _, '/*!'),
	!.
disp_search_lib_comment_init(Buffer,Stream,Module) 
	:-
	var(Module),
	sub_atom(Buffer, 0, 6, _, 'module'),
	atomread(Buffer, Term),
	Term = module(Module),
	!,
	search_lib_comment_init(Stream,_).
disp_search_lib_comment_init(_,Stream,Module)
	:-
	search_lib_comment_init(Stream,Module).


/*------------------------------------------
 *-----------------------------------------*/
strip_leader([],[],_).
strip_leader([C | RestL],Tail,Skips)
	:-
	dmember(C,Skips),!,
	strip_leader(RestL,Tail,Skips).
strip_leader(Tail,Tail,_).

/*------------------------------------------
 *-----------------------------------------*/
get_pred_doc(Stream, FirstLine, Doc)
	:-
	get_pred_desc(FirstLine,PredDesc),

	get_header_doc(Stream, HeaderDoc),
	(dmember(end_of_comment, HeaderDoc) ->
		BriefDescrip = end_of_comment
		;
		get_brief_descrip(Stream, BriefDescrip)
	),
	(BriefDescrip = end_of_comment ->
		ExtendedDescrip = end_of_comment
		;
		get_extended_descrip(Stream, ExtendedDescrip)
	),
	(HeaderDoc = [TextPredDesc |  CallingForm_IOPatterns] ->
		true
		;
		TextPredDesc = '??',
		CallingForm_IOPatterns = []
	),
	!,
	Doc = doc(PredDesc, (TextPredDesc, BriefDescrip), 
	          CallingForm_IOPatterns, 
%		  [TextPredDesc | ExtendedDescrip]
		  BriefDescrip,
		  ExtendedDescrip
		  ).

get_pred_desc(FirstLine,PredDesc)
	:-
	bufread(FirstLine, PredDesc, [syntax_errors(fail)]),
	!.

get_pred_desc(FirstLine,'??'/'?').

/*------------------------------------------
 *-----------------------------------------*/
get_header_doc(Stream, HeaderDoc)
	:-
	get_line(Stream, NextLineUIA),
	name(NextLineUIA,NextLineChars),
	strip_leader(NextLineChars, StrippedNextLine, [0' ,9,0'|,0'*]),
	disp_get_header_doc(StrippedNextLine, HeaderDoc, Stream).

disp_get_header_doc([], [], Stream) :-!.
disp_get_header_doc([0'! | _], [end_of_comment], Stream) :-!.
disp_get_header_doc(StrippedNextLineCs, 
					[StrippedNextLine | RestHeaderDoc], Stream)
	:-
	atom_codes(StrippedNextLine, StrippedNextLineCs),
	get_header_doc(Stream, RestHeaderDoc).

/*------------------------------------------
 *-----------------------------------------*/
get_brief_descrip(Stream, BriefDescrip)
	:-
	get_line(Stream, NextLineUIA),
	name(NextLineUIA,NextLineChars),
	strip_leader(NextLineChars, StrippedNextLine, [0' ,9,0'|,0'*, 0'-]),
	disp_get_brief_descrip(StrippedNextLine, BriefDescripCs, Stream),
	atom_codes(BriefDescrip, BriefDescripCs).

disp_get_brief_descrip('', BriefDescrip,Stream)
	:-!,
	get_brief_descrip(Stream, BriefDescrip).
disp_get_brief_descrip(BriefDescrip, BriefDescrip,Stream).

/*------------------------------------------
 *-----------------------------------------*/
get_extended_descrip(Stream, ExtendedDescrip)
	:-
	get_line(Stream, NextLineUIA),
	name(NextLineUIA,NextLineChars),
	disp_get_x2(NextLineChars, ExtendedDescrip, Stream).

disp_get_x2([0' ,0'*, 0'- | _], [], Stream)
	:-!.

disp_get_x2([0'	,0'*, 0'- | _], [], Stream)  %% tab
	:-!.

disp_get_x2(NextLineChars, ExtendedDescrip, Stream)
	:-
	strip_leader(NextLineChars, StrippedNextLine0, [0' ,9]),
	StrippedNextLine0 = [_ | StrippedNextLine ],
	disp_get_extended_descrip(StrippedNextLine,ExtendedDescrip,Stream).

disp_get_extended_descrip(StrippedNextLine,[],Stream)
	:-
	StrippedNextLine = [0'! | _], !.

disp_get_extended_descrip(StrippedNextLineCs,
                          [StrippedNextLine | RestExtendedDescrip],Stream)
	:-
	atom_codes(StrippedNextLine, StrippedNextLineCs),
	get_extended_descrip(Stream, RestExtendedDescrip).

/*------------------------------------------
 *-----------------------------------------*/
combine_docs([],[], [], [],[]).
combine_docs([Doc | RestSortedDocList],
			 [NameEntry | RestNamesList], 
			 FormsList, 
			 [ExtendedDescrip | RestDescriptionsList],
			 [PredDesc | RestDocumentedPredsList])
	:-
	Doc = doc(PredDesc, NameEntry,
	          CallingForm_IOPatterns, ShortNote, ExtendedDescrip),
	dappend(CallingForm_IOPatterns, RestFormsList, FormsList),
	combine_docs(RestSortedDocList,
	             RestNamesList, 
				 RestFormsList, 
				 RestDescriptionsList,
			     RestDocumentedPredsList).

combine_docs([_ | RestSortedDocList], NamesList, FormsList, 
				 DescriptionsList, DocumentedPredsList)
	:-
	combine_docs(RestSortedDocList, NamesList, FormsList, 
				 DescriptionsList, DocumentedPredsList).

/*------------------------------------------
 *-----------------------------------------*/
write_names_list([],_).
write_names_list([(PredDesc,BriefDescrip) | RestNamesList],Stm)
	:-
	PredDesc \= end_of_comment,
	!,
	printf(Stm,'%t - %t.\n',[PredDesc,BriefDescrip],[quoted(true)]),
	write_names_list(RestNamesList,Stm).
write_names_list([_ | RestNamesList],Stm)
	:-
	write_names_list(RestNamesList,Stm).

/*------------------------------------------
 *-----------------------------------------*/
write_html_doc(SortedDocList, SourceFile, OutTgtFile, OutTgtStream)
	:-
	true.

/*------------------------------------------
 *-----------------------------------------*/
setup_hyper_desc(FileDesc, SourceFile, SortedDocList, Module,
		 OutTgtFile, HyperDoc)
	:-
	catenate('File: ', SourceFile, PageTitle),
	HyperDoc = 
	 target(OutTgtFile,
	 prolog_doc_page(
	   [title(PageTitle), metatag(description, FileDesc)], 
	   [body([], PageBody)] ) ),
	pd_pagebody(FileDesc, SourceFile, SortedDocList, 
		    FileDesc, Module, OutTgtFile, PageBody).

pd_pagebody(FileDesc, SourceFile, SortedDocList, 
		 FileDesc, Module, OutTgtFile, PageBody)
	:-
%<body bgcolor="#94acb9">
	PageBody = [
	  h2([],SourceFile),
	  h3([],FileDesc),
	  h3([], ['Module: ', Module]),
	  page_index(PageIndicies) | PredsDoc],
	pd_preds_doc(SortedDocList, PageIndicies, PredsDoc).

pd_preds_doc([], [], []).
pd_preds_doc([PDoc | SortedDocList], [PredIdx | PageIndicies], [PredDoc | PredsDoc])
	:-
	pred_docx(PDoc, PredIdx, PredDoc),
	pd_preds_doc(SortedDocList, PageIndicies, PredsDoc).

pred_docx( doc(PredDesc, NameEntry, CallingForm_IOPatterns, ShortNote, ExtendedDescrip),
		PredIdx, PredDoc)
	:-
	flattendesc(PredDesc, PIX),
	PredIdx = pix(PIX, PredDesc),
	NameEntry = (CallPattern, ShortNote),
	PredDoc = 
	  pd(PredDesc, PIX, CallPattern, CallingForm_IOPatterns, 
	  	ShortNote, ExtendedDescrip).

flattendesc(PredDesc, PIX)
	:-
	sprintf(atom(PDA), '%t', [PredDesc]),
	atom_codes(PDA, PDCs),
	out_slash(PDCs, PIXCs),
	atom_codes(PIX, PIXCs).

out_slash([], []).
out_slash([0'/ | PDCs], PIXCs)
	:-!,
	out_slash(PDCs, PIXCs).
out_slash([C | PDCs], [C | PIXCs])
	:-
	out_slash(PDCs, PIXCs).





%endmod.


