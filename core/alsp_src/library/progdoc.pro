/*==================================================================
 |		progdoc.pro
 |	Copyright (c) 1991-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |   Doc Tools for maintaining a library, including the ALS Library
 |		-- originally part of libmaint.pro, separated in 1993
 |
 |	Date: October, 1991
 |	Author: Ken Bowen
 |	Revised: May 1993; Dec 1995
 *==================================================================*/

module sys_maint.

		/*-----------------------------------------
		 |    LIBRARY DOCUMENTATION PROCESSING
		 *----------------------------------------*/
export prog_doc/3.

prog_doc(SourceFile, OutBrfFile, OutTgtFile)
	:-
	lib_man_info(SourceFile, OutBrfFile, OutTgtFile, _, _, _, _, _).

/*!-----------------------------------------------------------------------------
 *!----------------------------------------------------------------------------*/
lib_man_info(SourceFile, OutBrfFile, OutTgtFile, LibraryKey, ExportList, Module,
			 DocumentedPredsList, GroupsInfo)
	:-
	open(SourceFile, read, InStream, []),
	assemble_documentation(InStream, SortedDocList, NamesList, FormsList,
						DescriptionsList,DocumentedPredsList, GroupsInfo),
	close(InStream),

	open(OutBrfFile, write, OutBrfStream, []),
	write_names_list(NamesList,OutBrfStream),
	close(OutBrfStream),

	open(OutTgtFile, write, OutTgtStream, []),
	write_full_doc(SortedDocList, SourceFile, OutTgtFile, OutTgtStream),
	close(OutTgtStream).

eof_char(-1).
eoln_char(13).
eoln_char(10).

/*------------------------------------------
 *-----------------------------------------*/
assemble_documentation(Stream, SortedDocList, NamesList, FormsList, 
                       DescriptionsList, DocumentedPredsList, GroupsInfo)
	:-
	get_all_doc(Stream, FirstDocList, GroupsInfo),
	sort(FirstDocList,SortedDocList),
	combine_docs(SortedDocList,
	             NamesList, FormsList, DescriptionsList,
				 DocumentedPredsList).
	
/*------------------------------------------
 *-----------------------------------------*/
get_all_doc(Stream, [Doc | RestDoc], GroupsInfo)
	:-
	get_groups(Stream, GroupsInfo),
	get_doc(Stream, Doc),
	(Doc = empty_doc ->
		RestDoc = [];
		get_all_doc0(Stream, RestDoc)
	).

get_all_doc0(Stream, Docs)
	:-
	get_doc(Stream, Doc),
	!,
	disp_get_all_doc0(Doc,Docs, Stream).

disp_get_all_doc0(empty_doc,[], Stream).
disp_get_all_doc0(Doc,[Doc | RestDoc], Stream)
	:-
	get_all_doc0(Stream, RestDoc).


get_groups(Stream, []).

/*------------------------------------------
 *-----------------------------------------*/
get_doc(Stream,Doc)
	:-
	search_lib_comment(Stream,FirstLine),
	(dmember(FirstLine, [end_of_file,end_of_comment]) ->
		Doc = empty_doc;
		get_pred_doc(Stream, FirstLine, Doc)
	).

/*------------------------------------------
 *-----------------------------------------*/
search_lib_comment(Stream, FirstLine)
	:-
	search_lib_comment_init(Stream),
	!,
	skip_to_next(Stream,NextLineUIA),
	name(NextLineUIA, NextLineChars),
	strip_leader(NextLineChars, PotentialFirstLine, [0' ,9,0'|,0'*]),
	(PotentialFirstLine = [0'! | _] ->
		FirstLine = end_of_comment
		;
		FirstLine = PotentialFirstLine
	).
search_lib_comment(Stream, end_of_comment).

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
search_lib_comment_init(Stream)
	:-
	get_line(Stream,Line),
	disp_search_lib_comment_init(Line,Stream).

disp_search_lib_comment_init(end_of_file,Stream) :-!.
disp_search_lib_comment_init(Buffer,Stream) 
	:-
	'$uia_peekb'(Buffer, 0, 0'/),
	'$uia_peekb'(Buffer, 1, 0'*),
	'$uia_peekb'(Buffer, 2, 0'!),
	!.
disp_search_lib_comment_init(_,Stream)
	:-
	search_lib_comment_init(Stream).


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
	          CallingForm_IOPatterns, [TextPredDesc | ExtendedDescrip]).

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
	          CallingForm_IOPatterns, ExtendedDescrip),
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
:- dynamic(user_write_full_doc/4).

write_full_doc(DocList, SourceFile, OutTgtFile, OutTgtStream)
	:-
	user_write_full_doc(DocList, SourceFile, OutTgtFile, OutTgtStream),
	!.

write_full_doc(DocList, SourceFile, OutTgtFile, OutTgtStream)
	:-
	fm_write_full_doc(DocList, SourceFile, OutTgtFile, OutTgtStream).
	

/*------------------------------------------
 *-----------------------------------------*/
dflt_write_full_doc([], OutStream).
dflt_write_full_doc([Doc | SortedDocList], OutStream)
	:-
	printf(OutStream, '%t.\n', [Doc],[quoted(true)]),
	dflt_write_full_doc(SortedDocList, OutStream).


/*------------------------------------------
 *-----------------------------------------*/
write_documentation(Stm, NamesList, FormsList, 
                    DescriptionsList, DocumentedPredsList)
	:-
	printf(Stm,'\n''@-----Name/Brief Description-----''.\n\n',[quoted(true)]),
	write_names_list(NamesList,Stm).

%	printf(Stm,'\n''@-----Forms & I/O Patterns-----''.\n\n',[quoted(true)]),
%	write_string_list(FormsList,Stm),

%	printf(Stm,'\n''@-----Description-----''.\n\n',[quoted(true)]),
%	write_descrips_list(DescriptionsList,Stm).

endmod.


/*==================================================================
 |		fmt_fm.pro
 |	Copyright (c) 1995 Applied Logic Systems, Inc.
 |
 |	FrameMaker ALS Man page formatting for progdoc.pro
 |
 |	Date: December, 1995
 |	Author: Ken Bowen
 *==================================================================*/

ta :-
	fm_doc('/mailbox3/als_libs/accsys/addlp/accsysul.pro').

module sys_maint.

/*!-----------------------------------------------------------------------------
 *!----------------------------------------------------------------------------*/

export fm_doc/1.
fm_doc(InitSrcFile)
	:-
	pathPlusFile(Path,File,InitSrcFile),
	(filePlusExt(BaseFile,Ext,File) ->
		true
		;
		BaseFile = File, Ext = pro
	),
	filePlusExt(BaseFile,Ext,FF),
	pathPlusFile(Path,FF,SrcFile),
	filePlusExt(BaseFile,brf,BriefTgt),
	filePlusExt(BaseFile,mml,MMLTgt),

	lib_man_info(SrcFile, BriefTgt, MMLTgt, _, _, _, _, _).

/*!-----------------------------------------------------------------------------
 *!----------------------------------------------------------------------------*/
fm_write_full_doc(SortedDocList, SourceFile, OutTgtFile, OutStream)
	:-
	printf(OutStream, '<MML>\n',[]),
	printf(OutStream, '<Comment -- MML file %t>\n',[OutTgtFile]),
	printf(OutStream, '<Comment -- Generated from %t>\n\n',[SourceFile]),
	printf(OutStream, '<Comment -- Macro tags:>\n',[]),

	printf(OutStream, '<!DefineTag BltGuide1>\n',[]),
	printf(OutStream, '<!DefineTag BltGuide>\n',[]),
	printf(OutStream, '<!DefineTag BltCode>\n',[]),
	printf(OutStream, '<!DefineTag BltDesc>\n',[]),
	printf(OutStream, '<!DefineTag BltForms>\n',[]),
	printf(OutStream, '<!DefineTag BltBody>\n',[]),
	printf(OutStream, '<!DefineTag BltCmtBody>\n',[]),
	printf(OutStream, '<!DefineTag BltFirstCmtBody>\n',[]),
	printf(OutStream, '<!DefineTag BltArgs>\n',[]),
	printf(OutStream, '<!DefineTag BltIO_Head>\n',[]),
	printf(OutStream, '<!DefineTag BltName>\n',[]),
	printf(OutStream, '<!DefineTag BltStart>\n',[]),
	printf(OutStream, '<!DefineTag BltNameBody>\n',[]),
	printf(OutStream, '<!DefineTag Body>\n',[]),
	printf(OutStream, '<!DefineTag 2Heading>\n',[]),

	printf(OutStream, '<Comment -- Text Content:>\n\n',[]),

	pathPlusFile(Path,BaseFile,SourceFile),
	printf(OutStream, '<2Heading>\nSourceFile: %t\n<Body>\n', [BaseFile]),

	fm_write_full_doc(SortedDocList, OutStream).

/*------------------------------------------
 *-----------------------------------------*/

fm_write_full_doc([], OutStream).
fm_write_full_doc([Doc | DocList], OutStream)
	:-
	man_fmt_doc(Doc, OutStream),
	fm_write_full_doc(DocList, OutStream).

/*------------------------------------------
 *-----------------------------------------*/
man_fmt_doc(Doc, SS)
	:-
	Doc = doc(PredDesc, (TextPredDesc, BriefDescrip), 
	          CallingForm_IOPatterns, [TextPredDesc | ExtendedDescrip]),

	nl(SS),nl(SS),
	printf(SS, '<Comment ***** ----- %t ----->\n', [PredDesc]),
	printf(SS, '<BltName>:\n', []),
	printf(SS, '<BltNameBody>\n', []),
	printf(SS, '%t\t%t\n', [PredDesc,BriefDescrip]),
	printf(SS, '<BltForms>:\n', []),
	printf(SS, '<BltCode>\n', []),
	printf(SS, '%t\n<par>\n', [TextPredDesc]),
	out_all(CallingForm_IOPatterns,SS),
	nl(SS),
	printf(SS, '<BltDesc>:\n', []),
	printf(SS, '<BltFirstCmtBody>\n', []),
	xdesc(ExtendedDescrip,SS),
	nl(SS).

/*------------------------------------------
 *-----------------------------------------*/
out_all([],SS).
out_all([Line | Lines], SS)
	:-
	slashify_line(Line, XLine),
	printf(SS, '%t\n<par>\n', [XLine]),
	out_all(Lines, SS).

/*------------------------------------------
 *-----------------------------------------*/
xdesc([],SS).

xdesc([Line | ExtendedDescrip],SS)
	:-
	io_line(Line),
	!,
	printf(SS, '<par><BltIO_Head>\n%t\n<BltArgs>\n', [Line]),
	xdesc_args(ExtendedDescrip,SS).

xdesc([Line | ExtendedDescrip],SS)
	:-
	slashify_line(Line, XLine),
	printf(SS, '%t\n<par>\n<BltCmtBody>\n', [XLine]),
	xdesc(ExtendedDescrip,SS).

/*------------------------------------------
 *-----------------------------------------*/
xdesc_args([],SS).
xdesc_args([Line | ExtendedDescrip],SS)
	:-
	is_null_line(Line),
	printf(SS, '<par>\n<BltFirstCmtBody>\n', []),
	!,
	xdesc(ExtendedDescrip,SS).
xdesc_args([Line | ExtendedDescrip],SS)
	:-
	io_line(Line),
	printf(SS, '<par><BltIO_Head>\n%t\n<BltArgs>\n', [Line]),
	xdesc_args(ExtendedDescrip,SS).
xdesc_args([Line | ExtendedDescrip],SS)
	:-
	slashify_line(Line, XLine),
	printf(SS, '%t\n<par>\n', [XLine]),
	xdesc_args(ExtendedDescrip,SS).

/*------------------------------------------
 *-----------------------------------------*/
io_line('Inputs:') :-!.
io_line('Outputs:') :-!.
io_line(Line)
	:-
	atom_length(Line,LL),
	Stop is LL - 5,
	io_line(Line,1,5,Stop,'Input').
io_line(Line)
	:-
	atom_length(Line,LL),
	Stop is LL - 6,
	io_line(Line,1,6,Stop,'Output').

io_line(Line,Start,Size,Stop,SubAtom)
	:-
	sub_atom(Line,Start,Size,SubAtom),!.

io_line(Line,Start,Size,Stop,SubAtom)
	:-
	Start < Stop,
	NextStart is Start + 1,
	io_line(Line,NextStart,Size,Stop,SubAtom).

/*------------------------------------------
 *-----------------------------------------*/
slashify_line(Line, XLine)
	:-
	atom_codes(Line, LCs),
	slash_stuff(LCs, XLCs),
	atom_codes(XLine, XLCs).

/*------------------------------------------
 *-----------------------------------------*/
slash_stuff([], []).
slash_stuff([C | LCs], [0'\\, C | XLCs])
	:-
	needs_slash(C),
	!,
	slash_stuff(LCs, XLCs).
slash_stuff([C | LCs], [C | XLCs])
	:-
	slash_stuff(LCs, XLCs).

/*------------------------------------------
 *-----------------------------------------*/
needs_slash(0'<).
needs_slash(0'>).
needs_slash(0'\\).

/*------------------------------------------
 *-----------------------------------------*/
is_null_line('').
is_null_line(' ').
is_null_line('  ').
is_null_line('\t').
is_null_line('\t\t').
is_null_line('|').
is_null_line('\t|').
is_null_line(' |').
is_null_line('\t|').
is_null_line('| ').
is_null_line('|\t').
is_null_line(' | ').
is_null_line('\t| ').
is_null_line('|  ').
is_null_line('|\t ').
is_null_line(' |\t').
is_null_line('\t|\t').
is_null_line('| \t').
is_null_line('|\t\t').

endmod.

