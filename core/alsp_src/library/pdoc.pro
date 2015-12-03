/*==================================================================
 |		pdoc.pro
 |	Copyright (c) 1991-2004 Applied Logic Systems, Inc.
 |
 |   Doc Tools for maintaining  code documentation.
 |	-- originally part of libmaint.pro, separated in 1993
 |	-- HTML-oriented version replaced original 2003
 |	   - now VERY closely modelled on Sun's JavaDoc
 |      
 |   Requirements:
 |      - doc comments start with "/" followed by "*!"  or "**"
 |      - doc comments must occur within the module of the predicate
 |	  to which they apply; a construct such as
 |		<doccommennt> module(foo). foo :- ....
 |	  is not supported.
 |
 |	Date: October, 1991
 |	Author: Ken Bowen
 |	Revised: May 1993; Dec 1995; 2003-4
 *==================================================================*/

:-[pdoc_viewer, pdoc_html_desc, pdoc_indexes, pdoc_files, 'pdoc_stoplist.stopl'].  

module prologdoc.

export prologdoc/3.
export prolog_doc_file/6.

		/*-----------------------------------------
		 | PROLOG DOCUMENTATION PROCESSING 
		 *----------------------------------------*/

/*!----------------------------------------------------------------------*
 |  prologdoc/3
 |  prologdoc(General, Files, Groups)
 |  prologdoc(+, +, +)
 |
 |  - processes "prologdoc comments" to create html documentation
 |
 |  "Prologdoc comments" must begin with either '/' followed by '*!' or
 |  '**'; the rest of the line after this beginning is ignored.  All lines
 |  except the very first and the very last should begin with a space followed
 |  by '|' or '*', in turn follow by spaces and or tabs.  A line containing
 |  no non-whitespace except the opening '|' or '*' is regarded as empty.
 |  <p>
 |  The comment header starts on the very first line after the initial line.
 |  <br>
 |  The header should have the format:
 |  <pre>
 |	Pred/Arity  - e.g., foobar/2
 |	CallingDescription  - e.g., foobar(Ice, Cream)
 | 	I/O Patterns  - one or more lines like: foobar(+, -)
 |  </pre>
 |  The header is terminated by the first empty comment line.
 |  This header-terminating line should be followed by a single-line
 |  description opened with a dash, like this:
 |  <pre>
 |    - transforms ice to cream by using a frabjous filter
 |  </pre>
 |  The short description line should be followed by another empty
 |  comment line.  From this point to the end of the comment can be
 |  filled with a detailed description of the predicate.  HTML markup
 |  can be used.
 |  <p>
 |  The general processing goal <br>
 |  <pre>
 |         prologdoc(General, Files, Groups)
 |  </pre>
 |  specifies a collection of files and/or modules to be processed,
 |  together with additional related information.  The arguments are:<br>
 |  General = a list of tagged equations (tag=value) as follows:
 |  <pre>
 |    modulenames = list of module names to be processed;
 |    destdir     = path to a directory in which to write the output
 |    overwrite   = true/false; if true, will overwrite existing destdir;
 |    windowtitle = atom used as the window system title on the titlebar;
 |    doctitle=   = atom used as the overall document title;
 |    failonerror = true/false; controls behavior in face of errors;                   
 |  </pre>
 |  Files = a list of file specifications of the form
 |  <pre>
 |        files( dir=Path, FilesSpec)
 |  </pre>
 |  where Path is a path to a directory, and FilesSpec is either the atom
 |  'all', or else is a list of names of files occuring the directory Path.
 |  <br>
 |  Groups is a list of GroupSpecs used to provide conceptual organization
 |  on the overview page.  A GroupSpec is a term of the form
 |  <pre>
 |        group(Name,  Contents)
 |  </pre>
 |  where Name is an atom providing the name of the group (e.g., 'Tk Library Interface'),
 |  and Contents is a list containing two tagged equations
 |  <pre>
 |           modules=Modules, files=Files
 |  </pre>
 |  where Modules is a list of module names and Files is a list of file names;
 |  the contents of the group consist of all predicates occurring in one of
 |  the members of Files and belonging to one of the members of Modules.
 |  <p>
 |  The output of the PrologDoc processing is written into the specified
 |  destination directory (including subdirectories).  The HTML file
 |  <pre>
 |           index.html
 |  </pre>
 |  is the main entry to the API document.
 *-----------------------------------------------------------------------*/

prologdoc(General, Files, Groups)
	:-
	get_cwd(CurDir),
	datetime(Date, Time),
	sprintf(atom(DateTime), '%t %t', [Date, Time]),
		%% setup output directories:
	extend_gen(General, DateTime, XGeneral),
	setup_output(XGeneral),
	printf(user_output, '>>> Finished setting up output directory\n', []),
	change_cwd(CurDir),

		%% process all files into the Data struct:
	doc_process(Files, XGeneral, CurDir, Data),

		%% close off the xlists:
	close_off_everything(Data, ResultModLists, FilesList),

	printf(user_output, '>>> Finished gathering information\n', []),
		%% create the individual predicate html files:
	create_html_desc(ResultModLists, Data, XGeneral, OneLiners),
	printf(user_output, '>>> Finished creating individual HTML descriptions \n', []),
	!,
		%% create the reset of the viewer files:
	complete_prologdoc(Data, ResultModLists, XGeneral, Groups, CurDir, OneLiners).

    %% add keywords and date/time to the General properties:
extend_gen(General, DateTime, [datetime=DateTime | KGeneral])
	:-
	keywords(General, KGeneral).

    %% Setup the output directory structures and create 
    %% several simple near-constant files:
setup_output(General)
	:-
	get_cwd(Curdir),
	dmember(destdir=DestDir, General),
	dmember(overwrite=Overwrite, General),
	check_make_dir(DestDir, Overwrite),
	change_cwd(DestDir),
	dmember(modulenames=ModuleNames, General),
	setup_module_subdirs(ModuleNames),
	stylesheet_css,
        dmember(doctitle=DocTitle, General),
        index_html(DocTitle, General),
	dmember(datetime=DateTime, General),
	help_page(DocTitle, DateTime),
	overview_frame(ModuleNames, DocTitle, General), 
	change_cwd(Curdir).

doc_process(Files, General, TopDir, Data)
	:-
	dmember(modulenames=ModuleNames, General),
	dmember(destdir=DestDir, General),
	dmember(datetime=DateTime, General),
	start_index_datastruct(ModuleNames,DestDir,Data),
	doc_process(Files, ModuleNames, General, DateTime, TopDir, Data).


doc_process([], ModuleNames, General, DateTime, TopDir, Data).
doc_process([FilesDesc | Files], ModuleNames, General, DateTime, TopDir, Data)
	:-
	doc_process_fd(FilesDesc, ModuleNames, General, DateTime, TopDir, Data),
	doc_process(Files, ModuleNames, General, DateTime, TopDir, Data).

%    [ files( dir='.', all),
doc_process_fd(files(dir=SrcDir, WhichFiles), ModuleNames, General, DateTime, TopDir, Data)
	:-
	exists_file(SrcDir),
	!,
	check_add_mod_info(ModuleNames, SrcDir, Data),
	doc_process_files(WhichFiles, SrcDir, ModuleNames, General, DateTime, TopDir, Data).
doc_process_fd(files(dir=SrcDir, WhichFiles), ModuleNames, General, DateTime, TopDir, Data)
	:-
	sprintf(atom(Msg), '!! Directory %t does not exist! .. Aborting processing!\n', [SrcDir]),
	printf(user_output, Msg, []),
	throw(exception(Msg)).

doc_process_files([], SrcDir, ModuleNames, General, DateTime, TopDir, Data) :-!.
doc_process_files(all, SrcDir, ModuleNames, General, DateTime, TopDir, Data)
	:-!,
	files(SrcDir, '*.pro', FileList),
	pdoc_files(FileList, SrcDir, ModuleList, DateTime, TopDir, Data).
doc_process_files(FileList, SrcDir, ModuleNames, General, DateTime, TopDir, Data)
	:-
	pdoc_files(FileList, SrcDir, ModuleList, DateTime, TopDir, Data).


pdoc_files([], SourceDirPath, ModuleList, DateTime, TopDir, Data).
pdoc_files([File | Files], SourceDirPath, ModuleList, DateTime, TopDir, Data)
	:-
	do_pdoc_file(File, SourceDirPath, ModuleList, DateTime, TopDir, Data),
	pdoc_files(Files, SourceDirPath, ModuleList, DateTime, TopDir, Data).

do_pdoc_file(File, SourceDirPath, ModuleList, DateTime, TopDir, Data)
	:-
	%% Change to hack to ensure that we process only files ending in
	%% precisly ".pro"  -- of course what we really should do is correct
	%% the bug in files/3 so we are only getting such files to process.
	%% Hack: skip anything ending in ~:
	atom_length(File, K), 
	M is K-1, 
	sub_atom(File,M,_,0,'~'),
	!.
do_pdoc_file(File, SourceDirPath, ModuleList, DateTime, TopDir, Data)
	:-
	printf('%t\n', [File]),
	path_directory_tail(FilePath, SourceDirPath, File),
	prolog_doc_file(FilePath, ModuleList, DateTime, TopDir, File, Data).

keywords(General, General)
	:-
	dmember(keywords=KeyWords, General),
	!.
keywords(General, [keywords=KeyWords | General])
	:-
        dmember(doctitle=DocTitle, General),
	words_list(DocTitle, KeyWords).

words_list(DocTitle, KeyWords)
	:-
	atom_codes(DocTitle, DTCs),
	words_list_cs(DTCs, WLCs),
	flatten_with_commas(WLCs, KeyWordsCs),
	atom_codes(KeyWords, KeyWordsCs).

words_list_cs([], []).
words_list_cs(DTCs, [WordCs | WLCs])
	:-
	asplit0(DTCs, 0',, WordCs, RestCs),

	!,
	words_list_cs(RestCs, WLCs).
words_list_cs(DTCs, [WordCs | WLCs])
	:-
	asplit0(DTCs, 0' , WordCs, RestCs),
	!,
	words_list_cs(RestCs, WLCs).
words_list_cs(DTCs, [DTCs]).

flatten_with_commas([], []).
flatten_with_commas([WCs], WCs).
flatten_with_commas([WCs | WLCs], KeyWordsCs)
	:-
	flatten_with_commas(WLCs, RestKeyWordsCs),
	append(WCs, [0',,0'  | RestKeyWordsCs], KeyWordsCs).





check_make_dir(DestDir, Overwrite)
	:-
	exists_file(DestDir),
	Overwrite \= true,
	!,
	sprintf(atom(Msg), 'Target dir %t exists!', [DestDir]),
	throw(exception(Msg)).

check_make_dir(DestDir, true)
	:-
	exists_file(DestDir),
	!.

check_make_dir(DestDir, Overwrite)
	:-
	atom_codes(DestDir, DDCs),
	asplit0_all(DDCs, 0'/, SDLCs),
	all_to_atoms(SDLCs, PathList),
	get_cwd(CurDir),
	check_make_path(PathList),
	change_cwd(CurDir).

check_make_path([]).
check_make_path([SubDir | PathList])
	:-
	(exists_file(SubDir) ->
		true
		;
		make_subdir(SubDir),
		change_cwd(SubDir)
	),
	check_make_path(PathList).
	
setup_module_subdirs([]).
setup_module_subdirs([Module | ModuleNames])
	:-
	(exists_file(Module) ->
		true
		;
		make_subdir(Module)
	),
	setup_module_subdirs(ModuleNames).

check_add_mod_info(ModuleNames, SrcDir, Data)
	:-
	path_elements(SrcDir, SrcDirElts),
	check_add_mod_info0(ModuleNames, SrcDirElts, Data).

check_add_mod_info0([], _, _).
check_add_mod_info0([Mod | ModuleNames], SrcDirElts, Data)
	:-
	sprintf(atom(FileName), 'module-%t.html', [Mod]),
	append(SrcDirElts, [FileName], FilePathElts),
	path_elements(FilePath, FilePathElts),
	(exists_file(FilePath) ->
		add_mod_info(Mod, (FileName, FilePath), Data)
		;
		true
	),
	check_add_mod_info0(ModuleNames, SrcDirElts, Data).


		/*----------------------------------------------------
		 | PROLOG DOCUMENTATION PROCESSING - INDIVIDUAL FILES
		 *---------------------------------------------------*/

prolog_doc_file(SourcePath, ModuleList, DateTime, TopDir, File, Data)
	:-
	open(SourcePath, read, InStream, []),
	unwind_protect(
		get_all_doc_from_file(InStream, FirstDocList, FileDesc, File, user),
		close(InStream) ),
        group_by_mod(FirstDocList, GroupedDocList),
	findall(M, member(M-X, GroupedDocList), CurModList),
	add_file_to_mods(CurModList, File, Data),
	strip_mod_prefix(FirstDocList, FileDocList),
	add_file_info(File, FileDesc, FileDocList, Data),
	!,
	insert_mod_preds_list(GroupedDocList, Data).

prolog_doc_file(SourcePath, ModuleList, DateTime, TopDir, File, Data)
	:-
	printf(user_output, '!!WARNING: Failure gathering information from file: %t\n', [SourcePath]),
	printf(user_output, '    ---- Some information may have been skipped.\n',[]).

group_by_mod(FirstDocList, GroupedDocList)
	:-
	group_by_mod(FirstDocList, [], GroupedDocList).

group_by_mod([], Accum, Accum).
group_by_mod([Mod-PredDesc | DocList], Accum, GroupedDocList)
	:-
	dmember(Mod-Prev, Accum),
	Prev = (XList, t(XListTail)),
	XListTail = [PredDesc | NewXListTail],
	mangle(2, Prev, t(NewXListTail)),
	!,
	group_by_mod(DocList, Accum, GroupedDocList).
group_by_mod([Mod-PDesc | DocList], Accum, GroupedDocList)
	:-
	group_by_mod(DocList, [Mod-([PDesc | Tail], t(Tail)) | Accum], GroupedDocList).
	
strip_mod_prefix([], []).
strip_mod_prefix([_-PredInfo | FirstDocList], [PredInfo | FileDocList])
	:-
	strip_mod_prefix(FirstDocList, FileDocList).

/*!-----------------------------------------------------------------------------
 *!----------------------------------------------------------------------------*/

eof_char(-1).
eoln_char(13).
eoln_char(10).


/*------------------------------------------
 *-----------------------------------------*/
get_all_doc_from_file(Stream, DocsList, FileDesc, File, ModuleIn)
	:-
	get_file_desc(Stream, FileDesc),
	catch(
		get_doc(Stream, Doc, File, ModuleIn, ModuleMid),
		Ball,
		Doc = empty_doc),
	get_all_doc0(Stream, RestDoc, File, ModuleMid),
	(Doc = empty_doc ->
		DocsList = RestDoc
		;
		DocsList = [ModuleMid-Doc | RestDoc]
	).

get_all_doc0(Stream, Docs, File, ModuleIn)
	:-
	get_doc(Stream, Doc, File, ModuleIn, ModuleMid),
	!,
	disp_get_all_doc0(Doc, Docs, Stream, File, ModuleMid).

disp_get_all_doc0(empty_doc, [], Stream, File, Module).
disp_get_all_doc0(Doc, [Module-Doc | RestDoc], Stream, File, Module)
	:-
	get_all_doc0(Stream, RestDoc, File, Module).


/*------------------------------------------
 *-----------------------------------------*/
get_doc(Stream, Doc, File, ModuleIn, ModuleOut)
	:-
	search_lib_comment(Stream, FirstLine, ModuleIn, ModuleOut),
	(var(ModuleOut) -> 
		ModuleOut = ModuleIn 
		; 
		true
	),
	(dmember(FirstLine, [end_of_file,end_of_comment]) ->
		Doc = empty_doc
		;
		get_pred_doc(Stream, FirstLine, File, ModuleOut, Doc)
	).

/*------------------------------------------
 *-----------------------------------------*/
search_lib_comment(Stream, FirstLine, ModuleIn, Module)
	:-
	search_lib_comment_init(Stream, ModuleIn, Module),
	!,
	skip_to_next(Stream, NextLineUIA),
	name(NextLineUIA, NextLineChars),
	strip_leader(NextLineChars, PotentialFirstLine, [0' ,9, 0'|, 0'*]),
	PotentialFirstLine = [C | _],
	((C = 0'! ; C=0'-) ->
		FirstLine = end_of_comment
		;
		FirstLine = PotentialFirstLine
	).
search_lib_comment(Stream, end_of_comment, Module, Module).

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
/*------------------------------------------
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
 *-----------------------------------------*/

get_file_desc(Stream, FileDesc)
	:-
	skip_to_next(Stream, NextLineUIA),
	strip_white(NextLineUIA, StrippedLine),
	get_file_desc_first(StrippedLine, Stream, FileDesc).

get_file_desc_first('', Stream, FileDesc)
	:-!,
	get_file_desc(Stream, FileDesc).

get_file_desc_first(StrippedLine, Stream, FileDesc)
	:-
	sub_atom(StrippedLine, _, 2, _, '/*'),
	get_file_desc_body(Stream, FileDesc).

get_file_desc_first(StrippedLine, Stream, ['??']).


get_file_desc_body(Stream, FileDesc)
	:-
	skip_to_next(Stream, NextLineUIA),
	get_file_desc_body0(NextLineUIA, Stream, FileDesc).

get_file_desc_body0(end_of_file, Stream, []) :-!.
get_file_desc_body0(NextLineUIA, Stream, FileDesc)
	:-
	atom_codes(NextLineUIA, NextLineChars),
	strip_leader(NextLineChars, StrippedNextLineChars, [0' ,0'|,0'*, 0'	]),	% last skip is tab
	atom_codes(StrippedNextLine, StrippedNextLineChars),
	get_file_desc_body1(StrippedNextLine, Stream, FileDesc).

get_file_desc_body1(StrippedNextLine, Stream, [])
	:-
	sub_atom(StrippedNextLine, _, 2, _, '*/'),
	!.

get_file_desc_body1(StrippedNextLine, Stream, [StrippedNextLine | RestFileDesc])
	:-
	get_file_desc_body(Stream, RestFileDesc).


/*------------------------------------------
 *-----------------------------------------*/
search_lib_comment_init(Stream, CurMod, Module)
	:-
	get_line(Stream,Line),
	strip_white(Line, StrippedLine),
	disp_search_lib_comment_init(StrippedLine,Stream,CurMod,Module).

disp_search_lib_comment_init(end_of_file,Stream,Module,Module) :-!.
disp_search_lib_comment_init(Buffer,Stream,Module,Module) 
	:-
	sub_atom(Buffer, 0, 3, _, X3),
	(X3 = '/*!' 
		; (X3 = '/**', sub_atom(Buffer, 3, 1, _, CC), CC \= '*')  ),
	!.
disp_search_lib_comment_init(Buffer,Stream,CurMod,FinalModule) 
	:-
	sub_atom(Buffer, 0, 6, _, 'module'),
	atomread(Buffer, Term),
	Term = module(ThisModule),
	!,
	search_lib_comment_init(Stream,ThisModule,FinalModule).
disp_search_lib_comment_init(Buffer,Stream,CurMod,FinalModule) 
	:-
	    % already used strip_white to ensure no white space on left:
	sub_atom(Buffer, 0, 2, _, ':-'),
		% extend to handle white space after :-
	atom_length(Buffer, L),
		% greatest(X, (X >= 2, X<L, sub_atom(Buffer,X, 1, _, C), C != ' '), M)
	sub_atom(Buffer, 0, 8, _, ':-module'),
	atomread(Buffer, Term),
	Term = module(ThisModule),
	!,
	search_lib_comment_init(Stream,ThisModule,FinalModule).
disp_search_lib_comment_init(_,Stream,CurMod,Module)
	:-
	search_lib_comment_init(Stream,CurMod,Module).


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
get_pred_doc(Stream, FirstLine, File, Module, Doc)
	:-
	get_pred_desc(FirstLine,PredDesc),
%write('  '),write(PredDesc),nl,
	get_header_doc(Stream, HeaderDoc),
	(dmember(end_of_comment, HeaderDoc) ->
		BriefDescrip = end_of_comment
		;
		get_brief_descrip(Stream, BriefDescrip)
	),
	(BriefDescrip = end_of_comment ->
%		ExtendedDescrip = end_of_comment
		ExtendedDescrip = [BriefDescrip]
		;
		get_extended_descrip(Stream, InitExtendedDescrip),
		ExtendedDescrip = [BriefDescrip | InitExtendedDescrip]
	),
	(HeaderDoc = [TextPredDesc |  CallingForm_IOPatterns] ->
		true
		;
		TextPredDesc = '??',
		CallingForm_IOPatterns = []
	),
	!,
	make_doc_info(Doc, PredDesc, TextPredDesc, BriefDescrip, 
	          	CallingForm_IOPatterns, BriefDescrip, ExtendedDescrip,
		  	File, [], Module).

get_pred_doc(Stream, FirstLine, File, Doc)
	:-
	printf(user_output, '!!WARNING: Failure extracting information for predicate: %t\n', [FirstLine]).


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
	strip_leader(NextLineChars, StrippedNextLine, [0' ,0'|,0'*, 0'	]),	% last skip is tab
	disp_get_header_doc(StrippedNextLine, HeaderDoc, Stream).

disp_get_header_doc([], [], Stream) :-!.
disp_get_header_doc([0'! | _], [end_of_comment], Stream) :-!.
disp_get_header_doc([0'- | _], [end_of_comment], Stream) :-!.
disp_get_header_doc(StrippedNextLineCs, [StrippedNextLine | RestHeaderDoc], Stream)
	:-
	atom_codes(StrippedNextLine, StrippedNextLineCs),
	get_header_doc(Stream, RestHeaderDoc).

/*------------------------------------------
 *-----------------------------------------*/
get_brief_descrip(Stream, BriefDescrip)
	:-
	get_line(Stream, NextLineUIA),
	!,
	name(NextLineUIA,NextLineChars),
	strip_leader(NextLineChars, StrippedNextLine, [0' ,9,0'|,0'*, 0'-]),
	disp_get_brief_descrip(StrippedNextLine, BriefDescripCs, Stream),
	adjust_first_last_chars(BriefDescripCs, UCAdjustedBriefDescripCs),
	atom_codes(BriefDescrip, UCAdjustedBriefDescripCs).
get_brief_descrip(Stream, '??').

disp_get_brief_descrip('', BriefDescrip,Stream)
	:-!,
	get_brief_descrip(Stream, BriefDescrip).
disp_get_brief_descrip(BriefDescrip, BriefDescrip,Stream).



adjust_first_last_chars(BriefDescripCs, AdjustedBriefDescripCs)
	:-
	make_first_char_uc(BriefDescripCs, UCAdjustedBriefDescripCs),
	check_last_for_period(UCAdjustedBriefDescripCs, AdjustedBriefDescripCs).

make_first_char_uc([C | Cs], [AdjC | Cs])
	:-
	uc_adj(C, AdjC).

uc_adj(C, AdjC)
	:-
	0'a =< C,  C =< 0'z,
	!,
	AdjC is C - 32.
uc_adj(C, C).

check_last_for_period(Cs, AdjCs)
	:-
	dreverse(Cs, InitRCs),
	strip_white(InitRCs, RCs),
	RCs = [Last | RestRCs],
	disp_check_last_for_period(Last, RestRCs, NewRCs),
	dreverse(NewRCs, AdjCs).

disp_check_last_for_period(0'., RestRCs, [0'. | RestRCs])
	:-!.

disp_check_last_for_period(Last, RestRCs, [0'. | RestRCs])
	:-
	dmember(Last, [ 0';, 0',, 0'- ]),
	!.

disp_check_last_for_period(Last, RestRCs, [0'., Last | RestRCs]).


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

		/*-----------------------------------------
		 | INDEX DATA STRUCTURE DEFINITION
		 *----------------------------------------*/

start_index_datastruct(
		ModuleNames, 
		DestDir, 
		id(  ModsDirList,	% modules and directories
		     ModPredsLists, 	% per-module predicates
		     ModFilesLists, 	% per-module files
		     ModsInfoLists, 	% per-module misc info
		     (FF,p(FF))         % per-file information
		  )
		)
	:-
	path_elements(DestDir, DDElts),
	st_i_datas(ModuleNames, DDElts, ModsDirList, ModPredsLists, ModFilesLists, ModsInfoLists).

st_i_datas([], DDElts, [], [], [], []).
st_i_datas([Mod | ModuleNames], DDElts, 
		[Mod-ModDir | ModsDirList], 
		[Mod-(X,p(X)) | ModPredsLists],
		[Mod-(Y,p(Y)) | ModFilesLists],
		[Mod-(Z,p(Z)) | ModsInfoLists]
		)
	:-
	append(DDElts, [Mod], ModDirElts),
	path_elements(ModDir, ModDirElts),
	st_i_datas(ModuleNames, DDElts, ModsDirList, ModPredsLists, ModFilesLists, ModsInfoLists).

get_mods_dir_list(IndexDataStruct, ModsDirList)
	:-
	arg(1, IndexDataStruct, ModsDirList).

get_mods_preds_list(IndexDataStruct, ModsPredsList)
	:-
	arg(2, IndexDataStruct, ModsPredsList).
	
get_mods_files_list(IndexDataStruct, ModsFilesList)
	:-
	arg(3, IndexDataStruct, ModsFilesList).
	
get_mods_info_list(IndexDataStruct, ModsInfoList)
	:-
	arg(4, IndexDataStruct, ModsInfoList).
	
get_files_info_struct(IndexDataStruct, FilesInfoStruct)
	:-
	arg(5, IndexDataStruct, FilesInfoStruct).
	
get_mod_path(Mod, IndexDataStruct, ModPath)
	:-
	arg(1, IndexDataStruct, ModsDirList),
	dmember(Mod-ModPath, ModsDirList).

get_mod_xlist(Mod, IndexDataStruct, XList)
	:-
	arg(2, IndexDataStruct, MXLists),
	dmember(Mod-XList, MXLists).

insert_mod_preds_list([], _).
insert_mod_preds_list([Mod-DescList | GroupedDocList], IndexDataStruct)
	:-
	arg(2, IndexDataStruct, ModPredsLists),
	dmember(Mod-ModPreds, ModPredsLists),
	  %% Mod-([....|X], p(X))
	arg(2, ModPreds, p(CurTail)),
	DescList = (OpenTailDescList, t(NewTail)),
	CurTail = OpenTailDescList,
	mangle(2, ModPreds, p(NewTail)).
	insert_mod_preds_list(GroupedDocList, IndexDataStruct).

add_mod_file(Mod, File, IndexDataStruct)
	:-
	arg(3, IndexDataStruct, ModsFilesLists),
	dmember(Mod-XList, ModsFilesLists),
	  %% Mod-([....|X], p(X))
	arg(1, XList, OpenList),
	(careful_mem(OpenList, File) ->
		true
		;
		arg(2, XList, p(CurTail)),
		CurTail = [File | NewTail],
		mangle(2, XList, p(NewTail))
	).

add_file_to_mods([], _, _).
add_file_to_mods([Mod | CurModList], File, IndexDataStruct)
	:-
	add_mod_file(Mod, File, IndexDataStruct),
	add_file_to_mods(CurModList, File, IndexDataStruct).

careful_mem(XList, File)
	:-
	var(XList), !, fail.
careful_mem([File | XList], File) :-!.
careful_mem([_ | XList], File)
	:-
	careful_mem(XList, File).

add_mod_info(Mod, Item, IndexDataStruct)
	:-
	arg(4, IndexDataStruct, ModsFilesLists),
	dmember(Mod-XList, ModsFilesLists),
	  %% Mod-([....|X], p(X))
	(careful_mem(XList, Item) ->
		true
		;
		arg(2, XList, p(CurTail)),
		CurTail = [Item | NewTail],
		mangle(2, XList, p(NewTail))
	).

xappend([Pred], ModPreds, CurTail, FinalTail)
	:-!,
	CurTail = [Pred | FinalTail].

xappend([Pred | PredsList], ModPreds, CurTail, FinalTail)
	:-
	CurTail = [Pred | NextTail],
	xappend(PredsList, ModPreds, NextTail, FinalTail).

modules_list(IndexDataStruct, ModuleNames)
	:-
	arg(1, IndexDataStruct, ModsPaths),
	findall(M, member(M-P, ModsPaths), ModuleNames).

close_off_all([], []).
close_off_all([Mod-(List, p(Tail)) | Accum], [Mod-List | RestResult])
	:-
	Tail = [],
	close_off_all(Accum, RestResult).

close_off_everything(Data, ResultModLists, FilesList)
	:-
	get_mods_preds_list(Data, ModLists),
	close_off_all(ModLists, ResultModLists),
	get_files_info_struct(Data, FilesInfoStruct),
	FilesInfoStruct = (FilesList, p(FilesTail)),
	FilesTail = [].


add_file_info(File, FileDesc, FileDocList, IndexDataStruct)
	:-
	get_files_info_struct(IndexDataStruct, FilesInfoStruct),
	arg(2, FilesInfoStruct, p(CurTail)),
	CurTail = [File-(FileDesc,FileDocList) | NewTail],
	mangle(2, FilesInfoStruct, p(NewTail)).

get_files_list(IndexDataStruct, FilesList)
	:-
	get_files_info_struct(IndexDataStruct, FilesInfoStruct),
	FilesInfoStruct = (FilesList, p(Tail)),
	Tail = [].




%%%% ------- Test ----------
idt :-
	DestDir = 'foo/bar',

	start_index_datastruct([simple,simon], DestDir, IDS),
	get_mod_path(simple, IDS, SimplePath),
	get_mod_path(simon, IDS, SimonPath),
	printf('Paths: simple=%t  simon=%t\n', [SimplePath, SimonPath]),
	insert_mod_pred(simon, zipper/3, IDS),
	insert_mod_pred(simon, coat/2, IDS),
	get_mod_xlist(simon, IDS, SimonXList),
	printf('List:simon=%t\n', [SimonXList]).

/* output should be:
Paths: simple=foo\bar\simple  simon=foo\bar\simon
List:simon=[simple-(_A, p(_A)),simon-([zipper/3,coat/2|_B], p(_B))]
*/

%%%% ------- End Test ----------

		/*-----------------------------------------
		 | INDIVIDUAL DOC INFO STRUCTURE
		 *----------------------------------------*/

make_doc_info(Doc, PredDesc, TextPredDesc, BriefDescrip, 
          	CallingForm_IOPatterns, BriefDescrip, ExtendedDescrip,
	  	File, AdditionalInfo, Module)
	:-
	Doc = doc(PredDesc, 
		  (TextPredDesc, BriefDescrip), 
	          CallingForm_IOPatterns, 
		  BriefDescrip,
		  ExtendedDescrip,
		  File, 
		  AdditionalInfo, 
		  Module  ).

get_doc_preddesc(Doc, PredDesc)
	:-
	arg(1, Doc, PredDesc).
get_doc_descgroup(Doc, DescGroup)
	:-
	arg(2, Doc, DescGroup).
get_doc_calliopat(Doc, CallingForm_IOPatterns)
	:-
	arg(3, Doc, CallingForm_IOPatterns).
get_doc_briefdesc(Doc, BriefDesc)
	:-
	arg(4, Doc, BriefDesc).
get_doc_extdesc(Doc, ExtendedDesc)
	:-
	arg(5, Doc, ExtendedDesc).
get_doc_file(Doc, File)
	:-
	arg(6, Doc, File).

    /*----------------------------------------------------------
     |  AdditionalInfo should be a list of items of the form
     |			Tag = Value
     |  The list will be printed in the Additional Information segment,
     |  with one item per line (internal table row), in the form
     |			Tag: Value
     *----------------------------------------------------------*/
get_doc_addlinfo(Doc, AdditionalInfo)
	:-
	arg(7, Doc, AdditionalInfo).

add_addl_info(Doc, Tag, Value)
	:-
	arg(7, Doc, InitAdditionalInfo),
	NewAdditionalInfo = [Tag=Value | InitAdditionalInfo],
	mangle(Doc, 7, NewAdditionalInfo).


get_doc_module(Doc, Module)
	:-
	arg(8, Doc, Module).


endmod.


% some tests:
al :-
prologdoc(
    [ modulenames=[builtins,prologdoc,sio],
      destdir='als-lib-test',
      overwrite=true,
      windowtitle='ALS Prolog Library Components',
      doctitle= 'ALS Prolog Library Components',
      failonerror=true                        ],
    [  files( dir='../alspro/alsdir/library', all) ],
    [  group('String Manipulation', [modules=[builtins], files=['strings.pro']]),
       group('Tk Library Interface', [modules=[tk_alslib]]) ]
    ).

ab :-
prologdoc(
    [ modulenames=[builtins,sio,xconsult],
      destdir='als-btest',
      overwrite=true,
      windowtitle='ALS Prolog Builtins Components',
      doctitle= 'ALS Prolog Builtins Components',
      failonerror=true                        ],
    [  files( dir='../als_prolog/core/alsp_src/builtins', all), 
       files( dir='../als_prolog/core/alsp_src/library', all)
    ],
    [  group('String Manipulation', [modules=[builtins], files=['strings.pro']]),
       group('Tk Library Interface', [modules=[tk_alslib]]) ]
    ).

p :-
prologdoc(
    [ modulenames=[ alsdev, alsshell, app_gui_gen, app_utils, avl, builtins,
		cref, debugger, global_gv_info, macroxp, mpml, pgm_info,
		pml, prologdoc, pxml, rel_arith, shellmak, sio, sys_maint,
		tk_alslib, ttyshlmk, user, utilities, windows, xconsult     ],
      destdir='als-all',
      overwrite=true,
      windowtitle='ALS Prolog Builtins & Library Components',
      doctitle= 'ALS Prolog Builtins & Library Components',
      failonerror=true                        ],
    [  files( dir='../als_prolog/core/alsp_src/builtins', all), 
       files( dir='../als_prolog/core/alsp_src/library', all) ],
    [  group('String Manipulation', [modules=[builtins], files=['strings.pro']]),
       group('Tk Library Interface', [modules=[tk_alslib]]) ]
    ).

t :-
prologdoc(
    [ modulenames=[sio,builtins,prologdoc],
      destdir='test1',
      overwrite=true,
      windowtitle='Test1 Components',
      doctitle= 'Test1 Components',
      failonerror=true                        ],
    [  files( dir='../alspro/alsdir/library', all) ],
/*
 [
      files( dir='.', ['pd.pro'])
      ,files( dir='../alspro/alsdir/library', ['sio_misc.pro', 'strings.pro']),
      files( dir='testfiles2', ['control.pro', 'arithx1.pro'])                
      ],
*/
    [  group('String Manipulation', [modules=[builtins], files=['strings.pro']]),
       group('Tk Library Interface', [modules=[tk_alslib]]) ]
    ).
