/*==================================================================
 |		progdoc.pro
 |	Copyright (c) 1991-93 Applied Logic Systems, Inc.
 |
 |   Doc Tools for maintaining a library, including the ALS Library
 |		-- originally part of libmaint.pro, separated in 1993
 |
 |	Date: October, 1991
 |	Author: Ken Bowen
 |	Revised: May 1993
 *==================================================================*/

module sys_maint.

export dt/0.
dt :-
	InSourceFile = '/elvis/prolog/Utils/misc_db.pro',
	lib_man_info(Stream, FileName, LibraryKey, ExportList, Module,
			 DocumentedPredsList, GroupsInfo).

		/*-----------------------------------------
		 |    LIBRARY DOCUMENTATION PROCESSING
		 *----------------------------------------*/
/*------------------------------------------
 *-----------------------------------------*/
lib_man_info(SourceFile, FileName, LibraryKey, ExportList, Module,
			 DocumentedPredsList, GroupsInfo)
	:-
	open(SourceFile, read, InStream, []),
	assemble_documentation(InStream, NamesList, FormsList,  DescriptionsList, 
	                       DocumentedPredsList, GroupsInfo),
	close(InStream),
	write_documentation(NamesList, FormsList, 
                    DescriptionsList, DocumentedPredsList).

eof_char(-1).
eoln_char(13).
eoln_char(10).

/*------------------------------------------
 *-----------------------------------------*/
assemble_documentation(Stream, NamesList, FormsList, 
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
	bufread(FirstLine, ReadResult),
	((ReadResult = [ErrMsg | Pos],integer(Pos)) ->
		PredDesc = '??'/'?',
		cur_lib_file(CurLibFile),
		lib_error(parse(CurLibFile, FirstLine, ErrMsg, Pos));
		ReadResult = [PredDesc | Vars]
	),
	get_header_doc(Stream, HeaderDoc),
	(dmember(end_of_comment, HeaderDoc) ->
		BriefDescrip = end_of_comment;
		get_brief_descrip(Stream, BriefDescrip)
	),
	(BriefDescrip = end_of_comment ->
		ExtendedDescrip = end_of_comment;
		get_extended_descrip(Stream, ExtendedDescrip)
	),
	(HeaderDoc = [TextPredDesc |  CallingForm_IOPatterns] ->
		true;
		TextPredDesc = '??',
		CallingForm_IOPatterns = []
	),
	!,
	Doc = doc(PredDesc, (TextPredDesc, BriefDescrip), 
	          CallingForm_IOPatterns, [TextPredDesc | ExtendedDescrip]).

/*------------------------------------------
 *-----------------------------------------*/
get_header_doc(Stream, HeaderDoc)
	:-
%	readline(NextLine),
	get_line(Stream, NextLineUIA),
	name(NextLineUIA,NextLineChars),
	strip_leader(NextLineChars, StrippedNextLine, [0' ,9,0'|,0'*]),
	disp_get_header_doc(StrippedNextLine, HeaderDoc, Stream).

disp_get_header_doc([], [], Stream) :-!.
disp_get_header_doc([0'! | _], [end_of_comment], Stream) :-!.
disp_get_header_doc(StrippedNextLine, 
					[StrippedNextLine | RestHeaderDoc], Stream)
	:-
	get_header_doc(Stream, RestHeaderDoc).

/*------------------------------------------
 *-----------------------------------------*/
get_brief_descrip(Stream, BriefDescrip)
	:-
%	readline(NextLine),
	get_line(Stream, NextLineUIA),
	name(NextLineUIA,NextLineChars),
	strip_leader(NextLineChars, StrippedNextLine, [0' ,9,0'|,0'*, 0'-]),
	disp_get_brief_descrip(StrippedNextLine, BriefDescrip,Stream).

%disp_get_brief_descrip([], BriefDescrip,Stream)
disp_get_brief_descrip('', BriefDescrip,Stream)
	:-!,
	get_brief_descrip(Stream, BriefDescrip).
disp_get_brief_descrip(BriefDescrip, BriefDescrip,Stream).

/*------------------------------------------
 *-----------------------------------------*/
get_extended_descrip(Stream, ExtendedDescrip)
	:-
%	readline(NextLine),
	get_line(Stream, NextLineUIA),
	name(NextLineUIA,NextLineChars),
	strip_leader(NextLineChars, StrippedNextLine, [0' ,9,0'|,0'*]),
	disp_get_extended_descrip(StrippedNextLine,ExtendedDescrip,Stream).

disp_get_extended_descrip(StrippedNextLine,[],Stream)
	:-
	StrippedNextLine = [0'! | _], !.

disp_get_extended_descrip(StrippedNextLine,
                          [StrippedNextLine | RestExtendedDescrip],Stream)
	:-
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
write_documentation(Stm, NamesList, FormsList, 
                    DescriptionsList, DocumentedPredsList)
	:-
	printf(Stm,"\n@-----Name/Brief Description-----\n\n",[]),
	write_names_list(NamesList,Stm),

	printf(Stm,"\n@-----Forms & I/O Patterns-----\n\n",[]),
	write_string_list(FormsList,Stm),

	printf(Stm,"\n@-----Description-----\n\n",[]),
	write_descrips_list(DescriptionsList,Stm).

/*------------------------------------------
 *-----------------------------------------*/
write_names_list([],_).
write_names_list([(PredDesc,BriefDescrip) | RestNamesList],Stm)
	:-
    PredDesc \= end_of_comment,
	!,
	printf(Stm,"%t -- %t\n",[PredDesc,BriefDescrip]),
	write_names_list(RestNamesList,Stm).
write_names_list([_ | RestNamesList],Stm)
	:-
	write_names_list(RestNamesList,Stm).

/*------------------------------------------
 *-----------------------------------------*/
write_descrips_list([],_).
write_descrips_list([Descrip | RestDescriptionsList],Stm)
	:-
    Descrip \= [end_of_comment | _],
	!,
	write_string_list(Descrip,Stm),nl(Stm),nl(Stm),
	write_descrips_list(RestDescriptionsList,Stm).
write_descrips_list([_ | RestDescriptionsList],Stm)
	:-
	write_descrips_list(RestDescriptionsList,Stm).

write_string_list([],_).
write_string_list([String | Strings],Stm)
	:-
	printf(Stm,"%t\n",[String]),
	write_string_list(Strings,Stm).

lib_error(multi_module(CurLibFile))
	:-
	error_message("Error: Multiple modules declared in library file %t\n",
				  [CurLibFile]).
lib_error(multi_key(CurLibFile))
	:-
	error_message("Error: Multiple library keys declared in library file %t\n",
				  [CurLibFile]).
lib_error(parse(CurLibFile, Line, ErrMsg, Pos))
	:-
	error_message("Error: Bad parse in library file %t: \n%s\nPos=%t: %s\n",
				  [CurLibFile,Line,Pos,ErrMsg]).

lib_error(unknown(InputFileName))
	:-
	error_message("Error in processing library file: %t\n",[InputFileName]).

lib_error(bad_mod_close(TagSortMCs))
	:-
	MessageList = [
		"Mismatch on module closures:\n"-[],
		common_pattern("%t\n",TagSortMCs)  ],
	error_messages(MessageList).

lib_error(bad_exports(CommonExports,UncommonExports))
	:-
	MessageList = [
		"Mismatch on predicate exports--\n"-[],
		"Common exports are:\n%t\n=======\n"-[CommonExports],
		"Differences are:\n"-[],
		common_pattern("%t\n-----\n",UncommonExports)   ],
	error_messages(MessageList).

error_message(Pattern, Values)
	:-
    telling(Where), tell(user),
	printf(Pattern, Values),
    tell(Where).

error_messages([]) :-!.
error_messages(MessageList)
	:-
	telling(Where), tell(user),
	error_messages0(MessageList),
	tell(Where).

error_messages0([]).
error_messages0([Pattern-Data | MessageList])
	:-!,
	error_message(Pattern,Data),
	error_messages0(MessageList).
error_messages0([common_pattern(Pattern,DataList) | MessageList])
	:-!,
	error_messages1(DataList,Pattern),
	error_messages0(MessageList).

error_messages1([],_).
error_messages1([Datum | DataList],Pattern)
	:-
	error_message(Pattern, Datum),
	error_messages1(DataList,Pattern).




err_messages([],_).
err_messages([Datum | Data],Pattern)
	:-
	error_message(Pattern,Datum),
	err_messages(Data,Pattern).

export tc/0.
tc :-
install_lib([unix,fsunix,dos386,fsdos386], '/u/prolog/Utils/Develop',
		    LoadConditon, ReturnVar).

export install_lib/4.
install_lib(CondFilesList, SourceDir, LoadConditon, ReturnVar)
    :-
	lib_analyze_all(CondFilesList, SourceDir, Exports, 
					ModuleCloseups, Modules, LibraryKeys),

	consis_analyze(Exports, ModuleCloseups, Modules, LibraryKeys,
					TheExports, TheModuleClosures, Module, LibraryKey),

	lib_location(Disk, ALSDIR_Path, LibPath),
	dappend(LibPath,[info], LibInfoDirPath),

			%% get path to blt_lib.pro:
	rootPathFile(Disk,ALSDIR_Path,[blt_lib,pro],BltLibFile),

			%% Save all prev info in blt_lib.pro:
	collect_lib_load_info(BltLibFile, PrevLibInfo).


install_lib(CondFilesList, SourceDir, LoadConditon, ReturnVar)
    :-
	lib_error(unknown(CondFilesList)).

lib_analyze_all([], _, [], [],[], []).
lib_analyze_all([Item,InputFileName | RestCondFilesList], SourceDir,
			    Exports, ModuleCloseups,Modules, LibraryKeys)
	:-
	(filePlusExt(_,_,InputFileName) ->
		FileName = InputFileName;
		filePlusExt(InputFileName,pro,FileName)
	),
    pathPlusFile(SourceDir,FileName,SourceFile),
	(exists_file(SourceFile) ->
        true;
        printf("Source file %t does not exist.\n",[SourceFile]),
        fail
    ),

	see(SourceFile),
	lib_analyze(ExportList, ModuleClosures,Module, LibraryKey),
	seen,
	!,

   	(var(Module) ->
      	Module = user; true),

   	(var(LibraryKey) ->
      	LibraryKey = FileName; true),

	Exports = [Item-ExportList | RestExports],
	ModuleCloseups = [Item-ModuleClosures | RestModuleCloseups],
	Modules = [Item-Module | RestModules],
	LibraryKeys = [Item-LibraryKey | RestLibraryKeys],
	lib_analyze_all(RestCondFilesList, SourceDir, RestExports, 
					RestModuleCloseups, RestModules, RestLibraryKeys).

lib_analyze_all([_,InputFileName | RestCondFilesList], SourceDir,
			    Exports, ModuleCloseups,Modules, LibraryKeys)
	:-
	lib_error(unknown(InputFileName)),
	lib_analyze_all(RestCondFilesList, SourceDir, Exports, 
					ModuleCloseups, Modules, LibraryKeys).

consis_analyze(TagExports, TagModuleClosures, TagModules, TagLibraryKeys,
				TheExports, TheModuleClosures, Module, LibraryKey)
	:-
	remove_item_tags(TagModules, Modules),
	sort(Modules, ModulesResidue),
	(ModulesResidue = [Module] ->
		true;
		lib_error(multi_module(Modules)), fail
	),

	remove_item_tags(TagLibraryKeys, LibraryKeys),
	sort(LibraryKeys, LibraryKeysResidue),
	(LibraryKeysResidue = [LibraryKey] ->
		true;
		lib_error(multi_key(LibraryKeys)), fail
	),

	remove_item_tags(TagModuleClosures, ModuleClosures),
	sort_each(ModuleClosures, SortedModuleClosures),
	sort(SortedModuleClosures, ModuleClosuresResidue),
	(ModuleClosuresResidue = [TheModuleClosures] ->
		true;
		retag(TagModuleClosures,SortedModuleClosures,TagSortMCs),
		lib_error(bad_mod_close(TagSortMCs)), fail
	),

	remove_item_tags(TagExports, Exports),
	sort_each(Exports, SortedExports),
	sort(SortedExports, ExportsResidue),
	(ExportsResidue = [TheExports] ->
		true;
		intersect(ExportsResidue, CommonExports),
		all_list_diffs(TagExports, CommonExports, UncommonExports),
		lib_error(bad_exports(CommonExports,UncommonExports)), 
		fail
	).



remove_item_tags([], []).
remove_item_tags([_-Item | TagItems], [Item | Items])
	:-
	remove_item_tags(TagItems, Items).

sort_each([], []).
sort_each([List | Lists], [SortedList | SortedLists])
	:-
	sort(List, SortedList),
	sort_each(Lists, SortedLists).

retag([],[],[]).
retag([Tag-_ | RestTagged],[New | RestNew],[[Tag-New] | RestTaggedNew])
	:-
	retag(RestTagged,RestNew,RestTaggedNew).

all_list_diffs([], _, []).
all_list_diffs([Tag-ThisExports | TagExports], CommonExports, 
			   [[Tag-ThisDiff] | UncommonExports])
	:-
	list_diff(ThisExports, CommonExports, ThisDiff),
	all_list_diffs(TagExports, CommonExports, UncommonExports).

endmod.



