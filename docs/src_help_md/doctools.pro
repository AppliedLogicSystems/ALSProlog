/*====================================================================*
 |				doctools.pro
 |              Copyright (c) 2018-2019 Applied Logic Systems, Inc.
 |		Group: Maintenance
 |		DocTitle: file_doc/0
 |		-- Tools for library maintenance, and builtins,library documentation
 |
 |	Primary predicates:
 |	    file_doc/0 - command line access to library & builtins file mainentance
 |	    new_page(Path)  - creates skeletal new *.md page
 |
 *====================================================================*/
module builtins.

export file_doc/0.
export do_files_setup/3.
export doc_one_file/2.
export do_np/0.
export np_list/1.
export new_page/1.
export mknnp/0.

/* ====================================================================== *
 |
 |              -- Install and document ALS Library and Builtins files
 |      
 |	For library files, works from <PathToLib>/<libfile>.pro, and generates
 |		   <PathToLib>/<libfile>.alb
 |	    together with 
 |		   <PathToMDFiles>/<libfile>.md
 |
 |	For builtins files, works from <PathToBuiltins>/<bltfile>.pro, and generates
 |		   <PathToMDFiles>/<bltfile>.md
 |
 |	Usually:
 |		<PathToLib> = <PathToALSPrologTree>/core/alsp_src/library
 |		<PathToBuiltins> = <PathToALSPrologTree>/core/alsp_src/builtins
 |		<PathToMDFiles> = <PathToALSPrologTree>/docs/docs/ref/
 |
 |	Issue: Work out paths and code extension so that a regular
 |	installation can add a library file to a local installation.
 |
 |	Can be started from the command line in ~docs/src_help_md, 
 |	as for example:
 |		alspro doctools.pro -g file_doc -p -lib listutl1
 |		alspro doctools.pro -g file_doc -p -blt filepath
 |                  or
 |		alspro doctools.pro -g file_doc -p -lib listutl1 listutl2 listutl3
 |
 |	By default, file_doc/0 will NOT overwrite the target *.md file.
 |	If the command line switch -ow is present (rightwards of the -p), 
 |	then it WILL overwrite the corresponding target *.md files.
 |
 |	Documenting ALS builtins files:
 |
 |	Structure a builtins file, e.g., filepath.pro, just like ALS
 |	library files.  Then executing
 |		alspro doctools.pro -g file_doc -p -blt filepath
 |	will produce the doc file filepath.md
 * ====================================================================== */

	%% Default: assumes this is being run in ~docs/src_help_md:
mdFolderPath('../docs/ref').
src_folder_md_files( P ) :- mdFolderPath( P ).

/*!---------------------------------------------------------------------
 |	file_doc/0
 |	file_doc
 |
 |	- command line access to library file mainentance
 |
 |	Reads the command_line, detects whether the -ow switch is
 |	present, and invokes processing on the list of files obtained,
 |	using do_files_setup/3.
 *!--------------------------------------------------------------------*/
file_doc 
	:-
	catch(do_file_doc, fd_error(Info), fd_except(Info)).

fd_except(Info)
	:-
	printf('file_doc error: %t ... Exiting.\n', [Info]),
	halt.

do_file_doc
	:-
	get_cmdline_vals(SwitchVals),
	pull_out_nullswitches(SwitchVals, Items, ReducedSwitchVals),
%printf('SwitchVals=%t\nItems=%t\nReducedSwitchVals=%t\n',[SwitchVals,Items,ReducedSwitchVals]),
	chkSrcTreeTop(ReducedSwitchVals, Src_Tree_Top),
	printf('Using source tree top = %t\n', [Src_Tree_Top]),

	check_for_blt_lib(ReducedSwitchVals, Switch, Block),
	get_target_files(ReducedSwitchVals, Items, Switch, Block, FilesList),

	check_for_overwrite(ReducedSwitchVals, Overwrite),
	sort(FilesList, SortedFilesList),

	catch(
		do_files_setup(SortedFilesList, Block, Overwrite),
		libissue(File, IssueInfo),
		exit_libsetup(Block, File, IssueInfo)
	),
	halt.

chkSrcTreeTop(ReducedSwitchVals, Src_Tree_Top)
	:-
	member(['-tt', Src_Tree_Top], ReducedSwitchVals),
	!,
	verify_STT(Src_Tree_Top),
	do_doc:assert(als_tree_top(Src_Tree_Top)).

	%% Are we running default in ~docs/src_help_md in some tree?
chkSrcTreeTop(_, Src_Tree_Top)
	:-
	in_docs_src(Src_Tree_Top),
	do_doc:assert(als_tree_top(Src_Tree_Top)),
	!.

	%% Can't find a tree top:
chkSrcTreeTop(_, _)
	:-
	sprintf(MsgString, 'Can\'t find a source tree top.\nConsider using the switch -tt\n',[]),
	atom_codes(MsgAtom, MsgString),
	throw(fd_error(MsgAtom)).

verify_STT(Src_Tree_Top)
	:-
	get_cwd(CurrentDir),
	exists_file(Src_Tree_Top),
	!,
	check_if_STT(Src_Tree_Top, CurrentDir).

verify_STT(Src_Tree_Top)
	:-
	sprintf(InfoString, 'Folder does not exist: %t\n', [Src_Tree_Top]),
	atom_codes(Info, InfoString),
	throw(fd_error(Info)).

check_if_STT(Src_Tree_Top, StartingDir)
	:-
	change_cwd(Src_Tree_Top),
	subdirs(STT_Dirs),
	member(core, STT_Dirs),
	member(foreign_sdk, STT_Dirs),
	member(unix, STT_Dirs),
	member(win32, STT_Dirs),
	!,
	change_cwd(StartingDir).

check_if_STT(Src_Tree_Top, _)
	:-
	sprintf(InfoString, '%t is not the top of an ALS Prolog source tree.\n', [Src_Tree_Top]),
	atom_codes(Info, InfoString),
	throw(fd_error(Info)).
 
in_docs_src(ParentSeg) :-
	get_cwd(CWD),
	split_path(CWD, CWDElements),
	reverse(CWDElements, RCWDElements),
	src_help_md_path_rev(RSHMP),
	init_seg_list(RSHMP, RCWDElements, RevParentSegElts),
	reverse(RevParentSegElts, ParentSegElts),
	join_path(ParentSegElts, ParentSeg).
	
src_help_md_path_rev([src_help_md,docs]).

init_seg_list([], ParentSegElts, ParentSegElts) :-!.
init_seg_list([A | LeftTail], [A | RightTail], ParentSegElts)
	:-!,
	init_seg_list(LeftTail, RightTail, ParentSegElts).


check_for_blt_lib(ReducedSwitchVals, '-lib', library)
	:-
	dmember(['-lib'], ReducedSwitchVals),
	!.

check_for_blt_lib(ReducedSwitchVals, '-lib', library)
	:-
	dmember(['-lib', _], ReducedSwitchVals),
	!.

check_for_blt_lib(ReducedSwitchVals, '-blt', builtins)
	:-
	dmember(['-blt'], ReducedSwitchVals),
	!.

check_for_blt_lib(ReducedSwitchVals, '-blt', builtins)
	:-
	dmember(['-blt', _], ReducedSwitchVals),
	!.

check_for_blt_lib(_, '-lib', library)
	:-
	printf('No -lib/-blt switch found. Using default: -lib/library.\n', []).

get_target_files([], [], Switch, Block, Items)
	:-!,
	sprintf(InfoString, 'Can\'t find any target files to process. \n', []),
	atom_codes(Info, InfoString),
	throw(fd_error(Info)).


get_target_files(ReducedSwitchVals, Items, Switch, Block, FilesList)
	:-
	setof(FF, member([Switch, FF], ReducedSwitchVals), RSVFiles),
	!,
	append(Items, RSVFiles, FilesList).
get_target_files(ReducedSwitchVals, Items, Switch, Block, Items).

check_for_overwrite(ReducedSwitchVals, true)
	:-
	member(['-ow'], ReducedSwitchVals),
	!,
	printf('Overwriting permission switch (-ow) detected\n',[]).

check_for_overwrite(ReducedSwitchVals, false)
	:-
	printf('Overwriting permission switch (-ow) NOT detected.\nExisting *.md files will not be overwritten.\n',[]).

libissue(LibFile, IssueInfo)
	:-
	printf('Errors in library file %t.pro:\n%t\n', [LibFile, IssueInfo]).

exit_libsetup(Block, File, IssueInfo)
	:-
	printf(user_output, '=======================\n',[]),
	printf(user_output, 'Issue while processing %t: %t:\n%t\n\n', [Block, File, IssueInfo]),

	printf(user_output, '+++++++++++++++++++++++\n',[]),
	printf(user_output, 'Exiting from doctools.pro/Block=%t setup due to problems in file %t\n',[BLock,File]),
	halt.

/*!---------------------------------------------------------------------
 |	do_files_setup/3
 |	do_files_setup(FilesList, Block, Overwrite)
 |	do_files_setup(+, +, +)
 |
 |	-- Processes a list of files using doc_one_file/2
 |
 |	If Block = library or builtins, and if Overwrite = true,false,
 |	and if FilesList is a a list of files from the library (when
 |	Block = library) or from ~builtins (when Block = builtins),
 |	recursively applies doc_one_file/2 to each file.
 *!--------------------------------------------------------------------*/
do_files_setup([], _, _).
do_files_setup([File | FilesList], Block, Overwrite)
	:-
	printf('Start processing %t file %t\n', [Block,File]),
	doc_one_file(File, Block, Overwrite),
	printf('%t file %t processed\n==================================\n\n', [Block,File]),
	do_files_setup(FilesList, Block, Overwrite).

/*!---------------------------------------------------------------------
 |	doc_one_file/2
 |	doc_one_file(LibFile, Overwrite)
 |	doc_one_file(+, +)
 |
 |	-- Workhorse for library maintenance on a single library file
 |
 |	Given the name (without extension) of a library file residing
 |	in the library area, constructs the path P to the file and
 |	reads all the lines in the file.  This file should be in the
 |	format specified by the file "lib_skeleton.txt" in the library
 |	area.  Then:
 |	A) Extracts Group and DocTitle from the head comment;
 |	B) Gets the Module;
 |	C) Gets the list EX of exported P/As;
 |	D) Assembles the contents CC of all of the lib-documenting comments
 |	   (comments with initial * followed by !);
 |	E) Extracts (and sorts) the list LPA of P/As obtained from CC;
 |	F) Checks for problems:
 |	   i) Mismatch between EX and LPA; 
 |	   ii) Checks that DocTitle is in both EX and LPA
 |	   If there is a mismatch, or ii) fails, throws a message and halts;
 |	G) Writes <LibFile>.alb in the library area ( next to <LibFile>.pro )
 |	H) If src_help_md/md_help/<LibFile>.md exists, then:
 |		1) If Overwrite==true, writes a new version of src_help_md/md_help/<LibFile>.md;
 |		2) If Overwrite\=true, skips this step
 |	I) If src_help_md/md_help/<LibFile>.md does not exist, then
 |	   writes a (new) version.
 *!--------------------------------------------------------------------*/
doc_one_file(File, Block, Overwrite)
	:-
	check_get_file_path(Block, File, FilePath, FolderPath),
	grab_lines(FilePath, LLs),
	headComment(LLs, HeadComment, HeadLinesTail),
	getGroupAndTitle(HeadComment, Group, DocTitle),

	getModule(HeadLinesTail, Module, AfterModLinesTail),
	crossWhiteLines(AfterModLinesTail, StartExportLines),
	getExports(StartExportLines, Exports, AfterExportLines),

	crossWhiteLines(AfterExportLines, NonWhiteLsAfterExports),
	findPredComments(NonWhiteLsAfterExports, CommentsList, PostBef, FinalTail),

	extr_doc(CommentsList, DocList),
	pullPAs(DocList, PAsList),
	issues_check(DocTitle, Exports, PAsList, File),

	SortedPAsList = PAsList, 

	setup_plain_file(File, PlainFile),
	(Block == library ->
		write_alb(PlainFile, Module, SortedPAsList, FolderPath)
		;
		true
	),
	write_md(PlainFile, Module, DocList, DocTitle, Group, Overwrite).

check_get_file_path(Block, File, FilePath, BlockPath)
	:-
	file_extension(File, F, X),
	block_path(Block, BlockPath),
	cont_file_path(X, File, Block, BlockPath, FilePath).


td_block_path(Leaf, [core,alsp_src,Leaf]).

block_path(Block, BlockPath)
	:-
	do_doc:als_tree_top(Src_Tree_Top),
	split_path(Src_Tree_Top, Src_Tree_Top_List),
	td_block_path(Block, BlockTail),
	append(Src_Tree_Top_List, BlockTail, BlockPathList),
	join_path(BlockPathList, BlockPath).
	
	%% No extension:
cont_file_path('', File, Block, BlockPath, FilePath)
	:-!,
	file_extension(FileName, File, pro),
	path_elements(FilePath, [BlockPath, FileName]),
	check_pro_file_exists(FilePath).

	%% .pro extension:
cont_file_path(pro, File, Block, BlockPath, FilePath)
	:-!,
	FileName = File,
	path_elements(FilePath, [BlockPath, FileName]),
	check_pro_file_exists(FilePath).
	
cont_file_path(X, File, Block, BlockPath, FilePath)
	:-
	sprintf(MsgString, 'Unknown file extension: %t\n', [X]),
	atom_codes(MsgAtom, MsgString),
	throw(fd_error(MsgAtom)).

check_pro_file_exists(FilePath)
	:-
	(exists_file(FilePath) ->
		true
		;
		sprintf(MsgString, 'File %t does not exist.\n', [FilePath]),
		atom_codes(MsgAtom, MsgString),
		throw(fd_error(MsgAtom))
	).

%%%--- ---------------------------------move to library:

remove_all([], _, []).

remove_all([Tgt | RestList], Tgt, TailList)
	:-!,
	remove_all(RestList, Tgt, TailList).

remove_all([Item | RestList], Tgt, [Item | TailList])
	:-
	remove_all(RestList, Tgt, TailList).

remove_all2([], _, []).

remove_all2([Tgt, Val | RestList], Tgt, Val, TailList)
	:-!,
	remove_all2(RestList, Tgt, Val, TailList).

remove_all2([Item | RestList], Tgt, Val, [Item | TailList])
	:-
	remove_all2(RestList, Tgt, Val, TailList).

	%Move to library
all_terms_to_atoms([], []).
all_terms_to_atoms([E | Exports], [AtomizedE | AtomizedExports])
	:-
	bufwrite(SE, E), 
	atom_codes(AtomizedE, SE),
	all_terms_to_atoms(Exports, AtomizedExports).


	%Move to library
insert_item_items([], _, []).
insert_item_items([Item], InsAtm, [Item])
        :-!.
insert_item_items([Item | RestIn_List], InsAtm, [Item, InsAtm | RestOut_List])
        :-
        insert_item_items(RestIn_List, InsAtm, RestOut_List).

%%%-----------------------------

	/*----------------------------------------------------------*
	 | Check for problems, and execute a throw if any are found.
	 *----------------------------------------------------------*/
issues_check(DocTitle, Exports, PAsList, LibFile)
	:-
	ErrMsgs0 = [],
	all_terms_to_atoms(Exports, AtomizedExports),

	list_diffs(AtomizedExports,PAsList,Exports_Not_PAsList,PAsList_NotExports),
	(PAsList_NotExports \= [] ->
		insert_item_items(PAsList_NotExports, '\n\t', PAL0),
		append(PAL0, ['\n'], PAL1),
		catenate(['----\nDocumented P/A''s not exported:\n\t' | PAL1], M_PA)
		;
		M_PA = ''
	),
	(Exports_Not_PAsList \= [] ->
		insert_item_items(Exports_Not_PAsList, '\n\t', EXP0),
		append(EXP0, ['\n'], EXP1),
		catenate(['----\nExported P/A''s not documented:\n\t' | EXP1], M_XP)
		;
		M_XP = ''
	),
	(not(member(DocTitle, AtomizedExports)) ->
		(not(member(DocTitle, PAsList)) ->
			catenate(['----\nDocTitle = ', DocTitle, ' neither exported nor documented\n'], M_DT)
			;
			catenate(['----\nDocTitle = ', DocTitle, ' not exported but is documented\n'], M_DT)
		)
		;
		% member(DocTitle, AtomizedExports) true
		(not(member(DocTitle, PAsList)) ->
			catenate(['----\nDocTitle = ', DocTitle, ' exported but not documented\n'], M_DT)
			;
			M_DT = ''
		)
	),
	catenate([M_PA, M_XP, M_DT], EMsg),
	(EMsg =='' ->
		write('No errors'),nl
		;
		throw(libissue(LibFile,  EMsg) )
	).

setup_plain_file(IncomingFile, PlainFile)
	:-
	file_extension(IncomingFile, F, X),
	fin_setup_plainfile(X, F, PlainFile).

fin_setup_plainfile('', PlainFile, PlainFile) :-!.
fin_setup_plainfile(pro, PlainFile, PlainFile) :-!.
fin_setup_plainfile(X, F, PlainFile) :-fail.

	/*----------------------------------------------------------*
	 | Extract components from the incoming list of lines.
	 *----------------------------------------------------------*/

headComment([L | LLs], HeadComment, LinesTail)
	:-
	blank_line(L),
	!,
	headComment(LLs, HeadComment, LinesTail).
headComment([L | LLs], [L | HeadComment], LinesTail)
	:-
	atom_codes(L, LCodes),
	head_comment_top(LCodes),
	headCommentRest(LLs, HeadComment, LinesTail).

headCommentRest([L | LLs], [L | HeadComment], LinesTail)
	:-
	atom_codes(L, LCodes),
	comment_interior(LCodes),
	!,
	headCommentRest(LLs, HeadComment, LinesTail).
headCommentRest([L | LLs], [L | HeadComment], LLs)
	:-
	atom_codes(L, LCodes),
	head_comment_bottom(LCodes).
	
comment_interior([0' , 0'| | LCodes]).

head_comment_bottom([0' , 0'* | LCodes])
	:-
	h_c_b_tail(LCodes).

h_c_b_tail(LCodes)
	:-
	cross_equals(LCodes, Tail),
	Tail = [0'*, 0'/].

getModule(['',ModLine | LinesTail], Module, LinesTail)
	:-
	getModule([ModLine | LinesTail], Module, LinesTail).
getModule([ModLine | LinesTail], Module, LinesTail)
	:-
	atomread(ModLine, module(Module)).

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
getExports([L | ExportLines], [Exp | Exports], AfterExportLines)
	:-
	atomread(L, export(Exp)),
	!,
	getExports(ExportLines, Exports, AfterExportLines).
getExports(AfterExportLines, [], AfterExportLines).

	/*-----------------------------------------------------------------------------
	 | findPredComments(NonWhiteLsAfterExports, CommentsList, PostBef, FinalTail),
	 |
 	 | 	-- Read all the special [/ *!--- ] comments into CommentsList
	 *----------------------------------------------------------------------------*/
findPredComments([], [], [], []).

findPredComments(Lines, [lcmt(Comment) | CommentsList], [b(BeforeTopLines) | RBTL], FinalTail)
	:-
	findNextComment(Lines, Comment, BeforeTopLines, InterLinesTail),
	!,
	findPredComments(InterLinesTail, CommentsList, RBTL, FinalTail).

findPredComments(Lines, [], [b(PLines)], Lines)
	:-
	peel_endmod(Lines, PLines).
	
findNextComment(Ls, [CmtTopLine | CommentLs], BeforeTopLines, InterLinesTail)
	:-	
	crossWhiteLines(Ls, NonWhiteLs),
	findCommentTop(Ls, CmtTopLine, BeforeTopLines, AfterTopTail),
	!,
	cmtRest(AfterTopTail, CommentLs, InterLinesTail).

	%% findCommentTop crosses whatever it sees until it hits a comment top;
	%% it accumulates what it crosses in BeforeTopLines.

findCommentTop([], _, _, _)
	:-!,
	fail.

findCommentTop([L | Ls], CmtTopLine, BeforeTopLines, AfterTopTail)
	:-
	sub_atom(L, 0, 4, _, '/*!-'),
	!,
	CmtTopLine = L,
	BeforeTopLines = [],
	AfterTopTail = Ls.

findCommentTop([L | Ls], CmtTopLine, [L | BeforeTopLines], AfterTopTail)
	:-
	findCommentTop(Ls, CmtTopLine, BeforeTopLines, AfterTopTail).

cmtRest([], [], []).
cmtRest([L | Lines], [L | CommentLs], LinesTail)
	:-
	sub_atom(L, 0, 2, _, ' |'),
	!,
	cmtRest(Lines, CommentLs, LinesTail).
cmtRest([L | LinesTail], [L | CommentLs], LinesTail)
	:-
	sub_atom(L, 0, 4, _, ' *!-'),
	!.

peel_endmod([Line | Lines], Lines)
	:-
	sub_atom(Line, 0, 7, _, 'endmod.'),
	!.
peel_endmod([L | Lines], [L | PLines])
	:-
	peel_endmod(Lines, PLines).

	/*---------------------------------------------------------------------
	 |	extr_doc(CommentsList, DocList).
 	 |
 	 |	-- extract predicate data from the CommentsList
 	 *--------------------------------------------------------------------*/

extr_doc([], []).
extr_doc([lcmt(C) | CommentsList], [D | DocList])
	:-
	C = [TopLine | RestLs],
	xtr_doc(RestLs, D),
	extr_doc(CommentsList, DocList).

%xtr_doc([L1, L2 | Ls], d(PA, Form, Summary, Desc))
xtr_doc([L1, L2 | Ls], d(PA, Form, Summary, Desc, Examples))
	:-
	clean_PA_entry(L1, PA),
	clean_white(L2, Form),
	getSummary(Ls, Summary, RestLs),
	crossWhiteLines(RestLs, StartDescLines),
	getDesc(StartDescLines, Desc, ExampleLines),
	getExamples(ExampleLines, Examples).

clean_PA_entry(L1, PA)
	:-
	clean_white(L1, CL1),
		% Now check that there is no trailing period:
	atom_codes(CL1, Codes),
	reverse(Codes, RCodes),
		% clean white space off the end:
	s_f(RCodes,  QCodes),
	(QCodes = [0'. | UCodes] ->
		ZCodes = UCodes ; ZCodes = QCodes),
	reverse(ZCodes, PACodes),
	atom_codes(PA, PACodes).
	
getSummary([SL | RestLs], Summary, RestLs)
	:-
		% Does SL start with ' |' ?
	sub_atom(SL, 0, 2, _, ' |'),
		% Yes, get the full tail of SL following the initial ' |':
	sub_atom(SL, 2, _, 0, SLX),
		% convert to codes & strip off initial blanks, tabs, and '|'s:
	strip_front(SLX, CodesSLX),
		% Extract the summary from the trailing codes, if it exists:
		% Note that the summary line must have a dash:
	xtr_summary(CodesSLX, Summary),
	!.

getSummary([L | Ls], Summary, RestLs)
	:-
	getSummary(Ls, Summary, RestLs).

	% Trailing codes CodesSLX start with a dash ('-'):
xtr_summary([0'- | TailCodesSLX], Summary)
	:-!,
		% Strip any more leading whitespace following the '-':
	s_f(TailCodesSLX, FinalCodesSLX),
	atom_codes(Summary, FinalCodesSLX).

	%% At the start of DESCRIPTION lines; break them
	%% into descriptions (Desc) followed by ExampleLines
getDesc(Ls, Desc, ExampleLines)
	:-
		%% Get the raw description lines
	getDesc0(Ls, InitDesc, ExampleLines),
	adj_desc_lines(InitDesc, Desc).

	%% Don't include comment ending line
getDesc0([L | _], [], [])
	:-
	sub_atom(L, 0, 4, _, ' *!-'),
	!.

	%% Don't include Examples start line (multiple versions)
getDesc0([L | ExampleLines], [], ExampleLines)
	:-
	sub_atom(L, 0, 11, _, ' | Examples'),
	!.
getDesc0([L | ExampleLines], [], ExampleLines)
	:-
	sub_atom(L, 0, 10, _, ' |Examples'),
	!.
getDesc0([L | ExampleLines], [], ExampleLines)
	:-
		% tab:
	sub_atom(L, 0, 11, _, ' |	Examples'),
	!.

	%% Ok description line
getDesc0([L | Ls], RR, ExampleLines)
	:-
	clean_white(L, R),
	(R == '' -> RR = Desc ; RR = [R | Desc]),
	getDesc0(Ls, Desc, ExampleLines).

adj_desc_lines([], []).
	%% Discard end-of-comment line
adj_desc_lines([Line | _], [])
	:-
	sub_atom(Line, 0, 4, _, ' *!-'),
	!.
adj_desc_lines([Line | Desc], [AdjLine | AdjDesc])
	:-
	strip_both_white_atom(Line, AdjLine),
	adj_desc_lines(Desc, AdjDesc).
	
getExamples([], []).
getExamples(InitExampleLines, AdjExamples)
	:-
	crossWhiteLines(InitExampleLines, StartExampleLines),
	adj_examp_lines(StartExampleLines, AdjExamples).

adj_examp_lines([], []).
adj_examp_lines([Line | _], [])
	:-
	sub_atom(Line, 0, 4, _, ' *!-'),
	!.
adj_examp_lines([Line | ExampleLines], AdjExamples)
	:-
	blank_line(Line),
	!,
	adj_examp_lines(ExampleLines, AdjExamples).
adj_examp_lines([Line | ExampleLines], [AdjLine | AdjExamples])
	:-
	strip_front(Line, CodesF),
	atom_codes(AdjLine, CodesF),
	adj_examp_lines(ExampleLines, AdjExamples).

	/*---------------------------------------------------------------------
	 | 	getGroupAndTitle(HeadComment, Group, DocTitle),
	 |
	 | 	-- extract the Group and Title from the Head comment
	 *--------------------------------------------------------------------*/
getGroupAndTitle([L, L0 | HeadCommentLs], Group, DocTitle)
	:-
	sub_atom(L, 0, 2, _, ' |'),
	sub_atom(L, 2, _, 0, GLine),
	strip_front(GLine, CodesF),
	append("Group: ", GLTail, CodesF),
	!,
	s_f(GLTail, CodesG),
	strip_white_tail(CodesG, Group),
	sub_atom(L0, 2, _, 0, TLine),
	strip_front(TLine, CodesT),
	append("DocTitle: ", TTail, CodesT),
	strip_white_tail(TTail, DocTitle).

getGroupAndTitle([L | HeadCommentLs], Group, DocTitle)
	:-
	getGroupAndTitle(HeadCommentLs, Group, DocTitle).
	
	
	/*---------------------------------------------------------------------
	 | 	write_alb(PlainFile, Module, PAsList, FolderPath)
	 |
	 |	-- write out the *.alb file in the library
	 *--------------------------------------------------------------------*/

write_alb(PlainFile, Module, PAsList, FolderPath)
	:-
	file_extension(AlbFullName, PlainFile, alb),
	join_path([FolderPath, AlbFullName], Alb_path),
		%% *.alb has no hand-entered info that is different
		%% from what is generated here, so it is ok
		%% to overwrite it:
	AlbTerm = libactivate(Module, [library, PlainFile], PAsList, []),

	open(Alb_path, write, OS),
	printf(OS, '%t.\n', [AlbTerm]),
	close(OS),
	open(Alb_path, read, IS),
		%% we're looking to see if there are syntax errors:
	read_term(IS, ABT, []),
	close(IS),
	printf('Library alb file %t written.\n', [AlbFullName]).

pullPAs([], []).
pullPAs([d(PA, Form, Summary, Desc, Examps) | DocList], [PA | PAsList])
	:-
	pullPAs(DocList, PAsList).

makePAsDs([], []).
makePAsDs([ d(PA, Form, Summary, Desc, Examps) | DocList],  [p(PA, Summary) | PAsDescs])
	:-
	makePAsDs(DocList, PAsDescs).

getPandA(PA, P, A)
        :-
        atomread(PA, PPAA, [syntax_errors(quiet)]),
        !,
        (PPAA = P/A ; P = PPAA, A=0).

getPandA(PA, P, A)
        :-
        atom(PA),
        sub_atom(PA, Bef, 1, _, '/'),
        sub_atom(PA, 0, Bef, AA, P),
        C is Bef + 1,
        sub_atom(PA, C, _, 0, Natom),
        atomread(Natom, A),
        !.
getPandA(P, P, 0)
        :-
        atom(P),
        !.

getPandA(P/A, P, A) :-!.

	/*---------------------------------------------------------------------
	 | 	write_md(PlainFile, Module, DocList, DocTitle, Group, Overwrite)
	 |
	 |	-- write out the *.md file in ../docs/ref
	 *--------------------------------------------------------------------*/

write_md(PlainFile, Module, DocList, DocTitle, Group, Overwrite)
	:-
	mdFolderPath(MDFolderPath),
	file_extension(MdFullName, PlainFile, md),
	join_path([MDFolderPath, MdFullName], Md_path),
		%% We can be entering EXAMPLES into <libfile>.md,
		%% so for now, refuse to overwrite it;
		%% ISSUE: Arrange for an additional commandline
		%% switch which will allow an overwrite.
	(Overwrite==true ->
		open(Md_path, write, OS)
		;
		(exists_file(Md_path) ->
			OS = user_output,
			printf(OS, 'File: %t already exists;\nRefusing to overwrite.\n',[Md_path]),
			printf(OS, 'Displaying generated MD file on console:\n', []),
			printf(OS, '========================================\n\n', [])
			;
			open(Md_path, write, OS)
		)
	),
	assemblePreds(DocList, Preds, Forms, Descs, Examps),

	yaml(DocTitle, als_library, Group, Module, Preds, OS),
	printf(OS, '## FORMS\n\n', []),
	writeFoms(Forms, OS),
	printf(OS, '## DESCRIPTION\n', []),
	writeDescriptions(Descs, OS),
	printf(OS, '\n## EXAMPLES\n\n', []),
	outExamps(Examps, OS),
	close(OS),
	printf('Library doc file %t written.\n', [MdFullName]).

assemblePreds([], [], [], [], []).
assemblePreds([d(PA, Form, Summary, Desc, Examps) | DocList], 
	      [p(PA,DescAtom) | Preds], 
	      [Form | Forms], 
	      [FullDesc | Descs], 
	      [x(PA,Examps) | RestExamps])
	:-
	(atom(Summary) -> DescAtom = Summary ;
		insert_spaces(Summary, SpacedSummary),
		catenate(SpacedSummary, DescAtom)
	),
	(Desc == [] ->
		SD = Summary,
		Desc0 = [SD]
		;
		Desc0 = Desc
	),
	mdDesc(PA, Desc0, FullDesc),
	assemblePreds(DocList, Preds, Forms, Descs, RestExamps).

mdDesc(PA, [Desc1 | RestDesc], [DL1 | RestFullDesc])
	:-
	sprintf(atom(DL1), '\n**`%t`** %t\n', [PA, Desc1]),
	mdDescFin(RestDesc, RestFullDesc).

mdDescFin([], []).
mdDescFin([RDL | RestDesc], [RFDL | RestFullDesc])
	:-
	sprintf(atom(RFDL), '    %t\n', [RDL]),
	mdDescFin(RestDesc, RestFullDesc).

writeFoms([], _).
writeFoms([Form | Forms], OS)
	:-
	printf(OS, '`%t`\n\n', [Form]),
	writeFoms(Forms, OS).

writeDescriptions([], OS).
writeDescriptions([Desc | Descs], OS)
	:-
	outDesc(Desc, OS),
	writeDescriptions(Descs, OS).
outDesc([], OS).
outDesc([DL | Desc], OS)
	:-
	write(OS, DL),
	outDesc(Desc, OS).

outExamps([], OS).
outExamps([x(PA, XLs) | Examps], OS)
	:-
	(XLs \= [] ->
		printf(OS, '**`%t`**\n```\n', [PA]),
		outXLs(XLs, OS),
		printf(OS, '```\n\n', [])
		;
		true),
	outExamps(Examps, OS).

outXLs([], OS).
outXLs([L | XLs], OS)
	:-
	printf(OS, '%t\n', [L]),
	outXLs(XLs, OS).


	/*---------------------------------------------------------------------
	 |		Utilities
	 *--------------------------------------------------------------------*/

blank_line('') :-!.
blank_line(L)
	:-
	atom_codes(L, LCodes),
	all_white(LCodes),
	!.

all_white([]).
all_white([0'  | LCodes])
	:-
	all_white(LCodes).
	% tab:
all_white([0'	 | LCodes])
	:-
	all_white(LCodes).

head_comment_top([0'/, 0'* | L])
	:-
	cross_equals(L, End),
	(End ==[] -> true ; End == [0'*]).
	
all_equals([]).
all_equals([0'= | L])
	:-
	all_equals(L).
cross_equals([0'= | LCodes], Tail)
	:-
	cross_equals(LCodes, Tail).

cross_equals(Codes, Codes).

crossWhiteLines([], []).
crossWhiteLines([L | Ls], NonWhiteLs)
	:-
	blank_line(L),
	!,
	crossWhiteLines(Ls, NonWhiteLs).
crossWhiteLines(NonWhiteLs, NonWhiteLs).

clean_white(Line, Result)
	:-
	strip_front(Line, CodesF),
	strip_white_tail(CodesF, Result).

strip_front(Line, CodesF)
	:-
	atom_codes(Line, LCodes),
	s_f(LCodes, CodesF).

s_f([0'  | LCodes], CodesF)
	:-!,
	s_f(LCodes, CodesF).
		% tab:
s_f([0'	 | LCodes], CodesF)
	:-!,
	s_f(LCodes, CodesF).
s_f([0'| | LCodes], CodesF)
	:-!,
	s_f(LCodes, CodesF).
s_f(LCodes, LCodes).

strip_white_tail(Codes, Result)
	:-
	reverse(Codes, RCodes),
	s_f(RCodes,  ZCodes),
	reverse(ZCodes, ResCodes),
	atom_codes(Result, ResCodes).

/* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&*/
/* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&*/

packages([core_prolog, alsdev, library, c_intf]).

default_package(core_prolog).

	% Readable texts for the packages:
pack_readable(core_prolog, 'Core Prolog').
pack_readable(alsdev, 'ALSDev').
pack_readable(als_library, 'ALS Library').
pack_readable(c_intf, 'C Interface').

	% Sub-Groups of the packages:
	%     pack_kid(<subgroup>, <package>).
pack_kid(control, core_prolog).
pack_kid(prolog_database, core_prolog).
pack_kid(terms, core_prolog).
pack_kid(input_output, core_prolog).
pack_kid(file_system, core_prolog).

pack_kid(lists, als_library).
pack_kid(strings, als_library).
pack_kid(web, als_library).

pack_kid(development_environment, alsdev).
pack_kid(gui_library, alsdev).
pack_kid(prolog_objects, alsdev).
pack_kid(tcltk_interface, alsdev).

	%% will probably grow:
pack_kid(c_intf, c_intf).

default_group(core_prolog, 'Terms').
default_group(alsdev, 'Development Env.').
default_group(als_library, 'Lists').
default_group(c_intf, 'C Data').

	% Readable texts for the package subgroups:
	% Core Prolog
group_display(control, 'Control') :-!.
group_display(input_output, 'Input Output') :-!.
group_display(prolog_database, 'Prolog Database') :-!.
group_display(terms, 'Terms') :-!.
group_display(file_system, 'File System') :-!.
group_display(uias, 'UIAs') :-!.

	% ALSDev
group_display(development_environment, 'Development Env.') :-!.
group_display(gui_library, 'Gui Library') :-!.
group_display(prolog_objects, 'Prolog Objects') :-!.
group_display(tcltk_interface, 'TclTk Interface') :-!.

	% ALS C-Interf
group_display(c_data, 'C Data') :-!.

	% ALS Library
group_display(G, GD)
	:-
	atom_codes(G, GCodes),
	GCodes = [C0 | RestGCodes],
	make_uc([C0], [UC0]),
	UcGCodes = [UC0 | RestGCodes],
	atom_codes(GD, UcGCodes).

default_module(core_prolog, builtins).
default_module(alsdev, alsdev).
default_module(als_library, builtins).
default_module(c_intf, builtins).


yaml(Title, Package, Group, Module, Preds, OS)
        :-
        printf(OS, '---\n', []),
        printf(OS, 'title: \'%t\'\n', [Title]),
	yaml_package(Package, OS),
	yaml_group(Group, Package, OS),
	yaml_module(Package, Group, Module, OS),
        printf(OS, 'predicates:\n', []),
        yaml_preds(Preds, OS),
        printf(OS, '---\n', []).

yaml_package('', OS) :-!.
yaml_package(Package, OS) 
	:-
	default_package(Package),
	!.
yaml_package(Package, OS) 
	:-
	pack_readable(Package, ReadablePackage),
	printf(OS, 'package: %t\n', [ReadablePackage]).

yaml_group('', Package, OS) :-!.
yaml_group(Group, Package, OS)
	:-
	default_group(Package, Group),
	group_display(Group, ReadableGroup),
	printf(OS, 'group: %t\n', [ReadableGroup]),
	!.
yaml_group(Group, Package, OS)
	:-
	group_display(Group, ReadableGroup),
	printf(OS, 'group: %t\n', [ReadableGroup]).

yaml_module(Package, Group, Module, OS)
	:-
	default_module(Package, Module), !.
yaml_module(Package, Group, Module, OS)
	:-
        printf(OS, 'module: %t\n', [Module]).

/*
yaml_preds([], _).
yaml_preds([p(PA,DescAtom) | Preds], OS)
        :-
        printf(OS, '- {sig: \'%t\', desc: \'%t\'}\n', [PA,DescAtom]),
        yaml_preds(Preds, OS).
*/
yaml_preds(Preds, OS)
	:-
	compact_preds(Preds, CompactedPreds),
	print_yaml_preds(CompactedPreds, OS).

compact_preds([], []).
compact_preds(Preds, [MPPAs | RestCompactedPreds])
	:-
	Preds = [p(PA,DescAtom) | _],
	getPandA(PA, P, A),
	xp(Preds, P, PAs, RestPreds),
	mkmp(PAs, P, MPPAs),
	compact_preds(RestPreds, RestCompactedPreds).
	
mkmp([p(PA,DA)], _, p(PA,DA)) :-!.
mkmp(PAsList, P, mp(P, PAsList)).

xp([], P, [], []).
xp([p(PA,DescAtom) | TailPreds], P, [p(P/A,DescAtom) | RestPAs], RestPreds)
	:-
	getPandA(PA, P, A),
	!,
	xp(TailPreds, P, RestPAs, RestPreds).
xp([p(PAX,DAX) | TailPreds], P, PAs, [p(PAX,DAX) | RestPreds])
	:-
	xp(TailPreds, P, PAs, RestPreds).

print_yaml_preds([], _).
print_yaml_preds([p(PA,DescAtom) | CompactedPreds], OS)
	:-!,
        printf(OS, '- {sig: \'%t\', desc: \'%t\'}\n', [PA,DescAtom]),
	print_yaml_preds(CompactedPreds, OS).

print_yaml_preds([mp(P, PAL) | CompactedPreds], OS)
	:-
	sort(PAL, SortedPAL),
	do_print_mp(SortedPAL, P, OS),
	print_yaml_preds(CompactedPreds, OS).
	
do_print_mp(PAL, P, OS)
	:-
	printf(OS, '- {sig: \'%t\', args: {\n', [P]),
	print_mp_args(PAL, OS),
	printf(OS, '  }}\n', []).

print_mp_args([], OS).
print_mp_args([p(P/A, D)], OS)
	:-!,
	strip_both_white_atom(D, StrippedD),
	printf(OS, '    %t: \'%t\'\n', [A, StrippedD]).
print_mp_args([p(P/A, D) | PAL], OS)
	:-
	strip_both_white_atom(D, StrippedD),
	printf(OS, '    %t: \'%t\',\n', [A, StrippedD]),
	print_mp_args(PAL, OS).
	

/*
short_descs([], OS).
short_descs([p(PA, DescAtom) | StrippedPreds], OS)
        :-
        printf(OS, '`%t` `--` %t\n\n', [PA,DescAtom]),
        short_descs(StrippedPreds, OS).
*/

	/* -------------------------------------------------------------------- *
	 |		new_page/1
	 |		new_page(Path)
	 |		new_page(+)
	 |
	 |	Creates the framework for a new alshelp page,
	 |	by making a skeletal *.md page in the folder given by
	 |
	 |		target_md_files('../docs/ref').
	 |
	 |	Path is a path to a "seed" file containing a term of the 
	 |	following form:
	 |
	 |		np(Package, Group, Title, FileName, Module,
	 |	    		[<List of <PAs with Descrips>>] ).
	 |
	 |	where:
	 |	-  member(Package, PList) holds, where packages(PList) holds (above).
	 |	-  pack_kid(Group, Package) holds
	 |	-  Title is of the form: P/A
	 |	-  ../docs/ref/<FileName>.md is the intended skeletal target
	 |	-  Module is where the predicates will be declared
	 |	-  Each <PAs with Descrips> is of the form
	 |		p(<quoted atom Q/R>, <atom which is a short description of Q/R>)
	 |
	 |	For example, here is np(...) seed for the curl help page 
	 |	    (in: xamps_np/np_for_curl.np):
	 |
	 |	np(
	 |	    core_prolog,     %Package
	 |	    input_output,    %Group
	 |	    'curl/[1,2,3]',  %Title
	 |	    curl123,         %FileName
	 |	    builtins,         %Module
	 |	    		     % Preds With Descriptions:
	 |	    [p('curl/1', 'Internet access via the curl package with URL, Options, Target in one list'),
	 |	     p('curl/2', 'Internet access via the curl package with separate URL arg, combined Options, Target in one list'),
	 |	     p('curl/3', 'Internet access via the curl package with separate URL, Options, Target args'),
	 |	     p('http/3', 'REST-inspired user-level interface for curl')
	 |	    ]
	 |	  ).
	 |
	 |	new_page/1 can be invoked from the command line using:
	 |
	 |		alspro doctools.pro -g do_np -p <path to xx.np>
	 |
	 |	The convenience predicate mknnp/0 will create the outline of
	 |	such a seed term np(...) in the file ./nnp.np.
	 * -------------------------------------------------------------------- */

mknnp :- open('./nnp.np', write, OS),
	printf(OS, 'np(\n   ,  %Package\n   ,  %Group\n   ,  %Title\n   ,  %FileName\n   ,  %Module\n  ',[]),
	printf(OS, '   % Preds With Descriptions:\n',[]),
	printf(OS, '   [p(   , \'  \'),\n',[]),
	printf(OS, '    p(   , \'  \'),\n',[]),
	printf(OS, '    p(   , \'  \')\n',[]),
	printf(OS, '   ]\n',[]),
	printf(OS,  '  ).\n', []),
	close(OS).
  
/*--------------------
%% Examples:
tt :- 
	Path = 'xamps_np/np_for_curl.np',
	new_page(Path).

tdf :- 
	Path = 'xamps_np/np_for_filepath.np',
	new_page(Path).
 --------------------*/

	%% alspro doctools.pro -g do_np -p <path to xx.np>
/*!---------------------------------------------------------------------
 |	do_np/0
 |	do_np
 |
 |	- command line access to new skeletal md file generation
 |
 |	Reads the command_line and invokes processing on the list of 
 |	files obtained, using np_list/1:
 |
 |	alspro doctools.pro -g do_np -p <path to xx1.np> <path to xx2.np> ...
 *!--------------------------------------------------------------------*/
do_np :-
	command_line(CommandLine),
	catch(
		np_list(CommandLine),
		npissue(IssueInfo, NPPath),
		exit_np(NPPath, IssueInfo)
	).

/*!---------------------------------------------------------------------
 *!--------------------------------------------------------------------*/
np_list([])
	:- halt.
np_list([NPPath | NPFiles])
	:-
	new_page(NPPath),
	np_list(NPFiles).

exit_np(IssueInfo, NPPath)
	:-
	printf(user_output, '=======================\n',[]),
	printf(user_output, 'Issue while trying to create  *.md file from: %t:\n', [NPPath]),
	printf(user_output, '    %t\n', [IssueInfo]).


/*!---------------------------------------------------------------------
 *!--------------------------------------------------------------------*/
new_page(Path)
	:-
	catch(do_new_page(Path),
		npissue(IssueInfo, Path),
		exit_np(IssueInfo, Path)
	).

do_new_page(Path)
	:-
	exists_file(Path),
	!,
	open(Path, read, IS),
	read(IS, NewPageTerm),
	close(IS),
	check_and_go(NewPageTerm, Path).
do_new_page(Path)
	:-
	sprintf(StringMsg, 'Path %t does not exist - skipping.\n', [Path]),
	atom_codes(IssueInfo, StringMsg),
	throw(npissue(IssueInfo, Path)).

check_and_go(np(Package, Group, Title, FileName, Module, PAsWithDescs), Path)
	:-
	check_package(Package, Path),
	check_group(Group, Package, Path),
	check_file(FileName, Path),
	check_PAsWithDs(PAsWithDescs, Path),
	make_new_page(Package, Group, Title, FileName, Module, PAsWithDescs).

check_package(Package, Path)
	:-
	packages(PList), 
	member(Package, PList),
	!.
check_package(Package, Path)
	:-

	sprintf(StringMsg,'Unknown package: %t in %t -- skipping %t\n', [Package, Path, Path]),
	atom_codes(IssueInfo, StringMsg),
	throw(npissue(IssueInfo, Path)).

check_group(Group, Package, Path)
	:-
	pack_kid(Group, Package),
	!.

check_group(Group, Package, Path)
	:-
	sprintf(StringMsg,'Bad Group: %t -- for package %t in: %t -- skipping %t\n', [Group,Package,Path,Path]),
	atom_codes(IssueInfo, StringMsg),
	throw(npissue(IssueInfo, Path)).

check_file(FileName,Path)
	:-
	file_extension(MDFile, FileName, md),
	mdFolderPath(MD_Src_Folder),
        join_path([MD_Src_Folder, MDFile], TgtMDFile),
	fin_check_file(TgtMDFile, Path).

fin_check_file(TgtMDFile, Path)
	:-
	not(exists_file(TgtMDFile)),
	!.

fin_check_file(TgtMDFile,Path)
	:-
	sprintf(StringMsg, 'File already exists: %t -- skipping %t\n', [TgtMDFile, Path]),
	atom_codes(IssueInfo, StringMsg),
	throw(npissue(IssueInfo, Path)).

check_PAsWithDs([], _).
check_PAsWithDs([PAD | PAsWithDescs], Path)
	:-
	check_pad(PAD, Path),
	!,
	check_PAsWithDs(PAsWithDescs, Path).

check_pad(p(QR, Desc), Path)
	:-
	atom(QR),
	!,
	open(atom(QR), read, IS),
	read_term(IS, QRTerm, [attach_fullstop(true)]),
	close(IS),
	cont_check_pad(QR, Desc, QRTerm, Path).

check_pad(p(Q/R, Desc), Path)
	:-
	fin_check_pad(QR, Q, R, Desc, Path).

check_pad(p(QR, Desc), Path)
	:-
	sprintf(StringMsg, 'In PAD=p(%t,...), %t must be atomic\n', [QR,QR]),
	atom_codes(IssueInfo, StringMsg),
	throw(npissue(IssueInfo, Path)).

cont_check_pad(QR, Desc, QRTerm, Path)
	:-
	QRTerm = Q/R,
	!,
	fin_check_pad(QR, Q, R, Desc, Path).

cont_check_pad(QR, Desc, QRTerm, Path)
	:-
	sprintf(StringMsg, 'In PAD=p(\'%t\',\'%t\'), %t must be an atom of the form Q/R,\n', [QR,Desc,QR]),
	atom_codes(IssueInfo, StringMsg),
	throw(npissue(IssueInfo, Path)).

fin_check_pad(QR, Q, R, Desc, Path)
	:-
	atom(Q), 
	(integer(R); integer_list(R) ),
	atom(Desc),
	!.

fin_check_pad(QR, Q, R, Desc, Path)
	:-
	sprintf(StringMsg, 'In PAD=p(\'%t\',\'%t\'), %t must be an atom of the form %t/R,\n  where R is an integer or list of integers, and %t must be atomic\n', [QR,Desc,QR,Q,Desc]),
	atom_codes(IssueInfo, StringMsg),
	throw(npissue(IssueInfo, Path)).



integer_list([]).
integer_list([N | Tail])
	:-
	integer(N),
	integer_list(Tail).

make_new_page(Package, Group, Title, FileName, Module, PAsWithDescs)
	:-
	file_extension(MDFile, FileName, md),
	mdFolderPath(MD_Src_Folder),
        join_path([MD_Src_Folder, MDFile], TgtMDFile),
	open(TgtMDFile, write, OS),
	sort(PAsWithDescs, SortedPAsWithDescs),
	(write_out_new_page(Package, Group, Title, Module, SortedPAsWithDescs, OS) 
		->
		close(OS),
		printf(user_output, '=======================\n',[]),
		printf(user_output, 'File %t created.\n',[MDFile])
		; 
		close(OS) 
	).

	/*---------------------------------------------------------------------
	 |		
	 *--------------------------------------------------------------------*/

write_out_new_page(Package, Group, Title, Module, PAsWithDescs, OS)
	:-
	yaml(Title, Package, Group, Module, PAsWithDescs, OS),
	length(PAsWithDescs, MinNumForms),
        printf(OS, '\n## FORMS\n\n', []),
	do_forms_skels(PAsWithDescs, OS),
        printf(OS, '\n## DESCRIPTION\n\n', []),
	do_descs_skels(PAsWithDescs, OS),
	finish_skeleton(OS).

	/*---------------------------------------------------------------------
	 |		
	 *--------------------------------------------------------------------*/

do_forms_skels([], OS).
do_forms_skels( [p(PA,DescAtom) | Preds], OS)
	:-
	getPandA(PA, P, A),
	B is A-1,
	n_of(B, ' %t', Fmt0),
	append(Fmt0, [' )`'], Fmt1),
	Fmt2 = [`, P, '(' | Fmt1],
	catenate(Fmt2, Fmt),
	n_of(B, ',', V0),
	printf(OS, Fmt,V0),
	nl(OS),
	blnk(OS),
	do_forms_skels(Preds, OS).

	/*---------------------------------------------------------------------
	 |		
	 *--------------------------------------------------------------------*/

do_descs_skels([], OS).
do_descs_skels( [p(PA,DescAtom) | Preds], OS)
	:-
	printf(OS, '\n**`%t`**  \n\n', [PA]),
	do_descs_skels(Preds, OS).

blnk(OS) :- printf(OS, '\n',[]).

finish_skeleton(OS)
	:-
        blnk(OS),
        printf(OS, '## EXAMPLES\n',[]), 
        blnk(OS),
        printf(OS, '```\n',[]),
        printf(OS, '1st example\n',[]),
        printf(OS, '```\n',[]),
        blnk(OS),
        printf(OS, '```\n',[]),
        printf(OS, '2nd example\n',[]), 
        printf(OS, '```\n',[]),
        blnk(OS),
        printf(OS, '## ERRORS\n',[]),
        blnk(OS),
        printf(OS, 'Errors text...\n',[]),
        blnk(OS),
        printf(OS, '## NOTES\n',[]),
        blnk(OS),
        printf(OS, 'Notes text...\n',[]),
        blnk(OS),
        printf(OS, '## SEE ALSO\n',[]),
        blnk(OS),
        printf(OS, '- `F/N1`\n',[]),
        printf(OS, '- `G/N2`\n',[]),
        printf(OS, '- [Bowen 91, 7.6 ]\n',[]),
        printf(OS, '- [Sterling 86, 9.2 ]\n',[]),
        printf(OS, '- [Bratko 86, 7.2 ]\n',[]),
        printf(OS, '- [Clocksin 81, 6.5 ]\n',[]).

endmod.
