/* ====================================================================== *
 |			libwork.pro
 |		Copyright (c) 2018 Applied Logic Systems, Inc.
 |		Group: Library Maintenance
 |		DocTitle: libsetup/0
 |      
 |              -- Install and document alslib library files
 |      
 |	Works from from <PathToLib>/<libfile>.pro, and generates
 |		<PathToLib>/<libfile>.alb
 |	together with 
 |	    ~docs/src_help_md/md_help/<libfile>.md
 |
 |	If pandoc is available, translates <libfile>.md to
 |	    ~docs/src_help_md/alshelp/<libfile>.html
 |
 |	Also makes an entry for <libfile> in
 |	     ~docs/src_help_md/docsdb_alslib.pro
 |	and makes entries for <libfile> in
 |	    ~docs/src_help_md/alshelp/toc_alslib.html
 |	and
 |	    ~docs/ref-manual/md_toc_alslib.html
 |
 |	Assumes one is running in ~docs/src_help_md	
 |	Issue: Work out paths and code extension so that a regular
 |	installation can add a library file.
 |
 |	Can be started from the command line in ~docs/src_help_md, 
 |	as for example:
 |
 |		alspro libwork.pro -g libsetup -p listutl1
 |                  or
 |		alspro libwork.pro -g libsetup -p listutl1 listutl2 listutl3
 |
 |	By default, libsetup will NOT overwrite the target *.md file.
 |	If the command line switch -ow is present, then it WILL overwrite
 |	the target *.md file.
 |
 |	Note: Depends on doctools.pro
 * ====================================================================== */
:-['doctools.pro'].

export libsetup/0.

	%% Assumes this is being run in ~docs/src_help_md:
mdFolderPath('./md_help').
src_helpPath('./').
lib_dev_path('../../core/alsp_src/library').

	%% Convenience during development:
tlu1 :- doc_lib_file(listutl1).
tlu3 :- doc_lib_file(listutl3).
tlu4 :- doc_lib_file(listutl4).

/*!---------------------------------------------------------------------
 |	libsetup/0
 |	libsetup
 |
 |	- command line access to library file mainentance
 |
 |	Reads the command_line, detects whether the -ow switch is
 |	present, and invokes processing on the list of files obtained.
 *!--------------------------------------------------------------------*/
libsetup :-
	command_line(CommandLine),
	(member('-ow', CommandLine) ->
		Overwrite = true,
		remove_all(CommandLine, '-ow', LibFilesList)
		;
		Overwrite = false,
		LibFilesList = CommandLine
	),
		%% remove any duplicates:
	sort(LibFilesList, SortedLibFilesList),
	catch(
		do_libsetup(SortedLibFilesList, Overwrite),
		libissue(LibFile, IssueInfo),
		exit_libsetup(LibFile, IssueInfo)
	),
	halt.

exit_libsetup(LibFile, IssueInfo)
	:-
	printf(user_output, '=======================\n',[]),
	printf(user_output, 'Issue while processing library: %t:\n%t\n\n', [LibFile, IssueInfo]),

	printf(user_output, '+++++++++++++++++++++++\n',[]),
	printf(user_output, 'Exiting from libwork.pro/libsetup due to problems in file %t\n',[LibFile]),
	halt.

%%%--- move to library:
remove_all([], _, []).

remove_all([Tgt | RestList], Tgt, TailList)
	:-!,
	remove_all(RestList, Tgt, TailList).

remove_all([Item | RestList], Tgt, [Item | TailList])
	:-
	remove_all(RestList, Tgt, TailList).
%%%---

do_libsetup([],_).
do_libsetup([LibFile | LibFilesList], Overwrite)
	:-
	printf('Processing Library file %t\n', [LibFile]),
	doc_lib_file(LibFile, Overwrite),
	printf('Library file %t processed\n==================================\n\n', [LibFile]),
	do_libsetup(LibFilesList, Overwrite).

/*!---------------------------------------------------------------------
 |	doc_lib_file/2
 |	doc_lib_file(LibFile, Overwrite)
 |	doc_lib_file(+, +)
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
 |	G) Writes <LibFile>.alb in the library area (next to <LibFile.pro>)
 |	H) i) If src_help_md/md_help/<LibFile>.md exists, then:
 |		1) If Overwrite==true, writes a new version of src_help_md/md_help/<LibFile>.md;
 |		2) If Overwrite\=true, skips this step
 |	I) If src_help_md/md_help/<LibFile>.md does not exist, then
 |	   writes a (new) version.
 |	J) Emits the system call
 |		pandoc -s -o <HtmlPath> <Md_path>
 |	   where 
 |		Md_path == ~src_help_md/md_help/<LibFile>.md 
 |	   and
 |	   	HtmlPath == ~src_help_md/alshelp/<LibFile>.html
 |	K) Invokes
 |		idx_lib_page(Group, DocTitle, LibFile, PAsWithDescs)
 |	   which makes (or updates) an entry for LibFile in
 |		~src_help_md/docsdb_alslib.pro
 |	   and then uses docsdb_alslib.pro to rewrite the table of contents
 |	   	~src_help_md/alshelp/toc_alslib.html
 |	Note that step K) relies on doctools.pro.
 *!--------------------------------------------------------------------*/
doc_lib_file(LibFile, Overwrite)
	:-
	lib_dev_path(LibraryFolderPath),
	file_extension(FullName, LibFile, pro),
	join_path([LibraryFolderPath, FullName], L_path),

	grab_lines(L_path, LLs),
	headComment(LLs, HeadComment, HeadLinesTail),
	getGroupAndTitle(HeadComment, Group, DocTitle),

	getModule(HeadLinesTail, Module, AfterModLinesTail),
	crossWhiteLines(AfterModLinesTail, StartExportLines),
	getExports(StartExportLines, Exports, AfterExportLines),

	crossWhiteLines(AfterExportLines, NonWhiteLsAfterExports),
	findPredComments(NonWhiteLsAfterExports, CommentsList, PostBef, FinalTail),

	extr_doc(CommentsList, DocList),
	pullPAs(DocList, PAsList),
	issues_check(DocTitle, Exports, PAsList, LibFile),

	sort(PAsList, SortedPAsList),

	write_alb(LibFile, Module, alslib, SortedPAsList, LibraryFolderPath),
	write_md(LibFile, Module, DocList, DocTitle, Group, Md_path, Overwrite),
	file_extension(LibHtml, LibFile, html),
	join_path(['./alshelp', LibHtml], HtmlPath),
	catenate(['pandoc -s -o ', HtmlPath , ' ', Md_path], PandocCmd),
	system(PandocCmd),
	write(PandocCmd),nl,

	makePAsDs(DocList, PAsWithDescs),
	idx_lib_page(Group, DocTitle, LibFile, PAsWithDescs).
	

	%Move to library
all_to_atoms([], []).
all_to_atoms([E | Exports], [AtomizedE | AtomizedExports])
	:-
	bufwrite(SE, E), 
	atom_codes(AtomizedE, SE),
	all_to_atoms(Exports, AtomizedExports).


	%Move to library
insert_item_items([], _, []).
insert_item_items([Item], InsAtm, [Item])
        :-!.
insert_item_items([Item | RestIn_List], InsAtm, [Item, InsAtm | RestOut_List])
        :-
        insert_item_items(RestIn_List, InsAtm, RestOut_List).


issues_check(DocTitle, Exports, PAsList, LibFile)
	:-
	ErrMsgs0 = [],
	all_to_atoms(Exports, AtomizedExports),

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


/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/

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

/*---------------------------------------------------------------------
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
	all_equals(L).

all_equals([]).
all_equals([0'= | L])
	:-
	all_equals(L).
cross_equals([0'= | LCodes], Tail)
	:-
	cross_equals(LCodes, Tail).

cross_equals(Codes, Codes).

getModule([ModLine | LinesTail], Module, LinesTail)
	:-
	atomread(ModLine, module(Module)).

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
/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
getExports([L | ExportLines], [Exp | Exports], AfterExportLines)
	:-
	atomread(L, export(Exp)),
	!,
	getExports(ExportLines, Exports, AfterExportLines).
getExports(AfterExportLines, [], AfterExportLines).

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
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

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/

peel_endmod([Line | Lines], Lines)
	:-
	sub_atom(Line, 0, 7, _, 'endmod.'),
	!.
peel_endmod([L | Lines], [L | PLines])
	:-
	peel_endmod(Lines, PLines).


extr_doc([], []).
extr_doc([lcmt(C) | CommentsList], [D | DocList])
	:-
	C = [TopLine | RestLs],
	xtr_doc(RestLs, D),
	extr_doc(CommentsList, DocList).

xtr_doc([L1, L2 | Ls], d(PA, Form, Summary, Desc))
	:-
	clean_PA_entry(L1, PA),
	clean_white(L2, Form),
	getSummary(Ls, Summary, RestLs),
	crossWhiteLines(RestLs, StartDescLines),
	getDesc(StartDescLines, Desc).

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
	

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
	
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

getDesc(Ls, Desc)
	:-
	getDesc0(Ls, InitDesc),
	adj_desc_lines(InitDesc, Desc).

adj_desc_lines([], []).
adj_desc_lines([Line | Desc], [AdjLine | AdjDesc])
	:-
	strip_front(Line, CodesF),
	reverse(CodesF, RCodesF),
	two_blank_end(RCodesF, 0, B2EndRCodesF),
	reverse(B2EndRCodesF, B2ECodesF),
	atom_codes(AdjLine, B2ECodesF),
	adj_desc_lines(Desc, AdjDesc).
	
two_blank_end(RCodes, 2, RCodes)
	:-!.

	% blank space:
two_blank_end([0'  | RCodesIn], N, [0' | RCodesOut])
	:-!,
	M is N+1,
	two_blank_end(RCodesIn, M, RCodesOut).

	% non-blank space; because of clause 1, N<2.
two_blank_end([Cd | RCodesIn], N, [0' | RCodesOut])
	:-!,
	M is N+1,
	two_blank_end([Cd | RCodesIn], M, RCodesOut).

getDesc0([L | _], [])
	:-
	sub_atom(L, 0, 4, _, ' *!-'),
	!.
getDesc0([L | Ls], RR)
	:-
	clean_white(L, R),
	(R == '' -> RR = Desc ; RR = [R | Desc]),
	getDesc0(Ls, Desc).

	% Trailing codes CodesSLX start with a dash ('-'):
xtr_summary([0'- | TailCodesSLX], Summary)
	:-!,
		% Strip any more leading whitespace following the '-':
	s_f(TailCodesSLX, FinalCodesSLX),
	atom_codes(Summary, FinalCodesSLX).

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/

/*---------------------------------------------------------------------
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
 *--------------------------------------------------------------------*/

write_alb(LibFile, Module, alslib, PAsList, FolderPath)
	:-
	file_extension(AlbFullName, LibFile, alb),
	join_path([FolderPath, AlbFullName], Alb_path),
		%% *.alb has no hand-entered info that is different
		%% from what is generated here, so it is ok
		%% to overwrite it:
	AlbTerm = libactivate(Module, [library, LibFile], PAsList, []),

	open(Alb_path, write, OS),
	printf(OS, '%t.\n', [AlbTerm]),
	close(OS),
	open(Alb_path, read, IS),
		%% we're looking to see if there are syntax errors:
	read_term(IS, ABT, []),
	close(IS).

pullPAs([], []).
pullPAs([d(PA, Form, Summary, Desc) | DocList], [PA | PAsList])
	:-
	pullPAs(DocList, PAsList).

makePAsDs([], []).
makePAsDs([ d(PA, Form, Summary, Desc) | DocList],  [p(PA, Summary) | PAsDescs])
	:-
	makePAsDs(DocList, PAsDescs).

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/

write_md(LibFile, Module, DocList, DocTitle, Group, Md_path, Overwrite)
	:-
	mdFolderPath(MDFolderPath),
	file_extension(MdFullName, LibFile, md),
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
			printf(OS, 'Displaying generated MD file on console:\n\n', [])
			;
			true
		)
	),
	sort(DocList, SortedDocList),
	assemblePreds(SortedDocList, Preds, Forms, Descs),
		%%%%---- From doctools.pro:
	yaml(DocTitle, alslib, Group, Module, Preds, OS),
	printf(OS, '## FORMS\n\n', []),
	writeFoms(Forms, OS),
	printf(OS, '## DESCRIPTION\n', []),
	writeDescriptions(Descs, OS),
	printf(OS, '\n## EXAMPLES\n\n', []),
	close(OS).

assemblePreds([], [], [], []).
assemblePreds([d(PA, Form, Summary, Desc) | DocList], [p(PA,DescAtom) | Preds], 
		[Form | Forms], [FullDesc | Descs])
	:-
	(atom(Summary) -> DescAtom = Summary ;
		insert_spaces(Summary, SpacedSummary),
		catenate(SpacedSummary, DescAtom)
	),
	mdDesc(PA, Desc, FullDesc),
	assemblePreds(DocList, Preds, Forms, Descs).

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

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/

idx_lib_page(Group, Title, FileName, PAsWithDescs)
	:-
	src_helpPath(SrcHelpPath),
		%% PkgFile is Full path to docsdb_alslib.pro
	join_path([SrcHelpPath, 'docsdb_alslib.pro'], PkgFile),
		%%%%---- From doctools.pro:
	do_rew_idx(PkgFile, alslib, Group, Title, FileName, PAsWithDescs).

