/* ====================================================================== *
 |                      libwork.pro
 |              Copyright (c) 2018 Applied Logic Systems, Inc.
 |      
 |              -- Install and document library (alslib) files
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
 * ====================================================================== */
:-['doctools.pro'].

	%% Assumes running in ~docs/src_help_md:
mdFolderPath('./md_help').
src_helpPath('./').
lib_dev_path('../../core/alsp_src/library').

tlu1 :- doc_lib_file(listutl1).
tlu4 :- doc_lib_file(listutl4).

libsetup :-
	command_line(LibFilesList),
	do_libsetup(LibFilesList).

do_libsetup([]).
do_libsetup([LibFile | LibFilesList])
	:-
	printf('Processing Library file %t\n', [LibFile]),
	doc_lib_file(LibFile),
	printf('Library file %t processed\n\n', [LibFile]),
	do_libsetup(LibFilesList).

doc_lib_file(LibFile)
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
%	dumpDocs(DocList),

	write_alb(LibFile, Module, alslib, DocList, LibraryFolderPath),
	write_md(LibFile, Module, DocList, DocTitle, Group, Md_path),
	file_extension(LibHtml, LibFile, html),
	join_path(['./alshelp', LibHtml], HtmlPath),
	catenate(['pandoc -s -o ', HtmlPath , ' ', Md_path], PandocCmd),
	system(PandocCmd),
	write(PandocCmd),nl,

	makePAsDs(DocList, PAsWithDescs),
	idx_lib_page(Group, DocTitle, LibFile, PAsWithDescs).

	
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


/*---------------------------------------------------------------------
match_up([], _, []).
match_up([C | CommentsList], [PB | PostBef], [m(C,PB) | MM])
	:-
	match_up(CommentsList, PostBef, MM).

readup([], []).
readup([m(lcmt(C),b(PB)) | MM], [m(lcmt(C),c(QQ)) | UU])
	:-
	insert_spaces(PB, Out_List),
	catenate(Out_List, EE),
	open(atom(EE), read, IS),
	read_terms(IS,QQ),
	close(IS),
	readup(MM, UU).

dump([]).
dump([m(lcmt(C),c(QQ)) | MM])
	:-
	write_lines(C),
	output_prolog_list(QQ),nl,
	nl,nl,
	dump(MM).
 *--------------------------------------------------------------------*/

extr_doc([], []).
extr_doc([lcmt(C) | CommentsList], [D | DocList])
	:-
	C = [TopLine | RestLs],
	xtr_doc(RestLs, D),
	extr_doc(CommentsList, DocList).

xtr_doc([L1, L2 | Ls], d(PA, Form, Summary, Desc))
	:-
	clean_white(L1, PA),
	clean_white(L2, Form),
	getSummary(Ls, Summary, RestLs),
	crossWhiteLines(RestLs, StartDescLines),
	getDesc(StartDescLines, Desc).

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
dumpDocs([]).
dumpDocs([Doc | DocList])
	:-
	dD(Doc),
	dumpDocs(DocList).

dD(d(PA, Form, Summary, Desc))
	:-
	write(PA),nl,
	write(Form),nl,
	write(Summary),nl,
	output_prolog_list(Desc),nl,nl.

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

write_alb(LibFile, Module, alslib, DocList, FolderPath)
	:-
	file_extension(AlbFullName, LibFile, alb),
	join_path([FolderPath, AlbFullName], Alb_path),
		%% *.alb has no hand-entered info that is different
		%% from what is generated here, so it is ok
		%% to overwrite it:
	open(Alb_path, write, OS),
%	OS = user_output,

	AlbTerm = 
	    libactivate(Module, [alslib, LibFile], PAsList, []),
	pullPAs(DocList, PAsList),
    % display on the console:
printf(user_output, '%t:\n', [AlbFullName]),
printf(user_output, '%t.\n', [AlbTerm]),
	printf(OS, '%t.\n', [AlbTerm]).

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

write_md(LibFile, Module, DocList, DocTitle, Group, Md_path)
	:-
	mdFolderPath(MDFolderPath),
	file_extension(MdFullName, LibFile, md),
	join_path([MDFolderPath, MdFullName], Md_path),
		%% We can be entering EXAMPLES into <libfile>.md,
		%% so for now, refuse to overwrite it;
		%% ISSUE: Arrange for an additional commandline
		%% switch which will allow an overwrite.
	(exists_file(Md_path) ->
		OS = user_output,
		printf(OS, 'File: %t already exists;\nRefusing to overwrite.\n',[Md_path]),
		printf(OS, 'Displaying generated MD file on console:\n\n', [])
		;
		open(Md_path, write, OS)
	),
	assemblePreds(DocList, Preds, Forms, Descs),
	yaml(DocTitle, alslib, Group, Preds, OS),
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

%%%%---- From doctools.pro:
yaml(Title, Package, Group, Preds, OS)
        :-
        printf(OS, '---\n', []),
        printf(OS, 'title: \'%t\'\n', [Title]),
        (Package == '' -> true;
                printf(OS, 'package: %t\n', [Package])
        ),
        (Group == '' -> true;
                printf(OS, 'group: %t\n', [Group])
        ),
        printf(OS, 'predicates:\n', []),
        yaml_preds(Preds, OS),
        printf(OS, '---\n', []).

yaml_preds([], _).
yaml_preds([p(PA,DescAtom) | Preds], OS)
        :-
        printf(OS, '- {sig: \'%t\', desc: \'%t\'}\n', [PA,DescAtom]),
        yaml_preds(Preds, OS).
%%%%---- 

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
	do_rew_idx(PkgFile, alslib, Group, Title, FileName, PAsWithDescs).

