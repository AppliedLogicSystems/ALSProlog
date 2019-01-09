/* ==================================================================== *
 |				doctools.pro
 |              Copyright (c) 2018-2019 Applied Logic Systems, Inc.
 |
 |		-- 
 |
 |	Primary predicates:
 |	    toc_from_docsdb - write out tables of contents
 |	    new_page(Path)  - creates skeletal new *.md page
 |	    idx_page(Path)  - indexes (toc) a new *.md page
 |
 * ==================================================================== */

src_folder_md_files('./md_help').

packages([core_prolog, alsdev, library, c_intf]).

	% Readable texts for the packages:
pack_readable(core_prolog, 'Core Prolog').
pack_readable(alsdev, 'ALSDev').
pack_readable(alslib, 'ALS Library').
pack_readable(c_intf, 'C Interface').

	% Sub-Groups of the packages:
pack_kid(control, core_prolog).
pack_kid(prolog_database, core_prolog).
pack_kid(terms, core_prolog).
pack_kid(input_output, core_prolog).
pack_kid(file_system, core_prolog).

pack_kid(development_environment, alsdev).
pack_kid(gui_library, alsdev).
pack_kid(prolog_objects, alsdev).
pack_kid(tcltk_interface, alsdev).

	%% will probably grow:
pack_kid(c_intf, c_intf).

%future:
%pack_kid(..., alslib).

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
group_display(G, G).

	/* ------------------------------------------------------ *
	 |	    toc_from_docsdb
	 |
	 |	Writes the tables of contents in alshelp/:
	 |	    toc_core_prolog.html
	 |	    toc_alsdev.html
	 |	    toc_alslib
	 |	    toc_c_intf
	 |	These are driven by prolog "databases":
	 |	    docsdb_core_prolog.pro
	 |	    docsdb_alsdev.pro
	 |	    docsdb_alslib.pro
	 |	    docsdb_c_intf.pro
	 |	These live in ~ALSProlog/docs/src_help_md, which
	 |	is the same folder containing doctools.pro.
	 * ------------------------------------------------------ */

	% Process a single package (convenience):
td:- toc_from_docsdb.
tdcore :- tocs_from_docsdbs([core_prolog]).
tdalsdev :- tocs_from_docsdbs([alsdev]).
tdc_int :- tocs_from_docsdbs([c_intf]).

	% Process all the packages:
toc_from_docsdb
        :-
        packages(Packages),
        tocs_from_docsdbs(Packages).

tocs_from_docsdbs([]).
tocs_from_docsdbs([Package | Packages])
        :-
        catenate(['./docsdb_', Package, '.pro'], DBFile),
        (not(exists_file(DBFile)) -> true
                ;
                open(DBFile, read, S),
                read(S, PackageData),
                close(S),
                write_toc(Package, PackageData)
        ),
        tocs_from_docsdbs(Packages).

/* --------------------------------------------------------------- *
        write_toc(PackageName, PackageData)

        Write out the table of contents (toc) for PackageName,
	using PackageData obtained from docsdb_<package>.pro
 * --------------------------------------------------------------- */

	% Paths to locations of alshelp/ when running in src_help_md:
toc_locn('./alshelp'). 		% running in src_help_md
%md_toc_locn('../ref-manual').	% running in src_help_md

	%% depending on the location of toc_<Pack>.pro:
toc2mdSrcDir('').    		%% for src_help_md/alshelp
%md_toc2mdSrcDir('../src_help_md/md_help/').    %% for ref-manual/alshelp

		%% packages([core_prolog, alsdev, library, c_intf]).
write_toc(PackageName, []) :-!.

/*
write_toc(PackageName, PackageData)
        :-
	PackageName == core_prolog,
	!,
	write_toc_core_prolog(PackageData).
*/

write_toc(PackageName, PackageData)
        :-
		%% write the toc file as file src_help_md/alshelp/toc_<PackAbbrev>.html:
        catenate(['toc_', PackageName, '.html'], TocFile),
        toc_locn(TocFolder),
        join_path([TocFolder, TocFile], TgtTocFile),
	toc2mdSrcDir(Toc2SrcMdDir),		 %% for src_help_md/alshelp
	write_out_toc(PackageData, TgtTocFile, Toc2SrcMdDir).

/*
		%% write the toc file as file ~docs/ref-manual/md_toc_<PackAbbrev>.html:
        catenate(['md_toc_', PackageName, '.html'], MDTocFile),
        md_toc_locn(MDTocFolder),
        join_path([MDTocFolder, MDTocFile], TgtMDTocFile),
	md_toc2mdSrcDir(MDToc2SrcDir), 
	write_out_toc(PackageData, TgtMDTocFile, MDToc2SrcDir).
*/

/*
write_toc_core_prolog(PackageData)
	:-
	PackageName = core_prolog,

        toc_locn(TocFolder),
	toc2mdSrcDir(Toc2SrcMdDir),		 %% for src_help_md/alshelp
        catenate(['toc_', PackageName, '.html'], TocFile),
        join_path([TocFolder, TocFile], TgtTocFile),
	write_out_toc(PackageData, TgtTocFile, Toc2SrcMdDir).
*/

write_out_toc(PackageData, TgtTocFile, ToSrcDir)
	:-
%OS = user_output,
        open(TgtTocFile, write, OS),
        write_toc_header(OS),
        sort(PackageData, SortedPackageData),
	last_toc_phase( SortedPackageData, ToSrcDir, OS),
	close(OS).

last_toc_phase( SortedPackageData, ToSrcDir, OS)
	:-
        breakout_groups(SortedPackageData, BrokenOutGroups, FinalBrokenOutGroups),
        write_pdata(BrokenOutGroups, ToSrcDir, OS),
        printf(OS, '</BODY>\n</HTML>\n', []).
        close(OS).

write_toc_header(S)
        :-
        printf(S,'<HTML>\n',[]),
        printf(S,'<HEAD>\n',[]),
        printf(S,'<STYLE>\n',[]),
        printf(S,'.idxTitle{\n',[]),
        printf(S,'font-size:1.2em;font-weight:bold;margin-bottom:0.5em;\n',[]),
        printf(S,'}\n',[]),
        printf(S,'.eentry {\n',[]),
        printf(S,'margin-top:-16px;\n',[]),
        printf(S,'margin-bottom:0px;\n',[]),
        printf(S,'margin-left:0.6em;\n',[]),
        printf(S,'}\n',[]),
        printf(S,'.hentry {\n',[]),
        printf(S,'font-size:1.1em;font-weight:bold;margin-left:1em;margin-top:0em;\n',[]),
        printf(S,'}\n',[]),
        printf(S,'.astyl { text-decoration: none; }\n',[]),
        printf(S,'</STYLE>\n',[]),
        printf(S,'</HEAD>\n',[]),
        printf(S,'<BODY>\n',[]).

breakout_groups([], FinalBrokenOutGroups, FinalBrokenOutGroups)
        :-
        FinalBrokenOutGroups = [].
breakout_groups(SortedPackageData, [GX | BrokenOutGroups], FinalBrokenOutGroups)
        :-
        SortedPackageData = [xpge(G, PA, FName, PlainPred, Preds) | _],
        brkout_grp(SortedPackageData, G, GX, TailSortedPackageData),
        breakout_groups(TailSortedPackageData, BrokenOutGroups, FinalBrokenOutGroups).

brkout_grp(SortedPackageData, G, G-XGrp, TailSortedPackageData)
        :-
        accum_grp(SortedPackageData, G, GroupEntries, TailSortedPackageData),
        expand_grp(GroupEntries, Expanded),
        sort(Expanded, SortedExpanded),
        XGrp = SortedExpanded.

accum_grp([], Group, [], []).

accum_grp([xpge(Group, PA, FName, PlainPred, Preds) | InterSortedPackageData],
          Group,
          [xpge(Group, PA, FName, PlainPred, Preds) | TailGroupEntries],  TailSortedPackageData)
        :-!,
        accum_grp(InterSortedPackageData, Group, TailGroupEntries, TailSortedPackageData).

accum_grp([xpge(NextGroup, PA, FName, PlainPred, Preds) | InterSortedPackageData],
           Group, [],
           [xpge(NextGroup, PA, FName, PlainPred, Preds) | InterSortedPackageData]).

expand_grp([], []).

expand_grp([xpge(Group, PA, FName, PlainPred, PredsWithDescs) | GroupEntries], Expanded)
        :-
        make_pa_descs(PredsWithDescs, FName, PADescs),
        append(PADescs, NextExpanded, Expanded),
        expand_grp(GroupEntries, NextExpanded).

make_pa_descs([], _, []).

make_pa_descs([p(PA, DescList) | PredsWithDescs], FName, [PA-DescAtom-FName | PADescs])
        :-
        (atom(DescList) ->
                DescAtom = DescList
                ;
                desc_atom(DescList, DescAtom)
        ),
        make_pa_descs(PredsWithDescs, FName, PADescs).

write_pdata([], _, OS).

write_pdata([Group-GData | BrokenOutGroups], Toc2SrcMdDir, OS)
        :-
        group_display(Group, GroupDisp),
        printf(OS, '<div class="hentry">%t</div><br>\n', [GroupDisp]),
	collapse_PAs(GData, CollapseGData),
        write_group(CollapseGData, Toc2SrcMdDir, OS),
        write_pdata(BrokenOutGroups, Toc2SrcMdDir, OS).

	%% in ref-manual/md_toc____: '../src_help_md/md_help/'
write_group([], _, OS).
write_group([PA-IX-FName| GData], Toc2SrcMdDir, OS)
        :-
        printf(OS,
            '<div class="eentry"><a class="astyl" target="content" href="%t%t.html" ix="%t -- %t">%t</a></div><br>\n',
            [Toc2SrcMdDir,FName,PA,IX,PA]),
        write_group(GData, Toc2SrcMdDir, OS).

collapse_PAs([], []).
			%% PA-DescAtom-FName
collapse_PAs([PA-DA-FN | GData], [PCCPA-IX-FN | CollapseGData])
	:-
	getPandA(PA, P, A),
	CPA = [A | TailCPA],
	collapse_from(GData, P, [DA], TailCPA, IX, TailGData),
	getPAL(CPA, CCPA),
	bufwrite(X, P/CCPA),
	atom_codes(PCCPA, X),
	collapse_PAs(TailGData, CollapseGData).

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

getPAL([A], A) :-!.
getPAL(L, L).

collapse_from([], P, DA_List, [], IX, [])
	:-
	reverse(DA_List, RDAL),
	catenate(RDAL, IX).
collapse_from([PA2-DA2-FN2 | TGData2], P, DA_List, TailCPA, IX, TailGData)
	:-
		%% Check whether the predicate (sans arity) in PA is == P:
	getPandA(PA2, P, A2),
	!,
	TailCPA = [A2 | TailCPA2],
	collapse_from(TGData2, P, [DA2, '; ' | DA_List], TailCPA2, IX, TailGData).

collapse_from(GData, P, DA_List, TailCPA, IX, TailGData)
	:-
	reverse(DA_List, RDAL),
	catenate(RDAL, IX),
	TailCPA=[],
	TailGData = GData.

	/* -------------------------------------------------------------------- *
	 |		new_page(Path)
	 |
	 |		Creates the framework for a new alshelp page,
	 |		by making a skeletal *.md page in folder md_help/
	 |
	 |	Path is a path to a file containing the following term:
	 |
	 |	np(Package, Group, Title, FileName, Module,
	 |	    [<List of <PAs with Descrips>>] ).
	 |
	 |	where:
	 |	-  packages(PList), member(Package, PList)
	 |	-  pack_kid(Group, Package)
	 |	-  Title is of the form: P/A
	 |	-  md_help/<FileName>.md is the intended skeletal target
	 |	-  Module is where the predicats have been declared
	 |	-  Each <PAs with Descrips> is of the form
	 |		p(<quoted atom Q/R>, <atom which is a short description of Q/R>)
	 |
	 |	new_page/1 can be invoked from the command line using:
	 |
	 |		alspro doctools.pro -g do_np -p <path to xx.np>
	 |
	 |	The convenience predicate mknnp/0 will create the outline of
	 |	such an np(...) in the file ./nnp.np.
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
  
%% Example:
tt :- 
	Path = 'np_for_curl.np',
	new_page(Path).

	%% alspro doctools.pro -g do_np -p <path to xx.np>
do_np :-
	command_line(CommandLine),
	catch(
		np_list(CommandLine),
		npissue(IssueInfo, NPPath),
		exit_np(NPPath, IssueInfo)
	).

exit_np(NPPath, IssueInfo)
	:-
	printf(user_output, '=======================\n',[]),
%	printf(user_output, 'Issue while trying to create  *.md file from: %t:\n\t%t\n', [NPPath, IssueInfo]),
	printf(user_output, 'Issue while trying to create  *.md file from: %t:\n\n', [NPPath]),

	printf(user_output, '+++++++++++++++++++++++\n',[]),
	printf(user_output, 'Exiting from doctools.pro/newpage due to problems with file %t\n',[NPPath]),
	halt.
	
np_list([])
	:- halt.
np_list([NPPath | NPFiles])
	:-
	new_page(NPPath),
	np_list(NPFiles).

new_page(Path)
	:-
	(exists_file(Path) ->
		do_new_page(Path)
		;
		printf('Path %t does not exist - exiting.\n', [Path]),
		abort
	).

do_new_page(Path)
	:-
	open(Path, read, IS),
	read(IS, NewPageTerm),
	close(IS),
	check_and_go(NewPageTerm),
	do_new_idx_tm(NewPageTerm).

check_and_go(np(Package, Group, Title, FileName, Module, PAsWithDescs))
	:-
	check_package(Package),
	check_group(Group, Package),
	check_file(FileName),
	check_PAsWithDs(PAsWithDescs),
	make_new_page(Package, Group, Title, FileName, Module, PAsWithDescs).

check_package(Package)
	:-
	packages(PList), 
	member(Package, PList),
	!.
check_package(Package)
	:-
	printf('Unknown package: %t\n', [Package]),
	abort.

check_group(Group, Package)
	:-
	pack_kid(Group, Package).

check_group(Group, Package)
	:-
	printf('Bad Group: %t -- for package: %t\n', [Group,Package]),
	abort.

check_file(FileName)
	:-
	file_extension(MDFile, FileName, md),
	src_folder_md_files(MD_Src_Folder),
        join_path([MD_Src_Folder, MDFile], TgtMDFile),
	(exists_file(TgtMDFile) ->
		printf('File already exists: %t\n', [TgtMDFile]),
		abort
		;
		true
	).
		
check_PAsWithDs([]).
check_PAsWithDs([PAD | PAsWithDescs])
	:-
	check_pad(PAD),
	!,
	check_PAsWithDs(PAsWithDescs).

check_pad(p(QR, Desc))
	:-
	atom(QR),
	open(atom(QR), read, IS),
	read_term(IS, QRTerm, [attach_fullstop(true)]),
	close(IS),
	!,
	QRTerm = Q/R,
	atom(Q), 
	(integer(R); integer_list(R) ),
	atom(Desc).

integer_list([]).
integer_list([N | Tail])
	:-
	integer(N),
	integer_list(Tail).

make_new_page(Package, Group, Title, FileName, Module, PAsWithDescs)
	:-
	file_extension(MDFile, FileName, md),
	src_folder_md_files(MD_Src_Folder),
        join_path([MD_Src_Folder, MDFile], TgtMDFile),
	open(TgtMDFile, write, OS),
	sort(PAsWithDescs, SortedPAsWithDescs),
	(write_out_new_page(Package, Group, Title, Module, SortedPAsWithDescs, OS) ->
		close(OS) ; close(OS) ).

write_out_new_page(Package, Group, Title, Module, PAsWithDescs, OS)
	:-
	yaml(Title, Package, Group, Module, PAsWithDescs, OS),
	length(PAsWithDescs, MinNumForms),
        printf(OS, '\n## FORMS\n\n', []),
	do_forms_skels(PAsWithDescs, OS),
        printf(OS, '\n## DESCRIPTION\n\n', []),
	do_descs_skels(PAsWithDescs, OS),
	finish_skeleton(OS).

yaml(Title, Package, Group, Module, Preds, OS)
        :-
        printf(OS, '---\n', []),
        printf(OS, 'title: \'%t\'\n', [Title]),
	(Package == '' -> true;
		printf(OS, 'package: %t\n', [Package])
	),
	(Group == '' -> true;
		printf(OS, 'group: %t\n', [Group])
	),
        printf(OS, 'module: %t\n', [Module]),
        printf(OS, 'predicates:\n', []),
        yaml_preds(Preds, OS),
        printf(OS, '---\n', []).

yaml_preds([], _).
yaml_preds([p(PA,DescAtom) | Preds], OS)
        :-
        printf(OS, '- {sig: \'%t\', desc: \'%t\'}\n', [PA,DescAtom]),
        yaml_preds(Preds, OS).

short_descs([], OS).
short_descs([p(PA, DescAtom) | StrippedPreds], OS)
        :-
        printf(OS, '`%t` `--` %t\n\n', [PA,DescAtom]),
        short_descs(StrippedPreds, OS).

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

do_descs_skels([], OS).
do_descs_skels( [p(PA,DescAtom) | Preds], OS)
	:-
	printf(OS, '\n**`%t`**  \n\n', [PA]),
	do_descs_skels(Preds, OS).

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

blnk(OS) :- printf(OS, '\n',[]).

	/* -------------------------------------------------------------------- *
	 * -------------------------------------------------------------------- */

do_docsdb(PkgFile, Group, Title, FileName, PAsWithDescs, SortedNewList)
	:-	
        open(PkgFile, read, IS),
	read_term(IS, XPGEList, [attach_fullstop(true)]),
	close(IS),

    	XPGETerm = xpge(Group,Title,FileName,PlainPred,PAsWithDescs),
	check_for_xpge_dup( XPGETerm, PAsWithDescs, XPGEList, PkgFile, Title, SortedNewList).

check_for_xpge_dup( XPGETerm, PAsWithDescs, XPGEList, PkgFile, Title, SortedNewList)
	:-
	XPGETerm = xpge(Group, Title, FileName, PlainPred, NewPAsWithDescs),
		%% Already have an entry in PkgFile for this xpge:
	member(xpge(Group, Title, FileName, PlainPred, PrevEntryPADs), XPGEList),
	!,
		%% Check if there's a change in the PAs with Descs:
	check_merge_PAs_Ds(NewPAsWithDescs, PrevEntryPADs, InitMergedPAsDs),
	sort(InitMergedPAsDs, SortedMergedPAsDs),
	replace_xpge( XPGEList, 
			xpge(Group, Title, FileName, PlainPred, SortedMergedPAsDs), 
			SortedNewList),

        open(PkgFile, write, OS2),
        write_term(OS2, SortedNewList, [ quoted(true) ]),
        write(OS2, '.'),nl(OS2),
        flush_output(OS2),
	close(OS2).

		%% No previous entry in PkgFile for this xpge:
check_for_xpge_dup( XPGETerm, _, XPGEList, PkgFile, Title, SortedNewList)
	:-
    	XPGETerm = xpge(Group,Title,FileName,PlainPred,PAsWithDescs),
	xtr_plain_title(Title, PlainPred),
	NewList = [XPGETerm | XPGEList],
	sort(NewList, SortedNewList),

        open(PkgFile, write, OS2),
        write_term(OS2, SortedNewList, [ quoted(true) ]),
        write(OS2, '.'),nl(OS2),
        flush_output(OS2),
        close(OS2).

	% Run thru the new PA/Descs checking each; 
	%   if ThisPA is not present, add it;
	%   if ThisPA is present, update OldDesc by NewDesc

	% Every new PAD has been dealt with:
check_merge_PAs_Ds([], _, []). 

check_merge_PAs_Ds([p(ThisPA,NewDesc) | RestNewPAsWithDescs], PrevEntryPADs, 
			[p(ThisPA, NewDesc) | RestMergedPAsDs])
	:-
		% Is there a p(ThisPA, __) old entry?:
	check_PA_status(PrevEntryPADs, p(ThisPA,NewDesc), UpdatedEntryPADs),
	!,
	check_merge_PAs_Ds(RestNewPAsWithDescs, UpdatedEntryPADs, RestMergedPAsDs).

	% p(ThisPA,_) is not present in PrevEntryPADs
check_merge_PAs_Ds([p(ThisPA,NewDesc) | RestNewPAsWithDescs], PrevEntryPADs, 
			[p(ThisPA,NewDesc) | RestMergedPAsDs])
	:-
	check_merge_PAs_Ds(RestNewPAsWithDescs, PrevEntryPADs, RestMergedPAsDs).

	
	% Is there a p(ThisPA, __) old entry? -- No, if PrevEntryPADs = [];
	% So no clause for PrevEntryPADs = []
	% (Want to fail into the 2nd clause for check_for_xpge_dup)

	% Is there a p(ThisPA, __) old entry?:
	% Yes, this is it.  Note that either NewDesc\=OldDesc, so we want to
	% replace OldDesc by NewDesc, or NewDesc==OldDes, and it doesn't matter
	% if we replace OldDesc by NewDesc, the result will stay the same.
check_PA_status([p(ThisPA, OldDesc) | RestPrevEntryPADs], 
			p(ThisPA,NewDesc), [p(ThisPA,NewDesc) | RestPrevEntryPADs])
	:-!.
	
	% Is there a p(ThisPA, __) old entry?:
	% Don't know, but this isn't it; skip over:
check_PA_status([p(ThisPA, OldDesc) | RestPrevEntryPADs], 
			p(ThisPA,NewDesc), [p(ThisPA, OldDesc) | RestPrevEntryPADs])
	:-!.
	check_PA_status(RestPrevEntryPADs, p(ThisPA,NewDesc), RestPrevEntryPADs).

	% XPGEList is sorted going in; this doesn't change that:
replace_xpge([], _, []).
replace_xpge([xpge(Group, Title, FileName, PlainPred, _) | TailXPGEList], 
		xpge(Group, Title, FileName, PlainPred, SortedMergedPAsDs), 
	     [xpge(Group, Title, FileName, PlainPred, SortedMergedPAsDs) | 
		TailXPGEList])
	:-!.

replace_xpge([XPGETerm | TailXPGEList], 
		xpge(Group, Title, FileName, PlainPred, SortedMergedPAsDs), 
	     [XPGETerm | TailSortedNewList])
	:-
	replace_xpge(TailXPGEList, 
		xpge(Group, Title, FileName, PlainPred, SortedMergedPAsDs), 
	     	TailSortedNewList).


xtr_plain_title(PlainPred/_, PlainPred)
	:-!.

xtr_plain_title(Title, PlainPred)
	:-
	atom(Title),
	open(atom(Title), read, IS),
	read_term(IS, PA, [attach_fullstop(true)]),
	close(IS),
	PA = PlainPred/_.

	/* -------------------------------------------------------------------- *
	 |		idx_page(Path)
	 |
	 |	Path is as described above for new_page(Path).
	 |	ASSUMES that the np(...) at Path has been processed by 
	 |	new_page(Path) to produce a page in ~/md_help.
	 |	(The skeletal page may or may not have been fleshed out, and
	 |	 the corresponding *.html may or may not have been produced
	 |	 using pandoc or Jekyll.)
	 |
	 |	idx_page(Path) reads the np(...) term out of the file at Path,
	 |	and does three things:
	 |	A.  It reads the list from docsdb_<Package>.pro, adds
	 |	    an appropriate entry for the np term at the beginning of the
         |          list, and sorts the list;
	 |	B.  Rewrites docsdb_<Package>.pro using the sorted list from step A.
	 |	C.  Uses the sorted list to rewrite a new file 
	 |		toc_<Package>.html 
	 |	    in folder alshelp/.
	 * -------------------------------------------------------------------- */
ii :- 
	Path = 'np_for_curl.np',
	idx_page(Path).

ij :-
	Path = 'calloc23.np',
	idx_page(Path).

idx_page(Path)
	:-
	(exists_file(Path) ->
		do_new_idx(Path)
		;
		printf('Path %t does not exist - exiting.\n', [Path]),
		abort
	).

do_new_idx(Path)
	:-
	open(Path, read, IS),
	read(IS, NewPageTerm),
	close(IS),
	do_new_idx_tm(NewPageTerm).

do_new_idx_tm(NewPageTerm)
	:-
trace,
	NewPageTerm = np(Package, Group, Title, FileName, Module, PAsWithDescs),
	    % All necessary checks made when new_page(Path) ran:
	rewrite_idx(Package, Group, Title, FileName, Module, PAsWithDescs).

rewrite_idx(Package, Group, Title, FileName, Module, PAsWithDescs)
	:-
		%% pack_readable(c_intf, 'C Interface').
	(pack_readable(PackAbbrev, Package) -> true;
		pack_readable(Package, _), PackAbbrev = Package ),
	catenate(['./docsdb_', PackAbbrev, '.pro'], PkgFile),
	(exists_file(PkgFile) -> true ; 
		open(PkgFile, write, OOS), printf(OOS, '[].\n', []), close(OOS)
	),
%	do_rew_idx(Pkgfile, PackAbbrev, Group, Title, FileName, Module, PAsWithDescs, TocFolder).
	do_rew_idx(PkgFile, PackAbbrev, Group, Title, FileName, Module, PAsWithDescs).
	
	/* -------------------------------------------------------------------- *
	 * -------------------------------------------------------------------- */

do_rew_idx(PkgFile, PackAbbrev, Group, Title, FileName, Module, PAsWithDescs)
	:-
		%% Need to include Module in the docsdb; but all entries need to be converted:
%	do_docsdb(PkgFile, Group, Title, FileName, Module, PAsWithDescs, SortedNewList),
	do_docsdb(PkgFile, Group, Title, FileName, PAsWithDescs, SortedNewList),

		%% write the toc file as file src_help_md/alshelp/toc_<PackAbbrev>.html:
        toc_locn(TocFolder),
	toc2mdSrcDir(Toc2SrcMdDir),
	write_toc_file(TocFolder, PackAbbrev, '', SortedNewList, Toc2SrcMdDir).

write_toc_file(TocFolder, PackAbbrev, TocFilePrefix, SortedNewList, Toc2SrcMdDir)
	:-
        catenate([TocFilePrefix, 'toc_', PackAbbrev, '.html'], TocFile),
        join_path([TocFolder, TocFile], TgtTocFile),
        open(TgtTocFile, write, OS),
        write_toc_header(OS),
	last_toc_phase( SortedNewList, Toc2SrcMdDir, OS),
	close(OS).

	/* -------------------------------------------------------------------- *
	 * -------------------------------------------------------------------- */


