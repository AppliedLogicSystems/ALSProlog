/* =================================================================== *
 |				doctools.pro
 |
 |	Primary predicates:
 |	    toc_from_docsdb - write out tables of contents
 |	    new_page(Path)  - creates skeletal new *.md page
 |	    idx_page(Path)  - indexes (toc) a new *.md page
 |
 * =================================================================== */

src_folder_md_files('./md_help').

packages([core_prolog, alsdev, library, fi]).

pack_kid(control, core_prolog).
pack_kid(prolog_database, core_prolog).
pack_kid(terms, core_prolog).
pack_kid(input_output, core_prolog).

pack_kid(development_environment, alsdev).
pack_kid(gui_library, alsdev).
pack_kid(prolog_objects, alsdev).
pack_kid(tcltk_interface, alsdev).

%future:
%pack_kid(..., library).
%pack_kid(..., fi).

	/* ------------------------------------------------------ *
	 |	    toc_from_docsdb
	 |
	 |	Writes the tables of contents in alshelp/:
	 |	    toc_core_prolog.html
	 |	    toc_alsdev.html
	 |		[Future: for library and foreign interface]
	 |	These are driven by prolog "databases":
	 |	    docsdb_core_prolog.pro
	 |	    docsdb_alsdev.pro
	 * ------------------------------------------------------ */

toc_locn('./alshelp').

td:- toc_from_docsdb.
tdalsdev :- tocs_from_docsdbs([alsdev]).

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

        Write out the table of contents (toc) for PackageName.
 * --------------------------------------------------------------- */
write_toc(PackageName, []) :-!.

write_toc(PackageName, PackageData)
        :-
        toc_locn(TocFolder),
        catenate(['toc_', PackageName, '.html'], TocFile),
        join_path([TocFolder, TocFile], TgtTocFile),
        open(TgtTocFile, write, OS),
%OS = user_output,
        write_toc_header(OS),
        sort(PackageData, SortedPackageData),
	last_toc_phase( SortedPackageData, OS).

last_toc_phase( SortedPackageData, OS)
	:-
        breakout_groups(SortedPackageData, BrokenOutGroups, FinalBrokenOutGroups),
        write_pdata(BrokenOutGroups, OS),
        printf(OS, '</BODY>\n</HTML>\n', []).
        close(OS).

group_display(control, 'Control').
group_display(input_output, 'Input Output').
group_display(prolog_database, 'Prolog Database').
group_display(prolog_objects, 'Prolog Objects').
group_display(terms, 'Terms').
group_display(development_environment, 'Development Env.').
group_display(gui_library, 'Gui Library').
group_display(tcltk_interface, 'TclTk Interface').

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
        SortedPackageData = [xpge(G, PA, FName, First, Preds) | _],
        brkout_grp(SortedPackageData, G, GX, TailSortedPackageData),
        breakout_groups(TailSortedPackageData, BrokenOutGroups, FinalBrokenOutGroups).

brkout_grp(SortedPackageData, G, G-XGrp, TailSortedPackageData)
        :-
        accum_grp(SortedPackageData, G, GroupEntries, TailSortedPackageData),
        expand_grp(GroupEntries, Expanded),
        sort(Expanded, SortedExpanded),
        XGrp = SortedExpanded.

accum_grp([], Group, [], []).

accum_grp([xpge(Group, PA, FName, First, Preds) | InterSortedPackageData],
          Group,
          [xpge(Group, PA, FName, First, Preds) | TailGroupEntries],  TailSortedPackageData)
        :-!,
        accum_grp(InterSortedPackageData, Group, TailGroupEntries, TailSortedPackageData).

accum_grp([xpge(NextGroup, PA, FName, First, Preds) | InterSortedPackageData],
           Group, [],
           [xpge(NextGroup, PA, FName, First, Preds) | InterSortedPackageData]).

expand_grp([], []).

expand_grp([xpge(Group, PA, FName, First, PredsWithDescs) | GroupEntries], Expanded)
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

write_pdata([], OS).

write_pdata([Group-GData | BrokenOutGroups], OS)
        :-
        group_display(Group, GroupDisp),
        printf(OS, '<div class="hentry">%t</div><br>\n', [GroupDisp]),
        write_group(GData, OS),
        write_pdata(BrokenOutGroups, OS).

write_group([], OS).
write_group([PA-DescAtom-FName| GData], OS)
        :-
        printf(OS,
            '<div class="eentry"><a class="astyl" target="content" href="%t.html" title="%t -- %t">%t</a></div><br>\n',
            [FName,PA,DescAtom,PA]),
        write_group(GData, OS).


	/* -------------------------------------------------------------------- *
	 |		new_page(Path)
	 |
	 |		Creates the framework for a new alshelp page,
	 |		by making a skeletal *.md page in folder md_help/
	 |
	 |	Path is a path to a file containing the following term:
	 |
	 |	np(Package, Group, Title, FileName,
	 |	    [<List of <PAs with Descrips>>] ).
	 |
	 |	where:
	 |	-  packages(PList), member(Package, PList)
	 |	-  pack_kid(Group, Package)
	 |	-  Title is of the form: P/A
	 |	-  md_help/<FileName>.md is the intended skeletal target
	 |	-  Each <PAs with Descrips> is of the form
	 |		p(<quoted atom Q/R>, <atom which is a short description of Q/R>)
	 |
	 |	The convenience predicate mknnp/0 will create the outline of
	 |	such an np(...) in the file ./nnp.np.
	 * -------------------------------------------------------------------- */

mknnp :- open('./nnp.np', write, OS),
	printf(OS, 'np(\n   ,  %Package\n   ,  %Group\n   ,  %Title\n   ,  %FileName\n', []),
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
	check_and_go(NewPageTerm).

check_and_go(np(Package, Group, Title, FileName, PAsWithDescs))
	:-
	check_package(Package),
	check_group(Group, Package),
	check_file(FileName),
	check_PAsWithDs(PAsWithDescs),
	make_new_page(Package, Group, Title, FileName, PAsWithDescs).

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
	printf('Bad Group %t for package %t\n', [Group,Package]),
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
	integer(R),
	atom(Desc).

make_new_page(Package, Group, Title, FileName, PAsWithDescs)
	:-
	file_extension(MDFile, FileName, md),
	src_folder_md_files(MD_Src_Folder),
        join_path([MD_Src_Folder, MDFile], TgtMDFile),
	open(TgtMDFile, write, OS),
	(write_out_new_page(Package, Group, Title, PAsWithDescs, OS) ->
		close(OS) ; close(OS) ).

write_out_new_page(Package, Group, Title, PAsWithDescs, OS)
	:-
	yaml(Title, PAsWithDescs, OS),
%	printf(OS, '# %t\n\n', [Title]),
%	short_descs(StrippedPreds, OS),
	short_descs(PAsWithDescs, OS),
	length(PAsWithDescs, MinNumForms),
        printf(OS, '\n## FORMS\n\n', []),
	do_forms_skels(MinNumForms, OS),
	finish_skeleton(OS).

yaml(Title, Preds, OS)
        :-
        printf(OS, '---\n', []),
        printf(OS, 'title: \'%t\'\n', [Title]),
        printf(OS, 'predicates:\n', []),
        yaml_preds(Preds, OS),
        printf(OS, '---\n', []).

yaml_preds([], _).
yaml_preds([p(PA,DescAtom) | Preds], OS)
        :-
        printf(OS, ' - \'%t\' : %t\n', [PA,DescAtom]),
        yaml_preds(Preds, OS).

short_descs([], OS).
short_descs([p(PA, DescAtom) | StrippedPreds], OS)
        :-
        printf(OS, '`%t` `--` %t\n\n', [PA,DescAtom]),
        short_descs(StrippedPreds, OS).
do_forms_skels(0, OS).
do_forms_skels(N, OS)
	:-
	N>0,
	printf(OS, '`P(B1,B2,...)`\n',[]),
	blnk(OS),
	M is N-1,
	do_forms_skels(M, OS).

finish_skeleton(OS)
	:-
        blnk(OS),
        printf(OS, '## DESCRIPTION\n',[]),
        blnk(OS),
        printf(OS, 'Description text...\n',[]),
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
	 |		idx_page(Path)
	 |
	 |	Path is as described above for new_page(Path).
	 |	ASSUMES that the np(...) at Path has been processed by 
	 |	new_page(Path) to produce a page in md_/help.
	 |	(The skeletal page may or may not have been fleshed out, and
	 |	 the corresponding *.html may or may not have been produced
	 |	 using pandoc or Jekyll.)
	 |
	 |	idx_page(Path) reads the np(...) term out of the file at Path,
	 |	and does three things:
	 |	A.  It reads the list from docsdb_<Package>.pro, adds
	 |	    an appropriate entry for the np term at the beginning of the
         |          list, and sorts the list;
	 |		docsdb_<Package>.pro
	 |	B.  Uses the sorted list to rewrite a new file 
	 |		toc_<Package>.html 
	 |	    in folder alshelp/.
	 |	C.  Rewrites docsdb_<Package>.pro using the sorted list
	 |	    produced in step A.
	 * -------------------------------------------------------------------- */
ii :- 
	Path = 'np_for_curl.np',
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
	NewPageTerm = np(Package, Group, Title, FileName, PAsWithDescs),
	    % All necessary checks made when new_page(Path) ran:
	rewrite_idx(Package, Group, Title, FileName, PAsWithDescs).

/*
rewrite_idx(NewPageTerm)
	:-
	file_extension(MDFile, FileName, md),
	src_folder_md_files(MD_Src_Folder),
        join_path([MD_Src_Folder, MDFile], TgtMDFile),
	open(TgtMDFile, write, OS),
	(rewrite_idx(Package, Group, Title, PAsWithDescs, OS) ->
		close(OS) ; close(OS) ).
*/

rewrite_idx(Package, Group, Title, FileName, PAsWithDescs)
	:-
	catenate(['./docsdb_', Package, '.pro'], PkgFile),
        open(PkgFile, read, IS),
	read_term(IS, XPGEList, [attach_fullstop(true)]),
	close(IS),

    	XPGETerm = xpge(Group,Title,FileName,PlainPred,PAsWithDescs),
	xtr_plain_title(Title, PlainPred),
	NewList = [XPGETerm | XPGEList],
	sort(NewList, SortedNewList),

        toc_locn(TocFolder),
        catenate(['toc_', Package, '.html'], TocFile),
        join_path([TocFolder, TocFile], TgtTocFile),
        open(TgtTocFile, write, OS),
        write_toc_header(OS),
	last_toc_phase( SortedNewList, OS),
	close(OS),

        open(PkgFile, write, OS2),
        write_term(OS2, SortedNewList, [ quoted(true) ]),
        write(OS2, '.'),nl(OS2),
        flush_output(OS2),
        close(OS2).

xtr_plain_title(PlainPred/_, PlainPred)
	:-!.

xtr_plain_title(Title, PlainPred)
	:-
	atom(Title),
	open(atom(Title), read, IS),
	read_term(IS, PA, [attach_fullstop(true)]),
	close(IS),
	PA = PlainPred/_.
