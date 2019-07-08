/*======================================================================
 |                      blts_xprt_udoc.pro
 |      Copyright (c) 2019 Applied Logic Systems, Inc.
 |              Group: Maintenance
 |              DocTitle: chk_blt_exps_docs
 |              -- List all exported, but undocumented, builtins
 *=====================================================================*/
:-['md_yml_process.pro'].
:-['wrk_mods.pro'].
:-['lib_undocd_files.pro'].

ref_path('../docs/ref/').

check_working_folders
	:-
	(exists_file('TmpDir') -> true ; make_subdir('TmpDir') ),
	(exists_file('ResultDir') -> true ; make_subdir('ResultDir') ).

	%% Remove temporary files:
cleanup_all :-
	(exists_file('TmpDir/exps_blts.txt') -> remove_file('TmpDir/exps_blts.txt'); true),
	(exists_file('TmpDir/blt_exported_grep.txt') 
		-> remove_file('TmpDir/blt_exported_grep.txt'); true),

	(exists_file('ResultDir/wrrk.txt') -> remove_file('ResultTmpDir/wrrk.txt'); true),

	(exists_file('ResultDir/undocd_exported_by_pred.txt') 
		-> remove_file('ResultDir/undocd_exported_by_pred.txt'); true),

	(exists_file('ResultDir/undocd_exported_by_bi_file.txt') 
		-> remove_file('ResultDir/undocd_exported_by_bi_file.txt'); true),

	(exists_file('ResultDir/docd_blts_by_pred.txt') 
		-> remove_file('ResultDir/docd_blts_by_pred.txt'); true),

	(exists_file('ResultDir/docd_blts_by_bi_file.txt') 
		-> remove_file('ResultDir/docd_blts_by_bi_file.txt'); true),

	(exists_file('ResultDir/docd_blts_by_md_file.txt') 
		-> remove_file('ResultDir/docd_blts_by_md_file.txt'); true),

	(exists_file('TmpDir/hashtable_install_index.txt') 
		->  remove_file('ResultDir/hashtable_install_index.txt'); true).


init_src_file('./TmpDir/exps_blts.txt').
	% raw working file -- created by shell script: xxp.sh, 
	% which is run by blt_files_exports/1 below

	%% dev:
cv :- chk_blt_exps_docs.
fb :- blt_files_exports(_).

start :- chk_blt_exps_docs.

/*!---------------------------------------------------------------------
 |	chk_blt_exps_docs/0
 |	chk_blt_exps_docs
 |	chk_blt_exps_docs
 |	
 |	-- Provides command line access to these routines:
 |	
 |	alspro blts_xprt_udoc.pro -g chk_blt_exps_docs -b
 |	alspro blts_xprt_udoc.pro -g start -b
 *!--------------------------------------------------------------------*/

chk_blt_exps_docs
	:-
	check_working_folders,
	cleanup_all,
	blt_files_exports(BixGroups),
	proc_all_yml(Md_Groups),
	printf('==========================\n',[]),
	check_lib_undocd.

/*!---------------------------------------------------------------------
 |      blt_files_exports/1
 |      blt_files_exports(BixGroups)
 |      blt_files(-)
 |
 |      - Creates the list of exported builtins, by file
 |
 |	First uses system/1 to run xxp.sh, creating TmpDir/exps_blts.txt.
 |	Then reads the lines of 'TmpDir/exps_blts.txt', and creates the 
 |	list BixGroups, while also writing to 'TmpDir/blt_exported_grep.txt',
 |	where all 'export' prefixes are deleted, using do_group/4, where
 | 	Groups are of the form Group = g(FF, GroupList), where FF is
 |      the name of the builtins file, and GroupList contains all the
 |	predicates exported from FF.
 |
 |	Creates files:
 |		TmpDir/exps_blts.txt
 |		TmpDir/blt_exported_grep.txt
 |		TmpDir/hashtable_install_index.txt
 |		ResultDir/undocd_exported.txt
 |		ResultDir/docd_blts.txt
 *!--------------------------------------------------------------------*/
blt_files_exports(BixGroups)
	:-
		%% grep for 'export' in all builtins files:
		%% target is init_src_file(Src):
	system('sh xxp.sh'),
	init_src_file(Src),
		%% get all the export lines:
	grab_lines(Src, Lines),
	open('TmpDir/blt_exported_grep.txt', write, BXStr),
	printf(BXStr, '>>>> Exports from builtins files: <<<<\n\n', []),
		%% Process raw export lines into groups by file,
		%% with all 'export' prefixes dropped:
	bix_groups(Lines, BixGroups, BXStr),
	close(BXStr),
		%% Build a hash table index of the md files: ../ALSProlog/docs/docs/ref
	md_index(Md_Groups),
		%% Run thru the BixGroups using the index to build lists of
		%% documented and undocumented builtins:
	classify(BixGroups, Md_Groups, Undocd, Docd),
	length(Undocd, NUn),
	length(Docd, ND),
	printf('Num undoc\'d, exported preds = %t  Num doc\'d preds = %t\n', [NUn, ND]),


	date(Date),
	sort(Undocd, SortedUndocd),
	sort_by_arg(Undocd, UndocdSortedByBIFile),
		%% Create file of the 'undocumented but exported' builtins:
	open('ResultDir/undocd_exported_by_pred.txt', write, UDXStr0),
	printf(UDXStr0, 'Undocumented But Exported Builtins Predicates by Predicate - %t\n\n', [Date]),
	columns([u(['Predicate', 'BltFile']) | SortedUndocd], UDXStr0),
	close(UDXStr0),

	open('ResultDir/undocd_exported_by_bi_file.txt', write, UDXStr1),
	printf(UDXStr1, 'Undocumented But Exported Builtins Predicates by Builtins File - %t\n\n', [Date]),
	columns([u(['Predicate', 'BltFile']) | UndocdSortedByBIFile], UDXStr1),
	close(UDXStr1),

	modsAndTitles(MTLs),
	add_mt_cols(Docd, MTLs, FullDocd),
	
	sort(FullDocd, SortedDocd),
	sort_by_arg(FullDocd, SortedByBIFile, SortedByMDFile),
		%% Create the file of exported & documented builtins,
		%% with the file each is exported from:
	open('ResultDir/docd_blts_by_pred.txt', write, DdStr0),
	printf(DdStr0, 'Exported & Documented Builtins Predicates by Predicate - %t\n\n', [Date]),
	columns([u(['Predicate', 'BltFile', 'MD File', 'Module', 'MdFileTitle']) | SortedDocd], DdStr0),
	close(DdStr0),

	open('ResultDir/docd_blts_by_bi_file.txt', write, DdStr1),
	printf(DdStr1, 'Exported & Documented Builtins Predicates by Builtins File - %t\n\n', [Date]),
	columns([u(['BltFile', 'Predicate', 'MD File', 'Module', 'MdFileTitle']) | SortedByBIFile], DdStr1),
	close(DdStr1),
	
	open('ResultDir/docd_blts_by_md_file.txt', write, DdStr2),
	printf(DdStr2, 'Exported & Documented Builtins Predicates by MD File - %t\n\n', [Date]),
	columns([u(['MD File', 'Predicate', 'BltFile' , 'Module', 'MdFileTitle']) | SortedByMDFile], DdStr2),
	close(DdStr2),

	printf('Output files for builtins are in ./ResultDir:\n', []),
	printf('     undocd_exported_by_pred.txt\n',[]),
	printf('     undocd_exported_by_bi_file.txt\n',[]),
	printf('     docd_blts_by_pred.txt\n',[]),
	printf('     docd_blts_by_bi_file.txt\n',[]),
	printf('     docd_blts_by_md_file.txt\n',[]),

	(exists_file('TmpDir/hashtable_install_index.txt') 
		->  remove_file('ResultDir/hashtable_install_index.txt'); true),
	(exists_file('TmpDir/exps_blts.txt') -> remove_file('TmpDir/exps_blts.txt'); true),
	(exists_file('TmpDir/blt_exported_grep.txt') 
		-> remove_file('TmpDir/blt_exported_grep.txt'); true).

	%% Process the raw exports lines grouped by builtins file:
bix_groups([], [], BXStr).
bix_groups(Lines, [Group | RestBixGroups], BXStr)
	:- 
	do_group(Lines, RestLines, Group, BXStr),
	bix_groups(RestLines, RestBixGroups, BXStr).

do_group([Line | Lines], RestLines, Group, BXStr)
	:-
	Line == '',
	!,
	do_group(Lines, RestLines, Group, BXStr).
	
do_group([Line | Lines], RestLines, Group, BXStr)
	:-
	sub_atom(Line, 9, _, 0, FF0),
		%% strip single quotes from file names:
	atom_codes(FF0, FF0Codes),
	FF0Codes = [_ | FF0CodesTail],
	dreverse(FF0CodesTail, [_ | RevFFCodes]),
	dreverse(RevFFCodes, FFCodes),
	atom_codes(FF, FFCodes),
	printf(BXStr, '\n >>> File: %t << \n\n', [FF]),
	get_exps(Lines, GroupList, RestLines, BXStr),
	Group = g(FF, GroupList).

get_exps([Line | RestLines], [], RestLines, BXStr)
	:-
	Line == '',
	!.
get_exps([Line | TailLines], [CleanExp | GroupList], RestLines, BXStr)
	:-
	sub_atom(Line, 7, _, 1, Exp),
	clean_up(Exp, CleanExp),
	printf(BXStr, '  %t\n', [CleanExp]),
	get_exps(TailLines, GroupList, RestLines, BXStr).

	% delete spaces before slash, and single quotes:
clean_up(Exp, CleanExp)
	:-
	atom_codes(Exp, ExpCodes),
	do_clean_up(ExpCodes, CleanExpCodes),
	atom_codes(CleanExp, CleanExpCodes).

do_clean_up([], []).
do_clean_up([0'' | ExpCodesTail], CleanExpCodesTail)
	:-!,
	do_clean_up(ExpCodesTail, CleanExpCodesTail).
do_clean_up([0' ,0'/ | ExpCodesTail], [0'/ | CleanExpCodesTail])
	:-!,
	do_clean_up(ExpCodesTail, CleanExpCodesTail).
do_clean_up([C | ExpCodesTail], [C | CleanExpCodesTail])
	:-!,
	do_clean_up(ExpCodesTail, CleanExpCodesTail).

	%% Build a hash table index of the md files: ../ALSProlog/docs/docs/ref
	%% Hash table base is: _mdf_index
mi :- md_index.

md_index(Md_Groups) 
	:-
	make_hash_table('_mdf_index'),
	reset_mdf_index,
		/*
		reset_mdf_index
		set_mdf_index(Key,Value)
		get_mdf_index(Key,Value)
		del_mdf_index(Key,Value)
		pget_mdf_index(KeyPattern,ValPattern)
		pdel_mdf_index(KeyPattern,ValPattern)
		*/
	proc_all_yml(Md_Groups),
	!,
	open('TmpDir/hashtable_install_index.txt', write, StrIDX),
	printf(StrIDX, 'PAs installed in hash table _mdf_index\n\n', []),
	install_index(Md_Groups, StrIDX),
	close(StrIDX).

install_index([], StrIDX).
install_index([Group | Md_Groups], StrIDX)
	:-
	Group = g(MD_File, PredBlocks, TagsVAls),
	/*   Single:
		PBlock = p(PA, Desc).
	     Multi:
		p(PA, Desc)

	g(trace.md,[mp([p(trace/0,turn on tracing),p(trace/1,trace the execution of a goal)]),p(notrace/0,turn off tracing)])
	 */
	index_preds(PredBlocks, MD_File, StrIDX),
	install_index(Md_Groups, StrIDX).

index_preds([], _, StrIDX).
index_preds([p(PA,D) | PredsBlocks], MD_File, StrIDX)
	:-
	(atom(PA) -> APA=atom, 
		AtomicPA = PA
		; APA=non_atom,
		term_codes(PA, PACodes), atom_codes(AtomicPA, PACodes)
	), 
	printf(StrIDX, '%t - sing - %t\n', [PA, APA]),
	set_mdf_index(AtomicPA,MD_File),
	!,
	index_preds(PredsBlocks, MD_File, StrIDX).

index_preds([mp(PAD_List) | PredsBlocks], MD_File, StrIDX)
	:-
	insert_list(PAD_List, MD_File, StrIDX),
	!,
	index_preds(PredsBlocks, MD_File, StrIDX).
index_preds([PAD | PredsBlocks], MD_File, StrIDX)
	:-
	printf('index_preds failed at %t\n', [PAD]).

insert_list([], _, StrIDX).
insert_list([p(PA,D) | PAD_List], MD_File, StrIDX)
	:-
	set_mdf_index(PA,MD_File),
	(atom(PA) -> APA=atom ; APA=non_atom),
	printf(StrIDX, '%t - mult - %t\n', [PA, APA]),
	insert_list(PAD_List, MD_File, StrIDX).


classify([], _, [], []).
classify([g(FF, GroupList) | BixGroups], 
		Md_Groups,
		Undocd,
		Docd)
	:-
(FF=='identity.md' -> trace ; true),
	do_classify(GroupList, FF, Md_Groups, Undocd, TailUndocd, Docd, TailDocd),
	classify(BixGroups, Md_Groups, TailUndocd, TailDocd).

do_classify([], FF, _, TailUndocd, TailUndocd, TailDocd, TailDocd).
do_classify([PA | GroupList], FF, Md_Groups, UndocdIn, UndocdOut, DocdIn, DocdOut)
	:-
	(atom(PA) -> APA=atom, 
		AtomicPA = PA
		; APA=non_atom,
		term_codes(PA, PACodes), atom_codes(AtomicPA, PACodes)
	), 

	( get_mdf_index(AtomicPA,PAValue) -> 
		member(g(PAValue, GG2, GG3), Md_Groups),
		!,
		DocdIn = [[AtomicPA, FF, PAValue] | TailDocd], 
		UndocdIn = UndocdOut
		;
	 	UndocdIn = [[PA, FF] | TailUndocd], 
		DocdIn = TailDocd
	), 
	do_classify(GroupList, FF, Md_Groups, TailUndocd, UndocdOut, TailDocd, DocdOut).

add_mt_cols([], _, []).
add_mt_cols([[PA,BiF,MdF] | Docd], MTLs, [[PA,BiF,MdF, Mod, Title] | FullDocd])
	:-
	member(t(MdF, Title, Mod), MTLs),
	add_mt_cols(Docd, MTLs, FullDocd).

sort_by_arg(Docd, SortedByBIFile)
	:-
	rotate_args(Docd, ByBIFile),
	sort(ByBIFile, SortedByBIFile).

rotate_args([], []).
rotate_args([[E1,E2 | TailEs] | DocdTail], [[E2,E1 | TailEs] |  SortedByBIFile])
	:-
	rotate_args(DocdTail, SortedByBIFile).

sort_by_arg(Docd, SortedByBIFile, SortedByMDFile)
	:-
	rotate_args(Docd, ByBIFile, ByMDFile),
	sort(ByBIFile, SortedByBIFile),
	sort(ByMDFile, SortedByMDFile).

rotate_args([], [], []).
rotate_args([[E1,E2,E3 | TailEs] | DocdTail], [[E2,E1,E3 | TailEs] |  SortedByBIFile], [[E3,E1,E2 | TailEs] | SortedByMDFile])
	:-
	rotate_args(DocdTail, SortedByBIFile, SortedByMDFile).


