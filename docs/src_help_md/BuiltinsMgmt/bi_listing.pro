/*======================================================================
 |                      bi_listing.pro
 |      Copyright (c) 2019 Applied Logic Systems, Inc.
 |              Group: Maintenance
 |              DocTitle: 
 |              -- Provides tables of info about builtins
 *=====================================================================*/
:-['md_yml_process.pro'].
:-['../ISO/iso.pro'].
:-['mods_blts.pro'].

ref_path('../../docs/ref/').
init_src_file('./TmpDir/exps_blts.txt').
iso_master_path('../ISO/master_iso.pro').
iso_status_info_path('../ISO/iso_status_info.txt').
iso_matched_list_path('../ISO/iso_matched_list.txt').
iso_plus_md_path('../ISO/iso_plus_md.pro').

blt_files_output('Exported_Undocd.txt').

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
		->  remove_file('TmpDir/hashtable_install_index.txt'); true).

bi :- bi_table_lists.

bi_table_lists
	:-
	cleanup_all,
	check_working_folders,

	printf('Processing docs/ref/*.md files\n', []),
	proc_all_yml(_, MdInfoByPreds, MdInfoByPredsTail),
	MdInfoByPredsTail = [],

	blt_files_exports(BixGroups),
	bg2_by_pred(BixGroups, PredsWithFiles),

	printf('Loading ISO info\n', []),
	iso_master_path(ISO_masterPath),
	open(ISO_masterPath, read, ISOIn),
	read(ISOIn, ISOMasterList),
	close(ISOIn),

	date(Date),
	blts_mods(BltsMods),
	printf('Begin processing nametable\n', []),
	grindNtbl(Date, BltsMods, RawNTblList),
	rearr(RawNTblList, MdInfoByPreds, PredsWithFiles, ISOMasterList, NearFinal, MatchedISO),

	iso_status_info_path(ISOstatusInfoPath),
	(exists_file(ISOstatusInfoPath) -> remove_file(ISOstatusInfoPath); true),
	open(ISOstatusInfoPath, write, ISOstream),
	list_diff(ISOMasterList, MatchedISO, UnMatchedISO),
	sort(UnMatchedISO, SortedUnMatchedISO),
	printf(ISOstream, '\n Unmatched ISO Items:\n\n', []),
	write_clauses(ISOstream, SortedUnMatchedISO, [quoted(true)]),
	sort(MatchedISO, SortedMatchedISO),
	printf(ISOstream, '\nMatched ISO Items:\n\n', []),
	write_clauses(ISOstream, SortedMatchedISO, [quoted(true)]),
	close(ISOstream),
	iso_matched_list_path(ISOmatchedListPath),
	open(ISOmatchedListPath, write, IMLstream),
	printf(IMLstream, 'iso_matched(%t).\n', [SortedMatchedISO], [quoted(true)]),
	close(IMLstream),

	sort(NearFinal, SortedNearFinal),
	open('final_pred_info.pro', write, FPIstream),
	printf(FPIstream, 'total_bi_pred_info(%t).\n', [SortedNearFinal],[quoted(true)]),
	close(FPIstream),

	undoc_bi_by_file(SortedNearFinal),

	iso_md(SortedNearFinal, ISOMd),
	iso_plus_md_path(ISOplusMDpro),
	open(ISOplusMDpro, write, MDISOstream),
	printf(MDISOstream, 'md_iso(%t).\n', [ISOMd],[quoted(true)]),
	close(MDISOstream),

	PHH = u(['Pred', 'Mod', 'Type', 'ISO', 'ISOlink', 'MDFile', 'SrcFile']),
	insert_page_headers(SortedNearFinal, 30, 30, PHH, HeadedFinal),
	printf('Begin writing master output table\n', []),

	(exists_file('primary_pred_info.txt') 
		-> remove_file('primary_pred_info.txt'); true),
	open('primary_pred_info.txt', write, PPstream),

	columns([ u(['Pred', 'Mod', 'Type', 'ISO', 'ISOlink', 'MDFile', 'SrcFile']) | HeadedFinal], PPstream),
	close(PPstream),
	printf('Finished writing master output table:  %t\n\n', ['primary_pred_info.txt']),

write('-----'),nl,
	printf('Generated files:\n',[]),
write('    ---'),nl,
	printf('primary_pred_info.txt\n', []),
	printf('final_pred_info.pro\n', []),
	printf('Exported_Undocd.txt\n', []),
	printf('ResultDir/grindNtbl.txt\n', []),
	printf('TmpDir/blt_exported_grep.txt\n', []),

	printf('..ISO/iso_status_info.txt\n', []),
	printf('../ISO/iso_matched_list.pro\n', []),
	printf('../ISO/iso_plus_md.pro\n', []),
	nl,

statistics.



grindNtbl(Date, BltsMods, RawNTblList) :-
	open('ResultDir/grindNtbl.txt', write, GStr),
	printf(GStr, 'Grind Predicates Analysis by Predicate - %t\n\n', [Date]),
	gg(0, GStr, BltsMods, RawNTblList),
	close(GStr).

gg(N, GStr, BltsMods, [])
	:-
	N > 16384,
	!.
gg(N, GStr, BltsMods, RawNTblList)
	:-
	'$procinfo'(N,Module,Pred,Arity,DBRef,ProcType),
	!,
	cont_gg(N, GStr, Module,Pred,Arity,DBRef,ProcType, BltsMods, RawNTblList).

gg(N, GStr, BltsMods, RawNTblList)
	:-
	M is N+1,
	gg(M, GStr, BltsMods, RawNTblList).

cont_gg(N, GStr, Module,Pred,Arity,DBRef,ProcType, BltsMods, RawNTblList)
	:-
	member(Module, BltsMods),
		%% skipping: 2=imported 3=Undefined (module_closure, etc)
	ProcType \= 3,
	ProcType \= 2,
	'$exported_proc'(Module,Pred,Arity),
		%semi_1561746628_99
	not(sub_atom(Pred,1,5,_,'semi_')),
	!,
	check_done_gg(N, GStr, Module,Pred,Arity,DBRef,ProcType, BltsMods, RawNTblList).

cont_gg(N, GStr, Module,Pred,Arity,DBRef,ProcType, BltsMods, RawNTblList)
	:-
	do_check_done_gg(N, GStr, Module,Pred,Arity,DBRef,ProcType, BltsMods, RawNTblList).

check_done_gg(N, GStr, Module,Pred,Arity,DBRef,ProcType, BltsMods, RawNTblList)
	:-
	'$nextproc'(N,_,NN),
	!,
	RawNTblList = [[N, NN, Module,Pred,Arity,ProcType] | RawNTblListTail],
	printf(GStr, 'N=%t [NN=%t] M=%t P=%t A=%t PT=%t\n',[N, NN, Module,Pred,Arity,ProcType,XP]),
	M is N+1,
	gg(M, GStr, BltsMods, RawNTblListTail).

check_done_gg(N, GStr, Module,Pred,Arity,DBRef,ProcType, BltsMods, []).

do_check_done_gg(N, GStr, Module,Pred,Arity,DBRef,ProcType, BltsMods, RawNTblListTail)
	:-
	'$nextproc'(N,_,NN),
	!,
	M is N+1,
	gg(M, GStr, BltsMods, RawNTblListTail).
do_check_done_gg(N, GStr, Module,Pred,Arity,DBRef,ProcType, BltsMods, []).

is_exported(Module,Pred,Arity, true)
	:-
	'$exported_proc'(Module,Pred,Arity),
	!.
is_exported(Module,Pred,Arity, false).


rearr([], MdInfoByPreds, PredsWithFiles, ISOMasterList, [], []).
	% [16259,16260,builtins,alarm,2,1,true]
rearr([NTEnt | RawNTblListTail], MdInfoByPreds, PredsWithFiles, ISOMasterList, [PredEnt | NearFinalTail], MatchedISO)
	:-
	rearr_ent(NTEnt, MdInfoByPreds, PredsWithFiles, ISOMasterList, PredEnt, MatchedISO, MatchedISOTail),
	rearr(RawNTblListTail, MdInfoByPreds, PredsWithFiles, ISOMasterList, NearFinalTail, MatchedISOTail).

	% rearr_ent(NTEnt, MdInfoByPreds, ISOMasterList, PredEnt),
	% [16259,16260,builtins,alarm,2,1,true]
rearr_ent([N, NN, Mod, Pred, Arity, ProcType], MdInfoByPreds, PredsWithFiles, ISOMasterList, 
		PredEnt, MatchedISO, MatchedISOTail)
	:-
	sprintf(PAString, '%t/%t', [Pred, Arity]),
	atom_codes(PA, PAString),
	procTypeCode(ProcType, PTCode),
	mdEntry(PA, MdInfoByPreds, MDFile, Tags, Desc),
	iso_info(PA, ISOMasterList, ISO, ISOlink, MDFile, MatchedISO, MatchedISOTail),
		%	PredEnt = [PA, Mod, PTCode, Exported, ISO, ISOlink, MDFile, Tags, Desc].
	sourceFile(PA, PredsWithFiles, SrcFile),
	PredEnt = [PA, Mod, PTCode, ISO, ISOlink, MDFile, SrcFile].

	%% skipping: 2=imported 3=Undefined (module_closure, etc)
procTypeCode(0, 'P_def').
procTypeCode(1, 'C_def').
procTypeCode(4, 'Lib').

	% member(p('write/1', D, MDF, Tags), MdInfoByPreds),
mdEntry(PA, MdInfoByPreds, MDFile, Tags, Desc)
	:-
	dmember(p(PA, Desc, MDFile, Tags), MdInfoByPreds),
	!.
	
mdEntry(PA, MdInfoByPreds, '', [], '').

iso_info(PA, ISOMasterList, 'ISO', ISOlink, MDFile, MatchedISO, MatchedISOTail)
	:-
	dmember([PA, ISOlink], ISOMasterList),
	!,
	MatchedISO = [ [PA, ISOlink, MDFile] | MatchedISOTail].
iso_info(PA, ISOMasterList, '', '', MDFile, MatchedISO, MatchedISO).


insert_page_headers([], _, _, _, []).
insert_page_headers(Rows, 0, N, PHH, [PHH | PHRows])
	:-!,
	insert_page_headers(Rows, N, N, PHH, PHRows).
insert_page_headers([Row | Rows], M, N, PHH, [Row | PHRows])
	:-
	K is M-1,
	insert_page_headers(Rows, K, N, PHH, PHRows).

sourceFile(PA, PredsWithFiles, SrcFile)
	:-
	dmember(fp(PA,SrcFile), PredsWithFiles),
	!.
sourceFile(PA, PredsWithFiles, '').

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
	close(BXStr).


bg2_by_pred(BixGroups, PredsFs)
	:-
	bg2p(BixGroups, PredsFs, LastTail),
	LastTail = [].

bg2p([], LastTail, LastTail).
bg2p([g(BIFile, PList) | BixGroups], PredsFs, LastTail)
	:-
	do_plf(PList, BIFile, PredsFs, InterTail),
	bg2p(BixGroups, InterTail, LastTail).

do_plf([], BIFile, Tail, Tail).
do_plf([PA | PListTail], BIFile, [fp(PA, BIFile) | InterPTail], LastPTail)
	:-
	do_plf(PListTail, BIFile, InterPTail, LastPTail).



iso_md([], []).
	%% ['close/1',sio,'P_def','ISO','#close','close.md','sio.pro'],
iso_md([Item | SNF], [Item | ISOMd])
	:-
	Item = [_,_,_,Iso,_,Md,_], Iso \= '', Md \= '', 
	!,
	iso_md(SNF, ISOMd).
iso_md([_ | SNF], ISOMd)
	:-
	iso_md(SNF, ISOMd).


undoc_bi_by_file(SortedNearFinal)
	:-
	printf('Begin undocumented, but exported, builtins preds.\n',[]),
	find_undoc_with_file(SortedNearFinal, File_Undoc),
	sort(File_Undoc, SortedUndoc_File),
	merge_groups(SortedUndoc_File, MergeGroups),
	outputGroups(MergeGroups).

/*
SortedNearFinal:
	.....
	[all_ntbl_entries/4,builtins,P_def,,,procedures.md,blt_sys.pro],
	[all_procedures/3,builtins,P_def,,,,blt_sys.pro],
	[all_procedures/4,builtins,P_def,,,procedures.md,blt_sys.pro],
	.....
	[assert_all/1,builtins,C_def,,,,],
	[assert_all/2,builtins,Lib,,,,],
	.....
 */
find_undoc_with_file([], []).
find_undoc_with_file([SNFE | RestSortedNearFinal], [(BltFile,PA) | RestUndocBList])
	:-
	SNFE = [PA,_,_,_,_,MDE,BltFile],
	MDE == '',
	BltFile \= '',
%printf('SNFE=%t  PA=%t  MDE=%t  BltFile=%t\n', [SNFE,PA,MDE,BltFile]),flush_output,
	!,
	find_undoc_with_file(RestSortedNearFinal, RestUndocBList).
find_undoc_with_file([SNFE | RestSortedNearFinal], UndocBList)
	:-
	find_undoc_with_file(RestSortedNearFinal, UndocBList).

merge_groups([], []).
merge_groups([(BltFile,PA) | RestSortedUndoc_File], [(BltFile,FileGroupPreds) | MergedGroups])
	:-
%printf('merge: %t\n', [BltFile]),
%(BltFile == 'xconsult.pro' -> trace ; true),
	xtractFileEntries([(BltFile,PA) | RestSortedUndoc_File], 
				BltFile, FileGroupPreds, RemSortedUndoc_File),
	merge_groups(RemSortedUndoc_File, MergedGroups).

xtractFileEntries([], _, [], []).
xtractFileEntries([(BltFile,PA) | RestSortedUndoc_File], 
			BltFile, [PA | RestFileGroupPreds], RemSortedUndoc_File)
	:-
	!,
	xtractFileEntries(RestSortedUndoc_File, BltFile, RestFileGroupPreds, RemSortedUndoc_File).

xtractFileEntries([(OtherBltFile,PA) | RestSortedUndoc_File], BltFile, [], RestSortedUndoc_File)
	:-
	OtherBltFile \= BltFile,
	!.

outputGroups(MergeGroups)
	:-
	blt_files_output(Expt_Undocd),
	open(Expt_Undocd, write, OutS),
%	OutS = user_output,
	outGroups(MergeGroups, OutS).

outGroups([], OutS).
outGroups([(FileName, Preds) | RestMergeGroups], OutS)
	:-
	sort(Preds, SortedPreds),
	printf(OutS, '\n%t:\n==========\n', [FileName]),
	write_lines(OutS,SortedPreds, [left_mar('      ')]),
	outGroups(RestMergeGroups, OutS).
	
