/*====================================================================
 |			cref.pro
 |	Copyright (c) 1989-96,2016 Applied Logic Systems, Inc.
 |
 |		Prolog cross-referencer
 |
 | Author(s): Ken Bowen, Aida Batarekh
 | Date(s):	November, 1988 (KAB - original)
 |		January, 1989 (Expansion - Aida Batarekh) 	
 |		AVL tree tools added July 1991 (KAB)
 | 		November, 2016 (KAB)
 |
 |	Main entry points:
 |		Both these are library hooks, auto-loading cref.pro:
 |	    cref(SuiteName) 	- runs "nonstop", no cref shell
 |	    cref(SuiteName, Opts)) 
 |				- enter interactive cref shell after initial analysis;
 |				  type 'help' to shell for commands
 |		Neither of the following  auto-load cref.pro; from the OS console, issue:
 |		    	> alspro cref
 |	    c(SuiteName)	- equivalent to cref(SuiteName, [])
 |	    d(SuiteName)	- equivalent to cref(SuiteName, nonstop)
 |				  runs without pausing ofr cref shell
 |
 |	Several test/example suites are built-in, recorded in cref_suite_db.pro:
 |	    suite_info(hickory, 'examples/als',
 |			['hickory.pro','id.pro'],'hickory.xrf').
 |	    suite_info(tc, 'alsdir/library/tests',
 |			['cref_test1.pro','cref_test2.pro', 'cref_test3.pro'],'tc.xrf').
 |	    suite_info(tc2,'alsdir/library/tests',
 | 			['cref_test_lib1.crf'],'tc2.xrf').
 |	    suite_info(chat80,'examples/chat80',
 |			['als_chat.pro','*.pl'],'chat80.xrf').
 |	If myfile.pro is present in the current directory, then to run cref on myfile.pro,
 |	just issue
 |		cref('myfile.pro').
 |	To execute cref on <path>/myfile.pro (where <path> \= '.'), just issue
 |		cref('<path>/myfile.pro').
 |	The report file myfile.xrf will be writte in the current directory.
 |
 | NOTE: To use the debugger on this file, comment out
 |		noshow_module(cref).
 | 	 in ~/builtins/debugger.pro
 *====================================================================*/

:-[ - 'alsdir/library/suite_db.pro'].

	/*-------------------------*
	 |	cref
 	 *-------------------------*/
module cref.

:-set_prolog_flag(unknown, fail).
:-dynamic(op_cref/2).

:-dynamic(nonstop/0).

	/* Callable as library predicates (this file will auto load): */
export cref/1.
export cref/2.
export cref_shell/0.
export start_cref_shell/2.
export rcs/0.
export restart_cref_shell/0.
export cx/1.

	/* Callable after this file is loaded; (start with "alspro cref"): */
export c/1.
export d/1.
export ct0/0.
export ct1/0.

	/*-------------------------------------------------------
   	 |	Global variables which hold the AVL trees
 	 *--------------------------------------------------------*/
		/* Now executed at end of file, after defStruct generated code:
		   :-make_gv('CallsTree'),avl_create(T),setCallsTree(T).
		   :-make_gv('ShellStruct'), makeCRSH(S), setShellStruct(S).
		   :-make_gv('MiscInfo'), makeMI(S), setMiscInfo(S).
		*/

:- dynamic(suite_info/4).

	/*-------------------------------------------------------
   	 |	Standard included suites for demo/testing.
 	 *--------------------------------------------------------*/
ct0 :- cref('alsdir/library/tests/cref_test0.crf').
ct1 :- cref('alsdir/library/tests/cref_test_lib1.crf').

	/*-------------------------------------------------------
   	 |	Suite (file collections) for demo/testing
	 |	--- see cref_suite_db.pro
 	 *--------------------------------------------------------*/


	%% SuiteDesc == <path>/file.crf
get_make_info(SuiteDesc,Directory,FilesList,TargetFile, ConfigInfo, SuiteName)
	:-
	file_extension(SuiteDesc, PathSansExt, 'crf'),
	exists_file(SuiteDesc),
	!,
	open(SuiteDesc,read,ISS,[]),
	read_terms(ISS,SpecList),
	close(ISS),

	(dmember(name=SuiteName, SpecList) ->
		true
		;
		pathPlusFile(_, SuiteName, PathSansExt) 
	),

	(dmember(dir=RawDirectory, SpecList) ->
		(is_absolute_path(RawDirectory) ->
			Directory = RawDirectory
			;
			path_directory_tail(Directory, '.', RawDirectory)
		)
                ;
		Directory = '.'
        ),
	dmember(files=RawFilesList,SpecList),
	pathPlusFilesList(RawFilesList, Directory, FilesList),
	assert(commonFilesLocn(Directory)),
	assert(strippedFiles(RawFilesList)),

        (dmember(config=ConfigInfo, SpecList) ->
                true
                ;
                ConfigInfo = []
        ),
	(dmember(tgt = TargetFile, SpecList) ->
		true
		;
		file_extension(TargetFile,SuiteName,xrf)
	).

	% suite_info(hickory,'examples/als',['hickory.pro','id.pro'],'hickory.xrf').
get_make_info(SuiteDesc,Directory,FilesList,TargetFile, ConfigInfo, SuiteDesc)
	:-
	suite_info(SuiteDesc,Directory,FilesList,TargetFile, ConfigInfo), 
	!.

	% suite_info(hickory,'examples/als',['hickory.pro','id.pro'],'hickory.xrf').
get_make_info(SuiteDesc,Directory,FilesList,TargetFile, [], SuiteDesc)
	:-
	suite_info(SuiteDesc,Directory,RawFilesList,TargetFile), 
	!,
	pathPlusFilesList(RawFilesList, Directory, FilesList),
	assert(commonFilesLocn(Directory)),
	assert(strippedFiles(RawFilesList)).


	% cref('/Users/ken/ALS/GitHub/ALSProlog/examples/als/diff.pro').
get_make_info(SuiteDesc,Path,FilesList,TargetFile, [], SuiteName)
	:-
	pathPlusFile(Path, File, SuiteDesc),
	Path \= '',
	file_extension(File,SuiteName,pro),
	!,
	FilesList = [SuiteDesc],
	assert(commonFilesLocn(Path)),
	assert(strippedFiles([File])),
	file_extension(TargetFile,SuiteName,xrf).

	% cref('bench.pro').
get_make_info(SuiteDesc,'.',FilesList,TargetFile, [], SuiteName)
	:-
	file_extension(SuiteDesc,SuiteName,pro),
	!,
	FilesList = [SuiteDesc],
	assert(commonFilesLocn('.')),
	assert(strippedFiles(FilesList)),
	file_extension(TargetFile,SuiteName,xrf).

/*!-----------------------------------------------------------------------
 |	cref/1
 |	cref(SuiteName) 
 |	cref(+) 
 |
 |	- perform cref processing on program suite SuiteName
 *-----------------------------------------------------------------------*/
cref(SuiteName) 
	:-
	cref(SuiteName, nonstop).

c(SuiteName) :- cref(SuiteName, []).
d(SuiteName) :- cref(SuiteName, nonstop).

/*!-----------------------------------------------------------------------
 |	cref/2
 |	cref(SuiteName, Options) 
 |	cref(+, +) 
 |
 |	- perform cref processing on program suite SuiteName, using Options
 *-----------------------------------------------------------------------*/
cref(SuiteDesc, Options)
	:-
	(Options = nonstop ->
		assert(nonstop)
		;
		process_options(Options)
	),
	abolish(commonFilesLocn/1),
	abolish(strippedFiles/1),
	abolish(definedList/1),
	abolish(nodefList/1),
	remove_prev_op_decls,

	get_make_info(SuiteDesc,Directory,FilesList,TargetFile, ConfigInfo, SuiteName),

	get_cwd(CurDir),
%	change_cwd(Directory),

		%%  Clear out the calls tree in case we are re-running:
	avl_create(ET),
	setCallsTree(ET),

	avl_create(T),setCallsTree(T),
	makeCRSH(S0), setShellStruct(S0),
	makeMI(S1), setMiscInfo(S1),


	do_cref(FilesList),
	change_cwd(CurDir),
	als_advise('Cref: Finished with %t\n',[SuiteName]),
	(not(nonstop) ->
		start_cref_shell(SuiteName, [dir=Directory, files=FilesList | Options])
		;
		true
	),
		%Exited interactive shell
	(dmember(TargetFile,[user,user_output]) ->
		OutSt = user_output
		;
		open(TargetFile, write, OutSt, [])
	),
	file_extension(TargetFile, SuiteBaseName, xrf),
	file_extension(TargetHTML, SuiteBaseName, html),
	open(TargetHTML, write, OutHTML, []),
	
	gen_file_header(OutSt,cref_suite-SuiteName,TargetFile), 
	printf(OutSt,'\t--by library/cref.pro\n\n',[]),
	(commonFilesLocn(CFL) -> 
		printf(OutSt, '    Common Files Location: %t\n', [CFL])
		; 
		printf(OutSt, '    No Common Files Location\n', [])
	),
	strippedFiles(PlainFilesList),
	printf(OutSt, '    Suite Files:\n\t%t\n\n',[PlainFilesList]),

	gen_HTML_header(OutHTML,SuiteName,SuiteDesc,TargetFile), 

	getCallsTree(CallsTree),
	write_cref_file(FilesList,CallsTree,OutSt,SuiteName,OutHTML),

	(dmember(TargetFile,[user,user_output]) ->
		true
		;
		close(OutSt)
	),
	finish_HTML_doc(OutHTML),
	als_advise('Cref: Finished with %t for %t\n',[TargetFile, SuiteName]).

process_options([]) :-!.
process_options([Opt | Options])
	:-!,
	process_opt(Opt),
	process_options(Options).
process_options(Options)
	:-
	process_options([Options]).

process_opt(include_mods=List)
	:-
	process_include_mods(List),
	!.

process_opt(nonstop)
	:-
	assert(nonstop).

process_opt(Opt)
	:-
	als_advise('Unknown cref option: %t\n', [Opt]).

process_include_mods([]) :-!.
process_include_mods([Mod | List])
	:-!,
	(retract(excluded_mod(Mod)) ; 
		als_advise('Options: Module %t not excluded\n',[Mod])),
	!,
	process_include_mods(List).
process_include_mods(List)
	:-
	process_include_mods([List]).

	%% Undo operator declarations carried out by process_goal:
remove_prev_op_decls
	:-
	op_cref(OMs, op(_, Spec, Op)),
	zero_out_op(OMs, Spec, Op),
	fail.
remove_prev_op_decls.

zero_out_op([], Spec, Op).
zero_out_op([M | OMs], Spec, Op)
	:-
	(builtins:current_op(BP, Spec, Op) ->
		M:op(BP, Spec, Op)
		;
		M: op(0, Spec, Op)
	),
	zero_out_op(OMs, Spec, Op).

do_cref(Files)
	:-
	do_cref(Files, [user]).

do_cref([], [user])
	:-!.
do_cref([], [])
	:-!,
	als_advise('!!Warning: Too many endmods (at closing last file)',[]).
do_cref([], Mods)
	:-!,
	list_delete(Mods, user, DMods),
	als_advise('!!Warning: At closing the last file, the following modules\n',[]), 
	als_advise('           are still open:%t\n\n',[DMods]).

do_cref([File | Files], OpenMods)
	:-
	resolve_file(File, FileName),
	(FileName = [_|_] ->
			%% it's a list of file names:
		do_cref(FileName, OpenMods)
		;
		als_advise('>>Starting file %t\n',[FileName]),
		!,
		open(FileName, read, InS, []),
		cross_ref(OpenMods,NewOpenMods,FileName, AddlFiles, InS),
		close(InS),
 		rcrd(file(FileName)),
		als_advise('\n<<Finished file %t\n',[FileName]),
		resolve_file_list(AddlFiles, AddlFileNames),
		sort(AddlFileNames, SortedAddlFileNames),
		appendNew(SortedAddlFileNames, Files, NewFiles),
		do_cref(NewFiles, NewOpenMods)
	).

do_cref([File | Files], OpenMods)
	:-
	als_advise('!!!>>!!! ERROR: File %t does not exist!\n',[File]),
	do_cref(Files, OpenMods).

resolve_file(File, File)
	:-
	exists_file(File),
	!.

resolve_file(File, ExtendedFilesList)
	:-
	pathPlusFile(Path, FName, File),
	file_extension(FName, '*', Ext),
	files(Path, FName, FilesList),
	pathPlusFilesList(FilesList, Path, ExtendedFilesList).

resolve_file(File, FileName)
	:-
	file_extension(FileName, File, pro),
	exists_file(FileName),
	!.
resolve_file(File, FileName)
	:-
	file_extension(FileName, File, pl),
	exists_file(FileName),
	!.

resolve_file_list([], []).
resolve_file_list([F | Files], [FN | FileNames])
	:-
	resolve_file(F, FN),
	!,
	resolve_file_list(Files, FileNames).
resolve_file_list([_ | Files], FileNames)
	:-
	resolve_file_list(Files, FileNames).

appendNew([], Files, Files).
appendNew([FN | AddlFileNames], Files, NewFiles)
	:-
	member(FN, Files),
	!,
	appendNew(AddlFileNames, Files, NewFiles).
appendNew([FN | AddlFileNames], Files, [FN | NewFiles])
	:-
	appendNew(AddlFileNames, Files, NewFiles),
	appendNew(AddlFileNames, Files, NewFiles).

	%% ------- Write the output file -------- %%

write_cref_file(Source,CallsTree,OutSt,SuiteName,OutHTML) 
	:-
	modules(CallsTree,OutSt,OutHTML),
 	group(CallsTree,DependsOnList,OutSt,OutHTML),
	avl_inorder(CallsTree, InOrderList),
 	asserteds(CallsTree,InOrderList,OutSt,OutHTML),
	opdeclsDisp(OutSt,OutHTML),
	libfilesDisp(OutSt,SuiteName,OutHTML),
	uncalleds(InOrderList,CalledList,OutSt,OutHTML),
 	undefs(DependsOnList,CalledList,OutSt,OutHTML).

	/*-----------------------------------------------------
	 |	MAIN PROCESSING LOOP:
  	 *------------------------------------------------------*/
cross_ref(OMs, NOMs, File, AddlFs, InS)
	:-
	read_term(InS,Item, []),
	disp_cross_ref(Item, OMs, NOMs, File, AddlFs, InS).

disp_cross_ref(end_of_file, OMs, OMs, File, [], InS)
	:-!.

disp_cross_ref(Item,OMs,NOMs,File, AddlFs, InS)
	:-
	act_cross_ref(Item, OMs, NextOMs, File, AddlFs, AddlFsTail),
	!,
	put_code(0'.),flush_output,
	cross_ref(NextOMs,NOMs,File, AddlFsTail, InS).

	%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% DISPATCH FOR MAIN LOOP:
	%%%%%%%%%%%%%%%%%%%%%%%%%%

	% Goal in file:
act_cross_ref(':-'(Goal),OMs,OMs,File,AFs,AFsT) 
	:-!,
	process_goal(Goal,OMs,File,AFs,AFsT).

	%% Clause with body:
act_cross_ref((Head :- Body),OMs,OMs,File,AFs,AFs) 
	:-!,
 	functor(Head, F, N),
	OMs = [Mod | _], 
 	rcrd(inc_clause_cnt(F,N,Mod,File)),
 	analyze_clause(Body, F,N,Mod,File).

	%% DCG rule:
act_cross_ref((Head --> Body),OMs,OMs,File,AFs,AFs) 
	:-!,
	dcg_expand((Head --> Body), XDCG),
	act_cross_ref(XDCG,OMs,OMs,File,AFs,AFs).

	%% module declarations:
act_cross_ref((module Mod),OMs,[Mod | OMs],File,AFs,AFs)
	:-!,
 	rcrd(module(Mod,File)).

act_cross_ref((endmod),[_ | OMs], OMs,File,AFs,AFs)
	:-!.

act_cross_ref((export A),OMs,OMs,File,AFs,AFs)
	:-!,
	OMs = [Mod | _],
	record_exports(A, Mod, File).

act_cross_ref((use A), OMs, OMs,File,AFs,AFs)
	:-!,
	OMs = [Mod | _],
	record_uses(A, Mod, File).
	
	%% A Fact (default):
act_cross_ref(Fact,OMs,OMs,File,AFs,AFs) 
	:-
 	functor(Fact, F, N),
	OMs = [Mod | _],
 	rcrd(fact(F,N,Mod,File)).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Processing: Goal in file:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_goal(Goal,OMs,File,AFs,AFsT)
	:-
	Goal = [_ | _],
	!,
	open_tail(Goal, AFs, AFsT).

process_goal((Goal_A, Goal_B), OMs,File,AFs,AFsT)
	:-!,
	process_goal(Goal_A, OMs,File,AFs,InterAFs),
	process_goal(Goal_B, OMs,File,InterAFs,AFsT).

process_goal(consult(F), OMs, File, [F | AFsT], AFsT)
	:-!.

process_goal(Goal, OMs,File,AFs,AFs)
	:-
	functor(Goal,op,3),
	!,
	call(Goal),
	getMiscInfo(MIS),
	accessMI(op_decls, MIS, OpDecls),
	setMI(op_decls, MIS, [Goal+File | OpDecls]),
	assert(op_cref(OMs,Goal)).

process_goal(make_gv(Tag), OMs,File,AFs,AFs)
	:-!,
	catenate(get,Tag,GetCall), functor(GetHead,GetCall,1),
	act_cross_ref((GetHead :- gv_get(_,_)),OMs,OMs,File,AFs,AFs),
	catenate(set,Tag,SetCall), functor(SetHead,SetCall,1),
	act_cross_ref((SetHead :- gv_set(_,_)),OMs,OMs,File,AFs,AFs).
% Need to handle calls after the make_gv call:: :-make_gv('CallsTree'),avl_create(T),setCallsTree(T).

process_goal(dynamic(Tag), OMs,File,AFs,AFs)
	:-!,
	Tag = F/N,
	OMs = [Mod | _],
 	rcrd(dynamic(F,N,Mod,File)).

% Need to handle module_closure

process_goal(Goal, OMs,File,AFs,AFs).

open_tail([], AFs, AFs).
open_tail([H | T], [H | RO], AFsT)
	:-
	open_tail(T, RO, AFsT).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Recording export declarations:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

record_exports([], _, _).
record_exports([A | T], Mod, File)
	:-!,
	rcrd(export(Mod,A,File)),
	record_exports(T, Mod, File).

record_exports(A, Mod, File)
	:-
	rcrd(export(Mod,A,File)).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Recording use declarations:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

record_uses([], Mod, File).
record_uses([A | T], Mod, File)
	:-!,
 	rcrd(use(Mod,A,File)),
	record_uses(T, Mod, File).

record_uses(A, Mod, File)
	:-
 	rcrd(use(Mod,A,File)).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Analyzing clause bodies:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% ---------------------------
	%% -- Skip uninstantiated goals:
analyze_clause(X,_,_,_,_)
	:- var(X),!.

	%% ---------------------------
	%% -- Disjunction:
analyze_clause(  ( (SG1; SG2); H), F, N, Mod,CurFile) 
	:-!,
 	analyze_clause(SG1, F, N, Mod,CurFile),
 	analyze_clause(SG2, F, N, Mod,CurFile),
 	analyze_clause(H, F, N, Mod,CurFile).

analyze_clause( (G; H), F, N, Mod,CurFile) 
	:-!,
 	analyze_clause(G, F, N, Mod,CurFile),
 	analyze_clause(H, F, N, Mod,CurFile).

	%% ---------------------------
	%% -- Conditional:
analyze_clause(  (Head -> If; Else), F, N, Mod,CurFile) 
	:-!,
 	analyze_clause(Head, F, N, Mod,CurFile),
 	analyze_clause(If,   F, N, Mod,CurFile),
 	analyze_clause(Else, F, N, Mod,CurFile).

analyze_clause(  (Head -> If), F, N, Mod,CurFile) 
	:-!,
 	analyze_clause(Head, F, N, Mod,CurFile),
 	analyze_clause(If,   F, N, Mod,CurFile).

	%% ---------------------------
	%% -- Conjunction:
analyze_clause(  ( (SG1, SG2), H), F, N, Mod,CurFile) 
	:-!,
 	analyze_clause(SG1, F, N, Mod,CurFile),
 	analyze_clause(SG2, F, N, Mod,CurFile),
 	analyze_clause(H, F, N, Mod,CurFile).

analyze_clause( (G, H), F, N, Mod,CurFile) 
	:-!,
 	analyze_clause(G, F, N, Mod,CurFile),
 	analyze_clause(H, F, N, Mod,CurFile).

	%% ---------------------------
	%% -- Special Builtins - setof,setOf,bagof bagOf:
analyze_clause(setOf(_, G, _), F, N, Mod,CurFile) 
	:-
 	nonvar(G), !,
 	rcrd(rule(F,N,setOf,3,Mod,CurFile)), 
 	analyze_clause_special(G, F, N, Mod,CurFile).

analyze_clause(setOf(_, G, _), F, N, Mod,CurFile) 
	:-
 	rcrd(rule(F,N,setOf,3,Mod,CurFile)). 

analyze_clause(setof(_, G, _), F, N, Mod,CurFile) 
	:-
 	nonvar(G),
 	analyze_clause_special(G, F, N, Mod,CurFile).

analyze_clause(bagOf(_, G, _), F, N, Mod,CurFile) 
	:-
 	nonvar(G), !,
 	rcrd(rule(F,N,bagOf,3,Mod,CurFile)), 
 	analyze_clause_special(G, F, N, Mod,CurFile).

analyze_clause(bagOf(_, G, _), F, N, Mod,CurFile) 
	:-
 	rcrd(rule(F,N,bagOf,3,Mod,CurFile)). 

analyze_clause(bagof(_, G, _), F, N, Mod,CurFile) 
	:-
 	nonvar(G),
 	analyze_clause_special(G, F, N, Mod,CurFile).

analyze_clause_special(X^G, F, N, Mod,CurFile)
	:- !,
 	analyze_clause(G,F,N,Mod,CurFile).

analyze_clause_special(G, F, N, Mod,CurFile)
	:- 
 	analyze_clause(G,F,N,Mod,CurFile).

	%% ---------------------------
	%% -- call:
analyze_clause(call(G), F, N, Mod,CurFile) 
	:-
 	nonvar(G),
 	analyze_clause(G, F, N, Mod,CurFile).

	%% ---------------------------
	%% -- assert:
analyze_clause(assertz(G), F, N, Mod,CurFile) 
	:-
	handle_assert(G, F, N, Mod, CurFile).

analyze_clause(asserta(G), F, N, Mod,CurFile) 
	:-
	handle_assert(G, F, N, Mod, CurFile).

analyze_clause(assert(G), F, N, Mod,CurFile) 
	:-
	handle_assert(G, F, N, Mod, CurFile).

handle_assert(G, F, N, Mod, CurFile)
	:-
 	nonvar(G),
 	functor(G,FG,NG),
	FG \= ':-',
	!,
 	rcrd(assert(FG,NG, F,N, Mod,CurFile)).

handle_assert((Head :- Body), F, N, Mod, CurFile)
	:-
	nonvar(Head),
	!,
 	functor(Head,FHead,NHead),
 	rcrd(assertRule(FHead,NHead, F,N, Mod,CurFile)).

handle_assert((Head :- Body), F, N, Mod, CurFile)
	:-
 	rcrd(assertVHeadRule(Body, F,N, Mod,CurFile)).

	%% ---------------------------
	%% -- Explicit import from a module:
analyze_clause(Module:not(G), F, N, Mod,CurFile) 
	:-!,
	functor(G, FG, NG),
	rcrd(expl_import(ToMod,FG,NG,FromModule,CurFile)),
 	rcrd(inc_called_cnt(FG,NG,F/N,Mod,CurFile)),
	rcrd(rule(F, N, Module:not(FG), NG, Mod,CurFile)).

analyze_clause(Module:G, F, N, Mod,CurFile) 
	:-
	functor(G, FG, NG),
	rcrd(expl_import(Mod,FG,NG,Module,CurFile)),
 	rcrd(inc_called_cnt(FG,NG,F/N,Mod,CurFile)),
	rcrd(rule(F, N, Module:FG, NG, Mod,CurFile)).

	%% ---------------------------
	%% -- Negation:
analyze_clause(not(Module:G), F, N, Mod,CurFile)
	:-!,
	functor(G, FG, NG),
	rcrd(expl_import(Mod,FG,NG,Module,CurFile)),
 	rcrd(inc_called_cnt(FG,NG,F/N,Mod,CurFile)),
	rcrd(rule(F, N, Module:not(FG), NG, Mod,CurFile)).

analyze_clause(not(G), F, N, Mod,CurFile)
	:-
	functor(G, FG, NG),
	rcrd(rule(F, N, not(FG),NG, Mod,CurFile)).
 
	%% ---------------------------
	%% -- Default (ordinary call):
analyze_clause(G, F, N, Mod,CurFile) 
	:-
	functor(G, FG, NG),
	rcrd(rule(F, N, FG, NG, Mod,CurFile)),
 	rcrd(inc_called_cnt(FG,NG,F/N,Mod,CurFile)).


	/*-------------------------------------------------
	 |   recording needed info
	 *------------------------------------------------*/

rcrd(file(File))
	:-
	getMiscInfo(MIS),
	accessMI(files, MIS, Files),
	(dmember(File, Files) ->
		true
		;
		setMI(files, MIS, [File | Files])
	).

:-dynamic(excluded_file/1).
:-dynamic(excluded_mod/1).

excluded_head(F, N, Mod, CurFile)
	:-
	(excluded_file(CurFile) ; excluded_mod(Mod)),
	!.

excluded_call(FG, NG)
	:-
	all_procedures(MG, FG, NG, _),
	excluded_mod(MG),
	!.

	%% default:
excluded_mod(builtins).
excluded_mod(sio).

rcrd(inc_clause_cnt(F,N,Mod,File))
	:-
	getCallsTree(CurCallsTree),
	avl_insert(F/N, inc_clause_cnt(F,N,Mod,File),CurCallsTree,NewCallsTree),
	setCallsTree(NewCallsTree).

rcrd(inc_called_cnt(F,N,_,_,_))
	:-
	excluded_call(F, N),
	!.

rcrd(inc_called_cnt(F,N,CalledBy,Mod,File))
	:-
	getCallsTree(CurCallsTree),
	avl_insert(F/N, inc_called_cnt(F,N,CalledBy,Mod,File),CurCallsTree,NewCallsTree),
	setCallsTree(NewCallsTree).

rcrd(rule(F, N, FG, NG, Mod,CurFile)) 
	:-
	excluded_head(F, N, Mod, CurFile),
	!.

rcrd(rule(F, N, FG, NG, Mod,CurFile)) 
	:-
	excluded_call(FG, NG),
	!.

rcrd(rule(F, N, FG, NG, Mod,CurFile)) 
	:-
	getCallsTree(CurCallsTree),
	avl_insert(F/N, calls(F,N,FG,NG, Mod,CurFile),CurCallsTree,NewCallsTree),
	setCallsTree(NewCallsTree).

rcrd(fact(F, N, Mod,CurFile)) 
	:-
	excluded_head(F, N, Mod, CurFile),
	!.

rcrd(fact(F,N, Mod,CurFile)) 
	:-
	getCallsTree(CurCallsTree),
	avl_insert(F/N, fact(F,N,Mod,CurFile),CurCallsTree,NewCallsTree),
	setCallsTree(NewCallsTree).

rcrd(dynamic(F,N,Mod,CurFile)) 
	:-
	getCallsTree(CurCallsTree),
	avl_insert(F/N, dynamic(F,N,Mod,CurFile),CurCallsTree,NewCallsTree),
	setCallsTree(NewCallsTree).

rcrd(export(FromMod,P/N,CurFile))
	:-
	getCallsTree(CurCallsTree),
	avl_insert(P/N, export(P,N,FromMod,CurFile),CurCallsTree,NewCallsTree),
	setCallsTree(NewCallsTree),
	getMiscInfo(MIS),
	accessMI(mods_exp_preds, MIS, CurFromModExports),
	enter_in(mods_exp_preds, CurFromModExports, FromMod, MIS, P/N).

	% at ~line 605: rcrd(expl_import(ToMod,FG,NG,FromModule,CurFile)),
rcrd(expl_import(ToMod,P,N,FromModule,CurFile) )
	:-
	getCallsTree(CurCallsTree),
	avl_insert(P/N, import(P,N,ToMod,FromModule,CurFile),CurCallsTree,NewCallsTree),
	setCallsTree(NewCallsTree),
	getMiscInfo(MIS),
	accessMI(mods_imp_preds, MIS, CurToModImports),
	enter_in(mods_imp_preds, CurToModImports, ToMod, MIS, import(P,N,FromModule,CurFile)).

rcrd(module(Mod,CurFile))
	:-
	getMiscInfo(MIS),
	accessMI(mods, MIS, Mods),
	accessMI(mods_files, MIS, ModsFiles),
	(dmember(Mod, Mods) ->
		true
		;
		setMI(mods, MIS, [Mod | Mods])
	),
	enter_in(mods_files, ModsFiles, Mod, MIS, CurFile),
	accessMI(files_mods, MIS, FilesMods),
	enter_in(files_mods, FilesMods, CurFile, MIS, Mod).

rcrd(assert(FG,NG, F,N, Mod,CurFile)) 
	:-
	getCallsTree(CurCallsTree),
	avl_insert(FG/NG, assert(FG,NG,F,N, Mod,CurFile),CurCallsTree,NewCallsTree),
	setCallsTree(NewCallsTree).

rcrd(assertRule(FH,NH, F,N, Mod,CurFile))
	:-
	getCallsTree(CurCallsTree),
	avl_insert(FH/NH, assertRule(FH,NH,F,N, Mod,CurFile),CurCallsTree,NewCallsTree),
	setCallsTree(NewCallsTree).

rcrd(assertVHeadRule(Body, F,N, Mod,CurFile))
	:-
	getCallsTree(CurCallsTree),
	avl_insert(varHeadRule/0, assertVHeadRule(Body,F,N, Mod,CurFile),CurCallsTree,NewCallsTree),
	setCallsTree(NewCallsTree).

	/*----------------------------------------------------------------
	 |	NOTE: -- validate when switching to avl trees:
	 |	If Module  uses predicates from other modules,
	 |	'asserta' (instead of 'assert') is used to record the modules 
	 |	used by Mod. Predicates called by Module but not defined
	 |	inside Module will be searched for using the recorded info
	 |	(searched in the correct order).
	 *----------------------------------------------------------------*/

rcrd(use(Mod,Module,CurFile))
	:-
	getMiscInfo(MIS),
	accessMI(mods_use, MIS, ModUse),
	    %% ModUse: [ [UsingMod+[UsedMod1,...]
	enter_in(mods_use, ModUse, Mod, MIS, Module).

	% enter_in(mods_exp_preds, CurFromModExports, FromMod, MIS, export(Module,P,N)).
	% enter_in(mods_imp_preds, CurToModImports, ToMod, MIS, import(FG,NG,FromModule,CurFile)).
enter_in(Tag, PlusList, Mod, MIS, Item)
	:-
	locm(PlusList, Mod, Entry),
	!,
	Entry = Mod+ItemList,
	(dmember(Item, ItemList) ->
		true
		;
		(Tag=end(_) -> 
			append(ItemList, [Item], NewList)
			;
			NewList = [Item | ItemList]
		),
		mangle(2,Entry,NewList)
	).

enter_in(end(Tag), PlusList, Mod, MIS, Item)
	:-!,
	setMI(Tag, MIS, [Mod+[Item] | PlusList]).
enter_in(Tag, PlusList, Mod, MIS, Item)
	:-
	setMI(Tag, MIS, [Mod+[Item] | PlusList]).


locm([Entry | _], Mod, Entry)
	:-
	Entry = Mod+_,
	!.
locm([_ | Mods], Mod, Entry)
	:-
	locm(Mods, Mod, Entry).


	/*-----------excluded predicates---------------------------------*/
excluded(functor,3).
excluded(arg,3).
excluded(';',2).
excluded(see,1).
excluded(read,1).
excluded(assert,1).
excluded('=',2).

	/*----------------------------------------------------------
	 |	group: lists each defined predicate together with 
	 |	       all the predicates it eventually calls.
	 *----------------------------------------------------------*/

group(CallsTree,DependsOnList,OutSt,OutHTML) 
	:-
	getMiscInfo(MIS),
	inorder_defined(CallsTree, CallsTree, MIS, [], DependsOnList),
	dependency_header(OutSt,OutHTML),
	cref_out(DependsOnList,CallsTree,NoDefList,OutSt,OutHTML),
	assert(nodefList(NoDefList)),
	nl(OutSt).

	/*------------- modules appearing --------------------------*/

modules(CallsTree,OutS,OutHTML)
	:-
	getMiscInfo(MIS),
	accessMI(mods, MIS, RawModules),
	sort(RawModules, Modules),
	getImplicitImportsInfo(MIS, Modules, ModsImports),

	(Modules = [] -> 
 		printf(OutS,'%t\n',['\tNo Modules Defined']),
 		printf(OutHTML,'<h3>%t</h3>\n',['\tNo Modules Defined'])
		;
   		printf(OutS,' ==============================\n',[]),
   		printf(OutS,'     Modules:\n',[]),
   		printf(OutS,' ==============================\n\n',[]),

   		printf(OutHTML,'<hr/>\n',[]),
   		printf(OutHTML,'<h2>Modules</h2>\n',[]),
		report_modules(Modules, MIS, ModsImports, OutS, OutHTML)
	).

report_modules([], _, _, OutS, OutHTML)
	:-
	nl(OutS).
report_modules([M | Modules], MIS, ModsImports, OutS, OutHTML)
	:-
	do_mod_report(M, MIS, ModsImports, OutS, OutHTML),
	report_modules(Modules, MIS, ModsImports, OutS, OutHTML).

do_mod_report(M, MIS, ModsImports, OutS, OutHTML)
	:-
   	printf(OutS,'%t: \n',[M]),
   	printf(OutHTML,'<h3>%t:</h3>\n',[M]),
   	printf(OutHTML,'<div style="margin-left:3em">\n',[]),
	mod_rpt_detail(mods_files, M, MIS, OutS, OutHTML),
	mod_rpt_detail(mods_use, M, MIS, OutS, OutHTML),
	mod_rpt_detail(mods_exp_preds, M, MIS, OutS, OutHTML),

	(dmember(M+Imps, ModsImports) ->
   		printf(OutS, '\tImplicit Imports (via use/export):\n', []),
   		printf(OutHTML, '<br><b>Implicit Imports (via use/export):</b>\n', []),
   		printf(OutHTML,'<div style="margin-left:3em;margin-top:-1em;">\n',[]),
		listImports(Imps, OutS, OutHTML)
		;
   		printf(OutS, '\tImplicit Imports: []\n', []),
   		printf(OutHTML, '<br><b>Implicit Imports: </b>[]\n', [])
	),
	mod_rpt_detail(mods_imp_preds, M, MIS, OutS, OutHTML),
   	printf(OutHTML,'</div>\n',[]).

mod_rpt_detail(Tag, M, MIS, OutS, OutHTML)
	:-
        accessMI(Tag, MIS, TagInfoList), 
	(dmember(M+TagInfo, TagInfoList) -> true ; TagInfo = []),
	display_tag_info(Tag, TagInfo, M, OutS, OutHTML).

display_tag_info(mods_files, TagInfo, M, OutS, OutHTML)
	:-!,
   	printf(OutS, '\tFiles:\t[', []),
   	printf(OutHTML, '<b>Files:&nbsp;</b>[', []),
	sort(TagInfo, STagInfo),
	listFiles(STagInfo, OutS, OutHTML).

display_tag_info(mods_imp_preds, TagInfo, M, OutS, OutHTML)
	:-!,
   	printf(OutS, '\tExplicit Imports: [', []),
   	printf(OutHTML, '<br><b>Explicit Imports: </b>[', []),
	listExplicitImports(TagInfo, OutS, OutHTML).

display_tag_info(Tag, TagInfo, M, OutS, OutHTML)
	:-
	tag_succeed_string(Tag, SucceedString),
   	printf(OutS, SucceedString, [TagInfo]),
	html_tag_succeed_string(Tag, HTMLSucceedString),
   	printf(OutHTML, HTMLSucceedString, [TagInfo]).
	
tag_succeed_string(mods_use, '\tUses:\t  %t\n').
html_tag_succeed_string(mods_use, '<br><b>Uses: </b> %t\n').

tag_succeed_string(mods_exp_preds, '\tExported: %t\n').
html_tag_succeed_string(mods_exp_preds, '<br><b>Exported: </b> %t\n').

tag_succeed_string(mods_imp_preds, '\tExplicit imports: %t\n').
html_tag_succeed_string(mods_imp_preds, '<br><b>Explicit imports: </b>%t\n').

listFiles([], OutS, OutHTML)
	:-
   	printf(OutS, ']\n', []),
   	printf(OutHTML, ']</h4>\n', []).
listFiles([RawFile | TagInfo], OutS, OutHTML)
	:-
	pathPlusFile(P, File, RawFile),
   	printf(OutS, '%t ', [File]),
   	printf(OutHTML, '%t ', [File]),
	listFiles(TagInfo, OutS, OutHTML).

getImplicitImportsInfo(MIS, Modules, ModsImports)
	:-
        accessMI(mods_files, MIS, ModsFiles), 
        accessMI(mods_use, MIS,  ModsUses),
        accessMI(mods_exp_preds, MIS,  ModsExports),
	recurse_imports(Modules, ModsUses, ModsExports, ModsImports).

recurse_imports([], ModsUses, ModsExports, []).
recurse_imports([M | Modules], ModsUses, ModsExports, [M+MImports | ModsImports])
	:-
	dmember(M+MUsesList, ModsUses),
	getExps(MUsesList, ModsExports, MImports),
	recurse_imports(Modules, ModsUses, ModsExports, ModsImports).
recurse_imports([M | Modules], ModsUses, ModsExports, ModsImports)
	:-
	recurse_imports(Modules, ModsUses, ModsExports, ModsImports).

getExps([], _, []).
getExps([UM | MUsesList], ModsExports, [XPs+UM | MImports])
	:-
	dmember(UM+XPs, ModsExports),
	!,
	getExps(MUsesList, ModsExports, MImports).
getExps([_ | MUsesList], ModsExports, MImports)
	:-
	getExps(MUsesList, ModsExports, MImports).

listImports([], _, OutHTML)
	:-
   	printf(OutHTML,'</div>\n',[]).
listImports([Preds+XMod | Imps], OutS, OutHTML)
	:-
   	printf(OutS, '\t    %t from %t\n', [Preds,XMod]),
   	printf(OutHTML, '<br>%t from %t\n', [Preds,XMod]),
	listImports(Imps, OutS, OutHTML).

listExplicitImports([], OutS, OutHTML) 
	:-
   	printf(OutS, ']\n\n', []),
   	printf(OutHTML, ']\n\n', []).
	
listExplicitImports([ImpInfo | ImpInfos], OutS, OutHTML)
	:-
	ImpInfo = import(Pred, Arity, FromMod, File),
   	printf(OutS, ' %t:%t/%t ', [FromMod,Pred,Arity]),
   	printf(OutHTML, ' %t:%t/%t ', [FromMod,Pred,Arity]),
	listExplicitImports(TagInfo, OutS, OutHTML).

	/*------------ Asserted Predicates -------------------------*/

asserteds(CallsTree,InOrderList,OutS,OutHTML)
	:-
	extractAsserteds(InOrderList, AssertedList),
 	printf(OutS,'\n ======================================\n',[]),
 	printf(OutHTML,'<hr>\n',[]),
 	(AssertedList = [] ->
 		printf(OutS,'%t\n',['\tNo Asserted Facts']),
 		printf(OutHTML,'<h2>%t</h2>\n',['No Asserted Facts'])
		;
 		printf(OutS,'%t\n',['    Asserted Facts (P/N in [Module + File])']),
 		printf(OutS,' ======================================\n\n',[]),
 		printf(OutHTML,'%t\n',['<h2>Asserted Facts (P/N in [Module + File])</h2>']),
   		printf(OutHTML,'<div style="margin-left:3em;margin-top:-1em;">\n',[]),
		output_asserted_facts(AssertedList, OutS,OutHTML)
	),
	handleAssertedRules(CallsTree,InOrderList,OutS,OutHTML),
   	printf(OutHTML,'</div>\n',[]).

extractAsserteds([], []).
extractAsserteds([Key-Data | RestInOrderList], AssertedList)
	:-
	accessCRF(whereasserted,Data,WhereAsserted),
	WhereAsserted \= [],
	!,
	accessCRF(dynamicdecl,Data,DynamicList),
	(DynamicList = [] ->
		DynNote = '\tNo dynamic decl'
		;
		DynNote = DynamicList
	),
	sort(WhereAsserted, SortedWhereAsserted),
	AssertedList=[Key-SortedWhereAsserted-DynNote | RestAssertedList],
	extractAsserteds(RestInOrderList, RestAssertedList).

extractAsserteds([Key-Data | RestInOrderList], AssertedList)
	:-
	accessCRF(dynamicdecl,Data,DynamicList),
	DynamicList \= [],
	!,
	sort(DynamicList, SortedDynamicList),
	AssertedList=[Key-' dynamic - implicit assert' - SortedDynamicList | RestAssertedList],
	extractAsserteds(RestInOrderList, RestAssertedList).

extractAsserteds([Key-Data | RestInOrderList], AssertedList)
	:-
	extractAsserteds(RestInOrderList, AssertedList).

output_asserted_facts([],_,_).
output_asserted_facts([AA | AssertedList], OutS,OutHTML)
	:-
	out_asserted(AA, OutS,OutHTML),
	output_asserted_facts(AssertedList, OutS,OutHTML).

out_asserted(AA-D-D, OutS,OutHTML)
	:-
	D \= [],
	!,
	printf(OutS,'    %t - %t\n',[AA,D]),
	printf(OutHTML,'<br><b>%t</b> - %t\n',[AA,D]).

out_asserted(AA-D-(dynamic - 'implicit assert'), OutS,OutHTML)
	:-
	D \= [],
	!,
	printf(OutS,'    %t - %t\n',[AA,D]),
	printf(OutHTML,'<br><b>%t</b> - %t\n',[AA,D]).

out_asserted(AA-D, OutS,OutHTML)
	:-
	D \= [],
	!,
	printf(OutS,'    %t - %t\n',[AA,D]),
	printf(OutHTML,'<br><b>%t</b> - %t\n',[AA,D]).

out_asserted(AA, OutS,OutHTML)
	:-
	printf(OutS,'    %t\n',[AA]),
	printf(OutHTML,'<br><b>%t</b>\n',[AA]).

handleAssertedRules(CallsTree,InOrderList,OutS,OutHTML)
	:-
	extractAssertedRules(InOrderList, AssertedRulesList),
	displayAssertedRules(AssertedRulesList, OutS,OutHTML).

displayAssertedRules([], OutS,OutHTML)
	:-!,
	printf(OutS, '\n        No Asserted Normal Rules\n', []).

displayAssertedRules(RulesList, OutS,OutHTML)
	:-!,
	printf(OutS, '        -------------------------------\n', []),
	printf(OutS, '        Asserted Rules\n', []),
	printf(OutS, '        -------------------------------\n', []),
	outAssertedRules(RulesList, OutS,OutHTML).


extractAssertedRules([], []).
extractAssertedRules([Key-Data | RestInOrderList], AssertedRulesList)
	:-
	accessCRF(ruleasserted,Data,WhereRulesAsserted),
	WhereRulesAsserted \= [],
	!,
	sort(WhereRulesAsserted, SortedWhereRulesAsserted),
	AssertedRulesList=[Key-SortedWhereRulesAsserted | RestAssertedRulesList],
	extractAssertedRules(RestInOrderList, RestAssertedRulesList).

extractAssertedRules([_ | RestInOrderList], AssertedRulesList)
	:-
	extractAssertedRules(RestInOrderList, AssertedRulesList).

	%% [foo/1-[user+b3.pro]]
	%% foo/1 - [virtualrule/1+user+b3.pro]
outAssertedRules([], OutS,OutHTML).

outAssertedRules([varHeadRule/0-VarHeadBodies | RestRulesList], OutS,OutHTML)
	:-!,
	outDispVRuleHeadBodies(VarHeadBodies, OutS,OutHTML),
	outAssertedRules(RestRulesList, OutS,OutHTML).

outAssertedRules([RF/RN-[PF/PN+Mod+FilesList] | RestRulesList], OutS,OutHTML)
	:-
	printf(OutS, '        + %t/%t in %t/%t -  %t+%t\n', [RF, RN, PF, PN, Mod,FilesList]),
	printf(OutHTML, '<br>&#x25B8; %t/%t in %t/%t -  %t--%t\n', [RF, RN, PF, PN, Mod,FilesList]),
	outAssertedRules(RestRulesList, OutS,OutHTML).


outDispVRuleHeadBodies([], OutS,OutHTML)
	:-
	printf(OutS, '    ++++    \n', []).
%	printf(OutHTML, '<br>++++    \n', []).
outDispVRuleHeadBodies([VHB | VarHeadBodies], OutS,OutHTML)
	:-
	oDVrH(VHB, OutS, OutHTML),
	outDispVRuleHeadBodies(VarHeadBodies, OutS, OutHTML).

oDVrH(vH=Body+F/N+Mod+FilesList, OutS,OutHTML)
	:-
	nonvar(Body),
	!,
	printf(OutS, '        + variable rule head in %t/%t -  %t+%t\n          Rule Body: %t\n', [F, N, Mod,FilesList,Body]),
	printf(OutHTML, '<br>&#x25B8; variable rule head in %t/%t -  %t&nbsp;+&nbsp;%t\n          Rule Body: %t\n', [F, N, Mod,FilesList,Body]).

oDVrH(vH=Body+F/N+Mod+FilesList, OutS,OutHTML)
	:-
	printf(OutS, '        + variable rule head in %t/%t -  %t+%t\n           Rule Body also variable\n', [F, N, Mod,FilesList,Body]),
	printf(OutHTML, '<br>&#x25B8; variable rule head in %t/%t -  %t&nbsp;+&nbsp;%t\n           Rule Body also variable\n', [F, N, Mod,FilesList,Body]).


	/*------------- operator declarations --------------------------*/

opdeclsDisp(OutS,OutHTML)
	:-
	getMiscInfo(MIS),
	accessMI(op_decls, MIS, OpDecls),
 	printf(OutS,'\n ======================================\n',[]),
 	printf(OutHTML,'<hr>\n',[]),
 	(OpDecls = [] ->
 		printf(OutS,'%t\n',['\tNo Operator Declarations']),
 		printf(OutHTML,'<h2>%t</h2>\n',['\tNo Operator Declarations'])
		;
 		printf(OutS,'%t\n',['\tOperator Declarations']),
 		printf(OutS,' ======================================\n\n',[]),
 		printf(OutHTML,'<h2>%t</h2>\n',['\tOperator Declarations']),
		output_opdecls(OpDecls, OutS,OutHTML)
	).

output_opdecls([], OutS,OutHTML).
output_opdecls([OpDec + File | RestOpDecls], OutS,OutHTML)
	:-
 	printf(OutS,'     :- %t in %t\n',[OpDec,File]),
 	printf(OutHTML,'<br>:- %t in %t\n',[OpDec,File]),
	output_opdecls(RestOpDecls, OutS,OutHTML).


	/*------------- library predicates used  --------------------------*/

libfilesDisp(OutS, SuiteName,OutHTML)
	:-
	getMiscInfo(MIS),
       	accessMI(lib_files, MIS,  LibFilesList),
 	printf(OutS,'\n ======================================\n',[]),
 	printf(OutHTML,'<Hr>\n',[]),
 	(LibFilesList = [] ->
 		printf(OutS,'\t%t\n',['No Library Files Used']),
 		printf(OutHTML,'<h2>%t</h2>\n',['No Library Files Used'])
		;
 		printf(OutS,'\t%t\n',['Library Files Used']),
 		printf(OutS,' ======================================\n\n',[]),
 		printf(OutHTML,'<h2>%t</h2>\n',['Library Files Used']),
		sort(LibFilesList, SortedLibFilesList), 
 		printf(OutHTML,'<div style="margin-left:3em;margin-top:-2em;">\n',[]),
		showLibFilesList(SortedLibFilesList, OutS,OutHTML),
 		printf(OutHTML,'</div>\n',[])
	),
	abolish(lib_files_used/2),
	assert(lib_files_used(SuiteName, LibFilesList)).
	
	
showLibFilesList([], OutS,OutHTML).
showLibFilesList([ lf(P,N,XM,LF) | RestLibFilesList], OutS,OutHTML)
	:-
 	printf(OutS,'     %t/%t exported from %t in %t\n',[P,N,XM,LF]),
 	printf(OutHTML,'<br><b>%t/%t</b> exported from %t in %t\n',[P,N,XM,LF]),
	showLibFilesList(LibFilesList, OutS,OutHTML).


	/*------------- uncalled predicates --------------------------*/
uncalleds(InOrderList,CalledList,OutS,OutHTML)
	:-
	extractUncalleds(InOrderList, UncalledList, CalledList),
 	printf(OutS,'\n ======================================\n',[]),
 	printf(OutHTML,'<hr>\n',[]),
 	(UncalledList = [] ->
 		printf(OutS,'%t\n',['\tNo Uncalled Predicates']),
 		printf(OutHTML,'<h2>%t</h2>\n',['\tNo Uncalled Predicates'])
		;
 		printf(OutS,'%t\n',['    Uncalled (toplevel) Predicates (P/N in [Module + Files])']),
 		printf(OutS,' ======================================\n\n',[]),
 		printf(OutHTML,'<h2>%t</h2>\n',['    Uncalled (toplevel) Predicates (P/N in [Module + Files])']),
 		printf(OutHTML,'<div style="margin-left:3em;margin-top:-2em;">\n',[]),
		output_uncalleds(UncalledList, OutS,OutHTML),
 		printf(OutHTML,'</div>\n',[])
	).

extractUncalleds([], [], []).
extractUncalleds([Key-Data | InOrderList], [Key-[Mod+Files] | UncalledList], CalledList)
	:-
	accessCRF(calledby,Data, CalledBy),
	CalledBy = [],
	!,
	accessCRF(mod,Data, Mod),
	accessCRF(files,Data, Files),
	extractUncalleds(InOrderList, UncalledList, CalledList).
extractUncalleds([Key-Data  | InOrderList], UncalledList, [Key-Data | CalledList])
	:-
	extractUncalleds(InOrderList, UncalledList, CalledList).

output_uncalleds([], _,_).
output_uncalleds([Unc | UncalledList], OutS,OutHTML)
	:-
	out_unc(Unc, OutS,OutHTML),
	output_uncalleds(UncalledList, OutS,OutHTML).

out_unc(PN-[Mod+FList], OutS,OutHTML)
	:-!,
 	printf(OutS,'    %t - [%t+[', [PN,Mod]),
 	printf(OutHTML,'<br><b>%t</b> - [%t+[', [PN,Mod]),
	sort(FList, SFList),
	listFiles(SFList, OutS,OutHTML).

	/*-------------------------------------------------------
 	 |	undefs
	 *-------------------------------------------------------*/

	%% DependsOnList:: '$depends_on'(P,N,Mod,UsesList,Files),
undefs(DependsOnList,CalledList,OutS,OutHTML)
	:-
 	printf(OutS,'\n ======================================\n',[]),
 	printf(OutHTML,'<hr>\n',[]),
	(nodefList(NoDefList) ->
 		printf(OutS,'%t\n',['    Undefined Predicates (P/N in [Module + Files])']),
 		printf(OutHTML,'<H2>%t</h2>\n',['    Undefined Predicates (P/N in [Module + Files])']),
 		printf(OutS,' ======================================\n\n',[]),
 		printf(OutHTML,'<div style="margin-left:3em;margin-top:-1.5em;">\n',[]),
		output_nodefs(NoDefList, OutS,OutHTML),
 		printf(OutHTML,'</div>\n',[])
		;
 		printf(OutS,'%t\n',['\tNo Undefined Predicates'])
	),
 	printf(OutS,'\n ======================================\n',[]).
	

output_nodefs([],_,OutHTML).
output_nodefs([AA | NoDefList], OutS,OutHTML)
	:-
	out_nodef(AA, OutS,OutHTML),
	output_nodefs(NoDefList, OutS,OutHTML).

out_nodef(FN+(Mod+FList), OutS,OutHTML)
	:-
 	printf(OutS,'    %t - [%t+[', [FN,Mod]),
 	printf(OutHTML,'<br><b>%t</b> - [%t+[', [FN,Mod]),
	sort(FList, SFList),
	listFiles(SFList, OutS,OutHTML).


%%%% ============== OUTPUT PREDICATES =========================

%%--------------- Dependency Header -----------------------------

dependency_header(OutS,OutHTML)
	:-
   printf(OutS,' ==============================\n',[]),
   printf(OutS,' Predicate Dependencies & Calls:\n',[]),
   printf(OutS,' ==============================\n\n',[]),

   printf(OutHTML,'<hr>\n',[]),
   printf(OutHTML,'<h2>Predicate Dependencies & Calls:</h2>\n',[]).

gen_file_header(OutSt,cref_suite-SuiteName,TargetFile)
	:-
 	printf(OutSt,'    Cref Report: %t\n',[TargetFile]),
 	printf(OutSt,'    \tFor: %t\n\n',[SuiteName]).

gen_HTML_header(OutHTML,SuiteName, SuiteDesc, TargetFile)
	:-
	printf(OutHTML,'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">\n',[]),
	printf(OutHTML,'<HTML xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">\n',[]),
	printf(OutHTML,'<HEAD>\n',[]),
	printf(OutHTML,'<meta http-equiv="content-type" content="text/html; charset=utf-8" />\n',[]),
	printf(OutHTML,'<meta name="description" content="ALS Prolog Open Source" />\n',[]),
	printf(OutHTML,'<meta name="keywords" content="programming, prolog, opensource" />\n',[]),
	printf(OutHTML,'<title>Cref Report: %t</title>\n',[SuiteName]),
	printf(OutHTML,'</HEAD>\n',[]),
	printf(OutHTML,'<BODY>\n',[]),

 	printf(OutHTML,'<h1>Cref Report for %t<br></h1>\n',[SuiteDesc]),
	printf(OutHTML,'<h3>----by library/cref.pro</h3>\n\n',[]),
	(commonFilesLocn(CFL) -> 
		printf(OutHTML, '<h3>Common Files Location: %t</h3>\n', [CFL])
		; 
		printf(OutHTML, '<h3>No Common Files Location</h3>\n', [])
	),
	strippedFiles(PlainFilesList),
	printf(OutHTML, '<h3>Suite Files:\n\t%t</h3>\n\n',[PlainFilesList]).

finish_HTML_doc(OutHTML)
	:-
	printf(OutHTML,'</BODY>',[]),
	printf(OutHTML,'</HTML>',[]).

write_HTML_cref_file(FilesList,CallsTree,OutHTML,SuiteName)
	:-
	true.

%%--------------- Report Output -----------------------------

setup_report(List, NoItemsMessage, ItemsMessage, OutS)
	:-
 	printf(OutS,'\n ==============================\n',[]),
 	(List = [] ->
 		printf(OutS,'%t\n',[NoItemsMessage])
		;
 		printf(OutS,'%t\n',[ItemsMessage]),
		write_list_display(4, List, OutS)
	),
 	printf(OutS,'\n ==============================',[]),
	printf(OutS,'\n',[]).

/*------------------------------------------------------------------------
	cref_out 
 *------------------------------------------------------------------------*/
cref_out([],_,[],_,OutHTML).

cref_out(['$depends_on'(P,N,Mod,UseList,F) | Rest],CallsTree, NoDefL, OutS,OutHTML)
	:-
	P/N \= varHeadRule/0,
	!,
	avl_search(P/N, TreeDataPN, CallsTree),
	displayDependency(P,N,Mod,UseList,F,TreeDataPN, NoDefL, NoDefLInter, OutS,OutHTML),
 	cref_out(Rest,CallsTree,NoDefLInter, OutS,OutHTML).

cref_out([_ | Rest],CallsTree, NoDefL, OutS,OutHTML)
	:-
 	cref_out(Rest,CallsTree,NoDefl, OutS,OutHTML).

displayDependency(P,N,Mod,UseList,F,TreeDataPN, NoDefL, NoDefLTail, OutS,OutHTML)
	:-
	accessCRF(calledby,TreeDataPN, CalledBys),
	accessCRF(clausecount,TreeDataPN, ClauseCount),
	accessCRF(factcount, TreeDataPN, FactCount),
	getMiscInfo(MIS),
	accessMI(mods_exp_preds, MIS, ExpPreds),

	sort(UseList, NiceUseList),
	printf(OutS,'%t/%t  #clauses=%t   #facts=%t',[P,N,ClauseCount,FactCount]),
	printf(OutHTML,'<br><b>%t/%t</b>  #clauses=%t   #facts=%t',[P,N,ClauseCount,FactCount]),

	(member(P/N, NiceUseList) ->
		printf(OutS,'   --recursive--  ',[]) ;
		printf(OutHTML,'   --recursive--  ',[]) ;
		true
	),
	printf(OutS,'  Module: %t',[Mod]),
	printf(OutHTML,'  Module: %t',[Mod]),

	((dmember(Mod+ModExpPreds, ExpPreds),
		dmember(P/N, ModExpPreds)) ->
		printf(OutS,'  --exported ',[]) ;
		printf(OutHTML,'  --exported ',[]) ;
		true
	),

	accessCRF(dynamicdecl, TreeDataPN, DynDecl),
	(DynDecl == [] ->
		true
		;
		printf(OutS,' -- dynamic',[DynDecl]),
		printf(OutHTML,' -- dynamic',[DynDecl])
	),

	builtins:lib_mod_list(LibModList),
	((ClauseCount == 0, FactCount ==0) ->
			    % XM = library module exporting predicate P/N in file LF:
		(find_expm(LibModList, P, N, XM, LF) ->
			printf(OutS,'\n     * Library: exported from: %t in %t',[XM, LF]),
	printf(OutHTML, '<div style="margin-left:3em;">',[]),
			printf(OutHTML,'* Library: exported from: %t in %t',[XM, LF]),
	printf(OutHTML, '</div>',[]),

        		accessMI(lib_files, MIS,  LibFilesList),
			(dmember(lf(P,N,XM,LF), LibFilesList) ->
				true
				;
				setMI(lib_files, MIS, [lf(P,N,XM,LF) | LibFilesList])
			),
			NoDefL = NoDefLTail
			;
			NoDefL = [P/N + (Mod + F) | NoDefLTail],
			printf(OutS,'   <<< UNDEFINED  ',[]),
			printf(OutHTML,'&nbsp;&nbsp;&lt;&lt;&lt; UNDEFINED  ',[])
		)
		;
			%% either ClauseCount > 0 or FactCount > 0:
		NoDefL = NoDefLTail
	),
	printf(OutHTML, '<div style="margin-left:3em;margin-top:-1em;">',[]),

	printf(OutS, '\n      files=[',[]),
	printf(OutHTML, '<br>      files=[',[]),
	sort(F, SF),
	listFiles(SF, OutS,OutHTML),
	printf(OutS, '    * Depends on:  %t\n', [NiceUseList]),
	printf(OutHTML, '<br>Depends on:  %t\n', [NiceUseList]),
	printf(OutS,'    * Called by:   %t\n',[CalledBys]),
	printf(OutHTML,'<br>Called by:   %t\n',[CalledBys]),
	printf(OutHTML, '</div>',[]).

find_expm([XM/F | LibModList], P, N, XM, F)
	:-
	builtins:'$exported_proc'(XM, P, N),
	!.
find_expm([_ | LibModList], P, N, XM, F)
	:-
	find_expm(LibModList, P, N, XM, F).

fixUseListNice([],[]).
fixUseListNice([p(Mod:Q,N,F) | RestUseList],
					[Q/N | RestNiceUseList])
	:-
	fixUseListNice(RestUseList,RestNiceUseList).

use_list_out_2([]).
use_list_out_2([p(Module,File,[])|Rest]) 
	:- !,
	use_list_out_2(Rest).
use_list_out_2([p(Module,File,ImportList)|Rest]) 
	:-
 	write(Module), write(' imports :'), 
 	write_list(5,ImportList),
 	use_list_out_2(Rest).

write_list(_,[]).
write_list(Tab,[p(Mod,P,N,F)|Rest])
	:-
 	nl, tab(Tab),
 	write(Mod:P), write('/'),write(N),
 	write_list(Tab,Rest).

write_list_display(_,[], _).
write_list_display(Tab,[Mod+List|Rest],OutS)
	:-
	printf(OutS,'%t:\n\t%t\n',[Mod,List]),
 	write_list_display(Tab,Rest,OutS).

use_list_out([],_,_).
use_list_out([p(Mod:Q,N,F)],J,Mod) 
	:-
 	(J = 5, !, nl, tab(5); true),
 	write(Q),write('/'),write(N).
use_list_out([p(M:Q,N,F)],J,Mod) 
	:-
 	Mod \== M,
 	(J = 5, !, nl, tab(5); true),
 	write(M:Q),write('/'),write(N).

use_list_out([p(Mod:Q,N,F) | Rest],J,Mod) 
	:-
 	(J = 5, !, nl, tab(5), K is 1; K is J+1),
 	write(Q),write('/'),write(N), write(', '),
 	use_list_out(Rest,K,Mod).
use_list_out([p(M:Q,N,F) | Rest],J,Mod) 
	:-
 	Mod \== M,
 	(J = 5, !, nl, tab(5), K is 1; K is J+1),
 	write(M:Q),write('/'),write(N), write(', '),
 	use_list_out(Rest,K,Mod).

	/*----------Miscellaneous---------------------------------*/

vvmember(P,[H| _])
	:-	
	nonvar(H), P=H,!.
vvmember(P,[H | Rest])
	:- 
	nonvar(H),
	vvmember(P,Rest).

memb(P,N,F,[p(P,N,F) | _]).
memb(P,N,F,[_ | Rest]) 
	:- 
	memb(P,N,F,Rest).
non_mem(_,_,_,L) 
	:- 
	var(L),!.
non_mem(P,N,F,[p(P,N,F) | _]) 
	:-!,fail.
non_mem(P,N,F,[p(Mod:P,N,F) | _] ) 
	:-!,fail.
non_mem(P,N,F,[_ | Rest]) 
	:- 
	non_mem(P,N,F,Rest).

/*===================================================================*
 |		-- avl tree program: customized for cref
 |
 | Authors: Kevin A. Buettner, Ken Bowen
 | Created: Original (direct avl tree program): 4/2/90
 | Revision History:
 |  Modfied to utilize struct-oriented avl tree program: 8/94 - Ken Bowen
 |	  Now the data entry of a node is a structure; 
 |	  The subkey of an insertion indicates to which slot of the 
 |	    structure the passed-in item is to be attached (maybe added 
 |	    to the list in that slot, etc.)
 |
 | Module Name:	cref
 | Procedures:
 | ----------
 |
 |	avl_create(Tree)	
 |		-- create an empty tree, Tree
 |
 |	avl_insert(Key,Data,InTree,OutTree)
 |		-- insert Key and Data into InTree producing OutTree
 |
 |	avl_search(Key,Data,Tree)
 |		-- search Tree for Key and return Data
 |
 |	avl_inorder(Tree,List)	
 |		-- List = list of keys in Tree found by an inorder traversal
 |
 |
 | Data Structures:
 | ---------------
 |	* The tree is represented by a structured term.  
 |
 |	* empty 
 |		-	the empty tree.
 |
 |	* bn(Key,Data,Left,Right) 
 |		-	a tree whose left and right subtrees have the same height.
 |
 |	* ln(Key,Data,Left,Right) 
 |		-	a tree the height of whose left subtree is one
 |			larger than the height of the right subtree.
 |
 |	* rn(Key,Data,Left,Right)
 |		-	a tree the height of whose right subtree is one
 |			larger than the height of the left subtree.
 *===================================================================*/

/*!-----------------------------------------------------------------------
 |	cref_shell/0
 |	cref_shell
 |	cref_shell
 |
 |	- enter the cref shell for exploring analyzed programs
 |
 |	Enter the cref shell with Calls Tree & MIS Struct pulled from
 |	global vars, but shell struct restarted:
 *-----------------------------------------------------------------------*/
cref_shell 
	:- 
	OutS = user_output,
	InS = user_input,
	(getCallsTree(CallsTree) ->
		true
		;
		CallsTree = empty
	),
	(getMiscInfo(MIS) ->
		true
		;
		makeMI(MIS)
	),
	makeCRSH(Struct),
	setShellStruct(Struct),
	cref_shell(CallsTree, MIS, InS, OutS, Struct).

/*!-----------------------------------------------------------------------
 |	start_cref_shell/2
 |	start_cref_shell(SuiteName, Options)
 |	start_cref_shell(+, +)
 |
 |	- Start the cref_shell on a suite, with possible options
 *-----------------------------------------------------------------------*/
start_cref_shell(SuiteName, Options)
	:- 
	check_default(Options, shell_struct, Struct^(cref:makeCRSH(Struct)), Struct),
	setCRSH(suite,Struct,SuiteName),
	setCRSH(options,Struct,Options),
	check_default(Options, cref_tree, CallsTree^(cref:getCallsTree(CallsTree)), CallsTree),
	check_default(Options, mistruct, MIS^(cref:getMiscInfo(MIS)), MIS),
	
	check_default(Options, outstream, user_output, OutS),
	check_default(Options, instream, user_input, InS),

	begin_cref_shell(CallsTree, MIS, InS, OutS, Struct).

/*!-----------------------------------------------------------------------
 |	rcs/0
 |	rcs
 |	rcs
 |
 | - invoke restart_cref_shell/0
 *-----------------------------------------------------------------------*/
rcs 	
	:- 
	restart_cref_shell.

/*!-----------------------------------------------------------------------
 |	restart_cref_shell/0.
 |	restart_cref_shell
 |	restart_cref_shell
 |
 |	- do a warm re-start of the cref shell
 |
 |	Warm start:
 |	Enter the cref shell with Calls Tree & MIS Struct & ShellStruct
 |	pulled from global vars (keeps previous history, etc.):
 *-----------------------------------------------------------------------*/
restart_cref_shell
	:- 
	getShellStruct(Struct),
	getCallsTree(CallsTree),
	getMiscInfo(MIS),
	accessCRSH(options,Struct, Options),
	check_default(Options, outstream, user_output, OutS),
	check_default(Options, instream, user_input, InS),
	cref_shell(CallsTree, MIS, InS, OutS, Struct).

/*!-----------------------------------------------------------------------
 |	cx/1
 |	cx(Command)
 |	cx(+)
 |
 |	- execute cref shell Command in current context
 | 	
 |	In effect, do a warm start, without really entereing the shell,
 |	and then execute command;
 |	Enter the cref shell with Calls Tree & MIS Struct & ShellStruct
 |	pulled from global vars (keeps previous history, etc.), and the
 |	execute command, and exit the shell.
 *-----------------------------------------------------------------------*/
cx(Command)
	:- 
	getShellStruct(Struct),
	getCallsTree(Tree),
	getMiscInfo(MIS),
	cref_act_on(Command, Tree, MIS, user_input,user_output, Struct, _).

/*---------------------------------------------------------------
 |	cref_shell/5
 |	cref_shell(Tree, MIS, InS, OutS, Struct) 
 |	cref_shell(+, +, +, +, +) 
 |
 |			MAIN LOOP
 |	Structured as it is to cater to both the "in control"
 |	"TTY Version" appearing here, as well as a "passive
 |	callback version" in a GUI environment; the goal is to
 |	isolate the actions of a single cycle around the loop cleanly
 |	in the middle of the loop (here: cref_act_on/7).  The GUI
 |	callback simply invokes cref_act_on (or even something more
 |  like cx/1 below).  The 7th "Continue" argument of cref_act_on/7
 |	is simply for the use of the TTY loop (in continue_cref_shell/6);
 |	It is unused in the GUI version or in cx/1.
 *--------------------------------------------------------------*/
begin_cref_shell(Tree, MIS, InS, OutS, Struct)
        :-
        setShellStruct(Struct),
        setCallsTree(Tree),
        setMiscInfo(MIS),

        get_command(Tree, MIS, InS, OutS, Struct, Command),
        cref_act_on(Command, Tree, MIS, InS,OutS, Struct, Continue),
        continue_cref_shell(Continue, Tree, MIS, InS, OutS, Struct).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% cref_shell MAIN LOOP
	%% 
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%cref_shell(Tree, MIS, InS, OutS, Struct) 
cref_shell(_, _, _, _, _) 
	:-
	getShellStruct(Struct),
	getCallsTree(Tree),
	getMiscInfo(MIS),
	accessCRSH(in_stream,Struct,InS),
	accessCRSH(out_stream,Struct,OutS),
	get_command(Tree, MIS, InS, OutS, Struct, Command), 

	cref_act_on(Command, Tree, MIS, InS,OutS, Struct, Continue),

	continue_cref_shell(Continue, Tree, MIS, InS, OutS, Struct).

continue_cref_shell(end_of_file, Tree, MIS, InS, OutS, Struct) 
	:-!,
	setShellStruct(Struct).

continue_cref_shell(continue, Tree, MIS, InS, OutS, Struct) 
	:-
	cref_shell(Tree, MIS, InS, OutS, Struct).

cref_act_on((Command > Target), Tree, MIS, InS, OutS, Struct, Continue)
	:-
	is_stream(Target, TgtS),
	!,
	cref_act_on(Command, Tree, MIS, InS, TgtS, Struct, Continue).

cref_act_on((Command > Target), Tree, MIS, InS, OutS, Struct, Continue)
	:-
	open(Target, write, TgtS, []),
	!,
	cref_act_on(Command, Tree, MIS, InS, TgtS, Struct, Continue),
	close(TgtS).

cref_act_on((Command > Target), Tree, MIS, InS, OutS, Struct, Continue)
	:-!,
	printf(OutS, 'Error:%t not a stream & not openable!\n', [Target]).

cref_act_on((Command, Commands), Tree, MIS, InS, OutS, Struct, Continue)
	:-
	cref_act_on(Command, Tree, MIS, InS, OutS, Struct, _),
	cref_act_on(Commands, Tree, MIS, InS, OutS, Struct, Continue).

cref_act_on(Command, Tree, MIS, InS, OutS, Struct, Continue)
	:-
	accessCRSH(cycle,Struct, M),
	N is M+1,
	setCRSH(cycle,Struct,N),
	accessCRSH(history,Struct,History),
	setCRSH(history,Struct,[N-Command | History]),
	act_on(Command, Tree, MIS, InS,OutS, Struct, Continue).

get_command(Tree, MIS, InS, OutS, Struct, Command)
	:-
	accessCRSH(cycle,Struct, M),
	N is M+1,
	printf(OutS,'cref(%t): ',[N]),
			%% Later: soup this up, so that a separate text window
			%% is used for crefshell interaction in windows mode,
			%% and the interaction is non-blocking, like the 
			%% main shell (create a general facility).
	read_term(InS,Command,[blocking(true)]).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% EXITING: Only cases with Continue = end_of_file
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% Go on to output:
act_on(quit,_,_,_,_,Struct,end_of_file) :-!.
act_on(q,_,_,_,_,Struct,end_of_file) :-!.
act_on(exit,_,_,_,_,Struct,end_of_file) :-!.
act_on(end_of_file,_,_,_,_,Struct,end_of_file) :-!.


	%% really halt or exit=abort:
act_on(halt,_,_,_,_,Struct) :-!, halt.
act_on(abort,_,_,_,_,Struct,end_of_file) :-!, abort.	


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% ALL OTHER CASES OF act_on/7 give
	%%		Continue = continue:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

act_on(Command, Tree, MIS, InS,OutS, Struct, continue)
	:-
	act_on(Command, Tree, MIS, InS,OutS, Struct).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% DISPLAYING THE TREE
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		%% Vomit:
act_on(writetree, Tree, MIS, InS, OutS, Struct) 
	:-!,
	write_term(OutS, Tree, [maxdepth(50)]), nl.

		%% Decent display:
act_on(write,Tree, MIS, InS, OutS, Struct) 
	:-!,
	act_on(inorder,Tree, MIS, InS, OutS, Struct).
act_on(inorder,Tree, MIS, InS,OutS, Struct) 
	:-!,
	avl_inorder(Tree,List),
	display_nodes(List, OutS),
	accessCRSH(outputs,Struct,Outputs),
	accessCRSH(cycle,Struct,N),
	setCRSH(outputs,Struct,[N-List | Outputs]).

avl_inorder(Tree,List) 
	:-
	inorder(Tree,[],List).

inorder(empty,L,L) 
	:- !.
inorder(Tree,InL,OutL) 
	:-
	avl_key(Tree,Key),
	avl_data(Tree,Data),
	avl_left(Tree,Left),
	avl_right(Tree,Right),
	inorder(Right,InL,RL),
	inorder(Left, [ Key-Data | RL ], OutL).

display_nodes([], _).
display_nodes([Node | List], OutS)
	:-
	display_cref_node(Node, OutS),
	display_nodes(List, OutS).

display_cref_node(Pred/Arity - Node, OutS)
	:-
	accessCRF(mod, Node, Mod),
	accessCRF(files, Node, Files),
	accessCRF(exported, Node, Exported),
	accessCRF(calls, Node, Calls),
	accessCRF(calledby, Node, Calledby),
	accessCRF(dependson, Node, Dependson),
	accessCRF(basis, Node, Basis),
	accessCRF(clausecount, Node, Clausecount),
	accessCRF(factcount, Node, Factcount),

	printf(OutS,'%t/%t: mod=%t exported=%t, files=%t\n\tclausecount=%t factcount=%t\n',
			[Pred,Arity,Mod,Exported,Files,Clausecount,Factcount]),
	printf(OutS,'\tcalls=%t calledby=%t\n',[Calls,Calledby]),
	printf(OutS,'\tdependson=%t\n',[Dependson]),
	printf(OutS,'\tbasis=%t\n',[Basis]).
	

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% SIMPLE STATISTICS
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

act_on(stats,Tree, MIS, InS,OutS, Struct) 
	:-
	!,
	height(Tree,Height),
	nelems(Tree,N),
	printf('Height=%t  Node Count=%t\n',[Height,N]).

height(empty,-1) 
	:- !.
height(Tree,Height) 
	:-
	avl_left(Tree,Left),
	avl_right(Tree,Right),
	height(Left,LH),
	height(Right,RH),
	max(LH,RH,H),
	Height is H+1.

nelems(empty,0) 
	:- !.
nelems(Tree,N) 
	:-
	avl_left(Tree,Left),
	avl_right(Tree,Right),
	nelems(Left,NL),
	nelems(Right,NR),
	N is NL+NR+1.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% FIND & DISPLAY A NODE
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

act_on(search(Key),Tree, MIS, InS,OutS, Struct) 
	:-
	Key = _/_,
	avl_search(Key,Data,Tree),
	!,
	display_cref_node(Key - Data, OutS),
	set_output((Key-Data),Struct,MIS).

act_on(search(Key),Tree, MIS, InS,OutS, Struct) 
	:-!,
	printf(OutS,'Key %t not found.\n',[Key]).

set_output(Output,Struct,MIS)
	:-
	accessCRSH(outputs,Struct,Outputs),
	accessCRSH(cycle,Struct,N),
	setCRSH(outputs,Struct,[N-Output | Outputs]).

act_on(r(Key,Slot,Val),Tree, MIS, InS,OutS, Struct) 
	:-
	Key = _/_,
	avl_search(Key,Data,Tree),
	!,
	(accessCRF(Slot,Data,Val) ->
		true
		;
		printf(OutS,'Slot %t not found.\n',[Slot])
	).

act_on(r(Key,_,_),Tree, MIS, InS,OutS, Struct) 
	:-!,
	printf(OutS,'Key %t not found.\n',[Key]).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% HISTORY MECHANISM
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

act_on(hist,Tree, MIS, InS,OutS, Struct) 
	:-!,
	act_on(hist(1),Tree, MIS, InS,OutS, Struct).
act_on(history,Tree, MIS, InS,OutS, Struct) 
	:-!,
	act_on(hist(1),Tree, MIS, InS,OutS, Struct).
act_on(history(Lim),Tree, MIS, InS,OutS, Struct) 
	:-!,
	act_on(hist(Lim),Tree, MIS, InS,OutS, Struct).
act_on(hist(Lim),Tree, MIS, InS,OutS, Struct) 
	:-!,
	show_hist(Lim, MIS, InS,OutS, Struct).

show_hist(Lim, MIS, InS,OutS, Struct)
	:-
	accessCRSH(cycle,Struct,CurN),
	show_hist(CurN, Lim, MIS, InS,OutS, Struct).

show_hist(CurN, Lim, MIS, InS,OutS, Struct)
	:-
	accessCRSH(history,Struct,History),
	return_hist(History, CurN, Lim, MIS, InS,OutS).

return_hist([], CurN, Lim, MIS, InS,OutS).
return_hist(History, CurN, Lim, MIS, InS,OutS)
	:-
	CurN < Lim, !.
return_hist([Cmd | History], CurN, Lim, MIS, InS,OutS)
	:-
	printf(OutS, '%t\n', [Cmd]),
	NextN is CurN - 1,
	return_hist(History, NextN, Lim, MIS, InS,OutS).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% HISTORY: OUTPUT of COMMANDS
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

act_on(out(N),Tree, MIS, InS,OutS, Struct) 
	:-
	accessCRSH(outputs,Struct,Outputs),
	dmember((N-Output), Outputs),
	!,
	printf(OutS, '%t - %t\n', [N, Output]).

act_on(out(N),Tree, MIS, InS,OutS, Struct) 
	:-!,
	printf(OutS, 'No output found for command %t\n', [N]).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% I/O (TREES) to FILES
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

act_on(savetree,Tree, MIS, InS,OutS, Struct) 
	:-!,
	accessCRSH(suite,Struct,SuiteName),
	act_on(savetree(SuiteName),Tree, MIS, InS,OutS, Struct).
	
act_on(savetree(File),Tree, MIS, InS,OutS, Struct) 
	:-!,
	file_extension(FileName,File,cft),
	accessCRSH(suite,Struct,SuiteName),
	date(Date),time(Time),
	open(FileName,write,FileStream,[]),
	printf(FileStream,'%t.\n',[crft(SuiteName,Tree,MIS,Date,Time)],[quoted(true)]),
	close(FileStream),
	printf(OutS,'Current tree written to file %t\n',[FileName]).

act_on(loadtree(FileName),Tree, MIS, InS,OutS, Struct) 
	:-
	atom(FileName),
	(file_extension(FileName,File,cft) ->
		TreeFile = FileName
		;
		file_extension(TreeFile,FileName,cft)
	),
	exists_file(TreeFile),
	open(TreeFile,read,FileStream,[]),
	read_term(FileStream, crft(NewSuiteName,NewTree,NewMIS,_,_),[]),
	close(FileStream),
	!,
	accessCRSH(suite,Struct,OldSuiteName),
	accessCRSH(context_stack,Struct,OldStack),
	(OldSuiteName = unknown ->
		NewStack = OldStack
		;
		insert_tree(OldStack,OldSuiteName,Tree,MIS,NewStack)
	),
	setCRSH(context_stack,Struct,NewStack),
	printf(OutS,'Switching to tree %t read from file %t\n',
				[NewSuiteName,TreeFile]),
	setCRSH(suite,Struct,NewSuiteName),
	setCallsTree(NewTree),
	setMiscInfo(NewMIS).

act_on(loadtree(FileName),Tree, MIS, InS,OutS, Struct) 
	:-
	printf(OutS,'Can''t load tree from file %t\n',[FileName]).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% SAVING TREE CONTEXTS
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% Display current list of contexts:
act_on(ctxs, Tree, MIS, InS,OutS, Struct) 
	:-
	accessCRSH(context_stack,Struct,Stack),
%	show_ctx_names(Stack, OutS).
	write_lines(OutS, Stack).

	%% Restore a named context:
act_on(restore(NewSuiteName), Tree, MIS, InS,OutS, Struct) 
	:-
	accessCRSH(context_stack,Struct,OldStack),
	dmember(NewSuiteName-(NewTree/NewMIS), OldStack),
	!,
	list_delete(OldStack, NewSuiteName-_, InterStack),
	accessCRSH(suite,Struct,SuiteName),
	insert_tree(InterStack,SuiteName,Tree,MIS,NewStack),
	setCRSH(context_stack,Struct,NewStack),
	setCRSH(suite,Struct,NewSuiteName),
	setCallsTree(NewTree),
	setMiscInfo(NewMIS),
	printf(OutS,'Switching to tree %t\n',[NewSuiteName]).
	
act_on(restore(Name), Tree, MIS, InS,OutS, Struct) 
	:-
	printf(OutS,'Cannot find tree labelled %t!\n',[Name]).

insert_tree(OldStack,SuiteName,Tree,MIS,NewStack)
	:-
	dmember(SuiteName-_, OldStack),
	!.
insert_tree(OldStack,SuiteName,Tree,MIS,[SuiteName-(Tree/MIS) | NewStack]).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% STORING OUTPUTS in the DATABASE
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

act_on(store, Tree, MIS, InS,OutS, Struct) 
	:-
	accessCRSH(cycle,Struct,N),
	M is N-1,
	accessCRSH(outputs,Struct,Outputs),
	(dmember((M-Output), Outputs) ->
		user:assert(scrf(M,Output))
		;
		printf(OutS,'No output for command #%t!\n',[M])
	).

act_on(store(M), Tree, MIS, InS,OutS, Struct) 
	:-
	accessCRSH(outputs,Struct,Outputs),
	(dmember((M-Output), Outputs) ->
		user:assert(scrf(M,Output))
		;
		printf(OutS,'No output for command #%t!\n',[M])
	).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% HELP
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

act_on(help,Tree, MIS, InS,OutS, Struct) 
	:-!,
	cref_help.

cref_help
	:-
	printf('\nRecognized commands:\n',[]),
	printf('-------------------\n',[]),
	printf('q, quit, exit ] -- exit cref_shell, generate output, and return to prolog.\n',[]),
	printf('abort           -- immediately abort back to prolog.\n',[]),
	printf('halt            -- immediately exit prolog.\n\n',[]),
	printf('help            -- print this list.\n',[]),
	printf('hist            -- equiv to hist(1).\n',[]),
	printf('hist(N)         -- show history back to command N.\n',[]),
	printf('out(N)          -- show output of command N (if any).\n\n',[]),
	printf('stats           -- get height and number of nodes of tree.\n',[]),
	printf('inorder         -- print P/Ns & data with inorder traversal of tree.\n',[]),
	printf('write           -- print P/Ns & data with inorder traversal of tree.\n',[]),
	printf('writetree       -- dump the tree on the terminal.\n',[]),
	printf('files           -- list the files processed.\n',[]),
	printf('mods            -- list the mods, with files.\n',[]),
	printf('exp(Mod)        -- show P/N exported by module Mod.\n',[]),
	printf('search(P/N)     -- search for node with key P/N; print result.\n\n',[]),
	printf('store           -- assert OUTPUT of last command (#N) as scrf(N,OUTPUT)\n',[]),
	printf('savetree        -- save tree in file <suite name>.cft\n',[]),
	printf('savetree(File)  -- save tree in file File.cft\n',[]),
	printf('loadtree(File)  -- load tree from file File.cft\n',[]),
	printf('ctxs 		-- show stored tree contexts\n',[]),
	printf('restore(Name)   -- restore a stored tree context\n\n',[]).

/*
	printf('depends         -- perform cref "depends on" processing\n',[]),
	printf('exports         -- determine P/N exported by each module\n',[]),
%	printf('calledby        -- perform cref "called by" processing\n',[]),

	printf('undefs          -- returns list of undefined predicates\n',[]),
	printf('show_undefs     -- returns list of undefined predicates\n',[]),
	printf('xall            -- perform all additional cref processing\n',[]).
*/


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% FILES and MODULES
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

act_on(files,Tree, MIS, InS,OutS, Struct) 
	:-!,
	accessMI(files, MIS, InitFiles),
	sort(InitFiles, Files),
	printf(OutS, 'Files handled = %t\n', [Files]),
	set_output(Files,Struct,MIS).

act_on(mods,Tree, MIS, InS,OutS, Struct) 
	:-!,
	accessMI(mods, MIS, InitMods),
	sort(InitMods, Mods),
	printf(OutS, 'Modules information:\n', []),
	accessMI(mods_files, MIS, MFiles),
	accessMI(mods_use, MIS, MUse),
	mods_out(Mods, MFiles, MUse, OutS),
	set_output(Mods,Struct,MIS).

mods_out([], MFiles, MUse, OutS).
mods_out([Mod | Mods], MFiles, MUse, OutS)
	:-
	(dmember(Mod+ModFiles, MFiles); MFiles = []),
	sort(MFiles, SortedMFiles),
	printf(OutS, 'Mod: %t - in files: %t\n',[Mod,SortedModFiles]),
	(dmember(Mod+Use, MUse) ->
		printf(OutS, '\tuses mods: %t',[Use])  ; true ),
	nl(OutS),
	!,
	mods_out(Mods, MFiles, MUse, OutS).

act_on(exp(M),Tree, MIS, InS,OutS, Struct) 
	:-!,
	accessMI(mods_exp_preds, MIS, MExp),
	(dmember(M+Exps, MExp) -> true ; Exps = []),
	printf(OutS, 'Mod %t exports:\n\t%t\n',[M,Exps]),
	set_output(Exps,Struct,MIS).

/*
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% FURTHER CREF PROCESSING
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

act_on(xall,Tree, MIS, InS,OutS, Struct) 
	:-!,
	printf(OutS, 'Doing depends...\n',[]),
	pass_depends(Tree,Tree),
	printf(OutS, 'Doing basis...\n',[]),
	walk_process(Tree,Tree,basis_deps,MIS,cref,_),
	printf(OutS, 'Doing exports...\n',[]),
	walk_process(Tree,Tree,exports,MIS,cref,_),
	printf(OutS, 'Doing mcalls...\n',[]),
	walk_process(Tree,Tree,mcalls,MIS,cref,_),
	printf(OutS, 'Doing fcalls...\n',[]),
	walk_process(Tree,Tree,fcalls,MIS,cref,_),
	printf(OutS, 'All Done.\n',[]).

act_on(depends,Tree, MIS, InS,OutS, Struct) 
	:-!,
	pass_depends(Tree,Tree).

	%% Gather exports/imports, by module:
act_on(exports,Tree, MIS, InS,OutS, Struct) 
	:-!,
	walk_process(Tree,Tree,exports,MIS,cref,_).

/ *
exports(empty,TreeTop,_,MIS,Tail,Tail) :-!.
exports(DataNode,TreeTop,P/N,MIS,Out,Tail)
	:-
	accessCRF(exported, DataNode, Exported),
	accessCRF(mod, DataNode, Mod),
	accessCRF(importto, DataNode, Importto),
	(Exported = true ->
		accessMI(mods_exp_preds, MIS, ModsX),
		enter_in(mods_exp_preds, ModsX, Mod, MIS, P/N)
		;
		true
	).
* /

	%% Gather all procs called in each module:
act_on(mcalls,Tree, MIS, InS,OutS, Struct) 
	:-!,
	walk_process(Tree,Tree,mcalls,MIS,cref,_).

act_on(called_in(M),Tree, MIS, InS,OutS, Struct) 
	:-!,
	accessMI(mods_c_preds, MIS, MCPreds),
	(dmember(M+MCalls, MCPreds) -> true ; MCalls = []),
	printf(OutS,'Called in mod %t:\n\t%t\n',[M,MCalls]),
	set_output(M-MCalls,Struct,MIS).

mcalls(empty,TreeTop,_,MIS,Tail,Tail) :-!.
mcalls(DataNode,TreeTop,P/N,MIS,Out,Tail)
	:-
	accessCRF(calls, DataNode, PNCalls),
	accessCRF(mod, DataNode, PNMod),
	accessMI(mods_c_preds, MIS, MCPreds),
	(locm(MCPreds, PNMod, Entry) ->
		Entry = PNMod + ModCPreds,
		union(PNCalls, ModCPreds, NewModCPreds),
		mangle(2, Entry, NewModCPreds)
		;
		setMI(mods_c_preds, MIS, [PNMod+PNCalls | MCPreds])
	).
	
	%% Gather all procs called in each file:
act_on(fcalls,Tree, MIS, InS,OutS, Struct) 
	:-!,
	walk_process(Tree,Tree,fcalls,MIS,cref,_).

act_on(called_in_file([FN|FE]),Tree, MIS, InS,OutS, Struct) 
	:-!,
	file_extension(File,FN,FE),
	act_on(called_in_file(File),Tree, MIS, InS,OutS, Struct) .
act_on(called_in_file(F),Tree, MIS, InS,OutS, Struct) 
	:-!,
	accessMI(files_c_preds, MIS, FCPreds),
	(dmember(F+FCalls, FCPreds) -> true ; FCalls = []),
	printf(OutS,'Called in file %t:\n\t%t\n',[F,FCalls]),
	set_output(F-FCalls,Struct,MIS).

act_on(def_in_file([FN|FE]),Tree, MIS, InS,OutS, Struct) 
	:-!,
	file_extension(File,FN,FE),
	act_on(def_in_file(File),Tree, MIS, InS,OutS, Struct) .
act_on(def_in_file(F),Tree, MIS, InS,OutS, Struct) 
	:-!,
	accessMI(files_d_preds, MIS, FDPreds),
	(dmember(F+FDefs, FDPreds) -> true ; FDefs = []),
	printf(OutS,'Defined in file %t:\n\t%t\n',[F,FDefs]),
	set_output(F-FDefs,Struct,MIS).

act_on(def_in_file(F,FDefs),Tree, MIS, InS,OutS, Struct) 
	:-!,
	accessMI(files_d_preds, MIS, FDPreds),
	(dmember(F+FDefs, FDPreds) -> true ; FDefs = []).

fcalls(empty,TreeTop,_,MIS,Tail,Tail) :-!.
fcalls(DataNode,TreeTop,P/N,MIS,Out,Tail)
	:-
	accessCRF(calls, DataNode, PNCalls),
	accessCRF(files, DataNode, PNFiles),
	add_calls_to(PNFiles, PNCalls, P/N, MIS).

add_calls_to([], PNCalls, P/N, MIS).
add_calls_to([FF | PNFiles], PNCalls, P/N, MIS)
	:-
	accessMI(files_c_preds, MIS, FCPreds),
	(locm(FCPreds, FF, CEntry) ->
		CEntry = FF + FFCPreds,
		union(PNCalls, FFCPreds, NewFFCPreds),
		mangle(2, CEntry, NewFFCPreds)
		;
		setMI(files_c_preds, MIS, [FF+PNCalls | FCPreds])
	),
	accessMI(files_d_preds, MIS, FDPreds),
	(locm(FDPreds, FF, DEntry) ->
		DEntry = FF + FFDPreds,
		(dmember(P/N, FFDPreds) ->
			true
			;
			mangle(2, DEntry, [P/N | FFDPreds])
		)
		;
		setMI(files_d_preds, MIS, [FF+[P/N] | FDPreds])
	),
	add_calls_to(PNFiles, PNCalls, P/N, MIS).
	
act_on(show_undefs,Tree, MIS, InS,OutS, Struct) 
	:-!,
	act_on(undefs,Tree, MIS, InS,OutS, Struct).

act_on(undefs,Tree, MIS, InS,OutS, Struct) 
	:-!,
	walk_process(Tree,Tree,gundefs,MIS, cref, Undefs, []),
	set_output(Undefs, Struct, MIS),
	user:abolish(undefs,1),
	user:assert(undefs(Undefs)),
	write_lines(OutS, Undefs).

gundefs(Node, TreeTop, Key, MIS, InputList, Output)
	:-
	accessCRF(factcount, Node, 0),
	accessCRF(clausecount, Node, 0),
	!,
	InputList = [Key | Output].
	
gundefs(Node, TreeTop, Key, MIS, Input, Input).

act_on(basis_deps,Tree, MIS, InS,OutS, Struct) 
	:-!,
	walk_process(Tree,Tree,basis_deps,MIS,cref,_).

basis_deps(Node, TreeTop, TreeKey, MIS, InputList, Output)
	:-
	accessCRF(dependson, Node, DepsOnList),
	ult_basis(DepsOnList,TreeTop, UltBasis),
	!,
	setCRF(basis,Node,UltBasis).
basis_deps(Node, TreeKey, TreeTop, MIS, Input, Input).

ult_basis([],TreeTop, []).
ult_basis([P/N | DepsOnList], TreeTop, UltBasis)
	:-
	avl_search(P/N,PNNode,TreeTop),
	disp_ult_basis(PNNode, P/N, UltBasis, UltBasisTail),
	ult_basis(DepsOnList,TreeTop, UltBasisTail).

disp_ult_basis(PNNode, P/N, [P/N | UltBasisTail], UltBasisTail)
	:-
	accessCRF(factcount, PNNode, 0),
	accessCRF(clausecount, PNNode, 0),
	!.
disp_ult_basis(PNNode, P/N, UltBasis, UltBasis).

*******/

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% MISCELLANEOUS
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

act_on(clear,Tree, MIS, InS,OutS, Struct) 
	:-!,
	makeMI(NewMIS),
	setMiscInfo(NewMIS),
	NewTree = empty,
	setCallsTree(NewTree).

act_on([H | T],Tree, MIS, InS,OutS, Struct) 
	:-!,
	[H | T].

act_on(mis,Tree, MIS, InS,OutS, Struct) 
	:-!,
	dump_mis(OutS,MIS).

dump_mis(OutS,MIS)
	:-
	accessMI(files,MIS,Files),
	accessMI(files_mods,MIS,FilesMods),
	accessMI(files_d_preds,MIS,FilesDPreds),
	accessMI(files_c_preds,MIS,FilesCPreds),
	accessMI(mods,MIS,Mods),
	accessMI(mods_files,MIS,ModsFiles),
	accessMI(mods_use,MIS,ModsUse),
	accessMI(mods_d_preds,MIS,ModsDPreds),
	accessMI(mods_c_preds,MIS,ModsCPreds),
	accessMI(mods_exp_preds,MIS,ModsExpPreds),
	accessMI(mods_imp_preds,MIS,ModsImpPreds),

	printf(OutS,'files=%t\n',[Files]),
	printf(OutS,'files_mods=%t\n',[FilesMods]),
	printf(OutS,'fildes_d_preds=%t\n',[FilesDPreds]),
	printf(OutS,'files_c_preds=%t\n',[FilesCPreds]),
	printf(OutS,'mods=%t\n',[Mods]),
	printf(OutS,'mods_files=%t\n',[ModsFiles]),
	printf(OutS,'mods_use=%t\n',[ModsUse]),
	printf(OutS,'mods_d_preds=%t\n',[ModsDPreds]),
	printf(OutS,'mods_c_preds=%t\n',[ModsCPreds]),
	printf(OutS,'mods_exp_preds=%t\n',[ModsExpPreds]),
	printf(OutS,'mods_imp_preds=%t\n',[ModsImpPreds]).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% USER-DEFINED PROCESSING
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

?-dynamic(user_cref/6).
act_on(Cmd,Tree, MIS, InS,OutS, Struct) 
	:-
	user_cref(Cmd, Tree, MIS, InS, OutS, Output),
	!,
	accessCRSH(outputs,Struct,Outputs),
	accessCRSH(cycle,Struct,N),
	setCRSH(outputs,Struct,[N-Output | Outputs]).

act_on(walk(Pred),Tree, MIS, InS,OutS, Struct) 
	:-
	walk_process(Tree,Tree,Pred,MIS,user,Output),
	end_walk(Pred, MIS, OutS, Output),
	!,
	accessCRSH(outputs,Struct,Outputs),
	accessCRSH(cycle,Struct,N),
	setCRSH(outputs,Struct,[N-Output | Outputs]).

act_on(walk(Pred,Output),Tree, MIS, InS,OutS, Struct) 
	:-
	walk_process(Tree,Tree,Pred,MIS,user,Output),
	!,
	accessCRSH(outputs,Struct,Outputs),
	accessCRSH(cycle,Struct,N),
	setCRSH(outputs,Struct,[N-Output | Outputs]).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% DEFAULT - WHAT?
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

act_on(_,Tree, MIS, InS,OutS, Struct) 
	:-
	printf('Unrecognized command.\n'),
	cref_help.

inorder_defined(empty,_,_,L,L) 
	:- !.

	%% InL = incoming DependsOnList   OutL = resulting DependsOnList
inorder_defined(Tree,CompleteTree,MIS,InL,OutL) 
	:-
	avl_key(Tree,Key),
	avl_left(Tree,Left),
	avl_right(Tree,Right),
	avl_data(Tree, TreeData),

	accessCRF(pred,TreeData,P),
	accessCRF(arity,TreeData,N),
	accessCRF(mod,TreeData,Mod),
	accessCRF(files,TreeData,Files),

        accessMI(mods_use, MIS,  ModsUses),

	accessCRF(calls,TreeData,P_Calls),
	ultimate_uses(P_Calls, CompleteTree, [], UsesList),
 	sort(UsesList, NiceUsesList),

	KeyResult = '$depends_on'(P,N,Mod,NiceUsesList,Files),

	inorder_defined(Right,CompleteTree, MIS,InL,RL),
	inorder_defined(Left,CompleteTree, MIS,[KeyResult|RL],OutL).

ultimate_uses([], Tree, Result, Result).
ultimate_uses([(MM:Q)/N | Calls], Tree, CurUsesList, FinalUsesList)
	:-!,
	ultimate_uses([Q/N | Calls], Tree, CurUsesList, FinalUsesList).

ultimate_uses([Q/N | Calls], Tree, CurUsesList, FinalUsesList)
	:-
	member(Q/N, CurUsesList),
	!,
	ultimate_uses(Calls, Tree, CurUsesList, FinalUsesList).

ultimate_uses([Q/N | Calls], Tree, CurUsesList, FinalUsesList)
	:-
	avl_search(Q/N, NewData,Tree),
	(accessCRF(calls,NewData,Q_Calls) ->
		ultimate_uses(Q_Calls, Tree, [Q/N | CurUsesList], QUsesList)
		;
		QUsesList = [Q/N | CurUsesList]
	),
	ultimate_uses(Calls, Tree, QUsesList, FinalUsesList).



/*----------------------------------------------------------------*
 * avl_create(Tree)	-- create an empty tree.
 *----------------------------------------------------------------*/

avl_create(empty).


/*----------------------------------------------------------------*
 * avl_key(Tree,Key)
 * avl_data(Tree,Data)
 * avl_left(Tree,LeftSubTree)
 * avl_right(Tree,RightSubTree)
 *
 * Each of these two argument procedures takes a non-empty avl-tree as
 * its first arguments and unifies the second argument with the obvious
 * component of the node.
 *----------------------------------------------------------------*/

avl_key(Tree,Key) :- arg(1,Tree,Key).
avl_data(Tree,Data) :- arg(2,Tree,Data).
avl_left(Tree,Left) :- arg(3,Tree,Left).
avl_right(Tree,Right) :- arg(4,Tree,Right).

/*----------------------------------------------------------------*
 * avl_insert(Key,Data,InTree,OutTree)
 *
 *	Inserts Key and Data into the avl-tree passed in through InTree
 *	giving a tree which is unified with OutTree.  If the Key is already
 *	present in the tree, then Data replaces the old data value in the
 *	tree.
 *----------------------------------------------------------------*/

avl_insert(Key,Data,InTree,OutTree) 
	:-
	insert(InTree,Key,Data,OutTree,_,InTree),
	!.
avl_insert(Key,Data,T,T) 
	:-
	telling(Current),
	tell(user),
	write('avl_insert failed on '),write(Key:Data),nl,
	height(T,Height),
	nelems(T,N),
	write('Height'=Height),write('  Element Count'=N),nl,
	tell(Current).

	/*---------------------------------------------------
	 |	insert/6
	 |	insert(InTree,Key,Data,OutTree,Longer,TreeTop)
	 |	insert(+,+,+,-,-,+)
	 |	
	 |	insert an item into an avl tree
	 *--------------------------------------------------*/
	% incoming tree is empty:
insert(empty,Key,Data,bn(Key,TreeData,empty,empty),longer,TreeTop) 
	:- !,
	make_new_node(Key,Data,TreeData,TreeTop).

	% incoming tree (node) is a balanced tree node:
insert(bn(TreeKey,TreeData,Left,Right),Key,Data,OutTree,Longer,TreeTop) 
	:-!,
	compare(Cmp,Key,TreeKey),
	insert_b(Cmp,TreeKey,TreeData,Left,Right,Key,Data,OutTree,Longer,TreeTop).

	% incoming tree (node) has its left subtree longer:
insert(ln(TreeKey,TreeData,Left,Right),Key,Data,OutTree,same,TreeTop) 
	:-!,
	compare(Cmp,Key,TreeKey),
	insert_l(Cmp,TreeKey,TreeData,Left,Right,Key,Data,OutTree,TreeTop).

	% incoming tree (node) has its right subtree longer:
insert(rn(TreeKey,TreeData,Left,Right),Key,Data,OutTree,same,TreeTop) 
	:-!,
	compare(Cmp,Key,TreeKey),
	insert_r(Cmp,TreeKey,TreeData,Left,Right,Key,Data,OutTree,TreeTop).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% insert_b/10
	%% insert into a balanced tree node
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
insert_b(=,_,TreeData,Left,Right,Key,Data,bn(Key,NewData,Left,Right),same,TreeTop) 
	:- 
	add_data(Data,TreeData,NewData,TreeTop).

insert_b(<,TreeKey,TreeData,Left,Right,Key,Data,OutTree,Longer,TreeTop) 
	:- !,
	insert(Left,Key,Data,NewLeft,NewLonger,TreeTop),
	insert_bul(NewLonger,
	       		TreeKey,TreeData,NewLeft,Right,
	       		OutTree,Longer,TreeTop).

insert_b(>,TreeKey,TreeData,Left,Right,Key,Data,OutTree,Longer,TreeTop) 
	:-
	insert(Right,Key,Data,NewRight,NewLonger,TreeTop),
	insert_bur(NewLonger,
	       		TreeKey,TreeData,Left,NewRight,
	       		OutTree,Longer,TreeTop).
	
insert_bul(same, Key,Data,Left,Right, bn(Key,Data,Left,Right),same,TreeTop) 
	:- !.

insert_bul(longer, Key,Data,Left,Right, ln(Key,Data,Left,Right),longer,TreeTop).

insert_bur(same, Key,Data,Left,Right, bn(Key,Data,Left,Right),same,TreeTop) 
	:- !.

insert_bur(longer, Key,Data,Left,Right, rn(Key,Data,Left,Right),longer,TreeTop).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% insert_l/9
	%% insert when left subtree is longer
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_l(=,_,TreeData,Left,Right,Key,Data,ln(Key,NewData,Left,Right),TreeTop) 
	:- !,
	add_data(Data,TreeData,NewData,TreeTop).

insert_l(<,TreeKey,TreeData,Left,Right,Key,Data,OutTree,TreeTop) 
	:- !,
	insert(Left,Key,Data,NewLeft,NewLonger,TreeTop),
	insert_lul(NewLonger,
	       TreeKey,TreeData,NewLeft,Right,
	       OutTree,TreeTop).

insert_l(>,TreeKey,TreeData,Left,Right,Key,Data,OutTree,TreeTop) 
	:-
	insert(Right,Key,Data,NewRight,NewLonger,TreeTop),
	insert_lur(NewLonger,
	       TreeKey,TreeData,Left,NewRight,
	       OutTree,TreeTop).

insert_lul(same, Key,Data,Left,Right, ln(Key,Data,Left,Right),TreeTop) 
	:- !.

insert_lul(longer, Key,Data,Left,Right, OutTree,TreeTop) 
	:-
	insert_lfix(Left,Key,Data,Right, OutTree,TreeTop).

insert_lur(same, Key,Data,Left,Right, ln(Key,Data,Left,Right),TreeTop) 
	:- !.

insert_lur(longer, Key,Data,Left,Right, bn(Key,Data,Left,Right),TreeTop).



insert_lfix(ln(KeyL,DataL,LeftL,RightL), Key,Data,Right,
	    bn(KeyL,DataL,LeftL,bn(Key,Data,RightL,Right)),TreeTop) 
	:- !.

insert_lfix(rn(KB,DB,LB,bn(KC,DC,LC,RC)), KA,DA,RA,
	    bn(KC,DC,bn(KB,DB,LB,LC),bn(KA,DA,RC,RA)),TreeTop) 
	:- !.

insert_lfix(rn(KB,DB,LB,ln(KC,DC,LC,RC)), KA,DA,RA,
	    bn(KC,DC,bn(KB,DB,LB,LC),rn(KA,DA,RC,RA)),TreeTop) 
	:- !.

insert_lfix(rn(KB,DB,LB,rn(KC,DC,LC,RC)), KA,DA,RA,
	    bn(KC,DC,ln(KB,DB,LB,LC),bn(KA,DA,RC,RA)),TreeTop).


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% insert_r/9
	%% insert when right subtree is longer
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_r(=,_,TreeData,Left,Right,Key,Data,rn(Key,NewData,Left,Right),TreeTop) 
	:- !,
	add_data(Data,TreeData,NewData,TreeTop).

insert_r(<,TreeKey,TreeData,Left,Right,Key,Data,OutTree,TreeTop) 
	:- !,
	insert(Left,Key,Data,NewLeft,NewLonger,TreeTop),
	insert_rul(NewLonger,
	       TreeKey,TreeData,NewLeft,Right,
	       OutTree,TreeTop).

insert_r(>,TreeKey,TreeData,Left,Right,Key,Data,OutTree,TreeTop) 
	:-
	insert(Right,Key,Data,NewRight,NewLonger,TreeTop),
	insert_rur(NewLonger,
	       TreeKey,TreeData,Left,NewRight,
	       OutTree,TreeTop).


insert_rul(same, Key,Data,Left,Right, rn(Key,Data,Left,Right),TreeTop) 
	:- !.

insert_rul(longer, Key,Data,Left,Right, bn(Key,Data,Left,Right),TreeTop).

insert_rur(same, Key,Data,Left,Right, rn(Key,Data,Left,Right),TreeTop) 
	:- !.

insert_rur(longer, Key,Data,Left,Right, OutTree,TreeTop) 
	:-
	insert_rfix(Right, Key,Data,Left, OutTree,TreeTop).


insert_rfix(rn(KB,DB,LB,RB), KA,DA,LA,
	    bn(KB,DB,bn(KA,DA,LA,LB),RB),TreeTop) 
	:- !.

insert_rfix(ln(KB,DB,bn(KC,DC,LC,RC),RB), KA,DA,LA,
	    bn(KC,DC,bn(KA,DA,LA,LC),bn(KB,DB,RC,RB)),TreeTop) 
	:- !.

insert_rfix(ln(KB,DB,rn(KC,DC,LC,RC),RB), KA,DA,LA,
	    bn(KC,DC,ln(KA,DA,LA,LC),bn(KB,DB,RC,RB)),TreeTop) 
	:- !.

insert_rfix(ln(KB,DB,ln(KC,DC,LC,RC),RB), KA,DA,LA,
	    bn(KC,DC,bn(KA,DA,LA,LC),rn(KB,DB,RC,RB)),TreeTop).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%% NODE MANIPULATION %%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%%%%%%%%%%%%%%%%%%%%%%%
	%% MAKE a NEW NODE
	%%%%%%%%%%%%%%%%%%%%%%%

	%% make_new_node(hates/2,calls(hates,2,m2:loves,2,user,'ken.pro'),TreeData)
make_new_node(Key,Data,TreeData, TreeTop)
	:-
	functor(Data, Index0, _),
	(Index0=assertVHeadRule ->
		Index = varHeadRule
		;
		Index = Index0
	),
	makeCRF(TreeData),
	cref_new_entries(Index, Data, TreeData, TreeTop).

	%% cref_new_entries(calls,calls(hates,2,m2:loves,2,user,'ken.pro'),TreeData, TreeTop)
cref_new_entries(calls, Data, TreeData, TreeTop)
	:-
		%% Goal may be CM:CP
	Data = calls(Pred,Arity,Goal,CArity,DefMod,File),
	setCRF(pred,TreeData,Pred),
	setCRF(arity,TreeData,Arity),
	setCRF(mod,TreeData,DefMod),
	setCRF(files,TreeData,[File]),
	setCRF(calls,TreeData,[Goal/CArity]).

cref_new_entries(fact, fact(Pred, Arity, DefMod, File), TreeData, TreeTop)
	:-
	setCRF(pred,TreeData,Pred),
	setCRF(arity,TreeData,Arity),
	setCRF(mod,TreeData,DefMod),
	setCRF(factcount,TreeData,1),
	setCRF(files,TreeData,[File]).

cref_new_entries(dynamic, dynamic(Pred, Arity, DefMod, File), TreeData, TreeTop)
	:-
	setCRF(pred,TreeData,Pred),
	setCRF(arity,TreeData,Arity),
	setCRF(mod,TreeData,DefMod),
	setCRF(files,TreeData,[File]),
	setCRF(dynamicdecl,TreeData,[DefMod+File]).

cref_new_entries(export, export(Pred, Arity, DefMod, File), TreeData, TreeTop)
	:-
	setCRF(pred,TreeData,Pred),
	setCRF(arity,TreeData,Arity),
	setCRF(mod,TreeData,DefMod),
	setCRF(exported,TreeData,true),
	setCRF(files,TreeData,[File]).

cref_new_entries(import, import(Pred, Arity, ImpMod, File), TreeData, TreeTop)
	:-
	setCRF(pred,TreeData,Pred),
	setCRF(arity,TreeData,Arity),
	setCRF(mod,TreeData,DefMod),
	setCRF(importto,TreeData,[ImpMod]),
	setCRF(files,TreeData,[File]).

cref_new_entries(assert, assert(Pred, Arity, F,N, Mod, File), TreeData, TreeTop)
	:-
	setCRF(pred,TreeData,Pred),
	setCRF(arity,TreeData,Arity),
	setCRF(mod,TreeData,Mod),
	setCRF(whereasserted,TreeData,[F/N+Mod+File]),
	setCRF(files,TreeData,[File]).

cref_new_entries(assertRule, assertRule(Pred, Arity, F,N, Mod, File), TreeData, TreeTop)
	:-
	setCRF(pred,TreeData,Pred),
	setCRF(arity,TreeData,Arity),
	setCRF(mod,TreeData,Mod),
	setCRF(ruleasserted,TreeData,[F/N+Mod+File]),
	setCRF(files,TreeData,[File]).

cref_new_entries(varHeadRule, assertVHeadRule(Body, F,N, Mod, File), TreeData, TreeTop)
	:-
	setCRF(pred,TreeData,varHeadRule),
	setCRF(arity,TreeData,0),
	setCRF(mod,TreeData,Mod),
	setCRF(ruleasserted,TreeData,[vH=Body+F/N+Mod+File]),
	setCRF(files,TreeData,[File]).

cref_new_entries(minimal, minimal(Pred, Arity, TreeData), TreeData, TreeTop)
	:-
	setCRF(pred,TreeData,Pred),
	setCRF(arity,TreeData,Arity).

cref_new_entries(min_rule, min_rule(Pred, Arity, TreeData), TreeData, TreeTop)
	:-
	setCRF(pred,TreeData,Pred),
	setCRF(arity,TreeData,Arity),
	setCRF(clausecount,TreeData,1).

cref_new_entries(inc_clause_cnt, inc_clause_cnt(Pred, Arity, DefMod, File), TreeData, TreeTop)
	:-
	setCRF(pred,TreeData,Pred),
	setCRF(arity,TreeData,Arity),
	setCRF(mod,TreeData,DefMod),
	setCRF(files,TreeData,[File]),
	setCRF(clausecount,TreeData,1).

cref_new_entries(inc_called_cnt, inc_called_cnt(Pred, Arity, CalledBy, DefMod, File), TreeData, TreeTop)
	:-
	setCRF(pred,TreeData,Pred),
	setCRF(arity,TreeData,Arity),
	setCRF(mod,TreeData,DefMod),
	setCRF(files,TreeData,[File]),
	setCRF(calledcount,TreeData,1),
	setCRF(calledby,TreeData,[CalledBy]).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% INSERT DATA INTO AN EXISTING NODE (DESTRUCTIVELY)
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_data(Data, TreeData, TreeData, TreeTop)
	:-
	functor(Data, Index0, _),
	(Index0 = assertVHeadRule ->
		Index = varHeadRule
		;
		Index = Index0
	),
	cref_add_entries(Index, Data, TreeData, TreeTop).

cref_add_entries(fact, fact(Pred, Arity, DefMod, File), TreeData, TreeTop)
	:-!,
	accessCRF(files,TreeData, OldFiles),
	(dmember(File, OldFiles) ->
		true
		;
		setCRF(files,TreeData,[File | OldFiles])
	),
	accessCRF(factcount, TreeData, OldCount),
	NewCount is OldCount+1,
	setCRF(factcount, TreeData, NewCount).

cref_add_entries(dynamic, dynamic(Pred, Arity, DefMod, File), TreeData, TreeTop)
	:-!,
	accessCRF(files,TreeData, OldFiles),
	(dmember(File, OldFiles) ->
		true
		;
		setCRF(files,TreeData,[File | OldFiles])
	),
	accessCRF(dynamicdecl,TreeData, DynamicList),
	(dmember(DefMod+_, DynamicList) ->
		ModAndDynamicEntry = DefMod+ModDynamic,
		dmember(ModAndDynamicEntry, DynamicList),
		(dmember(Pred/Arity, ModDynamic) ->
			true
			;
			NewModDynamic = [Pred/Arity | ModDynamic],
			mangle(2, ModAndDynamicEntry, NewModDynamic)
		)
		;
		NewDynamicList = [DefMod + [Pred/Arity] | DynamicList],
		setCRF(dynamicdecl,TreeData, NewDynamicList)
	).

cref_add_entries(calls, Data, TreeData, TreeTop)
	:-!,
	Data = calls(Pred,Arity,Goal,CArity,DefMod,File),
	accessCRF(calls,TreeData,OldCalls),
	(dmember(Goal/CArity, OldCalls) ->
		true
		;
		setCRF(calls,TreeData,[Goal/CArity | OldCalls])
	),
	accessCRF(files,TreeData, OldFiles),
	(dmember(File, OldFiles) ->
		true
		;
		setCRF(files,TreeData,[File | OldFiles])
	).

cref_add_entries(export, export(Pred, Arity, DefMod, File), TreeData, TreeTop)
	:-
	setCRF(exported,TreeData,true),
	accessCRF(files,TreeData, OldFiles),
	(dmember(File, OldFiles) ->
		true
		;
		setCRF(files,TreeData,[File | OldFiles])
	).

cref_add_entries(import, import(Pred, Arity, ImpMod, File), TreeData, TreeTop)
	:-
	accessCRF(importto,TreeData,IMods),
	setCRF(importto,TreeData,[ImpMod | IMods]),
	(dmember(File, OldFiles) ->
		true
		;
		setCRF(files,TreeData,[File | OldFiles])
	).

cref_add_entries(assert, assert(Pred, Arity, F,N, Mod, File), TreeData, TreeTop)
	:-
	accessCRF(whereasserted,TreeData,ALcns),
	(member(F/N+Mod+File, ALcns) -> NewALcns = ALcns ; NewALcns = [F/N+Mod+File | ALcns]), 
	setCRF(whereasserted,TreeData,NewALcns),
	accessCRF(files,TreeData,OldFiles),
	(dmember(File, OldFiles) ->
		true
		;
		setCRF(files,TreeData,[File | OldFiles])
	).

cref_add_entries(assertRule, assertRule(Pred, Arity, F,N, Mod, File), TreeData, TreeTop)
	:-
	accessCRF(ruleasserted,TreeData,ALcns),
	(member(F/N+Mod+File, ALcns) -> NewALcns = ALcns ; NewALcns = [F/N+Mod+File | ALcns]), 
	setCRF(ruleasserted,TreeData,NewALcns),
	accessCRF(files,TreeData,OldFiles),
	(dmember(File, OldFiles) ->
		true
		;
		setCRF(files,TreeData,[File | OldFiles])
	).

cref_add_entries(varHeadRule, assertVHeadRule(Body, F,N, Mod, File), TreeData, TreeTop)
	:-
	accessCRF(ruleasserted,TreeData,ALcns),
	NewALcns = [vH=Body+F/N+Mod+File | ALcns], 
	setCRF(ruleasserted,TreeData,NewALcns),
	accessCRF(files,TreeData,OldFiles),
	(dmember(File, OldFiles) ->
		true
		;
		setCRF(files,TreeData,[File | OldFiles])
	).

cref_add_entries(inc_clause_cnt, inc_clause_cnt(Pred, Arity, DefMod, File), TreeData, TreeTop)
	:-!,
	accessCRF(clausecount, TreeData, OldClauseCount),
	NewClauseCount is OldClauseCount + 1,
	setCRF(clausecount, TreeData, NewClauseCount).
	
cref_add_entries(inc_called_cnt, inc_called_cnt(Pred, Arity, CalledBy, DefMod, File), TreeData, TreeTop)
	:-!,
	accessCRF(calledcount, TreeData, OldCalledCount),
	NewCalledCount is OldCalledCount + 1,
	setCRF(calledcount, TreeData, NewCalledCount),
	accessCRF(calledby, TreeData, OldCalledBy),
	(dmember(CalledBy, OldCalledBy) ->
		true
		;
		setCRF(calledby, TreeData, [CalledBy | OldCalledBy])
	).

extend_basis(AddlBasis, TreeData)
	:-
	accessCRF(basis, TreeData, CurBasis),
	(present_already(CurBasis, AddlBasis) ->
		true
		;
		CurBasis = [nil | CurTail],
		NewTail = [AddlBasis | CurTail],
		mangle(2, CurBasis, NewTail)
	).

present_already([], Item) :- !, fail.
present_already([Item | RestCurBasis], Item)
	:-!.
present_already([Head | RestCurBasis], Item)
	:-
	accessCRF(pred, Head, Pred),
	accessCRF(pred, Item, Pred),
	accessCRF(arity, Head, Arity),
	accessCRF(arity, Item, Arity),
	!.
present_already([_ | RestCurBasis], Item)
	:-
	present_already(RestCurBasis, Item).

locate_basis(TreeTop, Goal/CArity, Basis)
	:-
	avl_search(Goal/CArity,DataNode,TreeTop),
	!,
	accessCRF(basis,DataNode,Basis).

locate_basis(TreeTop, Goal/CArity, Basis)
	:-
	avl_insert(Goal/CArity, minimal(Goal,CArity,Basis),TreeTop,NewCallsTree),
	setCallsTree(NewCallsTree).


/*---------------------------------------------------------------------------*
 * avl_search(Key,Data,Tree)
 *
 *	Tree is searched in for Key.  Data is unified with the corresponding
 *	data value if found.  If Key is not found, avl_search will fail.
 *---------------------------------------------------------------------------*/

avl_search(_,_,empty) :-
	!,
	fail.
avl_search(Key,Data,Tree) :-
	avl_key(Tree,TreeKey),
	compare(Cmp,Key,TreeKey),
	search(Cmp,Key,Data,Tree).

search(=,_,Data,Tree) :-
	!,
	avl_data(Tree,Data).
search(<,Key,Data,Tree) :-
	!,
	avl_left(Tree,Left),
	avl_search(Key,Data,Left).
search(>,Key,Data,Tree) :-
	avl_right(Tree,Right),
	avl_search(Key,Data,Right).

/***************
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% FURTHER CREF PROCESSING: UNDERLYING ROUTINES
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% PROPAGATING DEPENDENCIES
	%% Walk the tree; at each node, collect
	%% the predicates that node depends on
	%% (get_deps/_), and install in dependson slot
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% Walk the tree:
pass_depends(empty,_) 
	:- !.
pass_depends(Tree,TreeTop) 
	:-
	avl_data(Tree,DataNode),
	collect_depends(DataNode,[],TreeTop),
	avl_right(Tree,Right),
	pass_depends(Right,TreeTop),
	avl_left(Tree,Left),
	pass_depends(Left,TreeTop).

	%% If depends on slot \= nil, skip; this is because
	%% we might have already filled in this slot due to
	%% the earlier recursive following of a dependency:
collect_depends(DataNode,_,TreeTop)
	:-
	accessCRF(dependson, DataNode, Deps),
	Deps \= nil,
	!.
collect_depends(DataNode,CallPath,TreeTop)
	:-
	accessCRF(calls, DataNode, CallsList),
	get_deps(CallsList, TreeTop, CallPath, [], InitDepList),
	sort(InitDepList, DepList),
	setCRF(dependson, DataNode, DepList).

	%% For each predicate P/N called by this node, collect
	%% the predicates on which P/N depends (recursively
	%% follow the dependencies in the tree):

get_deps([], _, _, Accum, Accum).

	%% Avoid loops:
get_deps([P/N | CallsList], TreeTop, CallPath, Accum, DepList)
	:-
	dmember(P/N, Accum),
	!,
	get_deps(CallsList, TreeTop, CallPath, Accum, DepList).

	%% Avoid loops:
get_deps([P/N | CallsList], TreeTop, CallPath, Accum, DepList)
	:-
	dmember(P/N, CallPath),
	!,
	get_deps(CallsList, TreeTop, CallPath, Accum, DepList).

get_deps([P/N | CallsList], TreeTop, CallPath, Accum, DepList)
	:-
	avl_search(P/N, ThisData,TreeTop),
	!,
	accessCRF(calls, ThisData, ThisCalls),
	(ThisCalls \= [] ->
		collect_depends(ThisData,[P/N | CallPath], TreeTop),
		accessCRF(dependson, ThisData, PNCallsList),
		union([P/N | Accum], PNCallsList, NewAccum)
		;
		NewAccum = [P/N | Accum]
	),
	get_deps(CallsList, TreeTop, CallPath, NewAccum, DepList).

get_deps([P/N | CallsList], TreeTop, CallPath, Accum, DepList)
	:-
	all_procedures(MM, P, N, _),
	dmember(MM, [builtins, sio]),
	!.

get_deps([P/N | CallsList], TreeTop, CallPath, Accum, DepList)
	:-
	asserted_by_prog(P,N,_,_),
	!,
	NewAccum = [P/N | Accum],
	get_deps(CallsList, TreeTop, CallPath, NewAccum, DepList).

get_deps([P/N | CallsList], TreeTop, CallPath, Accum, DepList)
	:-
	als_advise('Bad deps item: %t/%t -- skipping.\n',[P,N]),
	als_advise('\tCallPath context was: %t\n',[CallPath]).

:- dynamic('$asserted'/4).

asserted_by_prog(P,N,_,_)
	:-
	'$asserted'(P,N,_,_).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% USER_DEFINED SUPPORT ROUTINES
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

end_walk(Pred, MIS, OutS, Output)
	:-
	write_term(OutS,Output,[]),
	nl(OutS).

walk_process(Tree, TreeTop, ProcessPred, MIS, M, Result) 
	:-
	walk_process(Tree, TreeTop, ProcessPred, MIS, M, Result, []).

walk_process(empty, TreeTop, ProcessPred, MIS, M, End, End) 
	:- !,
	exec_process(empty, ProcessPred, empty, TreeTop, MIS, M, End, End).

walk_process(Tree, TreeTop, ProcessPred, MIS, M, Input, Tail) 
	:-
	avl_data(Tree, DataNode),
	avl_key(Tree,Key),
	exec_process(DataNode, ProcessPred, Key, TreeTop, MIS, M, Input, NodeOut),
	avl_left(Tree,  Left),
	avl_right(Tree, Right),
	walk_process(Right, TreeTop, ProcessPred,  MIS, M, NodeOut, InterTail),
	walk_process(Left,  TreeTop, ProcessPred,  MIS, M, InterTail, Tail).

exec_process(Item, ProcessPred, Key, TreeTop, MIS, M, Out, Tail)
	:-
	Call =.. [ProcessPred, Item, TreeTop, Key, MIS, Out, Tail],
	call(M:Call).
	
ino(empty, _, _, _, L, L) :-!.
ino(Node, TreeTop, Key, MIS, [Key | Tail], Tail).

*************/


endmod.

/*===============================================================*
	Expanded below:
module cref.
:-
defStruct(crfCRF, [
        propertiesList = [
		pred,
		arity,
		mod,
		files,
		exported,
		calls,
		basis,
		calledby,
		calledcount,
		factcount,
		clausecount,
		depthcount,
		callinmod,
		importto,
		dependson,
		whereasserted,   % facts
		dynamicdecl,
		ruleasserted
	],
	accessPred =    accessCRF,
        setPred =       setCRF,
        makePred =      makeCRF,
        structLabel =   crfCRF
    ]).
endmod.
*===============================================================*/



/*-------------------------------------------------------------*
                    crefstrt.pro
               defStruct Type definitions generated from file:
                    crefstrt.typ
               by ALS defStruct Type Generator
               Macros written to file: crefstrt.mac
 *-------------------------------------------------------------*/

module utilities.
use cref.
endmod.

module cref.
use utilities.


%--- crefStructs defStruct ---

export accessCRF/3.
export setCRF/3.
accessCRF(pred,_A,_B) :- arg(1,_A,_B).
setCRF(pred,_A,_B) :- mangle(1,_A,_B).

accessCRF(arity,_A,_B) :- arg(2,_A,_B).
setCRF(arity,_A,_B) :- mangle(2,_A,_B).

accessCRF(mod,_A,_B) :- arg(3,_A,_B).
setCRF(mod,_A,_B) :- mangle(3,_A,_B).

accessCRF(files,_A,_B) :- arg(4,_A,_B).
setCRF(files,_A,_B) :- mangle(4,_A,_B).

accessCRF(exported,_A,_B) :- arg(5,_A,_B).
setCRF(exported,_A,_B) :- mangle(5,_A,_B).

accessCRF(calls,_A,_B) :- arg(6,_A,_B).
setCRF(calls,_A,_B) :- mangle(6,_A,_B).

accessCRF(basis,_A,_B) :- arg(7,_A,_B).
setCRF(basis,_A,_B) :- mangle(7,_A,_B).

accessCRF(calledby,_A,_B) :- arg(8,_A,_B).
setCRF(calledby,_A,_B) :- mangle(8,_A,_B).

accessCRF(calledcount,_A,_B) :- arg(9,_A,_B).
setCRF(calledcount,_A,_B) :- mangle(9,_A,_B).

accessCRF(factcount,_A,_B) :- arg(10,_A,_B).
setCRF(factcount,_A,_B) :- mangle(10,_A,_B).

accessCRF(clausecount,_A,_B) :- arg(11,_A,_B).
setCRF(clausecount,_A,_B) :- mangle(11,_A,_B).

accessCRF(depthcount,_A,_B) :- arg(12,_A,_B).
setCRF(depthcount,_A,_B) :- mangle(12,_A,_B).

accessCRF(callinmod,_A,_B) :- arg(13,_A,_B).
setCRF(callinmod,_A,_B) :- mangle(13,_A,_B).

accessCRF(importto,_A,_B) :- arg(14,_A,_B).
setCRF(importto,_A,_B) :- mangle(14,_A,_B).

accessCRF(dependson,_A,_B) :- arg(15,_A,_B).
setCRF(dependson,_A,_B) :- mangle(15,_A,_B).

accessCRF(whereasserted,_A,_B) :- arg(16,_A,_B).
setCRF(whereasserted,_A,_B) :- mangle(16,_A,_B).

accessCRF(dynamicdecl,_A,_B) :- arg(17,_A,_B).
setCRF(dynamicdecl,_A,_B) :- mangle(17,_A,_B).

accessCRF(ruleasserted,_A,_B) :- arg(18,_A,_B).
setCRF(ruleasserted,_A,_B) :- mangle(18,_A,_B).

export makeCRF/1.
makeCRF(_A) :-
        _A=..[crfCRF,_B,_C,user,[],false,[],nil,[],0,0,0,0,[],[],nil,[],[],[]].

export makeCRF/2.
makeCRF(_A,_B) :-
        struct_lookup_subst(
            [pred,arity,mod,files,exported,calls,basis,calledby,calledcount,
                factcount,clausecount,depthcount,callinmod,importto,
                dependson,whereasserted,dynamicdecl],ruleasserted,
            [_C,_D,user,[],false,[],nil,[],0,0,0,0,[],[],nil,[],[],[]],_B,_E),
        _A=..[crfCRF|_E].

export xmakeCRF/2.
xmakeCRF(crfCRF(_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M,_N,_O,_P,_Q,_R),
    [_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M,_N,_O,_P,_Q,_R]).


%--- cshls defStruct ---

export accessCRSH/3.
export setCRSH/3.
accessCRSH(cycle,_A,_B) :- arg(1,_A,_B).
setCRSH(cycle,_A,_B) :- mangle(1,_A,_B).

accessCRSH(history,_A,_B) :- arg(2,_A,_B).
setCRSH(history,_A,_B) :- mangle(2,_A,_B).

accessCRSH(outputs,_A,_B) :- arg(3,_A,_B).
setCRSH(outputs,_A,_B) :- mangle(3,_A,_B).

accessCRSH(suite,_A,_B) :- arg(4,_A,_B).
setCRSH(suite,_A,_B) :- mangle(4,_A,_B).

accessCRSH(options,_A,_B) :- arg(5,_A,_B).
setCRSH(options,_A,_B) :- mangle(5,_A,_B).

accessCRSH(context_stack,_A,_B) :- arg(6,_A,_B).
setCRSH(context_stack,_A,_B) :- mangle(6,_A,_B).

accessCRSH(in_stream,_A,_B) :- arg(7,_A,_B).
setCRSH(in_stream,_A,_B) :- mangle(7,_A,_B).

accessCRSH(out_stream,_A,_B) :- arg(8,_A,_B).
setCRSH(out_stream,_A,_B) :- mangle(8,_A,_B).

export makeCRSH/1.
makeCRSH(_A) :- _A=..[crfCRSH,0,[],[],unknown,[],[],user_input,user_output].

export makeCRSH/2.
makeCRSH(_A,_B) :-
        struct_lookup_subst(
            [cycle,history,outputs,suite,options,context_stack,in_stream,
                out_stream],
            [0,[],[],unknown,[],[],user_input,user_output],_B,_C),
        _A=..[crfCRSH|_C].

export xmakeCRSH/2.
xmakeCRSH(crfCRSH(_A,_B,_C,_D,_E,_F,_G,_H),[_A,_B,_C,_D,_E,_F,_G,_H]).

/*===============================================================*
	Expanded below:
module cref.
:-
defStruct(crfMI, [
        propertiesList = [
                files,
                files_mods,
                files_d_preds,
                files_c_preds,
                mods,
                mods_files,
                mods_use,
                mods_d_preds,
                mods_c_preds,
                mods_exp_preds,
                mods_imp_preds,
		op_decls,
		lib_files
        ],
        accessPred =    accessMI,
        setPred =       setMI,
        makePred =      makeMI,
        structLabel =   crfMI
    ]).

endmod.
*===============================================================*/

%--- mi defStruct ---

export accessMI/3.
export setMI/3.
accessMI(files,_A,_B) :- arg(1,_A,_B).
setMI(files,_A,_B) :- mangle(1,_A,_B).

accessMI(files_mods,_A,_B) :- arg(2,_A,_B).
setMI(files_mods,_A,_B) :- mangle(2,_A,_B).

accessMI(files_d_preds,_A,_B) :- arg(3,_A,_B).
setMI(files_d_preds,_A,_B) :- mangle(3,_A,_B).

accessMI(files_c_preds,_A,_B) :- arg(4,_A,_B).
setMI(files_c_preds,_A,_B) :- mangle(4,_A,_B).

accessMI(mods,_A,_B) :- arg(5,_A,_B).
setMI(mods,_A,_B) :- mangle(5,_A,_B).

accessMI(mods_files,_A,_B) :- arg(6,_A,_B).
setMI(mods_files,_A,_B) :- mangle(6,_A,_B).

accessMI(mods_use,_A,_B) :- arg(7,_A,_B).
setMI(mods_use,_A,_B) :- mangle(7,_A,_B).

accessMI(mods_d_preds,_A,_B) :- arg(8,_A,_B).
setMI(mods_d_preds,_A,_B) :- mangle(8,_A,_B).

accessMI(mods_c_preds,_A,_B) :- arg(9,_A,_B).
setMI(mods_c_preds,_A,_B) :- mangle(9,_A,_B).

accessMI(mods_exp_preds,_A,_B) :- arg(10,_A,_B).
setMI(mods_exp_preds,_A,_B) :- mangle(10,_A,_B).

accessMI(mods_imp_preds,_A,_B) :- arg(11,_A,_B).
setMI(mods_imp_preds,_A,_B) :- mangle(11,_A,_B).

accessMI(op_decls,_A,_B) :- arg(12,_A,_B).
setMI(op_decls,_A,_B) :- mangle(12,_A,_B).

accessMI(lib_files,_A,_B) :- arg(13,_A,_B).
setMI(lib_files,_A,_B) :- mangle(13,_A,_B).

export makeMI/1.
makeMI(_A) :- _A=..[crfMI,[],[],[],[],[],[],[],[],[],[],[],[],[]].

export makeMI/2.
makeMI(_A,_B) :-
        struct_lookup_subst(
            [files,files_mods,files_d_preds,files_c_preds,mods,mods_files,
                mods_use,mods_d_preds,mods_c_preds,mods_exp_preds,
                mods_imp_preds,op_decl,libfiles],
            [[],[],[],[],[],[],[],[],[],[],[],[],[]],_B,_C),
        _A=..[crfMI|_C].

export xmakeMI/2.
xmakeMI(crfMI(_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M),
    [_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M]).

endmod.

module utilities.
typeProperties(crefStructs,
    [pred,arity,mod,user,files,[],exported,false,calls,[],basis,nil,calledby,
        [],calledcount,0,factcount,0,clausecount,0,depthcount,0,callinmod,[],
        importto,[],dependson,nil,whereasserted,[]]).
noteOptionValue(crefStructs,_A,_B,_C) :- setCRF(_A,_C,_B).
typeProperties(cshls,
    [cycle,0,history,[],outputs,[],suite,unknown,options,[],context_stack,[],
        in_stream,user_input,out_stream,user_output]).
noteOptionValue(cshls,_A,_B,_C) :- setCRSH(_A,_C,_B).
typeProperties(mi,
    [files,[],files_mods,[],files_d_preds,[],files_c_preds,[],mods,[],
        mods_files,[],mods_use,[],mods_d_preds,[],mods_c_preds,[],
        mods_exp_preds,[],mods_imp_preds,[]]).
noteOptionValue(mi,_A,_B,_C) :- setMI(_A,_C,_B).
endmod.

    %% MUST be here, after defs of  makeCRSH, makeMI;
    %% These goals run during loading:
module cref.
:-make_gv('CallsTree'),avl_create(T),setCallsTree(T).
:-make_gv('ShellStruct'), makeCRSH(S), setShellStruct(S).
:-make_gv('MiscInfo'), makeMI(S), setMiscInfo(S).
endmod.
