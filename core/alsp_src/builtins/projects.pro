/*=============================================================*
 |				projects.pro
 |		Copyright (c) 1998 Applied Logic Systems, Inc.
 |
 |			Prolog Project Management
 |			"$Id: projects.pro,v 1.5 1998/05/18 01:36:56 ken Exp $"
 *=============================================================*/

module alsdev.
use tk_alslib.

check_root([], '.') :-!.
check_root([Head | Elts], ProjectDirList)
	:-
	sys_env(OS,MinorOS,Proc),
	(OS = mswin32 ->
		repair_path(Head, FixedHead)
		;
		Head = FixedHead
	),
	ProjectDirList = [FixedHead | Elts].

check_dir_root(Head, Head)
	:-
	sub_atom(Head,K,1,0,'/'),
	!,
	'$uia_pokeb'(Head,K,0'\\).
check_dir_root(Head, Head).


export load_project_file/2.
load_project_file(InProjectDir, BaseFile)
	:-
	check_root(InProjectDir, ProjectDirList),
	join_path(ProjectDirList, ProjectDir),
	file_extension(FF,XX,BaseFile),
	file_extension(FF,XX,RedoneFile),

	change_cwd(ProjectDir),
	printf('Loading project from file %t \n', [BaseFile]),
	get_curProject(OldProject),
	close_previous_project(OldProject),
	set_curProject([]),
	append(ProjectDirList, [RedoneFile], FileList),
	join_path(FileList, TheFile),
	printf('Loading project from file %t \n', [TheFile]),

%% Ought to be able to use:
%	grab_terms(TheFile, CurProject),
%% But it has problems (??), so:
	open(TheFile, read, SS, []),
	read_terms(SS, CurProject),
	close(SS),

	set_curProject(CurProject),
	load_the_project(CurProject, BaseFile),
	listener_prompt.

	/*-----------------------------------------------------------*
	 |	name = project name
	 |	search_dirs  = list of dirs to search
	 |	search_trees = list of roots of dirs to search as trees
	 |
	 |	prolog_files = list of prolog files (no extensions)
	 |	typ_files    = list of *.typ files
	 |  
	 |	initialization_goal    = 0-ary application initialization call
	 *-----------------------------------------------------------*/
load_the_project(ProjList, BaseFile)
	:-
	check_default(ProjList, name,  BaseFile, ProjName),
	check_default(ProjList, search_dirs,  [], SearchDirs),
	check_default(ProjList, search_trees,  [], SearchTrees),
	check_default(ProjList, prolog_files,  [], PrologFiles),
	check_default(ProjList, typ_files,  [], TypFiles),
	check_default(ProjList, initialization_goal,  true, InitGoal),

	assert_searchdirs(SearchDirs),
%	setup_searchtrees(SearchTrees),

	consult_files(PrologFiles),
	consult_files(TypFiles),

	printf('Project %t loaded.\n', [ProjName]),
	printf('Initialization goal is \n\t%t\n\n', [InitGoal]).

close_previous_project(OldProject).


assert_searchdirs([]).
assert_searchdirs([Dir | SearchDirs])
	:-
	builtins:assertz(searchdir(Dir)),
	assert_searchdirs(SearchDirs).
assert_searchdirs(Dir)
	:-
	atom(Dir),
	Dir \= [],
	builtins:assertz(searchdir(Dir)).

consult_files(L) :- consult(L).

add_mult_files(PrevFiles, ListBoxWin)
	:-
	files('*.pro', ProFiles),
	files('*.pl',  PLFiles),
	list_diff(ProFiles, PrevFiles, NewProFiles),
	list_diff(PLFiles, PrevFiles, NewPLFiles),
	append(NewProFiles, NewPLFiles, ChoiceFiles),
	Options = [mode=multiple, title='Choose Files'],
	popup_select_items(shl_tcli, ChoiceFiles, Options, ChoiceList),
	tcl_call(shl_tcli, [merge_into_listbox, ChoiceList, ListBoxWin], _).


	%% 
save_project(Title, ProjFile, Start, ProFiles, SDirs)
	:-
	atomread(ProjFile, PProjFile),
	(file_extension(BaseName, _, PProjFile) ->
		true
		;
		BaseName = PProjFile
	),
	file_extension(BaseName, ppj, OutputFile),

		%% do check & query user:
	(exists_file(OutputFile) ->
		catenate(['File ',OutputFile,' already exists! Continue?'], Msg),
		yes_no_dialog(shl_tcli, Msg, 'File Exists', Answer),
write(save_project_ans=Answer),nl,flush_output,
		(Answer = 'Yes' ->
			fin_save_project(OutputFile,Title, ProjFile, Start, ProFiles, SDirs)
			;
			true
		)
		;
		fin_save_project(OutputFile,Title, ProjFile, Start, ProFiles, SDirs)
	).

fin_save_project(OutputFile,Title, ProjFile, Start, ProFiles, SDirs)
	:-
	open(OutputFile, write, OStr, []),
	write_clause(OStr, name=Title),
	write_clause(OStr, search_dirs=SDirs),
	write_clause(OStr, prolog_files=ProFiles),
	write_clause(OStr, initialization_goal=Start),
	close(OStr).






export open_project_file/2.

open_project_file(InProjectDir, BaseFile)
	:-
	check_root(InProjectDir, ProjectDirList),
	join_path(ProjectDirList, ProjectDir),
	file_extension(FF,XX,BaseFile),
	file_extension(FF,XX,RedoneFile),

	change_cwd(ProjectDir),
	printf('Loading project from file %t \n', [BaseFile]),
	get_curProject(OldProject),
	close_previous_project(OldProject),
	set_curProject([]),
	append(ProjectDirList, [RedoneFile], FileList),
	join_path(FileList, TheFile),
	printf('Loading project from file %t \n', [TheFile]),

%% Ought to be able to use:
%	grab_terms(TheFile, CurProject),
%% But it has problems (??), so:
	open(TheFile, read, SS, []),
	read_terms(SS, CurProject),
	close(SS),

	set_curProject(CurProject),
	open_the_project(CurProject, BaseFile),
	listener_prompt.

open_the_project(ProjList, BaseFile)
	:-
	check_default(ProjList, name,  BaseFile, ProjName),
	check_default(ProjList, search_dirs,  [], SearchDirs),
	check_default(ProjList, search_trees,  [], SearchTrees),
	check_default(ProjList, prolog_files,  [], PrologFiles),
	check_default(ProjList, typ_files,  [], TypFiles),
	check_default(ProjList, initialization_goal,  true, InitGoal),

	sys_env(OS,MinorOS,Proc),
	bld_skel(OS, BldCmd),
	tcl_call(shl_tcli, [display_project, ProjName, BaseFile, InitGoal, 
						PrologFiles, SearchDirs, OS, MinorOS, BldCmd], _).



export build_project/0.
build_project
	:-
	tcl_call(shl_tcli, ['.ppj_spec.filename.entry', get], ProjFile),
	grab_terms(ProjFile, ProjectDesc),
	sys_env(OS,MinorOS,Proc),
	temp_file_name(OS,BldFile),
	open(BldFile, write, BSt, []),

	dmember(package_name = PackageName, ProjectDesc),
	catenate(xx_bld_, PackageName, BldPredName),
	printf(BSt, '%t :- \n\t', [BldPredName], [quoted(true)]),
	dmember(search_dirs = SearchDirs, ProjectDesc),
	add_search_dirs_cl(SearchDirs, OS, BSt),

	dmember(prolog_files = PrologFiles, ProjectDesc),
	printf(BSt, '\t%t,\n', [PrologFiles], [quoted(true)]),

	dmember(package_start_goal = PStG, ProjectDesc),
	dmember(lib_files = ProjLibFiles, ProjectDesc),
	StdLibFiles = [miscterm,msc_ioin,strctutl,strings,tcl_sppt,tk_alslib],
	append(ProjLibFiles,StdLibFiles,LF0),
		%% sort to remove duplicates:
	sort(LF0, LibFiles),
	SaveOptions = [start_goal(PStG),select_lib(library,LibFiles)],

	printf(BSt, '\tsave_image(%t,%t).\n', [PackageName,SaveOptions], [quoted(true)]),

	close(BSt),

	bld_skel(OS, BldPattern),
	open(atom(Cmd), write, CS, []),
	printf(CS, BldPattern, [BldFile, BldPredName]),
	close(CS),
	printf('Calling: %t \n', [Cmd]),
	system(Cmd),
	listener_prompt.




	%% bld_skel(unix, Pattern)
	%% printf(Cmd,   Pattern, [<BldFile>, BldPredName>])
	%%
bld_skel(unix, 'kalspro_b -b -giac  -no_dot_alspro %t -g %t ').

%bld_skel(unix, 'kalspro_b -b -quiet -giac  -no_dot_alspro %t -g %t ').
bld_skel(mswin32, 'ALS Prolog Base.exe -b -quiet -giac  -no_dot_alspro %t -g %t ').
bld_skel(macos, 'ALS Prolog Base -b -quiet -giac  -no_dot_alspro %t -g %t ').



add_search_dirs_cl([], OS, BSt).
add_search_dirs_cl([DirList | SearchDirs], OS, BSt)
	:-
	join_path(OS, DirList, Path),
	printf(BSt, '\tbuiltins:assert(searchdir(%t)),\n', [Path], [quoted(true)]),
	add_search_dirs_cl(SearchDirs, OS, BSt).

temp_file_name(mswin32, 'A19ztmp.bat') :-!.
temp_file_name(_, 'A19ztmp').

endmod.
