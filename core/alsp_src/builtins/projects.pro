/*=============================================================*
 |				projects.pro
 |		Copyright (c) 1998 Applied Logic Systems, Inc.
 |
 |			Prolog Project Management
 |			"$Id: projects.pro,v 1.2 1998/03/14 02:05:24 ken Exp $"
 *=============================================================*/

module alsdev.

	%% Temporary: Remove when projects are made visible:
export ppj/0.
ppj
	:-
	extend_main_menubar('Projects',
		[ 'Open Project' + tcl(open_project) ]
	).

export tp/1.
tp(X) :-
	grab_terms('pm.ppj', X).

open_project_file(ProjectFilePath,BaseFile)
	:-
	printf('Loading project from file %t [%t]\n', [BaseFile,ProjectFilePath]),
	get_curProject(OldProject),
write(op=OldProject),nl,
	close_previous_project(OldProject),
write(done_close),nl,
	set_curProject([]),
write(call(gt(ProjectFilePath))),nl,flush_output,
write(zing),nl,
	grab_terms(ProjectFilePath, CurProject),
write(gt=CurProject),nl,flush_output,
	set_curProject(CurProject),
	open_the_project(CurProject, BaseFile).

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
open_the_project(ProjList, BaseFile)
	:-
write(open_the_project(ProjList, BaseFile)),nl,flush_output,

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


consult_files([]).
consult_files([File | PrologFiles])
	:-
	(exists_file(File) -> 
		consult(File) 
		; 
		printf(user_output, 'Can\'t find file %t .. skipping ..\n', [File])
	),
	consult_files(PrologFiles).
consult_files(File)
	:-
	atom(File),
	File \= [],
	consult(File).


endmod.
