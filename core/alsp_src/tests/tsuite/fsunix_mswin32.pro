:-[test].

test_fsunix_mswin32
	:-
	sys_env(OS,_,_),
	do_test_fs(OS).

do_test_fs(OS)
	:-
	test([
		test_make_subdir1,
		test_make_subdir2,
		test_recursive_dir_path,
		test_recursive_dir_paths,
	true]).

do_test_fs(OS)
	:-
	test([
	true]).

test_make_subdir1
	:-
	(remove_subdir(myNewTestSubdir),! ; true),
	test([
	(get_cwd(TestDir),
	    make_subdir(myNewTestSubdir), 
	    path_directory_tail(SubdirPath, TestDir, myNewTestSubdir),
	    change_cwd(myNewTestSubdir),
	    get_cwd(ThisPath),
	    SubdirPath == ThisPath,
	    change_cwd('..')),
		%% next make_subdir fails because myNewTestSubdir exists:
	(not(make_subdir(myNewTestSubdir))),
	true]),
	(remove_subdir(myNewTestSubdir),! ; true).

test_make_subdir2 :-
	(remove_subdir(myNewTestSubdir),! ; true),
	test([
	(get_cwd(TestDir),
	    make_subdir(myNewTestSubdir,457), 
	    file_status(myNewTestSubdir, Status),
	    member(permissions=Permissions, Status),
	    Permissions = [read,write,execute],
	    path_directory_tail(SubdirPath, TestDir, myNewTestSubdir),
	    change_cwd(myNewTestSubdir),
	    get_cwd(ThisPath),
	    SubdirPath == ThisPath,
	    change_cwd('..')),
	(not(make_subdir(myNewTestSubdir))),
	true]),
	(remove_subdir(myNewTestSubdir),! ; true).

test_recursive_dir_path :-
	get_cwd(TestDir),
	clean_dirs(TestDir, [dir1,dir2,dir3], _),

	test([
	    (Path_List = [dir1,dir2,dir3], 
		recursive_dir_path(Path_List, Path),
		clean_dirs(TestDir, [dir1,dir2,dir3], Status),
		Status == ok,
		change_cwd(TestDir)),
	    true ]).

clean_dirs(TestDir, DirsList, Status) :-
	do_clean_dirs([TestDir | DirsList], [], Status).

do_clean_dirs([], Stack, Status) :-
	climb_and_clean(Stack, Status).
	
do_clean_dirs([Dir | ListOfDirsList], Stack, Status) :-
	(exists_file(Dir) ->
		change_cwd(Dir), 
		do_clean_dirs(DirsList, [Dir | Stack], Status)
		;
		Status = fail
	).

climb_and_clean([], ok).
climb_and_clean([Top], ok) :- !,
	change_cwd('..').
climb_and_clean([Dir | Stack], Status) :-
	change_cwd('..'),
	remove_subdir(Dir),
	climb_and_clean(Stack, Status).

list_clean_dirs([], TestDir, Status).
list_clean_dirs([Path_List | List_of_Path_Lists], TestDir, Status) :-
	do_clean_dirs([TestDir | List_of_Path_Lists], [], Status).
	
	
/*
	rr/
	  qq/                  pp/
            kk/ mm/    nn/       aa/
                  jj/              bb/

	[[rr,qq,kk],[rr,qq,mm,jj],[rr,qq,nn],[rr,pp,aa,bb]]
 */

test_recursive_dir_paths :-
	List_of_Path_Lists = [[rr,qq,kk],[rr,qq,mm,jj],[rr,qq,nn],[rr,pp,aa,bb]],
	get_cwd(TestDir),
	clean_dirs(TestDir, List_of_Path_Lists, _),

	test([
	    (recursive_dir_paths(List_of_Path_Lists, Paths),
	    clean_dirs(TestDir, List_of_Path_Lists, Status),
	    Status == ok,
	    change_cwd(TestDir)),
	    true ]).

