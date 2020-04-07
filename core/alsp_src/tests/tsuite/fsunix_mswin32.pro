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
	
do_clean_dirs([Dir | DirsList], Stack, Status) :-
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


check_multi_dirs([], [], TestDir, ok).
check_multi_dirs([Path_List], [Top], TestDir, Status) :-
	Path_List = [Top | _],
	check_list(Path_List, TestDir, [], Status),
	Status = ok.
check_multi_dirs([Path_List | List_of_Path_Lists], [Top | Tops], TestDir, Status) :-
	Path_List = [Top | _],
	check_list(Path_List, TestDir, [], Status),
	check_multi_dirs(List_of_Path_Lists, Tops, TestDir, Status).

check_list([], TestDir, Stack, Status) :-
	climb_dirs(Stack, TestDir, Status).

check_list([Dir | DirsList], TestDir, Stack, Status) :-
	(exists_file(Dir) ->
		change_cwd(Dir), 
		check_list(DirsList, TestDir, [Dir | Stack], Status)
		;
		Status = fail,
	    	change_cwd(TestDir)
	).


climb_dirs([], TestDir, Status).
climb_dirs([Dir | Stack], TestDir, Status) :-
	change_cwd('..'),
	climb_dirs(Stack, TestDir, Status).
	
	
/* 	Multiple paths forming a tree:

	rr/
	  qq/                  pp/
            kk/ mm/    nn/       aa/
                  jj/              bb/

	[[rr,qq,kk],[rr,qq,mm,jj],[rr,qq,nn],[rr,pp,aa,bb]]
 */

test_recursive_dir_paths :-
	List_of_Path_Lists = [[rr,qq,kk],[rr,qq,mm,jj],[rr,qq,nn],[rr,pp,aa,bb]],
	get_cwd(TestDir),

	test([
	    (recursive_dir_paths(List_of_Path_Lists, Paths),
	    check_multi_dirs(List_of_Path_Lists, Tops, TestDir, Status),
	    Status == ok,
	    change_cwd(TestDir),
	    remove_list_dirs(Tops)),
	    true ]).

check_multi_dirs([], [], TestDir, ok).
check_multi_dirs([Path_List], [Top], TestDir, Status) :-
	Path_List = [Top | _],
	check_list(Path_List, TestDir, [], Status),
	Status = ok.
check_multi_dirs([Path_List | List_of_Path_Lists], [Top | Tops], TestDir, Status) :-
	Path_List = [Top | _],
	check_list(Path_List, TestDir, [], Status),
	check_multi_dirs(List_of_Path_Lists, Tops, TestDir, Status).

check_list([], TestDir, Stack, Status) :-
	climb_dirs(Stack, TestDir, Status).

check_list([Dir | DirsList], TestDir, Stack, Status) :-
	(exists_file(Dir) ->
		change_cwd(Dir), 
		check_list(DirsList, TestDir, [Dir | Stack], Status)
		;
		Status = fail,
	    	change_cwd(TestDir)
	).

climb_dirs([], TestDir, Status).
climb_dirs([Dir | Stack], TestDir, Status) :-
	change_cwd('..'),
	climb_dirs(Stack, TestDir, Status).
	
remove_list_dirs([]).
remove_list_dirs([Top | Tops]) :-
	(exists_file(Top) ->
		kill_subdir(Top) ; true),
	remove_list_dirs(Tops).
