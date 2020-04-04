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
	clean_dirs123(TestDir),
builtins:clause(recursive_dir_path(Path_List, Path), BODY),
printf(user_output, ">trdp:rdp_clause_body=%t\n", [BODY]),

	test([
printf(user_output, "Enter test_recursive_dir_path-test\n", []),
	    (Path_List = [dir1,dir2,dir3], 
		recursive_dir_path(Path_List, Path),
	        path_directory_tail(FullPath, TestDir, Path),
		change_cwd(Path),
		get_cwd(ThisPath),
		FullPath == ThisPath,
		clean_dirs123(TestDir)),
	    true ]).

clean_dirs123(TestDir) :-
	change_cwd(TestDir),
	(exists_file(dir1) -> 
		change_cwd(dir1),
		(exists_file(dir2) -> 
			change_cwd(dir2),
			(exists_file(dir3) -> 
				remove_subdir(dir3),
				change_cwd('..'),
				remove_subdir(dir2),
				change_cwd('..'),
				remove_subdir(dir1)
				;
				true
			)
			;
			change_cwd('..'),
			remove_subdir(dir2),
			change_cwd('..'),
			remove_subdir(dir1)
		)
		; 
		true
	).
	
clean_dirsfoobar(TestDir) :-
	change_cwd(TestDir),
	(exists_file(foo) -> 
		change_cwd(foo),
		(exists_file(bar) -> 
			remove_subdir(bar),
			change_cwd('..'),
			remove_subdir(foo)
			;
			true
		)
		;
		true
	).

test_recursive_dir_paths :-
	List_of_Path_Lists = [[dir1,dir2,dir3], [foo,bar]],
	get_cwd(TestDir),
	test([
	(recursive_dir_paths(List_of_Path_Lists, Paths),
		Paths = [PathA, PathB],
	        path_directory_tail(FullPathA, TestDir, PathA),
	        path_directory_tail(FullPathB, TestDir, PathB),
		change_cwd(PathA),
		get_cwd(ThisPathA),
		FullPathA == ThisPathA,
		change_cwd(TestDir),
		change_cwd(PathB),
		get_cwd(ThisPathB),
		FullPathB == ThisPathB),
		(clean_dirs123(TestDir),clean_dirsfoobar(TestDir)),
	true]).





