:-[test].

test_fsunix_mswin32
        :-
        sys_env(OS,OSVar,_),
        do_test_fs(OS,OSVar).

do_test_fs(OS,OSVar)
        :-
        test([
	    test_file_status(OS),
	    test_files2(OS),
	    test_files3(OS),
	    test_move_file(OS),
	    
	    test_make_subdir1(OS),
	    test_make_subdir2(OS),
	    test_subdirs(OS,OSVar),
	    test_subdirs_red(OS,OSVar),

	    test_remove_subdir(OS),
	    test_kill_subdir(OS),

	    test_directory(OS, OSVar),
%	    test_recursive_dir_path(OS),
%	    test_recursive_dir_paths(OS),
        true]).

test_file_status(mswin32) 
	:-
	test([
	    (file_status('alspro.exe', Status),
		member(type = regular, Status),
		member(permissions = [read,write,execute], Status),
		member(mod_time = _, Status),
		member(size = _, Status),
		all_eq(Status) ),
	    true]).



test_file_status(unix) 
	:-
	test([
	    (file_status(alspro, Status),
		member(type = regular, Status),
		member(permissions = [read,write,execute], Status),
		member(mod_time = _, Status),
		member(size = _, Status),
		all_eq(Status) ),
	    true]).

all_eq([]). 
all_eq([T | Status])
	:-
	T = (G = B),
	(G==type; G==permissions; G==mod_time; G==size),
	all_eq(Status).

test_files2(mswin32)
	:-
	test([
	    (files('*.pst', Files),
		member('alsdev.exe.pst', Files),
		member('alspro.exe.pst', Files),
	    all_f2(Files, mswin32) ),
	    true]).

test_files2(unix)
	:-
	test([
	    (files('*.pst', Files),
		member('alsdev.pst', Files),
		member('alspro.pst', Files),
	    all_f2(Files, unix) ),
	    true]).

all_f2([], _).
all_f2([F | Files], OS)
	:-
	(OS==mswin32 -> (F == 'alsdev.exe.pst' ; F == 'alspro.exe.pst'; 
			 F == 'app_image0.exe.pst'; F == 'app_image1.exe.pst'; 
			 F == 'app_image2.exe.pst'; F == 'libalspro.dll.pst')
				;
			(F == 'alsdev.pst' ; F == 'alspro.pst';
			 F == 'app_image0.pst'; F == 'app_image1.pst'; F == 'app_image2.pst';
            		 (F == 'libalspro.dylib.pst' ; F == 'libalspro.so.pst') )
	),
	all_f2(Files, OS).

test_files3(mswin32)
        :-
        test([
            (files('alsdir\\builtins', 'c*', Files),
             Files == ['comp_d10.pro', 'cutils.pro']),
            true]).

test_files3(unix)
	:-
	test([
	    (files('examples/more', 'p*', Files),
 	     Files == ['primes_coroutine.pro']),
	    true]).

test_move_file(mswin32)
	:-
	system('del /f  barFile-beer.txt'),
	system('echo foo > barFile.txt'),
	test([
	    (move_file('barFile.txt', 'barFile-beer.txt'),
	     file_status('barFile-beer.txt', Status),
		member(type = regular, Status)),
	    system('del /f  barFile-beer.txt'),
	    true]).
test_move_file(unix)
	:-
	system('rm -rf barFile-beer.txt'),
	system('echo foo > barFile.txt'),
	test([
	    (move_file('barFile.txt', 'barFile-beer.txt'),
	     file_status('barFile-beer.txt', Status),
		member(type = regular, Status)),
	    system('rm -rf barFile-beer.txt'),
	    true]).

test_make_subdir1(mswin32)
	:-
	system('RMDIR myNewTestSubdir'),
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
	system('RMDIR myNewTestSubdir').

test_make_subdir1(unix)
	:-
        system('rm -rf myNewTestSubdir'),
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
        system('rm -rf myNewTestSubdir').



test_make_subdir2(mswin32)
	:-
	system('RMDIR myNewTestSubdir'),
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
	system('RMDIR myNewTestSubdir').

test_make_subdir2(unix)
	:-
        system('rm -rf myNewTestSubdir'),
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
        system('rm -rf myNewTestSubdir').

test_subdirs(mswin32,_)
	:-
	test([
	    (subdirs(SDList),
	    SDList == ['.','..',alsdir,mswinnt]),
        true]).

test_subdirs(unix,OSVar)
	:-
  	test([
	    (subdirs(SDList),
	    (OSVar == linux ->
	        SDList == [alsdir,examples,linux]
		;
	        SDList == [alsdir,darwin,examples]) ),
        true]).

test_subdirs_red(mswin32,_)
	:-
	test([
	    (subdirs_red(SDList),
	    SDList == [alsdir,mswinnt]),
        true]).

test_subdirs_red(unix,OSVar)
	:-
  	test([
	    (subdirs_red(SDList),
	    (OSVar == linux ->
	        SDList == [alsdir,examples,linux]
		;
	        SDList == [alsdir,darwin,examples]) ),
        true]).

test_remove_subdir(mswin32)
	:-
        system('rmdir myNewTestSubdir'),
        test([
            (make_subdir(myNewTestSubdir),
	     subdirs(List1),
	     member(myNewTestSubdir, List1),
	     remove_subdir(myNewTestSubdir),
	     subdirs(List2),
	     not(member(myNewTestSubdir, List2)) ),
        true]),
        system('rmdir myNewTestSubdir').

test_remove_subdir(unix)
	:-
        system('rm -rf myNewTestSubdir'),
        test([
            (make_subdir(myNewTestSubdir),
	     subdirs(List1),
	     member(myNewTestSubdir, List1),
	     remove_subdir(myNewTestSubdir),
	     subdirs(List2),
	     not(member(myNewTestSubdir, List2)) ),
        true]),
        system('rm -rf myNewTestSubdir').

test_kill_subdir(mswin32)
	:-
        system('rmdir myNewTestSubdir'),
        test([
            (make_subdir(myNewTestSubdir),
	     subdirs(List1),
	     member(myNewTestSubdir, List1),
	     kill_subdir(myNewTestSubdir),
	     subdirs(List2),
	     not(member(myNewTestSubdir, List2)) ),
        true]),
        system('rmdir myNewTestSubdir').

test_kill_subdir(unix)
	:-
        system('rm -rf myNewTestSubdir'),
        test([
            (make_subdir(myNewTestSubdir),
	     subdirs(List1),
	     member(myNewTestSubdir, List1),
	     kill_subdir(myNewTestSubdir),
	     subdirs(List2),
	     not(member(myNewTestSubdir, List2)) ),
        true]),
        system('rm -rf myNewTestSubdir').

test_directory(mswin32,_)
	:-
	test([
	    (directory('*', 1, FL0),
printf(user_output, 'd1 = %t\n', [FL0]),
	    FL0 == ['.','..',alsdir,mswinnt],
	    directory('*.pst', 4, FL1),
printf(user_output, 'd4 = %t\n', [FL1]),
	    FL1 == ['alsdev.exe.pst','alspro.exe.pst','app_image0.exe.pst','app_image1.exe.pst',
                        'app_image2.exe.pst','libalspro.dll.pst']),
        true]).

test_directory(unix,OSVar)
	:-
  	test([
	    (directory('*', 1, FL0),
	    (OSVar == linux ->
	        FL0 == [alsdir,examples,linux]
		;
	        FL0 == [alsdir,darwin,examples]),
	    directory('*.pst', 4, FL1),
	    (OSVar == linux ->
	    	FL1 == ['alsdev.pst','alspro.pst','app_image0.pst','app_image1.pst',
			'app_image2.pst','libalspro.so.pst']
		;
	    	FL1 == ['alsdev.pst','alspro.pst','app_image0.pst','app_image1.pst',
			'app_image2.pst','libalspro.dylib.pst'])),
        true]).

