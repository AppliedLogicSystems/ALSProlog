:-[test].

test_fsunix_mswin32
        :-
        sys_env(OS,_,_),
        do_test_fs(OS).

do_test_fs(OS)
        :-
        test([
	    test_file_status,
	    
%	    test_make_subdir1,
%	    test_make_subdir2,
%	    test_recursive_dir_path,
%	    test_recursive_dir_paths,
        true]).

do_test_fs(OS)
        :-
        test([
        true]).


test_file_status 
	:-
	system('rm -rf regFile1.txt'),
	system('echo "hi" > regFile1.txt'),
	test([
	    (file_status('regFile1.txt', Status),
	     nonvar(Status),
	     member(mod_time = M, Status),
	     Status == [type = regular,permissions = [read,write], mod_time = M, size = 3]),
	    true]),
	system('rm -rf regFile1.txt'). 



