:-[test].

test_fsunix_mswin32
        :-
        sys_env(OS,_,_),
        do_test_fs(OS).

do_test_fs(OS)
        :-
        test([
	    test_file_status(OS),
	    
%	    test_make_subdir1,
%	    test_make_subdir2,
%	    test_recursive_dir_path,
%	    test_recursive_dir_paths,
        true]).

do_test_fs(OS)
        :-
        test([
        true]).

test_file_status(mswin32) 
	:-
	test([
	    (file_status('alspro.exe', Status),
  %Status == [type = regular,permissions = [read,write,execute], mod_time = M, size = 3]),
		member(type = regular, Status),
		member(permissions = [read,write,execute], Status),
		member(mod_time = _, Status),
		member(size = _, Status)),
	    true]).


test_file_status(unix) 
	:-
	test([
	    (file_status(alspro, Status),
  %Status == [type = regular,permissions = [read,write,execute], mod_time = M, size = 3]),
		member(type = regular, Status),
		member(permissions = [read,write,execute], Status),
		member(mod_time = _, Status),
		member(size = _, Status)),
	    true]).


