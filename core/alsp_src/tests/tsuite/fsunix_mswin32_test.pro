:-[test].

test_fsunix_mswin32
        :-
        sys_env(OS,_,_),
        do_test_fs(OS).

do_test_fs(OS)
        :-
        test([
	    test_file_status(OS),
	    test_files2(OS),
	    test_files3(OS),
	    test_move_file(_),
	    
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
	    all_f(Files, mswin32) ),
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
	(OS==mswin32 -> (F == 'alsdev.exe.pst' ; F == 'alspro.exe.pst')
				;
			(F == 'alsdev.pst' ; F == 'alspro.pst') ),
	all_f2(Files, OS).

test_files3(_)
	:-
	test([
	    (files('examples/more', 'p*', Files),
 	     Files == ['primes_coroutine.pro']),
	    true]).

test_move_file(_)
	:-
	test([
	    (move_file('README.txt', 'intro-README.txt'),
	     file_status('intro-README.txt', Status),
		member(type = regular, Status)),
	    true]).

