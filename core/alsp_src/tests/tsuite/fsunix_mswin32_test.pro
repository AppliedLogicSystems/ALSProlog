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
	    test_move_file(OS),
	    
%	    test_make_subdir1(OS),
%	    test_make_subdir2(OS),
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

