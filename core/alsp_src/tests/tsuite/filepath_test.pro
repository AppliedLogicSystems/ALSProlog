:-[test].

test_filepath
	:-
	sys_env(OS,_,_),
	do_test_filepath(OS).

do_test_filepath(OS)
	:-
	test([
	test_file_extension,
	test_path_directory_tail(OS),
	test_is_absolute_path(OS),
%	test_tilda_expand,
%	test_make_change_cwd,
	test_pathPlusFile(OS),
	test_pathPlusFilesList(OS),
	test_path_elements(OS),
	test_path_type(OS),
	test_split_path(OS),
	test_join_path(OS),
	test_directory_self,
	test_parent_path,
	true]).

test_file_extension :-
	test([
	(file_extension(foo, Name, Ext), Name == foo, Ext == ''),
	(file_extension('foo.pro', Name, Ext), Name == foo, Ext == pro),
	(file_extension(FullName, bar, pro), FullName == 'bar.pro'),
	true ]).
	
test_path_directory_tail(mswin32) :-!,
	test([
	(path_directory_tail(Path, 'mom\\kids', 'bar\\zip.pro'), 
		Path == 'mom\\kids\\bar\\zip.pro'),
	(path_directory_tail('mom\\kids\\bar\\zip.pro', 'mom\\kids\\bar', Tail),
		Tail == 'zip.pro'),
	(path_directory_tail('mom\\kids\\bar\\zip.pro', Directory, 'zip.pro'),
		Directory == 'mom\\kids\\bar'),
	(path_directory_tail('mom\\kids\\bar\\zip.pro', Directory, Tail),
		Directory == 'mom\\kids\\bar',
		Tail == 'zip.pro'),
	(path_directory_tail('zip.pro', Directory, Tail),
		Directory == '.' ,
		Tail == 'zip.pro'),
	true ]).

test_path_directory_tail(unix) :-
	test([
	(path_directory_tail(Path, 'mom/kids', 'bar/zip.pro'),
		Path == 'mom/kids/bar/zip.pro'),
	(path_directory_tail('mom/kids/bar/zip.pro', 'mom/kids/bar', Tail1),
		Tail1 == 'zip.pro'),
	(path_directory_tail('mom/kids/bar/zip.pro', Directory2, 'zip.pro'),
		Directory2 == 'mom/kids/bar'),
	(path_directory_tail('mom/kids/bar/zip.pro', Directory3, Tail3),
		Directory3 == 'mom/kids/bar',
		Tail3 == 'zip.pro'),
	(path_directory_tail('zip.pro', Directory4, Tail4),
		Directory4 == '.' ,
		Tail4 == 'zip.pro'),
	true ]).

test_is_absolute_path(mswin32) :-!,
	test([
	is_absolute_path('\\foo\\bar'),
	not(is_absolute_path('foo\\bar')),
	true ]).

test_is_absolute_path(unix) :-
	test([
	is_absolute_path('/foo/bar'),
	not(is_absolute_path('foo/bar')),
	true ]).

test_tilda_expand :-
	test([
	(tilda_expand('~/foo/bar.pro', Path),
		split_path(Path, Elements),
		Elements = [/,'Users',_,foo,'bar.pro']),  % Must use unify (=) here
	true ]).
	
test_pathPlusFile(mswin32) :-!,
	test([
	(pathPlusFile('foo\\bar', 'zip.pro', PathAndFile),
		PathAndFile == 'foo\\bar\\zip.pro'),
	(pathPlusFile(Path,File,'foo\\bar\\zip.pro'),
		Path == 'foo\\bar',
		File == 'zip.pro'),
	true ]).

test_pathPlusFile(unix) :-
	test([
	(pathPlusFile('foo/bar', 'zip.pro', PathAndFile),
		PathAndFile == 'foo/bar/zip.pro'),
	(pathPlusFile(Path,File,'foo/bar/zip.pro'),
		Path == 'foo/bar',
		File == 'zip.pro'),
	true ]).

test_pathPlusFilesList(mswin32) :-!,
	test([
	(pathPlusFilesList(['foo.pro','bar.pro','zip.pro'], 'mom\\kids', EFL),
		EFL == ['mom\\kids\\foo.pro','mom\\kids\\bar.pro','mom\\kids\\zip.pro']),
	true ]).

test_pathPlusFilesList(unix) :-
	test([
	(pathPlusFilesList(['foo.pro','bar.pro','zip.pro'], 'mom/kids', EFL),
		EFL == ['mom/kids/foo.pro','mom/kids/bar.pro','mom/kids/zip.pro']),
	true ]).

test_path_elements(mswin32) :-!,
	test([
	(path_elements('mom\\kids\\foo.pro', Elements),
		Elements == [mom,kids,'foo.pro']),
	(path_elements(Path, [mom,kids,'foo.pro']),
		Path == 'mom\\kids\\foo.pro'),
	(path_elements(Path, ['mom\\dad',kids,'foo.pro']),
		Path == 'mom\\dad\\kids\\foo.pro'),
	true ]).

test_path_elements(unix) :-
	test([
	(path_elements('mom/kids/foo.pro', Elements),
		Elements == [mom,kids,'foo.pro']),
	(path_elements(Path, [mom,kids,'foo.pro']),
		Path == 'mom/kids/foo.pro'),
	(path_elements(Path, ['mom/dad',kids,'foo.pro']),
		Path == 'mom/dad/kids/foo.pro'),
	true ]).

test_path_type(mswin32) :-!,
	test([
	(path_type('mom\\kids\\foo.pro', Type),
		Type == relative),
	(path_type('\\mom\\kids\\foo.pro', Type),
		Type == volume_relative),
	(path_type('C:\\mom\\kids\\foo.pro', Type),
		Type == absolute),
	true ]).

test_path_type(unix) :-
	test([
	(path_type('mom/kids/foo.pro', Type),
		Type == relative),
	(path_type('/mom/kids/foo.pro', Type),
		Type == absolute),
	true ]).

test_split_path(mswin32) :-!,
	test([
	(split_path('mom\\kids\\foo.pro', List),
		List == [mom,kids,'foo.pro']),
	true ]).

test_split_path(unix) :-
	test([
	(split_path('mom/kids/foo.pro', List),
		List == [mom,kids,'foo.pro']),
	true ]).

test_join_path(mswin32) :-!,
	test([
	(join_path(['mom\\dad',kids,'foo.pro'], Path),
		Path == 'mom\\dad\\kids\\foo.pro'),
	true ]).

test_join_path(unix) :-
	test([
	(join_path(['mom/dad',kids,'foo.pro'], Path),
		Path == 'mom/dad/kids/foo.pro'),
	true ]).

test_directory_self :-
	test([
	(directory_self(Self),
		Self == '.'),
	(directory_self('.')),
	true ]).

test_parent_path :-
	test([
	(parent_path(PP),
		PP == '..'),
	true ]).
