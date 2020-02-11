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
	file_extension(foo, Name1, Ext1),
	Name1 == foo,
	Ext1 == '',
	file_extension('foo.pro', Name2, Ext2),
	Name2 == foo,
	Ext2 == pro,
	file_extension(FullName, bar, pro),
	FullName == 'bar.pro'.
	
test_path_directory_tail(mswin32) :-!,
	path_directory_tail(Path, 'mom\\kids', 'bar\\zip.pro'),
	Path == 'mom\\kids\\bar\\zip.pro',
	path_directory_tail('mom\\kids\\bar\\zip.pro', 'mom\\kids\\bar', Tail1),
	Tail1 == 'zip.pro',
	path_directory_tail('mom\\kids\\bar\\zip.pro', Directory2, 'zip.pro'),
	Directory2 == 'mom\\kids\\bar',
	path_directory_tail('mom\\kids\\bar\\zip.pro', Directory3, Tail3),
	Directory3 == 'mom\\kids\\bar',
	Tail3 == 'zip.pro',
	path_directory_tail('zip.pro', Directory4, Tail4),
	Directory4 == '.' ,
	Tail4 == 'zip.pro'.

test_path_directory_tail(_) :-
	path_directory_tail(Path, 'mom/kids', 'bar/zip.pro'),
	Path == 'mom/kids/bar/zip.pro',
	path_directory_tail('mom/kids/bar/zip.pro', 'mom/kids/bar', Tail1),
	Tail1 == 'zip.pro',
	path_directory_tail('mom/kids/bar/zip.pro', Directory2, 'zip.pro'),
	Directory2 == 'mom/kids/bar',
	path_directory_tail('mom/kids/bar/zip.pro', Directory3, Tail3),
	Directory3 == 'mom/kids/bar',
	Tail3 == 'zip.pro',
	path_directory_tail('zip.pro', Directory4, Tail4),
	Directory4 == '.' ,
	Tail4 == 'zip.pro'.

test_is_absolute_path(mswin32) :-!,
	is_absolute_path('\\foo\\bar'),
	not(is_absolute_path('foo\\bar')).

test_is_absolute_path(_) :-
	is_absolute_path('/foo/bar'),
	not(is_absolute_path('foo/bar')).

test_tilda_expand :-
	tilda_expand('~/foo/bar.pro', Path),
	split_path(Path, Elements),
	Elements = [/,'Users',_,foo,'bar.pro'].  % Must use unify (=) here
	

/*
test_make_change_cwd :-

*/

test_pathPlusFile(mswin32) :-!,
	pathPlusFile('foo\\bar', 'zip.pro', PathAndFile),
	PathAndFile='foo\\bar\\zip.pro',
	pathPlusFile(Path,File,'foo\\bar\\zip.pro'),
	Path='foo\\bar',
	File='zip.pro'.

test_pathPlusFile(_) :-
	pathPlusFile('foo/bar', 'zip.pro', PathAndFile),
	PathAndFile='foo/bar/zip.pro',
	pathPlusFile(Path,File,'foo/bar/zip.pro'),
	Path='foo/bar',
	File='zip.pro'.

test_pathPlusFilesList(mswin32) :-!,
	pathPlusFilesList(['foo.pro','bar.pro','zip.pro'], 'mom\\kids', EFL),
	EFL == ['mom\\kids\\foo.pro','mom\\kids\\bar.pro','mom\\kids\\zip.pro'].

test_pathPlusFilesList(OS) :-
	pathPlusFilesList(['foo.pro','bar.pro','zip.pro'], 'mom/kids', EFL),
	EFL == ['mom/kids/foo.pro','mom/kids/bar.pro','mom/kids/zip.pro'].

test_path_elements(mswin32) :-!,
	path_elements('mom\\kids\\foo.pro', Elements),
	Elements == [mom,kids,'foo.pro'],
	path_elements(Path0, [mom,kids,'foo.pro']),
	Path0 == 'mom\\kids\\foo.pro',
	path_elements(Path1, ['mom\\dad',kids,'foo.pro']),
	Path1 == 'mom\\dad\\kids\\foo.pro'.

test_path_elements(_) :-
	path_elements('mom/kids/foo.pro', Elements),
	Elements == [mom,kids,'foo.pro'],
	path_elements(Path0, [mom,kids,'foo.pro']),
	Path0 == 'mom/kids/foo.pro',
	path_elements(Path1, ['mom/dad',kids,'foo.pro']),
	Path1 == 'mom/dad/kids/foo.pro'.

test_path_type(mswin32) :-!,
	path_type('mom\\kids\\foo.pro', Type0),
	Type0== relative,
	path_type('\\mom\\kids\\foo.pro', Type1),
	Type1 == volume_relative,
	path_type('C:\\mom\\kids\\foo.pro', Type2),
	Type2 == absolute.

test_path_type(_) :-
	path_type('mom/kids/foo.pro', Type0),
	Type0== relative,
	path_type('/mom/kids/foo.pro', Type1),
	Type1 == absolute.

test_split_path(mswin32) :-!,
	split_path('mom\\kids\\foo.pro', List),
	List == [mom,kids,'foo.pro'].

test_split_path(_) :-
	split_path('mom/kids/foo.pro', List),
	List == [mom,kids,'foo.pro'].

test_join_path(mswin32) :-!,
	join_path(['mom\\dad',kids,'foo.pro'], Path),
	Path == 'mom\\dad\\kids\\foo.pro'.

test_join_path(_) :-
	join_path(['mom/dad',kids,'foo.pro'], Path),
	Path == 'mom/dad/kids/foo.pro'.

test_directory_self :-
	directory_self(Self),
	Self='.',
	directory_self('.').

test_parent_path :-
	parent_path(PP),
	PP == '..'.
