/*=================================================================
 |		filepath.pro
 |	Copyright (c) 1991-96 Applied Logic Systems, Inc.
 |		Group: File System
 |		DocTitle: file_extension/3
 |
 |
 |	Abstract handling of building and decomposing file names with paths.
 |
 |  Author: Chuck Houpt, Ken Bowen
 |  Date:	May, 1991
 |  Revised with "root" terminology: October,1991
 |
 |	Terminology
 |	-----------
 |		File -- the name part of a file designator, as:
 |					zip in 'zip.pro' ;
 |		Extension -- the extension part of a file designator, as:
 |					pro in 'zip.pro' ;
 |		Full file name -- name part plus extension (if any), as:
 |					'zip.pro',    zap (no extension)
 |		(Sub)path -- sequence of names of (sub)directories, each
 |					a subdirectory of the preceeding, as:
 |					foo/bar/silly
 |					foo\bar\silly
 |		Root -- the file root symbol, combined with a "disk" indicator,
 |					if any, as:
 |					'c:\'
 |					'\'
 |		Parent (of a file,directory) -- the directory containing
 |					a given file or (sub)directory, as:
 |					mom/kid.txt   mom is parent of kid.txt
 |
 |  Revised to use sub_atom, atom_concat, etc: November 1993 by K. Buettner
 *================================================================*/
module builtins.

export file_extension/3.
export path_directory_tail/3.
export is_absolute_path/1.
export tilda_expand/2.
export make_change_cwd/1.
export pathPlusFile/3.
export pathPlusFilesList/3.
export path_elements/2.
export path_type/2.
export split_path/2.
export join_path/2.
export directory_self/1.
export parent_path/1.

/*!---------------------------------------------------------------------
 |	file_extension/3
 |	file_extension(FullName, Name, Ext)
 |	file_extension(+/-, +/-, +/-)
 |
 |	- Access or add the extension for a filename.
 |
 |	If FullName is instantiated, decomposes it to yield the 
 |	underlying Filename (unified with Name)  and extension (unified
 |	with Ext.  If FullName is instantiated but has no extension, then
 |	Name is unified with FullName and Ext is unified with ''.
 |	If FullName is uninstantiated and both Name, Ext are instantiated, 
 |	composes the filename plus extension out of Name, Ext and unifies 
 |	that with FullName.
 |
 | Examples
 |	?- file_extension(foo, Name1, Ext1).
 |	Name1=foo 
 |	Ext1='' 
 |	?- file_extension('foo.pro', Name2, Ext2).
 |	Name2=foo 
 |	Ext2=pro 
 |	?- file_extension(FullName, bar, pro).
 |	FullName='bar.pro' 
 *!--------------------------------------------------------------------*/
file_extension(FullName, Name, Ext) :-
	nonvar(FullName),
	!,
	split_path(FullName, Elements),
	dreverse(Elements, [Last | RRestElts]),
	once((
		rev_sub_atom(Last, Before, 1, After, '.')
		;
		(atom_length(Last, Before), After = 0)
	)),
	(Before == 0 ->
		Name = Last, Ext = ''
		;
		sub_atom(Last, _, After, 0, Ext),
		sub_atom(Last, 0, Before, _, BName),
		dreverse([BName | RRestElts], NameElts),
		join_path(NameElts, Name)
	).

file_extension(FullName, FileName, Ext) :-
	atom_concat(FileName,'.',FileNameDot),
	atom_concat(FileNameDot, Ext, FullName).

/*!---------------------------------------------------------------------
 |	path_directory_tail/3
 |	path_directory_tail(Path, Directory, Tail)
 |	path_directory_tail(+/-, +/-, +/-)
 |
 |	- Compose or decompose a path to a file in a directory.
 |
 |	If Path is uninstantiated, then joins Directory with Tail, and
 |	unifies the result with Path.
 |	If Path is instantiated, removes the last element from Path,
 |	leaving Not-Last, unifies the last element with Tail, and: 
 |	if Not-Last is empty, unifies Directory with '.', else
 |	unifies Directory with Not-Last.
 |
 | Examples
 |	?- path_directory_tail(Path, 'mom/kids', 'bar/zip.pro').
 |	Path == 'mom/kids/bar/zip.pro'
 |	?- path_directory_tail('mom/kids/bar/zip.pro', 'mom/kids/bar', Tail).
 |	Tail == 'zip.pro'
 |	?- path_directory_tail('mom/kids/bar/zip.pro', Directory, 'zip.pro').
 |	Directory == 'mom/kids/bar'
 |	?- path_directory_tail('mom/kids/bar/zip.pro', Directory, Tail).
 |	Directory == 'mom/kids/bar'
 |	Tail='zip.pro'
 |	?- path_directory_tail('zip.pro', Directory, Tail).
 |	Directory='.'
 |	Tail='zip.pro'
 *!--------------------------------------------------------------------*/
path_directory_tail(Path, Directory, Tail) :-
	var(Path),
	!,
	join_path([Directory, Tail], Path).
path_directory_tail(Path, Directory, Tail) :-
	split_path(Path, Elements),
	dreverse(Elements, [Tail | RevDirElements]),
	dreverse(RevDirElements, DirElements),
	(DirElements = [] -> directory_self(Directory) ; join_path(DirElements, Directory)).

/*!---------------------------------------------------------------------
 |	is_absolute_path/1
 |	is_absolute_path(Path)
 |	is_absolute_path(Path)
 |
 |	- Determines whether Path begins at the file system root.
 |
 |	Succeeds if Path begins at the file system root, fails otherwise.
 |
 | Examples
 |	?- is_absolute_path('/foo/bar').
 |	yes.
 |      ?- not(is_absolute_path('foo/bar')).
 |	yes.
 *!--------------------------------------------------------------------*/
is_absolute_path(Path) :-
	path_type(Path, PathType),
	PathType \= relative.

/*!---------------------------------------------------------------------
 |	tilda_expand/2
 |	tilda_expand(TildaPath, Path)
 |	tilda_expand(+, -)
 |
 | 	- Expands a tilda-path to an absolute file system path.
 |
 |	Replaces the leading tilde (~) appropriately.
 |
 | Examples
 |	?- tilda_expand('~/foo/bar.pro', Path).
 |	Path='/Users/mike/foo/bar.pro'
 *!--------------------------------------------------------------------*/
tilda_expand(TildaPath, Path) :-
	sub_atom(TildaPath, 0, 1, _, '~'),
	split_path(TildaPath, [Head | Rest]),
	sub_atom(Head, 0, 1, After, '~'), 
	sub_atom(Head, 1, After, 0, Name),
	(After = 0 -> getenv('HOME', Home) ; get_user_home(Name, Home)),
	!,
	(Home = '' -> Path = TildaPath ; join_path([Home | Rest], Path)).
tilda_expand(Path, Path).

/*!---------------------------------------------------------------------
 |	make_change_cwd/1
 |	make_change_cwd(P)
 |	make_change_cwd(+)
 |
 |	- Creates path P to a directory, as needed, and changes to it.
 |
 |	If P is instantiated to a legal path to a directory, then: 
 |	If the directory exists, executes change_cwd(P).  
 |	Otherwise, creates the directory, including all intermediate
 |	directories, and then executes change_cwd(P).
 *!--------------------------------------------------------------------*/
make_change_cwd(P)
	:-
	exists_file(P),
	!,
	change_cwd(P).

make_change_cwd(P)
	:-
	path_elements(P, PElts),
	make_path_segments(PElts),
	exists_file(P),
	change_cwd(P).

make_path_segments(PElts)
	:-
	make_path_segments(PElts, []).

make_path_segments([], _).
make_path_segments([E | PElts], IS)
	:-
	dreverse([E | IS], SI),
	path_elements(SIP, SI),
	(exists_file(SIP) -> true ; make_subdir(SIP)),
	make_path_segments(PElts, [E | IS]).

/*!---------------------------------------------------------------------
 |	pathPlusFile/3
 |	pathPlusFile(Path, File, PathAndFile)
 |	pathPlusFile(+/-, +/-, +/-)
 |
 |	- Compose/decompose a path with a terminating file.
 |
 |	If PathAndFile is uninstantiated, adds File to the end of 
 |	Path and unifies that with PathAndFile.  If PathAndFile is
 |	instantiated, removes the final element from PathAndFile,
 |	unifies the remainder with Path, and unifies that last
 |	element with File.
 |
 | Examples
 |	?- pathPlusFile('foo/bar', 'zip.pro', PF).
 |	PF='foo/bar/zip.pro' 
 |	?- pathPlusFile(P,F,'foo/bar/zip.pro').
 |	P='foo/bar' 
 |	F='zip.pro'
 *!--------------------------------------------------------------------*/
pathPlusFile(Path, File, PathAndFile)
	:-
	var(PathAndFile),
	!,
	split_path(Path, PathElts),
	dappend(PathElts, [File], PFElts),
	join_path(PFElts, PathAndFile).

pathPlusFile(Path, File, PathAndFile)
	:-
	nonvar(PathAndFile),
	split_path(PathAndFile, PFElts),
	dreverse(PFElts, [File | RevPathElts]),
	dreverse(RevPathElts, PathElts),
	join_path(PathElts, Path).

/*!---------------------------------------------------------------------
 |	pathPlusFilesList/3.
 |	pathPlusFilesList(SourceFilesList, Path, ExtendedFilesList)
 |	pathPlusFilesList(+, +, -)
 |
 |	- attaches a path to each of a list of file names
 |
 |	If SourceFilesList is list of items denoting file names, and
 |	if Path denotes a path, creates a list of atoms which consists 
 |	of the Path prepended to each of the file names.  Functionally
 |	identical to prefix_dir/3 in library file miscatom.pro.
 |
 | Examples
 |	?- pathPlusFilesList(['foo.pro','bar.pro','zip.pro'], 'mom/kids', EFL).
 |	EFL=['mom/kids/foo.pro','mom/kids/bar.pro','mom/kids/zip.pro']
 *!--------------------------------------------------------------------*/
pathPlusFilesList([], _, []).
pathPlusFilesList([File | SourceFilesList], Path, 
					[XFile | ExtendedFilesList]) :-
	pathPlusFile(Path,File,XFile),
	pathPlusFilesList(SourceFilesList, Path, ExtendedFilesList).

/*!---------------------------------------------------------------------
 |	path_elements/2
 |	path_elements(Path, Elements)
 |	path_elements(+/-, +/-)
 |
 |	- compose/decompose a file system path from/to its elements
 |
 | 	If Path is a file system path to a file or directory, then
 |	this call decomposes Path into a list of it's minimal elements, 
 |	and unifies that list with Elements.  If Path is uninstantiated 
 |	and Elements is a list of path elements (some of which may consist 
 |	of more than a single element), this call composes the list
 |	Elements into an atom and unifies that atom with Path.
 |
 | Examples
 |	?- path_elements('mom/kids/foo.pro', E).
 |	E=[mom,kids,'foo.pro'] 
 |	?- path_elements(P, [mom,kids,'foo.pro']).
 |	P='mom/kids/foo.pro' 
 |	?- path_elements(P, ['mom/dad',kids,'foo.pro']).
 |	P='mom/dad/kids/foo.pro' 
 *!--------------------------------------------------------------------*/
path_elements(Path, Elements) :-
	var(Path),
	!,
	join_path(Elements, Path).
path_elements(Path, Elements) :-
	split_path(Path, Elements).

/*!---------------------------------------------------------------------
 |	path_type/2
 |	path_type(Path, Type)
 |	path_type(+, +)
 |
 |	- determines the type (absolute,relative) of a file system path
 |
 |	If Path is a file system path, unifies Type with 'absolute' or
 |	'relative' according to the type of Path.  Utilized by
 |	is_absolute_path/1 above.	
 | 
 | Examples
 |	?- path_type('mom/kids/foo.pro', T).
 |	T=relative 
 |	?- path_type('/mom/kids/foo.pro', T).
 |	T=absolute 
 *!--------------------------------------------------------------------*/
path_type(Path, Type) :-
	sys_env(OS, _, _),
	!,
	path_type(OS, Path, Type).

path_type(unix, Path, Type) :- unix_path_type(Path, Type).
path_type(macos, Path, Type) :- macos_path_type(Path, Type).
path_type(mswin32, Path, Type) :- win32_path_type(Path, Type).
path_type(win32, Path, Type) :- win32_path_type(Path, Type).

unix_path_type(Path, absolute) :-
	sub_atom(Path, 0, 1, _, '/'),
	!.
unix_path_type(Path, relative).

macos_path_type(Path, absolute) :-
	once(sub_atom(Path, Before, 1, _, ':')),
	Before > 0,
	!.
macos_path_type(Path, relative).

win32_path_type(Path, absolute) :-
	(find_sub(Path, 1, _, [':/', ':\\'])
	; find_sub(Path, 0, _, ['//', '\\\\', '/\\', '\\/'])),
	!.
win32_path_type(Path, volume_relative) :-
	(sub_atom(Path, 1, 1, _, ':')
	; find_sub(Path, 0, _, ['/', '\\'])),
	!.
win32_path_type(Path, relative).

/*!---------------------------------------------------------------------
 | 	split_path/2
 | 	split_path(Path, List)
 | 	split_path(+, -)
 |
 |	- decomposes Path into List of elements
 |
 |	If Path is a file system path, decomposes Path into a List
 |	of it's minimal elements, and unifies that list with List
 |	in an OS-independent manner. Utilized by path_elements/2 above.
 |
 | Examples
 |	?- split_path('mom/kids/foo.pro', List).
 |	List=[mom,kids,'foo.pro'] 
 *!--------------------------------------------------------------------*/
split_path(Path, List) :-
	sub_atom(Path,0,1,_,'{'),
	!,
	(sub_atom(Path,Before,1,_,'}') ->
		Before0 is Before - 1,
		sub_atom(Path,1,Before0,_,Path0)
		;
		sub_atom(Path,1,_,0,Path0)
	),
	split_path(Path0, List).
split_path(Path, List) :-
	sys_env(OS, _, _),
	!,
	split_path(OS, Path, List).

/*!---------------------------------------------------------------------
 |	join_path/2
 |	join_path(List, Path)
 |	join_path(+, -)
 | 
 |	- composes Path from a List of elements
 |
 |	If List consists of atoms representing elements of a file system
 |	path, composes Path out of List in an OS-independent manner.
 |
 | Examples
 |	?- join_path(['mom/dad',kids,'foo.pro'], Path),
 |	Path='mom/dad/kids/foo.pro'.
 *!--------------------------------------------------------------------*/
join_path(List, Path) :-
	sys_env(OS, _, _),
	join_path(OS, List, Path).

split_path(unix, Path, List) :- unix_split_path(Path, List).
split_path(macos, Path, List) :- macos_split_path(Path, List).
split_path(mswin32, Path, List) :- win32_split_path(Path, List).
split_path(win32, Path, List) :- win32_split_path(Path, List).

join_path(unix, List, Path) :- unix_join_path(List, Path).
join_path(macos, List, Path) :- macos_join_path(List, Path).
join_path(mswin32, List, Path) :- win32_join_path(List, Path).
join_path(win32, List, Path) :- win32_join_path(List, Path).

unix_split_path(Path, ['/' | Tail]) :-
	sub_atom(Path, 0, 1, _, '/'),
	!,
	sub_atom(Path, 1, _, 0, Remainder),
	unix_split_relpath(Remainder, Tail).
unix_split_path(Path, List) :-
	unix_split_relpath(Path, List).
 
unix_split_relpath('', []) :- !.
unix_split_relpath(Path, [Head | Tail]) :-
	sub_atom(Path, Before, 1, After, '/'),
	!,
	sub_atom(Path, 0, Before, _, Head),
	sub_atom(Path, _, After, 0, Remainder),
	unix_split_relpath(Remainder, Tail).
unix_split_relpath(Path, [Path]).

unix_join_path([], '').
unix_join_path([Path], Path) :- !.
unix_join_path([A, B], Path) :-
	!,
	unix_join_path(A, B, Path).
unix_join_path([Head | Tail], Path) :-
	unix_join_path(Tail, TailPath),
	unix_join_path([Head, TailPath], Path).

unix_join_path('/', B, Path) 
	:-
	atom_concat('/', B, Path),
	!.
unix_join_path(A, B, Path) :-
	sub_atom(A, _, 1, 0, '/'),
	!,
	atom_concat(A, B, Path).
unix_join_path(A, B, Path) :-
	atom_concat(A, '/', AHead),
	atom_concat(AHead, B, Path).

macos_split_path(':', [':']) :- !.
macos_split_path(Path, [Drive | Tail]) :-
	once(sub_atom(Path, Before, 1, After, ':')),
	Before > 0,
	!,
	DriveLen is Before + 1,
	sub_atom(Path, 0, DriveLen, _, Drive),
	sub_atom(Path, _, After, 0, Remainder),
	macos_split_relpath(Remainder, Tail).
macos_split_path(Path, List) :-
	macos_split_relpath(Path, List).

macos_split_relpath('', []) :- !.
macos_split_relpath(Path, ['::' | Tail]) :-
	sub_atom(Path, 0, 2, After, '::'),
	!,
	SubLen is After+1,
	sub_atom(Path, _, SubLen, 0, Remainder),
	macos_split_relpath(Remainder, Tail).
macos_split_relpath(Path, List) :-
	sub_atom(Path, 0, 1, After, ':'),
	!,
	sub_atom(Path, _, After, 0, Remainder),
	macos_split_relpath(Remainder, List).
macos_split_relpath(Path, [Head | Tail]) :-
	sub_atom(Path, Before, 1, After, ':'),
	!,
	sub_atom(Path, 0, Before, _, Head),
	SubLen is After+1,
	sub_atom(Path, _, SubLen, 0, Remainder),
	macos_split_relpath(Remainder, Tail).
macos_split_relpath(Path, [Path]).


macos_join_path([], '').
macos_join_path([Path], Path) :- !.
macos_join_path([A, B], Path) :-
	!,
	macos_join_path(A, B, Path).
macos_join_path([Head | Tail], Path) :-
	macos_join_path(Tail, TailPath),
	macos_join_path([Head, TailPath], Path).

macos_join_path(A, B, Path) :-
	once(sub_atom(A, Before, 1, _, ':')),
	Before >= 0,
	!,
	macos_join_path0(A, B, Path).
macos_join_path(A, B, Path) :-
	atom_concat(':', A, AHead),
	macos_join_path0(AHead, B, Path).
	
macos_join_path0(A, B, Path) :-
	sub_atom(A, Before, 1, 0, ':'),
	sub_atom(B, 0, 1, _, ':'),
	!,
	sub_atom(A, 0, Before, _, AHead),  
	atom_concat(AHead, B, Path).
macos_join_path0(A, B, Path) :-
	not(sub_atom(A, Before, 1, 0, ':')),
	not(sub_atom(B, 0, 1, _, ':')),
	!,
	atom_concat(A, ':', AHead),
	atom_concat(AHead, B, Path).
macos_join_path0(A, B, Path) :-
	atom_concat(A, B, Path).	

	 
win32_split_path(Path, [Drive | Tail]) :-
	find_sub(Path, 1, _, [':/', ':\\']),
	!,
	sub_atom(Path, 0, 3, _, Drive),
	sub_atom(Path, 3, _, 0, Remainder),
	win32_split_relpath(Remainder, Tail).
win32_split_path(Path, [DriveRel | Tail]) :-
	sub_atom(Path, 1, 1, _, ':'),
	!,
	sub_atom(Path, 0, 2, _, DriveRel),
	sub_atom(Path, 2, _, 0, Remainder),
	win32_split_relpath(Remainder, Tail).
win32_split_path(Path, [HostShare | Tail]) :-
	find_sub(Path, 0, _, ['//', '\\\\', '/\\', '\\/']),
	!,
	sub_atom(Path, 2, _, 0, HostPath),
	find_sub(HostPath, Before, _, ['/', '\\']),
	!,
	ShareStart is Before + 1,
	sub_atom(HostPath, ShareStart, _, 0, SharePath),
	find_sub(SharePath, RootBefore, _, ['/', '\\']),
	!,
	HostShareLength is RootBefore + ShareStart + 3,
	sub_atom(Path, 0, HostShareLength, _, HostShare),
	sub_atom(Path, HostShareLength, _, 0, Remainder),
	win32_split_relpath(Remainder, Tail).
win32_split_path(Path, ['/' | Tail]) :-
	sub_atom(Path, 0, 1, _, '/'),
	!,
	sub_atom(Path, 1, _, 0, Remainder),
	win32_split_relpath(Remainder, Tail).
win32_split_path(Path, ['\\' | Tail]) :-
	sub_atom(Path, 0, 1, _, '\\'),
	!,
	sub_atom(Path, 1, _, 0, Remainder),
	win32_split_relpath(Remainder, Tail).
win32_split_path(Path, List) :-
	win32_split_relpath(Path, List).

win32_split_relpath('', []) :- !.
win32_split_relpath(Path, []) :- 
	sub_atom(Path, 0, 1, _, '}').
win32_split_relpath(Path, [Head | Tail]) :-
	find_sub(Path, Before, After, ['/', '\\']),
	!,
	sub_atom(Path, 0, Before, _, Head),
	sub_atom(Path, _, After, 0, Remainder),
	win32_split_relpath(Remainder, Tail).
win32_split_relpath(Path, [Path]).

win32_join_path([], '').
win32_join_path([Path], Path) :-!.
win32_join_path([A, B], Path) :-
	!,
	win32_join_path(A, B, Path).
win32_join_path([Head | Tail], Path) :-
	win32_join_path(Tail, TailPath),
	win32_join_path([Head, TailPath], Path).

win32_join_path('/', B, Path) :-
	atom_concat('/', B, Path).
win32_join_path('\\', B, Path) :-
	atom_concat('\\', B, Path).
win32_join_path(A, B, Path) :-
	sub_atom(A, 1, 1, 0, ':'),
	!,
	atom_concat(A, B, Path).
win32_join_path(A, B, Path) :-
	(sub_atom(A, _, 1, 0, '/') ; sub_atom(A, _, 1, 0, '\\')),
	!,
	atom_concat(A, B, Path).
win32_join_path(A, B, Path) :-
	atom_concat(A, '\\', AHead),
	atom_concat(AHead, B, Path).


% Utility routine:

find_sub(_, _, _, []) :- !, fail.
find_sub(Atom, Before, After, [SubAtom | Tail]) :-
	!,
	(sub_atom(Atom, Before1, _, After1, SubAtom)
	; (Before1 = Before2, After1 = After2)),
	(find_sub(Atom, Before2, After2, Tail) ;
	(Before2 = Before1, After2 = After1)),
	nonvar(Before1), nonvar(Before2),
	!,
	(Before1 =< Before2 ->
		(Before = Before1, After = After1)
		;
		(Before = Before2, After = After2)).
find_sub(Atom, Before, After, SubAtom) :-
	!,
	sub_atom(Atom, Before, _, After, SubAtom).

rev_sub_atom(Atom, Before, Length, After, SubAtom) :-
	sub_atom(Atom, LBefore, Length, LAfter, SubAtom),
	!,
	sub_atom(Atom, _, LAfter, 0, Right),
	((rev_sub_atom(Right, RBefore, Length, After, SubAtom),
	 Before is LBefore + Length + RBefore) ; 
	(Before = LBefore, After = LAfter)).
rev_sub_atom(_, _, _, _, _) :- fail.

/*!---------------------------------------------------------------------
 |	directory_self/1
 |	directory_self(Self)
 |	directory_self(+/-)
 |
 |	- Returns an atom representing a relative path to the current directory.
 |
 |	Unifies Self with an atom representing the minimal relative path
 |	to the current directory, in an OS-independent manner.
 |
 | Examples
 |	?- directory_self(Self).
 |	Self='.' 
 |	?- directory_self('.').
 |	yes. 
 *!--------------------------------------------------------------------*/
directory_self(Self) :-
	sys_env(OS, _, _),
	!,
	directory_self(OS, Self).

directory_self(unix, '.').
directory_self(macos, ':').
directory_self(mswin32, '.').
directory_self(win32, '.').

/*!---------------------------------------------------------------------
 |	parent_path/1
 |	parent_path(PP)
 |	parent_path(+/-)
 |
 |	- Returns an atom representing a relative path to the parent of the current directory.
 |
 |	Unifies PP with an atom representing the minimal relative path
 |	to the parent of the current directory, in an OS-independent manner.
 |
 | Examples
 |	?- parent_path(PP).
 |	PP='..' 
 |	?- parent_path('..').
 |	yes.	
 *!--------------------------------------------------------------------*/
parent_path(PP) :-
	sys_env(OS, _, _),
	!,
	parent_path(OS, PP).

parent_path(unix, '..').
parent_path(macos, '::').
parent_path(mswin32, '..').
parent_path(win32, '..').

endmod.
