/*=================================================================
 |		filepath.pro
 |	Copyright (c) 1991-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |	Abstract handling of building and decomposing file names with paths.
 |
 |  Author: Ken Bowen
 |  Date:	May, 1991
 |	Revised with "root" terminology: October,1991
 |
 |	Terminology
 |	-----------
 |		File -- the name part of a file designator, as:
 |					zip in 'zip.pro' ;
 |		Extension -- the extension part of a file designator:
 |					pro in 'zip.pro' ;
 |		Full file name -- name part plus extension (if any):
 |					'zip.pro',    zip
 |		(Sub)path -- sequence of names of (sub)directories, each
 |					a subdirectory of the preceeding:
 |					foo/bar/silly
 |					foo\bar\silly
 |		Root -- the file root symbol, combined with a "disk" indicator,
 |			if any:
 |				'c:\'
 |				'\'
 |	Revised to use sub_atom, atom_concat, etc: November 1993 by K. Buettner
 *================================================================*/
module builtins.

export file_extension/3.
export path_elements/2.
export path_directory_tail/3.
export is_absolute_path/1.
export is_absolute_path/2.
export path_type/2.
export path_type/3.
export split_path/2.
export split_path/3.
export join_path/2.
export join_path/3.
export tilda_expand/2.
export directory_self/2.
export directory_self/3.

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
	sub_atom(Last, _, After, 0, Ext),
	sub_atom(Last, 0, Before, _, BName),
	dreverse([BName | RRestElts], NameElts),
	join_path(NameElts, Name).

file_extension(FullName, FileName, Ext) :-
	atom_concat(FileName,'.',FileNameDot),
	atom_concat(FileNameDot, Ext, FullName).

path_directory_tail(Path, Directory, Tail) :-
	var(Path),
	!,
	join_path([Directory, Tail], Path).
path_directory_tail(Path, Directory, Tail) :-
	split_path(Path, Elements),
	dreverse(Elements, [Tail | RevDirElements]),
	dreverse(RevDirElements, DirElements),
	(DirElements = [] -> directory_self(Directory) ; join_path(DirElements, Directory)).

path_elements(Path, Elements) :-
	var(Path),
	!,
	join_path(Elements, Path).
path_elements(Path, Elements) :-
	split_path(Path, Elements).


is_absolute_path(Path) :-
	path_type(Path, PathType),
	PathType \= relative.

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
win32_join_path([Path], Path).
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


% Utility routine.

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


tilda_expand(TildaPath, Path) :-
	sub_atom(TildaPath, 0, 1, _, '~'),
	split_path(TildaPath, [Head | Rest]),
	sub_atom(Head, 0, 1, After, '~'), 
	sub_atom(Head, 1, After, 0, Name),
	(After = 0 -> getenv('HOME', Home) ; get_user_home(Name, Home)),
	!,
	(Home = '' -> Path = TildaPath ; join_path([Home | Rest], Path)).
tilda_expand(Path, Path).

directory_self(Self) :-
	sys_env(OS, _, _),
	!,
	directory_self(OS, Self).

directory_self(unix, '.').
directory_self(macos, ':').
directory_self(mswin32, '.').
directory_self(win32, '.').


/*!-------------------------------------------------------*
	pathPlusFilesList/3.
	pathPlusFilesList(SourceFilesList, Path, ExtendedFilesList)
	pathPlusFilesList(+, +, -)

	- attaches a path to each of a list of file names

	If SourceFilesList is list of items denoting files, and
	if Path denotes a path, creates a list of atoms which
	consist of the Path prepended to each of the file names.
 *!-------------------------------------------------------*/

pathPlusFilesList([], _, []).
pathPlusFilesList([File | SourceFilesList], Path, 
					[XFile | ExtendedFilesList]) :-
	pathPlusFile(Path,File,XFile),
	pathPlusFilesList(SourceFilesList, Path, ExtendedFilesList).

/*!-------------------------------------------------------*
	same_path/2
	same_path(Path1, Path2)
	same_path(+, +)

	-	determines whether two file paths are the same

	If Path1 and Path2 are two lists denoting file paths,
	determines whether they denote the same path, allowing
	for identification of uppercase and lowercase names 
	as appropriate for the OS.
 *!-------------------------------------------------------*/

export same_path/2.
same_path([], []).
same_path([Node1 | RestPath1], [Node2 | RestPath2]) :-
	(Node1 = Node2 ;
			%% Must make case identif. conditional on the os:
		builtins:sys_env(OS,_,_),
		identify_case(OS),
		same_uc(Node1, Node2) ),
	!,
	same_path(RestPath1, RestPath2).

/*!-------------------------------------------------------*
	same_disk/2
	same_disk(Disk1, Disk2)
	same_disk(+, +)

	-	determines whether two disks are the same

	If Disk1 and Disk2 are atoms denoting disks, determines
	whether they are the same, allowing for identification
	of upper and lower case letters, as appropriate for the
	os.

 *!-------------------------------------------------------*/

export same_disk/2.
same_disk(Disk, Disk) :-
	!.
same_disk(Disk1, Disk2) :-
		%% Must make case identif. conditional on the os:
	builtins:sys_env(OS,_,_),
	identify_case(OS),
	same_uc(Disk1, Disk2).

%%% Convenience routines

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




endmod.
