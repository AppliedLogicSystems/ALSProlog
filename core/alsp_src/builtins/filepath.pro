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
export file_directory/2.

/*
file_extension(Name, Ext, FullName) :-
	nonvar(FullName),
	!,
	once((
		rev_sub_atom(FullName, Before, 1, After, '.')
		;
		(atom_length(FullName, Before), After = 0)
	)),
	sub_atom(FullName, 0, Before, _, Name),
	sub_atom(FullName, _, After, 0, Ext).
*/
file_extension(Name, Ext, FullName) :-
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

file_extension(FileName,Ext,FullName) :-
	atom_concat(FileName,'.',FileNameDot),
	atom_concat(FileNameDot, Ext, FullName).

file_directory(FilePath, Directory)
	:-
	split_path(FilePath, PathElts),
	dreverse(PathElts, [_ | RevDirElts]),
	dreverse(RevDirElts, DirElts),
	join_path(DirElts, Directory).

path_elements(Path, Elements) :-
	var(Path),
	!,
	join_path(Elements, Path).
path_elements(Path, Elements) :-
	split_path(Path, Elements).


/*
is_absolute_path(Path) :-
	not(path_type(Path, relative)).
*/
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


%%%%%%%%%%%%%%%%%%%5-----------------------------------------
% BELOW IS OBSOLETE 


export filePlusExt/3.
export pathPlusFile/3.
export subPath/2.
export subPath/3.
export extendPath/3.
export rootPlusPath/3.
export rootPathFile/4. 
export pathPlusFilesList/3.

identify_case(dos).


%%
%% Figure out which system we are running in and set up some prolog facts
%% for the particular system.  These will be:
%%
%%	file_separator/1	-- character which indicates a filename
%%				extension such as the dot in .pro or .obp
%%	directory_separator/1	-- character which separates the subdirectory
%%				components in a pathname.  On most systems,
%%				if this character appears at the start of
%%				the pathname, then that pathname is absolute.
%%	disk_separator/1	-- character which separates the device name
%%				from the path (on systems where such a thing
%%				has any meaning).
%%	path_separator/1	-- character which separates paths such as 
%%				are found on the PATH environment variable.

:- 
	compiletime,
	sys_env(OS,_,_),
	( OS = dos, !,		%% DOS
		addclause(builtins,file_separator('.')),
		addclause(builtins,directory_separator('\\')),
		addclause(builtins,disk_separator(':')),
		addclause(builtins,path_separator(';')),
		addclause(builtins, 
			  (is_absolute_pathname(Path) :- 
				sub_atom(Path,2,1,':'),!) ),  
		addclause(builtins,
					(is_absolute_pathname(Path) :-
					 directory_separator(DS),
					 sub_atom(Path,1,_,DS), !) )
		
    ; OS = mswin32, !,		%% Microsoft Win32
		addclause(builtins,file_separator('.')),
		addclause(builtins,directory_separator('\\')),
		addclause(builtins,disk_separator(':')),
		addclause(builtins,path_separator(';')),
		addclause(builtins, 
			  (is_absolute_pathname(Path) :- 
				sub_atom(Path,2,1,':'),!) ),    
		addclause(builtins,
					(is_absolute_pathname(Path) :-
					 directory_separator(DS),
					 sub_atom(Path,1,_,DS), !) )

	; OS = macos, !,	%% Mac
		addclause(builtins,file_separator('.')),
		addclause(builtins,directory_separator(':')),
		%%addclause(builtins,disk_separator(':')),
		addclause(builtins,(disk_separator(_) :- !,fail)),
		addclause(builtins,path_separator(';')),
		addclause(builtins,
					(is_absolute_pathname(Path) :-
					 sub_atom(Path,1,1,C1),    
					 C1 \= ':',atom_split(Path,':',_,_),!) )
/*
		addclause(builtins,
					(is_absolute_pathname(Path) :-
					 atom_split(Path,':',_,_)))
*/
	; OS = unix, !,		%% Unix
		addclause(builtins,file_separator('.')),
		addclause(builtins,directory_separator('/')),
/*		addclause(builtins,(disk_separator(_) :- !,fail)),  */
			%% Use this for intel-based systems (djgpp2, mswins-xxx):
		addclause(builtins,disk_separator(':')),
		addclause(builtins,path_separator(':')),
		addclause(builtins,
					(is_absolute_pathname(Path) :-
					 directory_separator(DS),
					 sub_atom(Path,1,_,DS), !) )

	; OS = vms, !,		%% VMS -- FIXME: not correct
		addclause(builtins,file_separator('.')),
		addclause(builtins,directory_separator(']')),
		addclause(builtins,(disk_separator(_) :- !,fail)),
		addclause(builtins,path_separator(',')),
		addclause(builtins,
					(is_absolute_pathname(Path) :-
					 directory_separator(DS),
					 sub_atom(Path,1,_,DS), !) )
	; true ).

%%
%% is_absolute_pathname/1 succeeds if its single argument is an atom
%% representing an absolute pathname.  Note that DOS systems have an
%% additional clause preceding this one which gets defined above.
%%

/*

is_absolute_pathname(Path) :-
	directory_separator(DS),
	sub_atom(Path,1,_,DS),
	!.
*/



%% atom_split/4 -- like asplit, but Splitter is also an atom. 
%% In addition, atom_split may be used for constructing atoms as well
%% as splitting them.

export atom_split/4.
atom_split(Atom, Splitter, Left, Right) :-
	var(Atom),
	!,
	atom_concat(Left,Splitter, Left_plus_Splitter),
	atom_concat(Left_plus_Splitter, Right, Atom).
atom_split(Atom, Splitter, Left, Right) :-
	sub_atom(Atom, SplitPos, SplitLen, Splitter),
	LeftLen is SplitPos - 1,
	sub_atom(Atom, 1, LeftLen, Left),
	atom_length(Atom, AtomLen),
	RightPos is SplitPos + SplitLen,
	RightLen is AtomLen - RightPos + 1,
	sub_atom(Atom, RightPos, RightLen, Right).

%% atom_rsplit/4 -- like atom_split, except it starts from the right.

export atom_rsplit/4.

atom_rsplit(Atom, Splitter, Left, Right) :-
	atom_split(Atom, Splitter, L, R),
	atom_rsplit(R, Splitter, LL, Right),
	atom_split(Left, Splitter, L, LL).
atom_rsplit(Atom, _, '', Atom).

/*!--------------------------------------------------------
	dirFilePath/3
	dirFilePath(Directory, File, FullFilePath).
	dirFilePath(+, +, -)
	
	- decomposes a full path name into a file and
	  directory.
 *!-------------------------------------------------------*/

export dirFilePath/3.

dirFilePath(Dir, File, Path) :-
	directory_separator(S),
	atom_rsplit(Path, S, Dir, File).

/*!--------------------------------------------------------
	filePlusExt/3.
	filePlusExt(FileName,Ext,FullName)
	filePlusExt(+,+,-)
	filePlusExt(-,-,+)

	- builds or decomposes a file plus extension

	1) If FileName and Ext are atoms or UIAs, composes them
	into the complete name FullName with appropriate separator
	(e.g., foo,bar --> 'foo.bar' );
	2) If FullName is instantiated to an atom or UIA which is
	an appropriate complete file name, decomposes it into the
	FileName proper and the extension, Ext;
	(e.g., 'foo.bar' --> foo, bar ).  If FullName does not
	have an extension (e.g., 'foo'), fails.
 *!-------------------------------------------------------*/	


filePlusExt(FileName,Ext,FullName) :-
	nonvar(FullName),!,
	pathPlusFile(Path,FilePlusExt,FullName),
	file_separator(Sep),
	atom_split(FilePlusExt, Sep, FileName0, Ext),
	!,
	pathPlusFile(Path,FileName0,FileName).

filePlusExt(FileName,Ext,FullName) :-
	file_separator(Sep),
	atom_concat(FileName,Sep,FileName_plus_Sep),
	atom_concat(FileName_plus_Sep, Ext, FullName).


/*!--------------------------------------------------------
	pathPlusFile/3.
	pathPlusFile(Path,File,CompletePath)
	pathPlusFile(+,+,-)
	pathPlusFile(-,-,+)

	- builds or decomposes a path plus a file name

	1)	If Path is an atom or UIA instantiated to a path
	to a (sub)directory, and File is an atom or UIA denoting
	a file, composes them into a full name of the file,
	CompletePath; 
	( '/u/prolog/alsdir', 'builtins.pro' -->
			'/u/prolog/alsdir/builtins.pro')
	2)	If CompletePath is an atom or UIA instatitated to the
	complete name of a file, decomposes it into the path and
	file parts (e.g., '/u/prolog/alsdir/builtins.pro' -->
	'/u/prolog/alsdir', 'builtins.pro' ).
 *!-------------------------------------------------------*/

pathPlusFile(Path,File,CompletePath)
	:-
	var(CompletePath),
	!,
	subPath(PathList, Path),
	dappend(PathList, [File], CompletePathList),
	subPath(CompletePathList, CompletePath).

pathPlusFile(Path,File,CompletePath)
	:-
	subPath(CompletePathList,CompletePath),
	dappend(PathList, [File], CompletePathList),
	subPath(PathList, Path).


/*!-------------------------------------------------------*
	subPath/2.
	subPath(PathList,SubPath)
	subPath(+,-)
	subPath(-,+)

	- builds or decomposes a subdirectory path

	1)	If PathList is a list of atoms or UIAs which are
	directory names (intended to be successive subdirectories),
	creates the corresponding UIA SubPath denoting that path
	(['',usr,bin,prolog] --> '/usr/bin/prolog').
	2)	If SubPath is an atom or UIA appropriately describing
	a (sub)path, decomposes this into a list, PathList, of
	the atoms constituting the (sub)directories in the path
	('/usr/bin/prolog' --> ['',usr,bin,prolog] ).

 *!-------------------------------------------------------*/

subPath(List, Path) :- list_path(List, Path).
subPath(List, Path, OS) :- list_path(OS, List, Path).

directory_separator(unix, '/').
directory_separator(mswin32, '\\').

list_path(List, Path) :-
	sys_env(OS, _, _),
	list_path(OS, List, Path).

list_path(OS, List, Path) :-
	var(List),
	!,
	path_to_list(OS, Path, List), !.
list_path(OS, List, Path) :-
	nonvar(List),
	!,
	list_to_path(OS, List, Path), !.
	
list_to_path(unix, List, Path) :-
	unix_win32_list_to_path(List, Path, unix).
list_to_path(mswin32, List, Path) :-
	unix_win32_list_to_path(List, Path, mswin32).
list_to_path(macos, List, Path) :-
	macos_list_to_path(List, Path).

unix_win32_list_to_path([], '', OS).
unix_win32_list_to_path([''], DS, OS) :-
	directory_separator(OS, DS).
unix_win32_list_to_path([X], X, OS).
unix_win32_list_to_path([Head | Tail], Path, OS) :-
	directory_separator(OS, DS),
	unix_win32_list_to_path(Tail, TailPath, OS),
	atom_concat(Head, DS, HeadPath),
	atom_concat(HeadPath, TailPath, Path).

macos_list_to_path([], '').
macos_list_to_path([''], _) :-
	throw(error(domain_error(path_list, ['']), builtins:subPath/2)).
macos_list_to_path([X], X).
macos_list_to_path(['', X], Path) :-
	atom_concat(X, ':', Path).
macos_list_to_path(['', X | Tail], Path) :-
	macos_list_to_rel_path(Tail, TailPath),
	atom_concat(X, TailPath, Path).
macos_list_to_path(List, Path) :-
	macos_list_to_rel_path(List, Path).
	
macos_list_to_rel_path([], '').
macos_list_to_rel_path(['::'], '::').
macos_list_to_rel_path([X], Path) :-
 	atom_concat(':', X, Path).
macos_list_to_rel_path(['::' | Tail], Path) :-
	macos_list_to_rel_path(Tail, TailPath),
	atom_concat(':', TailPath, Path).
macos_list_to_rel_path([Head | Tail], Path) :-
	macos_list_to_rel_path(Tail, TailPath),
	atom_concat(':', Head, HeadPart),
	atom_concat(HeadPart, TailPath, Path).
 	
path_to_list(unix, Path, List) :-
	unix_win32_path_to_list(Path, List, unix).
path_to_list(mswin32, Path, List) :-
	unix_win32_path_to_list(Path, List, mswin32).
path_to_list(macos, Path, List) :-
	macos_path_to_list(Path, List).

unix_win32_path_to_list('', [], _).
unix_win32_path_to_list(Path, [C | Cs], OS) :-
	directory_separator(OS, Slash),
	atom_split(Path,Slash,C,SubPath),
	!,
	unix_win32_path_to_list(SubPath, Cs, OS).
unix_win32_path_to_list(X, [X], _).

macos_path_to_list('', []).
macos_path_to_list(':', []).
macos_path_to_list(Path, ['' | Tail]) :-
	not(sub_atom(Path, 1, 1, ':')),
	sub_atom(Path, _, 1, ':'),
	atom_concat(':', Path, RelativePath),
	macos_path_to_list(RelativePath, Tail).
macos_path_to_list(Path, ['::' | Tail]) :-
	sub_atom(Path, 1, 2, '::'),
	atom_length(Path, PathLength),
	TailPathLength is PathLength - 1,
	sub_atom(Path, 2, TailPathLength, TailPath),
	macos_path_to_list(TailPath, Tail).
macos_path_to_list(Path, [Head | Tail]) :-
	sub_atom(Path, 1, 1, ':'),
	atom_length(Path, PathLength),
	((sub_atom(Path, HeadEnd, 1, ':'), HeadEnd > 1)
	 ; HeadEnd is PathLength + 1),
	HeadLength is HeadEnd - 2,
	sub_atom(Path, 2, HeadLength, Head),
	TailLength is 1 + PathLength - HeadEnd,
	sub_atom(Path, HeadEnd, TailLength, TailPath),
	macos_path_to_list(TailPath, Tail).
macos_path_to_list(X, [X]).

/*!-------------------------------------------------------*
 |	extendPath/3
 |	extendPath(Upper,Lower,Whole)
 |	extendPath(+,+,-)
 |
 | -	'glues together' two path segments
 *!-------------------------------------------------------*/
extendPath(Upper,Lower,Whole)
	:-
	subPath(UpperList,Upper),
	subPath(LowerList,Lower),
	append(UpperList,LowerList,WholeList),
	subPath(WholeList,Whole).


/*!-------------------------------------------------------*
	rootPlusPath/3
	rootPlusPath(Disk, Path, DiskPlusPath)
	rootPlusPath(+, +, -)
	rootPlusPath(-, -, +)

	- builds or decomposes a root (disk) plus a path

	1)	If Disk is an atom or UIA denoting a root, and if
	Path is a list of atoms or UIAs denoting a (sub)directory
	Path (as appropriate for subPath/2), composes these together
	to produce an atom DiskPlusPath denoting the rooted
	path ( c,[usr,bin,prolog] --> 'c:\usr\bin\prolog');
	recognizes 'root' as a distinguished "disk" name for
	systems such as unix where no named disks are used:
	( root, [usr,bin,prolog] --> '\usr\bin\prolog' )
	2)	If DiskPlusPath is an atom or UIA denoting a rooted
	path (ie, beginning with a root), decomposes this to produce an 
	atom Disk naming the root, and a list Path of atoms denoting the 
	sequence of subdirectories in the path as appropriate for subPath/2;
	( 'foo:\usr\bin\prolog' -->  foo,[usr,bin,prolog] ).
	( '\usr\bin\prolog' -->  root,[usr,bin,prolog] ).
 *!-------------------------------------------------------*/

rootPlusPath(Disk, PathList, DiskPlusPath) :-
	nonvar(DiskPlusPath),
	disk_separator(DS),
	atom_split(DiskPlusPath,DS,Disk,Path),
	!,
	subPath(PathList, Path).

rootPlusPath('', PathList, Path) 
	:-
	subPath(PathList, Path),
	!.

rootPlusPath(root, PathList, DiskPlusPath) 
	:-
	var(DiskPlusPath),
	!,
	(PathList = ['' | _] -> NPL = PathList ; NPL = ['' | PathList]),
	rootPlusPath('', NPL, DiskPlusPath).

rootPlusPath(Disk, PathList, DiskPlusPath) 
	:-
	var(DiskPlusPath),
	disk_separator(DS),
	atom_concat(Disk, DS, DiskWithSep),
	subPath(PathList, Path),
	!,
	atom_concat(DiskWithSep, Path, DiskPlusPath).

/*!-------------------------------------------------------*
	rootPathFile/4
	rootPathFile(Disk,Path,File,CompletePath)
	rootPathFile(+,+,+,-)
	rootPathFile(-,-,-,+)

	- builds or decomposes a root(disk), path, and file name

	1)	If Disk, File are atoms or UIAs, denoting a disk and
	a file, respectively, and if Path is a listing of atoms
	denoting a (sub)directory path, composes these to produce
	CompletePath, an atom denoting the complete name of the file
		c, [usr,bin,prolog], 'alspro.exe' -->
			'c:\usr\bin\prolog\alspro.exe'
	2)	If CompletePath is an atom or UIA denoting a file,
	decomposes this to Disk,Path,File:
		'c:\usr\bin\prolog\alspro.exe'  -->
				c, [usr,bin,prolog], 'alspro.exe'
		'c:\usr\bin\prolog\foobar'  -->
				c, [usr,bin,prolog], foobar     ).
		'/usr/bin/prolog/alspro.exe'  -->
				root, [usr,bin,prolog], 'alspro.exe' 
		'/usr/bin/prolog/foobar'  
				--> root, [usr,bin,prolog], foobar     ). 
 *!-------------------------------------------------------*/ 

rootPathFile(Disk, Path, File, CompletePath) :-
	var(CompletePath),
	!,
	(File = '' ->
		PathPlusFile = Path
		;
		dappend(Path, [File], PathPlusFile)
	),
	rootPlusPath(Disk, PathPlusFile, CompletePath).

rootPathFile(Disk,Path,File,CompletePath) :-
	rootPlusPath(Disk, PathPlusFile, CompletePath),
	dappend(Path, [File], PathPlusFile).



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

endmod.
