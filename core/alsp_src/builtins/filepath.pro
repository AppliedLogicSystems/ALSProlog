/*=================================================================
 |	filepath.pro
 |	Copyright (c) 1991-92 Applied Logic Systems, Inc.
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

export filePlusExt/3.
export pathPlusFile/3.
export subPath/2.
export extendPath/3.
export subPath0/2.
export rootPlusPath/3.
export rootPlusPath0/3.
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
%%

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

	; OS = macos, !,	%% Mac
		addclause(builtins,file_separator('.')),
		addclause(builtins,directory_separator(':')),
		addclause(builtins,disk_separator(':')),
		addclause(builtins,
					(is_absolute_pathname(Path) :-
					 sub_atom(Path,1,1,C1), 
					 C1 \= ':', !) ),
		addclause(builtins,
					(is_absolute_pathname(Path) :-
					 atom_split(Path,':',_,_)))

	; OS = unix, !,		%% Unix
		addclause(builtins,file_separator('.')),
		addclause(builtins,directory_separator('/')),
		addclause(builtins,(disk_separator(_) :- !,fail)),
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

is_absolute_pathname(Path) :-
	directory_separator(DS),
	sub_atom(Path,1,_,DS),
	!.




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

	9/11/92:  I added clauses to subPath/2 which account for the 
	Mac's insistence that a "subpath" contain a leading colon.
	(Ron)
 *!-------------------------------------------------------*/

%% FIXME: Add leading colon stuff for the Mac

subPath([], '') :- !.
subPath([C | Cs], Path) :-
	nonvar(Path),
	directory_separator(Slash),
	atom_split(Path,Slash,C,SubPath),
	!,
	subPath(Cs,SubPath).
subPath([Path], Path) :- !.
subPath([C | Cs], Path) :-
	var(Path),
	directory_separator(Slash),
	subPath(Cs,SubPath),
	!,
	atom_split(Path,Slash,C,SubPath).

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
rootPlusPath(Disk, PathList, DiskPlusPath) :-
	var(DiskPlusPath),
	disk_separator(DS),
	subPath(PathList, Path),
	!,
	atom_split(DiskPlusPath,DS,Disk,Path).
rootPlusPath('', PathList, Path) :-
	subPath(PathList, Path).


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
