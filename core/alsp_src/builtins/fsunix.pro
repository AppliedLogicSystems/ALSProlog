/*========================================================================sts=
 |		fsunix.pro
 |	Copyright (c) 1988-92 Applied Logic Systems, Inc.
 |
 |	Miscellaneous low-level file system functions
 |			-- Unix Version		(fsunix.pro)
 |
 |	Authors: Keith Hughes, Ilyas Cicekli, & Ken Bowen
 |	Date:	Begun 4/88
 |	Revision: Ken Bowen -- 11/88, 1/91 
 |		-- library version: 11/91
 |
 |	Note: Depends on core/alsp_src/unix/unix_makefile & friends
 |	Note: Some examples below utilize ls/0 as defined in blt_sys.pro.
 *===========================================================================*/
module builtins.

export file_status/2.
export files/2.
export files/3.
export move_file/2.
export make_subdir/1.
export make_subdir/2.
export subdirs/1.
export subdirs_red/1.
export remove_subdir/1.
export kill_subdir/1.
export directory/3.
export recursive_dir_path/2.
export recursive_dir_paths/2.
export get_current_drive/1.
export change_current_drive/1.

/*!------------------------------------------------------------------
 |	file_status/2
 |	file_status(FileName, Status) 
 |	file_status(+, -) 
 |
 |	- Returns OS information about a file named FileName
 |
 |	If FileName is the name associated with an entry in the
 |	OS filesystem, returns OS information about that entry,
 |	as in the following two examples:
 |
 | Examples
 |	?- file_status(alspro, Status).
 |
 |	Status=[type = regular,permissions = [read,write,execute],
 |	    mod_time = 1586731762.0,size = 462720] 
 |
 |	?- file_status(alsdir, Status).
 |
 |	Status=[type = directory,permissions = [read,write,execute],
 |	    mod_time = 1586652989.0,size = 204]
 *!-----------------------------------------------------------------*/
file_status(FileName, Status) 
	:-
	'$getFileStatus'(FileName,
			 fileStatus(FileTypeCode, ModTime, OwnerPermiss,
					ByteSize,NBlocks)),
	fileTypeCode(FileTypeCode,FileType),
	ownerPermissionsCoding(OwnerPermiss, Permissions),
	Status = [type=FileType, permissions=Permissions,
			  mod_time=ModTime, size=ByteSize].

/*---------------------------------------------------------------
 |	File types/attributes -- at the abstract (Prolog) level:
 |		regular -- an ordinary file
 |		directory
 *---------------------------------------------------------------*/

/*----------------------------------------------------------------
 | Unix file status/type codes:
 |
 |		0 = unknown
 |		1 = directory
 |		2 = character_special
 |		3 = block_special
 |		4 = regular
 |		5 = symbolic_link
 |		6 = socket
 |		7 = fifo_pipe
 *----------------------------------------------------------------*/

fileTypeCode(0, unknown).
fileTypeCode(1, directory).
fileTypeCode(2, character_special).
fileTypeCode(3, block_special).
fileTypeCode(4, regular).
fileTypeCode(5, symbolic_link).
fileTypeCode(6, socket).
fileTypeCode(7, fifo_pipe).

ownerPermissionsCoding(0,[]).
ownerPermissionsCoding(1,[execute]).
ownerPermissionsCoding(2,[write]).
ownerPermissionsCoding(3,[write,execute]).
ownerPermissionsCoding(4,[read]).
ownerPermissionsCoding(5,[read,execute]).
ownerPermissionsCoding(6,[read,write]).
ownerPermissionsCoding(7,[read,write,execute]).

/*!----------------------------------------------------------------
 |	files/2
 |	files(Pattern,FileList)
 |	files(+,-)
 |
 |	- returns a list of regular files in the current directory matching a pattern
 |
 |	Returns the list (FileList) of all ordinary (regular) files 
 |	in the current directory which match Pattern, which can 
 |	includethe usual '*' and '?' wildcard characters.
 |
 | Examples
 |      Executed in the ALS Prolog distribution directory:
 |      ?- files('*.pst', F).
 |
 |	F=['alsdev.pst','alspro.pst']
 |
 |	?-  files('*', F).
 |
 |	F=['LICENSE.txt','README.txt','als-prolog-manual.pdf',
 |	'als-ref-manual.pdf',alsdev,'alsdev.pst',alspro,'alspro.1',
 |	'alspro.pst','libalspro.a','libalspro.dylib','test.pro']
 *!----------------------------------------------------------------*/
files(Pattern, FileList)
	:-
	directory(Pattern, 4, FileList).

/*!----------------------------------------------------------------
 |	files/3
 |	files(Directory, Pattern,FileList)
 |	files(+,+,-)
 |
 |	- returns a list of regular files residing in Directory matching a pattern
 |
 |	Returns the list (FileList) of all ordinary files in the 
 |	directory Directory which match Pattern, which can include
 |	the usual '*' and '?' wildcard characters.
 |
 | Examples
 |      Executed in the ALS Prolog distribution directory:
 |	?- files('examples/more', '*', F).
 |
 |	F=['concurrent_interrupts.pro','core_concurrent.pro',
 |	   'finger.pro','freeze.pro','interrupts_coroutine.pro',
 |	   'mathserv1.pro','mathserv2.pro','primes_coroutine.pro',
 |	   'simple_coroutine.pro'] 	
 |
 |	?- files('examples/more', 'p*', F).
 |
 |	F=['primes_coroutine.pro'] 
 *!----------------------------------------------------------------*/
files(Directory, Pattern, List) 
	:-
	getDirEntries(Directory, Pattern, FirstResult),
	!,
	fixFileType(regular, InternalFileType),
	filterForFileType(FirstResult, Directory, InternalFileType, List).

/*!----------------------------------------------------------------
 |	move_file/2
 |	move_file(Source, Target)
 |	move_file(+, +)
 |
 |	- Change the name of a file from Source to Target
 |
 |	If both Source and Target are atoms which can be the
 |	names of a file, and if Source is the name of a file
 |	existing in the file system, then the name of that file
 | 	will be changed from Source to Target.
 |
 | Examples
 |      Executed in the ALS Prolog distribution directory:
 |	ls i*
 |	> ls i*
 |	ls: i*: No such file or directory
 |	?- move_file('README.txt', 'intro-README.txt').
 |	> ls i*
 |	intro-README.txt
 *!----------------------------------------------------------------*/
move_file(Source, Target)
	:-
	sprintf(atom(Cmd),'mv %t %t', [Source, Target]),
	system(Cmd).

/*!--------------------------------------------------------------
 |	make_subdir/1
 |	make_subdir(NewDir)
 |	make_subdir(+)
 |
 |	- creates a subdirectory in the current working directory
 |	- with default permissions
 |
 |	If NewDir is an atom, creates a subdirectory named NewDir in 
 |	the current working directory, if possible.
 |
 | Examples
 |      Executed in the ALS Prolog distribution directory:
 |  > ls
 | ALS_Prolog_Foreign_SDK/ alsdev*                 alspro.pst
 | LICENSE.txt             alsdev.pst              docs/
 | README.txt              alsdir/                 examples/
 | als-prolog-manual.pdf   alspro*                 libalspro.a
 | als-ref-manual.pdf      alspro.1                libalspro.dylib*
 | .....
 | ?- make_subdir(myNewTestSubdir).
 |
 | yes.
 | ?- halt.
 | ls
 | ALS_Prolog_Foreign_SDK/ alsdev.pst              examples/
 | LICENSE.txt             alsdir/                 libalspro.a
 | README.txt              alspro*                 libalspro.dylib*
 | als-prolog-manual.pdf   alspro.1                myNewTestSubdir/
 | als-ref-manual.pdf      alspro.pst
 | alsdev*                 docs/
 *!--------------------------------------------------------------*/
make_subdir(NewDir)
	:-
			%%[rwx,rwx,rwx]
	make_subdir(NewDir,511).

/*!--------------------------------------------------------------
 |	make_subdir/2
 |	make_subdir(NewDir,Permissions)
 |	make_subdir(+,+)
 |
 |	- creates a subdirectory in the current working directory
 |	- with the indicated permissions
 |
 |	If NewDir is an atom, creates a subdirectory named NewDir in 
 |	the current working directory, if possible; if Permissions
 |	is an integer of appropriate value, assigns the indicated
 |	permissions to the directory.
 |
 |	Permissions can appear in one of three forms:
 |
 |	*  An appropriate  integer: 0 =< N =< 511
 |	*  A list of approriate atoms, for example
 |		[rwx,rx,x]
 |	*  A list of lists of appropriate atoms, for example:
 |		[[read,write,execute],
 |		 [read,write],
 |		 [execute]  ]
 |
 | Examples
 |      Executed in the ALS Prolog distribution directory:
 |  > ls
 | ALS_Prolog_Foreign_SDK/ alsdev*                 alspro.pst
 | LICENSE.txt             alsdev.pst              docs/
 | README.txt              alsdir/                 examples/
 | als-prolog-manual.pdf   alspro*                 libalspro.a
 | als-ref-manual.pdf      alspro.1                libalspro.dylib*
 | .....
 | ?- make_subdir(myNewTestSubdir,457).
 | 
 | yes.
 | ?- halt.
 | > ls -l
 | total 26448
 | drwxr-xr-x  6 user  staff      204 Apr 17 15:01 ALS_Prolog_Foreign_SDK/
 | -rw-r--r--  1 user  staff     1101 Apr 17 15:01 LICENSE.txt
 | -rw-r--r--  1 user  staff     2738 Apr  9 09:33 README.txt
 | -rw-r--r--  1 user  staff  1938443 Apr  9 09:33 als-prolog-manual.pdf
 | -rw-r--r--  1 user  staff  1136668 Apr  9 09:33 als-ref-manual.pdf
 | -rwxr-xr-x  1 user  staff   482560 Apr 17 14:58 alsdev*
 | -rw-r--r--  1 user  staff  4194488 Apr 17 14:58 alsdev.pst
 | drwxr-xr-x  6 user  staff      204 Apr 17 14:57 alsdir/
 | -rwxr-xr-x  1 user  staff   462720 Apr 17 14:58 alspro*
 | -rw-r--r--  1 user  staff     8181 Apr  9 09:33 alspro.1
 | -rw-r--r--  1 user  staff  4194488 Apr 17 14:58 alspro.pst
 | drwxr-xr-x  7 user  staff      238 Apr 17 14:58 docs/
 | drwxr-xr-x  9 user  staff      306 Apr 17 15:01 examples/
 | -rw-r--r--  1 user  staff   634664 Apr 17 14:58 libalspro.a
 | -rwxr-xr-x  1 user  staff   463764 Apr 17 14:58 libalspro.dylib*
 | drwx--x--x  2 user  staff       68 Apr 19 19:03 myNewTestSubdir/
 *!----------------------------------------------------------------*/
make_subdir(NewDir,Permissions)
	:-
	integer(Permissions),
	!,
	0 =< Permissions, Permissions =< 512,
	mkdir(NewDir,Permissions).

make_subdir(NewDir,Permissions)
	:-
	symb_perms(Permissions,64,0,NumPermissions),
	mkdir(NewDir,NumPermissions).

symb_perms([],_,N,N)
	:-!.

symb_perms(_,0,N,N)
	:-!.

symb_perms([P | Perms],F,Cur,Final)
	:-
	(atom(P) ->
		name(P, PCodes)
		;
		PCodes = P
	),
	iperms(PCodes,F, 0,IT),
	Next is Cur + IT,
	NextF is F div 8,
	symb_perms(Perms,NextF,Next,Final).

iperms([],_,Cur,Cur).

iperms([PC | PCodes],F,Cur,IT)
	:-
	indv_perm_code(PC, PCVal),
	Next is Cur + (F * PCVal),
	iperms(PCodes,F,Next,IT).

indv_perm_code(0'r, 4).
indv_perm_code(0'w, 2).
indv_perm_code(0'x, 1).
indv_perm_code(read, 4).
indv_perm_code(write, 2).
indv_perm_code(execute, 1).

/*!----------------------------------------------------------------
 |	subdirs/1
 |	subdirs(SubdirList)
 |	subdirs(-)
 |
 |	- returns a list of subdirectories 
 |
 |	Returns the list of all subdirectories of the current 
 |	working directory.  On unix, the system files '.' and '..'
 |	are removed from the list; on mswin32, they are included.
 |
 | Examples
 |      Executed in the ALS Prolog distribution directory:
 |
 |  > ls
 | ALS_Prolog_Foreign_SDK/ alsdev*                 alspro.pst
 | LICENSE.txt             alsdev.pst              docs/
 | README.txt              alsdir/                 examples/
 | als-prolog-manual.pdf   alspro*                 libalspro.a
 | als-ref-manual.pdf      alspro.1                libalspro.dylib*
 | .....
 | ?- subdirs(SDs).
 | 
 | SDs=['ALS_Prolog_Foreign_SDK',alsdir,docs,examples] 
 | 
 | yes.
 *!----------------------------------------------------------------*/
subdirs(SubdirList)
	:-
	directory('./*',1,SubdirList).

/*!----------------------------------------------------------------
 |	subdirs_red/1
 |	subdirs_red(SubdirList)
 |	subdirs_red(-)
 |
 |	- returns a list of subdirectories, omitting '.' and '..'
 |
 |	Returns the list of all subdirectories of the current 
 |	working directory, omitting '.' and '..'
 |
 | Examples
 |      Executed in the ALS Prolog distribution directory:
 |
 |  > ls
 | ALS_Prolog_Foreign_SDK/ alsdev*                 alspro.pst
 | LICENSE.txt             alsdev.pst              docs/
 | README.txt              alsdir/                 examples/
 | als-prolog-manual.pdf   alspro*                 libalspro.a
 | als-ref-manual.pdf      alspro.1                libalspro.dylib*
 | .....
 | ?- subdirs_red(SDs).
 | 
 | SDs=['ALS_Prolog_Foreign_SDK',alsdir,docs,examples] 
 | 
 | yes.
 *!----------------------------------------------------------------*/
subdirs_red(SubdirList)
	:-
	directory('*',1,SubdirList0),
	list_delete(SubdirList0, '.', SubdirList1),
	list_delete(SubdirList1, '..', SubdirList).

/*!--------------------------------------------------------------
 |	remove_subdir/1
 |	remove_subdir(SubDir)
 |	remove_subdir(+)
 |
 |	- removes a subdirectory from the current working directory
 |
 |	If SubDir is an atom, remove the subdirectory named SubDir from 
 |	the current working directory, if it exists AND is empty.
 |
 | Examples
 |      Executed in the ALS Prolog distribution directory:
 |
 |  > mkdir funnyFolder
 |  > ls
 | ALS_Prolog_Foreign_SDK/ alsdev.pst              examples/
 | LICENSE.txt             alsdir/                 funnyFolder/
 | README.txt              alspro*                 libalspro.a
 | als-prolog-manual.pdf   alspro.1                libalspro.dylib*
 | als-ref-manual.pdf      alspro.pst
 | alsdev*                 docs/
 | .....
 | ?- remove_subdir(funnyFolder).
 |
 | yes.
 | .....
 |  > ls
 | ALS_Prolog_Foreign_SDK/ alsdev*                 alspro.pst
 | LICENSE.txt             alsdev.pst              docs/
 | README.txt              alsdir/                 examples/
 | als-prolog-manual.pdf   alspro*                 libalspro.a
 | als-ref-manual.pdf      alspro.1                libalspro.dylib*
 *!--------------------------------------------------------------*/
remove_subdir(SubDir)
	:-
	rmdir(SubDir).

/*!--------------------------------------------------------------
 |	kill_subdir/1
 |	kill_subdir(SubDir)
 |	kill_subdir(+)
 |
 |	- removes a subdirectory from the current working directory
 |
 |	If SubDir is an atom, remove the subdirectory named SubDir from 
 |	the current working directory, if it exists; SubDir may
 |	contain files and other subdirs.
 |
 | Examples
 |      Executed in the ALS Prolog distribution directory:
 |
 |  > mkdir funnyFolder
 |  > echo hiThere > funnyFolder/AFile
 |  > ls
 | ALS_Prolog_Foreign_SDK/ alsdev.pst              examples/
 | LICENSE.txt             alsdir/                 funnyFolder/
 | README.txt              alspro*                 libalspro.a
 | als-prolog-manual.pdf   alspro.1                libalspro.dylib*
 | als-ref-manual.pdf      alspro.pst
 | alsdev*                 docs/
 | > cat funnyFolder/AFile
 | hiThere
 | .....
 | ?- kill_subdir(funnyFolder).
 |
 | yes.
 | .....
 |  > ls
 | ALS_Prolog_Foreign_SDK/ alsdev*                 alspro.pst
 | LICENSE.txt             alsdev.pst              docs/
 | README.txt              alsdir/                 examples/
 | als-prolog-manual.pdf   alspro*                 libalspro.a
 | als-ref-manual.pdf      alspro.1                libalspro.dylib*
 *!--------------------------------------------------------------*/
kill_subdir(SubDir)
	:-
	sprintf(atom(Cmd),'rm -r %t',[SubDir]),
	system(Cmd).


/*!------------------------------------------------------------------
 |	directory/3
 |	directory(Pattern,FileType,List)
 |	directory(+,+,-)
 |	
 |	- returns a list of files of a given type and matching a pattern
 |	
 |	If Pattern is a file name pattern, including possible '*' and
 |	'?' wildcard characters, and FileType is a numeric (internal)
 |	file type or a symbolic (abstract) file type, (see fileTypeCode/2
 |      and ownerPermissionsCoding/2 following fileStatus above),
 |	directory/3 unifies List with a sorted list of atoms of names 
 |	of file of type FileType, matching Pattern, and found in the 
 |	current directory. 
 |
 | Examples
 |      Executed in the ALS Prolog distribution directory:
 |
 |  > ls
 | ALS_Prolog_Foreign_SDK/ alsdev*                 alspro.pst
 | LICENSE.txt             alsdev.pst              docs/
 | README.txt              alsdir/                 examples/
 | als-prolog-manual.pdf   alspro*                 libalspro.a
 | als-ref-manual.pdf      alspro.1                libalspro.dylib*
 | .....
 | ?- directory('*', 1, X).
 |
 | X=['ALS_Prolog_Foreign_SDK',alsdir,darwin,docs,examples]
 |
 | yes.
 | ?- directory('*.pst', 4, FL).
 |
 | FL=['alsdev.pst','alspro.pst']
 |
 | yes.
 *!------------------------------------------------------------------*/

	% If no pattern has been give, assume a complete wildcard is wanted:
directory(Var,FileType,List) 
	:- 
	var(Var),
	!,
	directory('*',FileType,List).

directory([ Pattern1 | Patterns ], FileType, List) 
	:-!,
	directory(Pattern1, FileType, List1),
	directory(Patterns, FileType, RestList),
	dappend(List1, RestList, List).

directory([], FileType, []) :-!.

directory(Pattern, FileType, List) 
	:-
	atom(Pattern), 
	path_directory_tail(Pattern, InitPath, FilePattern),
	(InitPath = '', !; must_exist_file(InitPath)),
	!,
	(InitPath = '' -> Path = '.' ; Path = InitPath),
	getDirEntries(Path, FilePattern, FirstResult),
	!,
	fixFileType(FileType, InternalFileType),
	filterForFileType(FirstResult, Path, InternalFileType, List).

	%% If no match was found for the file pattern, return no elements:
directory(_,_,[]).

fixFileType([], []).

fixFileType([FileType | FileTypes], [InternalFileType | IFileTypes])
	:-
	fixFileType(FileType, InternalFileType),
	fixFileType(FileTypes, IFileTypes).

fixFileType(FileType, FileType)
	:-
	integer(FileType),
	!.

fixFileType(FileType, InternalFileType)
	:-
	fileTypeCode(InternalFileType, FileType).

filterForFileType([], _, _, []).
filterForFileType([FileName | Files], Path, FileType, List)
	:-
	filter_file(FileName, Path, FileType, List, ListTail),
	!,
	filterForFileType(Files, Path, FileType, ListTail).

	%% Need this error case since '$getFileStatus'/2 can fail when given
	%% a symbolic link to a non-existent file:
filterForFileType([FileName | Files], Path, FileType, List)
	:-
	filterForFileType(Files, Path, FileType, List).

filter_file(FileName, Path, FileType, [FileName | ListTail], ListTail)
	:-
	join_path([Path, FileName], FullFile),
	'$getFileStatus'(FullFile, StatusTerm),
	arg(1, StatusTerm, ThisFileType),
	fflt_ck(ThisFileType, FileType, Path, FullFile),
	!.

filter_file(FileName, Path, FileType, List, List).

fflt_ck(FileType, FileType, SrcPathList, FullFile) :-!.
fflt_ck(ThisFileType, FileType, SrcPathList, FullFile)
	:-
	dmember(ThisFileType, FileType),
	!.

	%% See if incoming file is a symbolic link; dereference and check type:
fflt_ck(ThisFileType, GoalFileType, SrcPath, FullFile)
	:-
	follow_link(FullFile, FinalFile, SrcPath, FinalFileTypeCode),
	(FinalFileTypeCode = GoalFileType, !; 
		dmember(FinalFileTypeCode, GoalFileType)).

follow_link(File, FinalFile, SrcPath, FinalTypeCode)
	:-
	'$getFileStatus'(File, FileStatusTerm),
	arg(1, FileStatusTerm, FileTypeCode),
	fileTypeCode(FileTypeCode, FileType),
	disp_follow_link(FileType, File, FileTypeCode, SrcPath, FinalFile, FinalTypeCode).

disp_follow_link(symbolic_link, File, _, SrcPath, FinalFile, FinalTypeCode)
	:-!,
	get_cwd(CWD),
	read_link(File, LinkTarget),
	path_directory_tail(LinkTarget, Path, TFile), 
	(Path \= '' -> 
		(is_absolute_path(Path) ->
			change_cwd(Path),
			NextPathList = PathElts 
			;
			join_path([SrcPath,Path], NextPath),
			change_cwd(NextPath)
		)
		; 
		NextPath = SrcPath
	),
	fin_disp_follow_link(TFile, FinalFile, CWD, NextPath, FinalTypeCode).


disp_follow_link(FinalType, File, FileTypeCode, SrcPath, File, FileTypeCode).

fin_disp_follow_link(TFile, FinalFile, CWD, NextPathList, FinalTypeCode)
	:-
	follow_link(TFile, FinalFile, NextPathList, FinalTypeCode),
	!,
	change_cwd(CWD).

fin_disp_follow_link(TFile, FinalFile, CWD, NextPathList, FinalTypeCode)
	:-
	change_cwd(CWD),
	fail.

export disj_to_string/2.
disj_to_string((Pattern1 ; Pattern2), PatternChars)
	:-!,
	disj_to_string(Pattern1, Pattern1Cs),
	disj_to_string(Pattern2, Pattern2Cs),
	dappend(Pattern1Cs, [0'| | Pattern2Cs], PatternChars).

disj_to_string(Pattern, PatternChars)
	:-
	name(Pattern, PatternChars).

make_reg_exp([],[]).
make_reg_exp([0'? | RestPattern],[0'. | RestRegex])
	:-!,
	make_reg_exp(RestPattern,RestRegex).
make_reg_exp([0'. | RestPattern],[0'\\, 0'. | RestRegex])
	:-!,
	make_reg_exp(RestPattern,RestRegex).
make_reg_exp([0'* | RestPattern],[0'., 0'* | RestRegex])
	:-!,
	make_reg_exp(RestPattern,RestRegex).
make_reg_exp([C | RestPattern],[C | RestRegex])
	:-
	make_reg_exp(RestPattern,RestRegex).

/*!----------------------------------------------------------------
 |      recursive_dir_path/2
 |      recursive_dir_path(Path_List, Path)
 |      recursive_dir_path(+, -)
 |
 |      Creates a nested directories path
 |
 |      If Path_List is a list of atoms which potentially describe
 |      a nested path of directories in the filesystem, (and which may
 |      need to be created), and if the last atom either describes a
 |      directory or a file, then:
 |      1) Path is an atom describing the path described by Path_List
 |              (as created by join_path/2), and
 |      2) That Path is created in the filesystem, if possible;
 |      2a) Moreover, either Path is absolute,
 |      2b) Or path is not absolute, and so is created relative to
 |       the current working directory.
 |      Fails if the mkdir command in the underlying filesystem (unix
 |      or mswin32) throws an error.
 |      If the underlying OS is mswin32, the first element of Path_List
 |      is permitted to be a drive letter atom (e.g., 'C:').
 |      If the underlying OS is mswin32,  enableextensions must be active.
 |
 | Examples
 | 
 | ?- recursive_dir_path([dir1,dir2,dir3], PL).
 | 
 | PL='dir1/dir2/dir3' 
 | 
 | yes.
 | .....
 | > ls -d dir1
 | dir1/
 |
 | > ls -R dir1
 | dir2/
 | 
 | dir1/dir2:
 | dir3/
 | 
 | dir1/dir2/dir3:
 *!----------------------------------------------------------------*/
recursive_dir_path(Path_List, Path)
        :-
        join_path(Path_List, Path),
        sprintf(atom(Cmd), 'mkdir -p -- %t\n', [Path]),
        system(Cmd).

/*!----------------------------------------------------------------
 |      recursive_dir_paths/2
 |      recursive_dir_paths(List_of_Path_Lists, Paths)
 |      recursive_dir_paths(+, -)
 |
 |      Creates multiple nested directory paths
 |
 |      If List_of_Path_Lists is a list of lists of atoms each of which
 |      potentially describe a nested path of directories in the
 |      filesystem, (and which may need to be created), and if the
 |      last atom of each list either describes a directory or a file,
 |      then:
 |      1) The length of Paths equals the length of List_of_Path_Lists,
 |         and each element of Paths is an atom;
 |      2) For each list Path_List on List_of_Path_Lists, Path is the
 |         corresponding atom on Paths and
 |              recursive_dir_path(Path_List, Path)
 |         holds.
 |
 | Examples
 |       Multiple paths forming a tree:
 | 
 |         rr/
 |           qq/                  pp/
 |             kk/ mm/    nn/       aa/
 |                   jj/              bb/
 | 
 |         [[rr,qq,kk],[rr,qq,mm,jj],[rr,qq,nn],[rr,pp,aa,bb]]
 | 
 | ?- recursive_dir_paths([[rr,qq,kk],[rr,qq,mm,jj],[rr,qq,nn],[rr,pp,aa,bb]], PL).
 | 
 | PL=['rr/qq/kk','rr/qq/mm/jj','rr/qq/nn','rr/pp/aa/bb'] 
 | 
 | yes.
 | .....
 | > ls -d rr
 | rr/
 | 
 | > ls -R rr
 | pp/ qq/
 | 
 | rr/pp:
 | aa/
 | 
 | rr/pp/aa:
 | bb/
 | 
 | rr/pp/aa/bb:
 | 
 | rr/qq:
 | kk/ mm/ nn/
 | 
 | rr/qq/kk:
 | 
 | rr/qq/mm:
 | jj/
 | 
 | rr/qq/mm/jj:
 | 
 | rr/qq/nn:
 |  > 
 *!----------------------------------------------------------------*/
recursive_dir_paths(List_of_Path_Lists, Paths)
        :-
        prepare_path_cmd_list(List_of_Path_Lists, Paths, Markers),
        sys_env(OS, _, _),
        (OS == unix ->
                catenate(['mkdir -p -- ' | Markers], Pattern),
                sprintf(atom(Cmd), Pattern, Paths)
                ;
                catenate(['mkdir ' | Markers], Pattern),
                sprintf(atom(Cmd), Pattern, Paths)
        ),
        system(Cmd).

prepare_path_cmd_list([], [], []).
prepare_path_cmd_list([Path_List | RestList_of_Path_Lists],
                        [Path | RestCmdList], ['%t ' | RestMarkers])
        :-
        join_path(Path_List, Path),
        prepare_path_cmd_list(RestList_of_Path_Lists, RestCmdList, RestMarkers).


/* ----
 *	The following are essentially no-ops on unix, but
 *	need to do something for portability to mswin32.  
 *	In accordance with the conventions in filepath.pro, 
 * 	the "drive" is taken to be: root.
 *----*/
get_current_drive(root).

change_current_drive(_).

endmod.
