/*==================================================================
 |		fswin32.pro
 |	Copyright (c) 1996 Applied Logic Systems, Inc.
 |
 |	Miscellaneous low-level file system functions
 |		-- MS Windows32 Version 	(fswin32.pro)
 |
 |	Authors:	Chuck Hoput (based on other fs-xxx.pro files)
 |	Date:		1/96
 |	Note: Depends on core/alsp_src/win32/win32_makefile & friends
 *====================================================================*/

module builtins.

export make_subdir/1.
export make_subdir/2.
export recursive_dir_path/2.
export recursive_dir_paths/2.
export remove_subdir/1.
export file_status/2.

export files/2.
export files/3.
export subdirs/1.
export subdirs_red/1.
export directory/3.

export get_current_drive/1.
export change_current_drive/1.

export move_file/2.

/*!--------------------------------------------------------------
 |	make_subdir/1
 |	make_subdir(NewDir)
 |	make_subdir(+)
 |
 |	- creates a subdirectory in the current working directory
 |
 |      make_subdir(NewDir) calls make_subdir(NewDir,511)
 |      where 511 indicates full permissions [rwx,rwx,rwx]
 |
 | Examples
 |      ?- make_subdir(myNewFolder).
 |      %% Creates subdir myNewFolder below the current
 |      %% working directory, with permissions
 |      %%      [rwx,rwx,rwx]
 *!--------------------------------------------------------------*/

/*!--------------------------------------------------------------
 |	make_subdir/2
 |	make_subdir(NewDir,Permissions)
 |	make_subdir(+,+)
 |
 |	- creates a subdirectory in the current working directory
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
 |      ?- make_subdir(myNewFolder, 457).
 |      %% Creates subdir myNewFolder below the current
 |      %% working directory, with permissions
 |      %%      [rwx,x,x]
 *!--------------------------------------------------------------*/

make_subdir(NewDir)
	:-
	make_subdir(NewDir,511).

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

/*!--------------------------------------------------------------
 |	remove_subdir/1
 |	remove_subdir(SubDir)
 |	remove_subdir(+)
 |
 |	- removes a subdirectory from the current working directory
 |
 |	If SubDir is an atom, remove the subdirectory named SubDir from 
 |	the current working directory, if it exists.
 *!--------------------------------------------------------------*/
 
remove_subdir(SubDir)
	:-
	rmdir(SubDir).

/*!--------------------------------------------------------------
 |      kill_subdir/1
 |      kill_subdir(SubDir)
 |      kill_subdir(+)
 |
 |      - kills a (possibly nonempty) subdirectory from the current working directory
 |
 |      If SubDir is an atom, remove the subdirectory named SubDir from
 |      the current working directory, if it exists, even if it
 |	is non-empty.
 *!--------------------------------------------------------------*/
kill_subdir(SubDir)
        :-
        sprintf(atom(Cmd), 'rmdir /Q /S %t', [SubDir]),
        system(Cmd).

/*!----------------------------------------------------------------
 |        recursive_dir_path/2
 |        recursive_dir_path(Path_List, Path)
 |        recursive_dir_path(+, -)
 |
 |        Creates a nested directories path
 |
 |        If Path_List is a list of atoms which potentially describe
 |        a nested path of directories in the filesystem, (and which may
 |        need to be created), and if the last atom either describes a
 |        directory or a file, then:
 |        1) Path is an atom describing the path described by Path_List
 |              (as created by join_path/2), and
 |        2) That Path is created in the filesystem, if possible;
 |        2a) Moreover, either Path is absolute,
 |        2b) Or path is not absolute, and so is created relative to
 |                the current working directory.
 |        Fails if the mkdir command in the underlying filesystem (unix
 |        or mswin32) throws an error.
 |        If the underlying OS is mswin32, the first element of Path_List
 |        is permitted to be a drive letter atom (e.g., 'C:').
 |        If the underlying OS is mswin32,  enableextensions must be active.
 *!----------------------------------------------------------------*/
recursive_dir_path(Path_List, Path)
        :-
        join_path(Path_List, Path),
        sprintf(atom(Cmd), 'mkdir %t\n', [Path]),
        system(Cmd).

/*!----------------------------------------------------------------
 |        recursive_dir_paths/2
 |        recursive_dir_paths(List_of_Path_Lists, Paths)
 |        recursive_dir_paths(+, -)
 |
 |        Creates multiple nested directory paths
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



/*!----------------------------------------------------------------
 |	files/2
 |	files(Pattern,FileList)
 |	files(+,-)
 |
 |	- returns a list of files in the current directory matching a pattern
 |
 |	Returns the list (FileList) of all ordinary files in the 
 |	current directory which match Pattern, which can include
 |	the usual '*' and '?' wildcard characters.
 *!----------------------------------------------------------------*/
files(Pattern, FileList)
	:-
	directory(Pattern, regular, FileList).

/*!----------------------------------------------------------------
 |	files/3
 |	files(Directory, Pattern,FileList)
 |	files(+,+,-)
 |
 |	- returns a list of files residing in Directory matching a pattern
 |
 |	Returns the list (FileList) of all ordinary files in the 
 |	directory Directory which match Pattern, which can include
 |	the usual '*' and '?' wildcard characters.
 *!----------------------------------------------------------------*/
 
 files(Directory, Pattern, List) 
	:-
	getDirEntries(Directory, Pattern, FirstResult),
	!,
	fixFileType(regular, InternalFileType),
	filterForFileType(FirstResult, Directory, InternalFileType, List).

/*!----------------------------------------------------------------
 |	subdirs/1
 |	subdirs(SubdirList)
 |	subdirs(-)
 |
 |	- returns a list of subdirectories 
 |
 |	Returns the list of all subdirectories of the current 
 |	working directory.
 *!----------------------------------------------------------------*/
subdirs(SubdirList)
	:-
	directory('*',directory, SubdirList).

/*!----------------------------------------------------------------
 |	subdirs_red/1
 |	subdirs_red(SubdirList)
 |	subdirs_red(-)
 |
 |	- returns a list of subdirectories, omitting '.' and '..'
 |
 |	Returns the list of all subdirectories of the current 
 |	working directory, omitting '.' and '..'
 *!----------------------------------------------------------------*/
subdirs_red(SubdirList)
	:-
	directory('*',1,SubdirList0),
	list_delete(SubdirList0, '.', SubdirList1),
	list_delete(SubdirList1, '..', SubdirList).
 
 /*---------------------------------------------------------------
 |	File types/attributes:
 |		-- at the abstract (Prolog) level:
 |
 |		regular -- an ordinary file
 |		directory
 *---------------------------------------------------------------*/

/*
fileTypeCode('????', unknown).
fileTypeCode('Fldr', directory).
fileTypeCode('TEXT', regular).
fileTypeCode(_,unknown).
*/

ownerPermissionCoding(0,[]).
ownerPermissionCoding(4,[read]).
ownerPermissionCoding(6,[read,write]).


fileTypeCode(0, unknown).
fileTypeCode(1, directory).
fileTypeCode(4, regular).
fileTypeCode(5, symbolic_link).
fileTypeCode(5, alias).

fileTypeCode(30 , mac_type('TEXT')).


ownerPermissionsCoding(0,[]).
ownerPermissionsCoding(1,[execute]).
ownerPermissionsCoding(2,[write]).
ownerPermissionsCoding(3,[write,execute]).
ownerPermissionsCoding(4,[read]).
ownerPermissionsCoding(5,[read,execute]).
ownerPermissionsCoding(6,[read,write]).
ownerPermissionsCoding(7,[read,write,execute]).

/*!------------------------------------------------------------------
 *!-----------------------------------------------------------------*/
file_status(FileName,Status) :-
	'$getFileStatus'(FileName,
				fileStatus(FileTypeCode,ModTime,OwnerPermiss,ByteSize,NBlocks)),
	fileTypeCode(FileTypeCode,FileType),
%	ownerPermissionCoding(OwnerPermiss,Permissions),
	ownerPermissionsCoding(OwnerPermiss,Permissions),
	Status = [type=FileType,permissions=Permissions,mod_time=ModTime,size=ByteSize].

/*!------------------------------------------------------------------
 |	directory/3
 |	directory(Pattern,FileType,List)
 |	directory(+,+,-)
 |	
 |	- returns a list of files of a given type and matching a pattern
 |	
 |	If Pattern is a file name pattern, including possible '*' and
 |	'?' wildcard characters, and FileType is a numeric (internal)
 |	file type or a symbolic (abstract) file type, directory/3	
 |	unifies List with a sorted list of atoms of names of file of
 |	type FileType, matching Pattern, and found in the 
 |	current directory. 
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
	(InitPath='',!; exists_file(InitPath)),
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

filterForFileType([],  _, _, []).
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
	fflt_ck(ThisFileType, FileType, FullFile),
	!.
filter_file(FileName, Path, FileType, List, List).

fflt_ck(FileType, FileType, FullFile) :-!.
fflt_ck(ThisFileType, FileType, FullFile)
	:-
	dmember(ThisFileType, FileType),
	!.
	%% Incoming file is a symbolic link; dereference and try again:
fflt_ck(ThisFileType, FileType, FullFile)
	:-
	fileTypeCode(ThisFileType, symbolic_link),
	read_link(FullFile, LinkTarget),
	'$getFileStatus'(LinkTarget, LinkStatusTerm),
	arg(1, LinkStatusTerm, LinkFileType),
	fflt_ck(LinkFileType, FileType, LinkTarget).

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
 |	file_size/2
 |	file_size(FileName,Size)
 |	file_size(+,-)
 |
 |	- returns the size of a file
 |
 |	If File is an atom (possibly quoted) which is the name of a
 |	file in the current working directory, Size is the size of
 |	that file in bytes.
 *!----------------------------------------------------------------*/
file_size(_,0)
	:-
	prolog_system_error(nyi, ['file_size/2',unix]).

%--------------------------------------------------------------------
%	get_current_drive/1
%--------------------------------------------------------------------

get_current_drive(Drive)
	:-
	getcwd(Path),
%	rootPathFile(Drive,_,_,Path).
	split_path(Path, [Drive | _]).

%--------------------------------------------------------------------
%	change_current_drive/1.
%
%		We check to make sure that the final character in the drive name
%		is a colon, otherwise it is not a valid drive descriptor.
%--------------------------------------------------------------------

change_current_drive(DriveName)
	:-
	name(DriveName,DriveList),
	reverse(DriveList,[0': |_]),
	!,
	change_cwd(DriveName).

change_current_drive(DriveName) :-
	name(DriveName,DriveList),
	append(DriveList,[0':],ProperDriveList),
	name(ProperDriveName,ProperDriveList),
	change_cwd(ProperDriveName).
	
/*!----------------------------------------------------------------
 *!----------------------------------------------------------------*/
move_file(Source, Target)
	:-
	sprintf(atom(Cmd),'rename %t %t', [Source, Target]),
	system(Cmd).


endmod.

