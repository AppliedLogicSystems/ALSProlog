/*==================================================================
 |		fswin32.pro
 |	Copyright (c) 1996 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |	Miscellaneous low-level file system functions
 |		-- MS Windows32 Version 	(fswin32.pro)
 |
 |	Authors:	Chuck Hoput (based on other fs-xxx.pro files)
 |	Date:		1/96
 *====================================================================*/

module builtins.

export date/1.
export time/1.

export change_cwd/1.
export get_cwd/1.
export make_subdir/1.
export make_subdir/2.
export remove_subdir/1.
export remove_file/1.
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
 |	date/1
 |	date(Date)
 |	date(-)
 |
 |	 -	gets the current date
 |
 |	Unifies the input with the current date which is represented
 |	by a term of the form ??/??/??.  The exact pattern (e.g.,
 |	YY/MM/DD or MM/DD/YY or ....) is determined by date_pattern/4.
 *!--------------------------------------------------------------*/
date(Date)
	:-
	'$time'(_,_,_,DD,Month,YY,_,_,_),
	MM is Month + 1,
	date_pattern(YY,MM,DD,Date).

/*!--------------------------------------------------------------
 |	time/1
 |	time(HH:MM:SS)
 |	time(-)
 |
 |	 - gets the current time
 |
 |	Unifies the input with the current time which is represented
 |	by a term of the form HH:MM:SS.
 *!--------------------------------------------------------------*/
time(HH:MM:SS)
	:-
	'$time'(SS,MM,HH,_,_,_,_,_,_).

/*!--------------------------------------------------------------
 |	change_cwd/1
 |	change_cwd(NewDir)
 |	change_cwd(+)
 |
 |	- change the current working directory
 |
 |	Changes the current working directory being used by the program
 |	to become NewDir (which must be an atom). Under DOS, this won't 
 |	change the drive.
 *!--------------------------------------------------------------*/
change_cwd(Path)
	:-
	chdir(Path).

/*!--------------------------------------------------------------
 |	get_cwd/1
 |	get_cwd(Path) 
 |	get_cwd(-) 
 |
 |	- returns the current working directory
 |
 |	Returns the current working directory being used by the program
 |	as a quoted atom.  Under DOS, the drive is included.
 *!--------------------------------------------------------------*/
get_cwd(Path)
	:-
	getcwd(Path).

/*!--------------------------------------------------------------
 |	make_subdir/1
 |	make_subdir(NewDir)
 |	make_subdir(+)
 |
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
 |	remove_file/1
 |	remove_file(FileName)
 |	remove_file(+)
 |
 |	- removes a file from the current working directory
 |
 |	If FileName is an atom (possibly quoted) naming a file in
 |	the current working directory, removes that file.
 *!--------------------------------------------------------------*/

remove_file(FileName)
	:-
	unlink(FileName).

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
	'$getDirEntries'(Directory, Pattern, FirstResult),
	!,
	rootPlusPath(Disk, PathList, Directory),
	fixFileType(regular, InternalFileType),
	filterForFileType(FirstResult, Disk, PathList, InternalFileType, List).

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
	ownerPermissionCoding(OwnerPermiss,Permissions),
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
	rootPathFile(Disk, PathList, FilePattern, Pattern),
	subPath(PathList, Path),
	!,
	'$getDirEntries'(Path, FilePattern, FirstResult),
	!,
	fixFileType(FileType, InternalFileType),
	filterForFileType(FirstResult, Disk, PathList, InternalFileType, List).

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

filterForFileType([], _, _, _, []).
filterForFileType([FileName | Files], Disk, PathList, FileType, List)
	:-
	filter_file(FileName, Disk, PathList, FileType, List, ListTail),
	filterForFileType(Files, Disk, PathList, FileType, ListTail).

	%% Need this error case since '$getFileStatus'/2 can fail when given
	%% a symbolic link to a non-existent file:
filterForFileType([FileName | Files], Disk, PathList, FileType, List)
	:-
	filterForFileType(Files, Disk, PathList, FileType, List).

filter_file(FileName, Disk, PathList, FileType, [FileName | ListTail], ListTail)
	:-
	rootPathFile(Disk, PathList, FileName, FullFile),
	'$getFileStatus'(FullFile, StatusTerm),
	arg(1, StatusTerm, ThisFileType),
	fflt_ck(ThisFileType, FileType, FullFile),
	!.
filter_file(FileName, Disk, PathList, FileType, List, List).

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
	rootPathFile(Drive,_,_,Path).

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
	printf(atom(Cmd),'mv %t %t', [Source, Target]),
	system(Cmd).


endmod.

