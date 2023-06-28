/*======================================================================
 |		fsdos.pro
 |		Copyright (c) 1988-91 Applied Logic Systems, Inc.
 |
 |	Miscellaneous low-level file system functions
 |			-- Unix Version		(fsunix.pro)
 |			-- DOS 386 Version	(fsdos386.pro)
 |			-- VMS Version 		(fsvms.pro)
 |
 |	Authors: Keith Hughes, Bola Odulate, and Nick Karonis
 |	Date:	4/88
 |	Revision: Ken Bowen -- 11/88
 |		-- library version: 11/91
 *=======================================================================*/
module builtins.

export date/1.
export time/1.
export make_subdir/1.
export remove_subdir/1.
export file_status/2.
export files/2.
export files/3.
export subdirs/1.
export subdirs_red/1.
export collect_files/3.
export directory/3.
export get_current_drive/1.
export change_current_drive/1.

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
date(Date) 				% get current date
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
time(HH:MM:SS) 				% get current time
	:-
	'$time'(SS,MM,HH,_,_,_,_,_,_).

/*!--------------------------------------------------------------
 |	make_subdir/1
 |	make_subdir(NewDir)
 |	make_subdir(+)
 |
 |	- creates a subdirectory in the current working directory
 |
 |	If NewDir is an atom, creates a subdirectory named NewDir in 
 |	the current working directory, if possible.
 *!--------------------------------------------------------------*/
make_subdir(NewDir)
	:-
	'$mkdir'(NewDir).

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
	'$rmdir'(SubDir).

/*!----------------------------------------------------------------
 |	files/2
 |	files(Pattern,FileList)
 |	files(+,-)
 |
 |	- returns a list of files matching a pattern
 |
 |	Returns the list (FileList) of all ordinary files in the 
 |	current directory which match Pattern, which can include
 |	the usual '*' and '?' wildcard characters.
 *!----------------------------------------------------------------*/
files(Pattern, FileList)
	:-
	directory(Pattern, regular, FileList1),
	directory(Pattern, read_only, FileList2),
    sorted_merge([FileList1,FileList2], FileList).

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
	directory('*',directory,SubdirList).

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
	directory('*',directory,SubdirList0),
	list_delete(SubdirList0, '.', SubdirList1),
	list_delete(SubdirList1, '..', SubdirList).

/*!----------------------------------------------------------------
 |	collect_files/3
 |	collect_files(PatternList,FileType,FileList)
 |	collect_files(+,+,-)
 |
 |	- returns a list of files meeting conditions
 |
 |	If PatternList is a list of file name patterns, possibly
 |	including the usual wildcard characters '*' and '?', and
 |	if FileType is a standard FileType (see below), then FileList
 |	is a sorted list of all files in the current working directory
 |	which are of type FileType, and which match at least one of
 |	the patterns on PatternList.  Works by recursively working down 
 |	PatternList and calling directory/3 on each element, and then
 |	doing a sorted merge on the resulting lists.
 *!----------------------------------------------------------------*/
collect_files([],FileType,[]).
collect_files([Pattern | RestPatterns],FileType,FileList)
	:-
	fileTypeCode(InternalType, FileType),
	collect_files0([Pattern | RestPatterns],InternalType,FileListList),
	sorted_merge(FileListList, FileList).

collect_files0([], _, []).
collect_files0([Pattern | RestPatterns], InternalType,
				[FileList | RestFileList])
	:-
	directory(Pattern, InternalType, FileList),
	collect_files0(RestPatterns, InternalType, RestFileList).

/*----------------------------------------------------------------
 | DOS file status/type codes returned from a call to $getFileStatus
 | File Type  :
 |		0 = unknown
 |		1 = directory
 |		2 = regular
 | File Permission :
 | 	        read << 2 | write << 1 | execute
 *----------------------------------------------------------------*/

fileTypeCode(0, unknown).
fileTypeCode(1, directory).
fileTypeCode(2, regular).

ownerPermissionsCoding(0,[]).
ownerPermissionsCoding(1,[execute]).
ownerPermissionsCoding(2,[write]).
ownerPermissionsCoding(3,[write,execute]).
ownerPermissionsCoding(4,[read]).
ownerPermissionsCoding(5,[read,execute]).
ownerPermissionsCoding(6,[read,write]).
ownerPermissionsCoding(7,[read,write,execute]).

/*---------------------------------------------------------------------
 *---------------------------------------------------------------------*/
file_status(FileName,Status)
	:-
	'$getFileStatus'(FileName,
			 fileStatus(FileTypeCode, ModTime, OwnerPermiss,
					ByteSize, DriveNum)),
	fileTypeCode(FileTypeCode,FileType),
	ownerPermissionsCoding(OwnerPermiss, Permissions),
	Status = [type=FileType, permissions=Permissions,
			  mod_time=ModTime, size=ByteSize].

/*!-------------------------------------------------------------------
 * 			directory(Pattern,Attribute,List)
 *
 * Unify List with a sorted list of atoms of file names found in the current
 * directory. Pattern and Attribute dictate what sorts of files are searched
 * for. Pattern says what sort of files names to look for, using the
 * normal conventions of * and ? as wildcards in the name. Attribute
 * refers to an integer for the file attribute that DOS gives to every file.
 * Cf. pg 116 in P. Norton, Programmer's Guide to the IBM PC (microSoft Press)
 * for a description of the attributes.
 *
 *	Example attributes
 *		0  - Normal files (read-write)
 *		1  - Normal read-only files
 *		2  - Hidden files
 *		4  - System files
 *		8  - VolId files
 *		16 - sub-directories
 *	        32 - Archive files
 *--------------------------------------------------------------------*/

	% If no pattern has been give, assume a wildcard is wanted.
directory(Pattern,FileType,List) 
	:- 
	var(Pattern),
	!,
	directory('*.*',FileType,List).

directory([ Pattern1 | Patterns ], FileType, List) 
	:-!,
	directory(Pattern1, FileType, List1),
	directory(Patterns, FileType, RestList),
	dappend(List1, RestList, List).

directory([], FileType, []) :-!.

directory(FileNamePattern, FileType, List) 
	:-
	fileTypeToAttr(FileType, Attribute),
	getDirEntries(FileNamePattern, Attribute, List).

fileTypeToAttr(Attr, Attr) :-
	integer(Attr),
	!.
fileTypeToAttr(regular,    0).
fileTypeToAttr(read_only,  1).
fileTypeToAttr(hidden,     2).
fileTypeToAttr(system,     4).
fileTypeToAttr(volid,      8).
fileTypeToAttr(directory, 16).
fileTypeToAttr(archive,   32).

/*---------------------------------------------------------------------
 |	Determine current disk drive. Drive = constant (1=A, 2=B, etc).
 *---------------------------------------------------------------------*/
get_current_drive(Drive)
	:-
	'$getdrive'(DriveNum),
	W is DriveNum - 1 + 0'A,
	name(Drive,[W]).

/*---------------------------------------------------------------------
 |	Change the current drive being used. Can be used in one of two ways,
 |	by either giving the number of the drive (A=0, B=1, etc), or by
 |	giving the drive name (A, B, etc).
 *---------------------------------------------------------------------*/

		%% Handle the A,B,C etc case.
change_current_drive(Drive) 
	:-
	atom(Drive),
	!,
			% See if one character and convert to the drive number
	name(Drive,[H0]),
	(0'a =< H0 ->
		H is H0 -32
	;
		H = H0
	),
	DriveNum is H - 0'A + 1,
	1 =< DriveNum, DriveNum =< 26,
	'$chdrive'(DriveNum).

		%% Handle the numeric case
change_current_drive(DriveNum) 
	:-
	integer(DriveNum),
			% Only drives 1-26 are allowed.
	1 =< DriveNum, DriveNum =< 26,
	'$chdrive'(DriveNum).

endmod.
