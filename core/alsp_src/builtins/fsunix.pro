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
 *===========================================================================*/
module builtins.

export make_subdir/1.
export make_subdir/2.
export remove_subdir/1.
export kill_subdir/1.
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
	%% This may go away later:
make_subdir(NewDir)
	:-
	sys_env(unix,djgpp,_),
	!,
	mkdir(NewDir).

make_subdir(NewDir)
	:-
			%%[rwx,rwx,rwx]
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

kill_subdir(SubDir)
	:-
	sprintf(atom(Cmd),'rm -r %t',[SubDir]),
	system(Cmd).

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
	directory(Pattern, 4, FileList).

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

/*!------------------------------------------------------------------
 |	file_status/2
 |	file_status(FileName, Status) 
 |	file_status(+, -) 
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

/*
 *	The following are essentially no-ops on unix, but
 *	need to do something for portability.  In accordance 
 *	with the conventions in filepath.pro, the "drive"
 *	is taken to be: root.
 */

get_current_drive(root).

change_current_drive(_).

/*!----------------------------------------------------------------
 *!----------------------------------------------------------------*/
move_file(Source, Target)
	:-
	sprintf(atom(Cmd),'mv %t %t', [Source, Target]),
	system(Cmd).

endmod.
