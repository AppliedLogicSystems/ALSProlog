/*==================================================================
|		fsmac.pro
|		Copyright (c) 1992 Applied Logic Systems, Inc.
|
|	Miscellaneous low-level file system functions
|		-- Macintosh Version (MacOS) 	(fsmac.pro)
|
|	Author:		Ron DiNapoli
|	Date:		8/92
|	Revisions:
|
*====================================================================*/

library_key(filesys).

module builtins.

export get_cwd/1.
export get_current_drive/1.
export file_status/2.
export change_cwd/1.
export change_current_drive/1.

fileTypeCode('????', unknown).
fileTypeCode('Fldr', directory).
fileTypeCode('TEXT', regular).
fileTypeCode(_,unknown).

ownerPermissionCoding(0,[]).
ownerPermissionCoding(4,[read]).
ownerPermissionCoding(6,[read,write]).

file_status(FileName,Status) :-
	'$getFileStatus'(FileName,
				fileStatus(FileTypeCode,ModTime,OwnerPermiss,ByteSize,NBlocks)),
	fileTypeCode(FileTypeCode,FileType),
	ownerPermissionCoding(OwnerPermiss,Permissions),
	Status = [type=FileType,permissions=Permissions,mod_time=ModTime,size=ByteSize].


%
%	get_cwd/1
%
%	Returns the current working directory being used by the program
%	as a quoted atom.  
%

get_cwd(Path) :-
	getcwd(Path).


%
%	get_current_drive/1
%

get_current_drive(Drive) :-
	getcwd(Path),
	rootPathFile(Drive,_,_,Path).


%
%	change_cwd/1
%

change_cwd(Path) :-
	chdir(Path).


%
%	change_current_drive/1.
%
%		We check to make sure that the final character in the drive name
%		is a colon, otherwise it is not a valid drive descriptor.
%

change_current_drive(DriveName) :-
	name(DriveName,DriveList),
	reverse(DriveList,[0': |_]),
	!,
	change_cwd(DriveName).

change_current_drive(DriveName) :-
	name(DriveName,DriveList),
	append(DriveList,[0':],ProperDriveList),
	name(ProperDriveName,ProperDriveList),
	change_cwd(ProperDriveName).

endmod.
