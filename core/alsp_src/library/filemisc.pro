/*========================================================================*
 |			miscfile.pro
 |	Copyright (c) 1991-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		Miscellaneous file handling predicates
 *========================================================================*/

module builtins.

export copy_dir_files_nl/3.
export copy_dir_files_nl/4.
export copy_fileslist_nl/4.
export copy_fileslist_nl/5.
export check_copy_file_nl/4.
export copy_file_nl/4.
export copy_stream_nl/3.

export install_file_links/2.
export install_links0/2.
export remove_files/1.
export comp_times/3.
export archive_file/2.
export move_file/2.

/*!-------------------------------------------------------------
 |	copy_dir_files_nl/3
 |	copy_dir_files_nl(SourceDir, TgtDir, NL_type)
 |	copy_dir_files_nl(+, +, +)
 |
 |	- copies files from dir to dir, using a given nl type 
 |
 *!------------------------------------------------------------*/
copy_dir_files_nl(SourceDir, TgtDir, NL_type)
	:-
	copy_dir_files_nl(SourceDir, TgtDir, NL_type, new).

/*!-------------------------------------------------------------
 |	copy_dir_files_nl/4
 |	copy_dir_files_nl(SourceDir, TgtDir, NL_type, NewUp)
 |	copy_dir_files_nl(+, +, +, +)
 |
 |	- copies (some) files from dir to dir, using a given nl type 
 |
 *!------------------------------------------------------------*/
copy_dir_files_nl(SourceDir, TgtDir, NL_type, NewUp)
	:-
	pathPlusFile(SourceDir, '*', SourcePattern),
	files(SourcePattern, FileList),
	copy_fileslist_nl(FileList, SourceDir, TgtDir, NL_type, NewUp).

/*!-------------------------------------------------------------
 |	copy_fileslist_nl/4
 |	copy_fileslist_nl(FileList, SourceDir, TgtDir, NL_type)
 |	copy_fileslist_nl(+, +, +, +)
 |
 |	- copies a list of files from one dir to another
 |
 *!------------------------------------------------------------*/
copy_fileslist_nl(FileList, SourceDir, TgtDir, NL_type)
	:-
	copy_fileslist_nl(FileList, SourceDir, TgtDir, NL_type, new).

/*!-------------------------------------------------------------
 |	copy_fileslist_nl/5
 |	copy_fileslist_nl(FileList, SourceDir, TgtDir, NL_type, NewUp)
 |	copy_fileslist_nl(+, +, +, +, +)
 |
 |	- copies (some of) a list of files from one dir to another
 |
 *!------------------------------------------------------------*/
copy_fileslist_nl([], SrcPath, DestDir, NL_type, _).

copy_fileslist_nl([SrcFileName | SrcFileNamesList], SrcPath, DestDir, 
					NL_type, NewUp)
	:-
	pathPlusFile(SrcPath, SrcFileName, SrcFile),
	pathPlusFile(DestDir, SrcFileName, DestFile),
	check_copy_file_nl(NewUp, SrcFile, DestFile, NL_type),
	copy_fileslist_nl(SrcFileNamesList, SrcPath, DestDir, NL_type, NewUp).

/*!-------------------------------------------------------------
 |	check_copy_file_nl/4
 |	check_copy_file_nl(NewUp, SrcFile, DestFile, NL_type)
 |	check_copy_file_nl(+, +, +, +)
 |
 |	- test and (maybe) copy a file
 |
 *!------------------------------------------------------------*/
check_copy_file_nl(new, SrcFile, DestFile, NL_type)
	:-!,
	copy_file_nl(SrcFile, DestFile, NL_type).

check_copy_file_nl(_, SrcFile, DestFile, NL_type)
	:-
	(comp_file_times(SrcFile, DestFile) ->
		true
		;
		copy_file_nl(SrcFile, DestFile, NL_type)
	).

/*!-------------------------------------------------------------
 |	copy_file_nl/3
 |	copy_file_nl(SrcFileName, DestFileName, NL_type)
 |	copy_file_nl(+, +, +)
 |
 |	- copies one file to another, using a given nl type
 |
 *!------------------------------------------------------------*/
copy_file_nl(SrcFileName, DestFileName, NL_type)
	:-
	open(SrcFileName, read, SrcS, []),
	open(DestFileName, write, TgtS, []),
	copy_stream_nl(SrcS, TgtS, NL_type),
	close(TgtS),
	close(SrcS).

/*!-------------------------------------------------------------
 |	copy_stream_nl/3
 |	copy_stream_nl(SrcS, TgtS, NL_type)
 |	copy_stream_nl(+, +, +)
 |
 |	- copies one stream to another, using a given nl type
 |
 *!------------------------------------------------------------*/
copy_stream_nl(SrcS, TgtS, NL_type)
	:-
	get_line(SrcS, Line),
	!,
	put_atom(TgtS, Line),
	output_nl(NL_type, TgtS),
	copy_stream_nl(SrcS, TgtS, NL_type).

copy_stream_nl(SrcS, TgtS, NL_type).

/****************
/*!-------------------------------------------------------------
 |	output_nl/2
 |	output_nl(NL_type, TgtS)
 |	output_nl(+, +)
 |
 |	- output a specified nl type to a stream
 |
 *!------------------------------------------------------------*/
output_nl(unix, TgtS)
	:-
%	nl(TgtS).
	put_code(TgtS, 10).

output_nl(mswin32, TgtS)
	:-
	put_code(TgtS, 13),
	put_code(TgtS, 10).

output_nl(dos, TgtS)
	:-
	put_code(TgtS, 13),
	put_code(TgtS, 10).

output_nl(macos, TgtS)
	:-
	put_code(TgtS, 13).
******************/

/*!-------------------------------------------------------------
 |	comp_times/3
 |	comp_times(SFL_Pairs, UpToDate, OutOfDate)
 |	comp_times(+, -, -)
 |
 |	- sort a list of file pairs by file date comparisions
 |
 *!------------------------------------------------------------*/
comp_times([], [], []).
comp_times([Source-Target | SFL_Pairs], UpToDate, OutOfDate)
	:-
	not(exists_file(Source)),
	!,
	comp_times(SFL_Pairs, UpToDate, OutOfDate).
comp_times([Source-Target | SFL_Pairs], UpToDate, [Source | OutOfDate])
	:-
	not(exists_file(Target)),
	!,
	comp_times(SFL_Pairs, UpToDate, OutOfDate).
comp_times([Source-Target | SFL_Pairs], UpToDate, [Source| OutOfDate])
	:-
		%% Source is Older than Target:
	comp_file_times(Target,Source),
	!,
	comp_times(SFL_Pairs, UpToDate, OutOfDate).
comp_times([Source-Target | SFL_Pairs], [Source | UpToDate], OutOfDate)
	:-
	comp_times(SFL_Pairs, UpToDate, OutOfDate).

/*!-------------------------------------------------------------
 |	install_file_links/2
 |	install_file_links(LinkDir, SrcDir)
 |	install_file_links(+, +)
 |
 |	- install links in a dir to all files in another 
 |
 *!------------------------------------------------------------*/
install_file_links(LinkDir, SrcDir)
	:-
	get_cwd(CurDir),
	change_cwd(LinkDir),
	pathPlusFile(SourceDir, '*', SourcePattern),
	files(SourcePattern, FileList),
	install_links0(FileList, SrcDir),
	change_cwd(CurDir).

/*!-------------------------------------------------------------
 |	install_links0/2
 |	install_links0(FileList,SrcDir)
 |	install_links0(+,+)
 |
 |	- install links in a dir to all files on a list
 |
 *!------------------------------------------------------------*/
install_links0(FileList,SrcDir).
install_links0([File | FileList],SrcDir)
	:-
	pathPlusFile(SrcDir,File,SrcFile),
	pathPlusFile('./',File, LinkFile),
	make_symlink(SrcFile,LinkFile),
	install_links0(FileList,SrcDir).

/*!-------------------------------------------------------------
 |	remove_files/1
 |	remove_files(FileList)
 |	remove_files(+)
 |
 |	- remove all files matching a list of patterns
 |
 *!------------------------------------------------------------*/
remove_files([]) :-!.
remove_files(Pattern)
	:-
	atom(Pattern),
	!,
	files(Pattern, FileList),
	remove_files(FileList).
remove_files([File | FileList])
	:-
	remove_file(File),
	remove_files(FileList).

/*!-------------------------------------------------------------
 |	archive_file/2
 |	archive_file(Original, Dir)
 |	archive_file(+, +)
 |
 | 	- archive a file in a directory (uses 'mv')
 |
 |	archive_file/3
 |	archive_file(Original, Dir,How)
 |	archive_file(+, +, +)
 |
 |	- archive a file, using move or copy
 |
 |	How = copy/move
 *!------------------------------------------------------------*/
archive_file(Original,Dir)
	:-
	archive_file(Original,Dir, move).

archive_file(Original,Dir,How)
	:-
	date(YY/MM/DD),
	sprintf(atom(FN), '%t/%t.%t-%t-%t',[Dir,Original,YY,MM,DD]),
	(exists_file(FN) ->
		time((HH:MMM:SS)),
		sprintf(atom(FFN), '%t.%t-%t-%t',[FN,HH,MMM,SS])
		;
		FFN = FN
	),
	(How = move ->
		move_file(Original, FFN)
		;
		builtins:als_system(SYSL),
		dmember(os=OS,SYSL),
		copy_file_nl(Original, FFN, OS)
	).

/*!-------------------------------------------------------------
 |	move_file/2
 |	move_file(Original, FFN)
 |	move_file(+, +)
 |
 |	- moves one file onto another
 |
 *!------------------------------------------------------------*/
move_file(Original, FFN)
	:-
	sprintf(Cmd, 'mv %t %t',[Original,FFN]),
	system(Cmd).

endmod.
