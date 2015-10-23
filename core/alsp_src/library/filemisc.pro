/*========================================================================*
 |			miscfile.pro
 |	Copyright (c) 1991-96 Applied Logic Systems, Inc.
 |
 |		Miscellaneous file handling predicates
 *========================================================================*/

module builtins.

export copy_dir_files/2.
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
 |	copy_dir_files/2
 |	copy_dir_files(SourceDir, TgtDir)
 |	copy_dir_files(+, +)
 |
 |	- copies files from dir to dir
 |
 *!------------------------------------------------------------*/
copy_dir_files(SourceDir, TgtDir)
	:-
	copy_dir_files(SourceDir, TgtDir, new).

/*!-------------------------------------------------------------
 |	copy_dir_files/3
 |	copy_dir_files(SourceDir, TgtDir, NewUp)
 |	copy_dir_files(+, +, +)
 |
 |	- copies (some) files from dir to dir
 |
 *!------------------------------------------------------------*/
copy_dir_files(SourceDir, TgtDir, NewUp)
	:-
%	pathPlusFile(SourceDir, '*', SourcePattern),
	split_path(SourceDir, SDElts),
	dappend(SDElts, ['*'], SPElts),
	join_path(SPElts, SourcePattern),
	files(SourcePattern, FileList),
%	copy_fileslist_nl(FileList, SourceDir, TgtDir, NL_type, NewUp).
	split_path(TgtDir, TgtDirElts),
	copy_fileslist(FileList, SDElts, TgtDirElts, NL_type, NewUp).

/*!-------------------------------------------------------------
 |	copy_fileslist/3
 |	copy_fileslist(FileList, SrcDirElts, TgtDirElts)
 |	copy_fileslist(+, +, +)
 |
 |	- copies a list of files from one dir to another
 |
 *!------------------------------------------------------------*/
copy_fileslist(FileList, SrcDirElts, TgtDirElts)
	:-
	copy_fileslist(FileList, SrcDirElts, TgtDirElts, new).

/*!-------------------------------------------------------------
 |	copy_fileslist/4
 |	copy_fileslist(FileList, SrcDirElts, TgtDirElts, NewUp)
 |	copy_fileslist(+, +, +, +)
 |
 |	- copies (some of) a list of files from one dir to another
 |
 *!------------------------------------------------------------*/
copy_fileslist_nl([], SrcDirElts, DestDirElts, _).

copy_fileslist_nl([SrcFileName | SrcFileNamesList], SrcDirElts, DestDirElts, NewUp)
	:-
%	pathPlusFile(SrcPath, SrcFileName, SrcFile),
	dappend(SrcDirElts, [SrcFileName], SFElts),
	join_path(SFNElts, SrcFile),

%	pathPlusFile(DestDir, SrcFileName, DestFile),
	dappend(DestDirElts, [SrcFileName], DestFile),

	check_copy_file_nl(NewUp, SrcFile, DestFile),
	copy_fileslist(SrcFileNamesList, SrcDirElts, DestDirElts, NewUp).

/*!-------------------------------------------------------------
 |	check_copy_file/3
 |	check_copy_file(NewUp, SrcFile, DestFile)
 |	check_copy_file(+, +, +)
 |
 |	- test and (maybe) copy a file
 *!------------------------------------------------------------*/
check_copy_file(new, SrcFile, DestFile)
	:-!,
	copy_file(SrcFile, DestFile).

check_copy_file(_, SrcFile, DestFile)
	:-
	(comp_file_times(SrcFile, DestFile) ->
		true
		;
		copy_file(SrcFile, DestFile)
	).

/*!-------------------------------------------------------------
 |	copy_file/2
 |	copy_file(SrcFileName, DestFileName)
 |	copy_file(+, +)
 |
 |	- copies one file to another
 *!------------------------------------------------------------*/
copy_file(SrcFileName, DestFileName)
	:-
	open(SrcFileName, read, SrcS, []),
	open(DestFileName, write, TgtS, []),
	copy_stream(SrcS, TgtS),
	close(TgtS),
	close(SrcS).

/*!-------------------------------------------------------------
 |	copy_stream/2
 |	copy_stream(SrcS, TgtS)
 |	copy_stream(+, +)
 |
 |	- copies one stream to another
 *!------------------------------------------------------------*/
copy_stream(SrcS, TgtS)
	:-
	get_line(SrcS, Line),
	!,
	put_atom(TgtS, Line),
	nl(TgtS),
	copy_stream(SrcS, TgtS).

copy_stream_nl(SrcS, TgtS).

/**************************
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
**************************/

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
%	pathPlusFile(SourceDir, '*', SourcePattern),
	split_path(SrcDir, SDElts),
	dappend(SDElts, ['*'], SPElts),
	join_path(SPElts, SourcePattern),
	files(SourcePattern, FileList),
	install_links0(FileList, SDElts),
	change_cwd(CurDir).

/*!-------------------------------------------------------------
 |	install_links0/2
 |	install_links0(FileList,SDElts)
 |	install_links0(+,+)
 |
 |	- install links in a dir to all files on a list
 |
 *!------------------------------------------------------------*/
install_links0(FileList,SDElts).
install_links0([File | FileList],SDElts)
	:-
%	pathPlusFile(SrcDir,File,SrcFile),
	dappend(SDElts, [File], SFElts),
	join_path(SFElts, SrcFile),
%	pathPlusFile('./',File, LinkFile),
	make_symlink(SrcFile,File),
	install_links0(FileList,SDElts).

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
