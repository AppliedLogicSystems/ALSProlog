/*========================================================================*
 |			miscfile.pro
 |		Copyright (c) 1991-4 Applied Logic Systems, Inc.
 |
 |		Miscellaneous file handling predicates
 *========================================================================*/

module builtins.

copy_dir_files_nl(SourceDir, TgtDir, NL_type)
	:-
	pathPlusFile(SourceDir, '/*', SourcePattern),
	files(SourcePathern, FileList),
	copy_fileslist_nl(FileList, SourceDir, TgtDir, NL_type).


copy_fileslist_nl([], SrcPath, DestDir, NL_type).

copy_fileslist_nl([SrcFileName | SrcFileNamesList], SrcPath, DestDir, NL_type)
	:-
	pathPlusFile(SrcPath, SrcFileName, SrcFile),
	pathPlusFile(DestDir, SrcFileName, DestFile),
	copy_file_nl(SrcFile, DestFile, NL_type),
	copy_fileslist_nl(SrcFileNamesList, SrcPath, DestDir, NL_type).

copy_file_nl(SrcFileName, DestFileName, NL_type)
	:-
	open(SrcFileName, read, SrcS, []),
	open(DestFileName, write, TgtS, []),
	copy_stream_nl(SrcS, TgtS, NL_type),
	close(TgtS),
	close(SrcS).

copy_stream_nl(SrcS, TgtS, NL_type)
	:-
	get_line(SrcS, Line),
	!,
	put_atom(TgtS, Line),
	output_nl(NL_type, TgtS),
	copy_stream_nl(SrcS, TgtS, NL_type).

copy_stream_nl(SrcS, TgtS, NL_type).

output_nl(unix, TgtS)
	:-
	nl(TgtS).

output_nl(dos, TgtS)
	:-
	put_code(TgtS, 13),
	put_code(TgtS, 10).

output_nl(mac, TgtS)
	:-
	put_code(TgtS, 13).

write_lines_nl(Lines, TgtS, NL_type)
	:-
	write_lines_nl(Lines, TgtS, NL_type, []).

write_lines_nl([], _, _, _).

write_lines_nl([Line | Lines], TgtS, NL_type, Options)
	:-
	write_term(TgtS, Line, Options),
	output_nl(NL_type, TgtS),
	write_lines_nl(Lines, TgtS, NL_type, Options).

export install_file_links/2.
install_file_links(LinkDir, SrcDir)
	:-
	get_cwd(CurDir),
	change_cwd(LinkDir),
	pathPlusFile(SourceDir, '/*', SourcePattern),
	files(SourcePathern, FileList),
	install_links0(FileList, SrcDir),
	change_cwd(CurDir).


install_links0(FileList,SrcDir).
install_links0([File | FileList],SrcDir)
	:-
	pathPlusFile(SrcDir,File,SrcFile),
	pathPlusFile('./',File, LinkFile),
	make_symlink(SrcFile,LinkFile),
	install_links0(FileList,SrcDir).

endmod.



/*************
export copyFiles/2.
/*!---------------------------------------------------------------------
  | copyFiles(SourceFilesList, TargetSubDirPath)
 *!--------------------------------------------------------------------*/
copyFiles(SourceFilesList, TargetSubDirPath)
	:-
	name(TargetSubDirPath, TargetSubDirPathChars),
	copyFiles0(SourceFilesList, TargetSubDirPathChars).

copyFiles0([], _).
copyFiles0([File | SourceFilesList], TargetSubDirPathChars)
	:-
	name(File, FileChars),
	append(TargetSubDirPathChars," > bucket.bit",Tmp1),
	append(FileChars, [0' | Tmp1], Tmp2),
	append("copy ",Tmp2,CmdChars),
	system(CmdChars),
	!,
	copyFiles0(SourceFilesList, TargetSubDirPathChars).
*************/

