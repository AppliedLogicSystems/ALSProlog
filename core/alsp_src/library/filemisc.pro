/*========================================================================*
 |			miscfile.pro
 |	Copyright (c) 1991-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		Miscellaneous file handling predicates
 *========================================================================*/

module builtins.

export get_lines/2.
export grab_lines/2.
export copy_dir_files_nl/3.
export copy_dir_files_nl/4.
export copy_fileslist_nl/4.
export copy_fileslist_nl/5.
export check_copy_file_nl/4.
export copy_file_nl/4.
export copy_stream_nl/3.
export write_lines/1.
export write_lines/2.
export write_lines_opt/2.
export write_lines_nl/3.
export write_lines_nl/4.
export write_clause/1.
export write_clause/2.
export write_clause/3.
export write_clauses/1.
export write_clauses/2.
export write_clauses/3.
export install_file_links/2.
export remove_files/1.


get_lines(SrcF,Lines)
	:-
	open(SrcF,read,SS,[]),
	grab_lines(SS,Lines),
	close(SS).
				 
grab_lines(SS,[Line | Lines])
	:-
	get_line(SS,Line),
	!,
	grab_lines(SS, Lines).

grab_lines(SS, []).


copy_dir_files_nl(SourceDir, TgtDir, NL_type)
	:-
	copy_dir_files_nl(SourceDir, TgtDir, NL_type, new).

copy_dir_files_nl(SourceDir, TgtDir, NL_type, NewUp)
	:-
	pathPlusFile(SourceDir, '*', SourcePattern),
	files(SourcePattern, FileList),
	copy_fileslist_nl(FileList, SourceDir, TgtDir, NL_type, NewUp).

copy_fileslist_nl(FileList, SourceDir, TgtDir, NL_type)
	:-
	copy_fileslist_nl(FileList, SourceDir, TgtDir, NL_type, new).

copy_fileslist_nl([], SrcPath, DestDir, NL_type, _).

copy_fileslist_nl([SrcFileName | SrcFileNamesList], SrcPath, DestDir, NL_type, NewUp)
	:-
	pathPlusFile(SrcPath, SrcFileName, SrcFile),
	pathPlusFile(DestDir, SrcFileName, DestFile),
	check_copy_file_nl(NewUp, SrcFile, DestFile, NL_type),
	copy_fileslist_nl(SrcFileNamesList, SrcPath, DestDir, NL_type, NewUp).

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

write_lines(List)
	:-
	sio:get_current_output_stream(TgtStream),
	write_lines(TgtStream, List).

write_lines(TgtStream, List)
	:-
	builtins:sys_env(OS,_,_),
	write_lines_nl(List, TgtStream, OS,[]).

write_lines_opt(List, Opts)
	:-
	sio:get_current_output_stream(TgtStream),
	builtins:sys_env(OS,_,_),
	write_lines_nl(List, TgtStream, OS, Opts).

write_lines_opt(TgtStream, List, Opts)
	:-
	builtins:sys_env(OS,_,_),
	write_lines_nl(List, TgtStream, OS, Opts).

write_lines_nl(Lines, TgtS, NL_type)
	:-
	write_lines_nl(Lines, TgtS, NL_type, []).

write_lines_nl([], _, _, _).

write_lines_nl([Line | Lines], TgtS, NL_type, Options)
	:-
	write_term(TgtS, Line, Options),
	output_nl(NL_type, TgtS),
	write_lines_nl(Lines, TgtS, NL_type, Options).


write_clause(Clause) :-
	sio:get_current_output_stream(Stream),
	write_clause(Stream, Clause).

write_clause(Stream, Clause) :-
	write_clause(Stream, Clause, []).

write_clause(Stream, Clause, Options) :-
	write_term(Stream,Clause, Options),
	put_code(Stream, 0'.),
	nl(Stream).

write_clauses(Clauses) :-
	sio:get_current_output_stream(Stream),
	write_clauses(Stream,Clauses).

write_clauses(Stream, Clauses) :-
	write_clauses(Stream,Clauses,[]).

write_clauses(Stream, Clauses, Options) :-
	write_clauses0(Clauses, Stream, Options).

write_clauses0([], Stream, Options).
write_clauses0([nl | Clauses], Stream, Options) :-
	!,
    	nl(Stream),
    	write_clauses0(Clauses, Stream, Options).
write_clauses0([Clause | Clauses], Stream, Options) :-
	write_clause(Stream, Clause, Options),
	write_clauses0(Clauses, Stream, Options).

install_file_links(LinkDir, SrcDir)
	:-
	get_cwd(CurDir),
	change_cwd(LinkDir),
	pathPlusFile(SourceDir, '*', SourcePattern),
	files(SourcePattern, FileList),
	install_links0(FileList, SrcDir),
	change_cwd(CurDir).


install_links0(FileList,SrcDir).
install_links0([File | FileList],SrcDir)
	:-
	pathPlusFile(SrcDir,File,SrcFile),
	pathPlusFile('./',File, LinkFile),
	make_symlink(SrcFile,LinkFile),
	install_links0(FileList,SrcDir).

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

split_ll_at_start([], _, [], [], '').

split_ll_at_start([Line | Tail], Init, [], Tail, Line)
	:-
	sub_atom(Line, 1, _, Init),
	!.

split_ll_at_start([Line | Lines], Init, [Line | Head], Tail, Splitter)
	:-
	split_ll_at_start(Lines, Init, Head, Tail, Splitter).


split_ll_by_blank([], [], []).

split_ll_by_blank(['' | Tail], [], Tail)
	:-!.

split_ll_by_blank([Line | Lines], [Line | Head], Tail)
	:-
	split_ll_by_blank(Lines, Head, Tail).

archive_file(Choice,Dir)
	:-
	date(YY/MM/DD),
	sprintf(atom(FN), '%t/%t.%t-%t-%t',[Dir,Choice,YY,MM,DD]),
	(exists_file(FN) ->
		time((HH:MMM:SS)),
		sprintf(atom(FFN), '%t.%t-%t-%t',[FN,HH,MMM,SS])
		;
		FFN = FN
	),
	sprintf(Cmd, 'mv %t %t',[Choice,FFN]),
	system(Cmd).

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


endmod.
