module sys_maint.


export libview/1.
libview(File)
	:-
	lib_location(Disk, ALSDIR_Path, LibPath),
	append(LibPath,[man], LibManDirPath),
    rootPathFile(Disk, LibManDirPath, [File,man], LibManFile),
	getLibInfo(LibManFile, HeaderInfo, Descrip),
	displayLibInfo(HeaderInfo, Descrip).

getLibInfo(LibManFile, HeaderInfo, Descrip)
	:-
	see(LibManFile),
	readLibInfo(HeaderInfo,Descrip),
	seen.

readLibInfo([NamesList,FormsList],DescripList)
	:-
	next_lib_break,
	read_line_list(NamesList),
	read_line_list(FormsList),
	read_line_list(DescripList).

next_lib_break
	:-
	readline(Line),
	disp_next_lib_break(Line).

disp_next_lib_break([0'@ | _]):-!.
disp_next_lib_break(_)
	:-
	next_lib_break.

read_line_list(LinesList)
	:-
	readline(NextLine),
	disp_read_line_list(NextLine, LinesList).

disp_read_line_list(end_of_file, []) :-!.
disp_read_line_list([0'@ | _], LinesList)
	:-!,
	LinesList = [].
disp_read_line_list([], LinesList)
	:-
	read_line_list(LinesList).
disp_read_line_list(NextLine, [NextLine | RestLinesList])
	:-
	read_line_list(RestLinesList).

displayLibInfo([Names,Forms], Descrip)
	:-
	builtins:put_string("NAMES"),nl,
	write_string_list(Names),

	builtins:put_string("FORMS"),nl,
	write_string_list(Forms),

	builtins:put_string("DESCRIPTION"),nl,
	write_string_list(Descrip).

endmod.
