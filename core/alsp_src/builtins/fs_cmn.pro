/*=========================================================================*
 |		fs_cmn.pro
 |	Copyright (c) 1988-95 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |	Miscellaneous common low-level file system functions
 |	Consolidation of parts of the separate files:
 |			-- Unix Version		(fsunix.pro)
 |			-- DOS 386 Version	(fsdos386.pro) <-- Now djgpp
 |			-- Mac Version 		(fsmac.pro)
 |			-- VMS Version 		(fsvms.pro)
 |
 |	Authors: Keith Hughes, Ilyas Cicekli, Ken Bowen
 |	Date:	Begun 4/88
 |	Revision: Ken Bowen -- 11/88, 1/91 
 |		-- library version: 11/91; various mods:1992-95
 *===========================================================================*/
module builtins.

export date_less/2.
export date_pattern/4.
export set_date_pattern/1.
export valid_date/1.
export valid_date/3.
export time_less/2.
export datetime_less/2.
export canon_path/2.
export get_cwd/1.
export change_cwd/1.
export remove_file/1.

/*!--------------------------------------------------------------
 |	date_less/2
 |	date_less(Date0, Date1)
 |	date_less(+,+)
 |
 |	 - tests two terms representing dates for ordering
 |
 |	If Date0 and Date1 are date terms of the form YY/MM/DD, succeeds
 |	if and only if Date0 represents a date earlier than Date1.
 *!--------------------------------------------------------------*/
date_less(YY0/MM0/DD0, YY1/MM1/DD1)
	:-
	(YY0 < YY1,!;
		(YY0 = YY1, 
			(MM0 < MM1,!; (MM0 = MM1, DD0 < DD1)))).

/*!--------------------------------------------------------------
 *!--------------------------------------------------------------*/
set_date_pattern(AA/BB/CC)
	:-
	name(AA, [AChar | _]),
	dmember(AChar, "ymd"),
	name(BB, [BChar | _]),
	dmember(BChar, "ymd"),
	name(CC, [CChar | _]),
	dmember(CChar, "ymd"),
	ACharU is AChar - 32,
	BCharU is BChar - 32,
	CCharU is CChar - 32,
	sort([ACharU,BCharU,CCharU],YMD_Var_Chars0),
	dreverse(YMD_Var_Chars0, YMD_Var_Chars1),
	insert_comma_chars(YMD_Var_Chars1, YMD_Var_Chars2),
	append([0'[ | YMD_Var_Chars2], [ACharU,0'/,BCharU,0'/,CCharU,0']], PatternChars),
	bufread(PatternChars, PatternArgsList),
	DatePattern =.. [date_pattern | PatternArgsList],
	abolish(date_pattern,4),
	assert(DatePattern).
 
insert_comma_chars([], []).
insert_comma_chars([C | RestCs], [C, 0', | RestICs])
	:-
	insert_comma_chars(RestCs, RestICs).

	%% Default:
date_pattern(YY,MM,DD,YY/MM/DD).

/*!--------------------------------------------------------------
 *!--------------------------------------------------------------*/
valid_date(YY-MM-DD)
	:-
	valid_date(YY/MM/DD).
valid_date(Date)
	:-
	date_pattern(YY,MM,DD,Date),
	valid_date(YY,MM,DD).

valid_date(YY,MM,DD)
	:-
	integer(YY), YY >= 0,
	integer(MM), 1 =< MM, MM =< 12,
	integer(DD), 1 =< DD, DD =< 31,
	(DD =< 28 ->
	    true
	    ;
	    end_of_month(DD,MM,YY)
	).

end_of_month(29,2,YY)
	:-
	0 is YY mod 4.

end_of_month(29,2,YY)
	:-!,
	fail.
end_of_month(29,MM,YY).

end_of_month(30,MM,YY)
	:-
	MM \= 2.

end_of_month(31,MM,YY)
	:-
	dmember(MM, [1,3,5,7,8,10,12]).

/*!--------------------------------------------------------------
 |	time_less/2
 |	time_less(Time0, Time1)
 |	time_less(+,+)
 |
 |	 - tests two terms representing time for ordering
 |
 |	If Time0 and Time1 are time terms of the form HH:MM:SS, succeeds
 |	if and only if Time0 represents a time earlier than Time1.
 *!--------------------------------------------------------------*/
time_less(HH0:MM0:SS0, HH1:MM1:SS1)
	:-
	(HH0 < HH1,!;
		(HH0 = HH1, 
			(MM0 < MM1,!; (MM0 = MM1, SS0 < SS1)))).

/*!--------------------------------------------------------------
 |	datetime_less/2
 |	datetime_less(Time0, Time1)
 |	datetime_less(+,+)
 |
 |	 - tests two terms representing datetime for ordering
 |
 |	If DateTime0 and DateTime1 are time terms of the form 
 |		(Date0,Time0), (Date1,Time1),
 |	succeeds iff DateTime0 preceeds or equals DateTime1.
 *!--------------------------------------------------------------*/
datetime_less((Date0,Time0), (Date1,Time1))
	:-
	date_less(Date0, Date1),!;
		Date0 = Date1, time_less(Time0, Time1).

/*!--------------------------------------------------------------
 |	canon_path/2
 |	canon_path(SrcPath,CanonPath)
 |	canon_path(+,-)
 |
 |	 -	canonicalizes a path name
 |
 |  If SrcPath is a path name, either to a file or to a directory,
 |	CanonPath is a canonicalized version of that path name, in the
 |	sense that all symbolic links  in the path (to either subdirs
 |	or the file at the end) are dereferenced out;
 *!--------------------------------------------------------------*/
/*
canon_path(SrcPath,CanonPath)
	:-
	get_cwd(WeAreHere),

	rootPathFile(Disk,SubDirList,EndPathFile,SrcPath),
	(change_cwd(SrcPath) ->
		get_cwd(CanonPath)
		;
		rootPathFile(Disk,SubDirList,'',ShortSrcPath),
		change_cwd(ShortSrcPath),
		get_cwd(ShortCanonPath),
		subPath(ShortSubDirList,ShortCanonPath),
		rootPathFile(_,ShortSubDirList,EndPathFile,CanonPath)
	),
	change_cwd(WeAreHere).
*/

/* canon_path needs a version of change_cwd that will return
   false if the directory doesn't exist.  This should use
   something like exists_dir/1, but for now use the following
   as a hack. */
test_and_change_cwd(NewDir)
	:-
	chdir(NewDir).

canon_path(SrcPath,CanonPath)
	:-
	get_cwd(WeAreHere),

	rootPathFile(Disk,SubDirList,EndPathFile,SrcPath),
	(test_and_change_cwd(SrcPath) ->
		get_cwd(CanonPath)
		;
		rootPathFile(Disk,SubDirList,'',ShortSrcPath),
		(ShortSrcPath \= '' ->
			change_cwd(ShortSrcPath)
			;
			true
		),
		get_cwd(ShortCanonPath),
		rootPathFile(Drive,ShortSubDirList,FF,ShortCanonPath),
		(FF = '' ->
			AddOn = []
			;
			AddOn = [FF]
		),
		dappend(ShortSubDirList, AddOn, TheSubdirList),
		rootPathFile(Drive,TheSubdirList,EndPathFile,CanonPath)
	),
	change_cwd(WeAreHere).

strip_last([_],[]) :-!.
strip_last([H | PathList], [H | ParPathList])
	:-
	strip_last(PathList, ParPathList).

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
	chdir(Path), !.
change_cwd(Path)
	:-
	system_error([change_cwd(Path)]).

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
	getcwd(Path), !.
get_cwd(Path)
	:-
	system_error([get_cwd(Path)]).

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
	unlink(FileName), !.
remove_file(FileName)
	:-
	system_error([unlink(FileName)]).


endmod.
