/*=========================================================================*
 |		fs_cmn.pro
 |	Copyright (c) 1988-95 Applied Logic Systems, Inc.
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

export date/1.
export time/1.
export datetime/2.
export gm_datetime/2.
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
export filename_equal/2.

export getDirEntries/3.
export must_exist_file/1.

/*!--------------------------------------------------------------
 |	date/1
 |	date(Date)
 |	date(-)
 |
 |	 - gets the current local date
 |
 |	Unifies the input with the current date which is represented
 |	by a term of the form ??/??/??.  The exact pattern (e.g.,
 |	YY/MM/DD or MM/DD/YY or ....) is determined by date_pattern/4.
 *!--------------------------------------------------------------*/
date(Date)
	:-
	'$time'(_,_,_,DD,Month,YY,_,_,_,0),
	MM is Month + 1,
	date_pattern(YY,MM,DD,Date).

/*!--------------------------------------------------------------
 |	time/1
 |	time(HH:MM:SS)
 |	time(-)
 |
 |	 - gets the current local time
 |
 |	Unifies the input with the current time which is represented
 |	by a term of the form HH:MM:SS.
 *!--------------------------------------------------------------*/
time(HH:MM:SS)
	:-
	'$time'(SS,MM,HH,_,_,_,_,_,_,0).

/*!--------------------------------------------------------------
 |	datetime/2
 |	datetime(Date, Time)
 |	datetime(-, -)
 |
 |	 - gets the current local date and time from the same 
 |	   call to the OS clock
 |
 |	Unifies the input with the current date which is represented
 |	by a term of the form ??/??/??.  The exact pattern (e.g.,
 |	YY/MM/DD or MM/DD/YY or ....) is determined by date_pattern/4.
 *!--------------------------------------------------------------*/
datetime(Date, HH:MM:SS)
	:-
	'$time'(SS,MM,HH,DD,Month,YY,_,_,_,0),
	Mos is Month + 1,
	date_pattern(YY,Mos,DD,Date).

/*!--------------------------------------------------------------
 |	gm_datetime/2
 |	gm_datetime(Date, Time)
 |	gm_datetime(-, -)
 |
 |	 - gets the current Greenwich UTC date and time from the same 
 |	   call to the OS clock
 |
 |	Formats as for datetime/2
 *!--------------------------------------------------------------*/
gm_datetime(Date, HH:MM:SS)
	:-
	'$time'(SS,MM,HH,DD,Month,YY,_,_,_,1),
	Mos is Month + 1,
	date_pattern(YY,Mos,DD,Date).

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
 |	set_date_pattern/1
 |	set_date_pattern(Pattern)
 |	set_date_pattern(+)
 |
 |	- sets the date pattern (= AA/BB/CC)
 |
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
 |	valid_date/1
 |	valid_date(Date)
 |	valid_date(+)
 |
 |	- determines whether a date (pattern) is a valid date
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
 |	datetime_less(DateTime0, DateTime1)
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

/* Post 3.1.1 - replace canon_path with C-level OS builtin for correctness */
canon_path(SrcPath,CanonPath)
	:-
	get_cwd(WeAreHere),
	unwind_protect((
		path_directory_tail(SrcPath, Directory, Tail),
		change_cwd(Directory),
		get_cwd(CanonDir),
		path_elements(CanonPath, [CanonDir, Tail])
	), change_cwd(WeAreHere)).


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
 |	If NewDir is a (quoted) atom representing an existing
 |	path in the filesystem, this predicates changes the 
 |	current working directory being used by the program
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
 |	Obtains the current working directory being used by the program
 |	as a quoted atom, and unifies it with Path. 
 | 	Under DOS or Windows, the drive is included.
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
 |	If FileName is a (quoted) atom naming a file in the 
 |	current working directory, removes that file.
 *!--------------------------------------------------------------*/

remove_file(FileName)
	:-
	unlink(FileName), !.
remove_file(FileName)
	:-
	system_error([unlink(FileName)]).

/*!--------------------------------------------------------------
 |	filename_equal/2
 |	filename_equal(Name1, Name2)
 |	filename_equal(+, +)
 | 
 |	- OS portable check for equality of file names 
 *!--------------------------------------------------------------*/
filename_equal(Name1, Name2)
	:-
	sys_env(OS,_,_),
	(OS = unix ->
		Name1 = Name2
		;
		atom_codes(Name1, N1Cs),
		atom_codes(Name2, N2Cs),
		lc_equal_codes(N1Cs, N2Cs)
	).

lc_equal_codes([], []).
lc_equal_codes([C1 | N1Cs], [C2 | N2Cs])
	:-
	mod_lc_eq_chrs(C1, C2),
	lc_equal_codes(N1Cs, N2Cs).

mod_lc_eq_chrs(C, C) :-!.
mod_lc_eq_chrs(C1, C2)
	:-
	0'A =< C1, C1 =< 0'Z, 
	C2 is C1 + 32, 
	!.
mod_lc_eq_chrs(C1, C2)
	:-
	0'a =< C1, C1 =< 0'z, 
	C2 is C1 - 32, 
	!.

/*!--------------------------------------------------------------
 | 	getDirEntries/3
 | 	getDirEntries(Path, FilePattern, FilesList)
 | 	getDirEntries(+, +, -)
 |
 |	- returns a list of files in a directory matching a pattern
 |
 |	If Path is a (quoted) atom representing a path to a
 |	folder (directory) in the file system, and if FilePattern
 |	is a pattern (possibly using *), then FilesList is the
 |	list of all files in folder Path (possibly including subfolders)
 |	which match FilePattern.
 *!--------------------------------------------------------------*/
getDirEntries(Path, FilePattern, FirstResult)
	:-
	'$getDirEntries'(Path, FilePattern, FirstResult), !.
getDirEntries(Path, FilePattern, FirstResult)
	:-
	system_error([getDirEntries(Path, FilePattern, FirstResult)]).

/*!--------------------------------------------------------------
 |	must_exist_file/1
 |	must_exist_file(FileName)
 |	must_exist_file(+)
 |
 |	- raises a system_error if exists_file fails
 |
 |	If FileName is a (quoted) atom representing a possible entry
 |	in the file system, calls exists_file/1 to determine if 
 |	FileName exists.  If File does not exist, raises a system 
 |	error (while exists_file/1 simply fails).
 *!--------------------------------------------------------------*/
must_exist_file(File)
	:- exists_file(File), !.
must_exist_file(File)
	:-
	system_error([must_exist_file(File)]).

endmod.


