/*==============================================================
 |		blt_io.pro
 | Copyright (c) 1986-1992 Applied Logic Systems, Inc.
 |
 | Builtin predicates for dealing with I/O
 |
 | Authors: Kevin A. Buettner, Ken Bowen, Chris White,
 |	    Keith Hughes, Ilyas Cicekli
 | Original Creation Date: 3/20/86
 | Merge3 Revision: Begun 3/6/92 - Ken Bowen
 |
 *==============================================================*/

module builtins.
use xconsult.

/*!----------------------------------------------------------------
 | exists_file/1
 | exists_file(Path)
 | exists_file(+)
 |
 |	-- Determine whether a file or subdir exists.
 |
 | If Path is an atom describing a path (possibly relative, possibly
 | just a FileName itself),  determines whether the file or subdir Path 
 | exists by calling $accesss/2.
 | [This calls the the Unix Section 2 access system service].  
 | Note that access mode 0 is used, which merely checks for the presence
 | of the file.  The access/2 modes are:
 |           0 -- test for presence of file
 |           1 -- test for execute (search) permission
 |           2 -- test for write permission
 |           3 -- test for read permission
 |
 | Note exists/1 is (temporarily) retained for backwards compatibility.
 *!----------------------------------------------------------------*/
export exists_file/1.
exists_file([Head | Tail])
	:-!,
	subPath([Head | Tail], Path),
	'$access'(Path,0).

exists_file(Path) 
	:-
	'$access'(Path,0).

%% Temporary (backwards compat):
export exists/1.
exists(FileName) :-
	'$access'(FileName,0).

/*----------------------------------------------------------------------*
 | Consult, reconsult, consultq, consult_to, consultq_to
 |
 | consultq - quiet consult: turns off consult messages, this consult;
 | consultd - source debbugging consult: consults the requested file(s),
 |			  adding hidden source debugging info;
 | consult(q)_to - behaves as if the consult were called from within 
 |				   a given module; specifically, clauses in the file
 |				   which appear outside of any explicit module declaration
 |				   are loaded into the specified module, instead of the
 |				   default behavior of being loaded into module 'user'
 *----------------------------------------------------------------------*/

export consultd/1.
export reconsultd/1.
export consult/1.
export consultq/1.
export reconsult/1.
export consult_to/2.
export consultq_to/2.

consultd(File) :-
	nonvar_ok(File),
	change_source_level_debugging(on,Prev),
	consult(File),
	change_source_level_debugging(Prev).
reconsultd(File) :-
	consultd(-File).

consultq(File) :-
	nonvar_ok(File),
	consultmessage(off),
	consult(File),
	consultmessage(on), !.

consult([]) :-!.
consult([File | Files]) :- !,
	consult(File),
	consult(Files).

consult(-source(File)) :- !,
	nonvar_ok(File),
	consult(source(-File)).

consult(source(FNStruct)) :- !,
	nonvar_ok(FNStruct),
	make_file_name(FNStruct, FileName, Type),
	consult(Type, source(FileName)).

consult(FNStruct) :- !,
	nonvar_ok(FNStruct),
	make_file_name(FNStruct, FileName, Type),
	consult(Type, FileName).

reconsult(File) :-
	nonvar_ok(File),
	consult(-File).

consultq_to(Mod,File) :-
	nonvar_ok(File),
	xconsult:pushmod(Mod),
	consultmessage(off),
	consult(File),
	consultmessage(on), !,
	xconsult:popmod.
			       
consult_to(Mod,File) :-
	nonvar_ok(File),
	xconsult:pushmod(Mod),
	consult(File),
	xconsult:popmod.

/*-------------------------------------------------------------*
 | consult/2
 | consult(Mode,FileName)
 | consult(+,+)
 |
 | Mode will either be consult or reconsult.
 |
 | FileName should be an atom representing the name 
 | of the file to consult.
 *-------------------------------------------------------------*/ 

consult(consult,TheFileName) 
	:- 
	consult_nature(TheFileName, FileName, Nature),
	consultmessage('Consulting %s ...\n',[FileName]),
	load(FileName, 0, Path, Nature, LoadedPath),
	!,
	source_debug_record(Path),
	consultmessage('%s consulted\n',[LoadedPath]).

consult(reconsult,TheFileName) 
	:-
	consult_nature(TheFileName, FileName, Nature),
	consultmessage('Reconsulting %s ...\n',[FileName]),
	load(FileName, 1, Path, Nature, LoadedPath),
	!,
	adjust_source_deb(Path),
	consultmessage('%s reconsulted\n',[LoadedPath]).

consult(_,FileName) 
	:-
	prolog_system_error(no_file,[FileName]).

:- dynamic(consulted/3).

consult_nature(source(FileName), FileName, source) 
	:- !.
consult_nature(FileName, FileName, obp).

source_debug_record(Path)
	:-
	source_level_debugging(on),
	!,
	pathPlusFile(_,File,Path),
	asserta(consulted(Path, File, debug)).
source_debug_record(Path)
	:-
	pathPlusFile(_,File,Path),
	asserta(consulted(Path, File, normal)).

adjust_source_deb(Path)
	:-
	consulted(Path, _, ConsultType),
	!,
	source_level_debugging(SLDB),
	deb_adj_act_on(SLDB, ConsultType, Path).

adjust_source_deb(Path)
	:-
	source_debug_record(Path).

deb_adj_act_on(on, debug, _)
	:-!.
deb_adj_act_on(on, _, Path)
	:-!,
	retract(consulted(Path, File, ConsultType)),
	asserta(consulted(Path, File, debug)).
deb_adj_act_on(off, debug, Path)
	:-
	retract(consulted(Path, File, ConsultType)),
	asserta(consulted(Path, File, normal)).
deb_adj_act_on(off, _, _).

/*-----------------------------------------------------------------*
 |	consultmessage/1
 |	consultmessage(Ctrl)
 |	consultmessage(+)
 |
 |	- Control whether messages are printed during consulting.
 |
 |	Ctrl is on/off
 *-----------------------------------------------------------------*/

export consultmessage/1.

:- make_gv('_consult_message'), set_consult_message(true).

consultmessage(on) 
	:- 
	set_consult_message(true).

consultmessage(off) 
	:- 
	set_consult_message(false).

consultmessage 
	:- 
	get_consult_message(true).

/*-----------------------------------------------------------------*
 | Create access methods to get at the global variable 
 | '_current_consult_directory'.  This should make our
 | consults go faster on certain machines where making
 | system calls to getcwd and chdir are somewhat expensive.
 *-----------------------------------------------------------------*/

:-  make_gv('_current_consult_directory'),
    set_current_consult_directory(''+[]).	%% use default drive and dir

/*-----------------------------------------------------------------------*
 * prepend_current_consult_directory/4
 * prepend_current_consult_directory(Drive,PathList,NewDrive,NewPathList)
 * prepend_current_consult_directory(+,+,-,-)
 *
 *	-- fix up incoming drive & pathlist for locating file
 *-----------------------------------------------------------------------*/

	%% no drive or path specified. Use default.
prepend_current_consult_directory('',[],NewDrive,NewPathList) 
	:-!, 		
	get_current_consult_directory(NewDrive+NewPathList).

	%% not an absolute path (i.e., relative),
	%% but using the same drive:
prepend_current_consult_directory(Drive,PathList,Drive,NewPathList) 
	:-
	PathList \= ['' | _],
	get_current_consult_directory(Drive+CurrentPath),
	dappend(CurrentPath,PathList,NewPathList).

	%% Pathlist is complete (i.e., absolute):
prepend_current_consult_directory(Drive,PathList,Drive,PathList).


/*-------------------------------------------------------------*
 * Used for loading a foreign object file.
 *-------------------------------------------------------------*/

export loadforeign/2.
export loadforeign/3.

loadforeign(File,InitFunction) :-
	loadforeign(File,'',InitFunction).

loadforeign(FNStruct,Libs,InitFunction) :-
	make_file_name(FNStruct,FileName,Type),
	consultmessage('Loading %s...\n',[FileName]),
	loadfor(FileName,Libs,InitFunction,Pathname),
	!,
	consultmessage('%s loaded.\n',[Pathname]).

loadforeign(File,Libs,InitFunction) :-
		%% fload_er:  "Error loading foreign file %t.\n"
	prolog_system_error(forload_er, [File]),
	flush_output(user).

loadfor(FileName,Libs,InitFunction,Pathname) :-
	possibleLocation(FileName,Pathname),
	exists_file(Pathname),		% fails back if not
	'$loadforeign'(Pathname,Libs,InitFunction),
	!.

/*-----------------------------------------------------------------------*
 | load/5
 | load(FileName,Type,CanonPath,Nature,LoadedPath)
 | load(+,+,-,+,-)
 |
 | 	- main entry for (re)consulting files
 |
 |	FileName 	-	an atom naming a file;
 |	Type		- 0 (=consult) / 1 (= reconsult);
 |	CanonPath 	- an uninstantiated variable -- it returns a canoncalized
 |				  path expression for the file which was loaded (if any);
 |	Nature		- source / obp
 *-----------------------------------------------------------------------*/

:-	make_gv('_next_clause_group'), set_next_clause_group(0),
	make_gv('_reconsult_flag'), set_reconsult_flag(0).

	%% File source is the user stream:
load(user,Type,user,Nature,LoadedPath) 
	:-!,
	load_canon(user,Type,Nature,LoadedPath).

	%% Source is a 'normal' file:
load(FileName,Type,CanonPath,Nature,LoadedPath) 
	:-
	possibleLocation(FileName, PathName),
	rootPathFile(Drive, PathList, File, PathName),
	prepend_current_consult_directory(Drive,PathList, NewDrive, CCDPathList),
	rootPathFile(NewDrive, CCDPathList, File, CCDPath),
	check_existence(CCDPath),
	!,
%	canonicalize_pathname(CCDPath,CanonPath),
	canon_path(CCDPath,CanonPath),
	get_current_consult_directory(OldCCD),
	(   
	    set_current_consult_directory(Drive+CCDPathList)
	;   
	      %% Set the CCD back to what it used to be in case if load_canon fails
	      %% FIXME: We should determine if load_canon ever can fail, if not
	      %% this code can be removed
	    set_current_consult_directory(OldCCD),
	      %% Propogate failure
	    fail
	),
	load_canon(CanonPath,Type,Nature,LoadedPath),
	!,
	set_current_consult_directory(OldCCD).

/*-----------------------------------------------------------------------*
 |	check_existence/1
 |	check_existence(Path)
 |	check_existence(+)
 |
 |	- determine whether a (generalized) path is a valid clause source
 |
 |	Valid sources:  
 |		- the user (keyboard) stream;
 |		- a regular file which (by defn) exists;
 |		- the path plus a 'pro' or 'obp' extension is an existing regular file
 *-----------------------------------------------------------------------*/
check_existence(user) :- !.
check_existence(Path) 
	:-
	exists_file(Path),
	file_status(Path,Status),
	(member(type=regular,Status) -> 
		true
		;
		member(type=symbolic_link, Status),
		follow_link(Path,_,FileCode),
		fileTypeCode(FileTypeCode, regular)
	),
	!.
check_existence(Path) 
	:-
	(Suffix=pro;Suffix=obp),
	filePlusExt(Path,Suffix,PathPlusSuffix),
	exists_file(PathPlusSuffix).

/*-----------------------------------------------------------------------*
 |	load_canon/4
 |	load_canon(Path,Type,Nature,LoadedPath)
 |	load_canon(+,+,+,-)
 | 
 |	Type:
 |		0 - consult
 |		1 - reconsult
 |	Nature:
 |		source
 |		obp
 |
 |	load_canon/3 first decomposes the incoming path in the full path to 
 |	the proper file name, and the extension, if it exists; if it doesn't, 
 |	the extension is represented as 'no(extension)'
 |
 |	load_canon/3 then sets up the clause group context for this 
 |	consult/reconsult, abolishing things if necessary;  Then it calls 
 |	load3/4, and afterward cleans up as needed.
 *-----------------------------------------------------------------------*/
load_canon(Path,Type,Nature,LoadedPath) 
	:-
	(filePlusExt(NoSuff,Ext,Path),
	 	(Ext = pro ; Ext = obp)
		; 
		NoSuff = Path, Ext = no(extension)
	),
	!,
	get_fcg(NoSuff,CG),
	push_clausegroup(CG),
	get_reconsult_flag(RFlag0),
	RFlag1 is RFlag0 \/ Type,
	set_reconsult_flag(RFlag1),
	load_canon_reconsult(RFlag1,CG),
	load3(Ext,Path,Type,Nature,LoadedPath),
	set_reconsult_flag(RFlag0),
	pop_clausegroup(_).

load_canon_reconsult(1,CG) 
	:- !,
	massively_abolish_clausegroup(CG).

load_canon_reconsult(0,_).

/*-----------------------------------------------------------------------*
 |	obpPath(Path,OPath)
 *-----------------------------------------------------------------------*/

:- dynamic(obpLocation/2).

export obp_with_pro/0.
obp_with_pro 
	:-
	abolish(obpLocation,2).

export obp_in_cur/0.
obp_in_cur 
	:-
	abolish(obpLocation,2),
	directory_self(Self),
	asserta(obpLocation(_,Self)).

obpPath(Path,OPath)
	:-
	pathPlusFile(PurePath,File,Path),
	obpLocation(PurePath,Dir),
	!,
	obpPath(Dir,File,OPath).

obpPath(Path,OPath)
	:-
	filePlusExt(Path,obp,OPath).


	%% The Dir for obp files is an absolute path:
obpPath(Dir,File,OPath)
	:-
	is_absolute_pathname(Dir),
	!,
	filePlusExt(File,obp,ObpFile),
	pathPlusFile(Dir,ObpFile,OPath).

	%% The Dir for obp files is non-absolute:
obpPath(Dir,File,OPath)
	:-
	get_cwd(CurPath),
	filePlusExt(File,obp,ObpFile),
	extendPath(CurPath, Dir, XPath),
	pathPlusFile(XPath, ObpFile, OPath).

/*-----------------------------------------------------------------------*
 |	load3/5
 |	load3(Ext,Path,Type,Nature,LoadedPath)
 |	load3(+,+,+,+,-)
 *-----------------------------------------------------------------------*/

	%% Requested source was user; load from it:
load3(_,user,Type,Nature,user) 
	:- !,
	load_source(user,Type).

	%% Requested source had an explicit obp extension; so load it:
	%% (It has already passed an existence test above):
load3(obp,OPath,Type,Nature,LoadedPath) 
	:- !,
	filePlusExt(Path,obp,OPath),
	filePlusExt(Path,pro,SPath),
	obp_load(OPath,Status),
	load4_checkstatus(Status,SPath,OPath,OPath).

	%% The requested source had no extension, and the explicit path
	%% exists, so load it as source:
load3(no(extension),Path,Type,Nature,Path) 
	:-
	exists_file(Path),
	file_status(Path,Status),
	dmember(type=regular,Status),
	!,
	load_source(Path,Type).

	%% The requested source had no extension, and (since the clause above
	%% failed), the explicit path file does not exist; so add on both the
	%% 'pro' and 'obp' extensions, and try to load from them; this is the
	%% most common case:
load3(no(extension),Path,Type,Nature,LoadedPath) 
	:- !,
	filePlusExt(Path,pro,SPath),
	obpPath(Path,OPath),
	load4(SPath,OPath,Type,Nature,LoadedPath).

	%% The requested path explicity ended in a 'pro' extension, so 
	%% directly load it:
load3(_,Path,Type,Nature,Path) 
	:-
	load_source(Path,Type).
	
/*-----------------------------------------------------------------------*
 |	load4/5
 |	load4(SPath,OPath,Type,Nature,LoadedPath)
 |	load4(+,+,+,+,-)
 *-----------------------------------------------------------------------*/
	%% We are told to load from source:
load4(SPath,OPath,Type,source,SPath) 
	:- !,
	load_source_object(SPath,OPath).

	%% Compare file times: source file older than obp file,
	%% so load the obp file:
load4(SPath,OPath,Type,_,OPath) 
	:-
	comp_file_times(SPath,OPath),	%% Succeeds if both files exist
									%% and SPath older than OPath
	!,
	obp_load(OPath,Status),
	load4_checkstatus(Status,SPath,OPath).

	%% If source exists it is newer, so load it:
load4(SPath,OPath,Type,_,SPath) 
	:-
	exists_file(SPath),
	deref_file_type(SPath,regular),
	!,
	load_source_object(SPath,OPath).

	%% Source doesn't exist, so load obp file:
load4(SPath,OPath,Type,_,OPath) 
	:-
	obp_load(OPath,Status),
	load4_checkstatus(Status,SPath,OPath).

export deref_file_type/2.
deref_file_type(Path,FType)
	:-
	file_status(Path,Status),
	dmember(type=InitType,Status),
	disp_deref_file_type(InitType,Path,FType).

disp_deref_file_type(symbolic_link,Path,FType) 
	:-!,
	read_link(Path, LinkTarget),
	deref_file_type(LinkTarget,FType).

disp_deref_file_type(FType,Path,FType) :-!.


/*-----------------------------------------------------------------------*
 |	load4_checkstatus/3
 |	load4_checkstatus(Status,SPath,OPath)
 |	load4_checkstatus(Status,SPath,OPath)
 *-----------------------------------------------------------------------*/
load4_checkstatus(0,SPath,OPath) :-	%% FLOAD_FAIL
	!,
		%% proload_er: "Error loading Prolog file %t.\n"
	prolog_system_error(proload_er, [OPath]),
	attempt_load_source_object(SPath,OPath).
load4_checkstatus(1,SPath,OPath) :-	%% FLOAD_SUCCESS
	!.
load4_checkstatus(2,SPath,OPath) :-	%% FLOAD_ILLOBP
		%% old_obp: "%t in old or unrecognized .obp format.\n"
	prolog_system_error(old_obp, [OPath]),
	attempt_load_source_object(SPath,OPath).

/*-----------------------------------------------------------------------*
 *-----------------------------------------------------------------------*/
load_source(Path,Type) :-
	obp_push_stop,
	consult_source(Path,NErrs),
	obp_pop,
	!.
load_source(_,_) :-
	obp_pop,
		%% ld_src_err: "Internal error in load_source for file %t\n"
	prolog_system_error(ld_src_err, []).

/*-----------------------------------------------------------------------*
 *-----------------------------------------------------------------------*/
attempt_load_source_object(SPath,OPath) :-
	exists_file(SPath),
	prolog_system_error(atmpt_cmp, [SPath]),
	load_source_object(SPath,OPath).
attempt_load_source_object(SPath,OPath) :-
	prolog_system_error(ld_fail, [SPath]).

/*-----------------------------------------------------------------------*
 *-----------------------------------------------------------------------*/
load_source_object(SPath,OPath) :-
	obp_open(OPath),
	!,
	consult_source(SPath,NErrs),
	obp_close,
	(   NErrs = 0 
	    ->  true
	;   unlink(OPath) 
	    ->  prolog_system_error(obp_removed,[SPath,OPath])
	;   prolog_system_error(obp_not_removed,[SPath,OPath])
	).
load_source_object(SPath,OPath)
	:-
	prolog_system_error(no_open_fil, [OPath]),
	prolog_system_error(no_obp, [OPath]),
	consult_source(SPath,NErrs).

/*-----------------------------------------------------------------------*
 *-----------------------------------------------------------------------*/
consult_source(Path,NErrs) 
	:- 
	xconsult(Path,NErrs).

/*-----------------------------------------------------------------------*
 *-----------------------------------------------------------------------*/

:- dynamic(file_clause_group/2).

get_fcg(Path,CG) :-
	file_clause_group(Path,CG),
	!.
get_fcg(Path,CG) :-
	get_next_clause_group(CG),
	CG1 is CG+1,
	set_next_clause_group(CG1),
	assertz_at_load_time(file_clause_group(Path,CG)).

/*-----------------------------------------------------------------------*
 * possibleLocation(File, Path)
 *
 *  Enumerate (through backtracking) the possible paths to the file File.
 *
 *  File : an atom representing the file (input)
 *  Path: an atom representing a possible path (output)
 *
 *  The possible paths are:
 *  a. The FileName itself and only the FileName itself if
 *     it is an absolute pathname.
 *  b. The FileName itself.  This will handle the current
 *     directory and the file user.
 *  c. The directories enumerated by the facts of the
 *     procedure searchdir/1.  These directories are obtained
 *     from the ALSPATH environment variable.
 *-----------------------------------------------------------------------*/

:- dynamic(searchdir/1).

possibleLocation(FileName, FileName) :-
	is_absolute_pathname(FileName),
	!.
possibleLocation(FileName, FileName).
possibleLocation(FileName, Path) :-
	searchdir(Dir),
	pathPlusFile(Dir, FileName, Path).


/*-----------------------------------------------------------------------*
 * make_file_name(FileStruct, FileName, Type)
 *
 * FileStruct is a (potentially) structured term representing a filename.
 * FileName will be instantiated to an atom which represents that filename.
 * Type will be instantiated to either consult or reconsult depending on
 * whether the leading character in the print representation was a '-' or
 * not.
 *
 * In addition, backslashes will be converted to slashes or vice versa
 * depending on the operating system.
 *-----------------------------------------------------------------------*/

%% FIXME: Does not substitute '.' in list cells.  This may be fixed by
%%	creating a new write option: dot_lists(Bool)

make_file_name(SName, AName, RFlag) :-
	open(atom(AName0),write,Stream),
	write(Stream, SName),
	close(Stream),
	mfn_check_reconsult(AName0, AName1, RFlag),
	directory_separator(DS_to),
	mfn_opposite_slash(DS_to,DS_from),
	!,
	mfn_fix_slashes(DS_from, DS_to, AName1, AName).

mfn_check_reconsult(NameIn, NameOut, reconsult) :-
	atom_concat('-', NameOut, NameIn),
	!.
mfn_check_reconsult(Name, Name, consult).

mfn_opposite_slash(/,\).
mfn_opposite_slash(\,/).
mfn_opposite_slash(_,'no_opposite_slash\1\2\3').

mfn_fix_slashes(DS_from, DS_to, NameIn, NameOut) :-
	atom_split(NameIn, DS_from, Left, Right),
	!,
	atom_split(NameFixed, DS_to, Left, Right),
	mfn_fix_slashes(DS_from, DS_to, NameFixed, NameOut).
mfn_fix_slashes(_, _, Name, Name).


/*---------------------------------------------------------------------------------*
 * printf/1, printf/2, printf/3, printf/4
 *
 *	Enhanced version of C's printf.  Handles everything that C does
 *	with exception of *'s in formats.  Even this should be handled
 *	someday.  Other bug:  Need field widths and precisions in
 *	printing of terms.
 *
 *	
 *	Formats special to this version of printf:
 *
 *	%t	-- prints Prolog Term
 *	%p	-- calls arg as print procedure
 *
 *	Any other characters will be either printed as is or will be
 *	considered part of a C-style format.
 *
 *	printf/4 allows options (the 4th arg) to be passed to underlying
 *	calls to write_term/[]
 *---------------------------------------------------------------------------------*/

%% FIXME:  Add type checking for ArgList and more type checking for Format

export printf/1.
export printf/2.
export printf/3.
export printf_opt/3.
export printf/4.

printf(Format) :- 
	current_output(Stream),
	printf_check_format(Format, FormatList),
	printf0(FormatList,Stream,[],[]),
	!.

printf(Format,ArgList) :- 
	current_output(Stream),
	printf_check_format(Format, FormatList),
	printf0(FormatList,Stream,ArgList,[]),
	!.

printf_opt(Format,ArgList,Options) :- 
	current_output(Stream),
	printf_check_format(Format, FormatList),
	printf0(FormatList,Stream,ArgList,Options),
	!.

printf(Stream_or_alias,Format,ArgList) :-
	sio:output_stream_or_alias_ok(Stream_or_alias, Stream),
	printf_check_format(Format, FormatList),
	printf0(FormatList,Stream,ArgList,[]).

printf(Stream_or_alias,Format,ArgList,Options) :-
	sio:output_stream_or_alias_ok(Stream_or_alias, Stream),
	printf_check_format(Format, FormatList),
	!,
	printf0(FormatList,Stream,ArgList,Options).

printf_check_format(Var, _) :-
	var(Var),
	!,
	instantiation_error(2).
printf_check_format([], []) :-
	!.
printf_check_format(Atom, List) :-
	atom(Atom),
	!,
	atom_codes(Atom,List).
printf_check_format(FormatList, FormatList) :-
	FormatList = [_ | _],
	!.
printf_check_format(Culprit,_) :-
	type_error(list,Culprit,2).

printf0([],_,_,_) :- !.

%%%%		%t -- print Prolog term
printf0([0'%,0't |Format], Stream, [Term|ArgList],Options) :-
	!,
	sio:write_term(Stream,Term,[quoted(false) | Options]),
	printf0(Format,Stream,ArgList,Options).

%%%%		%p -- call print procedure as argument
printf0([0'%,0'p |Format], Stream, [PrintGoal|ArgList],Options)  
	:-!,
	(PrintGoal = Stream^PrintGoal0 ->
		call(PrintGoal0)
		;
		(PrintGoal = [Stream,Options]^PrintGoal0 ->
			call(PrintGoal0)
			;
			call(PrintGoal)
		)
		
	), 
	printf0(Format,Stream,ArgList,Options).

%%%%		%% -- write out single percent
printf0([0'%,0'% | Format], Stream, ArgList,Options) :-
	!,
	put_code(Stream,0'%),
	printf0(Format, Stream, ArgList,Options).

%%%%		%% -- special case newlines (quoted or not):
printf0([0'\n | Format], Stream, ArgList,Options) 
	:-!,
	(dmember(line_end(false), Options) ->
		put_byte(Stream, 0'\\) ; true ),
	nl(Stream),
	printf0(Format, Stream, ArgList,Options).

%%%%		%k -- print interval
printf0([0'%,0'k |Format], Stream, [PrintItem|ArgList],Options)  
	:-!,
	printf_intv(Stream, '%f', PrintItem, Options),
	printf0(Format,Stream,ArgList,Options).

%%%%		Handle remaining formats:
printf0([0'% | Format], Stream, [Arg | ArgList],Options) 
	:-
	isformat(Format,CFormat,RestFormat,FormatStopper,EndSlot),
	!,
	disp_printf0(FormatStopper, CFormat, EndSlot, RestFormat, Stream, [Arg | ArgList],Options).

%%%%		%<WP>k -- print interval, with width/precision:
disp_printf0(0'k, KFormat, EndSlot, RestFormat, Stream, [Arg | ArgList],Options)
	:-!,
	EndSlot = 0'f,
	atomicize_arg([0'% | KFormat], AFormat),
	printf_intv(Stream, AFormat, Arg, Options),
	printf0(RestFormat,Stream,ArgList,Options).

%%%%		Pass all other formats to C (via sio_sprintf):
disp_printf0(FormatStopper, CFormat, EndSlot, RestFormat, Stream, [Arg | ArgList],Options)
	:-
	EndSlot = FormatStopper,
	atomicize_arg(Arg,AArg),
	atomicize_arg([0'% | CFormat], ACFormat),
	sio_sprintf(ACFormat,AArg,ArgBuf,_),
	put_atom(Stream,ArgBuf),
	printf0(RestFormat,Stream,ArgList,Options).

%%%%		Write any other character as is
printf0([Char |Format], Stream, ArgList,Options) 
	:-
	put_code(Stream,Char),
	printf0(Format,Stream,ArgList,Options).


isformat([Char | RestFormat],[End],RestFormat, Char, End) 
	:-
	isformatstopper(Char),
	!.
isformat([C | Cs], [C | Fs], RestFormat, Stopper, End) 
	:-
	isformat(Cs,Fs,RestFormat, Stopper, End).

isformatstopper(0'd).
isformatstopper(0'i).
isformatstopper(0'o).
isformatstopper(0'u).
isformatstopper(0'x).
isformatstopper(0'X).
isformatstopper(0'f).
isformatstopper(0'e).
isformatstopper(0'E).
isformatstopper(0'g).
isformatstopper(0'G).
isformatstopper(0'c).
isformatstopper(0's).
isformatstopper(0'k).

atomicize_arg(Arg,Arg) 
	:-
	atomic(Arg),
	Arg \= [],
	!.
atomicize_arg(List,Atom) 
	:-
	atom_codes(Atom,List),
	!.
atomicize_arg(_,bad_arg).

:-rel_arith:dynamic('$domain_term'/2).

printf_intv(Stream, Fmt, Item, Options)
	:-
	'$is_delay_var'(Item),
	'$delay_term_for'(Item, Var_DelayTerm),
	arg(4, Var_DelayTerm, ConstraintTerm),
	rel_arith:domain_term_from_constr(ConstraintTerm, DomainTerm),
	rel_arith:valid_domain(DomainTerm, Type, LArg, UArg),
	!,
	epsilon_show(Eps),
	Width is abs(UArg - LArg),
	(Width < Eps ->
		SPrt is (UArg + LArg)/2,
		sio_sprintf(Fmt, SPrt, Buf,_),
		put_atom(Stream, Buf)
		;
		SPrt = [LArg, UArg],
		sio_sprintf(Fmt, LArg, BufL,_),
		sio_sprintf(Fmt, UArg, BufU,_),
		put_code(Stream, 0'[),
		put_atom(Stream, BufL),
		put_code(Stream, 0',),
		put_atom(Stream, BufU),
		put_code(Stream, 0'])
	).

printf_intv(Stream, Fmt, Item, Options)
	:-
	put_atom(Stream, bad_arg).





/*
 *
 * sprintf(List,Format,Args)
 *
 *	Calls printf to put its output into List.
 */

export sprintf/3.
sprintf(Output,Format,Args) 
	:-
	nonvar(Output),
	!,
	(Output = atom(A) ->
		open(atom(A),write,Stream)
		;
		((Output = list(S) ; Output = string(S) ) ->
			open(string(S),write,Stream)
			;
			fail
		)
	),
	printf(Stream,Format,Args),
	close(Stream).

sprintf(Output,Format,Args) 
	:-
	open(string(Output),write,Stream),
	printf(Stream,Format,Args),
	close(Stream).

/*
 * bufwrite/2 and bufwriteq/2
 */

export bufwrite/2.
export bufwriteq/2.

bufwrite(String,Term) :-
	open(string(String),write,Stream),
	write_term(Stream,Term, [line_length(10000),quoted(false),
		maxdepth(20000), quoted_strings(false)]),
	close(Stream).

bufwriteq(String,Term) :-
	open(string(String),write,Stream),
	write_term(Stream,Term, [line_length(10000), quoted(true),
		maxdepth(20000), quoted_strings(false)]),
	close(Stream).

/*
 * old_bufread/2
 * bufread/2
 * bufread/3
 */

export old_bufread/2.
export bufread/2.
export bufread/3.

old_bufread(String,[Term|VarNames]) :- 
	open(string(String),read,Stream),
	read_term(Stream,Term,
		  [attach_fullstop(true),vars_and_names(_,VarNames)]),
	close(Stream).

bufread(String,Term) 
	:-
	bufread(String,Term,[]).

bufread(String,Term,Options) 
	:-
	open(string(String),read,Stream),
	read_term(Stream,Term,[attach_fullstop(true)|Options]),
	close(Stream).

export atomread/2.
export atomread/3.

atomread(Atom,Term) 
	:-
	atomread(Atom,Term,[]).

atomread(Atom,Term,Options) 
	:-
	open(atom(Atom),read,Stream),
	read_term(Stream,Term,[attach_fullstop(true)|Options]),
	close(Stream).

%
% Instantiate the variables in the ThawedTerm with their lexical
% names, and return the new term in FrozenTerm. The NameList, VarList,
% are assumed to be in the form provided by readvnv/3.
%

export varsubst/4.

varsubst(ThawedTerm,FrozenTerm,NameList,VarList) :-
	nonvar(NameList),
	nonvar(VarList),
	nonvar(ThawedTerm),
	subst(ThawedTerm,FrozenTerm,NameList,VarList), !.

subst(ThawedTerm,FrozenTerm,NameList,VarList) :-
	ThawedTerm =.. ThawedList,
	substList(ThawedList,FrozenList,NameList,VarList),
	FrozenTerm =.. FrozenList.


substList([],[],_,_) :- !.
substList([SubTerm|RestThawed],[NewSubTerm|RestFrozen],NameList,VarList) :-
	functor(SubTerm,Functor,Arity), Arity > 0,
	!,
	subst(SubTerm,NewSubTerm,NameList,VarList),
	substList(RestThawed,RestFrozen,NameList,VarList).
substList([SubTerm|RestThawed],[Name|RestFrozen],NameList,VarList) :-
	var(SubTerm), 
	member_identical(SubTerm,VarList,Name,NameList),
	!,
	substList(RestThawed,RestFrozen,NameList,VarList).
substList([SubTerm|RestThawed],[SubTerm|RestFrozen],NameList,VarList) :-
	substList(RestThawed,RestFrozen,NameList,VarList).

member_identical(Item,[H1|_],H2,[H2|_]) :-
	Item == H1,
	!.
member_identical(Item,[_|L1],Item2,[_|L2]) :-
	member_identical(Item,L1,Item2,L2).

:-	compiletime,
	module_closure(exec_to,2,exec_to).

export exec_to/3.
exec_to(Module, TargetStream, Code) :-
	telling(CurrentStream),
	tell(TargetStream),
	Module:call(Code),
	tell(CurrentStream).

endmod.		%% blt_io.pro: I/O Builtins File
