/*==============================================================
 |		blt_cslt.pro
 | Copyright (c) 1986-98 Applied Logic Systems, Inc.
 |
 | Consult, viewed as a development shell facility
 |
 | Authors: Kevin Buettner, Ken Bowen
 | Original Creation Date: 3/20/86
 | Merge3 Revision: Begun 3/6/92 - Ken Bowen
 |	Extracted from the old blt_io.pro & revised:  96-09-24 KAB
 *==============================================================*/

module builtins.
use xconsult.

		%% This is a stack of copt structures:
%:-  make_gv('_current_copts'), set_current_copts([]).
:-  make_gv('_current_copts', []).
%:-  make_gv('_next_clause_group'), set_next_clause_group(1).
:-  make_gv('_next_clause_group', 1).

/*--------------------------------------------------------------*
 |		Miscellaneous directives required by the
 |		ISO Standard.  May gravitate elsewhere if 
 |		better support is required/developed.
 *--------------------------------------------------------------*/

export multifile/1.
multifile(_).

export discontiguous/1.
discontiguous(_).

export include/1.
include(F) :- consult(F).

export ensure_loaded/1.
ensure_loaded(F) :- consult(F, [ensure_loaded(true)]).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% DYNAMIC PREDICATS AND GLOBAL VARIABLES FOR CONSULT
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(consulted/5).
:- dynamic(searchdir/1).

		%%%%%%%%%%%%%%%%%%%
		%% CONSULT OPTIONS
		%%%%%%%%%%%%%%%%%%%
/*------------------------------------------------------------*
 |	Options structure for consult processing:
 |
 |	co(Nature, BaseFile, SrcPath, Ext, Recon, Quiet, TgtMod, SearchPath
 |
 |	Nature	 - source = forced load from source), or
 |			   obp  	 = forced load from obp), or 
 |			   default - check (by date/time) if obp can be loaded
 |	ObpPath  - the actual path to the obp file either written or read, if any.
 |	Recon	 - 1/0 (default = reconsult)
 |	BaseFile - the pure (no path, no ext) file name
 |	OrigDir	 - the path (if any) passed on the filename 
 |	Ext	 	 - the extension (if any) passed on the filename
 |	Quiet	 - true/false - suppress messages (true) or not (false)
 |	TgtMod	 - module into which non-module qualified 
 |			   code is loaded (default = user)
 |	SearchPath	- search path (list), including passed as an option
 |				  in the consult goal, and global stuff
 |	SrcFilePath	- path to actual source file, if any, used in date/time
 |				  comparison against obp file
 |	LoadedPath	- path to actual file loaded
 |	DebugType	- normal (=default) / debug (consult with debug info)
 |	
 *------------------------------------------------------------*/

:-  '$icode'(-18,0,0,0,0), 
	defStruct(cslt_opts, [
	  propertiesList = [
		nature/default,		%%	- Nature: source/default/obp/ensure_loaded
		obp_path/'',		%%	- ObpPath
		recon/reconsult,	%%	- Reconsult flag: reconsult/no_reconsult
		base_file/'',		%%	- BaseFile 
		orig_dir/'',		%%	- OrigDir 
		ext/'',				%%	- Ext 
		verbosity/noisy,	%%	- quiet/noisy
		tgt_mod/user,		%%	- TgtMod 
		searchpath/[],		%%	- Search List for Directory Paths
		origfile/'',		%%	- OrigFileDesc 
		srcfilepath/'',		%%	- SrcFilePath - complete path
		loadedpath/'',		%%	- Path actually loaded (obp, etc)
		debug_type/no_debugging,	%%	- DebugType: debugging/no_debugging
		cg_flag/true,		%%  - true/fail Whether CGs are being used
		obp_locn/giac,		%%  - Obp location: gis/gic/gias/giac/no_obp
		fcg/''				%%  - FCG: File Clause Group (for return to caller)
	],
	accessPred =    access_cslt_opts,
	setPred =       set_cslt_opts,
	makePred =      make_cslt_opts,
	structLabel =   cslt_opts
  ]).


/* %% Create new consult opts structure, with defaults:
cgo_struct(
	co(
		default,	%%	1		- Nature 
		'',			%%	2		- ObpPath
		1,			%%	3		- Recon 
		'',			%%	4		- BaseFile 
		'',			%%	5		- OrigDir 
		'',			%%	6		- Ext 
		Quiet,		%%	7		- Quiet 
		user,		%%	8		- TgtMod 
		[],			%%	9		- SearchPath 
		'',			%%	10		- OrigFileDesc 
		'',			%%	11		- SrcFilePath
		'',			%%	12		- LoadedPath 
		normal		%%	13		- DebugType 
		))
	:-
	consultmessage(Quiet).
*/
	
/*-----------------------------------------------------------------*
 | 	Create access methods to get at the global variable 
 | 	'_current_copts'.  Primarily needed because consult
 |	can be recursively called from inside the subsidiary calls to
 |	xconsult.  Moreover, since we can execute :- goals inside
 |	files (ie, inside xconsult), we have no way of expressly passing
 |	information to such buried calls via arguments to consult/xconsult.
 |	We could use the Prolog database, but using global variables
 |	should be cleaner, and a tiny bit faster.
 *-----------------------------------------------------------------*/

push_copt(Copt)
	:-
	get_current_copts(PrevStack),
	set_current_copts([Copt | PrevStack]).

pop_copt(Copt)
	:-
	get_current_copts([Copt | RestStack]),
	set_current_copts(RestStack).

peek_copt(Copt)
	:-
	get_current_copts([Copt | _]).

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% TOP LEVEL OF CONSULT: consult/[1,2]
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*----------------------------------------------------------------------*
 | consult/1
 | consult(Files)
 | consult(+)
 |
 |	- calls consult(Files, [])
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
 | reconsult/1
 | reconsult(Files)
 | reconsult(+)
 |
 |	- calls consult(Files, [])
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
 | consult/2
 | consult(Files, OptionsList)
 | consult(+, +)
 |
 |	- loads Files, according to OptionsList
 *----------------------------------------------------------------------*/
export consult/1.
export reconsult/1.
export consult/2.

consult(What)
	:-
	consult(What, []).

reconsult(What)
	:-
	consult(What, []).

consult(What, Options)
	:-
	consult_global_options(Options, COpts),
	consult_files(What, COpts),
	builtins:get_primary_manager(ALSMgr),
	send(ALSMgr, refresh_wins).

consult_files([], _) 
	:-!.

consult_files([File | Files], COpts) 
	:-!,
	consult_file(File, COpts),
	!,
	consult_files(Files, COpts).

consult_files(File, COpts) 
	:-
	consult_file(File, COpts).

consult_file(File, COpts) 
	:- 
	local_consult_options(File, BaseFile, COpts, FCOpts),
	!,
	do_consult(BaseFile, FCOpts).

/*-----------------------------------------------------------*
 |	Process the incoming options list from consult/2 into
 |	the consult information struct
 *-----------------------------------------------------------*/
	%% Global defaults: can be over-ridden by command-line
	%% options or consult options:
file_clause_groups(true).
global_verbosity(noisy).
%global_obp_location(giac).

	%% Standard subdirs of alsdir:
system_subdir(shared).
system_subdir(builtins).
system_subdir(library).

consult_global_options(Options, COpts)
	:-
	make_cslt_opts(COpts),
	(clause(global_verbosity(GlobalVerbosity),_) ->
		set_cslt_opts(verbosity, COpts, GlobalVerbosity)
		;
		true
	),
%	(clause(global_obp_location(GlobalObpLocn),_) ->
	(current_prolog_flag(obp_location, GlobalObpLocn) ->
		set_cslt_opts(obp_locn, COpts, GlobalObpLocn)
		;
		true
	),
	(clause(file_clause_groups(true),_) ->
		set_cslt_opts(cg_flag, COpts, true)
		;
		true
	),
	(current_prolog_flag(debug, on) ->
		XOptions = [source(true) | Options],
		set_cslt_opts(debug_type, COpts, debugging)
		;
		XOptions = Options
	),
	proc_cgo(XOptions, COpts),
	check_for_searchdirs(Options, COpts).

proc_cgo([], _).
proc_cgo([Opt | Options], COpts)
	:-
	proc_copt(Opt, COpts),
	proc_cgo(Options, COpts).

proc_copt(source(true), COpts)
	:-!,
	set_cslt_opts(nature, COpts, source).

proc_copt(source(false), COpts)
	:-!,
	set_cslt_opts(nature, COpts, default).

proc_copt(ensure_loaded(true), COpts)
	:-!,
	set_cslt_opts(nature, COpts, ensure_loaded).

proc_copt(consult(true), COpts)
	:-!,
	set_cslt_opts(recon, COpts, no_reconsult).

proc_copt(consult(false), COpts)
	:-!,
	set_cslt_opts(recon, COpts, reconsult).

proc_copt(clause_groups(fail), COpts)
	:-!,
	set_cslt_opts(cg_flag, COpts, fail),
	set_cslt_opts(nature, COpts, source).

proc_copt(clause_groups(false), COpts)
	:-!,
	set_cslt_opts(cg_flag, COpts, fail),
	set_cslt_opts(nature, COpts, source).

proc_copt(quiet(Value), COpts)
	:-
	dmember(Value, [true,false]),
	!,
	(Value = true ->
		set_cslt_opts(verbosity, COpts, quiet)
		;
		set_cslt_opts(verbosity, COpts, noisy)
	).
proc_copt(verbosity(Value), COpts)
	:-
	dmember(Value, [quiet,noisy]),
	!,
	set_cslt_opts(verbosity, COpts, Value).

proc_copt(tgtmod(TgtMod), COpts)
	:-
	atom(TgtMod),
	!,
	set_cslt_opts(tgt_mod, COpts, TgtMod).

	%% "Weak" spec; we'll prepend (append) "./" (syssearchdir)
	%% if they aren't included; this is the "immediate"
	%% one-directory atom case:
proc_copt(search_path(DirPath), COpts)
	:-
	atom(DirPath),
	!,
	proc_copt(search_path([DirPath]), COpts).

	%% "Weak" spec; we'll prepend (append) "./" (syssearchdir)
	%% if they aren't included; this is the list case:
proc_copt(search_path(DirPath), COpts)
	:-
	DirPath = [_ | _],
	!,
	access_cslt_opts(searchpath, COpts, PrevSearchList),
	directory_self(Self),	
	(dmember(Self,  PrevSearchList) ->
		SL0 = DirPath ; SL0 = [Self | DirPath]
	),
	(dmember(sys_searchdir(_),  PrevSearchList) ->
		SL1 = SL0
		;
		builtins:sys_searchdir(SysSearchdir),
		append(SL0, [sys_searchdir(SysSearchdir)], SL1) 
	),
	append(PrevSearchList, SL1, NewSearchList),
	set_cslt_opts(searchpath, COpts, NewSearchList). 	

	%% "strict" spec; no additon of standard directories
	%% if they aren't included - immediate atom case:
proc_copt(strict_search_path(DirPath), COpts)
	:-!,
	access_cslt_opts(searchpath, COpts, PrevSearchList), 	
	(atom(DirPath) -> 
		append(PrevSearchList, [DirPath], NewSearchList)
		;
		append(PrevSearchList, DirPath, NewSearchList)
	),
	set_cslt_opts(searchpath, COpts, NewSearchList). 	

proc_copt(Opt, _)
	:-
	prolog_system_warning(bad_consult_opt, [Opt]).

check_for_searchdirs(Options, COpts)
	:-
	(dmember(search_path(_), Options) ;
		dmember(strict_search_path(_), Options) ),
	!.

check_for_searchdirs(_, COpts)
	:-
	findall(SD, builtins:searchdir(SD), SDs),
	builtins:sys_searchdir(SysSearchdir),
%	findall(SubDirPath, 
%			(system_subdir(SubD), join_path([SysSearchdir,SubD],SubDirPath) ),
%			SSubDs),
%	append(SDs, SSubDs, NewSearchList),
	append(SDs, [sys_searchdir(SysSearchdir)], NewSearchList),
	directory_self(Self),
	set_cslt_opts(searchpath, COpts, [Self | NewSearchList]). 	

/*-----------------------------------------------------------*
 |	Process the information from an individual file
 |	consult into the co() consult information struct
 *-----------------------------------------------------------*/
	%% source(_) forces loading from source:
local_consult_options(File, BaseFile, COpts, FCOpts) 
	:- !,
	access_cslt_opts(nature, COpts,GlobalNature), 
	access_cslt_opts(recon,  COpts,GlobalRecon),

	cslt_info_recon(File, GlobalNature, InitNature, GlobalRecon, Recon, FileDesc),
	file_extension(FileDesc,FF,Ext),
	path_directory_tail(FF, OrigDir, BaseFile),

	(Ext = pro -> Nature = source ; Nature = InitNature),

	copy_term(COpts, FCOpts),
	set_cslt_opts(nature,   FCOpts,Nature), 
	set_cslt_opts(recon,    FCOpts,Recon),
	set_cslt_opts(base_file,FCOpts,BaseFile),
	set_cslt_opts(orig_dir, FCOpts,OrigDir),
	set_cslt_opts(ext,      FCOpts,Ext),
	set_cslt_opts(origfile, FCOpts,FileDesc),

	check_for_shared(BaseFile, FCOpts).

check_for_shared(BaseFile, FCOpts)
	:-
	file_extension(BaseFile, _, Ext),
	cont_check_for_shared(Ext, BaseFile, FCOpts).

cont_check_for_shared(Ext, BaseFile, FCOpts)
	:-
	shared_file_ext(Ext),
	!,
	check_for_sys_shared_first(FCOpts).

cont_check_for_shared(_, _, _).

check_for_sys_shared_first(FCOpts)
	:-
	access_cslt_opts(searchpath, COpts, PrevSearchList), 	
	sys_searchdir(SysSearchdir),
	join_path([SysSearchdir, shared], SubDirPath),
		%% Probably should make sure it is also deleted from
		%% previous path...
	set_cslt_opts(searchpath, COpts, [SubDirPath | PrevSearchList]). 	

/*-----------------------------------------------------------*
 |	Process the source & reconsult information possibly 
 |	attached to an individual file
 *-----------------------------------------------------------*/

cslt_info_recon( user, _, source, _, no_reconsult, user) :-!.
cslt_info_recon(-user, _, source, _, reconsult, user) :-!.

cslt_info_recon(source(InFile), CurNature, Nature, CurRecon, Recon, FileDesc)
	:-!,
	cslt_info_recon(InFile, source, Nature, CurRecon, Recon, FileDesc).

cslt_info_recon(obp(InFile), CurNature, Nature, CurRecon, Recon, FileDesc)
	:-!,
	cslt_info_recon(InFile, obp, Nature, CurRecon, Recon, FileDesc).

cslt_info_recon(+InFile, CurNature, Nature, CurRecon, Recon, FileDesc)
	:-!,
	cslt_info_recon(InFile, CurNature, Nature, no_reconsult, Recon, FileDesc).

cslt_info_recon(-InFile, CurNature, Nature, CurRecon, Recon, FileDesc)
	:-!,
	cslt_info_recon(InFile, CurNature, Nature, reconsult, Recon, FileDesc).

cslt_info_recon(FileDesc, Nature, Nature, Recon, Recon, FileDesc).


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% MESSAGE LEVEL OF CONSULT 
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*-------------------------------------------------------------*
 | do_consult/2
 | do_consult(BaseFile, FileConsultOpts)
 | do_consult(+,+)
 |
 *-------------------------------------------------------------*/ 

do_consult(BaseFile, FCOpts)
	:-
	builtins:get_primary_manager(ALSMgr),
	send(ALSMgr, obtain_src_mgr(BaseFile, FileMgr)),
	send(ALSMgr, get_value(cslt_ctxt, PrevCntxts)),
	(PrevCntxts = [ParentFCOpts | _] ->
		merge_search_paths(FCOpts, ParentFCOpts)
		;
		true
	),
	send(ALSMgr, set_value(cslt_ctxt, [FCOpts | PrevCntxts])),
	catch( (exec_consult(BaseFile, FCOpts, ALSMgr, FileMgr),
				FinalMsg = end_consult),
			Ball,
			consult_except_resp(Ball,FCOpts,FileMgr,FinalMsg)
	     ),
	send(ALSMgr, set_value(cslt_ctxt, PrevCntxts)),
	record_consult(BaseFile, FCOpts, Ball, FileMgr, ALSMgr),
	!,
	consult_msg(FinalMsg, FCOpts).

consult_msg(_, FCOpts)
	:-
	access_cslt_opts(verbosity, FCOpts, quiet), 
	!.

consult_msg(start_consult, FCOpts)
	:-!,
	(builtins:global_verbosity(quiet) -> true;
		access_cslt_opts(origfile, FCOpts, OrigFileDesc), 
		printf(user_output, 'Attempting to consult %t...\n', [OrigFileDesc])
	).

consult_msg(end_consult, FCOpts)
	:-!,
	(builtins:global_verbosity(quiet) -> true;
		access_cslt_opts(loadedpath, FCOpts, LoadedPath), 
		printf(user_output, '... consulted %t\n', [LoadedPath])
	).

consult_msg(partial_consult, FCOpts)
	:-!,
	access_cslt_opts(loadedpath, FCOpts, LoadedPath), 
	printf(user_output, '... partially consulted %t (Errors)\n', [LoadedPath]).

consult_msg(loaded_builtins_file(File,Dir), FCOpts)
	:-!,
	printf(user_output, 'System file %t in %t already loaded.\n', [File, Dir]).

consult_msg(error_consult, FCOpts)
	:-!,
	access_cslt_opts(origfile, FCOpts, FileDesc), 
	printf(user_output, '... Consult of %t ABORTED\n', [FileDesc]).

consult_msg(fail_consult, FCOpts)
	:-!,
	access_cslt_opts(origfile, FCOpts, FileDesc), 
	printf(user_output, '... Consult of %t FAILED\n', [FileDesc]).

record_consult(BaseFile, FCOpts, Ball, FileMgr, ALSMgr)
	:-
	send(ALSMgr, record_src_mgr(BaseFile, FileMgr)),
	access_cslt_opts(fcg, FCOpts, FCG), 

/*
		nature/default,		%%	- Nature: source/default/obp/ensure_loaded
		obp_path/'',		%%	- ObpPath
		recon/reconsult,	%%	- Reconsult flag: reconsult/no_reconsult
		base_file/'',		%%	- BaseFile 
		orig_dir/'',		%%	- OrigDir 
		ext/'',				%%	- Ext 
		verbosity/noisy,	%%	- quiet/noisy
		tgt_mod/user,		%%	- TgtMod 
		searchpath/[],		%%	- Search List for Directory Paths
		origfile/'',		%%	- OrigFileDesc 
		srcfilepath/'',		%%	- SrcFilePath - complete path
		loadedpath/'',		%%	- Path actually loaded (obp, etc)
		debug_type/no_debugging,	%%	- DebugType: debugging/no_debugging
		cg_flag/true,		%%  - true/false Whether CGs are being used
		obp_locn/giac,		%%  - Obp location: gis/gic/gias/giac/no_obp
		fcg/''				%%  - FCG: File Clause Group (for return to caller)
*/

	access_cslt_opts(srcfilepath, FCOpts, SourceFilePath), 
	((FCG \= '', FCG > 0)->
		send(ALSMgr, insert_src_mgr_by_cg(FCG, FileMgr)),
		send(FileMgr, note_loaded(FCG, SourceFilePath))
		;
		true
	),
	access_cslt_opts(obp_path, FCOpts, ObpFilePath), 
	(ObpFilePath \= '' ->
		send(FileMgr, set_value(obp_file,ObpFilePath))
		;
		true
	),
	send(FileMgr, update_errors_wins(Ball)).

consult_except_resp(loaded_builtins_file(File,Dir),FCOpts,FileMgr,end_consult)
	:-!.

consult_except_resp( failed_xxconsult(Path), FCOpts,FileMgr,fail_consult)
	:-!.

consult_except_resp(Ball,FCOpts,FileMgr,partial_consult)
	:-
	Ball = error(consult_load, [src_load, SrcFilePath, ErrList] ),
	!,
	length(ErrList, NErrs),
	send(FileMgr, display_file_errors(NErrs, SrcFilePath, ErrList)).

consult_except_resp(Ball,FCOpts,FileMgr,error_consult)
	:-
	throw(Ball).

/*-------------------------------------------------------------*
 |	If there are any locations in the parent's search path
 |	which are not on the current search path, add them
 |	onto the end of the current path; Note that when we
 |	actually locate a source file to consult (exists_file is
 |	true), we will move that location to the front of the current
 |	search path; 
 |	we move it to the front to implement the traditional 
 |	"prolog loader" philosophy -- see 
 |		rotate_locations(SrcPath, FCOpts) below.
 *-------------------------------------------------------------*/

merge_search_paths(FCOpts, ParentFCOpts)
	:-
	access_cslt_opts(searchpath, FCOpts, ThisSearchPath),
	access_cslt_opts(searchpath, ParentFCOpts, ParentSearchPath),
	merge_sp(ParentSearchPath, ThisSearchPath, XSP),
	set_cslt_opts(searchpath, FCOpts, XSP).

merge_sp(ParentSearchPath, ThisSearchPath, XSP)
	:-
	x_new_places(ParentSearchPath, ThisSearchPath, New),
	(New = [] ->
		XSP = ThisSearchPath
		;
		dappend(ThisSearchPath, New, XSP)
	).

x_new_places([], XSP, []).
x_new_places([Place | ParentSearchPath], ThisSearchPath, New)
	:-
	dmember(Place, ThisSearchPath),
	!,
	x_new_places(ParentSearchPath, ThisSearchPath, New).

x_new_places([Place | ParentSearchPath], ThisSearchPath, [Place | New])
	:-
	x_new_places(ParentSearchPath, ThisSearchPath, New).

/*-------------------------------------------------------------*
	Path = path to the dir in which we found the source file
 *-------------------------------------------------------------*/
rotate_locations(SrcPath, FCOpts)
	:-
	access_cslt_opts(searchpath, FCOpts, SP1),
	path_directory_tail(SrcPath, DirPath, _),
	fin_rotate_locations(SP1, DirPath, FCOpts).

fin_rotate_locations(SP1, Path, FCOpts)
	:-
	list_delete(SP1, Path, SP2),
	set_cslt_opts(searchpath, FCOpts, [Path | SP2]).

/*-------------------------------------------------------------*
 *-------------------------------------------------------------*/

shared_file_ext(psl).

shared_lib_exts(Exts)
	:-
	findall(Ext, shared_file_ext(Ext), Exts).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% MAIN GUTS OF CONSULT 
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*-------------------------------------------------------------*
 | exec_consult/4
 | exec_consult(BaseFile, FileConsultOpts, ALSMgr, FileMgr)
 | exec_consult(+, +, +, +)
 |
 |	- decides whether consult is from user or otherwise,
 *-------------------------------------------------------------*/ 
exec_consult(user, FCOpts, ALSMgr, FileMgr)
	:-!,
	access_cslt_opts(tgt_mod, FCOpts, TgtMod),
	access_cslt_opts(debug_type, FCOpts, DebugMode),
	access_cslt_opts(cg_flag, FCOpts, CGFlag),
	load_file_source(user,user,TgtMod,no_reconsult,DebugMode,_,CGFlag,CG,ErrsList),
	fin_load_from_file([],user,user,CG,FCOpts,FileMgr).

exec_consult(BaseFile, FCOpts, ALSMgr, FileMgr)
	:-
	access_cslt_opts(origfile, FCOpts, OrigFileDesc),
	exec_consult(OrigFileDesc, BaseFile, FCOpts, ALSMgr, FileMgr).

/*-------------------------------------------------------------*
 |	exec_consult/8
 |	exec_consult(PathList, BaseFile, FCOpts, ALSMgr, FileMgr).
 |	exec_consult(+, +, +, +, +)
 |
 |	- dispatches non-user consults:
 |
 |	Attempts the consult grossly as follows:
 |	A.	Source path was absolute; commit to this approach
 |	B.	Source path is non-absolute:
 |		B-1.	Try using the local search path list
 |		B-2.	Try using the global search path
 |	C.	Raise exception for "not found"
 |
 |	Each option, except the last, simply sets up an appropriate
 |	drive+directory to attempt, and calls cont_consult/5.
 *-------------------------------------------------------------*/ 

/******* OLD -- NEED REVISION:
exec_consult(OrigFileDesc, BaseFile, FCOpts, ALSMgr, FileMgr)
	:-
	access_cslt_opts(nature, FCOpts, ensure_loaded), 
	!,
 	consulted(BaseFile, _, ObpPath, _, _),
	send(FileMgr, get_value(obp_file, ObpPath)),
	ObpPath \= nil,
	!,
	note_paths(FCOpts, SrcPath, ObpPath, ObpPath).
 **************/

		%%--------------------------------
		%% Incoming pathlist is absolute:
		%%--------------------------------
exec_consult(OrigFileDesc, BaseFile, FCOpts, ALSMgr, FileMgr)
	:-
	is_absolute_path(OrigFileDesc),
	!,
	(cont_consult(OrigFileDesc, BaseFile, FCOpts, FileMgr) ->
		true
		;
			%% Can't find file -- Throw exception:
		existence_error(file,OrigFileDesc,OrigFileDesc)
	).
		%%--------------------------------
		%% Incoming pathlist is NOT absolute:
		%%--------------------------------

exec_consult(OrigFileDesc, BaseFile, FCOpts, ALSMgr, FileMgr)
	:-
	access_cslt_opts(searchpath, FCOpts, SearchList),
	split_path(OrigFileDesc, PathList),
	path_to_try(SearchList, PathList, TryPath),
	cont_consult(TryPath, BaseFile, FCOpts, FileMgr),
	!.

		%%--------------------------------
		%% Can't find file -- Throw exception:
		%%--------------------------------
exec_consult(OrigFileDesc, BaseFile, FCOpts, ALSMgr, FileMgr)
	:-
	existence_error(file,OrigFileDesc,OrigFileDesc).

/*-------------------------------------------------------------*
 |	path_to_try_from_desc/3
 |	path_to_try_from_desc(SearchList, Desc, TryPath)
 |	path_to_try_from_desc(+, +, +)
 |
 |	SearchList = list of directories
 |	Desc = a file description, either basic, or relative path
 |	TryPath = combination of an element of SearchList with Desc
 |
 |	Resatisfiable...
 |	
 |	Convenience, for use by routines outside blt_cslt.pro
 *-------------------------------------------------------------*/ 
export path_to_try_from_desc/3.
path_to_try_from_desc([], Desc, TryPath)
	:-!,
	builtins:sys_searchdir(DirPath),
	directory_self(Self),
	SearchList = [Self,sys_searchdir(DirPath)],
	split_path(Desc, PathList),
	path_to_try(SearchList, PathList, TryPath0),
	exists_file(TryPath0),
	canon_path(TryPath0, TryPath).

path_to_try_from_desc(SearchList, Desc, TryPath)
	:-
	split_path(Desc, PathList),
	path_to_try(SearchList, PathList, TryPath).

/*-------------------------------------------------------------*
 |	path_to_try(SearchList, BaseFile, TryPath)
 *-------------------------------------------------------------*/ 

export path_to_try/3.
path_to_try([Head | RestSearchList], PathList, TryPath)
	:-
	Head = [_|_],
	!,
	(path_to_try(Head, PathList, TryPath) ;
		path_to_try(RestSearchList, PathList, TryPath) ).
	
path_to_try([sys_searchdir(DirPath) | RestSearchList], PathList, TryPath)
	:-
	split_path(DirPath, DirPathList),
	findall(TryPath, 
				( system_subdir(SubD),
				  append(DirPathList,[SubD],SrcPathList),
				  join_path(SrcPathList, TryPath)
				),
			TPs),
	!,
	tree_path_to_try(TPs, PathList, TryPath).

path_to_try([DirPath | RestSearchList], PathList, TryPath)
	:-
	DirPath \= sys_searchdir(_),
	split_path(DirPath, DirPathList),
	append(DirPathList,PathList,SrcPathList),
	join_path(SrcPathList, TryPath).

path_to_try([_ | RestSearchList], PathList, TryPath)
	:-
	path_to_try(RestSearchList, PathList, TryPath).

tree_path_to_try([DirPath | RestSearchList], PathList, TryPath)
	:-
	split_path(DirPath, DirPathList),
	append(DirPathList,PathList,SrcPathList),
	join_path(SrcPathList, TryPath).

tree_path_to_try([DirPath | RestSearchList], PathList, TryPath)
	:-
	exists_file(DirPath),
	split_path(DirPath, DirPathList),
	append(DirPathList,['*'],PatternList),
	join_path(PatternList, Pattern),
	directory(Pattern,directory,SubdirList0),
	rem_file_dots(SubdirList0, SubdirList),
	member(SSD, SubdirList),
	append(DirPathList, [SSD], SubDirPathList),
	join_path(SubDirPathList, SubDirPath),
	tree_path_to_try([SubDirPath], PathList, TryPath).

tree_path_to_try([DirPath | RestSearchList], PathList, TryPath)
	:-
	tree_path_to_try(RestSearchList, PathList, TryPath).

rem_file_dots([], []).
rem_file_dots(['.' | SubdirList0], SubdirList)
	:-!,
	rem_file_dots(SubdirList0, SubdirList).
rem_file_dots(['..' | SubdirList0], SubdirList)
	:-!,
	rem_file_dots(SubdirList0, SubdirList).
rem_file_dots([Item | SubdirList0], [Item | SubdirList])
	:-
	rem_file_dots(SubdirList0, SubdirList).
/*-------------------------------------------------------------*
 *-------------------------------------------------------------*/ 
cont_consult(Path, BaseFile, FCOpts, FileMgr)
	:-
	access_cslt_opts(ext, FCOpts, OrigExt),
	access_cslt_opts(obp_locn, FCOpts, ObpLocn),
	src_setup(OrigExt, BaseFile, ObpLocn, FCOpts, Path, SrcPath, ObpPath),
	cont_consult(SrcPath, ObpPath, Path, BaseFile, FCOpts, FileMgr).

/*-------------------------------------------------------------*
 *-------------------------------------------------------------*/ 
src_setup(obp, BaseFile, ObpLocn, FCOpts, Path, SrcPath, Path)
	:-!,
	SrcPath = Path.

src_setup(pro, BaseFile, ObpLocn, FCOpts, Path, Path, OPath)
	:-!,
	file_extension(Path, FP,pro),
	make_obp_locn(ObpLocn, FP, BaseFile, FCOpts, OPath).

src_setup(pl, BaseFile, ObpLocn, FCOpts, Path, Path, OPath)
	:-!,
	file_extension(Path,FP,pl),
	make_obp_locn(ObpLocn, FP, BaseFile, FCOpts, OPath).


src_setup('', BaseFile, ObpLocn, FCOpts, Path, Path, '').

src_setup('', BaseFile, ObpLocn, FCOpts, Path, SrcPath, OPath)
	:-
		%% Path has no extension; try adding .pro or .pl:
	member(Ext, [pro, pl]), 
	file_extension(SrcPath,Path,Ext),
	make_obp_locn(ObpLocn, Path, BaseFile, FCOpts, OPath).

src_setup(Ext, BaseFile, ObpLocn, FCOpts, Path, Path, '')
	:-
	Ext \= ''.

make_obp_locn(no_obp, _, BaseFile, FCOpts, '')
	:-!.

make_obp_locn(gis, FP, BaseFile, FCOpts, OPath)
	:-
	file_extension(OPath,FP,obp).

make_obp_locn(gic, FP, BaseFile, FCOpts, OPath)
	:-
	file_extension(BaseFile,RootName,_),
	file_extension(BaseObp,RootName,obp),
	directory_self(Self),
	join_path([Self,BaseObp], OPath).

make_obp_locn(gias, FP, BaseFile, FCOpts, OPath)
	:-
	path_directory_tail(FP, Dir, BaseName),
	sys_env(_,MinorOS,_),
	join_path([Dir, MinorOS], XDir),
	fin_arch_subdir(XDir, BaseName, MinorOS, RDirElts, OPath).

fin_arch_subdir(XDir, BaseName, MinorOS, RDirElts, OPath)
	:-
	(exists_file(XDir) -> true ; make_subdir(XDir)),
	!,
	join_path([XDir,BaseName], XFP),
	file_extension(OPath,XFP,obp).

fin_arch_subdir(XDir, BaseName, MinorOS, RDirElts, '').

make_obp_locn(giac, FP, BaseFile, FCOpts, OPath)
	:-
	file_extension(BaseFile,BaseName,_),
	sys_env(_,MinorOS,_),
	directory_self(Self),
	join_path([Self,MinorOS], XDir),
	fin_arch_subdir(XDir, BaseName, MinorOS, [Self], OPath).


make_obp_locn(gil(ExplPath), FP, BaseFile, FCOpts, OPath)
	:-
	file_extension(BaseFile,RootName,_),
	file_extension(BaseObp,RootName,obp),
	split_path(ExplPath, XPathElts),
	append(XPathElts, [BaseObp], ObpPathElts),
	join_path(ObpPathElts, OPath).

/*-------------------------------------------------------------*
 *-------------------------------------------------------------*/ 
cont_consult(SrcPath, OPath, Path, BaseFile, FCOpts, FileMgr)
	:-
	exists_file(SrcPath),
	file_status(SrcPath, Status),
	dmember(type = FileType, Status),
	(FileType = regular ; FileType = symbolic_link),
		%% This can raise an exception:
	check_for_system_file(BaseFile, SrcPath),
	canon_path(SrcPath, CanonSrcPath),
	rotate_locations(SrcPath, FCOpts),

	consult_msg(start_consult, FCOpts),
	access_cslt_opts(ext, FCOpts, Ext), 	
	access_cslt_opts(nature, FCOpts, Nature), 	


	push_copt(FCOpts),
	unwind_protect(
		load_from_file(Ext,Nature,BaseFile,CanonSrcPath,OPath,FCOpts,FileMgr),
		pop_copt(FCOpts)
	).

check_for_system_file(BaseFile, SrcPath)
	:-
	loaded_builtins_file(BaseFile,SysDir),
	split_path(SrcPath, PathElts),
	dreverse(PathElts, [_, SysDir | _]),
	!,
	throw(loaded_builtins_file(BaseFile,SysDir)).

check_for_system_file(_, _).

/*-------------------------------------------------------------*
 *-------------------------------------------------------------*/ 
load_from_file(Ext,_,BaseFile,CanonSrcPath,_,FCOpts,FileMgr)
	:-
	shared_file_ext(Ext),
	!,
	catch( ( load_slib(CanonSrcPath),
				ErrsList = [] ),
		   Ball,
		   ErrsList = [Ball]
		 ),
	fin_load_from_file(ErrsList,CanonSrcPath,'','',FCOpts,FileMgr).

		%% SrcExt = obp => force (attempted) loading from .obp:
load_from_file(obp,_,BaseFile,CanonSrcPath,OPath,FCOpts,FileMgr)
	:-!,
	(exists_file(OPath) ->
		access_cslt_opts(recon, FCOpts, Recon), 	
		access_cslt_opts(cg_flag, FCOpts, CGFlag), 	
		obp_load_from_path(OPath, CanonSrcPath, BaseFile,  Recon, CGFlag, CG, Status)
		;
		Status = 0
	),
	fin_obp_load_from_path(Status, OPath, CanonSrcPath, BaseFile, CG,FCOpts,FileMgr).

		%% Nature = obp => force (attempted) loading from .obp:
load_from_file(_,obp,BaseFile,CanonSrcPath,OPath,FCOpts,FileMgr)
	:-!,
	(exists_file(OPath) ->
		access_cslt_opts(recon, FCOpts, Recon), 	
		access_cslt_opts(cg_flag, FCOpts, CGFlag), 	
		obp_load_from_path(OPath, CanonSrcPath, BaseFile,  Recon, CGFlag, CG, Status)
		;
		Status = 0
	),
	fin_obp_load_from_path(Status, OPath, CanonSrcPath, BaseFile, CG,FCOpts,FileMgr).

		%% Nature = source => force (attempted) loading from .pro:
load_from_file(_,source,BaseFile,CanonSrcPath,OPath,FCOpts,FileMgr)
	:-!,
	access_cslt_opts(tgt_mod, FCOpts, TgtMod), 	
	access_cslt_opts(recon, FCOpts, Recon), 	
	access_cslt_opts(debug_type, FCOpts, DebugMode), 	
	access_cslt_opts(cg_flag, FCOpts, CGFlag), 	
	load_file_source(CanonSrcPath,BaseFile,TgtMod,Recon,DebugMode,'',CGFlag,CG,ErrsList),
	fin_load_from_file(ErrsList,CanonSrcPath,'',CG,FCOpts,FileMgr).

		%% No extension:
load_from_file('',_,BaseFile,CanonSrcPath,OPath,FCOpts,FileMgr)
	:-!,
	default_load(CanonSrcPath,BaseFile,OPath,FCOpts,FileMgr).

		%% Extension is pro or pl:
load_from_file(Ext,Nature,BaseFile,CanonSrcPath,OPath,FCOpts,FileMgr)
	:-
	dmember(Ext, [pro, pl]),
	!,
	access_cslt_opts(tgt_mod, FCOpts, TgtMod), 	
	access_cslt_opts(recon, FCOpts, Recon), 	
	access_cslt_opts(debug_type, FCOpts, DebugMode), 	
	access_cslt_opts(cg_flag, FCOpts, CGFlag), 	
	load_file_source(CanonSrcPath,BaseFile,TgtMod,Recon,DebugMode,OPath,CGFlag,CG,ErrsList),
	fin_load_from_file(ErrsList,CanonSrcPath,OPath,CG,FCOpts,FileMgr).

		%% Any other extension:
load_from_file(_,_,BaseFile,CanonSrcPath,OPath,FCOpts,FileMgr)
	:-
	access_cslt_opts(tgt_mod, FCOpts, TgtMod), 	
	access_cslt_opts(recon, FCOpts, Recon), 	
	access_cslt_opts(debug_type, FCOpts, DebugMode), 	
	access_cslt_opts(cg_flag, FCOpts, CGFlag), 	
	load_file_source(CanonSrcPath,BaseFile,TgtMod,Recon,DebugMode,'',CGFlag,CG,ErrsList),
	fin_load_from_file(ErrsList,CanonSrcPath,OPath,CG,FCOpts,FileMgr).

/*-------------------------------------------------------------*
 |	Load from either the source file or the .obp file,
 |	depending on date/time.
 *-------------------------------------------------------------*/ 
default_load(CanonSrcPath,BaseFile,ObpPath,FCOpts,FileMgr)
	:-
	comp_file_times( CanonSrcPath, ObpPath),
			%% obp file exists & is newer; load it:
	access_cslt_opts(recon, FCOpts, Recon), 	
	access_cslt_opts(cg_flag, FCOpts, CGFlag), 	
	obp_load_from_path(ObpPath, CanonSrcPath, BaseFile,  Recon, CGFlag, CG, Status),
	fin_obp_load_from_path(Status, ObpPath, CanonSrcPath, BaseFile, CG,FCOpts,FileMgr).

default_load(CanonSrcPath,BaseFile,OPath,FCOpts,FileMgr)
	:-
			%% load source:
	access_cslt_opts(tgt_mod, FCOpts, TgtMod), 	
	access_cslt_opts(recon, FCOpts, Recon), 	
	access_cslt_opts(debug_type, FCOpts, DebugMode), 	
	access_cslt_opts(cg_flag, FCOpts, CGFlag), 	
	load_file_source(CanonSrcPath,BaseFile,TgtMod,Recon,DebugMode,OPath,CGFlag,CG,ErrsList),
	fin_load_from_file(ErrsList,CanonSrcPath,OPath,CG,FCOpts,FileMgr).

/*-------------------------------------------------------------*
 *-------------------------------------------------------------*/ 
fin_load_from_file([],CanonSrcPath,OPath,CG,FCOpts,FileMgr)
	:-!,
	set_cslt_opts(loadedpath, FCOpts, CanonSrcPath),
	set_cslt_opts(srcfilepath, FCOpts, CanonSrcPath),
	set_cslt_opts(fcg, FCOpts, CG).

fin_load_from_file(ErrsList,CanonSrcPath,OPath,CG,FCOpts,FileMgr)
	:-
	copy_term(ErrsList, CErrsList),
	throw(error(consult_load, [src_load,CanonSrcPath,CErrsList])).

/*-------------------------------------------------------------*
 *-------------------------------------------------------------*/ 

	%% FLOAD_FAIL
fin_obp_load_from_path(0, OPath, CanonSrcPath, BaseFile, CG,FCOpts,FileMgr)
	:-
		%% obpload_er: "Error loading Prolog obp file %t.\n"
	prolog_system_error(obpload_er, [OPath]),
	attempt_load_source_object(CanonSrcPath,BaseFile,OPath,FCOpts,FileMgr).

	%% FLOAD_SUCCESS
fin_obp_load_from_path(1, OPath, CanonSrcPath, BaseFile, CG,FCOpts,FileMgr)
	:-
	canon_path(OPath, CanonObpPath),
	set_cslt_opts(obp_path, FCOpts, CanonObpPath), 	
	set_cslt_opts(loadedpath, FCOpts, CanonObpPath),
	set_cslt_opts(srcfilepath, FCOpts, CanonSrcPath),
	set_cslt_opts(fcg, FCOpts, CG).

	%% FLOAD_ILLOBP
fin_obp_load_from_path(2, OPath, CanonSrcPath, BaseFile, CG,FCOpts,FileMgr)
	:-
		%% old_obp: "%t in old or unrecognized .obp format.\n"
	prolog_system_error(old_obp, [OPath]),
	attempt_load_source_object(CanonSrcPath,BaseFile,OPath,FCOpts,FileMgr).

/*-------------------------------------------------------------*
 *-------------------------------------------------------------*/ 

attempt_load_source_object(CanonSrcPath,BaseFile,OPath,FCOpts,FileMgr)
	:-
	load_from_file('',source,BaseFile,CanonSrcPath,OPath,FCOpts,FileMgr).

/*-------------------------------------------------------------*
 *-------------------------------------------------------------*/ 
:- module_closure(simple_load,1).
export simple_load/2.
simple_load(Mod, FullPath)
	:-
	path_directory_tail(FullPath,Dir,FileTail),
	(file_extension(FileTail,NoSuffixFile,_) -> 
		true ; NoSuffixFile = FileTail),
	simple_load3(NoSuffixFile,FullPath,Mod,_).

export simple_load3/4.
simple_load3(NoSuffixFile, Path, Mod,CG)
	:-
	establish_fcg(true, NoSuffixFile, no_reconsult, CG),
	obp_push_stop,
	open(Path, read, Stream, []),
	catch(load_source0(Stream, Mod, no_debugging, '', Path, true, ErrsList),
			_, 
			(reset_fcg(true,_),close(Stream))),
	close(Stream),
	obp_pop,
	reset_fcg(true,_).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% FILE CLAUSE GROUP MANIPULATION
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*-------------------------------------------------------------*
 | establish_fcg/4
 | establish_fcg(CGFlag, NoSuffixPath, ReconsultFlag, CG)
 | establish_fcg(+,+,+,-)
 *-------------------------------------------------------------*/ 
establish_fcg(fail, _, _, 0).
establish_fcg(false, _, _, _).

establish_fcg(true, NoSuffixPath, ReconsultFlag, CG)
	:-
	get_fcg(NoSuffixPath,CG),
	push_clausegroup(CG),
	check_abolish_cg(ReconsultFlag, CG).

reset_fcg(fail,_).
reset_fcg(true,_)
	:-
	pop_clausegroup(_).

/*-------------------------------------------------------------*
 *-------------------------------------------------------------*/ 

check_abolish_cg(reconsult,CG) 
	:- !,
	massively_abolish_clausegroup(CG).

check_abolish_cg(no_reconsult,_).

/*-------------------------------------------------------------*
 *-------------------------------------------------------------*/ 
:- dynamic(file_clause_group/2).

get_fcg(Path,CG) :-
	file_clause_group(Path,CG),
	!.
get_fcg(Path,CG) :-
	get_next_clause_group(CG),
	CG1 is CG+1,
	set_next_clause_group(CG1),
	assertz_at_load_time(file_clause_group(Path,CG)).

/*-------------------------------------------------------------*
 |	For loading source (.pro) files with/or/without writing the .obp file:
 |
 |	load_file_source/9
 |	load_file_source(Path,BaseFile,TgtMod,Recon,DebugMode,OPath,CGFlag,CG,ErrsList) 
 |	load_file_source(+,+,+,+,+,+,+,-,-) 
 |
 |	Path		- path to appropriate source file (MUST exist), or user
 |	BaseFile	- tail of Path, or user
 |	TgtMod		- atom naming module into which non-module code will go
 |	Recon		- reconsult / no_reconsult
 |	DebugMode	- debugging / no_debugging
 |	OPath		- '' / path to *.obp file in which to write OBP code
 |	CGFlag		- true / false (use CGs, or not)
 |	CG			- integer; the clause group # used
 |	ErrsList 	- (possible empty) list of (syntax) errors encountered
 |
 |	Assumptions: Path leads to an appropriate source file which exists,
 |					or else, Path = BaseFile = user
 |
 |	OPath is meaningless if Path = user
 |	If Path \= user, but OPath = '', signifies to NOT create *.obp file
 |
 |	Cannot fail: Either succeeds or raises an exception.
 *-------------------------------------------------------------*/ 
export load_file_source/9.
load_file_source(Path,BaseFile,TgtMod,Recon,DebugMode,OPath,CGFlag,CG,ErrsList) 
	:-
	(Path = user ->
		current_alias(user_input,Stream)
		;
		open(Path, read, Stream, [])
	),
	establish_fcg(CGFlag, BaseFile, Recon, CG),
	catch( try_load_file_source(Stream,TgtMod,DebugMode,OPath,CGFlag, CG,Path,ErrsList),
			Ball,
			( (OPath = '' -> true ;
				(unlink(OPath) ->  
					prolog_system_error(obp_removed,[Path,OPath])
					;   
					prolog_system_error(obp_not_removed,[Path,OPath])
				)
			   ),
			throw(Ball)  )
		 ),
	(ErrsList = [] ->  
		true
		;  
		(OPath = '' ->
			true
			;
			(unlink(OPath) ->  
				prolog_system_error(obp_removed,[Path,OPath])
				;   
				prolog_system_error(obp_not_removed,[Path,OPath])
			)
		)
	).

try_load_file_source(Stream,TgtMod,DebugMode,OPath,CGFlag, CG,Path,ErrsList)
	:-
	catch( load_source0(Stream, TgtMod, DebugMode, OPath, Path, CGFlag, ErrsList),
			Ball,
			(	reset_fcg(CGFlag,_),
				(CGFlag -> massively_abolish_clausegroup(CG) ; true),
				throw(Ball)
			)
		  ),
	!,
	close(Stream),
	reset_fcg(CGFlag,_).

try_load_file_source(Stream,TgtMod,DebugMode,OPath,CGFlag, CG,Path,ErrsList)
	:-
	close(Stream),
	pop_clausegroup(CG),
	(CGFlag -> massively_abolish_clausegroup(CG) ; true),
		%% ld_src_err: "Internal error in load_source for file %t\n"
	prolog_system_error(ld_src_err, [Path]).

/*-------------------------------------------------------------*
 *-------------------------------------------------------------*/ 
obp_load_from_path(OPath, CanonSrcPath, BaseFile,  Recon, CGFlag, CG, Status)
	:-
	establish_fcg(CGFlag, BaseFile, Recon, CG),
	obp_load(OPath, Status),
	reset_fcg(CGFlag,_).

		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%% LOWEST-LEVEL FILE LOADING
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*-------------------------------------------------------------*
 *-------------------------------------------------------------*/ 
	%% NO OBP: OPath = ''
obp_mode_start('')
	:-!,
	obp_push_stop.

obp_mode_end('')
	:-!,
	obp_pop.

	%% Generate OBPs: OPath \= ''
obp_mode_start(OPath)
	:-
	obp_open(OPath).

obp_mode_end(OPath)
	:-
	obp_close.

/*-------------------------------------------------------------*
 | For loading source (.pro, .pl) files 
 |
 |	load_source0/7
 |	load_source0(Stream, TgtMod, DebugMode, OPath, Path, CGFlag, ErrsList) 
 |	load_source0(+, +, +, +, +, +, -) 
 |
 | 	-	if OPath = '',  -- without writing the .obp file
 | 	-	if OPath \= '', -- writes the .obp file to OPath
 *-------------------------------------------------------------*/ 
export load_source0/6.
load_source0(Stream, TgtMod, DebugMode, OPath, Path, CGFlag, ErrsList) 
	:-
	obp_mode_start(OPath),
	xconsult:pushmod(TgtMod),
	catch(xxconsult(Stream, Path, DebugMode, CGFlag, ErrsList),
			Ball, 
			( xconsult:popmod, 
			  obp_mode_end(OPath),
			  throw(Ball)   )
		),
	!,
	xconsult:popmod,
	obp_mode_end(OPath).

load_source0(_,_,_,OPath,Path, CGFlag, _) 
	:-
	xconsult:popmod,
	obp_mode_end(OPath),
	!,
		%% ld_src_err: "Internal error in load_source for file %t\n"
	prolog_system_error(ld_src_err, [Path]),
	throw( failed_xxconsult(Path) ).


		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%% LOWEST-LEVEL SLIB LOADING
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*----------------------------------------------------------------------*

load_slib/1

Description
-----------

load_slib(File) is true.

Procedurally, load_slib(File) is executed as follows:

a) The shared c-prolog library File is loaded.

b) The goal succeeds.

Templates and modes
-------------------

load_slib(+File)

Errors
------

a) File is a variable.
   - instantiation_error.

b) File is neither a variable nor an atom
   - type_error(atom, File).

c) The library specified by File does not exist
   - existence_error(shared_library, File).

d) The library specified by File cannot be executed
   - permission_error(execute, shared_library, File).

e) The file specified by File is not a shared library
   - system_error(not_shared_library(File)).

f) Insufficient memory to load the library specified by File
   - resource_error(shared_library_memory).

g) The library specified by File could not be initialized
   - system_error(shared_library_init_error(File, os_code(NativeResult))).

e) The library specified by File could not be loaded due to system error
   - system_error(shared_library_error(Error, File, os_code(NativeResult))).

Note - Some errors return os_code/1 term that contains the
numerical error code or string generated by the local operating
system.

 *----------------------------------------------------------------------*/

export load_slib/1.

load_slib(File) :-
	atom_ok(File),
	load_plugin(File, PluginInfo, Result, NativeResult),
	check_plugin_result(Result, File, NativeResult,
	                    builtins:load_slib(File)).

check_plugin_result(no_error, _, _, _) :- !.
check_plugin_result(file_not_found_error, File, NativeResult, Goal) :-
	existence_error(shared_library, File, Goal).
check_plugin_result(permission_error, File, NativeResult, _) :-
	permission_error(execute, shared_library, File, Goal).
check_plugin_result(not_plugin_error, File, NativeResult, Goal) :-
	system_error(not_shared_library(File)).
check_plugin_result(memory_error, File, NativeResult, _) :-
	resource_error(shared_library_memory, [os_code(NativeResult)]).
check_plugin_result(version_error, File, NativeResult, _) :-
	system_error(shared_library_version_error(File,
	incompatable_version(NativeResult))).
check_plugin_result(init_error, File, NativeResult, _) :-
	system_error(shared_library_init_error(File, os_code(NativeResult))).
check_plugin_result(Error, File, NativeResult, _) :-
	system_error(shared_library_error(Error, File, os_code(NativeResult))).

endmod.		%% blt_cslt.pro: Consult 

