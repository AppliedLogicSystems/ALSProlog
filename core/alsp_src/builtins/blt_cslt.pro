/*==============================================================
 |		blt_cslt.pro
 | Copyright (c) 1986-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
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

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% DYNAMIC PREDICATS AND GLOBAL VARIABLES FOR CONSULT
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(consulted/5).
:- dynamic(searchdir/1).
:- dynamic(obpLocation/2).
:- dynamic(src2srcLocation/3).

/*-----------------------------------------------------------------*
 | 	Create access methods to get at the global variable 
 | 	'_current_consult_directory'.  Primarily needed because consult
 |	can be recursively called from inside the subsidiary calls to
 |	xconsult.  Moreover, since we can execute :- goals inside
 |	files (ie, inside xconsult), we have no way of expressly passing
 |	information to such buried calls via arguments to consult/xconsult.
 |	We could use the Prolog database, but using global variables
 |	should be cleaner, and a tiny bit faster.
 *-----------------------------------------------------------------*/

:-  make_gv('_current_consult_directory'),
			%% use default drive and dir path list:
    set_current_consult_directory(''+[]).	

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
:-	make_gv('_next_clause_group'), set_next_clause_group(0).

/*-----------------------------------------------------------------*
 *-----------------------------------------------------------------*/
:-	make_gv('_reconsult_flag'), set_reconsult_flag(1).

adjust_recon_flag(FCOpts)
	:-
	arg(3, FCOpts, LocalRecon),		%%	3		- Recon 
	get_reconsult_flag(RFlag0),
	RFlag1 is RFlag0 /\ LocalRecon,
	set_reconsult_flag(RFlag1).

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
	cslt_init(MsgValue,ReconFlg, FCGValue),
	consult_global_options(Options, COpts),
	consult_files(What, COpts),
	cslt_cleanup(MsgValue,ReconFlg, FCGValue).

consult_files([], _) 
	:-!.

consult_files([File | Files], COpts) 
	:- 
	consult_files(File, COpts),
	!,
	consult_files(Files, COpts).

consult_files(File, COpts) 
	:- 
	local_consult_options(File, BaseFile, COpts, FCOpts),
	do_consult(BaseFile, FCOpts).

cslt_init(MessageValue,ReconFlg, FCGValue)
	:-
	consultmessage(MessageValue),
	file_clause_groups(FCGValue),
	get_reconsult_flag(ReconFlg).

cslt_cleanup(MessageValue,ReconFlg, FCGValue)
	:-
	set_consult_messages(MessageValue),
	set_file_clause_groups(FCGValue),
	set_reconsult_flag(ReconFlg).

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
	%% Create new consult opts structure, with defaults:
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
	
/*-----------------------------------------------------------*
 |	Process the incoming options list from consult/2 into
 |	the co() consult information struct
 *-----------------------------------------------------------*/
consult_global_options(Options, COpts)
	:-
	cgo_struct(COpts),
	proc_cgo(Options, COpts).

proc_cgo([], _).
proc_cgo([Opt | Options], COpts)
	:-
	proc_copt(Opt, COpts),
	proc_cgo(Options, COpts).

proc_copt(consult(true), COpts)
	:-!,
	mangle(3, COpts, consult).

proc_copt(consult(false), COpts)
	:-!,
	mangle(3, COpts, reconsult).

proc_copt(quiet(Value), COpts)
	:-
	dmember(Value, [true,false]),
	!,
	mangle(7, COpts, Value),
	set_consult_messages(Value).

proc_copt(tgtmod(TgtMod), COpts)
	:-
	atom(TgtMod),
	!,
	mangle(8, COpts, TgtMod).

proc_copt(s(DirPath), COpts)
	:-
	atom(DirPath),
	!,
	rootPlusPath(Drive, DirPathList, DirPath),
	arg(9, COpts, PrevSearchList), 	%%	9		- SearchPath 
	append(PrevSearchList, [Drive+DirPathList], NewSearchList),
	mangle(9, COpts, NewSearchList).

proc_copt(Opt, _)
	:-
	prolog_system_warning(bad_consult_opt, [Opt]).

/*-----------------------------------------------------------*
 |	Process the information from an individual file
 |	consult into the co() consult information struct
 *-----------------------------------------------------------*/

	%% source(_) forces loading from source:
local_consult_options(File, BaseFile, COpts, FCOpts) 
	:- !,
	arg(1,COpts,GlobalNature), 
	arg(3,COpts,GlobalRecon),
	cslt_info_recon(File, GlobalNature, Nature, GlobalRecon, Recon, FileDesc),
	cslt_path_x(FileDesc, SrcPath, BaseFile, Ext),

	copy_term(COpts, FCOpts),
	mangle(1,FCOpts,Nature), 
	mangle(3,FCOpts,Recon),
	mangle(5,FCOpts,SrcPath),
	mangle(4,FCOpts,BaseFile),
	mangle(6,FCOpts,Ext),
	mangle(10,FCOpts,FileDesc).

/*-----------------------------------------------------------*
 |	Process the source & reconsult information possibly 
 |	attached to an individual file
 *-----------------------------------------------------------*/

cslt_info_recon( user, _, source, _, 0, FileDesc) :-!.
cslt_info_recon(-user, _, source, _, 1, FileDesc) :-!.

cslt_info_recon(source(InFile), CurNature, Nature, CurRecon, Recon, FileDesc)
	:-!,
	cslt_info_recon(InFile, source, Nature, CurRecon, Recon, FileDesc).

cslt_info_recon(obp(InFile), CurNature, Nature, CurRecon, Recon, FileDesc)
	:-!,
	cslt_info_recon(InFile, obp, Nature, CurRecon, Recon, FileDesc).

cslt_info_recon(+InFile, CurNature, Nature, CurRecon, Recon, FileDesc)
	:-!,
	cslt_info_recon(InFile, CurNature, Nature, 0, Recon, FileDesc).

cslt_info_recon(-InFile, CurNature, Nature, CurRecon, Recon, FileDesc)
	:-!,
	cslt_info_recon(InFile, CurNature, Nature, 1, Recon, FileDesc).

cslt_info_recon(FileDesc, Nature, Nature, Recon, Recon, FileDesc).

cslt_path_x(user, ''+[], user, no(extension)) :-!.
cslt_path_x(FileDesc, Drive+SrcPathList, BaseFile, Ext)
	:-
	rootPathFile(Drive,SrcPathList,File,FileDesc),
	(filePlusExt(BaseFile,Ext,File),!
		; 
		BaseFile = File, Ext = no(extension)
	).

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
	arg(7, FCOpts, Quiet),		%%	7		- Quiet 
	arg(6, FCOpts, Ext),		%%	7		- Extension 
	consult_msg(Quiet, Ext, BaseFile, start_consult, FCOpts),
	get_current_consult_directory(OldCCD),
	catch( exec_consult(BaseFile, FCOpts),
			Ball,
			( set_current_consult_directory(OldCCD),
				set_reconsult_flag(1),
				throw(Ball)
			) ),

	record_consult(BaseFile, FCOpts),
	cleanup_cslt,
	!,
	consult_msg(Quiet, Ext, BaseFile, end_consult, FCOpts).

		%% true = "be quiet"
consult_msg(true, _, BaseFile, FCOpts, MsgCode) :-!.

		%% false = "write messages"
consult_msg(false, Ext, BaseFile, MsgCode, FCOpts)
	:-
	consult_msg_args(MsgCode, Ext, FCOpts, Args),
	revise_cslt_msg_code(MsgCode, Ext, FCOpts, RevMsgCode),
	prolog_system_warning(RevMsgCode, Args),
	(BaseFile = user -> als_advise('\n') ; true).

consult_msg_args(start_consult, Ext, FCOpts, [OrigFileDesc])
	:-
	arg(10, FCOpts, OrigFileDesc).

consult_msg_args(end_consult, Ext, FCOpts, [LoadedPath])
	:-
	arg(12, FCOpts, LoadedPath).

cleanup_cslt.

revise_cslt_msg_code(start_consult, Ext, FCOpts, start_shared_load)
	:-
	(shared_file_ext(Ext); Ext=share),
	!.
revise_cslt_msg_code(end_consult, Ext, FCOpts, end_shared_load)
	:-
	(shared_file_ext(Ext); Ext=share;
		Ext=no(extension),
		arg(12, FCOpts, LoadedPath),
		filePlusExt(_,LoadedExt,LoadedPath),
		shared_file_ext(LoadedExt)
	),
	!.
revise_cslt_msg_code(MsgCode, Ext, _, MsgCode).

/*
shared_file_ext(so).
shared_file_ext(sl).
shared_file_ext(dll).
*/
shared_file_ext(psl).

shared_lib_exts(Exts)
	:-
	findall(Ext, shared_file_ext(Ext), Exts).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% MAIN GUTS OF CONSULT 
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*-------------------------------------------------------------*
 | exec_consult/2
 | exec_consult(BaseFile, FileConsultOpts)
 | exec_consult(+, +)
 |
 |	- decides whether consult is from user or otherwise,
 *-------------------------------------------------------------*/ 
exec_consult(user, FCOpts)
	:-!,
	adjust_recon_flag(FCOpts),
	load_source(user,_),
	mangle(12, FCOpts, user),
	set_reconsult_flag(1).

exec_consult(BaseFile, FCOpts)
	:-
	arg(1, FCOpts, Nature),
	arg(5, FCOpts, Drive+PathList),		%%	5		- OrigDir 
	arg(6, FCOpts, SrcExt),
	exec_consult(PathList, Drive, BaseFile, Nature, SrcExt, FCOpts).

/*-------------------------------------------------------------*
 |	exec_consult/6
 |	exec_consult(PathList, Drive, BaseFile, Nature, SrcExt, FCOpts).
 |	exec_consult(+, +, +, +, +, +)
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

		%% Incoming pathlist is absolute:
		%%--------------------------------
exec_consult(['' | RestPathList], Drive, BaseFile, Nature, SrcExt, FCOpts)
	:-!,
	(cont_consult(SrcExt, Nature, Drive+['' | RestPathList], BaseFile, FCOpts) ->
		true
		;
			%% Can't find file -- Throw exception:
		arg(10, FCOpts, OrigFileDesc), 		%%	10		- OrigFileDesc 
		existence_error(file,OrigFileDesc,OrigFileDesc)
	).
		%%--------------------------------
		%% Incoming pathlist is NOT absolute:
		%%--------------------------------

exec_consult(PathList, Drive, BaseFile, Nature, SrcExt, FCOpts)
	:-
	path_to_try(SrcExt,PathList, Drive,BaseFile,Nature,FCOpts,TryDrive,TryPath),
	cont_consult(SrcExt, Nature, TryDrive, TryPath, BaseFile, FCOpts).


:-dynamic(searchdir/1).
:-dynamic(searchdir/2).


	%% Try the local search path list:
path_to_try(SrcExt,PathList, Drive, BaseFile, Nature,FCOpts,TryDrive,TryPath)
	:-
	arg(9, FCOpts, LocalSearchPathLists),		%%	9		- SearchPath 
	directory_self(SelfDir),
	subPath(SelfDirPathList, SelfDir),
	member(DD+PL, [Drive+SelfDirPathList | LocalSearchPathLists]), 
	append(PL,PathList,SrcPathList),
	prepend_current_consult_directory(Drive,PathList,TryDrive,TryPath).

		%% SrcExt is a shared library extension;
		%% Try loading from ALSDIR\shared first:
path_to_try(SrcExt,PathList, Drive, BaseFile, Nature,FCOpts,TryDrive,TryPath)
	:-
	(SrcExt = share; shared_file_ext(SrcExt)),
	sys_searchdir(ALSDIR),
	extendPath(ALSDIR, shared, SharedLibDir),
	rootPlusPath(TryDrive,TryPath,SharedLibDir).

		%% Try the global search path list, abstract form:
path_to_try(SrcExt,PathList, Drive, BaseFile, Nature,FCOpts,TryDrive,TryPath)
	:-
	searchdir(TryDrive, TryPath),
	SDPathList = [_|_].

		%% Try the global search path list, old form:
path_to_try(SrcExt,PathList, Drive, BaseFile, Nature,FCOpts,TryDrive,TryPath)
	:-
	searchdir(SearchDir),
	rootPlusPath(TryDrive,SDPathList,SearchDir),
	append(SDPathList,PathList,TryPath).

		%% Try the system builtins directory:
path_to_try(SrcExt,PathList, Drive, BaseFile, Nature,FCOpts,TryDrive,TryPath)
	:-
	sys_searchdir(ALSDIR),
	extendPath(ALSDIR, builtins, BuiltinsDir),
	rootPlusPath(TryDrive,TryPath,BuiltinsDir).

		%% Try the system shared directory (incoming had no extension):
path_to_try(SrcExt,PathList, Drive, BaseFile, Nature,FCOpts,TryDrive,TryPath)
	:-
	sys_searchdir(ALSDIR),
	extendPath(ALSDIR, shared, BuiltinsDir),
	rootPlusPath(TryDrive,TryPath,BuiltinsDir).

		%% Try the original system builtins directory (for packages):
path_to_try(SrcExt,PathList, Drive, BaseFile, Nature,FCOpts,TryDrive,TryPath)
	:-
	orig_sys_searchdir(ALSDIR),
	extendPath(ALSDIR, shared, SharedDir),
	rootPlusPath(TryDrive,TryPath,SharedDir).

		%% Can't find file
		%% Throw exception:
path_to_try(SrcExt,PathList, Drive, BaseFile, Nature,FCOpts,TryDrive,TryPath)
	:-
	arg(10, FCOpts, OrigFileDesc), 		%%	10		- OrigFileDesc 
	existence_error(file,OrigFileDesc,OrigFileDesc).

/**********************************
		%% Incoming pathlist is NOT absolute:
		%% Try the local search path list:
exec_consult(PathList, Drive, BaseFile, Nature, SrcExt, FCOpts)
	:-
	arg(9, FCOpts, LocalSearchPathLists),		%%	9		- SearchPath 
	directory_self(SelfDir),
	subPath(SelfDirPathList, SelfDir),
	member(DD+PL, [Drive+SelfDirPathList | LocalSearchPathLists]), 
	append(PL,PathList,SrcPathList),
	cont_consult(SrcExt, Nature, DD+SrcPathList, BaseFile, FCOpts).

		%% Incoming pathlist is NOT absolute;
		%% SrcExt is a shared library extension;
		%% Try loading from ALSDIR\shared first:
exec_consult(PathList, Drive, BaseFile, Nature, SrcExt, FCOpts)
	:-
	(SrcExt = share; shared_file_ext(SrcExt)),
	sys_searchdir(ALSDIR),
	extendPath(ALSDIR, shared, SharedLibDir),
	rootPlusPath(SDDrive,SrcPathList,SharedLibDir),
	cont_consult(SrcExt, Nature, SDDrive+SrcPathList, BaseFile, FCOpts).

		%% Incoming pathlist is NOT absolute:
		%% Try the global search path list:
exec_consult(PathList, Drive, BaseFile, Nature, SrcExt, FCOpts)
	:-
	searchdir(SearchDir),
	rootPlusPath(SDDrive,SDPathList,SearchDir),
	append(SDPathList,PathList,SrcPathList),
	cont_consult(SrcExt, Nature, SDDrive+SrcPathList, BaseFile, FCOpts).

exec_consult(PathList, Drive, BaseFile, Nature, SrcExt, FCOpts)
	:-
%	searchdir(SearchDir),
%	rootPlusPath(SDDrive,SDPathList,SearchDir),
	searchdir(SDDrive, SDPathList),
	SDPathList = [_|_],
	append(SDPathList,PathList,SrcPathList),
	cont_consult(SrcExt, Nature, SDDrive+SrcPathList, BaseFile, FCOpts).


		%% Try the system builtins directory:
exec_consult(PathList, Drive, BaseFile, Nature, SrcExt, FCOpts)
	:-
	sys_searchdir(ALSDIR),
	extendPath(ALSDIR, builtins, BuiltinsDir),
	rootPlusPath(SDDrive,SrcPathList,BuiltinsDir),
	cont_consult(SrcExt, Nature, SDDrive+SrcPathList, BaseFile, FCOpts).

		%% Try the system shared directory:
exec_consult(PathList, Drive, BaseFile, Nature, SrcExt, FCOpts)
	:-
	sys_searchdir(ALSDIR),
	extendPath(ALSDIR, shared, BuiltinsDir),
	rootPlusPath(SDDrive,SrcPathList,BuiltinsDir),
	cont_consult(SrcExt, Nature, SDDrive+SrcPathList, BaseFile, FCOpts).

		%% Try the original system builtins directory (for packages):
exec_consult(PathList, Drive, BaseFile, Nature, SrcExt, FCOpts)
	:-
	orig_sys_searchdir(ALSDIR),
	extendPath(ALSDIR, shared, BuiltinsDir),
	rootPlusPath(SDDrive,SrcPathList,BuiltinsDir),
	cont_consult(SrcExt, Nature, SDDrive+SrcPathList, BaseFile, FCOpts).

		%% Can't find file
		%% Throw exception:
exec_consult(_, Drive, BaseFile, Nature, SrcExt, FCOpts)
	:-
	arg(10, FCOpts, OrigFileDesc), 		%%	10		- OrigFileDesc 
	existence_error(file,OrigFileDesc,OrigFileDesc).

**********************************/

/*-------------------------------------------------------------*
 |	- Determine real source file path, and setup global
 |	  information for this specific consult;
 |	- Do the loading with load_from_file/6
 |	- Restore appropriate global information.
 *-------------------------------------------------------------*/ 
path_src_check(no(extension), BaseFile, NewDrive, NewPathList, SrcFilePath)
	:-!,
	rootPathFile(NewDrive,NewPathList,BaseFile,SrcFilePath).

path_src_check(SrcExt, BaseFile, NewDrive, NewPathList, SrcFilePath)
	:-
	filePlusExt(BaseFile, SrcExt, SrcFileName),
	rootPathFile(NewDrive,NewPathList,SrcFileName,SrcFilePath).

%cont_consult(SrcExt, Nature, Drive+PathList, BaseFile, FCOpts)
cont_consult(SrcExt, Nature, NewDrive, NewPathList, BaseFile, FCOpts)
	:-
%	prepend_current_consult_directory(Drive,PathList,NewDrive,NewPathList),
%NewDesc = NewDrive+NewPathList,
path_src_check(SrcExt, BaseFile, NewDrive, NewPathList, SrcFilePath),


	s2s_ext(Exts2Try),
	check_existence(SrcFilePath, Exts2Try,ExistingSrcFilePath, WorkingExt),
			%% ExistingSrcFilePath is a complete path, including
			%% extension, if appropriate; hence, so it CanonSrcPath:
	canon_path(ExistingSrcFilePath, CanonSrcPath),
		%%% Change the Drive+Path current_consult global variable:
	get_current_consult_directory(OldCCD),
	(   
	    set_current_consult_directory(NewDrive+NewPathList)
		;   
	      %% Set the CCD back to what it used to be in case if load_canon fails
	      %% FIXME: We should determine if load_canon ever can fail, if not
	      %% this code can be removed -- BETTER, pack this into exception
		  %% handling & cleanup:
	    set_current_consult_directory(OldCCD),
	      %% Propogate failure
	    fail
	),
		%%% Handle reconsult flag here............
	adjust_recon_flag(FCOpts),

	(shared_file_ext(WorkingExt) ->
		load_slib(CanonSrcPath),
		mangle(12, FCOpts, CanonSrcPath)
		;
		load_from_file(SrcExt,Nature,BaseFile,CanonSrcPath,WorkingExt,FCOpts)
	),

		%%% Undo CCD global change here...................
	set_current_consult_directory(OldCCD),
		%%% Undo reconsult flag here............
	set_reconsult_flag(1).

/*-------------------------------------------------------------*
 |	load_from_file/6
 |	load_from_file(SrcExt,Nature,BaseFile,CanonSrcPath,WkExt,FCOpts)
 |	load_from_file(+,+,+,+,+,+)
 |
 |	Branches on combination of SrcExt + Nature + WkExt;
 |	Implements rules governing loading of source vs. obp
 *-------------------------------------------------------------*/ 
		%% SrcExt = obp => force (attempted) loading from .obp:
load_from_file(obp,_,BaseFile,CanonSrcPath,_,FCOpts)
	:-!,
	obp_load_file(CanonSrcPath, BaseFile, FCOpts).

		%% Nature = obp => force (attempted) loading from .obp:
load_from_file(_,obp,BaseFile,CanonSrcPath,_,FCOpts)
	:-!,
	obp_load_file(CanonSrcPath, BaseFile, FCOpts).

load_from_file(no(extension),_,BaseFile,CanonSrcPath,no(extension),FCOpts)
	:-!,
	outFilePath(obp, CanonSrcPath, BaseFile, OutFilePath),
	default_load(CanonSrcPath,BaseFile,OutFilePath,FCOpts).

load_from_file(SrcExt,Nature,BaseFile,CanonSrcPath,WkExt,FCOpts)
	:-
			%% Found in blt_shlr.pro:
	transformer_db(WkExt, GenExt, DelFlag, ExtOpts), 
	outFilePath(GenExt, CanonSrcPath, BaseFile, OutFilePath),
	fin_load_from_file(GenExt,Nature,BaseFile,CanonSrcPath,
						OutFilePath,WkExt,ExtOpts,DelFlag,FCOpts).

fin_load_from_file(obp,Nature,BaseFile,CanonSrcPath,OutFilePath,
					WkExt,ExtOpts,DelFlag,FCOpts)
	:-!,
	default_load(CanonSrcPath,BaseFile,OutFilePath,FCOpts).

fin_load_from_file(GenExt,Nature,BaseFile,CanonSrcPath,OutFilePath,
					WkExt,ExtOpts,DelFlag,FCOpts)
	:-
	comp_file_times(CanonSrcPath, OutFilePath),
	!,
	load_from_file(GenExt,Nature,BaseFile,OutFilePath,GenExt,FCOpts).

fin_load_from_file(GenExt,Nature,BaseFile,SrcPath,OutFilePath,
					WkExt,ExtOpts,DelFlag,FCOpts)
	:-
	extract_opts(FCOpts, FCOptions),
	dappend(ExtOpts, FCOptions, Options),
			%% Found in blt_shlr.pro:
	pathPlusFile(SrcPath,_,SrcPathDir),
	src2src_inv(WkExt, BaseFile, SrcPath, Options),
	load_from_file(GenExt,Nature,BaseFile,OutFilePath,GenExt,FCOpts),
	(DelFlag = no_del ->
		true 
		;
		unlink(OutFilePath)
	).

/*-------------------------------------------------------------*
 |	Load from the obp file
 *-------------------------------------------------------------*/ 
obp_load_file(CanonSrcPath, BaseFile, FCOpts)
	:-
 	consulted(BaseFile, CanonSrcPath, ObpPath, _, _),
	obp_load_from_path(BaseFile,ObpPath, CanonSrcPath),
	!,
	note_paths(FCOpts, CanonSrcPath, ObpPath, ObpPath).

obp_load_file(CanonSrcPath, BaseFile, FCOpts)
	:-
	(filePlusExt(_, Ext, CanonSrcPath) ->
		(Ext = obp ->
			ObpFile = CanonSrcPath
			;
			fail %% change to exception
		)
		;
		filePlusExt(CanonSrcPath, obp, ObpFile)
	),
	obp_load_from_path(BaseFile,ObpPath, CanonSrcPath),
	!,
	note_paths(FCOpts, CanonSrcPath, ObpPath, ObpPath).

obp_load_from_path(BaseFile,ObpPath, CanonSrcPath)
	:-
	(filePlusExt(NoSuffixPath, _, CanonSrcPath),! ; NoSuffixPath = CanonSrcPath),
	establish_fcg(NoSuffixPath),
	full_obp_load(CanonSrcPath, ObpPath),
	reset_fcg(_).

/*-------------------------------------------------------------*
 |	Load from either the source file or the .obp file,
 |	depending on date/time.
 *-------------------------------------------------------------*/ 
default_load(CanonSrcPath,BaseFile,ObpPath,FCOpts)
	:-
	(comp_file_times( CanonSrcPath, ObpPath) ->
		obp_load_from_path(BaseFile,ObpPath, CanonSrcPath),
		note_paths(FCOpts, CanonSrcPath, ObpPath, ObpPath)
		;
		load_source_object(CanonSrcPath,ObpPath),
		note_paths(FCOpts, CanonSrcPath, ObpPath, CanonSrcPath)
	).

/*-------------------------------------------------------------*
 |	Record source/obp/loaded path information in the 
 |	FCOpts options structure
 *-------------------------------------------------------------*/ 
note_paths(FCOpts, SrcFilePath, ObpPath, LoadedPath)
	:-
	mangle(11, FCOpts, SrcFilePath),		%%  11      - SrcFilePath
	mangle(2, FCOpts, ObpPath),  			%%	2		- ObpPath
	mangle(12, FCOpts, LoadedPath). 		%%	12		- LoadedPath 

/*-------------------------------------------------------------*
 |	Low-level load from .obp file, with check on status
 |	afterwards:
 *-------------------------------------------------------------*/ 
full_obp_load(SrcPath, ObpPath)
	:-
	obp_load(ObpPath,Status),
	obp_load_checkstatus(Status,SrcPath,ObpPath).

/*-----------------------------------------------------------------------*
 |	obp_load_checkstatus/3
 |	obp_load_checkstatus(Status,SPath,OPath)
 |	obp_load_checkstatus(Status,SPath,OPath)
 *-----------------------------------------------------------------------*/
	%% FLOAD_FAIL
obp_load_checkstatus(0,SPath,OPath) 
	:-!,	
		%% obpload_er: "Error loading Prolog obp file %t.\n"
	prolog_system_error(obpload_er, [OPath]),
	attempt_load_source_object(SPath,OPath).

	%% FLOAD_SUCCESS
obp_load_checkstatus(1,SPath,OPath) 
	:-!.

	%% FLOAD_ILLOBP
obp_load_checkstatus(2,SPath,OPath) 
	:-	
		%% old_obp: "%t in old or unrecognized .obp format.\n"
	prolog_system_error(old_obp, [OPath]),
	attempt_load_source_object(SPath,OPath).

/*-----------------------------------------------------------------------*
 | prepend_current_consult_directory/4
 | prepend_current_consult_directory(Drive,PathList,NewDrive,NewPathList)
 | prepend_current_consult_directory(+,+,-,-)
 |
 |	-- fix up incoming drive & pathlist for locating file
 *-----------------------------------------------------------------------*/

	%% no drive or path specified. Use default.
prepend_current_consult_directory('',[],NewDrive,NewPathList) 
	:-!, 		
	get_current_consult_directory(InitNewDrive+InitNewPathList),
	check_default_local(InitNewDrive,InitNewPathList,NewDrive,NewPathList).

	%% not an absolute path (i.e., relative),
	%% but using the same drive:
prepend_current_consult_directory(Drive,PathList,NewDrive,NewPathList) 
	:-
	PathList \= ['' | _],
	!,
	get_current_consult_directory(InitDrive+InitCurrentPath),
	check_default_local(InitDrive,InitCurrentPath,NewDrive,CurrentPath),
	dappend(CurrentPath,PathList,NewPathList).

	%% Pathlist is complete (i.e., absolute):
prepend_current_consult_directory(Drive,PathList,Drive,PathList).

check_default_local('',[],NewDrive,NewPathList)
	:-!,
	get_cwd(NewPath),
	rootPlusPath(InitNewDrive, NewPathList, NewPath),
	(InitNewDrive = root -> NewDrive = '' ; NewDrive = InitNewDrive).

check_default_local(Drive,PathList,Drive,PathList).

/*-----------------------------------------------------------------------*
 |	check_existence/4
 |	check_existence(InPath,ExtList,OutPath,OutExt)
 |	check_existence(+,+,-,-)
 |
 |	- determine whether a (generalized) path is a valid clause source
 |
 |	Valid sources:  
 |		- the user (keyboard) stream;
 |		- a regular file which (by defn) exists;
 |		- the path plus a 'pro' or 'obp' extension is an existing regular file
 *-----------------------------------------------------------------------*/
export check_existence/4.

check_existence(user,_,user,no(extension)) :- !.

check_existence(Path,_,Path,Ext) 
	:-
	exists_file(Path),
	check_for_regular(Path),
	!,
	(filePlusExt(_,Ext,Path) -> true ; Ext=no(extension)).

check_existence(Path,ExtsList,PathPlusSuffix,Suffix) 
	:-
	member(Suffix, ExtsList),
	filePlusExt(Path,Suffix,PathPlusSuffix),
	exists_file(PathPlusSuffix),
	!,
	check_for_regular(PathPlusSuffix).

check_for_regular(Path)
	:-
	file_status(Path,Status),
	(member(type=regular,Status) -> 
		true
		;
		member(type=symbolic_link, Status),
			%% Must give follow_link/4 the correct directory context:
		pathPlusFile(DirPath,_,Path),
		subPath(DirPathSrcList, DirPath),
		follow_link(Path, _, DirPathSrcList, FinalTypeCode),
		fileTypeCode(FinalTypeCode, regular)
	).


/*-----------------------------------------------------------*
 |	Databse structure for recording information about
 |	consulted files:
 |
 |	consulted/5
 |	consulted(File, SrcPath, ObpPath, DbgType, Options)
 |	consulted(+, +, +, +, +)
 |
 |	-records information about loaded files
 |
 |	Asserted in module builtins by consult process.
 |
 |	File	= the unadorned file name (myfile), with any extension
 |			  which was passed in the original description (e.g.,
 |			  foo.pro, foo.obp, foo.oop, etc.), all as an atom
 |	SrcPath = complete path (e.g., /foa/bar/foo.pro,  or foa/bar/foo.oop)
 |            to source code which was (at some point) loaded, and used
 |			  to generate the obp file (if any) in ObpPath; '' otherwise
 |	ObpPath = path (eg, /foa/bar/foo.obp ) where the relevant 
 |			  *.obp file was located, if any; if none such
 |			  was involved,  use '' in this arg; this may
 |			  be either the path the obp file which was loaded,
 |			  or the path to the obp file which was written
 |	DbgType = {normal/debug}; default = normal
 |	Options	  = options structure, or '' if none used
 *-----------------------------------------------------------*/

record_consult(File, FCOpts)
	:-
	arg(2, FCOpts, ObpPath), 
	arg(5,  FCOpts, OrigDir),
	arg(13, FCOpts, DebugType),
	arg(11, FCOpts, SrcFilePath),
 	fin_record_consult(File, SrcFilePath, ObpPath, DebugType, '').

fin_record_consult(File, SrcFilePath, ObpPath, DebugType, Options)
	:-
 	consulted(File, OrigDir, A, B, C),
	!,
 	retract(consulted(File, SrcFilePath, A, B, C)),
 	assert(consulted(File, SrcFilePath, ObpPath, DebugType, Options)).

	%% Not previously recorded:
fin_record_consult(File, SrcFilePath, ObpPath, DebugType, Options)
	:-
 	assert(consulted(File, SrcFilePath, ObpPath, DebugType, Options)).

		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%% CONTROLLING MESSAGES
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*-----------------------------------------------------------------*
 |	consultmessage/1
 |	consultmessage(Ctrl)
 |	consultmessage(+)
 |
 |	- Control whether messages are printed during consulting.
 |
 |	Ctrl is true/false (false = print messages)
 *-----------------------------------------------------------------*/
/*-----------------------------------------------------------------*
 |	set_consult_messages/1
 |	set_consult_messages(Setting),
 |	set_consult_messages(+),
 |
 |	- Change consultmessage/1 setting
 *-----------------------------------------------------------------*/
consultmessage(false).

export set_consult_messages/1.

set_consult_messages(Value)
	:-
	consultmessage(Value),
	!.

set_consult_messages(Value)
	:-
	retract( consultmessage(_) ),
	assert( consultmessage(Value) ).

		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%% LOWEST-LEVEL FILE LOADING
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*-------------------------------------------------------------*
 | For loading source (.pro) files without writing the .obp file:
 *-------------------------------------------------------------*/ 
load_source(Path,Type) :-
	(filePlusExt(NoSuffixPath, _, Path),!; NoSuffixPath = Path),
	establish_fcg(NoSuffixPath),
	obp_push_stop,
	xconsult(Path,NErrs),
	obp_pop,
	reset_fcg(_),
	!.
load_source(_,_) :-
	obp_pop,
		%% ld_src_err: "Internal error in load_source for file %t\n"
	prolog_system_error(ld_src_err, []).

/*-------------------------------------------------------------*
 |	For loading source (.pro) files AND writing the .obp file:
 *-------------------------------------------------------------*/ 
load_source_object(SPath,OPath) :-
	(filePlusExt(NoSuffixPath, _, SPath),!; NoSuffixPath = SPath),
	establish_fcg(NoSuffixPath),
	obp_open(OPath),
	!,
	xconsult(SPath,NErrs),
	obp_close,
	reset_fcg(_),
	(NErrs = 0 ->  
		true
		;  
		(unlink(OPath) ->  
			prolog_system_error(obp_removed,[SPath,OPath])
			;   
			prolog_system_error(obp_not_removed,[SPath,OPath])
		)
	).
load_source_object(SPath,OPath)
	:-
	prolog_system_error(no_open_fil, [OPath]),
	prolog_system_error(no_obp, [OPath]),
	xconsult(SPath,NErrs).

/*-------------------------------------------------------------*
 |	For loading source (.pro) files AND writing the .obp file,
 |	BUT make sure the file exists, and give a warning message:
 *-------------------------------------------------------------*/ 
attempt_load_source_object(SPath,OPath) :-
	exists_file(SPath),
	prolog_system_error(atmpt_cmp, [SPath]),
	load_source_object(SPath,OPath).

attempt_load_source_object(SPath,OPath) :-
	prolog_system_error(ld_fail, [SPath]).

		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%% LOWEST-LEVEL SLIB LOADING
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*

load_slib/1

Description

load_slib(File) is true.

Procedurally, load_slib(File) is executed as follows:

a) The shared c-prolog library File is loaded.

b) The goal succeeds.

Templates and modes

load_slib(+File)

Errors

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

*/

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


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% FILE CLAUSE GROUP MANIPULATION
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*-------------------------------------------------------------*
 *-------------------------------------------------------------*/ 
load_canon_reconsult(1,CG) 
	:- !,
	(file_clause_groups(true) ->
		massively_abolish_clausegroup(CG)
		;
		true
	).

load_canon_reconsult(0,_).

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
 *-------------------------------------------------------------*/ 

:- dynamic(file_clause_groups/1).

file_clause_groups(true).

set_file_clause_groups(Value)
	:-
	file_clause_groups(Value),
	!.

set_file_clause_groups(Value)
	:-
	retract(file_clause_groups(_)),
	assert(file_clause_groups(Value)).

/*-------------------------------------------------------------*
 *-------------------------------------------------------------*/ 
establish_fcg(NoSuffixPath)
	:-
	get_fcg(NoSuffixPath,CG),
	push_clausegroup(CG),
	get_reconsult_flag(RFlag),
	load_canon_reconsult(RFlag,CG).

reset_fcg(_)
	:-
	pop_clausegroup(_).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% HOOKS FOR INVOKING TRANSFORMERS
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	/*--------------------------------------------------*
	 |	These all assume that the ALS Prolog development
	 |	shell is running, so that the library setup
	 |	in blt_shl.pro has been carried out.
	 |
	 |	Location of generated files is controlled
	 |	by genFileLocn/3.  The default is put generated
	 |	files with their various source files.
	 *--------------------------------------------------*/

extract_opts(FCOpts, [quiet(Value)])
	:-
	arg(7, FCOpts, Value). 	%%	7		- Quiet 

	/*--------------------------------------------------*
	 |	src2src_inv/3
	 |	src2src_inv(Ext, CanonSrcPath, OutProPath)
	 |	src2src_inv(+, +, +)
	 |
	 |	This is the direct hook between the consult
	 |	machinery and the individual source to source
	 |	transformers.  This 
	 *--------------------------------------------------*/

		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%% GENERATED FILE LOCATION CONTROL (INCLUDING OBP)
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*-----------------------------------------------------------------------*
 *-----------------------------------------------------------------------*/
:- dynamic(genFileLocn/3).

outFilePath(GenExt, CanonSrcPath, BaseFile, OutFilePath)
	:-
	genFileLocn(CanonSrcPath, OutDir, OutDisk),
	!,
	fin_o_f_path(OutDir, OutDisk, GenExt, CanonSrcPath, BaseFile, OutFilePath).

outFilePath(GenExt, CanonSrcPath, BaseFile, OutFilePath)
	:-
	fin_o_f_path('', '', GenExt, CanonSrcPath, BaseFile, OutFilePath).

/*-----------------------------------------------------------------------*
 *-----------------------------------------------------------------------*/
fin_o_f_path(subdir(cur), OutDisk, GenExt, CanonSrcPath, 
				BaseFile, OutFilePath)
	:-!,
	filePlusExt(BaseFile, GenExt, OutFile),
	get_cwd(CurDir),
	extendPath(CurDir,OutFile,OutFilePath).

fin_o_f_path(subdir(arch,Where), OutDisk, GenExt, CanonSrcPath, 
				BaseFile, OutFilePath)
	:-!,
	filePlusExt(BaseFile, GenExt, OutFile),
	rootPathFile(OrigDisk,OrigPathList,_,CanonSrcPath),
	sys_env(OS,MinorOS,Proc),
	SubDir = MinorOS,
	(Where = src ->
		(GenExt = obp ->
			append(OrigPathList, [SubDir], NewPathList)
			;
			NewPathList = OrigPathList
		)
		;
				%% take Where to be = cur (subdir of current):
		get_cwd(CurDir),
		rootPlusPath(OrigDisk,OutDirList,CurDir),
		append(OutDirList, [SubDir], NewPathList)
	),
	rootPlusPath(OrigDisk, NewPathList, OutDir),
	(exists_file(OutDir) -> true ; make_subdir(OutDir) ),
	rootPathFile(OrigDisk, NewPathList, OutFile, OutFilePath).

fin_o_f_path(OutDir, OutDisk, GenExt, CanonSrcPath, BaseFile, OutFilePath)
	:-
	is_absolute_pathname(OutDir),
	!,
	filePlusExt(BaseFile, GenExt, OutFile),
	rootPathFile(OutDisk, OutDir, OutFilePath).

	%% OutDir is a relative path; paste onto CanonSrc..:
fin_o_f_path(OutDir, OutDisk, GenExt, CanonSrcPath, BaseFile, OutFilePath)
	:-
	filePlusExt(BaseFile, GenExt, OutFile),
	rootPathFile(OrigDisk,OrigPathList,_,CanonSrcPath),
	subPath(OutDirList, OutDir),
	append(OrigPathList, OutDirList, NewPathList),
	rootPathFile(OrigDisk, NewPathList, OutFile, OutFilePath).

/*!----------------------------------------------------------------------*
 |	generated_with_src/0.
 |	generated_with_src
 |	generated_with_src
 |
 |	-	store generated files in same directory with source files
 *!----------------------------------------------------------------------*/
	%%  genFileLocn(CanonSrcPath, OutDir, OutDisk),

export generated_with_src/0.
generated_with_src 
	:-
	abolish(genFileLocn,3),
	dynamic(genFileLocn/3).

/*!----------------------------------------------------------------------*
 |	generated_in_cur/0.
 |	generated_in_cur
 |	generated_in_cur
 |
 |	-	store generated files in the current directory
 *!----------------------------------------------------------------------*/
export generated_in_cur/0.
generated_in_cur 
	:-
	abolish(genFileLocn,3),
	asserta(genFileLocn(_,subdir(cur), '')).

/*!----------------------------------------------------------------------*
 |	generated_in_locn/1
 |	generated_in_locn(DirPath)
 |	generated_in_locn(+)
 |
 |	-	store generated files in the directory DirPath
 *!----------------------------------------------------------------------*/
export generated_in_locn/1.
generated_in_locn(DirPath) 
	:-
	rootPlusPath(Disk, PathList, DirPath),
	subPath(PathList, Path),
	abolish(genFileLocn,3),
	asserta(genFileLocn(_,Path, Disk)).

/*!----------------------------------------------------------------------*
 |	generated_in_arch/1
 |	generated_in_arch(Which)
 |	generated_in_arch(+)
 |
 |	-	store generated files in an architcture subdirectory 
 |
 |	Which = {src/cur}; any input \= src is interpred as cur.
 *!----------------------------------------------------------------------*/

export generated_in_arch/1.
generated_in_arch(Which)
	:-
	abolish(genFileLocn,3),
	(Which = src -> Place = src ; Place = cur),
	asserta(genFileLocn(_,subdir(arch,Place), '')).

export setup_tcltk/0.
setup_tcltk
	:-
	setup_tcltk(shl_tcli).

setup_tcltk(Interp)
	:-
	consult('tclintf.psl'),
	tcl_new(Interp),
	tcl_eval(Interp, 'set TclLib [info library]', TclLibPath),
	sprintf(atom(Cmd0), 'source "%t/init.tcl"', [TclLibPath]),
	tcl_eval(Interp, Cmd0, _),

	tcl_eval(Interp, 'set TkLib $tk_library', TkLibPath),
	sprintf(atom(Cmd1), 'source "%t/tk.tcl"', [TkLibPath]),
	tcl_eval(Interp, Cmd1, _).

endmod.		%% blt_cslt.pro: Consult 
