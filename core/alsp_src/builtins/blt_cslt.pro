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
	cslt_init(CCD,MsgValue,ReconFlg, FCGValue),
	consult_global_options(Options, COpts),
	consult_files(What, COpts),
	cslt_cleanup(CCD,MsgValue,ReconFlg, FCGValue).

consult_files([], _) 
	:-!.

consult_files([File | Files], COpts) 
	:- !,
	consult_files(File, COpts),
	consult_files(Files, COpts).

consult_files(File, COpts) 
	:- 
	local_consult_options(File, BaseFile, COpts, FCOpts),
	do_consult(BaseFile, FCOpts).

cslt_init(CurDrive+CurDirPathList,MessageValue,ReconFlg, FCGValue)
	:-
	get_cwd(CurDir),
	subPath(CurDirPathList, CurDir),
	get_current_drive(InitCurDrive),
	(InitCurDrive = root -> CurDrive = '' ; CurDrive = InitCurDrive),
    set_current_consult_directory(CurDrive+CurDirPathList),	
	consultmessage(MessageValue),
	file_clause_groups(FCGValue),
	get_reconsult_flag(ReconFlg).

cslt_cleanup(CCD,MessageValue,ReconFlg, FCGValue)
	:-
    set_current_consult_directory(CCD),
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
	consult_msg(Quiet, BaseFile, start_consult, FCOpts),
	get_current_consult_directory(OldCCD),
	catch( exec_consult(BaseFile, FCOpts),
			Ball,
			( set_current_consult_directory(OldCCD),
				set_reconsult_flag(1),
				throw(Ball)
			) ),

	record_consult(BaseFile, FCOpts),
	cleanup_cslt,
	consult_msg(Quiet, BaseFile, end_consult, FCOpts).

		%% true = "be quiet"
consult_msg(true, BaseFile, FCOpts, MsgCode) :-!.

		%% false = "write messages"
consult_msg(false, BaseFile, MsgCode, FCOpts)
	:-
	consult_msg_args(MsgCode, FCOpts, Args),
	prolog_system_warning(MsgCode, Args),
	(BaseFile = user -> als_advise('\n') ; true).

consult_msg_args(start_consult, FCOpts, [OrigFileDesc])
	:-
	arg(10, FCOpts, OrigFileDesc).

consult_msg_args(end_consult, FCOpts, [LoadedPath])
	:-
	arg(12, FCOpts, LoadedPath).

cleanup_cslt.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% MAIN GUTS OF CONSULT 
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*-------------------------------------------------------------*
 | exec_consult/2
 | exec_consult(BaseFile, FileConsultOpts)
 | exec_consult(+, +)
 |
 |	- decides whether consult is from user or otherwise
 *-------------------------------------------------------------*/ 
exec_consult(user, FCOpts)
	:-!,
	load_source(user,_).

exec_consult(BaseFile, FCOpts)
	:-
	arg(1, FCOpts, Nature),
	arg(5, FCOpts, Drive+PathList),		%%	5		- OrigDir 
	arg(6, FCOpts, SrcExt),
	exec_consult(PathList, BaseFile, Nature, SrcExt, FCOpts).

/*-------------------------------------------------------------*
 |	exec_consult/5
 |	exec_consult(PathList, BaseFile, Nature, SrcExt, FCOpts).
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

		%% Incoming pathlist is absolute:
exec_consult(['' | RestPathList], BaseFile, Nature, SrcExt, FCOpts)
	:-!,
	cont_consult(SrcExt, Nature, Drive+['' | PathList], BaseFile, FCOpts).

		%% Incoming pathlist is NOT absolute:
		%% Try the local search path list:
exec_consult(PathList, BaseFile, Nature, SrcExt, FCOpts)
	:-
	arg(9, FCOpts, LocalSearchPathLists),		%%	9		- SearchPath 
	directory_self(SelfDir),
	subPath(SelfDirPathList, SelfDir),
	member(SrcPathList, [''+SelfDirPathList | LocalSearchPathLists]), 
	cont_consult(SrcExt, Nature, SrcPathList, BaseFile, FCOpts).

		%% Incoming pathlist is NOT absolute:
		%% Try the gobal search path list:
exec_consult(PathList, BaseFile, Nature, SrcExt, FCOpts)
	:-
	searchdir(SearchDir), 
	rootPlusPath(Drive,SDPathList,SearchDir),
	cont_consult(SrcExt, Nature, Drive+SDPathList, BaseFile, FCOpts).

		%% Can't find file
		%% Throw exception:
exec_consult(_, BaseFile, Nature, SrcExt, FCOpts)
	:-
	arg(10, FCOpts, OrigFileDesc), 		%%	10		- OrigFileDesc 
	existence_error(file,OrigFileDesc,OrigFileDesc).

/*-------------------------------------------------------------*
 |	- Determine real source file paht, and setup global
 |	  information for this specific consult;
 |	- Do the loading with load_from_file/6
 |	- Restore appropriate global information.
 *-------------------------------------------------------------*/ 
cont_consult(SrcExt, Nature, Drive+PathList, BaseFile, FCOpts)
	:-
	prepend_current_consult_directory(Drive,PathList,NewDrive,NewPathList),
	(SrcExt = no(extension) ->
		SrcFileName = BaseFile
		;
		filePlusExt(BaseFile, SrcExt, SrcFileName)
	),
	rootPathFile(NewDrive,NewPathList,SrcFileName,SrcFilePath),
	check_existence(SrcFilePath, ExistingSrcFilePath, WorkingExt),
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

	load_from_file(SrcExt,Nature,BaseFile,CanonSrcPath,WorkingExt,FCOpts),

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

		%% SrcExt = pro => force (attempted) loading from .pro:
		%% .pro extension + arbitrary Nature handled here:
load_from_file(pro,_,BaseFile,CanonSrcPath,_,FCOpts)
	:-!,
	source_ld_file(BaseFile,CanonSrcPath,FCOpts).

load_from_file(no(extension),_,BaseFile,CanonSrcPath,no(extension),FCOpts)
	:-!,
	source_ld_file(BaseFile,CanonSrcPath,FCOpts).

		%% No source extension + Nature = source =>
		%% force (attempted) loading from .pro:
load_from_file(no(extension),source,BaseFile,CanonSrcPath,WkExt,FCOpts)
	:-!,
	(WkExt = pro ->
		load_from_file(pro,source,BaseFile,CanonSrcPath,WkExt,FCOpts)
		;
		load_from_other(WkExt,source,BaseFile,CanonSrcPath,FCOpts)
	).

		%% No source extension + Nature = default =>
		%% compare .pro, .obp extensions by date/time:
load_from_file(no(extension),default,BaseFile,CanonSrcPath,WkExt,FCOpts)
	:-!,
	(WkExt = pro ->
		default_load(BaseFile,CanonSrcPath,FCOpts)
		;
		load_from_other(WkExt,default,BaseFile,CanonSrcPath,FCOpts)
	).

		%% SrcExt \= obp,  & SrcExt \= pro, & SrcExt \= no(extension), and
		%% Nature \= obp, so Nature = source or Nature = default:
		%% This is a hook for automatic application of source to
		%% source transforms such as typecomp, objectPro, macro, etc.
load_from_file(SrcExt,Nature,BaseFile,CanonSrcPath,WkExt,FCOpts)
	:-!,
	load_from_other(SrcExt,Nature,BaseFile,CanonSrcPath,FCOpts).

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
 |	Load from the source file
 *-------------------------------------------------------------*/ 
source_ld_file(BaseFile,CanonSrcPath,FCOpts)
	:-
 	consulted(BaseFile, CanonSrcPath, ObpPath, _, _),
	load_source(CanonSrcPath, ObpPath),
	!,
	note_paths(FCOpts, CanonSrcPath, ObpPath, CanonSrcPath).

source_ld_file(BaseFile,CanonSrcPath,FCOpts)
	:-
	(filePlusExt(PureFilePath, _, CanonSrcPath) ->
		true
		;
		PureFilePath = CanonSrcPath
	),
	obpPath(PureFilePath, ObpPath),
	load_source(CanonSrcPath, ObpPath),
	!,
	note_paths(FCOpts, CanonSrcPath, ObpPath, CanonSrcPath).

/*-------------------------------------------------------------*
 |	Load from either the source file or the .obp file,
 |	depending on date/time.
 *-------------------------------------------------------------*/ 
default_load(BaseFile,CanonSrcPath,FCOpts)
	:-
 	(consulted(BaseFile, CanonSrcPath, ObpPath, _, _)
		;
		filePlusExt(NosuffixCanonSrcPath, _, CanonSrcPath),
		obpPath(NosuffixCanonSrcPath, ObpPath)
	),
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

/*-----------------------------------------------------------------------*
 |	check_existence/3
 |	check_existence(InPath,OutPath,OutExt)
 |	check_existence(+,-,-)
 |
 |	- determine whether a (generalized) path is a valid clause source
 |
 |	Valid sources:  
 |		- the user (keyboard) stream;
 |		- a regular file which (by defn) exists;
 |		- the path plus a 'pro' or 'obp' extension is an existing regular file
 *-----------------------------------------------------------------------*/
check_existence(user,user,no(extension)) :- !.

check_existence(Path,Path,Ext) 
	:-
	exists_file(Path),
	check_for_regular(Path),
	!,
	(filePlusExt(_,Ext,Path) -> true ; Ext=no(extension)).

check_existence(Path,PathPlusSuffix,Suffix) 
	:-
%	(Suffix=pro;Suffix=obp),
	(s2s_ext(Suffix) ; Suffix=pro),
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
		follow_link(Path,_,FileCode),
		fileTypeCode(FileTypeCode, regular)
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
		%% OBP FILE LOCATION CONTROL
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*!----------------------------------------------------------------------*
 |	obp_with_pro/0.
 |	obp_with_pro
 |	obp_with_pro
 |
 |	-	store obp files in same directory with source pro files
 *!----------------------------------------------------------------------*/
export obp_with_pro/0.
obp_with_pro 
	:-
	abolish(obpLocation,2),
	dynamic(obpLocation/2).

/*!----------------------------------------------------------------------*
 |	obp_in_cur/0.
 |	obp_in_cur
 |	obp_in_cur
 |
 |	-	store obp files in the current directory
 *!----------------------------------------------------------------------*/
export obp_in_cur/0.
obp_in_cur 
	:-
	abolish(obpLocation,2),
	directory_self(Self),
	asserta(obpLocation(_,Self)).

/*!----------------------------------------------------------------------*
 |	obp_in_locn/1
 |	obp_in_locn(DirPath)
 |	obp_in_locn(+)
 |
 |	-	store obp files in the directory DirPath
 *!----------------------------------------------------------------------*/
export obp_in_locn/1.
obp_in_locn(DirPath) 
	:-
	abolish(obpLocation,2),
	asserta(obpLocation(_,DirPath)).

/*-----------------------------------------------------------------------*
 |	obpPath/2
 |	obpPath(Path, ObpPath)
 |	obpPath(+, -)
 |
 |	- Determine where an obp file was/should be stored
 *-----------------------------------------------------------------------*/

obpPath(Path,ObpPath)
	:-
	pathPlusFile(_,File,Path),
 	consulted(File, OrigDir, ObpPath, DbgType, Options),
	!.

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

:- dynamic(file_clause_group/2).

/*-------------------------------------------------------------*
 *-------------------------------------------------------------*/ 
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

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% HOOKS FOR INVOKING SOURCE 2 SOURCE TRANSFORMERS
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	/*--------------------------------------------------*
	 |	These all assume that the ALS Prolog development
	 |	shell is running, so that the library setup
	 |	in blt_shl.pro has been carried out.
	 |
	 |	Location of generated .pro files is controlled
	 |	by src2srcLocation/3, which is analogous to
	 |	obpLocation/2. The default is put generated
	 |	.pro files with their various source files.
	 *--------------------------------------------------*/

load_from_other(Ext,Nature,BaseFile,CanonSrcPath,FCOpts)
	:-
	pathPlusFile(PurePath, File, CanonSrcPath),
	src2srcPath(Ext, PurePath, BaseFile, OutProPath),
	extract_opts(FCOpts, Options),
	src2src_inv(Ext, BaseFile, CanonSrcPath, OutProPath, Options),
	default_load(BaseFile,OutProPath,FCOpts).

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

		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%% SRC2SRC FILE LOCATION CONTROL
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*!----------------------------------------------------------------------*
 |	src2src_with_pro/1.
 |	src2src_with_pro(Ext)
 |	src2src_with_pro(+)
 |
 |	-	store generated pro files in same directory with source Ext files
 *!----------------------------------------------------------------------*/
export src2src_with_pro/1.
src2src_with_pro(Ext)
	:-
	src2srcLocation(Ext, _, _),
	!,
	retract_all([src2srcLocation(Ext, _, _)]).
src2src_with_pro(_).

/*!----------------------------------------------------------------------*
 |	src2src_in_cur/1.
 |	src2src_in_cur(Ext)
 |	src2src_in_cur(+)
 |
 |	-	store src2src files generated pro files in the current directory
 *!----------------------------------------------------------------------*/
export src2src_in_cur/1.
src2src_in_cur(Ext)
	:-
	(src2srcLocation(Ext, _, _) ->
		retract_all([src2srcLocation(Ext, _, _)]) ; true),
	directory_self(Self),
	asserta(src2srcLocation(Ext,_,Self)).

/*!----------------------------------------------------------------------*
 |	src2src_in_locn/2
 |	src2src_in_locn(Ext, DirPath)
 |	src2src_in_locn(+,+)
 |
 |	-	store generated src2src pro files for Ext files in directory DirPath
 *!----------------------------------------------------------------------*/
export src2src_in_locn/2.
src2src_in_locn(Ext, DirPath) 
	:-
	(src2srcLocation(Ext, _, _) ->
		retract_all([src2srcLocation(Ext, _, _)]) ; true),
	asserta(src2srcLocation(Ext,_,DirPath)).

/*-----------------------------------------------------------------------*
 |	src2srcPath/4
 |	src2srcPath(Ext, InPath, File, OutProPath)
 |	src2srcPath(+, +, +, -)
 |
 |	- Determine where a generated pro file for an Ext file was/should be stored
 |
 |	Ext is the source extension (typ, oop, etc.) 
 |	InPath is a pure path to a directory; it is absolute
 |	File is pure base file name (no extension)
 |	OutProPath is a complete path to a .pro file
 *-----------------------------------------------------------------------*/
src2srcPath(Ext, InPath, File, OutProPath)
	:-
	src2srcDir(Ext, InPath, OutDirPath),
	filePlusExt(File, pro, ProFile),
	pathPlusFile(OutDirPath, ProFile, OutProPath).

/*-----------------------------------------------------------------------*
 |	src2srcPath/3
 |	src2srcPath(Ext, InPath, OutProPath)
 |	src2srcPath(+, +, -)
 |
 |	- Determine where a generated pro file for an Ext file was/should be stored
 |
 |	Ext is the source extension (typ, oop, etc.) 
 |	InPath is a pure path to a directory; it is absolute
 |	File is pure base file name (no extension)
 |	OutProPath is a complete path to a .pro file
 *-----------------------------------------------------------------------*/

src2srcDir(Ext,PurePath,Dir)
	:-
	src2srcLocation(Ext,PurePath,InitDir),
	src2srcFullDir(Ext,PurePath,InitDir,Dir).

	%% Default: Keep with source:
src2srcDir(Ext,PurePath,PurePath).

	%% Dir is an absolute path:
src2srcFullDir(Ext,PurePath,Dir,Dir)
	:-
	is_absolute_pathname(Dir),
	!.

	%% The Dir for src2src files is non-absolute:
src2srcFullDir(Ext,PurePath,Dir,OutDir)
	:-
	extendPath(PurePath, Dir, OutDir).

endmod.		%% blt_cslt.pro: Consult 
