/*=====================================================================
 | 		blt_pckg.pro
 |	Copyright (c) 1990-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		Packaging predicates to create object modules
 |		in COFF file format.
 |
 | Author : Ilyas Cicekli - Version A
 | Date   : 6/1/1990 
 | Modification History :
 |		  3/3/93 - P.Raman -- write raw data out to a file
 |
 | Author : Kevin Buettner - Version B
 | Date:    06/01/93
 *=====================================================================*/

module builtins.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%   Version B
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export attach_image/2.
export attach_image/1.

export save_image/2.
export save_image/1.

export pckg_init/0.

export save_package/5.
export save_base_package/1.

/*!--------------------------------------------------------------*
 |	save_image/2
 |	save_image(ImageName, Options)
 |	save_image(+, +)
 |
 |	- Create saved code state merged with current image, with options
 |
 |	Creates a saved code state and merges it with the current 
 |	(executing) image to make a new extended executable image,
 |	named ImageName (per Options); Applies Options.
 *---------------------------------------------------------------*/

fix_image_name(Name, FixedName) :-
	als_system(SystemList),
	dmember(os = OS, SystemList),
	(OS = mswin32 ->
		((sub_atom(Name, _, _, 0, '.exe')
		 ; sub_atom(Name, _, _, 0, '.EXE')) ->
			FixedName = Name
			;
			atom_concat(Name, '.exe', FixedName)
		)
		;
		FixedName = Name
	).

save_image(ImageName) :-
	save_image(ImageName, []).
	
save_image(ImageName, Options) :-
	get_current_image(ThisImage),
	fix_image_name(ImageName, FixedImageName),
	process_image_options(Options, FixedImageName, OptionedName),
	pbi_copy_file(ThisImage, OptionedName),
	(dmember(preserve(all), Options) -> DevelopFlag=develop ; DevelopFlag=production),
	attach_image(OptionedName, DevelopFlag).
	
attach_image(ImageName, DevelopFlag)
	:-
		%% Save initial state of '$start' and '$initialize'
		%% in case there is an error in processing options,
		%% so that we need to restore them:
	builtins:clause('$initialize',OldInit),	
	builtins:clause('$start',OldStart),	
	catch(attach_image0(ImageName,DevelopFlag),
			se(Error),
			restore_si(Error, OldInit, OldStart) ).

/*
save_image0(ImageName, Options)
	:-
	process_image_options(Options, ImageName, FinalImageName),
	(dmember(preserve(all), Options) ->
		save_image_develop(FinalImageName)
		;
		save_image(FinalImageName)
	).
*/

/*!--------------------------------------------------------------*
 |	save_image/1
 |	save_image(ImageName)
 |	save_image(+)
 |
 |	- Create saved code state merged with current image 
 |
 |	Creates a saved code state and merges it with the current 
 |	(executing) image to make a new extended executable image,
 |	named ImageName.
 *---------------------------------------------------------------*/

:- dynamic(alsdev_ini_path/1).
:- dynamic(save_clinfo/1).

attach_image0(NewImageName, DevelopFlag)
	:-
	adjust_sys_search_for_save(ALSDIR, OrigALSDIR),

	command_line(CmdLine),
	abolish(command_line,1),
		%% From alsdev::
	(alsdev_ini_path(ADIP) ->
		abolish(alsdev_ini_path,1) ; ADIP=no_path),
	(save_clinfo(CLInfo) ->
		abolish(save_clinfo,1) ; CLInfo = no_info),

	(DevelopFlag = production ->
			%% Must get rid of these so the image can create
			%% them as it starts up; but don't throw them away...
		(bagof(searchdir(SD0), searchdir(SD0), SDList0) -> true ; SDList0 = []),
		abolish(searchdir,1),
		builtins:dynamic(searchdir/1),

		(bagof(als_lib_lcn(SD1), als_lib_lcn(SD1), SDList1) -> true ; SDList1 = []),
		abolish(als_lib_lcn,1),
		builtins:dynamic(als_lib_lcn/1),


		(bagof( file_clause_group(Path2,CG2), 
				file_clause_group(Path2,CG2), 
				SDList2) 
	  	-> true ; SDList2 = []),
		abolish(file_clause_group,2),
		dynamic(file_clause_group/2),

		(bagof( consulted(A,B,C,D,E), 
				consulted(A,B,C,D,E), 
				SDList3) 
	  	-> true ; SDList3 = []),
		abolish(consulted,5),
		dynamic(consulted/5)
		;
		true   	%% DevelopFlag = develop
	),

	% attach the state
	attach_state(NewImageName),
	
	(DevelopFlag = production ->
			%% Re-install the search dir info:
		assert(sys_searchdir(OrigALSDIR)),
		laa(SDList0),
		laa(SDList1),
		laa(SDList2),
		laa(SDList3)
		;
		true   	%% DevelopFlag = develop
	),

%	(ADIP=no_path -> true ; assert(alsdev_ini_path(ADIP))),
%	(CLInfo=no_info -> true ; assert(save_clinfo(CLInfo))),
	assert(command_line(CmdLine)).

save_image_develop(NewImageName)
	:-
	adjust_sys_search_for_save(ALSDIR, OrigALSDIR),

	command_line(CmdLine),
	abolish(command_line,1),

		%% Save the new image based on OS
	als_system(SystemList),
	dmember(os = OS, SystemList),
	dmember(os_variation = OSVariation, SystemList),
	image_type(OS, OSVariation, Type),
	(OS = mswin32 ->
		(filePlusExt(_,_,NewImageName) ->
			IMN = NewImageName
			;
			filePlusExt(NewImageName,exe,IMN)
		)
		;
		IMN = NewImageName
	),
	save_image_type(Type, IMN, OrigALSDIR),

	assert(command_line(CmdLine)).


	%%----------------------
	%% This is really just assert_all from the library, but it
	%% needs to be written here since when it is run, the libary
	%% is inaccessible:

laa([]).
laa([Item | Rest])
	:-
	builtins:assert(Item),
	laa(Rest).

	% Must save away the path to the original ALSDIR for
	% possible use in subsequent save_image calls, since
	% when the system starts up after a saved image,
	% ALSDIR is set to the path to the directory in which
	% the image resides:

:- dynamic(orig_sys_searchdir/1).

adjust_sys_search_for_save(ALSDIR,OrigALSDIR)
	:-
	sys_searchdir(ALSDIR),
	(orig_sys_searchdir(OrigALSDIR) ->
		true
		;
		OrigALSDIR = ALSDIR,
		assert(orig_sys_searchdir(OrigALSDIR))
	),
		%% Must get rid of these so the image can create
		%% them as it starts up; but don't throw them away...
	abolish(sys_searchdir,1),
	builtins:dynamic(sys_searchdir/1).


/*!--------------------------------------------------------------*
 |	save_state/1.
 |	save_state(FileName)
 |	save_state(+)
 |	
 |	Writes (saves) an version of the current image's code state
 |	(including global variables) to the file FileName.
 *---------------------------------------------------------------*/
save_state(FileName) 
	:-
	get_shell_level(CurLev),
	set_shell_level(0),
	package_global_variables,	%% create initialization pred
	set_shell_level(CurLev),
		%% Defined at C level:
	save_state_to_file(FileName).

/*!--------------------------------------------------------------*
 *---------------------------------------------------------------*/
attach_state(NewImageName)
	:-
	get_shell_level(CurLev),
	set_shell_level(0),
	package_global_variables,	%% create initialization pred
	set_shell_level(CurLev),
		%% Defined at C level:
	attach_state_to_file(NewImageName).

save_image_with_state(NewImageName)
	:-
	get_shell_level(CurLev),
	set_shell_level(0),
	package_global_variables,	%% create initialization pred
	set_shell_level(CurLev),
		%% Defined at C level:
	save_image_with_state_to_file(NewImageName).
	
/*-------------------------------------------------------------------*
 |	pckg_init/0
 |	pckg_init 
 |	pckg_init 
 |
 |	This goal must be run as a start goal when the package is loaded.
 *-------------------------------------------------------------------*/
pckg_init 
	:-
	setInterruptVector('$interrupt'),
	initialize_global_variables,
	sio_pckg_init.


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%% MAC OS RESOURCE PACKAGING %%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*!--------------------------------------------------------------*
 |	save_package/5.
 |	save_package(NewName, OBPList, LoadList, Init, Start)
 |	save_package(+,+,+,+,+)
 |  
 |	- create Mac applications with parameters
 |
 |  Creates a new application or MPW Tool which contains all
 |  the OBP files in OBPList.  When the new application is 
 |  launched it will load the files in LoadList and then
 |  call Init and Start.
 *-------------------------------------------------------------------*/

save_package(NewName, OBPList, LoadList, Init, Start) :-
	save_app_with_obp(NewName, OBPList, LoadList, Init, Start).

base_obp_list([
    ':alsdir:builtins:blt_als.obp',
    ':alsdir:builtins:blt_atom.obp',
    ':alsdir:builtins:blt_brk.obp',
    ':alsdir:builtins:blt_cslt.obp',
    ':alsdir:builtins:blt_ctl.obp',
    ':alsdir:builtins:blt_db.obp',
    ':alsdir:builtins:blt_evt.obp',
    ':alsdir:builtins:blt_flgs.obp',
    ':alsdir:builtins:blt_frez.obp',
    ':alsdir:builtins:blt_io.obp',
    ':alsdir:builtins:blt_msg.obp',
    ':alsdir:builtins:blt_misc.obp',
    ':alsdir:builtins:blt_pckg.obp',
    ':alsdir:builtins:blt_shl.obp',
    ':alsdir:builtins:blt_shlr.obp',
    ':alsdir:builtins:blt_std.obp',
    ':alsdir:builtins:blt_stk.obp',
    ':alsdir:builtins:blt_sys.obp',
    ':alsdir:builtins:blt_term.obp',
    ':alsdir:builtins:builtins.obp',
    ':alsdir:builtins:cutils.obp',
    ':alsdir:builtins:dcgs.obp',
    ':alsdir:builtins:filepath.obp',
    ':alsdir:builtins:fsmac.obp',
    ':alsdir:builtins:fs_cmn.obp',
    ':alsdir:builtins:simplio.obp',
    ':alsdir:builtins:sio.obp',
    ':alsdir:builtins:sio_d10.obp',
    ':alsdir:builtins:sio_rt.obp',
    ':alsdir:builtins:sio_wt.obp',
    ':alsdir:builtins:xconsult.obp',  
    ':alsdir:builtins:debugger.obp'
  ]).

/*!--------------------------------------------------------------*
 |	save_base_package/1.
 |	save_base_package(NewName)
 |	save_base_package(+)
 |	
 |	- copy Mac application with builtins in resources
 |
 |	Creates a copy of the application with all the builtins
 |  stored as resources.
 *---------------------------------------------------------------*/
save_base_package(NewName) :-
	add_lib_qual(['debugger'],builtins,QualFilesList),
	force_libload_all(QualFilesList),
	base_obp_list(BList),
	save_app_with_obp(NewName, BList, [], '', '').

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%% END - MAC OS RESOURCE PACKAGING %%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*---------------------------------------------------------------*
 | mics_cmd_fmt/1
 | mics_cmd_fmt(MicsCmdFmt),
 | mics_cmd_fmt(-),
 *---------------------------------------------------------------*/
	%% Note the djgpp2 uses the default:
mics_cmd_fmt('go32 %sals-mics %s%s %s %s')
	:-
	als_system(SystemList),
	dmember(os_variation=djgpp, SystemList),
	!.
mics_cmd_fmt('%sals-mics %s%s %s %s').

	
restore_si(se(Error), OldInit, OldStart)
	:-
	prolog_system_error(bad_pack_opt, [Error]),

	builtins:abolish('$initialize',0),
	builtins:assert( ('$initialize' :- OldInit) ),

	builtins:abolish('$start',0),
	builtins:assertz( ('$start' :- OldStart) ).

/*---------------------------------------------------------------*
 |	process_image_options/3
 |	process_image_options(Options, OrigImageName, FinalImageName)
 |	process_image_options(+, +, _)
 *---------------------------------------------------------------*/

process_image_options([], ImageName, ImageName)
	:-!.

process_image_options([tag_name | Options], CurImageName, FinalImageName)
	:-!,
	als_system(SysList),
	dmember(os_variation=Suffix, SysList),
	sprintf(atom(NextImageName), '%t-%t', [CurImageName, Suffix]),
	process_image_options(Options, NextImageName, FinalImageName).

process_image_options([Option | Options], CurImageName, FinalImageName)
	:-
	process_image_option(Option),
	process_image_options(Options, CurImageName, FinalImageName).

/*---------------------------------------------------------------*
 |	process_image_option/1
 |	process_image_option(Option)
 |	process_image_option(+)
 *---------------------------------------------------------------*/
process_image_option(preserve(all))
	:-!.

process_image_option(init_goals(NewGoals))
	:-!,
	builtins:clause('$initialize',OldGoals),
	builtins:abolish('$initialize',0),
	builtins:assert( ('$initialize'
						:- OldGoals, user:NewGoals) ).

process_image_option(start_goal(G))
	:-!,
	builtins:abolish('$start',0),
	builtins:assertz( ('$start' :- user:G) ).

process_image_option(libload(true))
	:-!,
	force_libload_all.

process_image_option(libload(false))
	:-!.

process_image_option(select_lib(FilesList)) 
	:-!,
	add_lib_qual(FilesList,library,QualFilesList),
	force_libload_all(QualFilesList).

process_image_option(select_lib(Library,FilesList)) 
	:-!,
	add_lib_qual(FilesList,Library,QualFilesList),
	force_libload_all(QualFilesList).

%% FIXME:  When encountering an unknown option, we should give an error
%%	message and attempt to restore the previous state of $start/0 and
%%	$initialize/0.  Also after successfully saving the image, we
%%	should restore $start/0 and $initialize/0.

process_image_option(Unknown) 
	:-
%	als_advise('Ignoring unknown save image option \`%t\'\n', [Unknown]).
	throw(se(Unknown)).

/*---------------------------------------------------------------*
 |	add_lib_qual/3
 |	add_lib_qual(FilesList, Library, QualFilesList)
 |	add_lib_qual(+,+,-)
 *---------------------------------------------------------------*/
add_lib_qual([],_,[]) :-!.
add_lib_qual([File | FilesList],Library,[QualFile | QualFilesList])
	:-
	extendPath(Library,File,QualFile),
	add_lib_qual(FilesList,Library,QualFilesList).
add_lib_qual(File,Library,[QualFile])
	:-
	atom(File),
	extendPath(Library,File,QualFile).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% END VERSION B %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%%%%
	%%%%   Version A
	%%%%

/*-----------Ilyas's packaging starts here -----------------------*
 * package(PckgName)
 * package(PckgName,OptionsList) 
 *
 * Create two object files to save the given procedures.
 * The first file is going to contain disassembled name
 * table entries and clauses of given procedures.
 * The second file contains the token table, the module
 * table, and other information. (See the documentation
 * about packaging for more details).
 */


export package/1.
export package/2.

package(PckgName)
	:-
	package(PckgName,[]).

package(PckgName,Opts)
	:-
	package_global_variables,
	check_default0(procs,Opts,[_],Procs),
	check_default0(xprocs,Opts,[],XProcs),

	catenate(PckgName,'.o',DefPckgFile),
	check_default0(pckgFile,Opts,DefPckgFile,PckgFile),

	catenate(PckgName,'env.o',DefPckgEnvFile),
	check_default0(pckgEnvFile,Opts,DefPckgEnvFile,PckgEnvFile),

	catenate(PckgName,'raw.o',DefPckgRawFile),	
	check_default0(pckgRawFile,Opts,DefPckgRawFile,PckgRawFile),

	check_default0(startPred,Opts,-1,StartPred),

	'$current_package'(DefPrePckgName),
	check_default0(prePckgName,Opts,DefPrePckgName,PrePckgName),

	check_default0(defProcSwch,Opts,1,DefProcSwch),
	check_default0(envSwch,Opts,1,EnvSwch),
	check_default0(relTblSize,Opts,64,RelTblSize),
	check_default0(symTblSize,Opts,32,SymTblSize),
	check_default0(strTblSize,Opts,512,StrTblSize),

	StartTime is cputime,
	package(Procs,XProcs,PckgName,PckgFile,PckgEnvFile,PckgRawFile,
		StartPred,PrePckgName, DefProcSwch,EnvSwch,
		RelTblSize,SymTblSize,StrTblSize,Flag),

	end_package(Flag,
				Procs,XProcs,PckgName,PckgFile,PckgEnvFile,PckgRawFile,
				StartPred,PrePckgName, DefProcSwch,EnvSwch,
				RelTblSize,SymTblSize,StrTblSize,
				StartTime).

end_package(0,
	    Procs,XProcs,PckgName,PckgFile,PckgEnvFile,PckgRawFile,
	    StartPred,PrePckgName, DefProcSwch,EnvSwch,
	    RelTblSize,SymTblSize,StrTblSize, StartTime) :-
	!,
	FinishTime is cputime,
	TimeTaken is FinishTime - StartTime,

	als_advise('\nCompleted package=%t over prepackage=%t with:\n',[PckgName,PrePckgName]),
	als_advise('  --time taken = %d\n',[TimeTaken]),
	als_advise('  Procs=%t \nXProcs=%t\n',[Procs,XProcs]),
	als_advise('  StartPred=%t\n',[StartPred]),
	als_advise('  PckgFile=%t PckgEnvFile=%t\n',[PckgFile,PckgEnvFile]).

end_package(ErrFlag,
	    Procs,XProcs,PckgName,PckgFile,PckgEnvFile,PckgRawFile,
	    StartPred,PrePckgName, DefProcSwch,EnvSwch,
	    RelTblSize,SymTblSize,StrTblSize, StartTime) :-
	pckg_error(ErrFlag, ErrCode),
	prolog_system_error(ErrCode, [PckgName]).

pckg_error(-1, datax).			%% "Data areas exhausted!"
pckg_error(-6, xsyms_2big).		%% "Too many external symbols!\n"


check_default0(Field,Opts,Default,Value) :-
	dmember(Field=Value,Opts),
	!.
check_default0(Field,Opts,Default,Default).


package(Procs,XProcs,PckgName,PckgFile,PckgEnvFile,PckgRawFile,
		StartPred,PrePckgName, DefProcSwch,EnvSwch,
		RelTblSize,SymTblSize,StrTblSize,Flag) 
	:-
	als_advise('Starting packaging:\n	%t\n',
		[package(Procs,XProcs,PckgName,PckgFile,PckgEnvFile,PckgRawFile,
				StartPred,PrePckgName,DefProcSwch,EnvSwch,
				RelTblSize,SymTblSize,StrTblSize) 
		] ),

	check_arguments(PckgName,PckgFile,PckgEnvFile,PckgRawFile,
		StartPred,PrePckgName,DefProcSwch,EnvSwch,
		RelTblSize,SymTblSize,StrTblSize),

	als_advise('Package arguments ok...\n'),

	get_def_procs(DefaultProcs),
	length(DefaultProcs,NumDefProcs),
	als_advise('Num default procs = %t\n',[NumDefProcs]),

	get_pckg_procs(DefProcSwch,Procs,DefaultProcs,XProcs,PckgProcs),
	length(PckgProcs,NumPckgProcs),
	als_advise('Num packaged procs = %t\n',[NumPckgProcs]),
	run_pckg(PckgName,PckgFile,PckgEnvFile,PckgRawFile,
		StartPred,PrePckgName,DefProcSwch,EnvSwch,
		RelTblSize,SymTblSize,StrTblSize,PckgProcs,DefaultProcs,
		Flag).


run_pckg(PckgName,PckgFile,PckgEnvFile,PckgRawFile,
		StartPred,PrePckgName,DefProcSwch,EnvSwch,
		RelTblSize,SymTblSize,StrTblSize,PckgProcs,DefaultProcs, 0)
	:-
	als_advise('\nData Regions: RelTbl=%t SymTbl=%t, StrTbl=%t\n',
				[RelTblSize,SymTblSize,StrTblSize]),
	create_pckg_file(PckgProcs,PckgName,PrePckgName,PckgFile,PckgRawFile,
		RelTblSize,SymTblSize,StrTblSize), 
	create_env_file(EnvSwch,PckgName,PckgEnvFile,PckgRawFile,DefaultProcs,
		StartPred,RelTblSize,SymTblSize,StrTblSize),
	!.
run_pckg(_,_,_,_,_,_,_,_,_,_,_,_,_,Err) :-
	'$get_pckg_error'(Err).


/*-----------------------------------------------------------------
 * Check type of arguments of the predicate "package/11"
 *---------------------------------------------------------------*/

check_arguments(PckgName,PckgFile,PckgEnvFile,PckgRawFile,
	StartPred,PrePckgName,DefProcSwch,EnvSwch,
	RelTblSize,SymTblSize,StrTblSize)
	:-
	atom(PckgName),
	atom(PckgFile),
	atom(PckgRawFile),
	( atom(PckgEnvFile) ; EnvSwch == -1 ), !,
	( StartPred = (M:P), atom(M), atom(P) ; 
	  atom(StartPred) ; StartPred == -1 ), !,
	( DefProcSwch == 1 ; DefProcSwch == -1 ), !,
	( atom(PrePckgName) ; PrePckgName == -1), !,
	integer(RelTblSize),
	integer(SymTblSize),
	integer(StrTblSize).


/*-----------------------------------------------------------------
 * Get all default procedures
 *---------------------------------------------------------------*/

get_def_procs(DefaultProcs) :- 
	findall(P/A,get_default_procs(P,A),DefaultProcs), !.


get_default_procs(P,A) :-
	'$get_default_proc'(0,NP,NA),
	get_default_procs(0,NP,NA,P,A).

get_default_procs(N,P,A,P,A).
get_default_procs(N,_,_,P,A) :-
    NN is N+1,
	'$get_default_proc'(NN,NP,NA),
	get_default_procs(NN,NP,NA,P,A).


/*-----------------------------------------------------------------
 * Get all procedures (Prolog, C, imported or undefined) in 
 * the name table which match procedures defined by the term "Procs".
 * Include all default procedures too, if "DefProcSwch" is set.
 * And excude all default procedures, if "DefProcSwch" is not set.
 *---------------------------------------------------------------*/

get_pckg_procs(1,Procs,DefaultProcs,XProcs,PckgProcs) :- 
	!,
	dappend(Procs,DefaultProcs,IProcs),
	get_proc_list(IProcs,XProcs,PckgProcs).
get_pckg_procs(_,Procs,DefaultProcs,XProcs,PckgProcs) :- 
	dappend(DefaultProcs,XProcs,XXProcs),
	get_proc_list(Procs,XXProcs,PckgProcs).

get_proc_list(IProcs,XProcs,ProcList) :- 
	findall((M:P/A,DBRef), get_procs(IProcs,XProcs,M,P,A,DBRef), ProcList), !.

get_procs(IProcs,XProcs,M,P,A,DBRef) :-
	all_ntbl_entries(M,P,A,DBRef),
	member_proc(IProcs,M,P,A),
	not(member_proc(XProcs,M,P,A)).

member_proc([M:P/A|_],M,P,A) :- !.
member_proc([P/A|_],M,P,A) :- !.
member_proc([P|_],M,P,A) :- !.
member_proc([_|Rest],M,P,A) :- member_proc(Rest,M,P,A).


/************************************************************************
 * 			Create Package File				*
 ************************************************************************/

create_pckg_file(PckgProcs,PckgName,PrePckgName,PckgFile,PckgRawFile,
	RelTblSize,SymTblSize,StrTblSize)
	:-
	coff_open_rawfile(PckgRawFile),
	coff_init_package(RelTblSize,SymTblSize,StrTblSize),
	coff_align8,
	als_advise('.'),
	mark_package_procedures(PckgProcs),
	als_advise('.'),
   	package_procedures(PckgProcs,PckgName),
	als_advise('.'),
	unmark_package_procedures(PckgProcs),
	als_advise('.'),
   	package_nametable(PckgProcs,PckgName),
	als_advise('.'),
   	package_pckg_info(PckgName,PrePckgName),
	als_advise('.'),
	coff_align8,
	coff_create_obj_file(PckgFile),
	als_advise('.').


/*
 * Mark and unmark package procedures
 */

mark_package_procedures([]) :- !.
mark_package_procedures([(M:P/A,_)|Procs]) :- 
	'$package_mark_proc'(M,P,A),
	mark_package_procedures(Procs).

unmark_package_procedures([]) :- !.
unmark_package_procedures([(M:P/A,_)|Procs]) :- 
	'$package_unmark_proc'(M,P,A),
	unmark_package_procedures(Procs).



/*
 * Package the given procedures
 *
 * Packaged procedures are represented as follows:
 *
 *		align 	4
 * pckg_code_start:
 * 		<packaged name table entries>
 *		align 	4
 *	 	dd 		-1 			;; dummy used block
 *		<packaged clauses>
 *		align 	4
 *	 	dd 		-1 			;; dummy used block
 * pckg_code_end:
 */

package_procedures(ProcList,PckgName) :- 
	coff_align_code_block,
	coff_declare_symbol(pckg_code_start),
	package_ntbl_entries(ProcList,PckgName),
	coff_align4,
	coff_put_long(-1),
	package_clauses(ProcList,PckgName),
	coff_align4,
	coff_put_long(-1),
	coff_align_code_block,
	coff_declare_symbol(pckg_code_end).


package_ntbl_entries([],_).
package_ntbl_entries([(Module:Pred/Arity,_)|ProcList],PckgName) :-
    '$listasm_ntblentry'(Module,Pred,Arity,PckgName),
   	package_ntbl_entries(ProcList,PckgName).


package_clauses([],_).
package_clauses([(P,FirstClause)|ProcList],PckgName) :-
	listasm_clauses(FirstClause,PckgName),
   	package_clauses(ProcList,PckgName).

listasm_clauses(0,_) :- !.
listasm_clauses(DBRef,PckgName) :-
   	'$listasm_clause'(DBRef,PckgName),
   	'$clauseinfo'(DBRef,NextDBRef,_,_),  		% get next clause
   	listasm_clauses(NextDBRef,PckgName).



/*
 * Package Name Table
 *
 * A package name table is represented as follows:
 *
 *		align 	4
 *	pckg_ntbl:
 *		dd		# of procedures
 *		dd		<first procedure address>
 *			.
 *			.
 *		dd		<last procedure address>
 */

package_nametable(ProcList,PckgName) :-
	coff_align4,
	coff_declare_symbol(pckg_ntbl),
   	length(ProcList,NumofProcs),
   	coff_put_long(NumofProcs),
   	package_nametable0(ProcList,PckgName).

package_nametable0([],PckgName).
package_nametable0([(M:P/A,_)|Procs],PckgName) :-
   	coff_put_procname(PckgName,M,P,A),
   	package_nametable0(Procs,PckgName).



/*
 * Package File Information
 *
 * Package file information is represented as follows:
 *
 *		align 	4
 *		public 	<Package Name>
 * <Package Name>:
 * 		dd 		<Previous Package Name>
 *		dd 		pckg_code_start
 * 		dd 		pckg_code_end
 * 		dd		pckg_ntbl
 * 		db 		"<Package Name>"
 */
package_pckg_info(PckgName,PrePckgName) :-
	coff_align4,
   	coff_declare_global_symbol(PckgName),
	( integer(PrePckgName) -> 
		coff_put_long(PrePckgName)
	;
   		coff_put_symbol(PrePckgName)
	),
   	coff_put_symbol(pckg_code_start),
   	coff_put_symbol(pckg_code_end),
   	coff_put_symbol(pckg_ntbl),
   	coff_put_string(PckgName).
   





/************************************************************************
 * 		Create Package Token Table File				*
 ************************************************************************/

create_env_file(-1,_,_,_,_,_,_,_,_) :- !.
create_env_file(_,PckgName,PckgEnvFile,PckgRawFile,DefaultProcs,StartPred,
			 	 RelTblSize,SymTblSize,StrTblSize) :-
	coff_open_rawfile(PckgRawFile),
	coff_init_package(RelTblSize,SymTblSize,StrTblSize),
	coff_align8,
	als_advise('.'),
	package_toktbl_info(PckgName,StartPred),
	als_advise('.'),
   	package_modtable(DefaultProcs),
	als_advise('.'),
   	package_toktbl,
	als_advise('.'),
	coff_align8,
	coff_create_obj_file(PckgEnvFile),
	als_advise('.').




/*
 * Package Token Table Information
 *
 * Package token table information is represented as follows:
 *
 *		align 	4
 *		public 	system_pckg
 *	system_pckg:
 * 		dd 		<package name>
 *		align 	4
 *		public 	pckg_start_pred
 *	pckg_start_pred:
 *		<package start pred>
 */
package_toktbl_info(PckgName,StartPred) :-
	coff_align4,
	coff_declare_C_global_symbol('_system_pckg'),
	coff_put_symbol(PckgName),
	coff_align4,
	coff_declare_C_global_symbol('_pckg_start_pred'),
	package_start_pred(StartPred).

package_start_pred(-1) :- 
	!, 
	coff_put_long(-1).
package_start_pred((M:P)) :- 
	!,
	coff_put_string(M),
	coff_put_string(P).
package_start_pred(P) :- 
	!,
	coff_put_string(user),
	coff_put_string(P).




/*
 * Package Module Table
 *
 * A package module table is represented as follows:
 *
 *		align 	4
 *		public 	pckg_modtbl
 *	pckg_modtbl:
 *		<modules in the module table>
 *		align 	4
 *		<modules in the default use module table>
 *		align 	4
 *		<predicates in the default procedure table>
 */
package_modtable(DefaultProcs) :-
	coff_align4,
	coff_declare_C_global_symbol('_pckg_modtbl'),
   	findall((Mod,UseList),modules(Mod,UseList),ModTbl),
   	package_modules(ModTbl),
	findall(M,get_default_uses(M),DefaultUseMods), 
	coff_align4,
   	package_default_uses(DefaultUseMods),
	coff_align4,
   	package_default_procs(DefaultProcs).


/*
 * Get default use modules which are automatically included by each module.
 */
get_default_uses(M) :-
	'$get_default_use'(0,NM),
	get_default_uses(0,NM,M).

get_default_uses(N,M,M).
get_default_uses(N,_,M) :-
    NN is N+1,
	'$get_default_use'(NN,NM),
	get_default_uses(NN,NM,M).


/*
 *	Modules in the module table are represented as follows:
 *
 *		dd	<# of modules>
 *		db	<modname1>,0
 *		align 	4
 *		dd	<# of modules in the use list of mod1>
 *		db	<firstusemodname of mod1>,0
 *			.
 *		db	<lastusemodname of mod1>,0
 *			.
 *			.	
 *		<last module and its use list>
 */
package_modules(ModTbl) :-
	length(ModTbl,Len),
	coff_put_long(Len),
	package_modules0(ModTbl).

package_modules0([]) :- !.
package_modules0([(Mod,UseList)|ModTbl]) :-
   	package_modtbl_entry(Mod,UseList),
   	package_modules0(ModTbl).

package_modtbl_entry(Mod,UseList) :-
	coff_put_string(Mod),
   	length(UseList,Len), 
	coff_align4,
	coff_put_long(Len),
   	package_uselist(UseList).


/*
 * Modules in the default use module table are represented as follows:
 *
 *		dd 	<# of default use modules>
 *		db	<first default use module>,0
 *			.
 *		db 	<last default use module>,0
 */
package_default_uses(UseMods) :-
   	length(UseMods,Len),
	coff_put_long(Len),
   	package_uselist(UseMods).


package_uselist([]) :- !.
package_uselist([Mod|UseList]) :-
	coff_put_string(Mod),
   	package_uselist(UseList).


/*
 * Predicates in the default procedure table are represented as follows.
 *
 *		dd	<# of default predicates>
 *		db  <first default predicate name>
 *		align 	4
 *		dd	<first default predicate arity>
 *			.
 *		<last default predicate name and its arity>
 */
package_default_procs(Procs) :-
	length(Procs,Len), 
	coff_put_long(Len),
	package_default_proc_list(Procs).

package_default_proc_list([]) :- !.
package_default_proc_list([P/A|Procs]) :- 
	coff_put_string(P),
	coff_align4,
	coff_put_long(A),
   	package_default_proc_list(Procs).



/*
 * Package Token Table 
 *
 * Package token table is represented as follows.
 *
 *		align 	4
 *		public pckg_toktbl
 *	pckg_toktbl:
 *		<first token>
 *			.
 *		<last token>
 *		align 	4
 *		public pckg_toktbl_size
 *	pckg_toktbl_size:
 *		dd	<# of tokens in the package token table>
 */
package_toktbl :-
	coff_align4,
	coff_declare_C_global_symbol('_pckg_toktbl'),
  	'$package_toktbl'(NumofTokens),
	coff_align4,
	coff_declare_C_global_symbol('_pckg_toktbl_size'),
	coff_put_long(NumofTokens).


/*
 * Should a package be created, initialize_global_variables/0 will be
 * replaced with a clause which will initialize the globals.
 */

initialize_global_variables.

/*
 * package_global_variables/0
 *
 * This procedure just creates an initialization predicate which will be
 * run to initialize the global variables at run time. 
 */

package_global_variables :-
	gv_maxpossible(N),
	package_global_variables(N,true,Out),
	builtins:abolish(initialize_global_variables,0),
	builtins:assert((initialize_global_variables :- Out)).

package_global_variables(0, Out, Out) :-
	!.
package_global_variables(N, Gs, Out) :-
	gv_isfree(N),
	!,
	NN is N-1,
	package_global_variables(NN, Gs, Out).
package_global_variables(N, Gs, Out) :-
	gv_get(N,Val),
	NN is N-1,
	package_global_variables(NN, (gv_alloc_init(N,Val),Gs), Out).




/************************************************************************
 * 					COFF Predicates										*
 ************************************************************************/


coff_align_code_block :-
	builtins:als_system(SysInfo),
	( member(processor=m88k,SysInfo), !, 
	  coff_align4,
	  coff_align_code_block(4096)
	;
	  coff_align4
	).

coff_align_code_block(0) :- !.
coff_align_code_block(Size) :- 
	coff_put_long(0),
	NewSize is Size - 4,
	coff_align_code_block(NewSize).

%%
%% coff opcode = 1
%%

coff_init_package(RelTblSize,SymTblSize,StrTblSize) :-
	'$coff_operation'(1,RelTblSize,SymTblSize,StrTblSize,_).
coff_init_package(Rel,Sym,Str) :-
	'$get_pckg_error'(Error),
	get_new_sizes(Error,Rel,Sym,Str,NRel,NSym,NStr),
	als_advise('\nData Regions: RelTbl=%t SymTbl=%t, StrTbl=%t\n',
		[NRel,NSym,NStr]),
	coff_init_package(NRel,NSym,NStr).

	%% Error code -2 for raw data area overflow is not used
get_new_sizes(-3,Rel,Sym,Str,Rel,NSym,Str) :- NSym is Sym *2.
get_new_sizes(-4,Rel,Sym,Str,Rel,Sym,NStr) :- NStr is Str *2.
get_new_sizes(-5,Rel,Sym,Str,NRel,Sym,Str) :- NRel is Rel *2.

%%
%% coff copcode = 2
%%

coff_align4 :-
	'$coff_operation'(2,_,_,_,_).

%%
%% coff opcode = 3
%%

coff_put_char(Val) :-
	'$coff_operation'(3,Val,_,_,_).

%%
%% coff opcode = 4
%%

coff_put_short(Val) :-
	'$coff_operation'(4,Val,_,_,_).

%%
%% coff opcode = 5
%%

coff_put_long(Val) :-
	'$coff_operation'(5,Val,_,_,_).

%%
%% coff opcode = 6
%%

coff_put_string(String) :-
	'$coff_operation'(6,String,_,_,_).

%%
%% coff opcode = 7
%%

coff_put_symbol(Symbol) :-
	'$coff_operation'(7,Symbol,_,_,_).

%%
%% coff opcode = 8
%%

coff_put_procname(PckgName,M,P,A) :-
	'$coff_operation'(8,PckgName,M,P,A).

%%
%% coff opcode = 9
%%

coff_declare_symbol(Symbol) :-
	'$coff_operation'(9,Symbol,_,_,_).

%%
%% coff opcode = 10
%%

coff_declare_global_symbol(Symbol) :-
	'$coff_operation'(10,Symbol,_,_,_).

%%
%% coff opcode = 11
%%

coff_declare_C_global_symbol(Symbol) :-
	'$coff_operation'(11,Symbol,_,_,_).


%%
%% coff opcode = 12
%%

coff_create_obj_file(File) :-
	'$coff_operation'(12,File,_,_,_),
	!.
coff_create_obj_file(File) :-
	'$get_pckg_error'(-7),
		%% no_open_wrt: "Unable to open file %t for writing!\n"
	prolog_system_error(no_open_wrt,[File]),
	fail.

%%
%% coff opcode= 13
%%

coff_open_rawfile(File) :-
	'$coff_operation'(13,File,_,_,_),
	!.
coff_open_rawfile(File) :-
	'$get_pckg_error'(-7),
		%% no_open_wrt: "Unable to open file %t for update!\n"
	prolog_system_error(no_open_upd,[File]),
	fail.


%%
%% coff opcode= 14
%%

coff_put_uia(UIA,Size) :-
	'$coff_operation'(14,UIA,Size,_,_).


%%
%% coff copcode = 15
%%

coff_align8 :-
	'$coff_operation'(15,_,_,_,_).

endmod.
