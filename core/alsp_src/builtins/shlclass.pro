/*================================================================*
 |		shlclass.pro
 |	Copyright (c) 1998 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |	Common class stuff for blt_shl/blt_dvsh/blt_cslt
 |	The ObjectPro class compiler is temporaily loaded to compile
 |	and assert (at compile time) the given class definitions;
 |	then the class compiler is removed along with the aux
 |	predicates defined here.
 |
 |	Original Creation Date: 7/98
 *================================================================*/

module builtins.

:- 
	compiletime,

		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%%%%%   ALS_SHL_MGR  ObjectPro CLASS DEFINITIONS    %%%%%
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	defineClass(builtins,
	[   name = als_shl_mgr,
		subClassOf = genericObjects,
		export = yes,
		addl_slots =
			[ 
				shell_module, 	  	  %% module for the shell (alsshell/alsdev)
				prolog_library, 	  %% path to the ...
				initial_dir,    	  %% initial directory we wake up in
				initial_search_dirs,  %% initial search list
				source_mgrs,	   	  %% list of managers for consulted files
				cslt_ctxt,		   	  %% (list) stack of "current source_mgr" 
				break_level			  %% break shell level (old global BreakLevel)
			],
		defaults = [ 
				shell_module = alsshell,   %% make alsdev reset this...
				source_mgrs = [],
				cslt_ctxt	= [],
				break_level = [b(0,user,true)]
		]
	]),

		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%%%%%  	Consulted File Manager CLASS DEFINITIONS    %%%%%
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %% The manager for individual consulted files:
	defineClass(builtins,
	[   name=source_handler,
		subClassOf=genericObjects,
		export = yes,
		addl_slots=
			[ 
				source_type,		%% file/anon win/....
				source_file, 		%% OS path to the ...
				base_file,			%% underlying file name
				ext,				%% underlying extension
				obp_file,			%% OS path to obp file if exists, or nil
				fcg, 				%% File clause group # for this (consulted) file
				consult_mode,		%% normal/debug
				last_consult		%% Time of last consult,
			],
		defaults= [ 
			source_type		= file,
			ext				= '',
			fcg				= 0,
			consult_mode	= normal,
			last_consult	= 0
			]
	]).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% GETTING A SOURCE MANAGER
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

als_shl_mgrAction(obtain_src_mgr(BaseFileName, FileMgr), State) 
	:-!,
	accessObjStruct(source_mgrs, State, PrevMgrsList),
	finish_obtain_src_mgr(BaseFileName, PrevMgrsList, State, FileMgr).

finish_obtain_src_mgr(BaseFileName, PrevMgrsList, State, FileMgr)
	:-
	dmember(fm(BaseFileName, FileMgr), PrevMgrsList),
	!.

finish_obtain_src_mgr(BaseFileName, PrevMgrsList, State, FileMgr)
	:-
	accessObjStruct(shell_module, State, ShellModule),
	(clause(alsdev_running,true) -> 
		Class = source_trace_mgr 
		; 
		Class = shl_source_handler
	),
	ShellModule:create_object(
		[instanceOf = Class,
		 handle = true,
		 values =
			[ 	source_type = file,
				base_file = BaseFileName 
			]
		],
		FileMgr ),
	setObjStruct(source_mgrs, State, [fm(BaseFileName, FileMgr) | PrevMgrsList]),
	(clause(alsdev_running,true) -> 
		accessObjStruct(debugger_mgr,  State, DBGMGR),
		setObjStruct(debugger_mgr,  FileMgr, DBGMGR)
		;
		true
	).

als_shl_mgrAction(obtain_src_mgr_by_cg(CG, FileMgr), State) 
	:-!,
	accessObjStruct(source_mgrs, State, PrevMgrsList),
	finish_obtain_src_mgr_by_cg(PrevMgrsList, CG, State, FileMgr).

finish_obtain_src_mgr_by_cg([fm(_,FileMgr) | List], CG, State, FileMgr)
	:-
	accessObjStruct(fcg, FileMgr, CG),
	!.

finish_obtain_src_mgr_by_cg([_ | List], CG, State, FileMgr)
	:-
	finish_obtain_src_mgr_by_cg(List, CG, State, FileMgr).

als_shl_mgrAction(remove_mgr(BaseFileName, FileMgr), State) 
	:-!,
	accessObjStruct(source_mgrs, State, PrevMgrsList),
	list_delete(PrevMgrsList, fm(BaseFileName, FileMgr), NewMgrsList),
	setObjStruct(source_mgrs, State, NewMgrsList).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% RECORDING LOADING INFO (note_loaded)
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

source_handlerAction(note_loaded(CG, Path), State)
	:-
	setObjStruct(source_file, State, Path),
	setObjStruct(fcg, State, CG).

endmod.
