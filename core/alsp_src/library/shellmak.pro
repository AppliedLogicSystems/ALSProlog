/*============================================================================*
 |          shellmak.pro
 |  Copyright (c) 1995-96 Applied Logic Systems, Inc.
 |      Distribution rights per Copying ALS
 |
 |      Generator for creating both tty-based and
 |			socket-based interactive servers & shells
 |
 |		Combines the previously separate ttyshlmk.pro and sktshlmk.pro
 |
 |  Author: Ken Bowen
 |  Dates: 
 |		December, 1995	- ttyshlmk.pro
 |		August, 1996	- sktshlmk.pro
 |		Septtember,1996	- combination
 |
 |  Included as a library file; the exported routines are library predicates.
 *============================================================================*/
			   

module shellmak.

export make_shell_cl/0.
export make_shell/1.
export make_shell/2.
export make_shell_list/2.
export mk_shell/4.
export mk_shell/5.

/*--------------------------------------------------------------------*
 |	make_shell_cl/0
 |	make_shell_cl
 |	make_shell_cl
 |
 |	- command-line invocation of shell spec transformation
 |
 |	Assumes the system has been started with a command-line of the form:
 |
 |	alspro [...] -p [-q][-v][-tty][-socket] File [File,...]
 |
 |	where the option correspondence is:
 |		-q		= quiet(true)
 |		-v		= quiet(true)
 |		-tty	= app_type(tty)
 |		-socket	= app_type(socket)
 *--------------------------------------------------------------------*/
make_shell_cl
	:-
	builtins:command_line(CommandLine),
	parse_cl(CommandLine, Files, Options),
	make_shell_list(Files, Options).

/*--------------------------------------------------------------------*
 |	parse_cl/3
 |	parse_cl(CommandLine, Files, Options)
 |	parse_cl(+, -, -)
 *--------------------------------------------------------------------*/
parse_cl([], [], []).

parse_cl([['-null_switch', File] | CommandLine], [File | Files], Options)
	:-!,
	parse_cl(CommandLine, Files, Options).

parse_cl([['-q'] | CommandLine], Files, [quiet(true) | Options])
	:-!,
	parse_cl(CommandLine, Files, Options).

parse_cl([['-v'] | CommandLine], Files, [quiet(false) | Options])
	:-!,
	parse_cl(CommandLine, Files, Options).

parse_cl([['-socket'] | CommandLine], Files, [app_type(socket) | Options])
	:-!,
	parse_cl(CommandLine, Files, Options).

parse_cl([['-tty'] | CommandLine], Files, [app_type(tty) | Options])
	:-!,
	parse_cl(CommandLine, Files, Options).


parse_cl([_ | CommandLine], Files, Options)
	:-
	parse_cl(CommandLine, Files, Options).

/*--------------------------------------------------------------------*
 *--------------------------------------------------------------------*/
make_shell(Files)
	:-
	make_shell(Files, []).

make_shell([File | Files], Options)
	:-!,
	make_shell_list([File | Files], Options).

make_shell(File, Options)
	:-!,
	make_shell_list([File], Options).

/*--------------------------------------------------------------------*
 *--------------------------------------------------------------------*/
make_shell_list(Files, Options)
	:-
	(dmember(quiet(Quiet), Options),!; Quiet=false),
	(dmember(app_type(AppType), Options),!; AppType=tty),
	make_shell_list(Files, op(Quiet,AppType), Options).

/*--------------------------------------------------------------------*
 |	make_shell_list/3
 |	make_shell_list(Files, OpStr, Options)
 |	make_shell_list(+, +, +)
 |
 |	Principal recursive driver at the "list of files" level
 *--------------------------------------------------------------------*/
make_shell_list([], _, _).
make_shell_list([File | Files], OpStr, Options)
	:-
	make_shell(File, OpStr, Options),
	make_shell_list(Files, OpStr, Options).

/*--------------------------------------------------------------------*
 |	make_shell/3
 |	make_shell(InSrcFile, OpStr, Options)
 |	make_shell(+, +, +)
 |
 |	Housekeeping setup for principal individual file routine
 *--------------------------------------------------------------------*/

make_shell(InSrcFile, OpStr, Options)
	:-
		%% setup correct file names:
	(file_extension(BaseSrcFile,spc,InSrcFile) ->
		SrcFile = InSrcFile
		;
		file_extension(InSrcFile,spc,SrcFile),
		BaseSrcFile = InSrcFile
	),
%	file_extension(BaseSrcFile,pro,TgtFile),
	mk_shell(SrcFile, BaseSrcFile, Options).

/*--------------------------------------------------------------------*
 |	mk_shell/4
 |	mk_shell(SrcFile,BaseSrcFile, Options, OutFilePath)
 |	mk_shell(+, +, +, +)
 |
 |	Housekeeping setup for principal individual file routine
 *--------------------------------------------------------------------*/
mk_shell(SrcFile, BaseSrcFile, Options, OutFilePath)
	:-
	(dmember(quiet(Quiet), Options),!; Quiet=false),
	(dmember(app_type(AppType), Options),!; AppType=tty),
	OpStr = op(Quiet, AppType),
	mk_shell(SrcFile, BaseSrcFile, OpStr, Options, OutFilePath).

/*--------------------------------------------------------------------*
 *--------------------------------------------------------------------*/

quiet(op(Quiet, AppType), Quiet).
app_type(op(Quiet, AppType), AppType).

/*--------------------------------------------------------------------*
 |	mk_shell/5
 |	mk_shell(SrcFile,BaseSrcFile, OpStr, Options, OutFilePath)
 |	mk_shell(+, +, +, +, +)
 |
 |	catch-protected entry to principal individual file routine
 *--------------------------------------------------------------------*/
mk_shell(SrcFile, BaseSrcFile, OpStr, Options, OutFilePath)
	:-
	catch(
		x_mk_shell(SrcFile, BaseSrcFile, OpStr, Options, OutFilePath),
		Ball,
		mk_shell_err_h(Ball,SrcFile, BaseSrcFile, OpStr, Options)
	).

/*--------------------------------------------------------------------*
 |	Check for existence of source file
 *--------------------------------------------------------------------*/

x_mk_shell(SrcFile, BaseSrcFile, OpStr, Options, OutFilePath)
	:-
	exists_file(SrcFile),
	!,
	pathPlusFile(SrcFileDir, UnderSrcFile, SrcFile),
		%% setup correct file names:
	(file_extension(BaseSrcFile, spc, UnderSrcFile) ->
		true
		;
		BaseSrcFile = UnderSrcFile
	),
	(OutFilePath = '' ->
		file_extension(BaseSrcFile, pro, UnderTgtFile),
		pathPlusFile(SrcFileDir, UnderTgtFile, TgtFile)
		;
		TgtFile = OutFilePath
	),
	cont_x_mk_shell(SrcFile, TgtFile, BaseSrcFile, SrcFileDir, OpStr, Options).

x_mk_shell(SrcFile, BaseSrcFile, OpStr, Options, _)
	:-
	raise_mks_xcept('!!Error: File %t does not exist!\n',[SrcFile]).

/*--------------------------------------------------------------------*
 |	Compare the file times for the source and target files.  If
 |	both source and target exist, and if source is older than
 |	target, do nothing; else go to work.
 *--------------------------------------------------------------------*/
cont_x_mk_shell(SrcFile, TgtFile, BaseSrcFile, SrcFileDir, OpStr, Options)
	:-
			%% TgtFile exists & SrcFile is older:
	comp_file_times(SrcFile, TgtFile).

cont_x_mk_shell(SrcFile,TgtFile, BaseSrcFile, SrcFileDir, OpStr, Options)
	:-
	quiet(OpStr, Quiet),
	a_message(Quiet, '\nProcessing spc file %t...\n', [SrcFile]),
	do_mk_shell(SrcFile, TgtFile, BaseSrcFile, SrcFileDir, OpStr, Options),
	a_message(Quiet, 'Done with spc file %t.\n', [SrcFile]).

/*--------------------------------------------------------------------*
 |	Read the list SpcTerms of all terms from the source file;
 |	Do final fixup of tty/socket option based on SpcTerms & Options;
 |	Enter primary processed predicate.
 *--------------------------------------------------------------------*/
do_mk_shell(SrcFile, TgtFile, BaseSrcFile, SrcFileDir, OpStr, Options)
	:-
	grab_terms(SrcFile, SpcTerms),
	OpStr = op(Quiet, AppType), 

	(dmember(app_type=socket, SpcTerms) ->
		NewAppType = socket ; NewAppType = AppType),
	NewOpStr = op(Quiet, NewAppType) ,
	synth_shell_units(NewAppType, SpcTerms, SrcFile, TgtFile, SrcFileDir, Quiet),
	!.

do_mk_shell(SrcFile, _, _, _, _, _)
	:-
	raise_mks_xcept('Error!! Unknown error in spc file %t -- not processed',[SrcFile]).

/*****************************************
/*============================================================================*
 |			ttyshlmk.pro
 |	Copyright (c) 1995-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		Generator for creating interactive shells
 |
 |	Author: Ken Bowen
 |	Date: December, 1995
 |
 |	Included as a library file; the exported routines are library predicates.
 *============================================================================*/

export mk_tty_shell/6.

mk_tty_shell(SpcTerms, SrcFile, TgtFile, BaseSrcFile, OpStr, Options)
	:-
	OpStr=op(Quiet,AppTp),
	synth_shell_units(tty, SpcTerms, SrcFile, TgtFile, Quiet).

/*
	pathPlusFile(TgtDir, _, TgtFile),
	pathPlusFile(TgtDir, makefile, MainMakefile),
	main_makefile(MainMakefile),

	pathPlusFile(TgtDir, 'makefile.spc', SpcMakefile),
	spc_makefile(SpcMakefile,SrcFile,BaseSrcFile,TgtDir).
*/

*****************************************/

/*-------------------------------------------------------------------------------*
 |	synth_shell_units/5
 |	synth_shell_units(AppTp, SpcTerms, InSrcFile, TgtFile, Quiet)
 |	synth_shell_units(+, +, +, +, +)
 |
 |	- The core routine - organizes all work
 *-------------------------------------------------------------------------------*/

synth_shell_units(AppTp, SpcTerms, InSrcFile, TgtFile, SrcFileDir, Quiet)
	:-
	dmember(name=Name,SpcTerms),
	!,
		%% handle defaults:
	shell_items(AppTp, ItemsList),

	ssu_defaults(ItemsList,SpcTerms,Name,AppTp,XSpcTerms0),

		%% merge (new) defaults with other items from
		%% source list which are not touched:
	merge_tagged_lists(XSpcTerms0, [insrcfile=InSrcFile | SpcTerms], XSpcTerms),

		%% synthsize the code:
	shell_units(AppTp, UnitsList),
	ssu(UnitsList, XSpcTerms, AppTp, Quiet, SrcFileDir, XSpcCode),

	dmember(module = TheModule, XSpcTerms),
	append([module(socket_comms),use(TheModule),endmod,nl],
		   XSpcCode, YSpcCode),

		%% output code:
	open(TgtFile, write, TgtS, []),
	gen_file_header(TgtS, InSrcFile, TgtFile),
	write_clauses(TgtS, YSpcCode, [quoted(true)]),
	close(TgtS).

synth_shell_units(AppTp, SpcTerms, InSrcFile, TgtFile, Quiet)
	:-
	raise_mks_xcept('\nError!!: No name for spc system: %t !!\nExiting.\n',[]).

		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%% Spc Component Items to check for, 
		%%	and set defaults for if not present:
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

shell_items(AppTp, ItemsList)
	:-
	ItemsList = [
		name,
		module,
		use,
		global_var,
		banner,
		type_file,
		type_root,
		type_name,
		consults	| Tail],
	shell_items_tail(AppTp, Tail).

shell_items_tail(socket, [ config_file_name_type ]).
shell_items_tail(tty,    [ start, menu_type ]).

/*-------------------------------------------------------------------------------*
 *-------------------------------------------------------------------------------*/
ssu_defaults([], _, _, _, []).
ssu_defaults([Item | ItemList], SpcTerms, Name, AppTp, XSpcTerms)
	:-
	synth_unit_dflt(Item, SpcTerms, Name, XSpcTerms, AppTp, XSpcTermsTail),
	ssu_defaults(ItemList, SpcTerms, Name, AppTp, XSpcTermsTail).

/*-------------------------------------------------------------------------------*
 |	synth_unit_dflt/6
 |	synth_unit_dflt(Item, SpcTerms, Name, XSpcTerms, AppTp, XSpcTermsTail)
 |	synth_unit_dflt(+, +, +, +/-, +, -)
 |
 |	- check & set defaults for every item
 |
 |	(XSpcTerms, XSpcTermsTail) form a extensible list {hence +/- for XSpcTerms above}
 *-------------------------------------------------------------------------------*/

synth_unit_dflt(name, SpcTerms, Name, [name=Name | Tail], AppTp, Tail)
	:-!.

synth_unit_dflt(module, SpcTerms, Name, [module=ModName | Tail], AppTp, Tail)
	:-!,
	(dmember(module=ModName,SpcTerms) ->
		true
		;
		ModName = Name
	).

synth_unit_dflt(use, SpcTerms, Name, [use=UseList | Tail], AppTp, Tail)
	:-!,
	(dmember(use=UseList,SpcTerms) ->
		true
		;
		UseList = []
	).

synth_unit_dflt(global_var, SpcTerms, Name, [global_var=GVarName | Tail], AppTp, Tail)
	:-!,
	(dmember(global_var=GVarName,SpcTerms) ->
		true
		;
		make_uc_sym(Name,UCName),
		catenate(UCName,'_GV',GVarName)
	).

synth_unit_dflt(banner, SpcTerms, Name, 
				[banner=BannerName,choice_banner=ActionBanner | Tail], AppTp, Tail)
	:-!,
	(dmember(banner=BannerName,SpcTerms) ->
		true
		;
		make_banner(Name,BannerName)
	),
	(dmember(choice_banner = ActionBanner, SpcTerms) ->
		true
		;
		catenate(BannerName, ' action choice:\n',ActionBanner)
	).

synth_unit_dflt(start, SpcTerms, Name, 
			[start=StartName, loop=LoopName, short_start = SSName,
		     restart = RestartName, short_restart = SRSName,
			 act_on = ActOnName | Tail], AppTp, Tail)
	:-!,
	(dmember(start=StartName,SpcTerms) ->
		true
		;
		StartName = Name
	),
	catenate(StartName,'_ttyloop',LoopName),
	short_start_name(StartName, SSName),
	catenate('restart_',StartName,RestartName),
	catenate(r,SSName,SRSName),
	catenate(StartName,'_act_on',ActOnName).

synth_unit_dflt(type_file, SpcTerms, Name, 
					[type_file=FullTypeFile, base_type_file=BaseTypeFile | Tail], AppTp, Tail)
	:-!,
	(dmember(type_file=TypeFile,SpcTerms) ->
		(file_extension(BaseTypeFile,typ,TypeFile) ->
			FullTypeFile = TypeFile
			;
			file_extension(TypeFile,typ,FullTypeFile),
			BaseTypeFile = TypeFile
		)
		;
		dmember(name=Name, SpcTerms),
		make_type_file(Name,BaseTypeFile,FullTypeFile)
	).

synth_unit_dflt(type_root, SpcTerms, Name, [type_root=Root | Tail], AppTp, Tail)
	:-!,
	(dmember(type_root=Root, SpcTerms) ->
		true
		;
		dmember(name=Name, SpcTerms),
		atom_codes(Name, NCs),
		at_most_n(NCs,3,NC3),
		make_uc(NC3,UCNC3),
		append(UCNC3,"I",RootCs),
		atom_codes(Root, RootCs)
	).

synth_unit_dflt(type_name, SpcTerms, _, XTerms, AppTp, Tail)
	:-!,
	(dmember(type_name=TypeName, SpcTerms) ->
		XTerms =  [type_name = TypeName | Tail]
		;
		XTerms = Tail
	).

synth_unit_dflt(menu_type, SpcTerms, Name, [menu_type=MenuType | Tail], AppTp, Tail)
	:-!,
	(dmember(action(_,_)=_, SpcTerms) ->
		MenuType = default_numbered
		;
		MenuType = user_coded
	).

synth_unit_dflt(consults, SpcTerms, Name, [consults=FullConsults | Tail], AppTp, Tail)
	:-!,
	(dmember(consults=Consults,SpcTerms) ->
		true
		;
		Consults = []
	),
	standard_consults(AppTp, StCons),
	append(Consults, StCons, XCons),
		%% get rid of duplicates:
	sort(XCons, FullConsults).

synth_unit_dflt(config_file_name_type, SpcTerms, Name, 
		[config_file_name_type=config_file_name_type(ConfigFile,ConfigType) | Tail], AppTp, Tail)
	:-!,
	(dmember(config_file_name_type=ConfigFile+ConfigType, SpcTerms) ->
		true
		;
		ConfigType = terms,
		file_extension(Name,ini,ConfigFile)
	).

/*-------------------------------------------------------------------------------*
 *-------------------------------------------------------------------------------*/

standard_consults(tty, []).
standard_consults(socket, [sktserve,sktsvlib,usradmn]).

short_start_name(Name, SSName)
	:-
	atom_codes(Name, NCs),
	(NCs = [C] ->
		atom_codes(SSName, [C,C])
		;
		(dmember(0'_, NCs) ->
			asplit0(NCs, 0'_, [C1,LC2 | _], Right),
			(Right = [] ->
				C2 = LC2
				;
				Right = [C2 | _]
			),
			atom_codes(SSName, [C1, C2])
			;
			NCs = [C1, C2 | _],
			atom_codes(SSName, [C1, C2])
		)
	).

make_type_file(Name,BaseTypeFile,TypeFile)
	:-
	atom_codes(Name, NCs),
	at_most_n(NCs,4,NC4),
	append(NC4,"type",BTFCs),
	atom_codes(BaseTypeFile,BTFCs),
	append(NC4,"type.typ",TFCs),
	atom_codes(TypeFile,TFCs).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% 	     SYNTHESIZE THE CODE
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% Code units to synthesize:

shell_units(tty, [
	module,
	use,
	global_var,
	start,
	info_type,
	stock_code,
	actions
	]).

shell_units(socket,[
	module,
	use,
	global_var,
	info_type,
	config_file_name_type,
	actions
	]).

/*-------------------------------------------------------------------------------*
 *-------------------------------------------------------------------------------*/

	%%
	%% Recurse down the list of units, builting each code fragment:
	%%
ssu([], _, _, _, _,
		[
		 (quit_to_prolog(_) :- system('rm *.obp'),
				   printf('Exiting to Prolog...\n',[]), !, fail),
		 (exit_to_os(_) :- system('rm *.obp'),
				printf('Exiting to Operating System...\n',[]), !, halt),
		 endmod ]).

ssu([Unit | UnitList], XSpcTerms, AppTp, Quiet, SrcFileDir, XSpcCode)
	:-
	synth_unit(Unit, XSpcTerms, AppTp, SrcFileDir, XSpcCode, XSpcCodeTail),
	ssu(UnitList, XSpcTerms, AppTp, Quiet, SrcFileDir, XSpcCodeTail).


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Synthesis for each unit fragment:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*-------------------------------------------------------------------------------*
 *-------------------------------------------------------------------------------*/

	%%
	%% module
	%%
synth_unit(module, XSpcTerms, AppTp, SrcFileDir, Code, CodeTail)
	:-!,
	dmember(module=Mod, XSpcTerms),
	dmember(consults=InitConsults, XSpcTerms),
	dmember(base_type_file=BaseTypeFile, XSpcTerms),
	Consults = [BaseTypeFile | InitConsults],
	(Consults = [] ->
		Code = [(module Mod), (use windows),nl | CodeTail]
		;
		recons_fix(Consults, ReConsults),
		Code = [(:- ReConsults),nl, (module Mod), (use windows),nl | CodeTail]
	).

	%%
	%% config_file_name_type
	%%
synth_unit(config_file_name_type, XSpcTerms, AppTp, SrcFileDir, Code, CodeTail)
	:-!,
	dmember(config_file_name_type=CFT, XSpcTerms),
	Code = [(export config_file_name_type/2), CFT | CodeTail].

	%%
	%% use
	%%
synth_unit(use, XSpcTerms, AppTp, SrcFileDir, Code, CodeTail)
	:-!,
	dmember(use=UseList, XSpcTerms),
	(UseList = [] ->
		Code = CodeTail
		;
		mk_uses_items(UseList, Code, CodeTail)
	).

mk_uses_items([UseItem | UseList], [(use UseItem) | Code], CodeTail)
	:-
	mk_uses_items(UseList, Code, CodeTail).

mk_uses_items([], CodeTail, CodeTail).

	%%
	%% global_var
	%%
synth_unit(global_var, XSpcTerms, AppTp, SrcFileDir, 
			[(:- make_gv(GVarName)), nl | CodeTail], CodeTail)
	:-!,
	dmember(global_var=GVarName, XSpcTerms).

	%%
	%% start
	%%
synth_unit(start, XSpcTerms, AppTp, 
			SrcFileDir, 
			   [(export StartName/0),(StartName :- StartBody), nl,
				(export SSName/0), (SSName :- StartName), nl,
				(export RSName/0), (RSName :- RSBody), nl,
				(export SRSName/0), (SRSName :- RSName), nl
				| CodeTail],  CodeTail) 
	:-!,
	dmember(start=StartName, XSpcTerms),
	dmember(short_start=SSName, XSpcTerms),
	dmember(restart=RSName, XSpcTerms),
	dmember(short_restart=SRSName, XSpcTerms),

	dmember(loop=LoopName, XSpcTerms),
	LoopCall =..[LoopName, '%lettervar%'('Info')],

	dmember(banner=Banner, XSpcTerms),
	sprintf(atom(B0),'\n\n%t: TTY version\n',[Banner]),
	atom_length(Banner,L1),
	LL is L1 + 13,
	n_of(LL, 0'=, EEs),
	atom_codes(EE,EEs),
	catenate([B0,EE,'\n'],TheBanner),
	(dmember(info_initialize = InfoInitPred, XSpcTerms) ->
		InfoInitCall =.. [InfoInitPred, '%lettervar%'('Info')],
		StartBody =
			(printf(TheBanner,[]),
		 	setup_initial_info('%lettervar%'('Info')),
			InfoInitCall,
		 	LoopCall
			)
		;
		StartBody =
			(printf(TheBanner,[]),
		 	setup_initial_info('%lettervar%'('Info')),
		 	LoopCall
			)
	),

	catenate('Restarting ',Banner, ReBanner),
	sprintf(atom(ReB0),'\n\n%t: TTY version\n',[ReBanner]),
	atom_length(ReBanner,ReL1),
	ReLL is ReL1 + 13,
	n_of(ReLL, 0'=, ReEEs),
	atom_codes(ReEE,ReEEs),
	catenate([ReB0,ReEE,'\n'],ReTheBanner),

	dmember(global_var=GVarName, XSpcTerms),
	catenate(get,GVarName,GVGetPred),
	GVGetCall =.. [GVGetPred , '%lettervar%'('Info') ],
	RSBody =
		(printf(ReTheBanner,[]),
		 GVGetCall,
		 LoopCall
		).

	%%
	%% stock_code
	%%
synth_unit(stock_code, XSpcTerms, AppTp, SrcFileDir, Code, CodeTail)
	:-!,
	dmember(loop=LoopName, XSpcTerms),
	LoopCall =..[LoopName, '%lettervar%'('Info')],

	dmember(act_on=ActOnName, XSpcTerms),
	ActOnCall =..[ActOnName, '%lettervar%'('Request'), '%lettervar%'('Info')],
	ActOnQuit =..[ActOnName, quit, '%lettervar%'('Info')],

	dmember(choice_banner = ActionBanner, XSpcTerms),

	dmember(menu_type=MenuType, XSpcTerms),
	(MenuType = default_numbered ->
		GetRequestClause =
		(get_shell_request('%lettervar%'('Request'),'%lettervar%'('Info'))
			:-
			action_list('%lettervar%'('ActList')),
			printf(ActionBanner,[]),
			menu(_,'%lettervar%'('ActList'), '%lettervar%'('Request')) )

		; 		%% MenuType = user_coded

		GetRequestClause =
		(get_shell_request('%lettervar%'('Request'),'%lettervar%'('Info'))
			:-
			action_list('%lettervar%'('ActList')),
			code_list('%lettervar%'('CodeList')),
			printf(ActionBanner,[]),
			simple_menu('%lettervar%'('ActList'), '%lettervar%'('Request'),
							[codes='%lettervar%'('CodeList')]) )
	),

	((dmember(loop_notice=LoopNotice, XSpcTerms), LoopNotice\=nil) ->
		NoticeCall =..[LoopNotice , '%lettervar%'('Info')],
		MainLoopClause = 
			(LoopCall :- 
				NoticeCall,
				get_shell_request('%lettervar%'('Request'),'%lettervar%'('Info')),
				'%lettervar%'('Request') \= '$noChoice',
				!,
		 		ActOnCall) 
		;
		MainLoopClause = 
			(LoopCall :- 
				get_shell_request('%lettervar%'('Request'),'%lettervar%'('Info')),
				'%lettervar%'('Request') \= '$noChoice',
				!,
		 		ActOnCall) 
	),	

	Code = [ 

		MainLoopClause, nl,

		(LoopCall :- ActOnQuit), nl,

		GetRequestClause, nl | CodeTail].

	%%
	%% actions -- for sockets:
	%%
synth_unit(actions, XSpcTerms, socket, SrcFileDir, Code, CodeTail)
	:-!,
	assemble_init_acts(XSpcTerms, InitActList),
	!,
	GInfoVar = '%lettervar%'('SInfo'),

	(dmember(action(quit)-_, InitActList) ->
		ActList = InitActList,
		XXSpcTerms = XSpcTerms
		;
		((dmember(quit_action=QA, XSpcTerms), dmember(action(QA,_)-_,InitActList)) ->
			ActList = InitialActList,
			XXSpcTerms = [quit_alias(QAA) | XSpcTerms]
			;
			ActList = [action(quit)-true | InitActList],
			XXSpcTerms = XSpcTerms
		)
	),

	bagof(A, [B]^member(action(A)-B,ActList), Acts),

LoopName = noloop,
ActOnName= act_on_server_request,

	build_act_clauses(ActList,  GInfoVar, LoopName, ActOnName, socket,
						CodeTail, XXSpcTerms, ActClauses),
	server_vars(ServerVars),
	length(ServerVars, MM),
	NN is MM+1,
	Code = [nl,(export ActOnName/NN), action_list(Acts),nl | ActClauses].

assemble_init_acts(XSpcTerms, InitActList)
	:-
	bagof(action(Act)-B, member(action(Act)=B, XSpcTerms), InitActList0),
	bagof(action(Act)-B, xtenda(Act, B, XSpcTerms), InitActList1),
	append(InitActList0, InitActList1, InitActList).

xtenda(Act, B, XSpcTerms)
	:-
	member(action(Act), XSpcTerms),
	functor(Act, B, _).

	%%
	%% actions -- for tty:
	%%
synth_unit(actions, XSpcTerms, AppTp, SrcFileDir, Code, CodeTail)
	:-
	bagof(action(Act,X)-B, member(action(Act,X)=B, XSpcTerms), InitActList),
	!,
	(dmember(action(quit,_)-_, InitActList) ->
		ActList = InitActList,
		XXSpcTerms = XSpcTerms
		;
		((dmember(quit_action=QA, XSpcTerms), dmember(action(QA,_)-_,InitActList)) ->
			ActList = InitialActList,
			XXSpcTerms = [quit_alias(QAA) | XSpcTerms]
			;
			ActList = [action(quit,Info)-true | InitActList],
			XXSpcTerms = XSpcTerms
		)
	),
	bagof(A, [X,B]^member(action(A,X)-B,ActList), Acts),

	dmember(loop=LoopName, XSpcTerms),
	dmember(act_on=ActOnName, XSpcTerms),
	GInfoVar = '%lettervar%'('Info'),
	build_act_clauses(ActList,  GInfoVar, LoopName, ActOnName, AppTp,
						CodeTail, XXSpcTerms, ActClauses),
	Code = [nl,action_list(Acts),nl | ActClauses].

synth_unit(actions, XSpcTerms, AppTp, SrcFileDir, Code, CodeTail)
	:-
	bagof(action(ACd,Act,X)-B, member(action(ACd,Act,X)=B, XSpcTerms), InitActList),
	!,
	setup_quit2(InitActList, XSpcTerms, ActList, XXSpcTerms),
	bagof(A, [C,X,B]^member(action(C,A,X)-B,ActList), Acts),
	bagof(C, [A,X,B]^member(action(C,A,X)-B,ActList), Codes),

	dmember(loop=LoopName, XSpcTerms),
	dmember(act_on=ActOnName, XSpcTerms),
	GInfoVar = '%lettervar%'('Info'),
	build_act_clauses(ActList,  GInfoVar, LoopName, ActOnName, AppTp,
						CodeTail, XXSpcTerms, ActClauses),
	Code = [nl,action_list(Acts),nl,code_list(Codes) | ActClauses].

synth_unit(info_type, XSpcTerms, AppTp, SrcFileDir, Code, CodeTail)
	:-!,
	gen_type_spec(XSpcTerms, SrcFileDir, MakePred, AppTp),
	!,
	Code = [SetupInfo,nl | CodeTail],
	(dmember(init = InitCode-InfoVar, XSpcTerms) ->
		true
		;
		InitCode = nil
	),
	InfoVar = '%lettervar%'('Info'),
	MakeCall =.. [MakePred, InfoVar],
	dmember(global_var=GVarName, XSpcTerms),
	catenate(set,GVarName,GVSetPred),
	InstallCall =..[GVSetPred, InfoVar],
	(InitCode = nil ->
		SetupBody = (MakeCall,InstallCall)
		;
		SetupBody = (MakeCall,InitCode,InstallCall)
	),
	SetupInfo = 
	( setup_initial_info('%lettervar%'('Info')) 
		:-
		SetupBody ).
	
/*-------------------------------------------------------------------------------*
 *-------------------------------------------------------------------------------*/

setup_quit2(InitActList, XSpcTerms, InitActList, XSpcTerms)
	:-
	dmember(action(quit,_)-_, InitActList), !.

setup_quit2(InitActList, XSpcTerms, InitActList, [quit_alias(QAA) | XSpcTerms])
	:-
	dmember(quit_action=QA, XSpcTerms), 
	(dmember(action(QA,QAA,_)-_,InitActList) 
		; dmember(action(_,QA,QAA)-_,InitActList)),
	!.

setup_quit2(InitActList, XSpcTerms, [action(quit,Info)-true | InitActList], XSpcTerms).


/*-------------------------------------------------------------------------------*
 *-------------------------------------------------------------------------------*/

build_act_clauses([], _, LoopName,ActOnName,AppTp,CodeTail, _, [DefaultClause | CodeTail])
	:-
	build_act(default,default, GInfoVar,default,LoopName,ActOnName,AppTp,XSpcTerms, DefaultClause).

build_act_clauses([action(Act)-Body | ActList], GInfoVar, LoopName,
					ActOnName,AppTp,CodeTail, XSpcTerms, [AClause,nl | ActClauses])
	:-!,
	build_act(Act,LInfoVar, GInfoVar,Body,LoopName,ActOnName,AppTp,XSpcTerms, AClause),
	build_act_clauses(ActList, GInfoVar, LoopName,
						ActOnName,AppTp,CodeTail, XSpcTerms, ActClauses).

build_act_clauses([action(Act,LInfoVar)-Body | ActList], GInfoVar, LoopName,
					ActOnName,AppTp,CodeTail, XSpcTerms, [AClause,nl | ActClauses])
	:-!,
	build_act(Act,LInfoVar, GInfoVar,Body,LoopName,ActOnName,AppTp,XSpcTerms, AClause),
	build_act_clauses(ActList, GInfoVar, LoopName,
						ActOnName,AppTp,CodeTail, XSpcTerms, ActClauses).

build_act_clauses([action(Code,Act,LInfoVar)-Body | ActList], GInfoVar, LoopName,
					ActOnName,AppTp,CodeTail, XSpcTerms, [AClause,nl | ActClauses])
	:-
	build_act(Act,LInfoVar, GInfoVar,Body,LoopName,ActOnName,AppTp,XSpcTerms, AClause),
	build_act_clauses(ActList, GInfoVar, LoopName,
						ActOnName,AppTp,CodeTail, XSpcTerms, ActClauses).

/*-------------------------------------------------------------------------------*
 *-------------------------------------------------------------------------------*/

	%%
	%% quit -- tty:
	%%
build_act(quit,LInfoVar,GInfoVar,QB,LoopName,ActOnName,tty,XSpcTerms,AClause)
	:-!,
	AClause = (AHead :- Body),
	(var(LInfoVar) ->
		LInfoVar = GInfoVar
		;
		true
	),
	AHead =.. [ActOnName, quit, LInfoVar],

	dmember(banner=Banner,XSpcTerms),
	catenate(['Exiting ',Banner,'\n'],Msg),
	(QB = true ->
		Body = printf(Msg,[])
		;
		Body = (QB,printf(Msg,[]))
	).

	%%
	%% quit -- sockets:
	%%
build_act(quit,LInfoVar,GInfoVar,QB,LoopName,ActOnName,socket,XSpcTerms,AClause)
	:-!,
	AClause = (AHead :- Body),
	server_vars(ServerVars),
	(var(LInfoVar) ->
		LInfoVar = GInfoVar
		;
		true
	),
	AHead =.. [ActOnName, quit | ServerVars],

	(dmember(banner=Banner,XSpcTerms) -> true ; dmember(name = Banner, XSpcTerms)),
	catenate(['Exiting ',Banner,'\n'],Msg),
	(QB = true ->
		Body = printf(Msg,[])
		;
		Body =.. [QB | ServerVars]
	).

	%%
	%% default
	%%
build_act(default,default,GInfoVar,default,LoopName,ActOnName,tty,XSpcTerms,DefaultClause)
	:-!,
	DefaultClause = (AHead :- Body),
	AHead =.. [ActOnName, What, GInfoVar],
	ALoop =.. [LoopName, GInfoVar],
	(What = '$badInput$'(OutWhat) -> true ; OutWhat = What),
	Body = (printf('Unknown request: %t\n\n',[OutWhat]), ALoop).

build_act(default,default,GInfoVar,default,LoopName,ActOnName,socket,XSpcTerms,DefaultClause)
	:-!,
	DefaultClause = (AHead :- Body),
	server_vars(ServerVars),
	server_vars_states(ServerVars, State),
	server_vars_streams(ServerVars, _, SW),
	server_vars_sinfo(ServerVars, SInfo),
	AHead =.. [ActOnName,  '%lettervar%'('What') | ServerVars],
	Body = server_respond(unknown_request, ['%lettervar%'('What')], SW, SInfo).

	%%
	%% quit_alias
	%%
build_act(Act,LInfoVar,GInfoVar,QB,LoopName,ActOnName,AppTp,XSpcTerms,AClause)
	:-
	dmember(quit_alias(Act), XSpcTerms),
	!,
	AClause = (AHead :- Body),
	(var(LInfoVar) ->
		LInfoVar = GInfoVar
		;
		true
	),
	AHead =.. [ActOnName, Act, LInfoVar],
	dmember(banner=Banner,XSpcTerms),
	catenate(['Exiting ',Banner,'\n'],Msg),
	(QB = true ->
		Body = printf(Msg,[])
		;
		Body = (QB,printf(Msg,[]))
	).

	%%
	%% general -- socket
	%%
build_act(Act,LInfoVar,GInfoVar,Body,LoopName,ActOnName,socket,XSpcTerms,AClause)
	:-!,
	server_vars(ServerVars),
	(dmember(top_catch = ErrExpPred, XSpcTerms) ->
		AClause = (AHead :- CatchBody),
		CatchBody = catch(FullBody, CBall, ErrExp),
		ErrExp =.. [ErrExpPred, CBall | ServerVars],
		CBall = '%lettervar%'('Ball')
		;
		AClause = (AHead :- FullBody)
	),
	(var(LInfoVar) ->
		LInfoVar = GInfoVar
		;
		true
	),
			%%  act_on_server_request(Request,SR,SW,InState,OutState,Flag,SInfo)
	AHead =.. [ActOnName, Act | ServerVars],
	Act =.. [_ | ActArgs],
	Body =.. [BodyPred | BodyArgs],
	append([ActArgs, BodyArgs, ServerVars], FullBodyArgs),
	FullBody =.. [BodyPred | FullBodyArgs].

	%%
	%% general -- tty
	%%
build_act(Act,LInfoVar,GInfoVar,Body,LoopName,ActOnName,AppTp,XSpcTerms,AClause)
	:-
	(dmember(top_catch = top_catch(CBall,LInfoVar,ErrExp), XSpcTerms) ->
		AClause = (AHead :- CatchBody, ALoop),
		CatchBody = catch(Body, CBall, ErrExp),
		CBall = '%lettervar%'('Ball')
		;
		AClause = (AHead :- Body, ALoop)
	),
	(var(LInfoVar) ->
		LInfoVar = GInfoVar
		;
		true
	),
	AHead =.. [ActOnName, Act, LInfoVar],
	ALoop =.. [LoopName, LInfoVar].

/*-------------------------------------------------------------------------------*
 |	For socket servers:
 *-------------------------------------------------------------------------------*/

server_vars( [
		'%lettervar%'('SR'),
		'%lettervar%'('SW'),
		'%lettervar%'('State'),
		'%lettervar%'('Flag'),
		'%lettervar%'('SInfo') ] ).

server_vars_states([_,_,State,_,_], State).
server_vars_streams([SR,SW,_,_,_], SR, SW).
server_vars_sinfo([_,_,_,_,SInfo], SInfo).


/*-------------------------------------------------------------------------------*
 *-------------------------------------------------------------------------------*/

	%% Type file explicitly specified, and it exists -- make 
	%% sure it has %% reasonable contents, & if so, do nothing:
gen_type_spec(XSpcTerms, SrcFileDir, MakePred, AppTp)
	:-
	(AppTp = socket -> 
		TypeName = server_info
		;
		dmember(type_name=TypeName, XSpcTerms)
	),
	dmember(type_file=TypeFileName, XSpcTerms),

%	search_for_file(TypeFileName, TypeFile), 
	pathPlusFile(_,BaseTypeFileName, TypeFileName),
	pathPlusFile(SrcFileDir,BaseTypeFileName,TypeFile),

	exists_file(TypeFile),
	!,
	grab_terms(TypeFile, ISTerms),
	(dmember(defStruct(TypeName, Props), ISTerms)
		->
		(dmember(makePred = MakePred, Props) ->
			true
			;
			printf('Error! - Can''t locate makePred=<> in defStruct(%t,[...])\n', 
						[TypeName])
		)
		;
		printf('Error! - Can''t locate defStruct(%t,[...]) in file %t\n', 
					[TypeName,TypeFile])
	).

	%% Type file explicitly specified, but doesn't exist:
gen_type_spec(XSpcTerms, SrcFileDir, MakePred, AppTp)
	:-
	dmember(type_name=TypeName, XSpcTerms),
	dmember(type_file=TypeFile, XSpcTerms),
	!,
	printf('Error! - File %t does not exist!\n', [TypeFile]).

	%% Type name explicitly specified, but type file not
	%% specified in source specifications:
gen_type_spec(XSpcTerms, SrcFileDir, foobar, AppTp)
	:-
	dmember(type_name=TypeName, XSpcTerms),
	!,
	printf('Error! - Missing type file name (type_file = <name>_\n', []).

	%% Nothing about type file specified in source specifications;
	%% Generate the appropriate type file:
gen_type_spec(XSpcTerms, SrcFileDir, MakePred, AppTp)
	:-
	get_user_info_slots(XSpcTerms, SrcFileDir, UserInfoSlots),
	standard_info_slots(SIS),
	append(UserInfoSlots, SIS, PropsList),

	dmember(name=Name, XSpcTerms),
	dmember(type_root=Root, XSpcTerms),
	StructLabel = Root,
	catenate(access,Root,AccessPred),
	catenate(set,Root,SetPred),
	catenate(make,Root,MakePred),

	DefList = [
		propertiesList = PropsList,
		accessPred = AccessPred,
		setPred = SetPred,
		makePred = MakePred,
		structLabel = StructLabel ],

	dmember(type_file=TypeFileName, XSpcTerms),

	pathPlusFile(_,BaseTypeFileName, TypeFileName),
	pathPlusFile(SrcFileDir,BaseTypeFileName,TypeFile),

	dmember(module=Mod, XSpcTerms),

	open(TypeFile, write, TFS, []),
	dmember(insrcfile=InSrcFile, XSpcTerms),
	gen_file_header(TFS, InSrcFile, TypeFile),
	write_clause(TFS, (module Mod), [quoted(true)]), nl(TFS),
	write_defStruct(TFS, defStruct(Name, DefList)),
	write_clause(TFS, endmod ), nl(TFS),
	close(TFS),

			%% Generate makefile.typ:
			%% needs to be dependent on options:
	dmember(base_type_file=BaseTypeFile, XSpcTerms),
	file_extension(BaseTypeFile,pro,ProTypeFile),
	sprintf(atom(DepLine),'%t: %t',[ProTypeFile,TypeFile]),
	open('makefile.typ',read_write,MTOS,[]),
	(locate_line(DepLine, MTOS) ->
		close(MTOS) 		%% nothing to do
		;
		close(MTOS),
		open('makefile.typ',append,MTOS2,[]),
		printf(MTOS2,'\n\n%t: %t\n',[ProTypeFile,TypeFile]),
		close(MTOS2)
	).

/*-------------------------------------------------------------------------------*
 *-------------------------------------------------------------------------------*/

locate_line(Atom, Stream)
	:-
	atom_length(Atom, Len),
	locate_line(Atom, Len, Stream).

locate_line(Atom, Len, Stream)
	:-
	get_line(Stream, NextLine),
	(sub_atom(NextLine, 0, Len, _, Atom) ->
		true
		;
		locate_line(Atom, Len, Stream)
	).

/*-------------------------------------------------------------------------------*
 *-------------------------------------------------------------------------------*/

get_user_info_slots(XSpcTerms, SrcFileDir, UserInfoSlots)
	:-
	dmember(info_slots=UserInfoSlots0, XSpcTerms),
	!,
	fin_user_info_slots(UserInfoSlots0, SrcFileDir, UserInfoSlots).

get_user_info_slots(_, SrcFileDir, []).

fin_user_info_slots(include(InfoSrcFile), SrcFileDir, UserInfoSlots)
	:-
	(WorkingInfoSrcFile = InfoSrcFile ;
		pathPlusFile(_,BaseInfoSrcFile,InfoSrcFile),
		pathPlusFile(SrcFileDir,BaseInfoSrcFile,WorkingInfoSrcFile)
	),
	exists_file(WorkingInfoSrcFile),
	open(WorkingInfoSrcFile,read,ISFS,[]),
	read_terms(ISFS, ISFSTerms),
	close(ISFS),
	dmember(info_slots=UserInfoSlots, ISFSTerms).

fin_user_info_slots(include(_), SrcFileDir, []) :-!.

fin_user_info_slots(include(InfoSrcFile,DName), SrcFileDir, UserInfoSlots)
	:-
	(WorkingInfoSrcFile = InfoSrcFile ;
		pathPlusFile(_,BaseInfoSrcFile,InfoSrcFile),
		pathPlusFile(SrcFileDir,BaseInfoSrcFile,WorkingInfoSrcFile)
	),
	exists_file(WorkingInfoSrcFile),
	open(WorkingInfoSrcFile,read,ISFS,[]),
	read_terms(ISFS, ISFSTerms),
	close(ISFS),
	dmember(defStruct(DName, DList), ISFSTerms),
	dmember(propertiesList = UserInfoSlots, DList).

fin_user_info_slots(include(_,_), SrcFileDir, []) :-!.

fin_user_info_slots(UserInfoSlots, SrcFileDir, UserInfoSlots).

/*-------------------------------------------------------------------------------*
 *-------------------------------------------------------------------------------*/

write_defStruct(TFS, defStruct(Name, DefList))
	:-
	printf(TFS,'\ndefStruct(%t, [\n',[Name],[quoted(true)]),
	printf(TFS,'\tpropertiesList = [\n',[]),
		%% make sure properties are the first entry:
	list_delete(DefList, propertiesList=Props, DefList0),
	write_struct_items([propertiesList=Props | DefList0],TFS),
	printf(TFS,'\t]  ).\n\n',[]).

write_struct_items([],TFS).
write_struct_items([propertiesList=PList | DefList],TFS)
	:-!,
	write_struct_slots(PList,TFS),
	write_struct_items(DefList,TFS).
write_struct_items([Item | DefList],TFS)
	:-
	(DefList = [] ->
		printf(TFS, '\t%t\n',[Item],[quoted(true)])
		;
		printf(TFS, '\t%t,\n',[Item],[quoted(true)])
	),
	write_struct_items(DefList,TFS).
	
write_struct_slots([],TFS).
write_struct_slots([Slot | DefList],TFS)
	:-
	write_slot(Slot,DefList,TFS),
	write_struct_slots(DefList,TFS).

	%% 2nd arg tells us whether we are at end of list:
	%% difference is the trailing comma:
write_slot('$cmt'(Slot,Cmt), [], TFS)
	:-!,
	printf(TFS, '\t\t%t\t\t%%%% %t\n\t\t\t],\n',[Slot,Cmt],[quoted(true)]).

write_slot('$cmt'(Slot), _, TFS)
	:-
	printf(TFS, '\t\t%t,\t\t%%%% %t\n',[Slot,Cmt],[quoted(true)]).

write_slot(Slot, [], TFS)
	:-
	printf(TFS, '\t\t%t\n\t\t\t],\n',[Slot],[quoted(true)]).

write_slot(Slot, _, TFS)
	:-
	printf(TFS, '\t\t%t,\n',[Slot],[quoted(true)]).

/*-------------------------------------------------------------------------------*
 *-------------------------------------------------------------------------------*/

recons_fix([], []).
recons_fix([File | Consults], [-File | ReConsults])
	:-
	recons_fix(Consults, ReConsults).

/*-------------------------------------------------------------------------------*
 *-------------------------------------------------------------------------------*/

make_banner(Name,BannerName)
	:-
	atom_codes(Name, NCs),
	NCs = [C1 | RestCs],
	make_uc([C1], [UCC1]),
	rest_banner_cs(RestCs, RestBNCs),
	BNCs = [UCC1 | RestBNCs],
	atom_codes(BannerName, BNCs).

rest_banner_cs([], []).
rest_banner_cs([0'_, C | RestCs], [UCC | RestBNCs])
	:-!,
	make_uc([C], [UCC]),
	rest_banner_cs(RestCs, RestBNCs).
rest_banner_cs([0'_], []) :-!.
rest_banner_cs([C | RestCs], [C | RestBNCs])
	:-
	rest_banner_cs(RestCs, RestBNCs).

/*-------------------------------------------------------------------------------*
 *-------------------------------------------------------------------------------*/

standard_info_slots([
		in_stream/user_input,
		out_stream/user_output

	]).


/*********************

gen_type_spec(XSpcTerms, MakePred)
	:-
%	dmember(type_name=TypeName, XSpcTerms),
TypeName = server_info,
	dmember(type_file=TypeFile, XSpcTerms),
	exists_file(TypeFile),
	!,
	open(TypeFile, read, IS, []),
	read_terms(IS, ISTerms),
	close(IS),
	(dmember(defStruct(TypeName, Props), ISTerms)
		->
		(dmember(makePred = MakePred, Props) ->
			true
			;
			printf('Error! - Can''t locate makePred=<> in defStruct(%t,[...])\n', 
						[TypeName])
		)
		;
		printf('Error! - Can''t locate defStruct(%t,[...]) in file %t\n', 
					[TypeName,TypeFile])
	).

gen_type_spec(XSpcTerms, MakePred)
	:-
	dmember(type_name=TypeName, XSpcTerms),
	dmember(type_file=TypeFile, XSpcTerms),
	!,
	printf('Error! - File %t does not exist!\n', [TypeFile]).

gen_type_spec(XSpcTerms, foobar)
	:-
	dmember(type_name=TypeName, XSpcTerms),
	!,
	printf('Error! - Missing type file name (type_file = <name>_\n', []).

gen_type_spec(XSpcTerms, MakePred)
	:-
	get_user_info_slots(XSpcTerms, UserInfoSlots),
	standard_info_slots(SIS),
	append(UserInfoSlots, SIS, PropsList),

	dmember(name=Name, XSpcTerms),
	dmember(type_root=Root, XSpcTerms),
	StructLabel = Root,
	catenate(access,Root,AccessPred),
	catenate(set,Root,SetPred),
	catenate(make,Root,MakePred),

	DefList = [
		propertiesList = PropsList,
		accessPred = AccessPred,
		setPred = SetPred,
		makePred = MakePred,
		structLabel = StructLabel ],

	dmember(type_file=TypeFile, XSpcTerms),
	dmember(module=Mod, XSpcTerms),

	open(TypeFile, write, TFS, []),
	dmember(insrcfile=InSrcFile, XSpcTerms),
	gen_file_header(TFS, InSrcFile, TypeFile),
	write_clause(TFS, (module Mod), [quoted(true)]), nl(TFS),
	write_defStruct(TFS, defStruct(Name, DefList)),
	write_clause(TFS, endmod ), nl(TFS),
	close(TFS),

	dmember(base_type_file=BaseTypeFile, XSpcTerms),
	file_extension(BaseTypeFile,pro,ProTypeFile),
	sprintf(atom(DepLine),'%t: %t',[ProTypeFile,TypeFile]),
	open('makefile.typ',read_write,MTOS,[]),
	(locate_line(DepLine, MTOS) ->
		close(MTOS) 		%% nothing to do
		;
		close(MTOS),
		open('makefile.typ',append,MTOS2,[]),
		printf(MTOS2,'\n\n%t: %t\n',[ProTypeFile,TypeFile]),
		close(MTOS2)
	).

synth_unit(info_type, XSpcTerms, Code, CodeTail)
	:-!,
	gen_type_spec(XSpcTerms, MakePred),
	
	Code = [SetupInfo,nl | CodeTail],
	(dmember(init = InitCode-InfoVar, XSpcTerms) ->
		true
		;
		InitCode = nil
	),
	InfoVar = '%lettervar%'('Info'),
	MakeCall =.. [MakePred, InfoVar],
	dmember(global_var=GVarName, XSpcTerms),
	catenate(set,GVarName,GVSetPred),
	InstallCall =..[GVSetPred, InfoVar],
	(InitCode = nil ->
		SetupBody = (MakeCall,InstallCall)
		;
		SetupBody = (MakeCall,InitCode,InstallCall)
	),
	SetupInfo = 
	( setup_initial_info('%lettervar%'('Info')) 
		:-
		SetupBody ).
*********************/
	
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% MAKEFILE GENERATION STUFF
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main_makefile(MainMakefile)
	:-
	exists_file(MainMakefile),
	!.
main_makefile(MainMakefile)
	:-
	open(MainMakefile,write,MOS,[]),
	printf(MOS,'%%.pro:%%.typ\n',[]),
	printf(MOS,'\talspro -g comptype_cl -p $<\n\n',[]),
	printf(MOS,'%%.pro:%%.spc\n',[]),
	printf(MOS,'\talspro -g mk_tty_shell_cl -p $<\n\n',[]),
	printf(MOS,'%%.pro:%%.oop\n',[]),
	printf(MOS,'\talspro -obp -g objectProcessFile_cl -p -oopf $<\n\n',[]),
	printf(MOS,'\ninclude makefile.typ\n\n',[]),
	printf(MOS,'\ninclude makefile.spc\n\n',[]),
	close(MOS).

spc_makefile(SpcMakefile,SrcFile,BaseSrcFile,TgtFile)
	:-
	exists_file(SpcMakefile),
	!,
	grab_lines(SpcMakefile, Lines),
	(present_dep(Lines, TgtFile, BaseSrcFile,SrcFile) ->
		true
		;
		open(SpcMakefile, append, MSO, []),
		printf(MSO,'\n\n%t: %t\n\n',[TgtFile,SrcFile]),
		close(MSO)
	).

spc_makefile(SpcMakefile,SrcFile,BaseSrcFile,TgtFile)
	:-
	open(SpcMakefile, write, MSS, []),
	printf(MSS,'%t: %t\n\n',[TgtFile,SrcFile]),
	close(MSS).

present_dep(Lines, TgtFile, BaseName, SrcFile)
	:-
	file_extension(_, TgtExt, TgtFile),
	file_extension(_, SrcExt, SrcFile),
	present_dep0(Lines, BaseName, TgtExt, SrcExt).

present_dep0([Line | Lines], BaseName, TgtExt, SrcExt)
	:-
	asplit(Line, 0':, Left, Right),
	atomread(Left, [BaseName | TgtExt]),
 	read_as_list(Right, DepsList,[]),
	file_extension(BaseName, SrcExt, FileName),
	dmember(FileName, DepsList),
	!.

present_dep0([_ | Lines], BaseName, TgtExt, SrcExt)
	:-
	present_dep0(Lines, BaseName, TgtExt, SrcExt).





export setup_ttyshl/0.
export setup_ttyshl/2.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Initial Shell Spec Setup Wizard
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*!-----------------------------------------------------------------------
 |	setup_ttyshl/0
 |	setup_ttyshl
 |	setup_ttyshl
 |
 |	- interactive setup of tty shell specifications, on standard in/out
 *-----------------------------------------------------------------------*/
setup_ttyshl
	:-
	setup_ttyshl(user_input,user_output).

/*!-----------------------------------------------------------------------
 |	setup_ttyshl/2
 |	setup_ttyshl(IS, OS)
 |	setup_ttyshl(+, +)
 |
 |	- interactive setup of tty shell specifications, i/o on IS/OS
 *-----------------------------------------------------------------------*/
setup_ttyshl(IS,OS)
	:-
	printf(OS, 'Creating ttyshl spec:\n',[]),
	printf(OS, 'System name = ', []),
	get_line(IS, Name0),
	strip_clean(Name0, Name),

	get_value_for(OS,IS,'System defStruct name [%t]=',Name,TypeName),
	(TypeName \= Name ->
		DefTypeFile = TypeName
		;
		catenate(TypeName, '_typ.typ', DefTypeFile)
	),
	get_value_for(OS,IS,'defStruct file name [%t]=',DefTypeFile,TypeFile),
	get_value_for(OS,IS,'Module name [%t]=',Name,Module),
	get_value_for(OS,IS,'Start (entry) predicate [%t]=',start,Start),

	printf(OS, 
			'Other modules to use [hit return for blank line when finished]:\n',
			[]),
	get_lines_to_blank(OS,IS,'>',UseList),

	printf(OS, 
			'Files to consult [hit return for blank line when finished]:\n',
			[]),
	get_lines_to_blank(OS,IS,'>',ConsultList),

	catenate(Name, '_init', DefInitPred),
	get_value_for(OS,IS,'Initialization predicate [%t]=',DefInitPred,InitPred),

	ValList = [
		name = Name,
		type_name	= TypeName,
		type_file	= TypeFile,
		module 		= Module,
		start		= Start,
		use			= UseList,
		consults	= ConsultList,
		info_initialize = InitPred,
		banner = ' X X X - M A N A G E M E N T ',
		choice_banner = 'Choose menu item =====>\n'
		],

	build_tty_spc_files(ValList,OS,IS).

get_value_for(OS,IS,Pattern,Default,Result)
	:-
	printf(OS, Pattern, [Default]),
	get_line(IS, Result0),
	disp_get_value_for(Result0, OS,IS,Pattern,Default,Result).

disp_get_value_for('', OS,IS,Pattern,Default,Default) :-!.
disp_get_value_for(Result0, OS,IS,Pattern,Default,Result)
	:-
	strip_clean(Result0, Result).

get_lines_to_blank(OS,IS,Prompt,ConsultList)
	:-
	write(OS, Prompt),
	get_line(IS, Result0),
	disp_get_lines_to_blank(Result0,OS,IS,Prompt,ConsultList).

disp_get_lines_to_blank('',OS,IS,Prompt,[]) :-!.
disp_get_lines_to_blank(Result0,OS,IS,Prompt,[Result | ConsultList])
	:-
	strip_clean(Result0, Result),
	get_lines_to_blank(OS,IS,Prompt,ConsultList).

strip_clean(Result0, Result)
	:-
	atom_codes(Result0, N0Cs),
	strip_white(N0Cs, N1Cs),
	dreverse(N1Cs, RN1Cs),
	strip_white(RN1Cs, RN2Cs),
	(RN2Cs = [0'. | RN3Cs] -> true ; RN3Cs = RN2Cs),
	dreverse(RN3Cs, N3Cs),
	atom_codes(Result, N3Cs).

build_tty_spc_files(ValList,OS,IS)
	:-
	dmember(name=Name, ValList),
	file_extension(Name,spc,SpecFile),
	dmember(type_file=InitTypeFile, ValList),
	(file_extension(BaseFile,typ,InitTypeFile) ->
		TypeFile = InitTypeFile
		;
		file_extension(InitTypeFile,typ,TypeFile)
	),
	printf(OS, 'Writing %t ...',[TypeFile]),
	build_type_file(Name,TypeFile,ValList),
	printf(OS, '...done\n', []),

	printf(OS, 'Writing %t ...',[makefiles]),
	build_makefiles(Name,TypeFile,SpecFile,ValList),
	printf(OS, '...done\n', []),

	printf(OS, 'Writing %t ...',[SpecFile]),
	build_specfile(Name, SpecFile, TypeFile, ValList),
	printf(OS, '...done\n', []),

	printf(OS, '\nFinished.\n', []).





build_type_file(Name,TypeFile,ValList)
	:-
	dmember(type_name = TypeName, ValList),
	dmember(module = Module, ValList),

	atom_codes(TypeName, TNCs),
	at_most_n(TNCs, 5, TNC5),
	make_uc(TNC5, UCTNC5),
	atom_codes(AccessID, UCTNC5),

	open(TypeFile, write, TypeOS, []),
	gen_file_header(TypeOS,interaction,TypeFile),

	printf(TypeOS, '\nmodule %t.\n\n',[Module]),
	printf(TypeOS,'defStruct(%t,[\n',[TypeName]),
	printf(TypeOS, '\tpropertiesList = [\n',[]),
	printf(TypeOS, '\t\tcur_new_files_list/[],\n',[]),
	printf(TypeOS, '\t\tin_stream/user_input,\n',[]),
	printf(TypeOS, '\t\tout_stream/user_output\n',[]),
	printf(TypeOS, '\t\t],\n',[]),
	printf(TypeOS, '\taccessPred = access%tinfo,\n',[AccessID]),
	printf(TypeOS, '\tsetPred = set%tinfo,\n',[AccessID]),
	printf(TypeOS, '\tmakePred = make%tinfo,\n',[AccessID]),
	printf(TypeOS, '\tstructLabel = %t\n',[TypeName]),
	printf(TypeOS, '\t]  ).\n',[]),
	printf(TypeOS, '\nendmod.\n',[]),

	close(TypeOS).

build_makefiles(Name,TypeFile,SpecFile,ValList)
	:-
	file_extension(TypeBase,typ,TypeFile),
	file_extension(TypeBase,pro,TypePro), 

	open('makefile.typ', write, MTOS, []),
	printf(MTOS, 'TYPES=%t\n\n', [TypePro]),
	printf(MTOS, 'types: $(TYPES)\n\n', []),
	printf(MTOS, '%t: %t\n', [TypePro, TypeFile]),
	close(MTOS),

	file_extension(Name,spc,SpecFile),
	file_extension(Name,pro,SpecProFile),
	open('makefile.spc', write, MSOS, []),
	printf(MSOS, '%t: %t\n', [SpecProFile, SpecFile]),
	close(MSOS),


	open(makefile, write, MFOS, []),
	printf(MFOS, '#\n#   Makefile for %t programs\n#\n\n',[Name]),
	printf(MFOS, 'ALSPRO=alspro\n\n', []),

	printf(MFOS, '\n%.pro:%.typ\n',[]),
	printf(MFOS, '\t$(ALSPRO) -b -g comptype_cl -p $<',[]),
	printf(MFOS, '\n%.pro:%.spc\n',[]),
	printf(MFOS, '\t$(ALSPRO) -b -g mk_tty_shell_cl -p $<',[]),
	printf(MFOS, '\n%.pro:%.oop',[]),
	printf(MFOS, '\t$(ALSPRO) -b -obp -g objectProcessFile_cl -p -oopf $<\n',[]),
	printf(MFOS, '\n\ninclude makefile.typ\n',[]),
	printf(MFOS, '\n\ninclude makefile.spc\n',[]),
	close(MFOS).

build_specfile(Name, SpecFile, TypeFile, ValList)
	:-
	open(SpecFile, write, SFOS, []),
	CmtCmd = printf(SFOS,CCPattern,CCArgs),
	CCPattern = 
	  '\n\t%t program shell\n\n\tProper command line to build code from this is:\n\n\t\tmk_tty_shell_cl -p %t.spc\n\n\tUsing the makefile, just:\n\t\tmake %t.pro\n\n',
	CCArgs = [Name,Name,Name],
	gen_file_header(SFOS,interaction,SpecFile,CmtCmd),

	write_clauses(SFOS, ValList),
	nl(SFOS),
	write_clauses(SFOS, [loop_notice=nil]),
	printf(SFOS,'action(q,\'Quit to Prolog\', Info)\t= quit_to_prolog(Info).\n',[]),
	printf(SFOS,'action(h,\'Halt Prolog to OS\', Info)\t= exit_to_os(Info).\n',[]),

	close(SFOS).

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
a_message(Quiet, Message)
	:-
	a_message(Quiet, Message, []).

	%% true = "be quiet" - no messages
a_message(true, Message, Values) :-!.

	%% false = "be noisy" - write messages
a_message(false, Message, Values)
	:-
	printf(Message,Values),
	flush_output.

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
raise_mks_xcept(Pattern, Args)
	:-
	throw(mk_shell_error(Pattern, Args)).

mk_shell_err_h(mk_shell_error(Pattern,Args), SrcFile, BaseSrcFile, OpStr, Options)
	:-!,
	quiet(OpStr, Quiet),
	a_message(Quiet, Pattern, Args).

mk_shell_err_h(Ball,SrcFile, BaseSrcFile, OpStr, Options)
	:-
	throw(Ball).

/*
search_for_file(File, File).

search_for_file(FileName, File)
	:-
	builtins:searchdir(SearchDir),
	pathPlusFile(SearchDir,FileName,File).
*/

endmod.
