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

module ttyshlmk.

export mk_tty_shell_cl/0.
export mk_tty_shell_list/1.
export ttyshlmk/1.
export mk_tty_shell/1.
export mk_tty_shell/4.
export setup_ttyshl/0.
export setup_ttyshl/2.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Run it from the command line - format:
	%%	alspro -p File1 File2 ...
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*!-----------------------------------------------------------------------
 |	mk_tty_shell_cl/0
 |	mk_tty_shell_cl
 |	mk_tty_shell_cl
 |
 |	- invoke command line version of tty shell spec processing
 |
 |	Invoke command line version of tty shell spec processing; takes
 |	all entries rightwards of -p on the command line as being files
 |	to be processed.
 *-----------------------------------------------------------------------*/
mk_tty_shell_cl
	:-
	builtins:command_line(Files),
	mk_tty_shell_list(Files).

/*!-----------------------------------------------------------------------
 |	mk_tty_shell_list/1
 |	mk_tty_shell_list()
 |	mk_tty_shell_list(+)
 |
 |	- perform tty shell spec processing on a list of files
 *-----------------------------------------------------------------------*/
mk_tty_shell_list([]).
mk_tty_shell_list([File | Files])
	:-
	mk_tty_shell(File),
	mk_tty_shell_list(Files).

/*!-----------------------------------------------------------------------
 |	ttyshlmk/1
 |	ttyshlmk(InSrcFile)
 |	ttyshlmk(+)
 |
 |	- perform tty shell spec processing on a file
 *-----------------------------------------------------------------------*/
ttyshlmk(InSrcFile)
	:-
	mk_tty_shell(InSrcFile).

/*!-----------------------------------------------------------------------
 |	mk_tty_shell/1
 |	mk_tty_shell(InSrcFile)
 |	mk_tty_shell(+)
 |
 |	- perform tty shell spec processing on a file
 *-----------------------------------------------------------------------*/
mk_tty_shell(InSrcFile)
	:-
		%% setup correct file names:
	(filePlusExt(BaseSrcFile,spc,InSrcFile) ->
		SrcFile = InSrcFile
		;
		filePlusExt(InSrcFile,spc,SrcFile),
		BaseSrcFile = InSrcFile
	),
	filePlusExt(BaseSrcFile,pro,TgtFile),
	mk_tty_shell(SrcFile,TgtFile, BaseSrcFile, []),
	!,
	printf('Spc file %t processed.\n',[InSrcFile]).

mk_tty_shell(InSrcFile)
	:-
	printf('Warning! Error in spc file %t -- not processed\n',[InSrcFile]).

mk_tty_shell(SrcFile, TgtFile, BaseSrcFile, Options)
	:-
	exists_file(SrcFile),
	!,
	(dmember(quiet(Quiet), Options),!; Quiet=false),
	(comp_file_times(SrcFile, TgtFile) ->
			%% TgtFile exists & SrcFile is older:
		true
		;
		a_message(Quiet, 'Processing spc file %t...\n', [SrcFile]),
		do_mk_tty_shell(SrcFile, TgtFile, BaseSrcFile, Options),
		a_message(Quiet, 'Done with spc file %t.\n', [SrcFile])
	).

mk_tty_shell(SrcFile, TgtFile, BaseSrcFile, Options)
	:-
	(dmember(quiet(Quiet), Options),!; Quiet=false),
	a_message(Quiet, '!!Error: File %t does not exist!\n',[SrcFile]).

do_mk_tty_shell(SrcFile, TgtFile, BaseSrcFile, Options)
	:-
	open(SrcFile,read,InS,[]),
	read_terms(InS, SpcTerms),
	close(InS),

	pathPlusFile(SrcFileDir,_,SrcFile),

	pathPlusFile(TgtDir, _, TgtFile),
	synth_shell_units(SpcTerms, SrcFile, TgtFile, SrcFileDir),

	pathPlusFile(TgtDir, makefile, MainMakefile),
	main_makefile(MainMakefile),

	pathPlusFile(TgtDir, 'makefile.spc', SpcMakefile),
	spc_makefile(SpcMakefile,SrcFile,BaseSrcFile,TgtDir).

		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%% Items to check for, and set defaults
		%% for if not present:
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tty_shell_items([
	name,
	module,
	use,
	global_var,
	banner,
	type_file,
	type_root,
	type_name,
	start,
	menu_type,
	consults
	]).

synth_shell_units(SpcTerms, InSrcFile, TgtFile, SrcFileDir)
	:-
	dmember(name=Name,SpcTerms),
	!,
		%% handle defaults:
	tty_shell_items(ItemsList),
	ssu_defaults(ItemsList,SpcTerms,Name,XSpcTerms0),

		%% merge (new) defaults with other items from
		%% source list which are not touched:
	merge_tagged_lists(XSpcTerms0, [insrcfile=InSrcFile | SpcTerms], XSpcTerms),

		%% synthsize the code:
	tty_shell_units(UnitsList),
	ssu(UnitsList, XSpcTerms, SrcFileDir, XSpcCode),

		%% output code:
	open(TgtFile, write, TgtS, []),
	gen_file_header(TgtS, InSrcFile, TgtFile),
	write_clauses(TgtS, XSpcCode, [quoted(true)]),
	close(TgtS).

synth_shell_units(_, _, _, _, _)
	:-
	(dmember(quiet(Quiet), Options),!; Quiet=false),
	a_message(Quiet, 'Error: No name for shell!  Exiting.\n',[]).

ssu_defaults([], _, _, []).
ssu_defaults([Item | ItemList], SpcTerms, Name, XSpcTerms)
	:-
	synth_unit_dflt(Item, SpcTerms, Name, XSpcTerms, XSpcTermsTail),
	ssu_defaults(ItemList, SpcTerms, Name, XSpcTermsTail).


/*----------------------------
	synth_unit_dflt(Unit, SpcTerms, Name, XSpcTerms, XSpcTermsTail)

	name,
	module,
	use,
	global_var,
	banner,
	type_file,
	type_root,
	type_name,
	start,
	menu_type,
	consults
	]).
 *---------------------------*/
synth_unit_dflt(name, SpcTerms, Name, [name=Name | Tail], Tail)
	:-!.

synth_unit_dflt(module, SpcTerms, Name, [module=ModName | Tail], Tail)
	:-!,
	(dmember(module=ModName,SpcTerms) ->
		true
		;
		ModName = Name
	).

synth_unit_dflt(use, SpcTerms, Name, [use=UseList | Tail], Tail)
	:-!,
	(dmember(use=UseList,SpcTerms) ->
		true
		;
		UseList = []
	).

synth_unit_dflt(global_var, SpcTerms, Name, [global_var=GVarName | Tail], Tail)
	:-!,
	(dmember(global_var=GVarName,SpcTerms) ->
		true
		;
		make_uc_sym(Name,UCName),
		catenate(UCName,'_GV',GVarName)
	).

synth_unit_dflt(banner, SpcTerms, Name, 
				[banner=BannerName,choice_banner=ActionBanner | Tail], Tail)
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
			 act_on = ActOnName | Tail], Tail)
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
					[type_file=FullTypeFile, base_type_file=BaseTypeFile | Tail], Tail)
	:-!,
	(dmember(type_file=TypeFile,SpcTerms) ->
		(filePlusExt(BaseTypeFile,typ,TypeFile) ->
			FullTypeFile = TypeFile
			;
			filePlusExt(TypeFile,typ,FullTypeFile),
			BaseTypeFile = TypeFile
		)
		;
		dmember(name=Name, SpcTerms),
		make_type_file(Name,BaseTypeFile,FullTypeFile)
	).

synth_unit_dflt(type_root, SpcTerms, Name, [type_root=Root | Tail], Tail)
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

synth_unit_dflt(type_name, SpcTerms, _, XTerms, Tail)
	:-!,
	(dmember(type_name=TypeName, SpcTerms) ->
		XTerms =  [type_name = TypeName | Tail]
		;
		XTerms = Tail
	).

synth_unit_dflt(menu_type, SpcTerms, Name, [menu_type=MenuType | Tail], Tail)
	:-!,
	(dmember(action(_,_)=_, SpcTerms) ->
		MenuType = default_numbered
		;
		MenuType = user_coded
	).

synth_unit_dflt(consults, SpcTerms, Name, [consults=FullConsults | Tail], Tail)
	:-!,
	(dmember(consults=Consults,SpcTerms) ->
		true
		;
		Consults = []
	),
	standard_consults(StCons),
	append(Consults, StCons, XCons),
		%% get rid of duplicates:
	sort(XCons, FullConsults).

standard_consults([]).

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
	%%%% SYNTHESIZE THE CODE
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% Code units to synthesize:
tty_shell_units([
	module,
	use,
	global_var,
	start,
	info_type,
	stock_code,
	actions
	]).

	%% Recurse down the list of units, builting each code
	%% fragment:
ssu([], _, _, 
		[
		 (quit_to_prolog(_) :- system('rm *.obp'),
				   printf('Exiting to Prolog...\n',[]), !, fail),
		 (exit_to_os(_) :- system('rm *.obp'),
				printf('Exiting to Operating System...\n',[]), !, halt),
		 endmod ]).

ssu([Unit | UnitList], XSpcTerms, SrcFileDir, XSpcCode)
	:-
	synth_unit(Unit, XSpcTerms, SrcFileDir, XSpcCode, XSpcCodeTail),
	ssu(UnitList, XSpcTerms, SrcFileDir, XSpcCodeTail).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Synthesis for each unit fragment:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

synth_unit(module, XSpcTerms, SrcFileDir, Code, CodeTail)
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

synth_unit(use, XSpcTerms, SrcFileDir, Code, CodeTail)
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

synth_unit(global_var, XSpcTerms, SrcFileDir, [(:- make_gv(GVarName)), nl | CodeTail], CodeTail)
	:-!,
	dmember(global_var=GVarName, XSpcTerms).

synth_unit(start, XSpcTerms, SrcFileDir, 
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

synth_unit(stock_code, XSpcTerms, SrcFileDir, Code, CodeTail)
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


synth_unit(actions, XSpcTerms, SrcFileDir, Code, CodeTail)
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
	build_act_clauses(ActList,  GInfoVar, LoopName, ActOnName, 
						CodeTail, XXSpcTerms, ActClauses),
	Code = [nl,action_list(Acts),nl | ActClauses].

synth_unit(actions, XSpcTerms, SrcFileDir, Code, CodeTail)
	:-
	bagof(action(ACd,Act,X)-B, member(action(ACd,Act,X)=B, XSpcTerms), InitActList),
	!,
	setup_quit2(InitActList, XSpcTerms, ActList, XXSpcTerms),
	bagof(A, [C,X,B]^member(action(C,A,X)-B,ActList), Acts),
	bagof(C, [A,X,B]^member(action(C,A,X)-B,ActList), Codes),

	dmember(loop=LoopName, XSpcTerms),
	dmember(act_on=ActOnName, XSpcTerms),
	GInfoVar = '%lettervar%'('Info'),
	build_act_clauses(ActList,  GInfoVar, LoopName, ActOnName, 
						CodeTail, XXSpcTerms, ActClauses),
	Code = [nl,action_list(Acts),nl,code_list(Codes) | ActClauses].

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



build_act_clauses([], _, LoopName,ActOnName,CodeTail, _, [DefaultClause | CodeTail])
	:-
	build_act(default,default, GInfoVar,default,LoopName,ActOnName,XSpcTerms, DefaultClause).

build_act_clauses([action(Act,LInfoVar)-Body | ActList], GInfoVar, LoopName,
					ActOnName,CodeTail, XSpcTerms, [AClause,nl | ActClauses])
	:-!,
	build_act(Act,LInfoVar, GInfoVar,Body,LoopName,ActOnName,XSpcTerms, AClause),
	build_act_clauses(ActList, GInfoVar, LoopName,
						ActOnName,CodeTail, XSpcTerms, ActClauses).

build_act_clauses([action(Code,Act,LInfoVar)-Body | ActList], GInfoVar, LoopName,
					ActOnName,CodeTail, XSpcTerms, [AClause,nl | ActClauses])
	:-
	build_act(Act,LInfoVar, GInfoVar,Body,LoopName,ActOnName,XSpcTerms, AClause),
	build_act_clauses(ActList, GInfoVar, LoopName,
						ActOnName,CodeTail, XSpcTerms, ActClauses).

build_act(quit,LInfoVar,GInfoVar,QB,LoopName,ActOnName,XSpcTerms,AClause)
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

build_act(default,default,GInfoVar,default,LoopName,ActOnName,XSpcTerms,DefaultClause)
	:-!,
	DefaultClause = (AHead :- Body),
	AHead =.. [ActOnName, What, GInfoVar],
	ALoop =.. [LoopName, GInfoVar],
	(What = '$badInput$'(OutWhat) -> true ; OutWhat = What),
	Body = (printf('Unknown request: %t\n\n',[OutWhat]), ALoop).

build_act(Act,LInfoVar,GInfoVar,QB,LoopName,ActOnName,XSpcTerms,AClause)
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

build_act(Act,LInfoVar,GInfoVar,Body,LoopName,ActOnName,XSpcTerms,AClause)
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



gen_type_spec(XSpcTerms, SrcFileDir, MakePred)
	:-
	dmember(type_name=TypeName, XSpcTerms),
	dmember(type_file=InitTypeFile, XSpcTerms),
	(TypeFile = InitTypeFile ;
		pathPlusFile(_,BaseTypeFile,InitTypeFile),
		pathPlusFile(SrcFileDir,BaseTypeFile,TypeFile)
	),
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

gen_type_spec(XSpcTerms, SrcFileDir, MakePred)
	:-
	dmember(type_name=TypeName, XSpcTerms),
	dmember(type_file=TypeFile, XSpcTerms),
	!,
	printf('Error! - File %t does not exist!\n', [TypeFile]).

gen_type_spec(XSpcTerms, SrcFileDir, foobar)
	:-
	dmember(type_name=TypeName, XSpcTerms),
	!,
	printf('Error! - Missing type file name (type_file = <name>_\n', []).

gen_type_spec(XSpcTerms, SrcFileDir, MakePred)
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
	filePlusExt(BaseTypeFile,pro,ProTypeFile),
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

synth_unit(info_type, XSpcTerms, SrcFileDir, Code, CodeTail)
	:-!,
	gen_type_spec(XSpcTerms, SrcFileDir, MakePred),
	
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
	
locate_line(Atom, Stream)
	:-
	atom_length(Atom, Len),
	locate_line(Atom, Len, Stream).

locate_line(Atom, Len, Stream)
	:-
	get_line(Stream, NextLine),
	(sub_atom(NextLine, 1, Len, _, Atom) ->
		true
		;
		locate_line(Atom, Len, Stream)
	).

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

	open(WorkingInfoSrcFile,read,ISFS,[]),
	read_terms(ISFS, ISFSTerms),
	close(ISFS),
	dmember(defStruct(DName, DList), ISFSTerms),
	dmember(propertiesList = UserInfoSlots, DList).

fin_user_info_slots(include(_,_), SrcFileDir, []) :-!.

fin_user_info_slots(UserInfoSlots, SrcFileDir, UserInfoSlots).

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

recons_fix([], []).
recons_fix([File | Consults], [-File | ReConsults])
	:-
	recons_fix(Consults, ReConsults).

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

standard_info_slots([
		in_stream/user_input,
		out_stream/user_output

	]).

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
	filePlusExt(_, TgtExt, TgtFile),
	filePlusExt(_, SrcExt, SrcFile),
	present_dep0(Lines, BaseName, TgtExt, SrcExt).

present_dep0([Line | Lines], BaseName, TgtExt, SrcExt)
	:-
	asplit(Line, 0':, Left, Right),
	atomread(Left, [BaseName | TgtExt]),
 	read_as_list(Right, DepsList,[]),
	filePlusExt(BaseName, SrcExt, FileName),
	dmember(FileName, DepsList),
	!.

present_dep0([_ | Lines], BaseName, TgtExt, SrcExt)
	:-
	present_dep0(Lines, BaseName, TgtExt, SrcExt).


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
	filePlusExt(Name,spc,SpecFile),
	dmember(type_file=InitTypeFile, ValList),
	(filePlusExt(BaseFile,typ,InitTypeFile) ->
		TypeFile = InitTypeFile
		;
		filePlusExt(InitTypeFile,typ,TypeFile)
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
	filePlusExt(TypeBase,typ,TypeFile),
	filePlusExt(TypeBase,pro,TypePro), 

	open('makefile.typ', write, MTOS, []),
	printf(MTOS, 'TYPES=%t\n\n', [TypePro]),
	printf(MTOS, 'types: $(TYPES)\n\n', []),
	printf(MTOS, '%t: %t\n', [TypePro, TypeFile]),
	close(MTOS),

	filePlusExt(Name,spc,SpecFile),
	filePlusExt(Name,pro,SpecProFile),
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

endmod.
