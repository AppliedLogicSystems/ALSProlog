/* ===========================================================*
 |				typecomp.pro
 |	Copyright (c) 1990-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		Transformer to generate data structure code for
 |		"encapsulated structure datatypes".
 |
 |	Author: Ken Bowen
 |	Date:	March, 1990
 |	Revision: Aug, 1990: Add uniform "note" generation
 |	Revision: Feb, 1992: Restructuring; & add *.mac generation
 |	Revision: Jun, 1996: Add include facility
 |	User documentation in ..manual/Tools/Structs/defstruct.tex
 * ===========================================================*/

/*--------------------------------------------------------------------------	
				Sample specification:

	defStruct(window,
		propertiesList= [
			windowName, 	% text name (atom/UIA)  of the window
			......
			special			% unused -- user can play with it
			],
		accessPred=accessWI,
		setPred= setWI,
		makePred= makeWindowStruct,
		structLabel=wi
		]).
 *-------------------------------------------------------------------------*/


module builtins.
use windows.

%% Needed for the macro stuff; now in sio_rt.pro:
%% :-op(1200,xfx,=>).		

export comptype/0.
export comptype/1.
export comptype/2.
export xcomptype/2.
export comptype/3.
export comptype_cl/0.

/*!----------------------------------------------------------
 |	comptype_cl/0
 |	comptype_cl
 |	comptype_cl
 |
 |	- invokes defStruct processing on files from command line
 |
 |	Files and switches must be rightwards of -p 
 |	All entries on the command line are taken to be names of
 |	files for defStruct processing.
 *-----------------------------------------------------------*/
comptype_cl :-
	builtins:command_line(Files),
	set_prolog_flag(unknown, fail),
	!,
	comptype_files(Files).

comptype_files([]).
comptype_files([File | Files])
	:-
	comptype(File),
	comptype_files(Files).


/*!----------------------------------------------------------
 |	comptype/0
 |	comptype
 |	comptype
 |
 |	- prompts for a file name, and invokes defStruct processing
 *-----------------------------------------------------------*/
comptype :-
	set_prolog_flag(unknown, fail),
	chooseFile(_,'*.typ',FileName),
	!,
	comptype(FileName).

comptype :-
	ct_err('Can\'t see any *.typ files!').


/*!----------------------------------------------------------
 |	comptype/1
 |	comptype(File)
 |	comptype(+)
 |	
 |	- invokes defStruct processing on File
 *-----------------------------------------------------------*/
comptype(InputFileDescrip)
	:-
	(filePlusExt(FileName, Ext, InputFileDescrip) ->
		true
		;
		FileName = InputFileDescrip, Ext = typ
	),
	(Ext \= typ ->
		ct_error('Source file must have extension .typ \n')
		;
		filePlusExt(FileName,Ext,SourceFile),
		filePlusExt(FileName,pro,TargetFile),
		comptype(SourceFile, TargetFile, [])
	).

/*!----------------------------------------------------------
 *-----------------------------------------------------------*/

xcomptype(SourceFile, Options)
	:-
	pathPlusFile(SrcFileDir,SrcFile,SourceFile),
	filePlusExt(BaseSrcFile,typ,SrcFile),
	!,
	filePlusExt(BaseSrcFile,pro,BasicTgtFile),
	pathPlusFile(SrcFileDir,BasicTgtFile,TgtFile),
	comptype(SourceFile, TgtFile, Options).

xcomptype(SourceFile, Options)
	:-
	ct_error('TypeComp: source file must have extension .typ \n').

/*!----------------------------------------------------------
 |	comptype/3
 |	comptype(SourceFile, TgtFile, Options)
 |	comptype(+, +, +)
 |	
 |	- invokes defStruct processing on SourceFile, writing to TgtFile
 |
 |	Expects TgtFile to be of form <path><sep><name>.<ext>, 
 |	ususally with <ext> = pro
 *-----------------------------------------------------------*/

comptype(SourceFile, TgtFile)
	:-
	comptype(SourceFile, TgtFile, []).

comptype(SourceFile, TgtFile, Options)
	:-
	exists_file(SourceFile),
	!,
	(comp_file_times(SourceFile, TgtFile) ->
			%% TgtFile exists & is newer than SourceFile - nothing to do
		true
		;
		do_comptype(SourceFile, TgtFile, Options)
/*
			%% Either TgtFile doesn't exist or SourceFile is newer:
		(dmember(quiet(Quiet), Options),!; Quiet=false),
		filePlusExt(NoSuffixPath, _, TgtFile),
		filePlusExt(NoSuffixPath, mac, MacFile),
		cont_comptype(TgtFile,MacFile,SourceFile, Quiet)
*/
	).

comptype(SourceFile, TgtFile, Options)
	:-
	(dmember(quiet(Quiet), Options),!; Quiet=false),
	ct_message(Quiet,'\nError: File: %t does not exist!\n',[SourceFile]).

do_comptype(SourceFile, TgtFile, Options)
	:-
	(dmember(quiet(Quiet), Options),!; Quiet=false),
	filePlusExt(NoSuffixPath, _, TgtFile),
	filePlusExt(NoSuffixPath, mac, MacFile),
	cont_comptype(TgtFile,MacFile,SourceFile, Quiet).

cont_comptype(TgtFile,MacFile,SourceFile, Quiet)
	:-
	ct_message(Quiet,'\nGenerating type definitions for: %t\n',[SourceFile]),
	finish_comptype(TgtFile,MacFile,SourceFile, Quiet),
	!,
	ct_message(Quiet,'Type generation complete for: %t\n',[SourceFile]),
	ct_message(Quiet,'Generated code written to: %t\n',[TgtFile]).

cont_comptype(TgtFile,MacFile,SourceFile, Quiet)
	:-
	ct_message(Quiet,'\nError: In type generation for: %t\n',[SourceFile]).

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
finish_comptype(TargetFile,MacFile,SourceFile,Quiet)
	:-
	open(SourceFile, read, SourceStream, []),
	read_terms(SourceStream,DefinitionsList),
	close(SourceStream),
	open(TargetFile, write, TargetStream, []),
	open(MacFile, write, MacStream, []),
	write_file_headers(TargetFile,MacFile,SourceFile,TargetStream,MacStream),
	typeDefinitions(DefinitionsList,TargetStream, MacStream,Quiet),
	close(TargetStream),
	close(MacStream).

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
write_file_headers(TargetFile,MacFile,SourceFile,TStream,MStream)
	:-
	CL1 = '/*-------------------------------------------------------------*',
	CL2 = ' *-------------------------------------------------------------*/',
	Spaces15 = '               ',
	Spaces20 = '                    ',
	printf(TStream,'%t\n',[CL1]),
	printf(TStream,'%t%t\n',[Spaces20,TargetFile]),
	printf(TStream,'%t%t\n',
				[Spaces15,'defStruct Type definitions generated from file:']),
	printf(TStream,'%t%t\n',[Spaces20,SourceFile]),
	printf(TStream,'%t%t\n',[Spaces15,'by ALS defStruct Type Generator']),
	printf(TStream,'%t%t %t\n',[Spaces15,'Macros written to file:',MacFile]),
	printf(TStream,'%t\n\n',[CL2]).

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
typeDefinitions(DefList,TgtStream,MacStream,Quiet) 
	:-
	xall(DefList,user,Extras,TgtStream,MacStream,Quiet,DefList),

	printf(TgtStream,'\nmodule utilities.\n',[]),
	pp_all(Extras,TgtStream, [quoted(true)]),
	printf(TgtStream,'endmod.\n',[]).

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/

xall([],user,[],_,_,Quiet,DefList) 
	:-!.
xall([],OtherMod,[],TgtStream,MacStream,Quiet,DefList) 
	:-!,
	printf(TgtStream,'endmod.\n',[]).
xall([Def | Defs],CurModule,[Extras | MoreExtras],TgtStream,MacStream,Quiet,DefList) 
	:-
	type_comp(Def,CurModule,NewModule,Extras,TgtStream,MacStream,Quiet,DefList),
	xall(Defs,NewModule,MoreExtras,TgtStream,MacStream,Quiet,DefList).

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
type_comp(module(Mod),_,Mod,[],TgtStream,MacStream,Quiet,DefList)
	:-!,
	printf(TgtStream, 'module utilities.\n',[],[quoted(true)]),
	printf(TgtStream, 'use %t.\n',[Mod],[quoted(true)]),
	printf(TgtStream, 'endmod.\n\n',[],[quoted(true)]),
	printf(TgtStream, 'module %t.\n',[Mod],[quoted(true)]),
	printf(TgtStream, 'use utilities.\n\n',[],[quoted(true)]).

type_comp(endmod,_,user,[],TgtStream,MacStream,Quiet,DefList)
	:-!,
	printf(TgtStream,'endmod.\n',[],[quoted(true)]).

type_comp((defStruct(TypeName,SpecsList) :- Condition,Quiet,DefList),
				CurMod,NewMod,Extras,TgtStream,MacStream)
	:-!,
	call(CurMod:Condition), !,
	type_comp(defStruct(TypeName,SpecsList),
				CurMod,NewMod,Extras,TgtStream,MacStream,Quiet,DefList).

type_comp(defStruct(TypeName,SpecsList),Mod,Mod,Extras,TgtStream,MacStream,Quiet,DefList)
	:-!,
	ct_message(Quiet,'Transforming type = %t\n',[TypeName]),
	printf(TgtStream, '\n\%\%--- %t defStruct ---\n\n',[TypeName],[quoted(true)]),
	printf(MacStream, '\n\%\%--- %t defStruct Macros ---\n\n',[TypeName],[quoted(true)]),

	dmember(accessPred=AccessPred, SpecsList),
	dmember(setPred=SetPred, SpecsList),
	dmember(makePred=MakePred, SpecsList),
	dmember(structLabel=StructLabel, SpecsList),
	dmember(propertiesList=InitPropertiesList, SpecsList),
	expand_includes(InitPropertiesList, PropertiesList, Tail,Quiet,DefList),
	Tail = [],

	makeStructDefs(TypeName, AccessPred, SetPred, MakePred, 
					PropertiesList, StructLabel,Extras,TgtStream,MacStream).

type_comp((:- Cmd) ,Mod,Mod,[],TgtStream,MacStream,Quiet,DefList)
	:-
	call(Cmd),!.

type_comp(Item,Mod,Mod,[],TgtStream,MacStream,Quiet,DefList)
	:-
	ct_message(Quiet,'Unknown item -- skipping\n     %t\n',[Item]),
	printf(TgtStream,'%% Unknown item -- skipping:\n%%%t\n',[Item],[quoted(true)]).

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
makeStructDefs(TypeName, AccessPred, SetPred, Constructor, 
				PropertiesList, StructLabel,Extras,TgtStream,MacStream)
	:-
	printf(TgtStream, 'export %t/3.\n',[AccessPred],[quoted(true)]),
	printf(TgtStream, 'export %t/3.\n',[SetPred],[quoted(true)]),
	makeAccessPreds(PropertiesList, AccessPred, SetPred, 1, 
					NumProperties1, Defaults, TgtStream,MacStream),
	NumProperties is NumProperties1 - 1,
	makeConstructor(Constructor, StructLabel, NumProperties,
					Defaults,TgtStream,MacStream,PropertiesList),
	NoteOptionBody =.. [SetPred, Type, Structure, Value],
	flattenProps(PropertiesList, FullPropertiesList),
	Extras = [ typeProperties(TypeName, FullPropertiesList),
			   (noteOptionValue(TypeName, Type, Value, Structure) 
												:- NoteOptionBody )].

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
makeAccessPreds([],_,_, AccumNum, AccumNum,[],_,_).
makeAccessPreds([Item/Default | Rest], AccessPred, SetPred,AccumNum,
				FinalNum,[AccumNum-Default | RestDefaults],TgtStream,MacStream) 
	:-!,
	makeAccessPred(Item, AccessPred, SetPred, AccumNum, TgtStream,MacStream),
	NewAccumNum is AccumNum + 1,
	makeAccessPreds(Rest, AccessPred, SetPred, NewAccumNum, 
						FinalNum,RestDefaults,TgtStream,MacStream).

makeAccessPreds([Item | Rest], AccessPred, SetPred, 
				AccumNum, FinalNum,Defaults,TgtStream,MacStream) 
	:-
	makeAccessPred(Item, AccessPred, SetPred, AccumNum,TgtStream,MacStream),
	NewAccumNum is AccumNum + 1,
	makeAccessPreds(Rest, AccessPred, SetPred, 
					NewAccumNum, FinalNum,Defaults,TgtStream,MacStream).

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
makeAccessPred(Item, AccessPred, SetPred, AccumNum,TgtStream,MacStream) 
	:-
	AccessHead =.. [AccessPred, Item, Struct, Value],
	printf(TgtStream,'%t.\n',[(AccessHead :- arg(AccumNum, Struct, Value))],[quoted(true)]),
	printf(MacStream,'%t.\n',
				[define_macro((AccessHead => arg(AccumNum, Struct, Value)))],[quoted(true)]),
	SetHead =.. [SetPred, Item, Struct, Value],
	printf(TgtStream,'%t.\n\n',[(SetHead :- mangle(AccumNum, Struct, Value))],[quoted(true)]),
	printf(MacStream,'%t.\n\n',
				[define_macro((SetHead => mangle(AccumNum, Struct, Value)))],[quoted(true)]).

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
createAccessPreds([], AccessPred, SetPred, AccumNum,TgtStream,MacStream).
createAccessPreds([Item | Synonyms], AccessPred, SetPred, AccumNum,TgtStream,MacStream)
	:-
	makeAccessPred(Item, AccessPred, SetPred, AccumNum,TgtStream,MacStream),
	createAccessPreds(Synonyms, AccessPred, SetPred, AccumNum,TgtStream,MacStream).

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
makeConstructor(Constructor, StructLabel, NumProperties,
				Defaults, TgtStream, MacStream,PropertiesList)
	:-
	ConstructorHead =.. [Constructor, Structure],
	printf(TgtStream, 'export %t/1.\n',[Constructor],[quoted(true)]),
	constr_body(Defaults, Structure, StructLabel, NumProperties, Body),
	printf(TgtStream,'%t.\n\n',[(ConstructorHead :- Body)],[quoted(true)]),
	printf(MacStream, '%t.\n\n',[define_macro((ConstructorHead => Body))],[quoted(true)]),

	ConstructorHead2 =.. [Constructor, Structure2, RunArgsList],
	printf(TgtStream, 'export %t/2.\n',[Constructor],[quoted(true)]),
	constr_body2(PropertiesList,Structure2, StructLabel, RunArgsList, Body2),
	printf(TgtStream,'%t.\n\n',[(ConstructorHead2 :- Body2)],[quoted(true)]),
	printf(MacStream, '%t.\n\n',[define_macro((ConstructorHead2 => Body2))],[quoted(true)]),
	
	catenate(x,Constructor,XConstructor),
	functor(Structure, StructLabel, NumProperties),
	Structure =.. [StructLabel | SArgList],
	ConstructorHead3 =.. [XConstructor, Structure, SArgList],
	printf(TgtStream, 'export %t/2.\n',[XConstructor],[quoted(true)]),
	printf(TgtStream,'%t.\n\n',[ConstructorHead3],[quoted(true)]).


/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
constr_body([], Structure, StructLabel, NumProperties, Body)
	:-!,
	Body = functor(Structure, StructLabel, NumProperties).

constr_body(Defaults, Structure, StructLabel, NumProperties, Body)
	:-
	build_cstr_args(1,NumProperties, Defaults, ArgsList),
	Body =
		(Structure =.. [StructLabel | ArgsList] ).

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
constr_body2(PropsList,Structure, StructLabel, RunArgSpecs, Body)
	:-
	defaults_and_tags(PropsList, DefArgs, OrderedTags),
	Body =
	(
		struct_lookup_subst(OrderedTags, DefArgs, RunArgSpecs, ArgsList),
		Structure =.. [StructLabel | ArgsList]
	).

defaults_and_tags([], [], []).
defaults_and_tags([Prop | PropsList], [DefArg | DefArgs], [Tag | OrderedTags])
	:-
	struct_default_and_tag(Prop, DefArg, Tag),
	defaults_and_tags(PropsList, DefArgs, OrderedTags).

struct_default_and_tag(Tag/DefArg, DefArg, Tag) :-!.
struct_default_and_tag(Tag, DefArg, Tag).

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
/*
build_cstr_args(0, Defaults, [])
	:-!.

build_cstr_args(CurNum, Defaults, [ThisArg | ArgsList])
	:-
	(dmember(CurNum-Val, Defaults) ->
		ThisArg = Val
		;
		ThisArg = VV
	),
	NextNum is CurNum - 1,
	build_cstr_args(NextNum, Defaults, ArgsList).
*/
build_cstr_args(Ctr, Limit, Defaults, [])
	:-
	Ctr > Limit, !.

build_cstr_args(CurNum, Limit, Defaults, [ThisArg | ArgsList])
	:-
	(dmember(CurNum-Val, Defaults) ->
		ThisArg = Val
		;
		ThisArg = VV
	),
	NextNum is CurNum + 1,
	build_cstr_args(NextNum, Limit, Defaults, ArgsList).

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
flattenProps([], []).
flattenProps([Item/DefVal | PropertiesList], [Item | RestFullList])
	:-!,
	flattenProps(PropertiesList, RestFullList).
flattenProps([Item | PropertiesList], [Item | RestFullList])
	:-
	flattenProps(PropertiesList, RestFullList).

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/

expand_includes([], Tail, Tail,Quiet,DefList).
expand_includes([include(Name) | RestPropertiesList], PropertiesList, 
					FinalTail,Quiet,DefList)
	:-
	fetch_local_included_props(DefList, Name, PropertiesList, InterPropsListTail, Quiet, DefList),
	!,
	expand_includes(RestPropertiesList, InterPropsListTail, FinalTail,Quiet,DefList).

expand_includes([include(File,Name) | RestPropertiesList], PropertiesList, 
					FinalTail,Quiet,DefList)
	:-
	fetch_included_props(File, Name, PropertiesList, InterPropsListTail, Quiet),
	!,
	expand_includes(RestPropertiesList, InterPropsListTail, FinalTail,Quiet,DefList).

expand_includes([include(File,Name) | RestPropertiesList], PropertiesList, 
					FinalTail,Quiet,DefList)
	:-
	expand_includes(RestPropertiesList, PropertiesList, FinalTail,Quiet,DefList).

expand_includes([Item | InitPropertiesList], [Item | PropertiesList], 
					FinalTail,Quiet,DefList)
	:-
	expand_includes(InitPropertiesList, PropertiesList, FinalTail,Quiet,DefList).

fetch_included_props(File, Name, PropertiesList, InterPropsListTail, Quiet)
	:-
	(filePlusExt(BaseFile,typ,File) ->
		FullFile = File ; filePlusExt(File,typ,FullFile)
	),
	exists(FullFile),
	open(FullFile, read, InStr, []),
	!,
	read_terms(InStr, FTerms),
	close(InStr),
	fin_fetch_included_props(FTerms, File, Name, PropertiesList, InterPropsListTail, Quiet, FTerms).

fetch_included_props(File, Name, In, In, Quiet)
	:-
	ct_message(Quiet,'!Warning: Can\'t find included type file >> %t.typ << ...skipping.\n',
				[File]).

fin_fetch_included_props(FTerms, File, Name, PropertiesList, InterPropsListTail, Quiet, FTerms)
	:-
	dmember(defStruct(Name, FSpecs), FTerms),
	dmember(propertiesList=FProps, FSpecs),
	!,
	expand_includes(FProps, PropertiesList, InterPropsListTail, Quiet, FTerms).

fin_fetch_included_props(FTerms, File, Name, In, In, Quiet, FTerms)
	:-
	ct_message(Quiet,
			   '!Warning: Can\'t find included type >> %t << in file %t...skipping.\n',
				[Name,File]).


fetch_local_included_props(FTerms, Name, PropertiesList, InterPropsListTail, Quiet, FTerms)
	:-
	dmember(defStruct(Name, FSpecs), FTerms),
	dmember(propertiesList=FProps, FSpecs),
	!,
	expand_includes(FProps, PropertiesList, InterPropsListTail, Quiet, FTerms).

fetch_local_included_props(DefList, Name, PropertiesList, InterPropsListTail, Quiet, FTerms)
	:-
	ct_message(Quiet,
			   '!Warning: Can\'t find included type >> %t << in file %t...skipping.\n',
				[Name,'local-file']).

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
pp_all(Extras,Stream) :- pp_all(Extras,Stream, []).
pp_all([],_,_).
pp_all([Extras | MoreExtras],Stream, Options)
	:-!,
	pp_all(Extras,Stream, Options),
	pp_all(MoreExtras,Stream, Options).
pp_all(Extras,Stream, Options)
	:-
	(dmember(quoted(_),Options) ->
		TheOptions = Options
		;
		TheOptions = [quoted(true) | Options]
	),
	write_term(Stream,Extras,TheOptions),
	put_code(Stream,0'.),
	nl(Stream).

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
ct_err(Message)
	:-
	ct_err(Message,[]).

ct_err(Message,Values)
	:-
	printf(Message,Values),
	flush_output.

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
ct_message(Quiet, Message)
	:-
	ct_message(Quiet, Message,[]).

ct_message(true, Message,Values) :-!.

ct_message(false, Message,Values)
	:-
	printf(Message,Values),
	flush_output.

endmod.

