/* ===========================================================*
 |				tc_base.pro
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
 |	Revision: Aug, 1998: Spec -> Code -> Assert model
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

:- module_closure(defStruct,2,do_defStruct).

do_defStruct(Mod,TypeName,SpecsList)
	:-
	do_type_comp(TypeName,SpecsList,Mod).

export do_type_comp/3.
do_type_comp(TypeName,SpecsList,Mod)
	:-
	compiletime,
	do_type_comp(defStruct(TypeName,SpecsList),Mod, CodeList,MacList,ErrList),
	finish_type_comp(ErrList,TypeName,SpecsList,Mod, CodeList,MacList).

finish_type_comp([],TypeName,SpecsList,Mod, CodeList,MacList)
	:-!,
	xconsult:addclauses(CodeList, Mod).

finish_type_comp(ErrList,TypeName,SpecsList,Mod, CodeList,MacList)
	:-
	throw(error(defStruct,[TypeName,ErrList])).


export do_type_comp/5.
do_type_comp(defStruct(TypeName,SpecsList),Mod, CodeList,MacList,ErrList)
	:-
	do_type_comp(defStruct(TypeName,SpecsList),Mod,
			CodeList,[], MacList,[], ErrList,[]).


export do_type_comp/8.
do_type_comp(defStruct(TypeName,SpecsList),Mod,
			CodeList,CodeListTail, MacList,MacListTail, ErrList,ErrListTail)
	:-
	find_check_atom(
		[	accessPred=AccessPred, 
		 	setPred=SetPred, 
			makePred=MakePred, 
			structLabel=StructLabel],
		SpecsList,
		ErrList, EL2),
	( (dmember(propertiesList=InitPropertiesList, SpecsList),
			InitPropertiesList = [_ | _] ) ->
			EL2 = EL3
			;
			EL2 =[bad_missing(propertiesList) | EL3]
	),

	expand_includes(InitPropertiesList, PropertiesList, PLTail,Mod,EL3,ErrListTail),
	PLTail = [],

	ErrListTail = [],
	(ErrList \= [] ->
		CodeListTail=CodeList,MacListTail=MacList
		;
		makeStructDefs(TypeName, AccessPred, SetPred, MakePred, 
				PropertiesList, StructLabel,Mod,
				CodeList,CodeListTail, MacList,MacListTail)
	).



find_check_atom([], _, ErrOut, ErrOut).


find_check_atom([Tag=Value | Tgts], Src, ErrIn, ErrOut)
	:-
	dmember(Tag=Value, Src),
	!,
	(atom(Value) ->
		ErrIn = ErrInter
		;
		ErrIn = [bad_value_for(Tag, Value) | ErrInter]
	),
	find_check_atom(Tgts, Src, ErrInter, ErrOut).


find_check_atom([Tag=Value | Tgts], Src, [missing(Tag) | ErrIn], ErrOut)
	:-
	find_check_atom(Tgts, Src, ErrIn, ErrOut).


makeStructDefs(TypeName, AccessPred, SetPred, Constructor, 
				PropertiesList, StructLabel,Mod,
				CodeList,CodeListTail, MacList,MacListTail)
	:-
	CodeList = [(export AccessPred/3), (export SetPred/3) | CL1],

	makeAccessPreds(PropertiesList, AccessPred, SetPred, 
					1, NumProperties1, Defaults, 
					CL1, CL2, MacList,ML2),

	NumProperties is NumProperties1 - 1,
	makeConstructor(Constructor, StructLabel, NumProperties,
					Defaults,
					CL2,CL3,ML2,MacListTail,PropertiesList),
	CL3 = [note_struct_expanded(TypeName, PropertiesList) | CodeListTail].


/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
makeAccessPreds([],_,_, AccumNum, AccumNum,[],C,C,M,M).
makeAccessPreds([(Item/Default)-Comment | Rest], AccessPred, SetPred,
				AccumNum, FinalNum, [AccumNum-Default | RestDefaults],
				CodeIn,CodeOut,MacIn,MacOut) 
	:-!,
	makeAccessPreds([Item/Default | Rest], AccessPred, SetPred,
					AccumNum, FinalNum,[AccumNum-Default | RestDefaults],
					CodeIn,CodeOut,MacIn,MacOut).

makeAccessPreds([Item/Default | Rest], AccessPred, SetPred,
				AccumNum, FinalNum,[AccumNum-Default | RestDefaults],
				CodeIn,CodeOut,MacIn,MacOut) 
	:-!,
	makeAccessPred(Item, AccessPred, SetPred, AccumNum, 
					CodeIn,CodeInter,MacIn,MacInter),
	NewAccumNum is AccumNum + 1,
	makeAccessPreds(Rest, AccessPred, SetPred, 
					NewAccumNum, FinalNum,RestDefaults,
					CodeInter,CodeOut,MacInter,MacOut).

makeAccessPreds([Item - Comment | Rest], AccessPred, SetPred, 
				AccumNum, FinalNum,Defaults,
				CodeIn,CodeOut,MacIn,MacOut) 
	:-!,
	makeAccessPreds([Item | Rest], AccessPred, SetPred, 
					AccumNum, FinalNum,Defaults,
					CodeIn,CodeOut,MacIn,MacOut).

makeAccessPreds([Item | Rest], AccessPred, SetPred, 
				AccumNum, FinalNum,Defaults,
				CodeIn,CodeOut,MacIn,MacOut) 
	:-
	makeAccessPred(Item, AccessPred, SetPred, AccumNum, 
					CodeIn,CodeInter,MacIn,MacInter),
	NewAccumNum is AccumNum + 1,
	makeAccessPreds(Rest, AccessPred, SetPred, 
					NewAccumNum, FinalNum,Defaults,
					CodeInter,CodeOut,MacInter,MacOut).

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
makeAccessPred(Item, AccessPred, SetPred, AccumNum,
				CodeIn,CodeInter,MacIn,MacInter)
	:-
	AccessHead =.. [AccessPred, Item, Struct, Value],
	SetHead =.. [SetPred, Item, Struct, Value],

	CodeIn = [(AccessHead :- arg(AccumNum, Struct, Value)),
				(SetHead :- mangle(AccumNum, Struct, Value)) | CodeInter],

	MacIn = [define_macro((AccessHead => arg(AccumNum, Struct, Value))),
			 define_macro((SetHead => mangle(AccumNum, Struct, Value))) |
					MacInter].

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
makeConstructor(Constructor, StructLabel, NumProperties, Defaults, 
				CodeIn,CodeOut,MacIn,MacOut,PropertiesList) 
	:-
	ConstructorHead =.. [Constructor, Structure],
	CodeIn = [(export Constructor/1), ((ConstructorHead :- Body)) | CL2],
	constr_body(Defaults, Structure, StructLabel, NumProperties, Body),
	MacIn = [define_macro((ConstructorHead => Body)) | ML2],

	ConstructorHead2 =.. [Constructor, Structure2, RunArgsList],
	CL2 = [(export Constructor/2),(ConstructorHead2 :- Body2) | CL3],
	constr_body2(PropertiesList,Structure2, StructLabel, RunArgsList, Body2),
	ML2 = [define_macro((ConstructorHead2 => Body2)) | MacOut],
	
    '$atom_concat'(x,Constructor, XConstructor),
	functor(Structure, StructLabel, NumProperties),
	Structure =.. [StructLabel | SArgList],
	ConstructorHead3 =.. [XConstructor, Structure, SArgList],
	CL3 = [(export XConstructor/2), ConstructorHead3 | CodeOut].


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

struct_default_and_tag((Tag/DefArg)-Comment, DefArg, Tag) :-!.
struct_default_and_tag(Tag/DefArg, DefArg, Tag) :-!.
struct_default_and_tag(Tag-Comment, DefArg, Tag) :-!.
struct_default_and_tag(Tag, DefArg, Tag).

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/
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

expand_includes([], Tail, Tail,Mod,ErrOut,ErrOut).

expand_includes([include(Name) | RestPropertiesList], PropertiesList, 
					FinalTail,Mod,ErrIn,ErrOut)
	:-
	Mod:note_struct_expanded(TypeName, TypePropertiesList),
	!,
	expand_includes(TypePropertiesList,PropertiesList,InterPropsListTail, Mod, ErrIn,ErrInter),
	expand_includes(RestPropertiesList,InterPropsListTail, FinalTail, Mod, ErrInter,ErrOut).

expand_includes([include(Name) | RestPropertiesList], PropertiesList, 
					FinalTail,Mod,ErrIn,ErrOut)
	:-!,
	ErrIn = [missing_include(File,Name) | ErrInter],
	expand_includes(RestPropertiesList, InterPropsListTail, FinalTail,Mod,ErrInter,ErrOut).

expand_includes([include(File,Name) | RestPropertiesList], PropertiesList, 
					FinalTail,Mod,ErrIn,ErrOut)
	:-
	fetch_included_props(File, Name, PropertiesList, InterPropsListTail, Mod,ErrIn, ErrInter),
	!,
	expand_includes(RestPropertiesList, InterPropsListTail, FinalTail,Mod,ErrInter,ErrOut).

expand_includes([include(File,Name) | RestPropertiesList], PropertiesList, 
					FinalTail,Mod,ErrIn,ErrOut)
	:-
	ErrIn = [missing_include(File,Name) | ErrInter],
	expand_includes(RestPropertiesList, PropertiesList, FinalTail,Mod,ErrInter, ErrInter).

expand_includes([Item | InitPropertiesList], [Item | PropertiesList], 
					FinalTail,Mod,ErrIn,ErrOut)
	:-
	expand_includes(InitPropertiesList, PropertiesList, FinalTail, Mod,ErrIn,ErrOut).

/**********
fetch_included_props(File, Name, PropertiesList, InterPropsListTail, Quiet)
	:-
	(filePlusExt(BaseFile,typ,File) ->
		FullFile = File ; filePlusExt(File,typ,FullFile)
	),
	locate_include_file(FullFile, FullIncludeFile),
	open(FullIncludeFile, read, InStr, []),
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


locate_include_file(FullFile, FullFile)
	:-
	exists(FullFile),
	!.

locate_include_file(FullFile, FullIncludeFile)
	:-
	include_dir(Path),
	pathPlusFile(Path, FullFile, FullIncludeFile),
	exists(FullIncludeFile),
	!.

locate_include_file(FullFile, FullIncludeFile)
	:-
	builtins:searchdir(Path),
	pathPlusFile(Path, FullFile, FullIncludeFile),
	exists(FullIncludeFile),
	!.

locate_include_file(FullFile, FullIncludeFile)
	:-
	builtins:sys_searchdir(ALSDIR),
	extendPath(ALSDIR, includes, SysIncludes),
	pathPlusFile(SysIncludes, FullFile, FullIncludeFile),
	exists(FullIncludeFile),
	!.
************/


endmod.


