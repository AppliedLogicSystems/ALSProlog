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

%module dd.
module builtins.
%export do_defStruct/3.

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

	expand_includes(InitPropertiesList, PropertiesList, PLTail,Mod,EL3,EL4),
	PLTail = [],

	((nonvar(ErrList),ErrList \= []) ->
		CodeListTail=CodeList,MacListTail=MacList
		;
		makeStructDefs(TypeName, AccessPred, SetPred, MakePred, 
			PropertiesList, StructLabel,Mod,
			CodeList,CodeListTail0, MacList,MacListTail),
		(dmember(xfwrite=DelaySlots, SpecsList) ->
			dreverse(PropertiesList, [LastPP | _]),
			(LastPP = LastP/_ -> true ; LastPP = LastP),
			(member(LastP, DelaySlots) ->
				gen_xfwrite(TypeName,DelaySlots,LastP,PropertiesList,StructLabel,
							Mod, SpecsList, CodeListTail0, CodeListTail)
				;
				EL4 = [xfwrite_req(last_prop(LastP),'not_last_in_property_list')
							| ErrListTail],
				CodeListTail = CodeListTail0
			)
			;
			EL4 = ErrListTail,
			CodeListTail = CodeListTail0
		),
		ErrListTail = []
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

/*--------------------------------------------------------------------------*
 *--------------------------------------------------------------------------*/

gen_xfwrite(TypeName,DelaySlots,LastP,PropertiesList,StructLabel,
			Mod, SpecsList, CodeList, CodeListTail)
	:-
	catenate('xfwrite_',TypeName, WPred),
	findall(P, 
		(member(P, PropertiesList), not( member(P, DelaySlots)),
			not((P=P0/_, member(P0,DelaySlots))) ),
		NonDelaySlots),
	Head =.. [WPred, FileName, TaggedNonDelays, DelayOffsets, Stream],
	Dummy = '                ',
	setup_matchup(PropertiesList,DelaySlots,Dummy,NDSlotsVars,AllButLast,DelayOffsets),
	Clause1 = dslm([], _),
	Clause2 = ( dslm([Tag=Var |Rest],TgdVals) :- 
		dmember(Tag=Var,TgdVals),dslm(Rest,TgdVals) ),
	Clause3 = (Head :-
		dslm(NDSlotsVars, TaggedNonDelays),
		open(FileName, read_write, Stream),
		printf(Stream, '%t(\n',[StructLabel],[quoted(true)]),
		xfwrite_slots(AllButLast,DelayOffsets,Stream),
		write(Stream, '['),nl(Stream)
	),
	Clause4 = xfwrite_slots([],_,_),
	Clause5 = ( xfwrite_slots([P5=D5 | AllButLast5],[P5=V5 | DelayOffsets5],Stream5) :-!,
			stream_property(Stream5,position(V5)),
			write_term(Stream5, D5, [quoted(true)]),put_code(Stream5,0',),
			xfwrite_slots(AllButLast5,DelayOffsets5,Stream5) ),
	Clause6 = ( xfwrite_slots([P6=V6 | AllButLast6],DelayOffsets6,Stream6) :-
			write_term(Stream6, V6, [quoted(true)]),put_code(Stream6,0',),
			xfwrite_slots(AllButLast6,DelayOffsets6,Stream6) ),
	CodeList = [Clause1, Clause2, Clause3, Clause4, Clause5, Clause6
		| InterCodeListTail],
	xgen_xfwrite(TypeName,DelaySlots,NonDelaySlots,LastP,PropertiesList,StructLabel,
			NDSlotsVars,AllButLast, Dummy, Mod, SpecsList, InterCodeListTail, CodeListTail).
		
setup_matchup([],DelaySlots,Dummy,[],[],[]).
setup_matchup([Last],DelaySlots,Dummy,[],[],[])
	:-!.
setup_matchup([PP | PropertiesList],DelaySlots,Dummy,
				NDSlotsVars,[P=Dummy | AllButLast],[P=Off | DelayOffsets])
	:-
	(PP=P/_ -> true ; PP=P),
	dmember(P, DelaySlots),
	!,
	setup_matchup(PropertiesList,DelaySlots,Dummy,NDSlotsVars,AllButLast,DelayOffsets).

setup_matchup([PP | PropertiesList],DelaySlots,Dummy,
				[P=PVal | NDSlotsVars],[P=PVal | AllButLast],DelayOffsets)
	:-
	(PP=P/_ -> true ; PP=P),
	setup_matchup(PropertiesList,DelaySlots,Dummy,NDSlotsVars,AllButLast,DelayOffsets).
	

%%%%%%%%%%%% EXTENDED XFWRITE %%%%%%%%%%%%%%%%%%%%%%

xgen_xfwrite(TypeName,DelaySlots,NonDelaySlots,LastP,PropertiesList,StructLabel,
		NDSlotsVars,AllButLast, Dummy, Mod, SpecsList, InterCodeListTail, CodeListTail)
	:-
	catenate('xfwrite_open_',TypeName, OpenPred),
	OpenHead =.. [OpenPred,TgtFileName, TaggedNonDelays, XDelayOffsets, Stream],

	catenate('xfwrite_close_',TypeName, ClosePred),
	CloseHead =.. [ClosePred,XTaggedDelayValues, XDelayOffsets, Stream],

	length(NonDelaySlots, K),
	length(DelaySlots, N1),
	calc_dummy_pat(Dummy, Pat),

	append(AllButLast, [LastP=Dummy], AllSlots), 
	append(DelaySlots, [LastP=Nn], XDelaySlots),

	Clause7 = (OpenHead :-
		dslm(NDSlotsVars, TaggedNonDelays),
		open(TgtFileName, read_write, Stream),
		printf(Stream, '%t=[local=%t,global=%t].\n',[StructLabel,K,N1],[quoted(true)]),
		xfwrite_slots_eq(AllSlots,DelaySlots,XDelayOffsets,Stream)
	),
	
	Clause8 = xfwrite_slots_eq([],_,[],_),
	Clause9 = (  xfwrite_slots_eq([P9=D9 | AllButLast9],[P9 | DelaySlots9],
					[P9=V9 | DelayOffsets9], Stream9) 
				 :-!,
				 write_term(Stream9, P9, [quoted(true)]),
				 put_code(Stream9, 0'=),
				 stream_property(Stream9,position(V9)),
				 write_term(Stream9, D9, [quoted(true)]),
				 put_code(Stream9,0'.), nl(Stream9),
				 xfwrite_slots_eq(AllButLast9,DelaySlots9,DelayOffsets9,Stream9) 
			),
	Clause10 = ( xfwrite_slots_eq([P10=V10 | AllButLast10],DelaySlots10,
					DelayOffsets10,Stream10) 
				 :-
				 write_clause(Stream10, P10=V10, [quoted(true)]),
				 xfwrite_slots_eq(AllButLast10,DelaySlots10,DelayOffsets10,Stream10) ),

	Clause11 = ( CloseHead :-
					xfwrite_delay_vals(XTaggedDelayValues, XDelayOffsets, Stream),
					close(Stream)
			),

	Clause12 = xfwrite_delay_vals([], [], Stream),

	Clause13 = ( xfwrite_delay_vals(
					[Tag=Value | TaggedDelayValues], [Tag=Offset | TaggedOffsets], 
					Stream)
					:-
					stream_position(Stream, _, Offset),
					printf(Stream, Pat, [Value], [quoted(true)]),
					xfwrite_delay_vals(TaggedDelayValues, TaggedOffsets, Stream)
			),

	catenate('xfread_init_',TypeName, InitReadPred),
	InitReadHead =.. [InitReadPred, TaggedValues, N, Stream],

	Clause14 = ( InitReadHead :-
					stream_position(Stream, _, 0),
					read(Stream, Header),
					Header = (StructLabel=[local=NL,global=NG]),
					TN is NL+NG,
					read_n(TN, Tgd, Stream),
					dreverse(Tgd, [LastP=N0 | RevTaggedValues]),
					N is floor(N0),
					dreverse(RevTaggedValues, TaggedValues)
			),

	catenate('xfread_',TypeName, ReadPred),
	ReadHead2 =.. [ReadPred, TFile, TTerm],
	catenate('xfreads_',TypeName, ReadPredS),
	ReadHead2W =.. [ReadPredS, TTerm, Stream],
	ReadHead3 =.. [ReadPred, TFile, M, TTerm],
	ReadHead3W =.. [ReadPredS, M, TTerm, Stream],

	Clause20 = ( ReadHead2 :-
				    open(TFile, read_write, Stream),
					unwind_protect(ReadHead2W, close(Stream))
			),
	ReadHead3 =.. [ReadPred, TFile, M, TTerm],
	Clause21 = ( ReadHead3 :-
				    open(TFile, read_write, Stream),
					unwind_protect(ReadHead3W, close(Stream))
			),
	Clause22 = ( ReadHead2W :-
					InitReadHead,
					findall(V,member(_=V, TaggedValues), TValues),
					append(TValues, [Terms], TArgs),
					TTerm =.. [StructLabel | TArgs],
					read_all(Terms, Stream)
			),
	Clause23 = ( ReadHead3W :-
					InitReadHead,
					findall(V,member(_=V, TaggedValues), TValues),
					append(TValues, [Terms], TArgs),
					TTerm =.. [StructLabel | TArgs],
					read_n(Terms, M, Stream)
			),

	InterCodeListTail = [Clause7, Clause8, Clause9, Clause10, Clause11,
						 Clause12, Clause13, Clause14, 
				%		 Clause15, Clause16, Clause17, Clause18, Clause19,
						 Clause20, Clause21,
						 Clause22, Clause23 | CodeListTail].

calc_dummy_pat(Dummy, Pat)
	:-
	atom_length(Dummy, DL),
	DL2 is DL+2,
	sprintf(atom(Pat),'%% %t.8f',[DL2]).

endmod.
