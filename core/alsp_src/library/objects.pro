/*==========================================================================*
 |   			objects.pro
 |	Copyright (c) 1990-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		Object-oriented programming in Prolog
 |
 |	Author:	Ken Bowen
 |	Date:	August 1990 --
 *==========================================================================*/

module objects.
use sio.
use windows.

export defineClass/2.
export create_object/2.
export defineObject/1.



export set_object_messages/1.

:-module_closure(defineClass,1,defineClass).

	%%%%%%% CHANGE WHICH CLAUSE IS USED FOR THIS TEST.....
	%% Test whether objs_run.pro has already been loaded from the library;
	%% If not present, force the lib_load for it:
:- clause(send('$no$Object$', Message), (!, send(builtins,null_object,Message))),!;
	builtins:lib_load('library/objs_run',objects,true,0,objects,true).

:- dynamic(objectModule/2).
:- dynamic(subClassOf/2).
:- dynamic(classModule/2).
:- dynamic(slot_constraint/4).
:- dynamic(slot_default/3).
:- dynamic(object_messages/1).

:- dynamic(oop_include_dir/1).
:- dynamic(extension_installed/1).
:- dynamic(slots_profile_for/2).

			/*=================================
			 |   OBJECT DEFINITION/CREATION
			 *================================*/

/*!-----------------------------------------------------------------------
 |	create_object/2
 |	create_object(SpecList, Object)
 |	create_object(+, -)
 |
 |	- creates an (anonymous) object in specified by SpecList
 *-----------------------------------------------------------------------*/

create_object(SpecList, Object)
	:-
	dmember(instanceOf=ObjectClass, SpecList),
	classModule(ObjectClass, ObjectClassModule),
	(dmember(options=Options, SpecList) ->
		true;
		Options = []
	),
	(dmember(values=LocalValues, SpecList) ->
		true;
		LocalValues = []
	),
	create_class_make_pred(ObjectClass, ClassMakePred),
	CreationCall =.. [ClassMakePred, Object],
	call(CreationCall),
	setObjStruct(myClassPred, Object, ObjectClass),
	setObjStruct(myClassModule, Object, ObjectClassModule),

		%% get any class slot initialization values:
	bagOf(Slot=Value, slot_default(ObjectClass,Slot,Value), ClassValues),
		%% merge them:
	merge_values(ClassValues, LocalValues, Values),
	class_constr_init(ObjectClass,ClassInitVals),
	class_default_init(ObjectClass,ClassDefaultVals),
	object_values_init(Values, ObjInitVals),
	merge_init_vals(ClassDefaultVals, ObjInitVals, InterInitVals),
	merge_init_vals(InterInitVals, ClassInitVals, InitVals),
	set_all_slots(InitVals, Object).

set_all_slots([], Object).
set_all_slots([s(Slot,Val) | InitVals], Object)
	:-
	setObjStruct(Slot, Object, Val),
	set_all_slots(InitVals, Object).

defineObject(SpecList)
	:-
	dmember(name=ObjectName, SpecList),
	get_globalObjectName(ObjectName, Object),
	!.

defineObject(SpecList)
	:-
	dmember(name=ObjectName, SpecList),
	create_object(SpecList, Object),
	set_globalObjectName(ObjectName, Object).


merge_values([], LocalValues, LocalValues).
merge_values([Slot=Value | ClassValues], LocalValues, Values)
	:-
	dmember(Slot=_, LocalValues),
	!,
	merge_values(ClassValues, LocalValues, Values).
merge_values([Slot=Value | ClassValues], LocalValues, [Slot=Value | Values])
	:-
	merge_values(ClassValues, LocalValues, Values).

make_inheritance_calls([],_,_,_,_,_,_,X,X).
make_inheritance_calls([Class | ClassList],DispatchPred, Module,ClassInfo,
						MessageVar,StateVar,SetStatePred,CodeIn,CodeOut)
	:-
	(classModule(Class, ClassModule),!;
		dmember(classModule(Class, ClassModule), ClassInfo)),
	ClassActionCall =.. [Class, MessageVar, StateVar],
	(ClassModule = Module ->
		CodeIn = [(DispatchPred :- ClassActionCall) | CodeInter]
		;
		CodeIn = [nl, use(ClassModule), nl,
				  (DispatchPred :- ClassActionCall) | CodeInter]
	),
	make_inheritance_calls(ClassList,DispatchPred, Module, ClassInfo,
						   MessageVar,StateVar,SetStatePred,
						   CodeInter,CodeOut).

ancestor(Class, Class).
ancestor(ClassA, ClassB)
	:-
	subClassOf(ClassA, ParentClasses),
	member(Parent,ParentClasses),
	ancestor(Parent, ClassB).

class_constr_init(ObjectClass,ClassInitVals)
	:-
	setOf(s(Slot,Constant),
			[Parent,Value]^
				( ancestor(ObjectClass, Parent),
				  slot_constraint(Parent,Slot,Value,Value=Constant) ),
			ClassInitVals).

class_default_init(ObjectClass,ClassDefaultVals)
	:-
	setOf(s(Slot,Value),
			[Parent, Value]^
				( ancestor(ObjectClass, Parent),
				  slot_default(Parent,Slot,Value) ),
			ClassDefaultVals).

object_values_init(Values, InitVals)
	:-
	setOf(s(Slot,Constant),
			member(Slot=Constant,Values),
			InitVals).

		%% Arg 2 values have precedence over Arg 1 values:
merge_init_vals([], HigherVals, HigherVals).
merge_init_vals([s(Slot,LowVal) | LowerVals], HigherVals, ResultVals)
	:-
	dmember(s(Slot, _), HigherVals),
	!,
	merge_init_vals(LowerVals, HigherVals, ResultVals).
merge_init_vals([s(Slot,LowVal) | LowerVals], HigherVals, 
				[s(Slot,LowVal) | ResultVals])
	:-
	merge_init_vals(LowerVals, HigherVals, ResultVals).

			/*=================================
			 |       CLASS DEFINITION
			 *================================*/

/*!-----------------------------------------------------------------------
 |	defineClass/2
 |	defineClass(Module, SpecList)
 |	defineClass(+, +)
 |
 |	- defines a class in Module as specified by SpecList
 *-----------------------------------------------------------------------*/
defineClass(_, SpecList)
	:-
	dmember(name=ClassName,SpecList),
	classModule(ClassName, Module),
	!,
	(object_messages(on) ->
		objects_advise(class_ALREADY_defined(ClassName-Module));
		true
	).

defineClass(Module, SpecList)
	:-
	dmember(name=ClassName,SpecList),
	atom(ClassName),
	(dmember(module=ClassModule, SpecList) ->
		true
		;
		ClassModule = Module
	),
	defineClass(ClassModule, SpecList, [], _, InitialCode),
	sort_code(InitialCode, [ClassModule], objects, ObjsCode, ClassModule, ModCode),
	install_code([(module objects) | ObjsCode]),
	install_code([(module ClassModule) | ModCode]),
	!,
	(object_messages(on) ->
		dmember(name=ClassName,SpecList),
		objects_advise(class_defined(ClassName-ClassModule));
		true
	).

defineClass(Module, SpecList)
	:-
	(object_messages(on) ->
		dmember(name=ClassName, SpecList),
		objects_advise(class_definition_FAILURE(ClassName-Module));
		true
	).

defineClass(Module, SpecList, ClassInfoIn, ClassInfoOut, Code)
	:-
		%% get the name of the class:
	dmember(name=ClassName,SpecList),
	atom(ClassName),
		%% get the name of the class action predicate:
	((dmember(action=ActionPredicateName,SpecList),
	  ActionPredicateName \= ClassName)  ->
		true;
		catenate(ClassName,'Action',ActionPredicateName)
	),

		%% get the class's superclasses:
	(dmember(subClassOf=ClassList,SpecList) -> true ;
		dmember(subclassOf=ClassList,SpecList)),
	assert(subClassOf(ClassName,ClassList)),
	assert(classModule(ClassName, Module)),

		%% get the class's local slots:
	(dmember(addl_slots=LocalSlots,SpecList) ->
		true;
		LocalSlots = []
	),

	Code = [module(objects),
			 classModule(ClassName, Module),
			 subClassOf(ClassName,ClassList),
			 endmod(objects),
			 module(Module),
			 dynamic(Module,ActionPredicateName,2)
			 | Code1A ],

	(dmember(export=no,SpecList) ->
		Code1A = Code1
		;
		Code1A = [(export ClassName/2) | Code1]
	),

	createClassStatePreds(ClassName,ClassList,LocalSlots,
						  ClassInfoIn, ClassInfoOut0,
						  CompleteSlots,Module,Code1,Code2),
	ClassInfoOut = [classModule(ClassName,Module),
					subClassOf(ClassName,ClassList)
						| ClassInfoOut0],

		%% set up various call forms:
	ClassCall =.. [ClassName, MessageVar,StateVar],
	ActionCall =.. [ActionPredicateName,MessageVar,StateVar],
	Code3 = Code2,

		%% set up class methods (action) predicates:
	(dmember(local_action=LocalActionFlag,SpecList) ->
		true
		;
		LocalActionFlag = ok
	),

	(ClassList = [] ->
		(dmember(LocalActionFlag,[suppress,no]) ->
			Code3 = Code5
			;
			Code3 = [(ClassCall :- ActionCall) | Code5]
			)
		;
		(dmember(LocalActionFlag,[suppress,no]) ->
			Code3 = Code4
			;
			Code3 = [(ClassCall :- ActionCall) | Code4]
		),
		make_inheritance_calls(ClassList,ClassCall, Module, ClassInfoIn,
							   MessageVar,StateVar,SetStateVar,
							   Code4, Code5)
	),

	Code5 = [endmod(Module), module(objects) | Code6],
		%% handle any class contraints:
	(dmember(constrs=Constraints, SpecList) ->
		record_class_slot_constraints(Constraints, ClassName, Module, 
								CompleteSlots,Code6, Code7)
		;
		Code7 = Code6
	),
		%% handle any class slot defaults:
	(dmember(defaults=SlotDefaults, SpecList) ->
		record_class_slot_defaults(SlotDefaults, ClassName, Module, 
								CompleteSlots,Code7, Code8)
		;
		Code8 = Code7
	),
	!,
	Code8 = [endmod(objects)].

createClassStatePreds(ClassName,ClassList,LocalSlots,
					  ClassInfoIn, 
					  [slots_for(ClassName,CompleteSlots) | ClassInfoIn],
					  CompleteSlots,Module,CodeIn,CodeOut)
	:-
		%% install error-checking for overlapping slot names;
		%% i.e., the same slot name occurring in several
		%% different classes in ClassList, or in LocalSlots
		%% and one of the classes in ClassList.
	class_list_ok(ClassList, ClassInfoIn),
	get_super_slots(ClassList,SuperSlots,ClassInfoIn),
	new_slots_ok(LocalSlots, SuperSlots, ClassName, ClassList),
	append(SuperSlots, LocalSlots, CompleteSlots),
	create_class_make_pred(ClassName, MakePred),
	CodeIn = [make_pred(ClassName,MakePred) | CodeTail],
	makeObjectStruct(ClassName, MakePred, CompleteSlots, Module,
				 		CodeTail, CodeOut).

	%% assume that no entry on ClassList is a subclass of another
	%% (add a check/reducer for this)
class_list_ok([],_) :-!.
class_list_ok([Class],_) :-!.
class_list_ok([First | RestClassList], ClassInfoIn)
	:-
	class_list_ok(RestClassList, ClassInfoIn),
	one_class_ok(RestClassList, First, ClassInfoIn).

one_class_ok([], First, ClassInfoIn).
one_class_ok([ThisC | RestClassList], First, ClassInfoIn)
	:-
	check_slot_join(ThisC, First, ClassInfoIn),
	one_class_ok(RestClassList, First, ClassInfoIn).

check_slot_join(Class1, Class2, ClassInfoIn)
	:-
	(subClassOf(Class1,SuperClassList) ->
		true
		;
		dmember(subClassOf(Class1,SuperClassList), ClassInfoIn)
	),
	dmember(Class2, SuperClassList),
	!.

check_slot_join(Class1, Class2, ClassInfoIn)
	:-
	(subClassOf(Class2,SuperClassList) ->
		true
		;
		dmember(subClassOf(Class2,SuperClassList), ClassInfoIn)
	),
	dmember(Class1, SuperClassList),
	!.

check_slot_join(Class1, Class2, ClassInfoIn)
	:-
	retrieve_slots(Class1, ClassInfoIn, Class1_slots),
	retrieve_slots(Class2, ClassInfoIn, Class2_slots),

	member(X, Class1_slots),
	dmember(X, Class2_slots),
	no_common_slotted_ancestor(Class1, Class2, X, ClassInfoIn),
	objects_advise(
       "\nError: Slot '%t' common for classes:\n     %t / %t \nwith no common parent!\n",
		[X, Class1, Class2]),
	!,
	fail.
check_slot_join(_, _, _).

retrieve_slots(Class1, ClassInfoIn, Class1_slots)
	:-
	slots_for(Class1, Class1_slots),!.
retrieve_slots(Class1, ClassInfoIn, Class1_slots)
	:-
	dmember(slots_for(Class1,Class1_slots), ClassInfoIn).

parent_class(C, A, ClassInfoIn)
	:-
	subClassOf(C,SuperClassList),
	member(A, SuperClassList).
parent_class(C, A, ClassInfoIn)
	:-
	dmember(subClassOf(C,SuperClassList), ClassInfoIn),
	member(A, SuperClassList).

ancestor_class(C, A, ClassInfoIn)
	:-
	parent_class(C, A, ClassInfoIn).
ancestor_class(C, A, ClassInfoIn)
	:-
	parent_class(C, P, ClassInfoIn),
	ancestor_class(P, A, ClassInfoIn).

ancestor_with_slot(C, A, Slot, ClassInfoIn)
	:-
	ancestor_class(C, A, ClassInfoIn),
	(slots_for(A, A_Slots) ->
		true
		;
		dmember(slots_for(A, A_Slots), ClassInfoIn)
	),
	dmember(Slot, A_Slots).

no_common_slotted_ancestor(C1, C2, Slot, ClassInfoIn)
	:-
	ancestor_with_slot(C1, A, Slot, ClassInfoIn),
	ancestor_with_slot(C2, A, Slot, ClassInfoIn),
	!,
	fail.
no_common_slotted_ancestor(_, _, _, _).


new_slots_ok(LocalSlots, SuperSlots, ClassName, ClassList)
	:-
	member(X,LocalSlots),
	member(X,SuperSlots),
	objects_advise("\nError: Slot %t for %t exists in parent(s): %t\n",
				   [X, ClassName, ClassList]),
	!,
	fail.
new_slots_ok(_, _, _,_).

create_class_make_pred(ClassName, MakePred)
	:-
	catenate([make,ClassName,'Struct'], MakePred).

get_super_slots(ClassList, SuperSlots, ClassInfo)
	:-
	get_super_slots(ClassList, [], SuperSlots, ClassInfo).
	
get_super_slots([], SuperSlots, SuperSlots, _).
get_super_slots([Class | ClassList], Accum, SuperSlots, ClassInfo)
	:-
	(slots_for(Class, ClassSlots),!;
		dmember(slots_for(Class, ClassSlots),ClassInfo) ),
	push_diff(ClassSlots, Accum, NewAccum),
	get_super_slots(ClassList, NewAccum, SuperSlots, ClassInfo).

push_diff([], BaseList, BaseList).
push_diff([Item | RestItems], BaseList, Result)
	:-
	dmember(Item, BaseList),
	!,
	push_diff(RestItems, BaseList, Result).
push_diff([Item | RestItems], BaseList, [Item | Result])
	:-
	push_diff(RestItems, BaseList, Result).

record_class_slot_constraints([], _, _, _,Code, Code).
record_class_slot_constraints([Constr | RestConstraints], ClassName, 
							  Module, CompleteSlots, CodeIn, CodeOut)
	:-
	interp_slot_constr(Constr, CompleteSlots, ClassName, CodeIn, CodeInter),
	record_class_slot_constraints(RestConstraints, ClassName, Module, 
								  CompleteSlots, CodeInter, CodeOut).

interp_slot_constr(Slot < List, CompleteSlots, ClassName, CodeIn, CodeOut)
	:-
	member(Slot, CompleteSlots),
	!,
	CodeIn = [slot_constraint(ClassName,Slot,Value,member(Value,List)) 
			  | CodeOut].
interp_slot_constr(Slot < List, CompleteSlots, ClassName, CodeIn, CodeIn).

interp_slot_constr(Slot = Constant, CompleteSlots, ClassName, CodeIn, CodeOut)
	:-
	member(Slot, CompleteSlots),
	!,
	CodeIn = [slot_constraint(ClassName,Slot,Value,Value=Constant) 
			  | CodeOut].
interp_slot_constr(Slot = Constant, CompleteSlots, ClassName, CodeIn, CodeIn).

interp_slot_constr(SlotName-SlotVar^Constraint, CompleteSlots, ClassName, CodeIn, CodeOut)
	:-
	member(SlotName, CompleteSlots),
	!,
	CodeIn = [slot_constraint(ClassName,SlotName,SlotVar,Constraint) 
			  | CodeOut],
	assert(slot_constraint(ClassName,SlotName,SlotVar,Constraint)).
interp_slot_constr(SlotName-Slot^Constant, CompleteSlots, ClassName, CodeIn, CodeIn).

record_class_slot_defaults([], _, _, _,Code, Code).
record_class_slot_defaults([Constr | RestConstraints], ClassName, 
							  Module, CompleteSlots, CodeIn, CodeOut)
	:-
	interp_slot_default(Constr, CompleteSlots, ClassName, CodeIn, CodeInter),
	record_class_slot_defaults(RestConstraints, ClassName, Module, 
								  CompleteSlots, CodeInter, CodeOut).

interp_slot_default(Slot = Constant, CompleteSlots, ClassName, CodeIn, CodeOut)
	:-
	member(Slot, CompleteSlots),
	!,
	CodeIn = [slot_default(ClassName,Slot,Constant) 
			  | CodeOut],
	assert(slot_default(ClassName,Slot,Constant)).
interp_slot_default(Slot = Constant, CompleteSlots, ClassName, CodeIn, CodeIn).

/*-----------------------------------------------------------
 |		Top-most generic object class:
 |		-- defined at end of file.
 *----------------------------------------------------------*/

/* ===========================================================*
 |		In-memory transformer to generate data structure code 
 |		for "encapsulated structure datatypes" for object classes
 |		Specialized version of general file-oriented typecomp.pro
 * ===========================================================*/

makeObjectStruct(ClassName, Constructor, PropertiesList, 
				 Module, CodeIn, CodeOut)
	:-
	makeAccessPreds(PropertiesList, ClassName, 1, NumProperties1,
					Module, CodeIn, CodeInter),
	NumProperties is NumProperties1 - 1,
	makeConstructor(Constructor, ClassName, NumProperties,
				    Module, CodeInter, CodeTail),
	CodeTail = [module(objects),
				slots_for(ClassName,PropertiesList),
				endmod(objects)
				| CodeOut].

makeAccessPreds([],_, AccumNum, AccumNum,_,X,X).
makeAccessPreds([Item | Rest], ClassName, AccumNum, FinalNum,
				Module, CodeIn, CodeOut) 
	:-
	makeAccessPred(Item, ClassName, AccumNum, Module, CodeIn, CodeInter),
	NewAccumNum is AccumNum + 1,
	makeAccessPreds(Rest, ClassName, NewAccumNum, FinalNum,
					Module, CodeInter, CodeOut).

makeAccessPred(Item, ClassName, AccumNum, Module, CodeIn, CodeOut) 
	:-
	CodeIn = [module(objects),
			  slot_num(ClassName, Item, AccumNum),
			  endmod(objects)
			  | CodeOut].

makeConstructor(Constructor, ClassName, NumProperties,
				Module, CodeIn, CodeOut)
	:-
	ConstructorHead =.. [Constructor, Structure],
	CodeIn = [
			  export(Constructor/1),
			  (ConstructorHead :-
					functor(Structure, ClassName, NumProperties),
					init_nil(NumProperties,Structure)
				)
			  | CodeOut].


/*!-----------------------------------------------------------------------
 |	set_object_messages/1
 |	set_object_messages(New)		
 |	set_object_messages(New)		
 |
 |	- controls object processing messages with New = on,off
 *-----------------------------------------------------------------------*/
set_object_messages(New)		%% on,off
	:-
	abolish(object_messages,1),
	assert(object_messages(New)).

	%% controls writing of messages:
object_messages(on).		

objects_advise(MessageString, Args)
	:-
	printf(MessageString,Args),flush_output.

objects_advise(Message)
	:-
	write(Message),nl.

install_code(Code)
	:-
	install_code(Code,[user],Uses),
	sort(Uses,SUses),
	install_uses(SUses).

install_code([],_,[]).
install_code([nl | Code],ModStack,Uses)
	:-!,
	install_code(Code,ModStack,Uses).
install_code([Item | Code],ModStack,Uses)
	:-
	install_code_item(Item,ModStack,NewModStack,Uses,UsesTail),
	install_code(Code,NewModStack,UsesTail).

install_code_item(module(NewMod),CurModStack,[NewMod | CurModStack],U,U)
	:-!.  			%	$icode(-10,Mod,0,0,0).
install_code_item(endmod,[_ | ModStack],ModStack,U,U)
	:-!.  			%	$icode(-9,0,0,0,0).
install_code_item(endmod(_),[_ | ModStack],ModStack,U,U)
	:-!.  			%	$icode(-9,0,0,0,0).
install_code_item(use(Mod),ModStack,ModStack,[uses(CurMod,Mod) | UT], UT)
	:-!, 			%	$icode(-8,Mod,0,0,0).
	ModStack = [CurMod | _].
install_code_item(export(P/A),ModStack,ModStack,UT,UT)
	:-!,
	ModStack = [CurMod | _],
	CurMod:doexport(P/A).

install_code_item(dynamic(Module,P,A), ModStack,ModStack,UT,UT)
	:-!,
	builtins:dynamic0(P/A,Module).
	
install_code_item((:-Call),ModStack,ModStack,UT,UT)
	:-!,
	ModStack = [CurMod | _],
	CurMod:call(Call).
install_code_item('$execute'(Call),ModStack,ModStack,UT,UT)
	:-!,
	ModStack = [CurMod | _],
	CurMod:call(Call).
install_code_item(Item,ModStack,ModStack,UT,UT)
	:-
	ModStack = [CurMod | _],
	CurMod:assert_at_load_time(Item).

install_uses([]).
install_uses([uses(User,Used) | RestUses])
	:-
	'$icode'(-10,User,0,0,0),
	'$icode'(-8,Used,0,0,0),
	'$icode'(-9,0,0,0,0),
	install_uses(RestUses).

get_source_file(FileName)
	:-
	chooseFile(_,'*.oop',FileName0),
	(FileName0 = reconsult(FileName) ->
		true ;
		(FileName0 = consult(FileName) ->
			true ;
			FileName = FileName0
		)
	).


sort_code([], _, _, [nl,endmod,nl], _, []).
sort_code([Item | RestCode], ModStack, LeftMod, LeftCode, RightMod, RightCode)
	:-
	disp_sort_code(Item, ModStack, NextModStack,
			  	   LeftMod, LeftCode, NewLeftCode, 
			  	   RightMod, RightCode, NewRightCode),
	sort_code(RestCode, NextModStack, 
				LeftMod, NewLeftCode, RightMod, NewRightCode).

disp_sort_code((module NewMod), CurModStack, [NewMod | CurModStack],
		  		LeftMod, LeftCode, LeftCode, 
		  		RightMod, RightCode, RightCode)
	:-!.

disp_sort_code(endmod(_), [_ | NextModStack], NextModStack,
		  		LeftMod, LeftCode, LeftCode, 
		  		RightMod, RightCode, RightCode)
	:-!.

disp_sort_code(endmod, [_ | NextModStack], NextModStack,
		  		LeftMod, LeftCode, LeftCode, 
		  		RightMod, RightCode, RightCode)
	:-!.

disp_sort_code(Item, CurModStack, CurModStack,
		  		LeftMod, LeftCode, NewLeftCode, 
		  		RightMod, RightCode, NewRightCode)
	:-
	CurModStack = [CurMod | _],
	(CurMod = LeftMod ->
		LeftCode = [ Item | NewLeftCode],
		RightCode = NewRightCode
		;
		LeftCode = NewLeftCode,
		RightCode = [Item | NewRightCode]
	).



endmod.	% objects
