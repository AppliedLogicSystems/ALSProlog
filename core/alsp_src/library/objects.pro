/*==========================================================================*
 *==========================================================================*
 *   			objects.pro
 *
 *		Copyright (c) 1990-91 Applied Logic Systems, Inc.
 *
 *		Object-oriented programming in Prolog
 *
 *	Author:	Ken Bowen
 *	Date:	August 1990 --
 *==========================================================================*
 *==========================================================================*/

%% Note: must be consulted at end of file:
%% No longer consulted in this file since it is already loaded in alsd:
%:-[objs_run].

module objects.
use sio.
use windows.

		%% To force objs_run.pro (library file) to be loaded:
:- a := b; true.

			/*=================================
			 |   OBJECT DEFINITION/CREATION
			 *================================*/

/*!-----------------------------------------------------------------------
 *-----------------------------------------------------------------------*/
export defineObject/2.
%:-module_closure(defineObject,1,defineObject).

defineObject(Module, SpecList)
	:-
	dmember(name=ObjectName, SpecList),
	objectModule(ObjectName, Module),
	!,
	(object_messages(on) ->
		dmember(instanceOf=Class,SpecList),
		objects_advise(object_ALREADY_defined(ObjectName-Class-Module));
		true
	).

defineObject(Module, SpecList) 
	:-
	defineObject(Module, SpecList, InitialCode),
	sort_code(InitialCode, [Module], objects, ObjsCode, Module, ModCode),
	install_code([(module objects) | ObjsCode]),
	install_code([(module Module) | ModCode]),
	(object_messages(on) ->
		(dmember(name=ObjectName, SpecList) ->
			true;
			ObjectName = anonymous_object
		),
		dmember(instanceOf=Class, SpecList),
		objects_advise(object_defined(ObjectName-Class-Module));
		true
	).

defineObject(Module, SpecList)
	:-
	(object_messages(on) ->
		(dmember(name=ObjectName, SpecList) ->
			true;
			ObjectName = anonymous_object
		),
		dmember(instanceOf=Class, SpecList),
		objects_advise(object_definition_FAILURE(ObjectName-Class-Module));
		true
	).
 
export defineObject/3.
defineObject(Module, SpecList, Code) 
	:-
		%% get the object's name:
	(dmember(name=ObjectName, SpecList) ->
		true;
		gen_object_name(ObjectName),
			%% this hack is for the use of the defineObject
			%% clause of deref_val/3 in objs_run.pro,
			%% so that the 2nd goal in the body of that
			%% clause will find a value for name=.. on
			%% the SpecList:
		arg(2,SpecList,SpecListTail),
		mangle(2,SpecList,[name=ObjectName | SpecListTail])
	),

		%% get the list of classes the object is an
		%% instance of:
	dmember(instanceOf=ObjectClass, SpecList),
	(classModule(ObjectClass, ObjectClassModule) ->
		true
		;
		functor(ObjectClass, FF, 1),
		arg(1, ObjectClass, UnderClass),
		catenate(FF, UnderClass, XClassName),
		classModule(XClassName, ObjectClassModule)
	),

		%% record the object's module:
		%% options is currently trivial:
	(dmember(options=Options, SpecList) ->
		true;
		Options = []
	),
	(dmember(module=ObjectModule, SpecList) ->
		true;
		ObjectModule = Module
	),
	Code = [module(objects),
			objectModule(ObjectName, ObjectModule),
			objectClass(ObjectName, ObjectClass),
			endmod(objects),
			module(ObjectModule),
			use(ObjectClassModule),
			objectOptions(ObjectName, Options)
			| Code1],

		%% set up the object's global state variable:
	catenate('_', ObjectName, ObjectGlobalVarName),
	Code1 = [ nl,(:- make_det_gv(ObjectGlobalVarName)),nl | Code2],

		%% set up the access and set predicates for the
		%% object's (global) state:
	catenate('get_', ObjectName, GetStatePred),
	catenate('set_', ObjectName, SetStatePred),

		%% get any local slot initialization values:
	(dmember(values=LocalValues, SpecList) ->
		true;
		LocalValues = []
	),
		%% get any class slot initialization values:
	bagOf(Slot=Value, slot_default(ObjectClass,Slot,Value), ClassValues),
		%% merge them:
	merge_values(ClassValues, LocalValues, Values),

		%% set up the object initialization code:
		%% note that all initializations guaranteed by
		%% class genericObjects must be installed here;
		%% at the moment, this is only initialization of
		%% the myName slot, and is done by hand below;
		%% however, we should probably set things up
		%% so that these initializations are computed
		%% (once and for all) from the definition of
		%% genericObjects, and automatically installed
		%% below:
	ObjectInitCall =.. [ObjectName, initialize],
	create_class_make_pred(ObjectClass, ClassMakePred),
	MakeStateCall =.. [ClassMakePred, InitialState],
	SetInitStateCall =.. [SetStatePred, InitialState],
			%% worry about slot initializations:
	class_constr_init(ObjectClass,ClassInitVals),
	class_default_init(ObjectClass,ClassDefaultVals),
	object_values_init(Values, ObjInitVals),
	merge_init_vals(ClassDefaultVals, ObjInitVals, InterInitVals),
	merge_init_vals(InterInitVals, ClassInitVals, InitVals),
%	append(ClassInitVals,ObjInitVals,InitVals),
	GetStateCall_isNull =.. [GetStatePred, 0],
	(InitVals = [] ->
		InitBody0 = (GetStateCall_isNull,!,
%					 ObjectClassModule:MakeStateCall,
					 MakeStateCall,
					 setObjStruct(myName,InitialState,ObjectName),
					 SetInitStateCall
					)
		;
			/* NOTE: the peculiar order of makeState,
			   setState,getState, then set slots, is
			   necessary because the setState predicate
			   (generated by make_det_gv in blt_sys.pro)
			   installs a COPY of its arg in the global
			   variable (cf. blt_sys.pro)
			 */
		GetStateCall3 =.. [GetStatePred, State3],
		set_slots0(InitVals,SlotInitCode,State3,ObjectModule),
		InitBody0 = (GetStateCall_isNull,!,
%					 ObjectClassModule:MakeStateCall,
					 MakeStateCall,
					 SetInitStateCall,
					 GetStateCall3,
					 setObjStruct(myName,State3,ObjectName),
					 SlotInitCode)
	),
	flatten_comma_list(InitBody0, InitBody),
	Code2 = [ (ObjectInitCall :- InitBody), (ObjectInitCall :-!) | Code3],

		%% get the name of the object's action predicate,
		%% if any; to get a local action call, this must
		%% be supplied, even if one wants to use the
		%% object name:
	(dmember(action=ActionPredicateName, SpecList) ->
		true;
		ActionPredicateName = '$noAction'
	),

		%% make the schematic call on the action predicate:
	ActionCall =.. [ActionPredicateName,MessageVar,StateVar],
		%% make the schematic call on the object (for messages):
	ObjectCall =.. [ObjectName, MessageVar],
		%% make the schematic call for getting the 
		%% object state:
	GetStateCall =.. [GetStatePred, StateVar],

		%% set up the clauses connecting calls on the object
		%% (which occur when messages are sent to the object)
		%% with calls on the object's action predicate, and
		%% set the clauses in the action predicate which implement
		%% the inheritance of class methods (from classes given
		%% on ObjectClass):
	catenate('dispatch_', ObjectName, DispatchPred), 
	DispatchCall =.. [DispatchPred, MessageVar,StateVar],
	AccessDispatchCall =..[DispatchPred,(Exp00 := Exp01),StateVar],
	ClassCall =.. [ObjectClass, MessageVar, StateVar],
	(ActionPredicateName = '$noAction' ->
		Code3 = [(ObjectCall :- GetStateCall,DispatchCall),
			 	(AccessDispatchCall :- var(Exp00),!, 
										(Exp00 := StateVar^Exp01) ),
			 	(AccessDispatchCall :- !, (StateVar^Exp00 := Exp01) ),
			 	(DispatchCall :- ClassCall)
			 	| Code4]
		;
		Code3 = [(ObjectCall :- GetStateCall,DispatchCall),
			 	(AccessDispatchCall :- var(Exp00),!, 
										(Exp00 := StateVar^Exp01) ),
			 	(AccessDispatchCall :- !, (StateVar^Exp00 := Exp01) ),
			 	(DispatchCall :-ActionCall,!),   %% see below(##)
			 	(DispatchCall :- ClassCall)
			 	| Code4]
	),

	!,
	Code4 = [endmod(ObjectModule)].

/*  (##)  Later change this: replace this clause by specific
    (local) override methods (clauses for this dispatch predicate)
	which snag the messages invoking methods which this object
	is going to locally override
*/

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

%ancestor(ClassA, ClassB)
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

gen_object_name(ObjectName)
	:-
	get_next_object_counter(Count),
	catenate('anonymous_object_',Count,ObjectName).

get_next_object_counter(Count)
	:-
	retract(object_counter(CurCount)),
	!,
	Count is CurCount + 1,
	assert(object_counter(Count)).
get_next_object_counter(1)
	:-
	assert(object_counter(1)).
	

			/*=================================
			 |       CLASS DEFINITION
			 *================================*/

/*!-----------------------------------------------------------------------
 *-----------------------------------------------------------------------*/
export defineClass/2.
%:-module_closure(defineClass,1,defineClass).

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
	dmember(subclassOf=ClassList,SpecList),
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
			 module(Module)
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

/*
get_super_slots([],[],_).
get_super_slots([Class | RestClassList],SuperSlots,ClassInfo)
	:-
	(slots_for(Class, ClassSlots),!;
		dmember(slots_for(Class, ClassSlots),ClassInfo) ),
	get_super_slots(RestClassList, RestSuperSlots, ClassInfo),
	append(ClassSlots, RestSuperSlots, SuperSlots).
*/
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
 *-----------------------------------------------------------------------*/
export set_object_messages/1.
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
	'$icode'(-10,CurMod,0,0,0),
	'$icode'(-11,P,A,0,0),
	'$icode'(-9,0,0,0,0).
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

/*!----------------------------------------------------------
 *-----------------------------------------------------------*/

get_source_file(FileName)
	:-
%	selectFile(gui_file_select_manager,'*.oop',FileName0),
	chooseFile(_,'*.oop',FileName0),
	(FileName0 = reconsult(FileName) ->
		true ;
		(FileName0 = consult(FileName) ->
			true ;
			FileName = FileName0
		)
	).

export opf/1.
opf(FileName) :- 
	objectProcessFile(FileName,_).

export opf/0.
opf :-
	set_prolog_flag(undefined_predicate, fail),
	get_source_file(FileName),
	disp_opf(FileName).


disp_opf('$noChoice') 
	:-!,
	abolish(object_loaded_file,1),
	halt.
disp_opf(FileName)
	:-
	objectProcessFile(FileName,_),

	!,
	nl,nl,
	opf.




export objectProcessFile_cl/0.
objectProcessFile_cl
	:-
	set_prolog_flag(undefined_predicate, fail),
	builtins:command_line(Files),
	!,
	objectProcessFiles(Files).


export objectProcessFiles/1.
objectProcessFiles([]) :-!.
objectProcessFiles([FileName | FileNames])
	:-!,
	objectProcessFile(FileName,_),
	objectProcessFiles(FileNames).

export objectProcessFile/2.
objectProcessFile(FileName, Records)
	:-
	(filePlusExt(FN,XT,FileName) ->
		SourceFile = FileName,
		(XT \= pro ->
			filePlusExt(FN,pro,TargetFile)
			;
			filePlusExt(FN,ppo,TargetFile)
		)
		;
		catenate(FileName, '.oop', SourceFile),
		catenate(FileName, '.pro', TargetFile)
	),
	objectProcessFile(SourceFile, TargetFile, Records).

/*!----------------------------------------------------------
 *-----------------------------------------------------------*/
export objectProcessFile/3.

objectProcessFile(SourceFile, TargetFile,
				  objinfo(SourceFile,TargetFile,RecordInfo))
	:-
	open(SourceFile,read,SourceStream,[]),
	read_terms(SourceStream, SourceTerms),
	close(SourceStream),
	do_objectProcess(SourceTerms,SinkList, [], RecordInfo),
%	assert_clauses_for_mod(SinkList,objects),
	open(TargetFile,write,TargetStream,[]),
	write_clause(TargetStream, (:- op(600,xfy,'%lettervar%'(''':='''))),[]),
	write_clauses(TargetStream, SinkList, [line_length(63)]),
	close(TargetStream),
	abolish(slot_default,3),
	abolish(slot_constraint,4).
objectProcessFile(SourceFile, TargetFile)
	:-
%%% needs fixing:
	seen.

export do_objectProcess/4.
do_objectProcess(SourceList,SinkList,SinkListTail,RecordInfo)
	:-
	objectProcess([user],[],SourceList,SinkList,SinkListTail,RecordInfo,[]).

export do_objectProcess/5.
do_objectProcess(SourceList,Module,SinkList,SinkListTail,RecordInfo)
	:-
	objectProcess([Module,user],[],SourceList,SinkList,SinkListTail,
					RecordInfo,[]).

objectProcess(ModStack,ClassInfo,Source,Sink,SinkTail,Ri,RiT)
	:-
	Source = [Item | RestSource],
	!,
	ModStack = [TopMod|_],
	objs_def_msg(item_defined, [Item], TopMod),
	disp_objectProcess(Item, ModStack,ClassInfo, RestSource,Sink,SinkTail,
						Ri,RiT).

objectProcess(ModStack,ClassInfo,[],Sink,Sink,RiT,RiT).


disp_objectProcess(end_of_file, _,_,_,SinkTail,SinkTail,RiT,RiT) :-!.

disp_objectProcess((:- requires(Files)), ModStack,ClassInfo,
					Source,Sink,SinkTail,Ri,RiT)
	:-!,
	object_load_required(Files),
	objectProcess(ModStack,ClassInfo,Source,Sink,SinkTail,Ri,RiT).

disp_objectProcess((module Mod), ModStack,ClassInfo,
					Source,Sink,SinkTail,Ri,RiT)
	:-!,
	Sink = [ nl,(module Mod), (use objects), nl | RestSink],
	objectProcess([Mod | ModStack],ClassInfo,Source,RestSink,SinkTail,Ri,RiT).

disp_objectProcess(endmod, [_ | RestModStack],ClassInfo,
					Source,Sink,SinkTail,Ri,RiT)
	:-!,
	Sink = [ endmod | RestSink ],
	objectProcess(RestModStack,ClassInfo,Source,RestSink,SinkTail,Ri,RiT).

disp_objectProcess((:- defineObject(SpecList)), ModStack,ClassInfo,
					Source,Sink,SinkTail,Ri,RiT)
	:-!,
	disp_objectProcess(defineObject(SpecList), ModStack,ClassInfo,
						Source,Sink,SinkTail,Ri,RiT).

disp_objectProcess((defineObject(SpecList) :- Condition), ModStack,ClassInfo,
					Source,Sink,SinkTail,Ri,RiT)
	:-
	ModStack = [M | _],
	call(M:Condition), !,
	disp_objectProcess(defineObject(SpecList), ModStack,ClassInfo,
					Source,Sink,SinkTail,Ri,RiT).

disp_objectProcess(defineObject(SpecList), ModStack,ClassInfo,
					Source,Sink,SinkTail,Ri,RiT)
	:-
	ModStack = [ Mod | _],
	defineObject(Mod, SpecList, InitialCode),
	sort_code(InitialCode, [Mod], objects, ObjsCode, Mod, ModCode),
	append(ModCode, RestSink, Tmp),
	append([nl,(module objects) | ObjsCode], Tmp, Sink),
	
	obj_info(SpecList,Mod,Ri,IntRiT),
	!,
	objectProcess(ModStack,ClassInfo,Source,RestSink,SinkTail,IntRiT,RiT).
disp_objectProcess(defineObject(SpecList), ModStack,ClassInfo,
					Source,Sink,SinkTail,Ri,RiT)
	:-!,
	objs_error_msg(object,SpecList),
	objectProcess(ModStack,ClassInfo, Source,Sink,SinkTail,Ri,RiT).

disp_objectProcess((:- defineClass(SpecList)), ModStack,ClassInfo,
					Source,Sink,SinkTail)
	:-!,
	disp_objectProcess(defineClass(SpecList), ModStack,ClassInfo,
						Source,Sink,SinkTail,Ri,RiT).

disp_objectProcess((defineClass(SpecList) :- Condition), ModStack,ClassInfo,
					Source,Sink,SinkTail,Ri,RiT)
	:-
	ModStack = [M | _],
	call(M:Condition), !,
	disp_objectProcess(defineClass(SpecList), ModStack,ClassInfo,
					Source,Sink,SinkTail,Ri,RiT).
disp_objectProcess(defineClass(SpecList), ModStack,ClassInfo,
					Source,Sink,SinkTail,Ri,RiT)
	:-
	ModStack = [ Mod | _],
	dmember(name=TheName, SpecList),
	(atom(TheName) ->
		defineClass(Mod, SpecList, ClassInfo, NewClassInfo, InitialCode)
		;
		functor(TheName, Operator, _),
		defineCustomClass(Operator, SpecList, RevisedSpecList),
		defineClass(Mod, RevisedSpecList, ClassInfo, NewClassInfo, InitialCode)
	),

	sort_code(InitialCode, [Mod], objects, ObjsCode, Mod, ModCode),
	append(ModCode, RestSink, Tmp),
	append([nl,(module objects),nl | ObjsCode], Tmp, Sink),

	cls_info(SpecList,Mod,Ri,IntRiT),
	!,
	objectProcess(ModStack,NewClassInfo,Source,RestSink,SinkTail,IntRiT,RiT).

disp_objectProcess(defineClass(SpecList), ModStack,ClassInfo,
					Source,Sink,SinkTail,Ri,RiT)
	:-!,
	objs_error_msg(class,SpecList),
	objectProcess(ModStack,ClassInfo, Source,Sink,SinkTail,Ri,RiT).

disp_objectProcess(objects_extension(Tag,ClauseList), ModStack,ClassInfo,
					Source,Sink,SinkTail,Ri,RiT)
	:-
	\+(extesion_installed(Tag)),
	ModStack = [M | _],
	assert_all(ClauseList),
	assert(extension_installed(Tag)),
	append([nl,nl,(module objects) | ClauseList],
			[extension_installed(Tag),nl,endmod,nl,nl | NewSink], Sink),
	objectProcess(ModStack,ClassInfo, Source,NewSink,SinkTail,Ri,RiT).

disp_objectProcess(objects_extension(Tag,ClauseList), ModStack,ClassInfo,
					Source,Sink,SinkTail,Ri,RiT)
	:-
	objectProcess(ModStack,ClassInfo, Source,Sink,SinkTail,Ri,RiT).

disp_objectProcess(Item, ModStack,ClassInfo,Source,Sink,SinkTail,Ri,RiT)
	:-
	Sink = [Item | RestSink],
	objectProcess(ModStack,ClassInfo,Source,RestSink,SinkTail,Ri,RiT).

obj_info(SpecList,Mod,Ri,IntRiT)
	:-
	(dmember(instanceOf=ObjClass,SpecList) ->
		true;
		ObjClass = unknown
	),
	(dmember(name=ObjName,SpecList) ->
		true;
		ObjName = unknown
	),
	Ri = [object(ObjClass,ObjName,Mod) | IntRiT].

cls_info(SpecList,Mod,Ri,IntRiT)
	:-
	(dmember(subclassOf=SuperClasses,SpecList) ->
		true;
		SuperClasses = [genericObjects]
	),
	(dmember(name=ClsName,SpecList) ->
		true;
		ClsName = unknown
	),
	Ri = [class(ClsName,SuperClasses,Mod) | IntRiT].

object_load_required([]).
object_load_required([File | Files])
	:-!,
	object_load_required(File),
	object_load_required(Files).
object_load_required(File)
	:-
	(object_loaded_file(File) ; gui_loaded_file(File)),!.
object_load_required(File)
	:-
	consult(File),
	assert(object_loaded_file(File)).

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


export merge_defaults/3.
merge_defaults([],PrimaryDefaults,PrimaryDefaults).
merge_defaults([Tag=Val | BaseDefaults],SpecDefaults,NewDefaults)
	:-
	dmember(Tag=_,SpecDefaults),
	!,
	merge_defaults(BaseDefaults,SpecDefaults,NewDefaults).
merge_defaults([Tag=Val | BaseDefaults],SpecDefaults,[Tag=Val | NewDefaults])
	:-
	merge_defaults(BaseDefaults,SpecDefaults,NewDefaults).


objs_def_msg(Tag, Args, Mod)
	:-
	objs_msg_setup(Tag, Args, Mod, Format, FixedArgs),
	printf(Format, FixedArgs),
	flush_output.

objs_msg_setup(item_defined, [defineClass(PList)],  Mod,
				"\n>DefineClass: %t in mod=%t", [Name,Mod])
	:-!,
	dmember(name=Name, PList).

objs_msg_setup(item_defined, [defineObject(PList)],  Mod,
				"--DefineObject: %t Type=%t in mod=%t\n", [Name,Class,Mod])
	:-!,
	dmember(name=Name, PList),
	dmember(instanceOf=Class, PList).
	
objs_msg_setup(item_defined, [objects_extension(Name,_)], Mod,
			 ">%t\n", [objects_extension=Name])
	:-!.
objs_msg_setup(item_defined, [Arg], Mod, ".", []).


objs_error_msg(ItemType,SpecList)
	:-
	printf("\n>>>Error processing %t:\n   %t\n<<<\n\n",[ItemType,SpecList]).

assert_clauses_for_mod([],_).
assert_clauses_for_mod(end_of_file,_).
assert_clauses_for_mod([(module Mod) | List],Mod)
	:-!,
	assert_clauses_to_endmod(List,Mod, TailList),
	assert_clauses_for_mod(TailList,Mod).
assert_clauses_for_mod([_ | List],Mod)
	:-
	skip_clauses_to_mod(List,Mod,TailList),
	assert_clauses_for_mod(TailList,Mod).

skip_clauses_to_mod([],_,[]).
skip_clauses_to_mod([(module Mod) | List],Mod,[(module Mod) | List])
	:-!.
skip_clauses_to_mod([_ | List],Mod,ResultList)
	:-
	skip_clauses_to_mod(List,Mod,ResultList).

assert_clauses_to_endmod([],_, []).
assert_clauses_to_endmod([endmod | List],_, List)
	:-!.
assert_clauses_to_endmod([endmod(_) | List],_, List)
	:-!.
assert_clauses_to_endmod([Clause | List],Mod, TailList)
	:-
	Mod:assert(Clause),
	assert_clauses_to_endmod(List,Mod, TailList).

	/*=============================================
	 |	SOME DEVELOPMENT TOOLS
	 *============================================*/

	%% List of all modules which have objects defined in them:
export mods_with_objs/1.
mods_with_objs(ModsList)
	:-
	objects:setOf(M, Ob^objectModule(Ob,M), ModsList).

	%% List of all modules which have classes defined in them:
export mods_with_classes/1.
mods_with_classes(ModsList)
	:-
	objects:setOf(M, Cl^classModule(Cl,M), ModsList).

	%% List all objects defined in a module:
export objs_in_mod/2.
objs_in_mod(Mod, ObjsList)
	:-
	objects:setOf(Ob, objectModule(Ob,Mod), ObjsList).

	%% List all classes defined in a module:
export classes_in_mod/2.
classes_in_mod(Mod, ClassList)
	:-
	objects:setOf(Cl, classModule(Cl,Mod), ClassList).

export dpos/1.
dpos(State)
	:-
	dump_object_by_state(State, SlotEqns),
	write_eqns(SlotEqns).

	% Generate list of eqns: slotName=slotVal, for an object:
dump_object_by_state(State, SlotEqns)
	:-
	ObjectName := State^myName,
	objects:objectClass(ObjectName, ObjCl),
	objects:slots_for(ObjCl, SlotNameList),
	dump_obj_eqns(SlotNameList, 1, State, SlotEqns).

export don/1.
don(ObjName)
	:-
	dump_object_by_name(ObjName, SlotEqns),
	write_eqns(SlotEqns).

export dump_object_by_name/2.
dump_object_by_name(ObjName, SlotEqns)
	:-
	objects:objectModule(ObjName, ObjMod),
	catenate(get_,ObjName,AccessPred),
	StateCall =..[AccessPred, State],
	ObjMod:call(StateCall),
	objects:objectClass(ObjName, ObjCl),
	objects:slots_for(ObjCl, SlotNameList),
	dump_obj_eqns(SlotNameList, 1, State, SlotEqns).

dump_obj_eqns([], _, _, []).
dump_obj_eqns([SlotName | SlotNameList], SlotNum, State, 
			  [SlotName=SlotVal | SlotEqns])
	:-
	arg(SlotNum, State, SlotVal),
	NextSlotNum is SlotNum + 1,
	dump_obj_eqns(SlotNameList, NextSlotNum, State, SlotEqns).

	% Generate selective list of eqns: slotName=slotVal, for an object:
export sdo/1.
sdo(Obj) :- selective_dump_object(Obj).

export sdos/1.
sdos(State)
	:-
	selective_dump_object_by_state(State, SlotEqns),
	write_eqns(SlotEqns).

export selective_dump_object_by_state/2.
selective_dump_object_by_state(State, SlotEqns)
	:-
	ObjectName := State^myName,
	objects:objectClass(ObjectName, ObjCl),
	objects:slots_profile_for(ObjectName, SlotProfileList),
	dump_obj_eqns(SlotProfileList, State, SlotEqns).

export sdon/1.
sdon(ObjName)
	:-
	selective_dump_object_by_name(ObjName, SlotEqns),
	write_eqns(SlotEqns).

export selective_dump_object_by_name/2.
selective_dump_object_by_name(ObjName, SlotEqns)
	:-
	objects:objectModule(ObjName, ObjMod),
	catenate(get_,ObjName,AccessPred),
	StateCall =..[AccessPred, State],
	ObjMod:call(StateCall),
	objects:objectClass(ObjName, ObjCl),
	objects:slots_profile_for(ObjName, SlotProfileList),
	dump_obj_eqns(SlotProfileList, State, SlotEqns).

dump_obj_eqns([], _, []).
dump_obj_eqns([SlotName=SlotNum | SlotProfileList], State, 
			  [SlotName=SlotVal | SlotEqns])
	:-
	arg(SlotNum, State, SlotVal),
	dump_obj_eqns(SlotProfileList, State, SlotEqns).

export set_obj_profile/2.
set_obj_profile(SlotNameList, ObjName)
	:-
	objects:objectClass(ObjName,ObjClass),
	assign_slot_nums(SlotNameList,ObjClass,SlotProfileList),
	(objects:retract(slots_profile_for(ObjName,_)) -> true ; true),
	objects:assert(slots_profile_for(ObjName,SlotProfileList)).

assign_slot_nums([],_,[]).
assign_slot_nums([SlotName | SlotNameList],ObjClass,
				 [SlotName=SlotNum | SlotProfileList])
	:-
	objects:slot_num(ObjClass,SlotName,SlotNum),
	assign_slot_nums(SlotNameList,ObjClass,SlotProfileList).

write_eqns([]).
write_eqns([SlotName=SlotVal | SlotEqns])
	:-
	printf_opt('%t=%t\n',[SlotName,SlotVal],
			[maxdepth(7),depth_computation(flat)]),
	write_eqns(SlotEqns).

export obj_slots/2.
obj_slots(ObjName, SlotsList)
	:-
	objects:objectClass(ObjName, ObjCl),
	objects:slots_for(ObjCl, SlotsList).



endmod.	% objects

