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

module builtins.

			/*=================================
			 |       CLASS DEFINITION
			 *================================*/

:- module_closure(defineClass, 1, defineClass).

export defineClass/2.
defineClass(Module, SpecList)
	:-
	defineClass(Module, SpecList, Assertions),
	xconsult:addclauses(Assertions, Module).

/*!-----------------------------------------------------------------------
 |	defineClass/3
 |	defineClass(Module, SpecList, Assertions)
 |	defineClass(+, +, -)
 |
 |	- defines a class in Module as specified by SpecList, placing
 |	  the necessary directives and clauses on the list Assertsions,
 |	  in a form suitable for addclauses/2 in xconsult.pro
 *-----------------------------------------------------------------------*/
export defineClass/3.

defineClass(Module, SpecList, Code)
	:-
	dmember(name=ClassName,SpecList),
	atom(ClassName),
	(dmember(module=ClassModule, SpecList) ->
		true
		;
		ClassModule = Module
	),

		%% get the name of the class action predicate:
	((dmember(action=ActionPredicateName,SpecList),
	  ActionPredicateName \= ClassName)  ->
		true;
			%% catenate(ClassName,'Action',ActionPredicateName)
			% avoid calling in the library predicate:
		atom_codes(ClassName, ClassNameChars),
		dappend(ClassNameChars, "Action", CNCAs),
		atom_codes(ActionPredicateName, CNCAs)
	),
	dmember( subClassOf = ParentClass, SpecList ),
	find_parent_slots(ParentClass, ParentSlots, ParentModule),

		%% get the class's local slots:
	(dmember(addl_slots = LocalSlots, SpecList) ->
		true;
		LocalSlots = []
	),
	((member(XXX, LocalSlots), dmember(XXX, ParentSlots)) ->
		throw(error(defineClass(slot_interections(LocalSlots, ParentSlots))))
		;
		true
	),
	append(ParentSlots, LocalSlots, CompleteSlots),

	Code = [(module ClassModule),
			 subClassOf(ClassName,ParentClass),
			 parentClassModule(ClassName,ParentModule),
			 slots_for(ClassName, CompleteSlots)
			 | Code1A ],

	(dmember(export=no,SpecList) ->
		Code1A = Code1
		;
		Code1A = [(export ClassName/2) | Code1]
	),
	createClassStatePreds(ClassName,ParentList,
						  CompleteSlots,ClassModule,Code1,Code2),

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

	(dmember(LocalActionFlag,[suppress,no]) ->
		Code3 = Code4
		;
		Code3 = [(ClassCall :- ActionCall) | Code4]
	),
	make_inheritance_calls(ParentClass, ParentModule, ClassCall, ClassModule, 
							   MessageVar,StateVar,SetStateVar,
							   Code4, Code5),

	Code5 = Code6,

		%% handle any class contraints:
	(dmember(constrs=Constraints, SpecList) ->
		record_class_slot_constraints(Constraints, ClassName, ClassModule, 
								CompleteSlots,Code6, Code7a)
		;
		Code7a = Code6
	),
	propagate_parent_slot_constraints(
			ParentClass, ParentModule, ClassName, Module, Code7a, Code7),

		%% handle any class slot defaults:
	(dmember(defaults=SlotDefaults, SpecList) ->
		record_class_slot_defaults(SlotDefaults, ClassName, ClassModule, 
								CompleteSlots,Code7, Code8a)
		;
		SlotDefaults = [], 
		Code8a = Code7
	),
	propagate_parent_slot_defaults( ParentClass, ParentModule, 
							SlotDefaults, ClassName, Module, Code8a, Code8),
	!,
	Code8 = [ dynamic(slot_constraint,4), 
			  dynamic(slot_default,3),
			  endmod  ].


find_parent_slots(genericObjects, [myClassPred,myModule,myHandle,myName], builtins).
	:-!.


find_parent_slots(Class, Slots, M)
	:-
	all_procedures(M, slots_for, 2, _),
	M:slots_for(Class, Slots),
	!.




createClassStatePreds(ClassName,ClassList, CompleteSlots,Module,CodeIn,CodeOut)
	:-
	create_class_make_pred(ClassName, MakePred),
	MakeCall =.. [MakePred, Var],
	CodeIn = [make_pred(ClassName,Var,MakeCall) | CodeTail],
	makeObjectStruct(ClassName, MakePred, CompleteSlots, Module,
				 		CodeTail, CodeOut).

create_class_make_pred(ClassName, MakePred)
	:-
		%% 	catenate([make,ClassName,'Struct'], MakePred):
		%%  avoid calling in the library predicate:
	atom_codes(ClassName, CNCs),
	dappend(CNCs, "Struct", XJ),
	dappend("make", XJ, XK),
	atom_codes(MakePred, XK).

propagate_parent_slot_constraints(
			ParentClass, ParentModule, ClassName, Module, CodeIn, CodeOut)
	:-
	findall(slot_constraint(ClassName,Slot,Value,Call),
				ParentModule:clause(slot_constraint(ParentClass,Slot,Value,Call),true),
				Assertions),
	add_on_x(Assertions, CodeIn, CodeOut).

add_on_x([], CodeIn, CodeIn).
add_on_x([A | Assertions], [A | CodeIn], CodeOut)
	:-
	add_on_x(Assertions, CodeIn, CodeOut).

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
	CodeIn = [slot_constraint(ClassName,Slot,Value,member(Value,List)) | CodeOut].
interp_slot_constr(Slot < List, CompleteSlots, ClassName, CodeIn, CodeIn).

interp_slot_constr(Slot = Constant, CompleteSlots, ClassName, CodeIn, CodeOut)
	:-
	member(Slot, CompleteSlots),
	!,
	CodeIn = [slot_constraint(ClassName,Slot,Value,Value=Constant) | CodeOut].
interp_slot_constr(Slot = Constant, CompleteSlots, ClassName, CodeIn, CodeIn).

interp_slot_constr(SlotName-SlotVar^Constraint, CompleteSlots, ClassName, CodeIn, CodeOut)
	:-
	member(SlotName, CompleteSlots),
	!,
	CodeIn = [slot_constraint(ClassName,SlotName,SlotVar,Constraint) | CodeOut].

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
	CodeIn = [slot_default(ClassName,Slot,Constant) | CodeOut].

interp_slot_default(Slot = Constant, CompleteSlots, ClassName, CodeIn, CodeIn).

propagate_parent_slot_defaults( ParentClass, ParentModule, 
					SlotDefaults, ClassName,Module,CodeIn,CodeOut)
	:-
	findall(slot_default(ClassName,Slot,Constant),
				(ParentModule:clause(slot_default(ParentClass,Slot,Constant),true),
					not(member(Slot=_, SlotDefaults))  ),
				Assertions),
	add_on_x(Assertions, CodeIn, CodeOut).

makeObjectStruct(ClassName, Constructor, PropertiesList, 
				 Module, CodeIn, CodeOut)
	:-
	makeAccessPreds(PropertiesList, ClassName, 1, NumProperties1,
					Module, CodeIn, CodeInter),
	NumProperties is NumProperties1 - 1,
	makeConstructor(Constructor, ClassName, NumProperties,
				    Module, CodeInter, CodeOut).

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
	CodeIn = [ slot_num(ClassName, Item, AccumNum) | CodeOut].

makeConstructor(Constructor, ClassName, NumProperties,
				Module, CodeIn, CodeOut)
	:-
	ConstructorHead =.. [Constructor, Structure],
	CodeIn = [
			  export(Constructor/1),
			  (ConstructorHead :-
					functor(Structure, ClassName, NumProperties),
					set_all_args(1,NumProperties,Structure,nil)
				)
			  | CodeOut].


make_inheritance_calls(ParentClass, ParentModule, DispatchPred, Module,
						MessageVar,StateVar,SetStatePred,CodeIn,CodeOut)
	:-

	ParentClassActionCall =.. [ParentClass, MessageVar, StateVar],
	((ParentModule = Module ; ParentModule = builtins ;
		(modules(Modules, MUses), dmember(ParentModule, MUses)) ) 
			->
		CodeIn = [(DispatchPred :- ParentClassActionCall) | CodeOut]
		;
		CodeIn = [
				  (DispatchPred :- (ParentModule:ParentClassActionCall)) | CodeOut]
	).

endmod.	% object_classes
