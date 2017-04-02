/*==========================================================================*
 | 			objs_run.pro
 |	Copyright (c) 1990-96 Applied Logic Systems, Inc.
 |
 |		Object-oriented programming in Prolog
 |		Run-time support for ObjectPro, not including run-time
 |		definition of objects & classes on the fly;
 |
 |	Author:	Ken Bowen
 |	Date:	August 1990 --
 *==========================================================================*/

module builtins.

/*-----------------------------------------------------------------*
 |	The call to 
 |
 |			:- make_hash_table("_object_handle") 
 |
 |	will build the following access predicates: 
 |
 |		set_object_handle/2     get_object_handle/2
 |		reset_object_handle/0   del_object_handle/2   
 |		pget_object_handle/2    pdel_object_handle/2
 *-----------------------------------------------------------------*/

:-	make_hash_table("_object_handle"), set_object_handle(counter, 0).
export get_object_handle/2.
export set_object_handle/2.

export object_handle/2.
object_handle(Object, Handle)
	:-
	arg(3, Object, IH),
	(integer(IH) ->
		Handle = IH
		;
			% next_object_handle(Handle),
		get_object_handle(counter, Prev),
		Handle is Prev + 1,
		set_object_handle(counter, Handle),
		mangle(3, Object, Handle)
	).

			/*=================================
			 |   MESSAGE SENDING
			 *================================*/

/*!-----------------------------------------------------------------------
 |	send/2
 |	send(Object, Message)
 |	send(+, +)
 |
 |	- sends a message to an object
 *-----------------------------------------------------------------------*/
:-module_closure(send,2,do_send).

do_send(Module, Object, Message)
	:-
	send(Object, Message, Module).

send(ObjectName, Message, Module)
	:-
	atomic(ObjectName),
	!,
	get_object_handle(ObjectName, Object),
	arg(2, Object, MyModule),
	send(Object, Message, MyModule).

send(Object^SlotDescrip, Message, Module)
	:-!,
	Module:accessObjStruct(SlotDescrip, Object, SubObject),
	send(SubObject, Message, Module).

send(Object, Message, Module)
	:-
	Module:accessObjStruct(myClassPred, Object, ClassPred),
	Module:accessObjStruct(myModule, Object, MyModule),
	ActionCall =.. [ClassPred, Message, Object],
	call(MyModule:ActionCall).

/*!-----------------------------------------------------------------------
 *-----------------------------------------------------------------------*/
export send_self/2.
send_self(State, Message)
	:-
	arg(2, State, Module),
	send(State, Message, Module).

/*!-----------------------------------------------------------------------
 |	send_all/2
 |	send_all(List, Msg)
 |	send_all(+, +)
 |	
 |	- try to send Msg to all objects on List; fail if any fails
 *-----------------------------------------------------------------------*/
export send_all/2.
send_all([], _).
send_all([Kid | Children], Msg)
    :-
    send(Kid, Msg),
    send_all(Children, Msg).

/*!-----------------------------------------------------------------------
 |	send_each/2
 |	send_each(List, Msg)
 |	send_each(+, +)
 |	
 |	- send Msg to all objects on List independently of individual success/failure
 *-----------------------------------------------------------------------*/
export send_each/2.
send_each([], _).
send_each([Kid | Children], Msg)
    :-
    (send(Kid, Msg) -> true ; true),
    send_each(Children, Msg).

/*!-----------------------------------------------------------------------
 *-----------------------------------------------------------------------*/
export send_parent/2.
send_parent(Object, Msg)
	:-
	arg(1, Object, MyClass),
	arg(2, Object, MyModule),
	MyModule:subClassOf(MyClass, ParentClass),
	ParentCall =.. [ParentClass, Msg, Object],
	MyModule:parentClassModule(MyClass, ParentModule),
	ParentModule:ParentCall.

/*!-------------------------------------------------------
 |	accessObjStruct/3
 |	accessObjStruct(SlotExpr, Struct, Value)
 |	accessObjStruct(+, +, -)
 |
 | - access the value of a slot of an object
 |
 |	If SlotExpr is the name of a primitive (non-object) 
 |	slot of Struct, Value is unified with its (current) 
 |	value;  If SlotExpr is of the form 
 |		SlotName1^SlotName2
 |	where SlotName1 is a slot of Struct which itself an 
 |	object, obtains the object Value1 which is the current
 |	value of SlotName1 in Struct, and recursively 
 |	calls accessObjStruct(SlotName2, Value1, Value).
 *!------------------------------------------------------*/

export accessObjStruct/3.
accessObjStruct((SlotName1^SlotName2), Struct, Value)
	:-!,
	functor(Struct, Class, _),
	arg(2, Struct, Module),
	Module:slot_num(Class, SlotName1, Offset),
	arg(Offset, Struct, Value1),
	accessObjStruct(SlotName2, Value1, Value).

accessObjStruct(SlotName, Struct, Value)
	:-
	functor(Struct, Class, _),
	arg(2, Struct, Module),
	Module:slot_num(Class, SlotName, Offset),
	arg(Offset, Struct, Value).

/*!-------------------------------------------------------
 |	setObjStruct/3.
 |	setObjStruct(SlotExpr, Struct, Value)
 |	setObjStruct(+, +, +)
 |
 | - set (destructively) the value of a slot of an object
 |
 |	If SlotExpr is the name of a primitive (non-object) 
 |	slot of Struct, the current value of SlotExpr is
 |	destructively changed to Value using mangle/3;
 |	If SlotExpr is of the form 
 |		SlotName1^SlotName2
 |	where SlotName1 is a slot of Struct which itself an 
 |	object, obtains the object Value1 which is the current
 |	value of SlotName1 in Struct, and recursively 
 |	calls setObjStruct(SlotName2, Value1, Value).
 *!------------------------------------------------------*/

export setObjStruct/3.
setObjStruct((SlotName1^SlotName2), Struct, Value)
	:-!,
	functor(Struct, Class, _),
	arg(2, Struct, Module),
	Module:slot_num(Class, SlotName, Offset),
	arg(Offset, Struct, Value1),
	setObjStruct(SlotName2, Value1, Value).

setObjStruct(SlotName, Struct, Value)
	:-
	functor(Struct, Class, _),
	arg(2, Struct, Module),
	satisfy_slot_constrs(Class,SlotName,Module,Value),
	Module:slot_num(Class, SlotName, Offset),
	mangle(Offset, Struct, Value).

satisfy_slot_constrs(Class,SlotName,Module,Value)
	:-
	Module:clause(slot_constraint(Class,SlotName,Value,Call),true),
	!,
	( call(Module:Call) -> true ;
	  open(atom(Msg), write, CS),
	  printf(CS,'Constraint [%t slot %t, value=%t] failed: %t\n', [Class,SlotName,Value,Call]),
	  close(CS),
		%% do this to avoid unbinding a variable during the throw:
	  Ball =.. [constraint_error, Msg],
	  throw(Ball)
	).

satisfy_slot_constrs(_,_,_,_).

export all_setObjStruct/2.
all_setObjStruct([], _).
all_setObjStruct([Tag = Value | TagEqns], State)
	:-
	setObjStruct(Tag, State, Value),
	all_setObjStruct(TagEqns, State).

/*!-------------------------------------------------------
 |	':='/2.
 |	Expr1 := Expr2
 |	? := +
 |
 | - set or access the value of an object slot
 |
 |	If Expr1 is a variable or a non-variable which NOT of
 |	the form Struct^SlotExpr, then Expr2 must be of the form 
 |	Struct^SlotExpr, and the value of SlotExpr in Struct 
 |	is extracted and unified with Expr1;
 |
 |	If Expr1 is of the form A^SlotExpr, then A can either
 |	be an object state Struct, or can be the name of an
 |	object, and the value of SlotExpr in A is set to Value
 *!------------------------------------------------------*/
export (:=) / 2.

Var := Struct^SlotExpr
	:-
	var(Var),!,
	(atom(Struct) ->
		send(Struct,get_value(SlotExpr,Var))
		;
		accessObjStruct(SlotExpr, Struct, Var)
	).

Struct^SlotName := Value
	:-
	setObjStruct(SlotName, Struct, Value),!.

ObjectName^SlotName := Value
	:-
	send(ObjectName,set_value(SlotName,Value)).

Value := Struct^SlotExpr
	:-
	accessObjStruct(SlotExpr, Struct, Value).

			/*=================================
 			 | Top-most generic object class:
			 *================================*/
/*!-------------------------------------------------------
 |	genericObjects/2
 |	genericObjects(Message, State)
 |	genericObjects(+,+)
 |
 | -	Top-most generic object class definition
 |
 |	Define the generic top-most object class; note that
 |	the name of the action predicate must be different 
 |	from the class name in this case, since if they were
 |	the same, defineClass will produce an open-ended
 |	loop of the form p :- p  where all arguments in sight
 |	are variables. The current definition is:
 |
 |	:- defineClass(objects,[name=genericObjects,
 |					action=genObjs,
 |					subClassOf=[],
 |					addl_slots=[myClassPred,myModule,myName,myHandle],
 |					export = yes]).
 *!------------------------------------------------------*/

 /*
 |
 |	The above is installed in the file objs_generic.oop, and
 |	is processed with objectsProcessFile/1 to yield the
 |	file objs_generic.pro.  Remove the op declaration,
 |	the reconsult of objs_run.pro, and the module/endmod
 |	declarations, and include the result below:
 */


make_pred(genericObjects,Obj,makegenericObjectsStruct(Obj)).

slot_num(genericObjects,myClassPred,1).
slot_num(genericObjects,myModule,2).
slot_num(genericObjects,myHandle,3).
slot_num(genericObjects,myName,4).

makegenericObjectsStruct(Obj) 
	:-
    functor(Obj,genericObjects,2), 
	arg(1,Obj,nil),
	arg(2,Obj,user),
	arg(3,Obj,nil),
	arg(4,Obj,nil),
	arg(5,Obj,nil).

slots_for(genericObjects,[myClassPred,myModule,myName,myHandle]).


export genericObjects/2.
genericObjects(_1595,_1593) 
	:- 
	genObjs(_1595,_1593).

export set_all_args/4.
set_all_args(Cur,Size,FF,ArgVal)
	:-
	Cur > Size, !.
set_all_args(Cur,Size,FF,ArgVal)
	:-
	arg(Cur,FF,ArgVal),
	Next is Cur +1,
	set_all_args(Next,Size,FF,ArgVal).


genObjs(get_value(Slot,Value),State)
	:-
	accessObjStruct(Slot,State,Value).

genObjs(set_value(Slot,Value),State)
	:-
	setObjStruct(Slot,State,Value).

/*
genObjs(set_value_copy(Slot,Value),State)
	:-
	copy_term(Value, CopyValue),
	setObjStruct(Slot,State,CopyValue).
*/

genObjs(append_value(Slot,AddlValue),State)
	:-
	accessObjStruct(Slot,State,CurValue),
	append(CurValue,AddlValue,NewValue),
	setObjStruct(Slot,State,NewValue).

genObjs(push_value(Slot,AddlValue),State)
	:-
	accessObjStruct(Slot,State,CurValue),
	setObjStruct(Slot,State,[AddlValue | CurValue]).

genObjs(lookup(Slot,Pattern),State)
	:-
	accessObjStruct(Slot,State,CurSlotValue),
	dmember(Pattern, CurSlotValue),!.

genObjs(send_self(Message),State)
	:-
	send_self(State, Message).

genObjs(your_state(State),State).


/*!-------------------------------------------------------
 |	clone/2
 |	clone(Obj1, Obj2)
 |	clone(+, +)
 |
 | - clone an object
 |
 |	The values of all slots of Obj2 which are currently
 |	nil are set to the corresponding values from Obj1.
 *!------------------------------------------------------*/
clone(Obj1, Obj2)
	:-
	send(Obj1, your_state(State1)),
	send(Obj2, your_state(State2)),
	functor(State1, _, NumSlots),
	functor(State2, _, NumSlots),
	map_states(NumSlots,State1, State2).

map_states(0,State1, State2) :-!.
map_states(CurSlot,State1, State2)
	:-
	arg(CurSlot,State2, S2entry),
	(S2entry \= nil ->
		true ;
		arg(CurSlot,State1,S1entry),
		mangle(CurSlot,State2,S1entry)
	),
	NextSlot is CurSlot - 1,
	map_states(NextSlot,State1, State2).


			/*=================================
			 |   OBJECT DEFINITION/CREATION
			 *================================*/

:- module_closure(create_object, 2, create_object).

/*!-----------------------------------------------------------------------
 |	create_object/3
 |	create_object(Module, SpecList, Object)
 |	create_object(+, +, -)
 |
 |	- creates an (anonymous) object in specified by SpecList
 *-----------------------------------------------------------------------*/
:- dynamic(common_slot/2).
:- dynamic(common_slot_gv/2).

create_object(Module, SpecList, Object)
	:-
	dmember(instanceOf=ObjectClass, SpecList),
	(dmember(options=Options, SpecList) ->
		true;
		Options = []
	),

		%% Needs to take place before creation of the object
		%% so that existence of common slot vector is guaranteed,
		%% and so that vars in cs vector are older than vars in
		%% object slots, so when they are bound together, the
		%% vars in the object slots will point to the vars in
		%% the cs vector.
	((Module:clause(common_slot(ObjectClass, CV),_)) ->
		findall(CV, Module:common_slot(ObjectClass, CV), CommonSlots)
		;
		CommonSlots = []
	),
	setup_common_slots(CommonSlots, ObjectClass, Module),

	(dmember(values=LocalValues, SpecList) ->
		true;
		LocalValues = []
	),
	Module:make_pred(ObjectClass,Object,MakeCall),
	Module:call(MakeCall),
	mangle(2, Object, Module),
	insert_common_slots(CommonSlots, Object, ObjectClass, Module),

	Module:setObjStruct(myClassPred, Object, ObjectClass),
		%% get any class slot initialization values:
	(bagof(Slot=Value, Module:slot_default(ObjectClass,Slot,Value), ClassValues), !;
		ClassValues = []),
	
		%% merge them:
	merge_values(ClassValues, LocalValues, Values),
	object_values_init(Values, ObjInitVals),

	class_constr_init(ObjectClass,Module,ClassInitVals),
	class_default_init(ObjectClass,Module,ClassDefaultVals),
	merge_init_vals(ClassDefaultVals, ObjInitVals, InterInitVals),
	merge_init_vals(InterInitVals, ClassInitVals, InitVals),

	set_all_slots(InitVals, Object),

	(dmember( handle=true, SpecList) ->
		object_handle(Object, Handle),
		set_object_handle(Handle, Object)
		;
		true
	),
	(dmember( name=GlobalHandle, SpecList) ->
		set_object_handle(GlobalHandle, Object)
		;
		true
	).


setup_common_slots([], _, _) :-!.
setup_common_slots(CommonSlots, ObjectClass, Module)
	:-
	Module:clause(common_slot_gv(ObjectClass, CS_GV),_),
	!,
	common_slot_gv_setup(ObjectClass, CS_GV, CommonSlots).
setup_common_slots(CommonSlots, ObjectClass, Module)
	:-
	gv_alloc(CS_GV),
	Module:assert( common_slot_gv(ObjectClass, CS_GV) ),
	common_slot_gv_setup(ObjectClass, CS_GV, CommonSlots).

common_slot_gv_setup(ObjectClass, CS_GV, CommonSlots)
	:-
	gv_get(CS_GV, GVVal),
	fin_common_slot_gv_setup(GVVal, ObjectClass, CS_GV, CommonSlots).

fin_common_slot_gv_setup(GVVal, ObjectClass, CS_GV, CommonSlots)
	:-
	integer(GVVal),
	!,
	length(CommonSlots, N),
	functor(NewGVVal, cs, N),
	gv_set(CS_GV, NewGVVal).

fin_common_slot_gv_setup(GVVal, ObjectClass, CS_GV, CommonSlots)
	:-
	functor(GVVal, cs, N),
	length(CommonSlots, N).

insert_common_slots([], _, _, _) :-!.
insert_common_slots(CommonSlots, Object, ObjectClass, Module)
	:-
	Module:common_slot_gv(ObjectClass, CS_GV),
	gv_get(CS_GV, CSVector),
	fin_ins_cs(CommonSlots, 1, Object, CSVector, ObjectClass, Module).

fin_ins_cs([], _, _, _, _, _).
fin_ins_cs([SlotName | CommonSlots], CSNum, Object, CSVector, ObjectClass, Module)
	:-
	Module:slot_num(ObjectClass, SlotName, Offset),
	arg(Offset, Object, ObjSlotVar),
	arg(CSNum, CSVector, CSVar),
	ObjSlotVar = CSVar,
	NxtCSNum is CSNum + 1,
	fin_ins_cs(CommonSlots, NxtCSNum, Object, CSVector, ObjectClass, Module).



set_all_slots([], Object).
set_all_slots([s(Slot,Val) | InitVals], Object)
	:-
	setObjStruct(Slot, Object, Val),
	set_all_slots(InitVals, Object).

merge_values([], LocalValues, LocalValues).
merge_values([Slot=Value | ClassValues], LocalValues, Values)
	:-
	dmember(Slot=_, LocalValues),
	!,
	merge_values(ClassValues, LocalValues, Values).
merge_values([Slot=Value | ClassValues], LocalValues, [Slot=Value | Values])
	:-
	merge_values(ClassValues, LocalValues, Values).

class_constr_init(ObjectClass,Module,ClassInitVals)
	:-
	findall(s(Slot,Constant),
			  Module:clause(slot_constraint(ObjectClass,Slot,Value,Value=Constant),true),
			  ClassInitVals).

class_default_init(ObjectClass,Module,ClassDefaultVals)
	:-
	findall(s(Slot,Value),
			Module:clause(slot_default(ObjectClass,Slot,Value),true),
			ClassDefaultVals).

object_values_init(Values, InitVals)
	:-
	(findall(s(Slot,Constant),
			member(Slot=Constant,Values),
			InitVals), !; InitVals = []).

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


endmod.
