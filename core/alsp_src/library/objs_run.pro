/*==========================================================================*
 | 			objs_run.pro
 |	Copyright (c) 1990-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		Object-oriented programming in Prolog
 |		Run-time support for ObjectPro, not including run-time
 |		definition of objects & classes on the fly;
 |
 |	Author:	Ken Bowen
 |	Date:	August 1990 --
 *==========================================================================*/

module objects.

%% Declaration is now in sio_rt.pro:
%% :-op(600,xfy,':=').

export send/2.
export send/3.
export send_self/2.
export toggle_null_send_notify/0.
export send_all/2.
export send_each/2.
export queue_oop_event/1.
export insert_oop_event_request/2.
export inherit/3.
export init_nil/2.
export accessObjStruct/3.
export setObjStruct/3.
export (':=')/2.
export set_slots0/4.
export deref_val/3.
export standardObject/2.
export genericObjects/2.
export clone/2.

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
send('$no$Object$', Message)
	:-!,
	send(builtins,null_object,Message).

send(builtins, Message)
	:-!,
	send(builtins,null_object,Message).

send(anyone, Message)
	:-!,
	queue_oop_event(Message).

send(any_object, Message)
	:-!,
	queue_oop_event(Message).

send(Object, Message)
	:-
	atom(Object),
	!,
	objects:objectModule(Object, Module),
	send(Module, Object, Message).

send(Object^SlotDescrip, Message)
	:-!,
	(atom(Object) ->
		send(Object,your_state(ObjectState));
		ObjectState = Object
	),
	accessObjStruct(SlotDescrip, ObjectState, SubObject),
	!,
	send(SubObject, Message).

send(Object, Message)
	:-
	accessObjStruct(myName, Object, ObjectName),
	!,
	send(ObjectName, Message).

/*!-----------------------------------------------------------------------
 |	send/3
 |	send(Module, Object, Message)
 |	send(+, +)
 |
 |	- sends a message to an object
 *-----------------------------------------------------------------------*/
send(Module, null_object, Message)
	:-!.

send(Module, debugger_object, Message)
	:-!,
	debugger:debugger_object(Message).

send(Module, debug_4_port_object, Message)
	:-!,
	debugger:debug_4_port_object(Message).

send(Module, source_trace_object, Message)
	:-!,
	debugger:source_trace_object(Message).

send(Module, debug_controls_object, Message)
	:-!,
	debugger:debug_controls_object(Message).

send(Module, debug_graph_object, Message)
	:-!,
	debugger:debug_graph_object(Message).

send(Module, Object, Message)
	:-
	atom(Object),!,
	TheCall =.. [Object , Message],
	objects:do_object_goal(Module,TheCall),
	!.

/*!-----------------------------------------------------------------------
 |	send_self/2
 |	send_self(Message, ObjState)
 |	send_self(+, +)
 |
 |	- object sends a message to self
 *-----------------------------------------------------------------------*/
send_self(Message,ObjState)
	:-
	accessObjStruct(myName, ObjState, ObjectName),
	finish_send_self(ObjectName,Message,ObjState).

finish_send_self(debugger_object,Message,ObjState)
	:-!,
	debugger:debugger_object(Message).

finish_send_self(debug_4_port_object,Message,ObjState)
	:-!,
	debugger:debug_4_port_object(Message).


finish_send_self(source_trace_object, Message, ObjState)
	:-!,
	debugger:source_trace_object(Message).

finish_send_self(debug_controls_object, Message, ObjState)
	:-!,
	debugger:debug_controls_object(Message).

finish_send_self(debug_graph_object, Message, ObjState)
	:-!,
	debugger:debug_graph_object(Message).

finish_send_self(ObjectName,Message,ObjState)
	:-
	Call =.. [ObjectName, Message],
	objects:objectModule(ObjectName, Module),
	objects:do_object_goal(Module,Call),
	!.

/*!-----------------------------------------------------------------------
 |	toggle_null_send_notify/0
 |	toggle_null_send_notify
 |	toggle_null_send_notify
 |
 |	- toggles (on/off) notifications of sends to the null object
 *-----------------------------------------------------------------------*/
:- dynamic(notify_null_send_flag/0).

toggle_null_send_notify
	:-
	notify_null_send_flag,!,
	abolish(notify_null_send_flag,0).
toggle_null_send_notify
	:-
	assert(notify_null_send_flag).

/*!-----------------------------------------------------------------------
 |	send_all/2
 |	send_all(List, Msg)
 |	send_all(+, +)
 |	
 |	- try to send Msg to all objects on List; fail if any fails
 *-----------------------------------------------------------------------*/
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
send_each([], _).
send_each([Kid | Children], Msg)
    :-
    (send(Kid, Msg) -> true ; true),
    send_each(Children, Msg).


			/*=================================
			 |         EVENT QUEUING
			 *================================*/
			/*-- A naive implementation --*/

:-make_gv('_OOP_EventQueue').

/*!-------------------------------------------------------
 |	queue_oop_event/1.
 |	queue_oop_event(Message)
 |	queue_oop_event(+)
 |
 |	- broadcasts an oop event on the appropriate queues
 |
 |	This predicate causes the oop Message to be placed on all 
 |	oop event queues which it matches, with the effect that 
 |	Message is sent (via send/2) to all objects for which an 
 |	expression of interest has been made (via insert_oop_request/2).
 |	Note that, for convenience, both of the following are
 |	equivalent to queue_oop_event(Message):
 |		send(anyone, Message)
 |		send(any_object, Message)
 *!------------------------------------------------------*/
queue_oop_event(Message)
	:-
	get_OOP_EventQueue(QueueList),
	bagof(SubQueue,
			find_matching_subqueue(QueueList, Message, SubQueue),
			SubQs),
	!,
	send_to_each_q(SubQs, Message).
queue_oop_event(_).

send_to_each_q([], Message).
send_to_each_q([SubQueue | SubQs], Message)
	:-
	send_each(SubQueue, Message),
	send_to_each_q(SubQs, Message).

/*!-------------------------------------------------------
 |	insert_oop_event_request/2
 |	insert_oop_event_request(EventForm, Object)
 |	insert_oop_event_request(+, +)
 |
 |	- Inserts a request for messages of form EventForm
 |
 |	This predicate causes a request to be entered from Object 
 |	to be send all broadcasts of oop message events of the form 
 |	EventForm.  Note that this does not cause all messages of the 
 |	form EventForm to be direct ed to Object, only those which 
 |	are broadcast (using queue_oop_event/1).  Also note that since 
 |	this is a Prolog predicate, it can be invoked either from 
 |	within an object method, or can be invoked directly in Prolog code.
 *!------------------------------------------------------*/
insert_oop_event_request(Event, Object)
	:-
	get_OOP_EventQueue(QueueList),
	bagof(SubQueue,
			find_matching_subqueue(QueueList, Event, SubQueue),
			SubQs),
	!,
		%% Matching subqueues exist:
	insert_in_each_q(SubQs, Object).

insert_oop_event_request(Event, Object)
	:-
		%% No matching subqueues exist:
	get_OOP_EventQueue(QueueList),
	copy_term(Event,EventCopy),
	(QueueList = 0 ->
		set_OOP_EventQueue([ [EventCopy, Object] ])
		;
		set_OOP_EventQueue([ [EventCopy, Object] |  QueueList])
	).

insert_in_each_q([], Object).
insert_in_each_q([SubQueue | SubQs], Object)
	:-
		%% must be at least one item on the queue if the call succeeded:
		%% {the head item is the pattern):
	SubQueue = [Pattern | Rest],
	mangle(2, SubQueue, [Object | Rest]).
	insert_in_each_q(SubQs, Object).

	%% This could be done with one-sided unification, or
	%% etc....

find_matching_subqueue([[E | SubQueue] | QueueList], Event, SubQueue)
	:-
	not(not(E=Event)),
	!.

find_matching_subqueue([_ | QueueList], Event, SubQueue)
	:-
	find_matching_subqueue(QueueList, Event, SubQueue).

%% Need a delete request call

			/*=================================
			 |         INHERITANCE
			 *================================*/

/*!-------------------------------------------------------
 |	inherit/3
 |	inherit(Message,SuperClass,ObjState)
 |	inherit(+, +, +)
 |
 |	- ObjState reacts to Message using method from SuperClass
 *!------------------------------------------------------*/
inherit(Message,SuperClass,ObjState)
	:-
	classModule(SuperClass, Module),
	TheCall =.. [SuperClass,Message,ObjState],
	objects:do_object_goal(Module,TheCall),
	!.
			/*=================================
			 |        BASIC
			 *================================*/
	/*---------------------------------------*
	 |	Essentially identical to 
	 |	do_shell_query2 in blt_shl.pro; needs
	 |	to be here in this form for applications
	 |	packaged without the shell.
	 *---------------------------------------*/
do_object_goal(Mod,Goal)
	:-
	getPrologInterrupt(Int),
	(Int=debug_user ; Int=debug_init),
	!,
	dbg_spyon,
	callWithDelayedInterrupt(Mod,Mod:Goal).

do_object_goal(Mod,Goal)
	:-
	getPrologInterrupt(Int),
	(Int=spying ; Int=jumping),
	!,
	dbg_spyon,
	Mod:Goal.

do_object_goal(Mod,Goal)
	:-
	Mod:Goal.

			/*=================================
			 |        UTILITIES
			 *================================*/
/*!-------------------------------------------------------
 |	init_nil/2
 |	init_nil(Num, Structure)
 |	init_nil(+, +)
 |
 | - init args from 1..Num of Structure to nil
 *!------------------------------------------------------*/
init_nil(1,Structure)
	:-
	arg(1,Structure,nil).
init_nil(Num,Structure)
	:-
	arg(Num,Structure,nil),
	NewNum is Num - 1,
	init_nil(NewNum,Structure).

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
accessObjStruct((SlotName1^SlotName2), Struct, Value)
	:-!,
	functor(Struct, Class, _),
	slot_num(Class, SlotName1, Offset),
	arg(Offset, Struct, Value1),
	accessObjStruct(SlotName2, Value1, Value).
accessObjStruct(SlotName, Struct, Value)
	:-
	functor(Struct, Class, _),
	slot_num(Class, SlotName, Offset),
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
:-dynamic(slot_constraint/4).

setObjStruct((SlotName1^SlotName2), Struct, Value)
	:-!,
	functor(Struct, Class, _),
	slot_num(Class, SlotName, Offset),
	arg(Offset, Struct, Value1),
	setObjStruct(SlotName2, Value1, Value).
setObjStruct(SlotName, Struct, Value)
	:-
	functor(Struct, Class, _),
	satisfy_slot_constrs(Class,SlotName,Value),
	slot_num(Class, SlotName, Offset),
	mangle(Offset, Struct, Value).

satisfy_slot_constrs(Class,SlotName,Value)
	:-
	slot_constraint(Class,SlotName,Value,Call),
	!,
	call(Call).
satisfy_slot_constrs(_,_,_).


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

/*!-------------------------------------------------------
 |	set_slots0/4
 |	set_slots0(Vals,Code,State,Mod)
 |	set_slots0(+,-,+,+)
 |
 | - create code for setting slots to values in object State
 |
 |	Vals is a list of expressions of the form s(Slot,Val);
 |	Code will be a prolog term representing code which,
 |	when executed, will set each of the Slots of object 
 |	State to the corresponding Val, in module Mod.
 *!------------------------------------------------------*/
set_slots0([],true,_,_).
set_slots0([s(Slot,Val)],Code,State,Mod)
	:-!,
	Code =  (deref_val(Val, RealVal,Mod),
				setObjStruct(Slot, State, RealVal) ).

set_slots0([s(Slot,Val) | RestVals], (ThisCode, RestCode),State,Mod)
	:-
	ThisCode = ( deref_val(Val, RealVal,Mod),
					setObjStruct(Slot, State, RealVal) ),
	set_slots0(RestVals,RestCode,State,Mod).

/*!-------------------------------------------------------
 |	deref_val/3
 |	deref_val(OExpr, State, Mod)
 |	deref_val(+, -, +)
 |
 | - 'dereference' an object description
 |
 |	If OExpr is of the form '$object'(OName), and
 |	if OName is the name of defined object, then
 |	if the state of OName has been initialized, returns
 |	that state with State, and otherwise fails;
 |
 |	If OExpr is of the form defineObject(SpecList),
 |	invokes object definition processing on SpecList,
 |	then sends the resulting defined object an initialization
 |	message, and unifies the resulting state with State, if succssful,
 |	and fails otherwise;
 |
 |	In all other cases, unifies State with OExpr.
 *!------------------------------------------------------*/
deref_val('$object'(ObjectName), ObjectState,Mod)
	:-!,
	catenate('get_',ObjectName, GetPred),
	GetCall0 =.. [GetPred, ObjectState0],
	Mod:call(GetCall0),
	(ObjectState0 \= 0 ->
		ObjectState=ObjectState0;
		send(ObjectName, initialize),
		GetCall =.. [GetPred, ObjectState],
		Mod:call(GetCall)
	).
deref_val(defineObject(SpecList), ObjectState,Mod)
	:-!,
	Mod:defineObject(SpecList),
	dmember(name=ObjectName,SpecList),
	send(ObjectName,initialize),
	catenate('get_',ObjectName, GetPred),
	GetCall =.. [GetPred, ObjectState],
	Mod:call(GetCall).
deref_val(Val, Val,_).

/*!-------------------------------------------------------
 |	standardObject/2
 |	standardObject(OName, Class)
 |	standardObject(+, +)
 |
 | - define an object, named OName, as an instance of Class
 *!------------------------------------------------------*/
standardObject(ObjectName,ClassName)
	:-
	classModule(ClassName, Module),
	Module:defineObject(name=ObjectName,
						action=ObjectName,
						instanceOf=[ClassName],
						options=[]).


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
 |					addl_slots=[myName],
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

classModule(genericObjects,objects).
make_pred(genericObjects,'makegenericObjectsStruct').
slot_num(genericObjects,myName,1).
slot_num(genericObjects,myDBRefs,2).
makegenericObjectsStruct(_1554) 
	:-
    functor(_1554,genericObjects,2), 
	init_nil(1,_1554),
	init_nil(2,_1554).
slots_for(genericObjects,[myName,myDBRefs]).
genericObjects(_1595,_1593) 
	:- 
	genObjs(_1595,_1593).

/*----*
		%% necessary for testing when modifying defineClass:
		%% commented out when defineClass works:
slots_for(genericObjects,[myName,myDBRefs]).
classModule(genericObjects,objects).
 *----*/

genObjs(get_value(Slot,Value),State)
	:-
	accessObjStruct(Slot,State,Value).

genObjs(set_value(Slot,Value),State)
	:-
	setObjStruct(Slot,State,Value).

genObjs(set_value_copy(Slot,Value),State)
	:-
	copy_term(Value, CopyValue),
	setObjStruct(Slot,State,CopyValue).

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

genObjs(your_state(State),State).

genObjs(insert_oop_event_request(Event),State)
	:-
	accessObjStruct(myName,State,Object),
	insert_oop_event_request(Event,Object).

genObjs(insert_oop_event_request(Event,Object),State)
	:-
	insert_oop_event_request(Event,Object).

genObjs(queue_oop_event(Event),State)
	:-
	queue_oop_event(Event).

genObjs(remove_self, State)
	:-
	genObjs(hari_kari, State).
genObjs(hari_kari, State)
	:-
	accessObjStruct(myName,State,ObjName),
	accessObjStruct(myDBRefs,State,ObjDBRefs),
	catenate('get_',ObjName,GVAccessPred),
	functor(GetCall, GVAccessPred, 1),
	catenate('set_',ObjName,GVSetPred),

	objects:retract(objectModule(ObjName,ObjModule)),
	objects:retract(objectClass(ObjName,ObjClass)),

	ObjModule:clause(GetCall,gv_get(GVNum,_)),
	ObjModule:abolish(GVAccessPred,1),
	ObjModule:abolish(GVSetPred,1),
	gv_free(GVNum),

	dispose_obj_code(ObjDBRefs, ObjName, ObjModule).

dispose_obj_code(ObjDBRefs, ObjName, ObjModule)
	:-
	ObjDBRefs = [_|_],
	!,
	erase_all(ObjDBRefs).
dispose_obj_code(ObjDBRefs, ObjName, ObjModule)
	:-
	ObjModule:abolish(ObjName,1),
	catenate(dispatch_,ObjName,DispPred),
	ObjModule:abolish(DispPred,2).
		
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

endmod.

