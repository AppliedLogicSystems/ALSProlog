/*==========================================================================*
 *==========================================================================*
 *   			objs_run.pro
 *
 *		Copyright (c) 1990-91 Applied Logic Systems, Inc.
 *
 *		Object-oriented programming in Prolog
 *		Run-time support for ObjectPro, not including run-time
 *		definition of objects & classes on the fly;
 *
 *	Author:	Ken Bowen
 *	Date:	August 1990 --
 *==========================================================================*
 *==========================================================================*/

module objects.

%% Declaration is in blt_lib.pro
%%:-op(600,xfy,':=').

			/*=================================
			 |   MESSAGE SENDING
			 *================================*/
/*!-----------------------------------------------------------------------
 |	send/2
 |	send(Object, Message)
 |	send(+, +)
 |
 |	- sends a message to an object
 |
 |
 *-----------------------------------------------------------------------*/
%%export send/2.
send('$no$Object$', Message)
	:-!,
	send_to_null_object(Message).

send(Object, Message)
	:-
	atom(Object),
	!,
	objects:objectModule(Object, Module),
	!,
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
/*
	% Module:TheCall,
	 'als$cd'(0,'$colon'(Module,TheCall,CutPt),CutPt),
*/
	builtins:do_shell_query2(Module,TheCall),
	!.

/*!-----------------------------------------------------------------------
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
	builtins:do_shell_query2(Module,Call),
	!.
/*
	% Module:Call,
	'als$cd'(0,'$colon'(Module,Call,CutPt),CutPt),
	!.
*/

/*!-----------------------------------------------------------------------
 *-----------------------------------------------------------------------*/
:-dynamic(notify_null_send_flag/0).

send_to_null_object(Message)
	:-
	notify_null_send_flag,!,
	notify_null_send(Message).

send_to_null_object(Message).

notify_null_send(Message)
	:-
	printf(warning_output,"Send to nullObject: %t\n",[Message]),
	flush_output(warning_output).

/*!-----------------------------------------------------------------------
 *-----------------------------------------------------------------------*/
export toggle_null_send_notify/0.
toggle_null_send_notify
	:-
	notify_null_send_flag,!,
	abolish(notify_null_send_flag,0).
toggle_null_send_notify
	:-
	assert(notify_null_send_flag).

/*!-----------------------------------------------------------------------
 *-----------------------------------------------------------------------*/
%% export send_all/2.
send_all([], _).
send_all([Kid | Children], Msg)
    :-
    send(Kid, Msg),
    send_all(Children, Msg).

%% export send_each/2.
send_each([], _).
send_each([Kid | Children], Msg)
    :-
    (send(Kid, Msg) -> true ; true),
    send_each(Children, Msg).


%% export queue_object_call/4.
queue_object_call(Object,Event,Module,Message)
	:-
	TheCall =.. [Object, Message],
	Module:TheCall,
	!.

			/*=================================
			 |         EVENT QUEUING
			 *================================*/
			/*-- A naive implementation --*/

:-make_gv('_OOP_EventQueue').

%% export queue_oop_event/1.
queue_oop_event(Event)
	:-
	get_OOP_EventQueue(QueueList),
	find_matching_subqueue(QueueList, Event, SubQueue),
	!,
	send_each(SubQueue, Event).
queue_oop_event(Event).

%% export insert_oop_event_request/2.
insert_oop_event_request(Event, Object)
	:-
	get_OOP_EventQueue(QueueList),
	find_matching_subqueue(QueueList, Event, SubQueue),
	!,
		%% must be at least one item it call succeeded:
	SubQueue = [First | Rest],
	mangle(2, SubQueue, [Object | Rest]).
insert_oop_event_request(Event, Object)
	:-
	get_OOP_EventQueue(QueueList),
	copy_term(Event,EventCopy),
	(QueueList = 0 ->
		set_OOP_EventQueue([ [EventCopy, Object] ])
		;
		set_OOP_EventQueue([ [EventCopy, Object] |  QueueList])
	).

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

/*!-----------------------------------------------------------------------
 *-----------------------------------------------------------------------*/
%% export inherit/3.
inherit(Message,SuperClass,ObjState)
	:-
	classModule(SuperClass, Module),
	TheCall =.. [SuperClass,Message,ObjState],
/*
	% Module:Call.
	'als$cd'(0,'$colon'(Module,TheCall,CutPt),CutPt).
*/
	builtins:do_shell_query2(Module,TheCall),
	!.

			/*=================================
			 |        UTILITIES
			 *================================*/

%% export init_nil/2.
init_nil(1,Structure)
	:-
	arg(1,Structure,nil).
init_nil(Num,Structure)
	:-
	arg(Num,Structure,nil),
	NewNum is Num - 1,
	init_nil(NewNum,Structure).


/*!-----------------------------------------------------------------------
 *-----------------------------------------------------------------------*/
%% export accessObjStruct/3.
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

/*!-----------------------------------------------------------------------
 *-----------------------------------------------------------------------*/
:-dynamic(slot_constraint/4).

%% export setObjStruct/3.
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


%% export (':=')/2.
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

/*!-----------------------------------------------------------------------
 *-----------------------------------------------------------------------*/
%% export standardObject/2.
standardObject(ObjectName,ClassName)
	:-
	classModule(ClassName, Module),
	Module:defineObject(name=ObjectName,
						action=ObjectName,
						instanceOf=[ClassName],
						options=[]).

/*-----------------------------------------------------------
 |		Top-most generic object class:
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
 |					subclassOf=[],
 |					addl_slots=[myName],
 |					export = yes]).
 |	This is installed in the file objs_generic.oop, and
 |	is processed with objectsProcessFile/1 to yield the
 |	file objs_generic.pro.  Remove the op declaration,
 |	the reconsult of objs_run.pro, and the module/endmod
 |	declarations, and include the result below:
 *-----------------------------------------------------------*/

classModule(genericObjects,objects).
make_pred(genericObjects,'makegenericObjectsStruct').
slot_num(genericObjects,myName,1).
slot_num(genericObjects,myDBRefs,2).
makegenericObjectsStruct(_1554) :-
        functor(_1554,genericObjects,2), 
	init_nil(1,_1554),
	init_nil(2,_1554).
slots_for(genericObjects,[myName,myDBRefs]).

%% export genericObjects/2.
genericObjects(_1595,_1593) :- genObjs(_1595,_1593).

/*
		%% necessary for testing when modifying defineClass:
		%% commented out when defineClass works:
slots_for(genericObjects,[myName,myDBRefs]).
classModule(genericObjects,objects).
*/

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

