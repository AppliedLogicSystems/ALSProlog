/* -------------------------------------------------------- *
 |		oopex_vehicles.pro
 |
 |   Example from Sections 6.3-6/5 of
 |   https://github.com/AppliedLogicSystems/ALSProlog/wiki/6-ObjectPro%3A-Object-Oriented-Programming
 |
 |   defineClass calls for the example are run automatically
 |   when this file is loaded.
 |   
 |   create_object calls are made in the example code (run_vehicles)
 |   to build the objects which are passed around in the code.
 |
 |   The sample driver code supports a non-standard message 
 |   command 'full_dump' which causes the states of all 
 |   vehicles to the printed on the terminal.
 * -------------------------------------------------------- */

:- defineClass([name=vehicle, subClassOf=genericObjects,
			addl_slots=[locomotionType, powerSource] ]).

:- defineClass([name=wheeledVehicle, subClassOf=vehicle,
			addl_slots=[numWheels] ]).

:- defineClass([name=automobile, subClassOf=wheeledVehicle,
			addl_slots=[engine,autoClass,manufacturer] ]).

:- defineClass([name=engine, subClassOf=genericObjects,
			addl_slots=[powerType,fuel,engineClass,cur_rpm,running,temp],
			constrs=[engineClass < [internalCombustion,steam,electric]]     ]).

:- defineClass([name=iC_Engine, subClassOf=engine,
			addl_slots=[manuf],
			constrs = [engineClass = internalCombustion]      ]).

engineAction(start,State) 
	:-
	State^running := yes.

engineAction(stop, State) 
	:-
	State^running := no.

automobileAction(start,State)
	:-
	send(State^engine,start).

automobileAction(stop,State)
	:-
	send(State^engine,stop).

automobileAction(status(Status),State)
	:-
	send(State^engine,get_value(running,EngineStatus)),
	(EngineStatus = yes ->
		Status = running
		;
		Status = off
	).

automobileAction(dump_state, State)
	:-
	full_dump(State).

    /* ---------------------
       Command format:
           Message > AutoNum.
       
       Messages:
	quit
	start
	stop
	status(Status)
       --------------------- */
  
run_vehicles
     :-
	set_prolog_flag(unknown, fail),
		/*--------------------------------------------------
		  If the 
		     setObjStruct(engineClass, Engine1, steam)
		  call is uncommented below, the constraint
			[engineClass = internalCombustion]     
		  for iC_Engine will be violated, and the ObjectPro
		  mechanism will throw an exception of the form
			constraint_error(M)
		  So we set a catch here to catch that exception if
		  it is thrown, and at least report it, and perhaps
		  perform some sort of recovery.
		 *--------------------------------------------------*/
	catch(run_vehicles0, constraint_error(M), write(M)).

run_vehicles0
	:-
	create_object([instanceOf=iC_Engine ], Engine1),

	    %% This will fail the engineClass=internalCombustion constraint:
%	setObjStruct(engineClass, Engine1, steam),

	create_object([instanceOf=automobile,
			values=[engine=Engine1] ], Auto1),
	setObjStruct(autoClass, Auto1, suv),

	create_object([instanceOf=engine ], Engine2),
	setObjStruct(engineClass, Engine2, electric),

	create_object([instanceOf=automobile,
			values=[engine=Engine2] ], Auto2),
	setObjStruct(autoClass, Auto2, sedan),
	setObjStruct(manufacturer, Auto2, tesla),

	Obj =.. [autos, Auto1, Auto2],
	run_vehicles(Obj).

run_vehicles(Autos)
	:-
	printf('::>', []), flush_output,
	read(Cmd),
	disp_run_vehicles(Cmd, Autos).

disp_run_vehicles(quit, Autos) :-!.

disp_run_vehicles(full_dump, Autos) 
	:-!,
	Autos =.. [ _ | AutosList],
	dump_autos(AutosList, 1),
	nl,
	run_vehicles(Autos).

disp_run_vehicles(Cmd, Autos)
	:-
	exec_vehicles_cmd(Cmd, Autos),
	run_vehicles(Autos).

exec_vehicles_cmd(Msg > N, Autos)
	:-
	arg(N, Autos, AN),
	send(AN, Msg),
	!,
	printf('%t-|| %t\n', [N,Msg]),
	flush_output.

exec_vehicles_cmd(Cmd, Autos)
	:-
	printf('Can\'t understand: %t\n', [Cmd]).

dump_autos([], _).
dump_autos([Auto | AutosList], N)
	:-
	printf('Auto #%t:\n', [N]),
	dump(Auto),
	nl,
	M is N+1,
	dump_autos(AutosList, M).

dump(Auto)
	:-
	send(Auto, dump_state).

	%% Simple dump of the object state;
	%% Assumes that generated assertions 
	%% (e.g., subClassOf/2, slots_for/2, etc)
	%% are present in this module (normally so):
full_dump(State)
	:-
	arg(1, State, LocalClass),
	parentsFor(LocalClass, ParentClassesList),
	ClassesList = [LocalClass | ParentClassesList],
	printf('classesFor=%t\n',[ClassesList]),
	slots_for(LocalClass, SlotsFor),
	printf('slots_for(%t)=%t\n',[LocalClass,SlotsFor]),
	show_slot_values(SlotsFor, LocalClass, State, '').

parentsFor(genericObjects, [])
	:-!.
parentsFor(LocalClass, [SC | ParentClassesList])
	:-
	subClassOf(LocalClass, SC),
	parentsFor(SC, ParentClassesList).

show_slot_values([], _, _, _).
show_slot_values([Slot | Slots], ClassName, Obj, Indent)
	:-
	accessObjStruct(Slot, Obj, Value),
	disp_slot_v(Slot, Value, Indent),
	show_slot_values(Slots, ClassName, Obj, Indent).

disp_slot_v(_, Value, Indent)
	:-
	not(atomic(Value)),
	functor(Value, F, _),
	subClassOf(F,_),
	!,
	slots_for(F, SlotsFor),
	catenate(Indent,' |  ', NextIndent),
	show_slot_values(SlotsFor, F, Value, NextIndent).
disp_slot_v(SlotName, Value, Indent)
	:-
	printf('%t%t=%t\n',[Indent,SlotName,Value]).
	
