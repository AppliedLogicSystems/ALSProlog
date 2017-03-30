/* -------------------------------------------------------- *
 |		oopex_vehicles_global.pro
 |
 |   Global variable version of Example oopex_vehicles.pro
 |
 |   defineClass calls for the example are run automatically
 |   when this file is loaded.
 |
 |   The make_gv call for the global variable to hold the object
 |   is run automatically when this file is loaded, and
 |   in the same call, multiple create_objects are run and the 
 |   resulting autos object is stored in the global variable.
 |
 |   Note that if run_vehicles is executed, then quit is
 |   submitted, and then if run_vehicles is executed again in
 |   the same prolog session, the previous state of the
 |   autos object will be remembered.
 * -------------------------------------------------------- */

	%% Note: the defineClass calls  must appear before the make_gv call
	%% below since the latter runs create_object:

:- defineClass([name=vehicle, subClassOf=genericObjects,
			addl_slots=[locomotionType, powerSource] ]).

:- defineClass([name=wheeledVehicle, subClassOf=vehicle,
			addl_slots=[numWheels] ]).

:- defineClass([name=automobile, subClassOf=wheeledVehicle,
			addl_slots=[engine,autoClass,manufacturer] ]).

:- defineClass([name=engine, subClassOf=genericObjects,
			addl_slots=[powerType,fuel,engineClass,cur_rpm,running,temp],
			constrs=[engineClass<[internalCombustion,steam,electric]]     ]).

:- defineClass([name=iC_Engine, subClassOf=engine,
			addl_slots=[manuf],
			constrs = [engineClass = internalCombustion]      ]).

:- make_gv('_example_autos'),
	create_object([instanceOf=iC_Engine ], Engine1),
	create_object([instanceOf=automobile,
			values=[engine=Engine1] ], Auto1),

	create_object([instanceOf=iC_Engine ], Engine2),
	create_object([instanceOf=automobile,
			values=[engine=Engine2] ], Auto2),

	Obj = a(Auto1, Auto2),
        set_example_autos(Obj).

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
	get_example_autos(AutosObj),
	run_vehicles(AutosObj).

run_vehicles(Autos)
	:-
	printf('::>', []), flush_output,
	read(Cmd),
	disp_run_vehicles(Cmd, Autos).

disp_run_vehicles(quit, Autos) :-!.

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


