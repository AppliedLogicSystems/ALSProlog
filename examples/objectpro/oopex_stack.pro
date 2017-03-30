/* -------------------------------------------------------- *
 |		oopex_stack.pro
 |
 |   Simple implementation of a stack using ObjectPro
 |
 |   defineClass for the example is run automatically
 |   when this file is loaded.
 |   
 |   create_object is called in the example code (run_stack)
 |   to build the object which is passed around in the code.
 * -------------------------------------------------------- */

:- defineClass([name=stacker,
			 subClassOf=genericObjects,
			 addl_slots=[theStack, depth] 
			]).

defineObject([name=stack, 
			  instanceOf=stacker,
			  values=[theStack=[], depth=0] 
			 ]).


stackerAction(push(Item),State)
	:-
	accessObjStruct(theStack, State, CurStack),
	setObjStruct(theStack, State, [Item | CurStack]),
	accessObjStruct(depth, State, CurDepth),
	NewDepth is CurDepth + 1,
	setObjStruct(depth, State, NewDepth).

stackerAction(pop(Item),State)
	:-
	accessObjStruct(theStack, State, [Item | RestStack]),
	setObjStruct(theStack, State, RestStack),
	accessObjStruct(depth, State, CurDepth),
	NewDepth is CurDepth - 1,
	setObjStruct(depth, State, NewDepth).

stackerAction(cur_stack(Stack),State)
	:-
	accessObjStruct(theStack, State, Stack).

stackerAction(cur_depth(Depth),State)
	:-
	accessObjStruct(depth, State, Depth).


    %% -------------------------------------------------
    %%   Sample driver code using the ObjectPro code.
    %%   create_object is run at sample startup to
    %%   obtain the object, which is then passed
    %%   around in the code.
    %% -------------------------------------------------

run_stack :-
        create_object([name=stack, instanceOf=stacker, values=[theStack=[], depth=0] ], Obj),
        rs(Obj).

    /* --------------------------------
        Messages:
           push(<Thing>)
           pop(<Var>)
           cur_stack(<Var>)
           cur_depth(<Var>)
           quit
       -------------------------------- */

rs(Obj) :-
        write('4stack:>'),
	flush_output,
	read(Msg),
        rs(Msg, Obj).

rs(quit, _).

rs(M, Obj)
        :-
        send(Obj, M),
        !,
        printf('Msg=%t\n', [M]),
        flush_output,
        rs(Obj).

rs(M, Obj)
        :-
        printf('Error interpreting message: %t\n', [M]),
        flush_output,
        rs(Obj).

