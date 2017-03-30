/* -------------------------------------------------------- *
 |		oopex_stack_global.pro
 |
 |   Simple implementation of a stack using 
 |       ObjectPro and global variables
 |
 |   defineClass for the example is run automatically
 |   when this file is loaded.
 |   
 |   make_gv call for the global variable to hold the object
 |   is run automatically when this file is loaded, and
 |   in the same call, create_object is run and the resulting
 |   object is stored in the global variable.
 |
 |   Note that if run_stack is executed, then quit is
 |   submitted, and then if run_stack is executed again in
 |   the same prolog session, the previous state of the
 |   stack will be remembered.
 * -------------------------------------------------------- */

	%% Note: this must appear before the make_gv call
	%% below since the latter runs create_object:
:- defineClass([name=stacker,
			 subClassOf=genericObjects,
			 addl_slots=[theStack, depth] 
			]).

:- make_gv('_example_stack'),
        create_object([name=stack, instanceOf=stacker, 
		values=[theStack=[], depth=0] ], Obj),
        set_example_stack(Obj).

stackerAction(push(Item), State)
	:-
	accessObjStruct(theStack, State, CurStack),
	setObjStruct(theStack, State, [Item | CurStack]),
	accessObjStruct(depth, State, CurDepth),
	NewDepth is CurDepth + 1,
	setObjStruct(depth, State, NewDepth).

stackerAction(pop(Item), State)
	:-
	accessObjStruct(theStack, State, [Item | RestStack]),
	setObjStruct(theStack, State, RestStack),
	accessObjStruct(depth, State, CurDepth),
	NewDepth is CurDepth - 1,
	setObjStruct(depth, State, NewDepth).

stackerAction(cur_stack(Stack), State)
	:-
	accessObjStruct(theStack, State, Stack).

stackerAction(cur_depth(Depth), State)
	:-
	accessObjStruct(depth, State, Depth).

    %% -------------------------------------------------
    %%   Sample driver code using the ObjectPro code.
    %%   create_object is run at sample startup to
    %%   obtain the object, which is then passed
    %%   around in the code.
    %% -------------------------------------------------

run_stack :-
        rs.

    /* --------------------------------
        Messages:
           push(<Thing>)
           pop(<Var>)
           cur_stack(<Var>)
           cur_depth(<Var>)
           quit
       -------------------------------- */

rs :-
        write('4stack:>'),
	flush_output,
	read(Msg),
        rs(Msg).

rs(quit).

rs(M) :-
	get_example_stack(Obj),
        send(Obj, M),
        !,
        printf('Msg=%t\n', [M]),
        flush_output,
        rs.

rs(M) :-
        printf('Error interpreting message: %t\n', [M]),
        flush_output,
        rs.

