/*==========================================================================*
 |                  pert2
 |      Copyright (c) 1994 Applied Logic Systems, Inc.
 |
 |  Critical path scheduling carried out using functional arithmetic and
 |  general constraints (freeze/delay) -- Database oriented.
 |
 | This is a variant of file pert, the pert/cpm problem from 
 | BNRP User Guide (W. Older) 
 |
 | Author: Ken Bowen
 *==========================================================================*/

:- op(700,xfx, preceeds).

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %% Sample Project p1:
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
activity(act_a, p1, 10).
activity(act_b, p1, 20).
activity(act_c, p1, 30).
activity(act_d, p1, 18).
activity(act_e, p1,  8).
activity(act_f, p1,  3).
activity(act_g, p1,  4).

act_a preceeds act_d.
act_b preceeds act_d.
act_b preceeds act_e.
act_c preceeds act_e.
act_d preceeds act_f.
act_e preceeds act_g.
act_f preceeds act_g.

/*  Sample Queries for project p1:

?- project(p1, Schedule).
?- good_project(p1, Schedule).

*/
proj(ProjName)
	:-
	project(ProjName, Schedule),
	display_sched(Schedule).

good_project(ProjName)
	:-
	good_project(ProjName, Schedule),
	display_sched(Schedule).

    %% Top level entry to scheduler:
project(ProjName, Schedule)
    :-
    findall(act(ActName,Duration), activity(ActName,ProjName,Duration), Activities),
    schedule(Activities, Schedule).

schedule(Activities, Schedule)
    :-
    Start =  task(start,0,0),
    Finish = task(finish,FinishStart,FinishEnd),
    schedule(Activities, Start, Finish, [Start], Schedule).

good_project(ProjName, Schedule)
    :-
    project(ProjName, Schedule), 
    last(Schedule,Last), 
    Last = task(finish,FinSt,FinEnd), 
    {FinEnd == FinSt + 0}, 
	FinEnd::real(FELower, FEU), 
	{FinEnd =< FELower}.

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Schedule each activity:
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
schedule([], Start, Finish, Accum, Schedule)
    :-
    reverse([Finish | Accum], Schedule).

schedule([act(ActName,Dur) | Activities], Start, Finish, Accum, Schedule)
    :-
    {ActFinish == ActStart + Dur},
    init_or_end(ActName, ActStart, ActFinish, Start, Finish),
    precedence(Accum, ActName, ActStart, ActFinish),
    schedule(Activities, Start, Finish, [task(ActName,ActStart,ActFinish) | Accum], Schedule).

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Handle start or end activities:
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
init_or_end(ActName, ActStart, ActFinish, Start, Finish)
    :-
    init_act(ActName, ActStart, ActFinish, Start),
    end_act(ActName, ActStart, ActFinish, Finish).
    
init_act(ActName, ActStart, ActFinish, _)
    :-
    SomeAct preceeds ActName,
    !.

init_act(ActName, ActStart, ActFinish, task(start,StartStart,StartFinish))
    :-
    { StartFinish =< ActStart }.

end_act(ActName, ActStart, ActFinish, Finish)
    :-
    ActName preceeds SomeAct,
    !.

end_act(ActName, ActStart, ActFinish, task(finish,FinishStart,FinishEnd))
    :-
    { ActFinish =< FinishStart }.

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Handle precedence relations between the activity being
    %% added and all of the previously added activities:
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
precedence([], ActName, ActStart, ActFinish).
precedence([task(OA, OS, OF) | Accum], ActName, ActStart, ActFinish)
    :-
    do_prec(OA,OS,OF,ActName,ActStart,ActFinish),
    precedence(Accum, ActName, ActStart, ActFinish).

do_prec(OA,OS,OF,ActName,ActStart,ActFinish)
    :-
    OA preceeds ActName,
    !,
    { OF =< ActStart}.

do_prec(OA,OS,OF,ActName,ActStart,ActFinish)
    :-
    ActName preceeds OA,
    !,
    {ActFinish =< OS}.

do_prec(OA,OS,OF,ActName,ActStart,ActFinish).
    
    %%%%%%%%%%%%%%%%
    %% Utilities:
    %%%%%%%%%%%%%%%%
reverse(In, Out)
    :-
    reverse(In, [], Out).

reverse([], Accum, Accum).
reverse([Top | Rest], Accum, Out)
    :-
    reverse(Rest, [Top | Accum], Out).

last([task(finish,A,B) | _], task(finish,A,B)) :-!.
last([_ | Schedule], T)
    :-
    last(Schedule, T).

earliest( task(_,_,F))
    :- 
    lower_bound(F).


/*  Sample Queries:

?- activity(finish,0,E),project(task(start,0,0),E,L).
    _E = task(finish, [45.000, 3.4000e+38], [45.000, 3.4000e+38]),
    _L = [task(a, [0.0000, 3.4000e+38], [10.000, 3.4000e+38]),
          task(b, [0.0000, 3.4000e+38], [20.000, 3.4000e+38]),
          task(c, [0.0000, 3.4000e+38], [30.000, 3.4000e+38]),
          task(d, [20.000, 3.4000e+38], [38.000, 3.4000e+38]),
          task(e, [30.000, 3.4000e+38], [38.000, 3.4000e+38]),
          task(f, [38.000, 3.4000e+38], [41.000, 3.4000e+38]),
          task(g, [41.000, 3.4000e+38], [45.000, 3.4000e+38])]

?- activity(finish,0,E),project(task(start,0,0),E,L),earliest(E).
    _L = [task(a, [0.0000, 10.000], [10.000, 20.000]),
          task(b, [0.0000, 0.0000], [20.000, 20.000]),
          task(c, [0.0000, 3.0000], [30.000, 33.000]),
          task(d, [20.000, 20.000], [38.000, 38.000]),
          task(e, [30.000, 33.000], [38.000, 41.000]),
          task(f, [38.000, 38.000], [41.000, 41.000]),
          task(g, [41.000, 41.000], [45.000, 45.000])],
    _E = task(finish, [45.000, 45.000], [45.000, 45.000])
*/



display_sched([]).
display_sched([Task | Schedule])
	:-
	display_task(Task),
	display_sched(Schedule).

display_task(task(TaskName,Start,Finish))
	:-
	printf('%t:\t',[TaskName]),
	disp_num_int(Start),
	disp_num_int(Finish),
	nl.

disp_num_int(Val)
	:-
	number(Val),
	!,
	printf('%t\t\t',[Val]).
disp_num_int(Val)
	:-
	Val::real(V0,V1),
	printf('[%t,%t]\t',[V0,V1]).
