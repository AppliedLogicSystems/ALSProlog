/*---------------------------------------------------------------*
 *	russian2.pro
 *	Copyright (c) 1989-90 Applied Logic Systems, Inc.
 *
 *	Code fragements for revision of Russian Farmer puzzle
 *---------------------------------------------------------------*/


path(A, B, FinalPath) 
	:-
	path(A, B, [], ReversedFinalPath),
	reverse(ReversedFinalPath, FinalPath).


path(CurState, GoalState, PrevStates, [GoalState | PrevStates]) 
	:- 
	transition(CurState, GoalState).
path(CurState, GoalState, PrevStates, FinalPath) 
	:-
	transition(CurState, NextState), 
	not( member(NextState, PrevStates) ),
	path( CurState, GoalState, [CurState | PrevStates], FinalPath ).


loc(farmer, north).
loc(goal, south).
loc(cabbage, south).
loc(wolf, north).


holds( loc( Player, RiverBank ),  State)  
	:-  
	member( loc( Player, RiverBank ), State ).

transition(S0, S1 )
	:-
	holds(loc(farmer, F0), S0),
	holds(loc(cabbage, C0), S0),
	C0 = F0,
	opposite(F0,  F1),
	C1 = F1,
	change(loc(farmer, F0), S0, loc(farmer, F1), S_Inter),
	change(loc(cabbage, C0), S_Inter, loc(cabbage, C1), S1).

/*-----------------------------
	Simply by applying equality statements:

transition(S0, S1 )
	:-
	holds(loc(farmer, F0), S0),
	holds(loc(cabbage, F0), S0),
	opposite(F0,  F1),
	delete(S0, loc(farmer, F0), S0_A),  
	add(S0_A,  loc(farmer, F1), S_B),
	delete(S_B, loc(cabbage, F0), S_C), 
	add(S_C,  loc(cabbage, F1), S1).
 *----------------------------------------*/


add(State, Assertion, [Assertion | State] ).

delete( [], Assertion,  []).
delete([Assertion | Remainder],  Assertion,  Remainder)
	:-!.
delete([Item | Remainder ],  Assertion, [Item | ResultRemainder] )
	:-
	delete(Remainder,  Assertion,  ResultRemainder ).

