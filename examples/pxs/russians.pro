/*--------------------------------------------------------------------------
 |	russians.pro
 |	Copyright (c) 1989-90  Applied Logic Systems, Inc.
 |
 |	Author: Kenneth A. Bowen
 |
 |	Solution to a state space search problem
 --------------------------------------------------------------------------*/

/* Farmer crosses with goat */
transition(state(F0,F0,C,W), state(F1,F1,C,W) )
	:-
	opposite(F0,  F1).

/* Farmer crosses with cabbage */
transition(state(F0,G,F0,W), state(F1,G,F1,W) )
	:-
	opposite(F0,  F1),
	opposite(G, W).

/* Farmer crosses with wolf */
transition(state(F0,G,C,F0), state(F1,G,C,F1) )
	:-
	opposite(F0,  F1),
	opposite(G, C).

/* Farmer crosses alone */
transition(state(F0,G,C,W), state(F1,G,C,W) )
	:-
	opposite(F0,  F1),
	opposite(G, C),
	opposite(G, W).

opposite(n, s).
opposite(s, n).

path(A, B) :- transition(A, B).

path(A, B) :-
	transition(A, C), drawState(C), pause,path(C, B).

