

/*
 * mission.pro  -- a solution to the missionaries and cannibals problem
 *
 * Copyright (c) 1986, 1988 by Applied Logic Systems
 *
 * Author:  Kevin A. Buettner
 *
 */


/*
 *	Description:
 *
 *	Three missionaries and three cannibals seek to cross a river.
 *	A boat is available which holds two people, and which can be
 *	navigated by any combination of missionaries and cannibals
 *	involving one or two people.  If the missionaries on either
 *	bank of the river, or 'enroute' in the river, are outnumbered
 *	at any time by the cannibals, the cannibals will indulge
 *	their anthropophagic tendencies and do away with the
 *	missionaries.  Find the simplest schedule of crossing that
 *	will permit all the missionaries and canibals to cross the
 *	river safely.
 *
 *	Problem description from [CCP], Problem 64, Page 75.
 *
 *
 * 	Usage:
 *
 *	?- plan.
 *
 *	The program will then print out the solutions to this problem
 *	pictorially.  In the pictures, M stands for missionaries, C
 *	for cannibals, and B for the boat.  In the initial configuration
 *	the missionaries, cannibals, and the boat start out on the
 *	left hand side of the river.  In the final configuration, all
 *	of them are on the right and side of the river.
 *		
 *                                                                      
 * 	Book:                                                          
 *
 *	[CCP] = How to Solve it in Prolog, 
 *	Coelho, Cotta, Pereira, Lisbon, 1980            
 *
 */


/*
 * Data Structure:
 *	s(M,C,B)
 *		where M is the number of missionaries on the left side of the
 *                              river,
 *		      C is the number of cannibals on the left side of the
 *                              river,
 *                    B is which side of the river the boat is on, either
 *                              right or left.
 * 	Since all of the missionaries and cannibals start on the left side of
 *      the river, the initial state is s(3,3,left).  The configuration that
 *      we want to achieve is s(0,0,right).
 */


/*
 * Predicate:		plan
 * Normal Usage:	plan
 * Description:		Finds the solutions to the missionaries and cannibals
 *			problem and prints out the states in a meaningful
 *			manner.
 */

plan :- move(s(3,3,left),[s(3,3,left)],OutStates),
        print_states(OutStates),nl,nl,fail.
plan :- write('No more ways to solve this problem.'),nl.

print_states([]).
print_states([H | T]) :- print_states(T), print_state(H).


print_state(s(M,C,B)) :-
   RM is 3-M,
   RC is 3-C,
   print_chars(RM,0' ), print_chars(M,0'M),
   put(0'|), left_boat(B), right_boat(B), put(0'|),
   print_chars(RM,0'M),nl,
   print_chars(RC,0' ), print_chars(C,0'C),
   put(0'|), print_chars(2,0' ), put(0'|),
   print_chars(RC,0'C),nl,
   print_chars(10,0'-),nl,
   !.

print_chars(0,C) :- !.
print_chars(N,C) :- put(C), NP is N-1, print_chars(NP,C).

left_boat(left) :- put(0'B).
left_boat(right) :- put(0' ).
right_boat(left) :- put(0' ).
right_boat(right) :- put(0'B).



/*
 * Predicate:	  ok(State)
 * Normal Usage:  ok(in)
 * Description:   Takes a state State and succeeds if that state is permissible
 *                (no missionaries get eaten).
 */

ok(s(3,C,B)) :- !.		/* it is ok if there are three missionaries on 
				   the left side		*/
ok(s(0,C,B)) :- !.		/* it is ok if there are no missionaries on the
				   left side			*/

ok(s(MC,MC,B)).			/* it is ok if the number of missionaries on the
				 * left side is greater than or equal to the
				 * number of cannibals on the left side and if
				 * the number of missionaries on the right side
				 * is greater than or equal to the number of
				 * cannibals on the right side.  Which is to say
				 * that the number of cannibals equals the 
				 * number of missionaries 
				 *   (     M >= C and 3-M >= 3-C
				 *    ==>  M >= C and C >= M
				 *    ==>  M = C )
				 */


/*
 * Predicate:		move(State,InStateList,OutStateList)
 * Normal Usage:	move(in,in,out)
 * Description:		Takes as input a state State and a list of states
 *			that have already been visited (InStateList) and 
 *			returns as output the list of states (in reverse
 *			order) which solve the missionaries and cannibals
 *			problem.
 */

move(s(0,0,right),StateList,StateList).		/* final configuration */
move(InState,InStateList,OutStateList) :-
   cross(InState,NextState),			/* generate a next state */
   ok(NextState),				/* make sure no missionaries get
   						   eaten */
   not_member(NextState,InStateList),		/* make sure we're not
   						   looping */
   move(NextState,[NextState | InStateList],OutStateList).


/* 
 * Predicate:		cross(InState,OutState)
 * Normal Usage:	cross(in,out)
 * Description:		Takes as input InState and finds a state OutState.
 *			No claim is made about the suitability of OutState.
 *			That is, states in which cannibals get eaten can
 *			be generated.
 */


cross(s(M,C,B),s(NM,NC,NB)) :-
   cross(B,NB,M,NM,C,NC).

cross(left,right,M,NM,C,C) :- 
   M >= 2, NM is M-2.				/* 2 M cross from L to R */
cross(left,right,M,M,C,NC) :-
   C >= 2, NC is C-2.				/* 2 C cross from L to R */
cross(left,right,M,NM,C,NC) :-
   M >= 1, C > 1, NM is M-1, NC is C-1.		/* 1 M and 1 C cross L to R */
cross(left,right,M,NM,C,C) :-
   M >= 1, NM is M-1.				/* 1 M crosses alone L to R */
cross(left,right,M,M,C,NC) :-
   C >= 1, NC is C-1.				/* 1 C crosses alone L to R */

cross(right,left,M,M,C,NC) :-
   3-C >= 1, NC is C+1.				/* 1 C crosses alone R to L */
cross(right,left,M,NM,C,C) :-
   3-M >= 1, NM is M+1.				/* 1 M crosses alone R to L */
cross(right,left,M,NM,C,NC) :-
   3-M >= 1, 3-C >= 1, NM is M+1, NC is C+1.	/* 1 M and 1 C cross R to L */
cross(right,left,M,NM,C,C) :-
   3-M >= 2, NM is M+2.				/* 2 M cross from R to L */
cross(right,left,M,M,C,NC) :-
   3-C >= 2, NC is C+2.				/* 2 C cross from R to L */

/*
 * Predicate:		not_member(Element,List)
 * Normal Usage:	not_member(in,in)
 * Description:		Tests to see if Element is not a member of List.
 */

not_member(E,[E|_]) :- !, fail.
not_member(E,[_|T]) :- not_member(E,T).
not_member(E,[]).
