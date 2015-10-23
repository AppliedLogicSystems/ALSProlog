/*========================================================================*
 |			control.pro
 |	Copyright (c) 1991-96 Applied Logic Systems, Inc.
 |
 |			Miscellaneous control predicates
 |
 |	Authors:	Ken Bowen, Kevin Buettner
 |	Date:		1986-91
 *========================================================================*/

module builtins.

export bagOf/3.
export setOf/3.

:-module_closure(bagOf,3,bagOf).
:-module_closure(setOf,3,setOf).

/*!---------------------------------------------------------------------
 	bagOf/3
 	bagOf(Pattern, Goal, Result)
 	bagOf(+, +, -)

	-	Like bagof/3, but succeeds with empty list on no solutions

	bagOf/3 is just like bagof/3, except that if Goal has no solutions,
	bagof/3 fails, whereas bagOf/3 will succeed, binding Result to [].
 *!--------------------------------------------------------------------*/
bagOf(Module,Pattern, Goal, Result)
	:-
	bagof(Module,Pattern,Goal,Result),!.
bagOf(_,_, _, []).


/*!---------------------------------------------------------------------
 	setOf/3
 	setOf(Pattern, Goal, Result)
 	setOf(+, +, -)

	-	Like setof/3, but succeeds with empty list on no solutions

	setOf/3 is just like setof/3, except that if Goal has no solutions,
	setof/3 fails, whereas setOf/3 will succeed, binding Result to [].
 *!--------------------------------------------------------------------*/
setOf(Module,Pattern, Goal, Result)
	:-
	setof(Module,Pattern,Goal,Result),!.
setOf(_,_, _, []).

endmod.
