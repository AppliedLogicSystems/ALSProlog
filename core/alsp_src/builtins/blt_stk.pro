/*-----------------------------------------------------------------*
 | blt_stk.pro 	-- Stack Overflow Check
 |
 | Copyright (c) 1992 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 | Author: Ilyas Cicekli
 | Date  : 5/27/91
 *-----------------------------------------------------------------*/

module builtins.

export stack_overflow/0.
export stack_overflow/1.

stack_overflow :-
	stack_overflow(0x1000).

stack_overflow(Safety) :-
	'$stack_overflow'(Safety), !,
	nl(user), write(user,'Stack Overflow. '), nl(user),
	abort.
stack_overflow(_).

endmod.

