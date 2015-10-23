/*======================================================================
 |		blt_als.pro
 |		Copyright (c) 1986-1996 Applied Logic Systems, Inc.
 |
 |	Special Builtin predicates 
 |
 |	Authors: Kevin A. Buettner
 |	Original Creation Date: 3/20/86
 *====================================================================*/

module builtins.
 
/*
 * list_asm
 */

export list_asm/1.

list_asm(X) 
	:- 
	clauses_for_listing(X,C), 
	'$listasm_clause'(C), 
	nl, 
	fail.
list_asm(X).

endmod.
