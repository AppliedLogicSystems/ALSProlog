/*========================================================================*
 |			arithx-1.pro
 |	Copyright (c) 1991-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |				Miscellaneous arithmetic predicates
 |
 |	Authors:	Various
 |	Date:		1991--96
 *========================================================================*/

module builtins.

export max/3.
export min/3.

/*!-----------------------------------------------------------------------
 |	max/3
 |	max(A,B,C)
 |	max(A,B,C)
 |
 |	- C is maximum of results of evaluation of A,B with is/2
 *-----------------------------------------------------------------------*/
max(A,B,C) 
	:- 
	A0 is A, 
	B0 is B, 
	max0(A0,B0,C).
max0(A,B,A) 
	:- 
	A >= B,!.
max0(A,B,B).

/*!-----------------------------------------------------------------------
 |	min/3
 |	min(A,B,C)
 |	min(A,B,C)
 |
 |	- C is minimum of results of evaluation of A,B with is/2
 *-----------------------------------------------------------------------*/
min(A,B,C) 
	:- 
	A0 is A, 
	B0 is B, 
	min0(A0,B0,C).
min0(A,B,A) 
	:- 
	A =< B,!.
min0(A,B,B).

endmod.
