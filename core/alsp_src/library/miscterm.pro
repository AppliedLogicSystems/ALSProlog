/*=====================================================================
 | 			miscterm.pro		
 |	Copyright (c) 1990 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		Term manipulation utilities for the library
 *====================================================================*/

module builtins.

/*!---------------------------------------------------------------
 |	set_all_args/4
 |	set_all_args(Start,End,Term,ArgVal)	
 |	set_all_args(+,+,+,+)	
 |
 |	-	sets args Start to End of Term to ArgVal using arg/3
 *!--------------------------------------------------------------*/

export set_all_args/4.
set_all_args(Cur,Size,FF,ArgVal)
	:-
	Cur > Size, !. 
set_all_args(Cur,Size,FF,ArgVal)
	:-
	arg(Cur,FF,ArgVal),
	Next is Cur +1,
	set_all_args(Next,Size,FF,ArgVal).

endmod.
