/*=====================================================================
 | 			xlists.pro		
 |	Copyright (c) 1993-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		Simple xtensible list utilities
 *====================================================================*/

module builtins.

	/*--------------------------------------
	 |	xtensible lists are carried
	 |	around in the form
	 |		(Head, Tail)
	 |	The actual list may be standard
	 |	xtensible list
	 |		[a,b,c,d | T]
	 |	or may be a comma separated list:
	 |		(a, (b, (c, (d, T))))
	 |	It is up to the routines using these
	 |	tools to bind the tail variable to
	 |	the correct structure, or to create
	 |	the correct type of structure for
	 |	xappending to another such xlist.
	 *-------------------------------------*/

/*!---------------------------------------------------------------
 |	xlist_init/1
 |	xlist_init(Result)
 |	xlist_init(-)
 |
 |	-	creates a freshly initialized extensible list
 |
 *!--------------------------------------------------------------*/
xlist_init( (H, H) ).

/*!---------------------------------------------------------------
 |	xlist_unit_c/2
 |	xlist_unit_c(First, Result)
 |	xlist_unit_c(+, -)
 |
 |	-	creates a freshly initialized comma-type xlist with first elt
 |
 *!--------------------------------------------------------------*/
xlist_unit_c( First, ( (First, T), T) ).

/*!---------------------------------------------------------------
 |	xlist_unit_l/2
 |	xlist_unit_l(First, Result)
 |	xlist_unit_l(+, -)
 |
 |	-	creates a freshly initialized ordinary xlist with first elt
 |
 *!--------------------------------------------------------------*/
xlist_unit_l( First, ([First | T], T) ).

/*!---------------------------------------------------------------
 |	xlist_make/3
 |	xlist_make( Head, Tail, Result)
 |	xlist_make( +, +, -)
 |
 |	-	Makes an extenstible list data structure from Head,Tail
 |
 *!--------------------------------------------------------------*/
xlist_make(H,T, (H,T)).

/*!---------------------------------------------------------------
 |	xlist_head/2
 |	xlist_head( XList, Result)
 |	xlist_head( +, -)
 |
 |	-	returns the head of an extensible list
 |
 *!--------------------------------------------------------------*/
xlist_head( (H, _), H).

/*!---------------------------------------------------------------
 |	xlist_tail/2
 |	xlist_tail( XList, Result)
 |	xlist_tail( +, -)
 |
 |	-	returns the tail of an extensible list
 |
 *!--------------------------------------------------------------*/
xlist_tail( (_, T), T).

/*!---------------------------------------------------------------
 |	xlist_append/3
 |	xlist_append( Left, Right, Result)
 |	xlist_append( +, +, -)
 |
 |	-	appends two extensible lists
 |
 *!--------------------------------------------------------------*/
xlist_append( (H,T), (T,U), (H, U) ).

/*!---------------------------------------------------------------
 |	xlist_append/2
 |	xlist_append( ListOfXLists, Result)
 |	xlist_append( +, -)
 |
 |	-	appends an ordinary list of extensible lists
 |
 *!--------------------------------------------------------------*/
xlist_append([ XL ], XL) :-!.
xlist_append([XL1, XL2 | RestInput], Result)
	:-
	xlist_append(XL1, XL2, Inter),
	xlist_append0(RestInput, Inter, Result).

xlist_append0([], Result, Result).
xlist_append0([A | RestInput], Inter, Result)
	:-
	xlist_append(Inter, A, InterAgain),
	xlist_append0(RestInput, InterAgain, Result).

/*!---------------------------------------------------------------
 |	xlist_mem/2
 |	xlist_mem(List, Item)
 |	xlist_mem(+, +)
 *!--------------------------------------------------------------*/

xlist_mem(List, Item)
	:-
	var(List), !, fail.
xlist_mem([Item | List], Item).
xlist_mem([_ | List], Item)
	:-
	xlist_mem(List, Item).

/*!---------------------------------------------------------------
 |	xlist_dmem/2
 |	xlist_dmem(List, Item)
 |	xlist_dmem(+, +)
 *!--------------------------------------------------------------*/

xlist_dmem(List, Item)
	:-
	var(List), !, fail.
xlist_dmem([Item | List], Item) :-!.
xlist_dmem([_ | List], Item)
	:-
	xlist_dmem(List, Item).


endmod.
