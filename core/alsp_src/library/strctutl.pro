/*============================================================================*
 |			strctutl.pro
 |	Copyright (c) 1994-96 Applied Logic Systems, Inc.
 |
 |		Utilities supporting typedefs/structs manipulation
 |
 |	Author: Ken Bowen
 |	Date:	April, 1994  (Combed out of DBD and GUI Spec programs)
 *============================================================================*/


module builtins.
export locate_struct/3.
export delete_struct/3.
export delete_struct/4.

/*!------------------------------------------------------------*
 |	locate_struct/3
 |	locate_struct(List, Key, Term)
 |	locate_struct(+, +, -)
 |
 |	- locates a term on a list, based on the 1st arg of the term
 |
 |	If List is a list of terms, and Key is any Prolog term, Term
 |	is the leftmost occurrence of a compound term on List whose
 |	first argument unifies with Key, if any;  fails if there is
 |	no such term.
 *!------------------------------------------------------------*/
locate_struct([TheTerm | RestTerms], TheKey, TheTerm)
	:-
	arg(1, TheTerm, TheKey),
	!.

locate_struct([_ | RestTerms], TheKey, TheTerm)
	:-
	locate_struct(RestTerms, TheKey, TheTerm).

/*!------------------------------------------------------------*
 |	delete_struct/3
 |	delete_struct(List, Key, ResultList)
 |	delete_struct(+, +, -)
 |
 |	- Locates & deletes a term from a list, based on first arg of term
 *!------------------------------------------------------------*/
delete_struct(List, Key, ResultList)
	:-
	delete_struct(List, Key, ResultList, _).

/*!------------------------------------------------------------*
 |	delete_struct/4
 |	delete_struct(List, Key, ResultList, Term)
 |	delete_struct(+, +, -, -)
 |
 |	- Locates & deletes a term from a list, based on first arg of term
 |
 |	If List is a list of terms, and Key is any Prolog term, then
 |	ResultList is the result of deleting the leftmost occurrence
 |	of any compound term Term on List whose first argument unifies 
 |	with Key;  fails if no such term occurs on List.  If List has 
 |	two or more elements, the deletion is done destructively via mangle.
 *!------------------------------------------------------------*/
delete_struct([Term | Tail], Key, Tail, Term)
	:-
	arg(1,Term,Key),
	!.
delete_struct(List, Key, List, Term)
	:-
	List = [_ | Tail],
	delete_struct0(Tail, List, Key, Term).

		%% Invariant: List = [Head, Term | MoreTail]
delete_struct0([Term | MoreTail], List, Key, Term)
	:-
	arg(1, Term, Key),
	!,
	mangle(2, List, MoreTail).
	
delete_struct0(Tail, List, Key, Term)
	:-
	Tail = [_|MoreTail],
	delete_struct0(MoreTail, Tail, Key, Term).

endmod.


