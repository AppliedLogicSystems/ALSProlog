/*======================================================================
 |			listutl2.pro
 |		Copyright (c) 1991 Applied Logic Systems, Inc.
 |
 |		Various positional list predicates
 *=====================================================================*/
%! category(terms).
%! group(list).
%! subgroup(positional).

module builtins.

export deleteNth/3.
export change_nth/3.
export subst_nth/4.
export nth/3.
export nth_tail/4.
export at_most_n/3.
export position/3.
export position/4.
export get_list_tail/3.
export list_delete/3.
export sublist/4.

/*!---------------------------------------------------------------------
 | deleteNth/3
 | deleteNth(N, List, Remainder)
 | deleteNth(+, +, -)
 |
 |	- deletes the Nth element of a list
 |
 |	If N is a non-negative integer and List is a list, then Remainder
 |	is the result of deleting the Nth element of List; this predicate
 |	numbers the list beginning with 1.
 *!--------------------------------------------------------------------*/
deleteNth(1, [_ | Remainder], Remainder) :-!.
deleteNth(N, [Item | Tail], [Item | TailRemainder]) :-
   N > 1, M is N-1,
   deleteNth(M, Tail, TailRemainder).

/*!---------------------------------------------------------------------
 |	change_nth/3
 |	change_nth(N, List, NewItem)
 |	change_nth(+, +, +)
 |
 |	- destructively changes the Nth element of a list
 |
 |	If N is a non-negative integer, List is list, and NewItem is any
 |	non-var object, destructively changes the Nth element of List to become
 |	NewItem; this predicate numbers the list beginning with 0.
 *!--------------------------------------------------------------------*/
change_nth(0, List, NewItem)
	:-!,
	mangle(1, List, NewItem).
change_nth(N, [_ | List], NewItem)
	:-
	M is N-1,
	change_nth(M, List, NewItem).

/*!---------------------------------------------------------------------
 |	subst_nth/4
 |	subst_nth(N, List, NewItem, NewList)
 |	subst_nth(+, +, +, -)
 |
 |	- non-destructively changes the Nth element of a list
 |
 |	If N is a non-negative integer, List is list, and NewItem is any
 |	non-var object, NewList is the result of non-destrctively changing 
 |	the Nth element of List to become |	NewItem; 
 |	this predicate numbers the list beginning with 0.
 *!--------------------------------------------------------------------*/
subst_nth(0, [_ | List], NewItem, [NewItem | List]) :-!.
subst_nth(N, [Skip | List], NewItem, [Skip | NewList])
	:-
	M is N-1,
	subst_nth(M, List, NewItem, NewList).

/*!---------------------------------------------------------------------
 |	nth/3
 |	nth(N, List, X)
 |	nth(+, +, -)
 |
 |	 - returns the nth element of a list
 |
 |	If List is a list and N is a non-negative integer, then X is
 |	the nth element of List.
 *!--------------------------------------------------------------------*/
nth(0, [X | _], X) :-!.
nth(M, [_ | T], X) :-
	K is M-1,
	nth(K, T, X).

/*!---------------------------------------------------------------------
 |	nth_tail/4
 |	nth_tail(N, List, Head, Tail)
 |	nth_tail(+, +, -, -)
 |
 |	 - returns the nth head and tail of a list
 |
 |	If List is a list and N is a non-negative integer, then Head is
 |  the portion of List up to but not including the Nth element,
 |  and tail is the portion of List from the Nth element to the end.
 *!--------------------------------------------------------------------*/
nth_tail(0, L, [], L) :-!.
nth_tail(M, [X | T], [X | L_H], L) :-
	K is M-1,
	nth_tail(K, T, L_H, L).

/*!---------------------------------------------------------------------
 |	at_most_n/3
 |	at_most_n(List, N, Head)
 |	at_most_n(+, +, -)
 |
 |	- returns initial segment of list of length =< N
 |
 |	If List is a list and N is a non-negative integer, Head is the
 |	longest initial segment of List with length =< N.
 *!--------------------------------------------------------------------*/
at_most_n([], _, []).
at_most_n(_, 0, []) :-!.
at_most_n([X | T], N, [X | R])
	:-
	M is N-1,
	at_most_n(T, M, R).

/*!---------------------------------------------------------------------
 |	position/3
 |	position(List, Item, N)
 |	position(+, +, -)
 |
 |	-	returns the position number of an item in a list
 |
 |	If List is a list and Item occurs in List, N is the number of
 |	the leftmost occurrence of Item in List; fails if Item does not
 |	occur in List.
 *!--------------------------------------------------------------------*/
position(List, Item, Nth)
	:-
	position(List, Item, 0, Nth).

/*!---------------------------------------------------------------------
 |	position/4
 |	position(List, Item, M, N)
 |	position(+, +, +, -)
 |
 |	-	returns the position number of an item in a list
 |
 |	If List is a list and Item occurs in List, N-M is the number of
 |	the leftmost occurrence of Item in List; fails if Item does not
 |	occur in List.
 *!--------------------------------------------------------------------*/
position([Item | _], Item, Nth, Nth) :-!.
position([_ | ListTail], Item, Current, Nth)
	:-
	Next is Current+1,
	position(ListTail, Item, Next, Nth).

/*!---------------------------------------------------------------------
 |	get_list_tail/3
 |	get_list_tail(List, Item, Tail)
 |	get_list_tail(+, +, -)
 |
 |	- returns the tail of a list determined by an element
 |
 |	If List is a list and Item is any object, Tail is the portion
 |	of List extending from the leftmost occurrence of Item in List
 |	to the end of List; fails if Item does not belong to List.
 *!--------------------------------------------------------------------*/
get_list_tail(List, Item, List)
	:-
	List = [Item | _] , !.
get_list_tail([_ | RestList], Item, Tail)
	:-
	get_list_tail(RestList, Item, Tail).

/*!---------------------------------------------------------------------
 |	list_delete/3
 |	list_delete(List, Item, ResultList)
 |	list_delete(+, +, -)
 |
 |	- deletes all occurrences of an item from a list
 |
 |	If List is a list, and Item is any object, ResultList is obtained
 |	by deleting all occurrences of Item from List.
 *!--------------------------------------------------------------------*/
list_delete(X, _, X) :- var(X),!.
list_delete([], _, []).
list_delete([Item | Rest_In_List], Item, Out_List)
	:-!,
	list_delete(Rest_In_List, Item, Out_List).
list_delete([Keep | Rest_In_List], Item, [Keep | Rest_Out_List])
	:-
	list_delete(Rest_In_List, Item, Rest_Out_List).

/*!---------------------------------------------------------------------
 | 	sublist/4
 | 	sublist(List,Start,Length,Result) 
 | 	sublist(+,+,+,-) 
 |
 |	- extracts a sublist from a list
 |
 |	If List is an arbitrary list, Result is the sublist of length Length
 |	beginning at position Start in List.
 *!--------------------------------------------------------------------*/
sublist(List,0,Length,Result) 
	:- !,
	sublist1(List,Length,Result).
sublist([_|Rest],Cur,Length,Result) 
	:-
	Now is Cur-1,
	sublist(Rest,Now,Length,Result).

sublist1(_,0,[]) 
	:- !.
sublist1([Char|Rest],Cur,[Char|RRest]) 
	:-
	Now is Cur-1,
	sublist1(Rest,Now,RRest).

endmod.
