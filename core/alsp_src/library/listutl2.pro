/*======================================================================
 |			listutl2.pro
 |	Copyright (c) 1991-2019 Applied Logic Systems, Inc.
 |		Group: Lists
 |		DocTitle: nth_tail/4
 |		-- Various positional list predicates
 *=====================================================================*/
module builtins.

export deleteNth/3.
export change_nth/3.
export subst_nth/4.
export nth_tail/4.
export at_most_n/3.
export get_list_tail/3.
export sublist/4.
export last/2.
export delete_1st/3.
export nonmember/2.

%% -- moved to simplio.pro in builtins:
%export nth/3. 
%export position/3.
%export position/4.

/*!---------------------------------------------------------------------
 | deleteNth/3
 | deleteNth(N, List, Remainder)
 | deleteNth(+, +, -)
 |
 |	- deletes the Nth element of a list
 |
 |	If N is a non-negative integer and List is a list, then Remainder
 |	is the result of deleting the Nth element of List. 
 |	This predicate numbers the list beginning with 1.
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
 |	NewItem. This predicate numbers the list beginning with 0.
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
 |	non-var object, NewList is the result of non-destructively changing 
 |	the Nth element of List to become NewItem. 
 |	This predicate numbers the list beginning with 0.
 *!--------------------------------------------------------------------*/
subst_nth(0, [_ | List], NewItem, [NewItem | List]) :-!.
subst_nth(N, [Skip | List], NewItem, [Skip | NewList])
	:-
	M is N-1,
	subst_nth(M, List, NewItem, NewList).

/*!---------------------------------------------------------------------
 |	nth_tail/4
 |	nth_tail(N, List, Head, Tail)
 |	nth_tail(+, +, -, -)
 |
 |	 - returns the nth head and tail of a list
 |
 |	If List is a list and N is a non-negative integer, then Head is
 |  the portion of List up to and including the Nth element,
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
 |	get_list_tail/3
 |	get_list_tail(List, Item, Tail)
 |	get_list_tail(+, +, -)
 |
 |	- returns the tail of a list determined by an element
 |
 |	If List is a list and Item is any object, Tail is the portion
 |	of List extending from the leftmost occurrence of Item in List
 |	to the end of List. Fails if Item does not belong to List.
 *!--------------------------------------------------------------------*/
get_list_tail(List, Item, List)
	:-
	List = [Item | _] , !.
get_list_tail([_ | RestList], Item, Tail)
	:-
	get_list_tail(RestList, Item, Tail).

/*!---------------------------------------------------------------------
 | 	sublist/4
 | 	sublist(List,Start,Length,Result) 
 | 	sublist(+,+,+,-) 
 |
 |	- extracts a sublist from a list
 |
 |	If List is an arbitrary list, Result is the sublist of 
 |	length Length, beginning at position Start in List.
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

/*!---------------------------------------------------------------------
 |	last/2
 |	last(List, Item)
 |	last(+, -)
 |
 |	- returns last element of a list
 |
 |	If List is a non-empty list, Item is the last (right-most) element
 |	of List.
 *!--------------------------------------------------------------------*/

last([Item],Item)
	:-!.

last([_|Tail],Item)
	:-
	last(Tail, Item).

/*!---------------------------------------------------------------------
 |	delete_1st/3
 |	delete_1st(List, Item, Result)
 |	delete_1st(+, +, -)
 |
 |	- deletes the left-most entry of Item in List
 |
 |	If Item occurs on List, deletes the left-most entry of Item on
 |	List, returning in Result the tail of List beginning at that entry.
 |	Fails if Item is not on List.
 *!--------------------------------------------------------------------*/

delete_1st([Item | Result], Item, Result)
	:-!.
delete_1st([Skip | List], Item, [Skip | Result])
	:-
	delete_1st(List, Item, Result).

/*!---------------------------------------------------------------------
 |	nonmember/2
 |	nonmember(List, Item)
 |	nonmember(+, +)
 |
 |	-	tests for the failure of membership
 |
 |	Succeeds iff member(List, Item) fails
 *!--------------------------------------------------------------------*/
nonmember([], _).

nonmember([Item | _], Item)
	:-!,
	fail.

nonmember([_ | Tail], Item)
	:-
	nonmember(Tail, Item).

endmod.
