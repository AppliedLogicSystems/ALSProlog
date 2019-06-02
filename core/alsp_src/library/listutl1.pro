/*======================================================================
 |			listutl1.pro
 |	Copyright (c) 1991-2019 Applied Logic Systems, Inc.
 |		Group: Lists
 |		DocTitle: list_diff/3
 |		-- Various algebraic list predicates
 *=====================================================================*/
module builtins.

export append/2.
export list_diff/3.
export list_diffs/4.
export symmetric_diff/3.
export intersect/3.
export intersect/2.
export int_diff/4.
export union/3.
export sorted_merge/3.
export sorted_merge/2.
export subset/2.
export init_seg_list/2.

/*!---------------------------------------------------------------------
 |	append/2
 |	append(ListOfLists, Result)
 |	append(+, -)
 |
 |	- appends a list of lists together
 |
 |	If ListOfLists if a list, each of whose elements is a list, Result
 |	is obtained by appending the members of ListOfLists together in order.
 *!--------------------------------------------------------------------*/
append([], []).
append([List | ListOfLists], Result)
	:-
	append(ListOfLists, InterResult),
	append(List, InterResult, Result).

/*!---------------------------------------------------------------------
 |	list_diff/3
 |	list_diff(A, B, A_NotB)
 |	list_diff(+, +, +)
 |
 |	- returns the ordered difference of two lists
 |
 |	If A and B are lists, returns the difference A-B consisting of
 |	all items on A, but not on B.
 *!--------------------------------------------------------------------*/
list_diff([], _, []).
list_diff([X | RestA], B, A_NotB)
	:-
	dmember(X, B),!,
	list_diff(RestA, B, A_NotB).
list_diff([X | RestA], B, [X | RestA_NotB])
	:-
	list_diff(RestA, B, RestA_NotB).

/*!---------------------------------------------------------------------
 |	list_diffs/4
 |	list_diffs(A,B,A_NotB,B_NotA)
 |	list_diffs(+,+,-,-)
 |
 |	- returns both ordered differences of two lists
 |
 |	If A and B are lists, returns both the difference A-B together
 |	with the difference B-A.
 *!--------------------------------------------------------------------*/
list_diffs(A,B,A_NotB,B_NotA)
	:-
	list_diff(A, B, A_NotB),
	list_diff(B, A, B_NotA).

/*!---------------------------------------------------------------------
 |	symmetric_diff/3
 |	symmetric_diff(A,B,A_symd_B)
 |	symmetric_diff(+,+,-)
 |
 |	- returns the symmetric difference of two lists
 |
 |	If A and B are lists, returns the symmetric difference of A and B,
 |	which is the union of A-B and B-A.
 *!--------------------------------------------------------------------*/
symmetric_diff(A,B,A_symd_B)
	:-
	list_diffs(A,B,A_NotB,B_NotA),
	union(A_NotB, B_NotA, A_symd_B).

/*!---------------------------------------------------------------------
 |	intersect/3
 |	intersect(A,B,AintB)
 |	intersect(+,+,-)
 |
 |	- returns the intersection of two lists
 |
 |	If A and B are lists, returns the intersection AintB of A and B, 
 |	which is the list of all items common to both lists, in order.
 *!--------------------------------------------------------------------*/
intersect([],_,[]) :-!.
intersect(_,[],[]) :-!.
	%% Must be this way for failing the goal: intersect([a,b],[b,c],[]).
intersect([X | RestA],B,AintB)
	:-
	dmember(X,B),!,
	AintB = [X | RestAintB],
	intersect(RestA,B,RestAintB).
intersect([X | RestA],B,RestAintB)
	:-
	intersect(RestA,B,RestAintB).

/*!---------------------------------------------------------------------
 |	int_diff/4
 |	int_diff(A,B,AintB,AnotB)
 |	int_diff(+,+,-,-)
 |
 |	- returns the intersection and one difference of two lists
 |
 |	If A and B are lists, returns the intersection AintB of A and B, 
 |	together with the difference A - B.
 *!--------------------------------------------------------------------*/
int_diff([],_,[],[]) :-!.
int_diff(_,[],[],[]) :-!.
int_diff([X | RestA], B, [X | RestAintB], AnotB)
	:-
	dmember(X,B),!,
	int_diff(RestA, B, RestAintB, AnotB).
int_diff([X | RestA], B, RestAintB, [X | RestAnotB])
	:-
	int_diff( RestA, B, RestAintB, RestAnotB).

/*!---------------------------------------------------------------------
 |	intersect/2
 |	intersect(L,IntsectL)
 |	intersect(+,-)
 |
 |	- returns the intersection of a list of lists
 |
 |	If L is a list of lists, returns the intersection IntsectL of all 
 |	of the lists appearing on L.
 *!--------------------------------------------------------------------*/
intersect([], []) :-!.

intersect([A | RestL], IntsectL)
	:-
	intersect0(RestL, A, IntsectL). 

intersect0([], Accum, Accum). 
intersect0([B | RestL], Accum, IntsectL)
	:-
	intersect(B, Accum, NewAccum),
	intersect0(RestL, NewAccum, IntsectL). 

/*!---------------------------------------------------------------------
 |	union/3
 |	union(A,B, AuB)
 |	union(+,+, -)
 |
 |	- returns the ordered union of two lists
 |
 |	If A and B are lists, returns the ordered union of A and B, consisting
 |	of all items occurring on either A or B, with all occurrences of items
 |	from A occurring before any items from B-A; equivalent to:<br>
 |		append(A,B-A,AuB);<br>
 |	If both lists have the property that each element occurs no more 
 |	than once, then the union also has this property.
 *!--------------------------------------------------------------------*/
union([],B, B) :-!.
union(A,[], A) :-!.
union(A,B, AuB)
   :-
   union0(A,B,AuB).

union0([],A,A).
union0([X | RestB],A,[X | RestAuB])
	:-
	not(dmember(X,A)),!,
	union0(RestB,A,RestAuB).
union0([_ | RestB],A,RestAuB)
	:-
	union0(RestB,A,RestAuB).

/*!--------------------------------------------------------------------*
 |	sorted_merge/3
 |	sorted_merge(List1, List2, Union)
 |	sorted_merge(+, +, -)
 |
 |	- returns the sorted union of two lists
 |
 |	If List1 and List2 are lists of items, Union is 
 |	the sorted merge (non-repetitive union) of List1 and List2.
 *!--------------------------------------------------------------------*/
sorted_merge(List1, List2, Union) 
	:-
	append(List1, List2, Smear),
	sort(Smear, Union).

/*!--------------------------------------------------------------------*
 |	sorted_merge/2
 |	sorted_merge(ListOfLists, Union)
 |	sorted_merge(+, -)
 |
 |	- returns the sorted union of a list of lists
 |
 |	If ListOfLists is a list of lists, Union is the sorted merge 
 |	(non-repetitive union) of the members of ListsOfLists.
 *!--------------------------------------------------------------------*/
sorted_merge(ListOfLists, Union) 
	:-
	append(ListOfLists, AppendedLists),
	sort(AppendedLists, Union).

/*!--------------------------------------------------------------------*
 |	subset/2
 |	subset(LeftList, RightList)
 |	subset(+, +)
 |
 |	- determines if one list is a subset of another
 |
 |	If LeftList and RightList are both lists, this predicate is
 |	true if and only if every element of LeftList is also an
 |	element of RightList
 *!--------------------------------------------------------------------*/
subset([], _).
subset([E | T], RL)
	:-
	dmember(E, RL),
	subset(T, RL).

/*!--------------------------------------------------------------------*
 |	init_seg_list/2
 |	init_seg_list(LeftList, RightList)
 |	init_seg_list(+, +)
 |
 |	- determines if one list is an initial segment of another
 |
 |	If LeftList and RightList are both lists, this predicate is
 |	true if and only if LeftList is an initial sublist of  RightList.
 *!--------------------------------------------------------------------*/
init_seg_list([], _) :-!.
init_seg_list([A | LeftTail], [A | RightTail])
        :-!,
        init_seg_list(LeftTail, RightTail).

endmod.
