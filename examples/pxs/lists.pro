/*---------------------------------------------------------------*
 *	lists.pro
 *	Copyright (c) 1989-90 Applied Logic Systems, Inc.
 *
 *	Elementary list processing predicates
 *---------------------------------------------------------------*/


member(Item, [Item | Tail]).
member(Item, [Anything | Tail] )
	:- 
	member(Item, Tail).


length( [] , 0 ).

length( [ Head | Tail ],  M )
	:-
	length( Tail,  N ),
	M is N + 1.


append([], List2, List2).
append([Head | Tail], List2, [Head | Result_Tail])  
	:-
	append(Tail, List2, Result_Tail). 

%%----------Variations on reverse--------------

naive_reverse([], []).
naive_reverse([Head | Tail ],  Result)
	:-
	naive_reverse(Tail,  Reversed_Tail),
	append(Reversed_Tail, [ Head ], Result).


reverse(L, R) 
	:- 
	rev0(L, [], R).

rev0([], Accum, Accum).
rev0( [Next | RestInput], Accum, Output)
	:-
	rev0( RestInput, [ Next | Accum], Output).

%%----------Flattening lists--------------

flatten( [],  []).
flatten( [ Head | Tail ],  FlatList )
	:-
	flatten( Head,  FlatHead ),
	flatten( Tail,    FlatTail ),
	append( FlatHead,  FlatTail,  FlatList).
flatten( Item,  [ Item ] ).

%%----------Sorting--------------

insert_sort( [],  []).
insert_sort( [Head | Tail ],  Sorted_Result )
	:-
	insert_sort( Tail,  Sorted_Tail ),
	insert( Head,  Sorted_Tail,  Sorted_Result).


insert_sort2( Unsorted,  Sorted_Result)
	:-
	insert_sort3( Unsorted,  [],  Sorted_Result).

insert_sort3( [],  Accumulator,  Accumulator).
insert_sort3( [Head | Unsorted_Tail], Accumulator, Sorted_Result)
	:-
	insert(Head, Accumulator, New_Accumulator),
	insert_sort3(Unsorted_Tail, New_Accumulator, Sorted_Result).


insert( Item,  [],  [ Item ] ).
insert( Item,  [ Head | Tail ],  [Item, Head | Tail ] )
	:-
	comes_before(Item,  Head).	% needs definition
insert( Item,  [Head | Tail],  [ Head | Result_Tail] )
	:-
	comes_before( Head,  Item),
	insert( Item,  Tail,  Result_Tail).


quicksort( [],  [] ).
quicksort( [ H | Tail ], Result )
	:-
	partition( Tail, H,  T_Left, T_Right ),
	quicksort( T_Left,  Sorted_Left ),
	quicksort( T_Right,  Sorted_Right ),
	append( T_Right,  [ H | Sorted_Right ], Result ).

partition( Wedge,  [],  [],  [] ).
partition( Wedge,  [ H | Input_Tail],  [ H | Left_Tail], Right_Tail )
	:-
	H =< Wedge,
	partition( Wedge,  Input_Tail,  Left_Tail, Right_Tail ).
partition( Wedge,  [ H | Input_Tail],  Left_Tail,  [ H | Right_Tail  ] )
	:-
	Wedge < H,
	partition( Wedge,  Input_Tail,  Left_Tail, Right_Tail ).

