/*=====================================================================
 * 			commal.pro		
 *		Copyright (c) 1992 Applied Logic Systems, Inc.
 *
 *		Various utilities for dealing with comma lists
 *====================================================================*/

module builtins.

export flatten_comma_list/2.

/*!---------------------------------------------------------------
 |	flatten_comma_list/2
 |	flatten_comma_list(SourceList, ResultList)
 |	flatten_comma_list(+, -)
 |	
 |	- flattens nested comma lists and removes extraneous true's
 |	
 |	If SourceList is a comma list (i.e., (a,b,c,...) ), then
 |	ResultList is also a comma list which is the result of removing
 |	all extraneous nesting and all extraneous occurrences of 'true'.
 *!--------------------------------------------------------------*/

flatten_comma_list(B, B)
	:-
	var(B),!.

flatten_comma_list((VV, B), (VV, FlatB))
    :-
	var(VV), !,
    flatten_comma_list(B, FlatB).

flatten_comma_list((true, B), FlatB)
    :-!,
    flatten_comma_list(B, FlatB).

flatten_comma_list( ((VV,B), WW), ((VV, FlatB), WW))
    :-
	var(WW), var(VV), !,
    flatten_comma_list(B, FlatB).

flatten_comma_list( ((true,B), WW), (FlatB, WW))
    :-
	var(WW), !,
    flatten_comma_list(B, FlatB).

flatten_comma_list( ((VV,B), true), (VV, FlatB))
    :-
	var(VV), !,
    flatten_comma_list(B, FlatB).

flatten_comma_list( ((true,B), true), FlatB)
    :-!,
    flatten_comma_list(B, FlatB).

flatten_comma_list( ((true,B), C), Result)
    :-!,
    flatten_comma_list(B, FlatB),
    flatten_comma_list(C, FlatC),
    join_up(FlatB, FlatC, Result).

flatten_comma_list( ((A,B), true), Result)
    :-!,
    flatten_comma_list(A, FlatA),
    flatten_comma_list(B, FlatB),
    join_up(FlatA, FlatB, Result).

flatten_comma_list( ((A,B), C), Result)
    :-!,
    flatten_comma_list(A, FlatA),
    flatten_comma_list(B, FlatB),
    flatten_comma_list(C, FlatC),
    join_up(FlatB, FlatC, Res1),
    join_up(FlatA, Res1, Result).

flatten_comma_list((A, VV), (FlatA, VV))
    :-
	var(VV), !,
    flatten_comma_list(A, FlatA).

flatten_comma_list((A, true), FlatA)
    :-!,
    flatten_comma_list(A, FlatA).

flatten_comma_list( (VV, (B,C) ), (VV, Result))
    :-
	var(VV), !,
    flatten_comma_list(B, FlatB),
    flatten_comma_list(C, FlatC),
    join_up(FlatB, FlatC, Result).

flatten_comma_list( (true, (B,C) ), Result)
    :-!,
    flatten_comma_list(B, FlatB),
    flatten_comma_list(C, FlatC),
    join_up(FlatB, FlatC, Result).

flatten_comma_list( (A, (B,C) ), Result)
    :-!,
    flatten_comma_list(A, FlatA),
    flatten_comma_list((B,C), FlatBC),
    join_up(FlatA, FlatBC, Result).

flatten_comma_list(A, A).

join_up(A, B, (A, B))
	:-
	var(A),!.

join_up((A, B), C, Result)
    :-!,
    join_up(B, C, Res1),
    join_up(A, Res1, Result).

join_up(A, B, (A, B)).


endmod.
