/*======================================================================
 |			listutl3.pro
 |	Copyright (c) 1991-2019 Applied Logic Systems, Inc.
 |		Group: Lists
 |		DocTitle: number_list/3
 |		-- Miscellaneous list predicates
 *=====================================================================*/
module builtins.

export nobind_member/2.
export flatten/2.
export is_length/2.
export n_of/3.
export number_list/2.
export number_list/3.
export encode_list/3.
export check_default/4.
export check_default_del/5.
export remove_tagged/3.
export merge_plists/3.
export mangle_change_tagged/3.
export subst_tagged/4.
export struct_lookup_subst/4.
export merge_tagged_lists/3.
export merge_in_list/3.
export insert_item_in_list/3.
export insert_spaces/2.

/*!---------------------------------------------------------------------
 |	nobind_member/2
 |	nobind_member(X, List)
 |	nobind_member(+, +)
 |
 |	- tests list membership without binding any variables
 |
 |	Logically, nobind_member(X, List) holds and only if X is a member 
 |	of List; if the test is successful, no variables in either input are bound.
 *!--------------------------------------------------------------------*/
nobind_member(X, [Y | _])
	:-
	not(not(X=Y)),!.
nobind_member(X, [_ | T])
	:-
	nobind_member(X, T).

/*!---------------------------------------------------------------------
 |	flatten/2
 |	flatten(List, FlatList)
 |	flatten(+, -)
 |
 |	- flattens a nested list
 |
 |	If List is a list, some of whose elements may be nested lists,	
 |	FlatList is the flattened version of List obtained by traversing
 |	the tree defining List in depth-first, left-to-right order;
 |	compound structures other than list structures are not flattened.
 *!--------------------------------------------------------------------*/
flatten([], []) :-!.
flatten([Head | Tail], Result)
	:- !,
	flatten(Head, FlatHead),
	flatten(Tail, FlatTail),
	append(FlatHead, FlatTail, Result).
flatten(Item, [Item]).

/*!---------------------------------------------------------------------
 |	n_of/3
 |	n_of(N, Item, Result)
 |	n_of(+, +, -)
 |	
 |	- creates a list of N copies of an item	
 |	
 |	Result is a list of length N all of whose elements are the entity Item.
 *!--------------------------------------------------------------------*/
n_of(0, _, []) :-!.
n_of(N, Item, [Item | RestItems])
   :-
   M is N -1,
   n_of(M, Item, RestItems).

/*!---------------------------------------------------------------------
 |	is_length/2
 |	is_length(List, N)
 |	is_length(?, ?)
 |	
 |	- invertible length predicate
 |	
 |	List is of length N - works in both directions in the sense
 |	that either List or N can be uninstantiated with the other
 |	variable of correct type, and is_length(List, N) will succeed.
 *!--------------------------------------------------------------------*/
is_length([], 0) :-!.
is_length([_ | Tail], N)
	:-
	integer(N),
	N > 0,
	!,
	N1 is N - 1,
	is_length(Tail, N1).

is_length([_ | Tail], N)
	:-
	var(N),
	is_length(Tail, N1),
	N is N1 + 1.

/*!---------------------------------------------------------------------
 |	number_list/2
 |	number_list(List, NumberedList)
 |	number_list(+, -)
 |	
 |	- creates a numbered list from a source list
 |	
 |	If List is a list, NumberedList is a list of terms of the form
 |	N-Item, where the Item components are simply the elements of List
 |	in order, and N is a integer, sequentially numbering the elements
 |	of List, beginning with 1.
 *!--------------------------------------------------------------------*/
number_list(List, NumberedList)
	:-
	number_list(List, 1, NumberedList).

/*!---------------------------------------------------------------------
 |	number_list/3
 |	number_list(Items, StartNum, NumberedItems)
 |	number_list(+, +, -)
 |
 |	 - numbers the elements of a list
 |
 |	If Items is a list, and StartNum is an integer, NumberedItems is
 |	the list obtained by replacing each element X in Items by N-X,
 |	where N is the number of the position of X in Items; i.e., the
 |	list is numbered beginning with StartNum.
 *!--------------------------------------------------------------------*/
number_list([], _, []).
number_list([Item | Items], CurNum, [CurNum-Item | NumberedItems])
	:-
	NextNum is CurNum+1,
	number_list(Items, NextNum, NumberedItems).

/*!---------------------------------------------------------------------
 |	encode_list/3
 |	encode_list(Items, Codes, CodedItems)
 |	encode_list(+, +, -)
 |
 |	 - encodes the elements of a list with provided codes
 |
 |	If Items is a list, and Codes is a list of atoms with 
 |	length(Items) = length(Codes), then CodedItems is
 |	the list obtained by replacing each element X in Items by C-X,
 |	where C is the element of Codes in the position of X in Items.
 *!--------------------------------------------------------------------*/
encode_list([], [], []).
encode_list([Item | Items], [CurCode | RestCodes], [CurCode-Item | CodedItems])
	:-
	encode_list(Items, RestCodes, CodedItems).

/*!---------------------------------------------------------------------
 |	struct_lookup_subst/4
 |	struct_lookup_subst(OrderedTags, DefArgs, ArgSpecs, ArgsList)
 |	struct_lookup_subst(+, +, +, -)
 |
 |	 - performs substs for structs package constructors
 |
 |	OrderedTags and DefArgs are lists of the same length.
 |	The output ArgsList will be the same length as OrderedTags.
 |	ArgSpecs is a list of equations of the form<br>
 |		Tag = Value<br>
 |	where each of the Tags in such an equation must be on the list
 |	OrderedTags (but not all OrderedTags elements must occur on ArgSpecs);
 |	in fact, ArgSpecs can be empty.  The elements X of ArgsList are defined
 |	as follows:  if X corresponds to Tag on OrderedTags, then:<br>
 |	if Tag=Val occurs on ArgSpecs, X is Val;<br> 
 |	otherwise, X is the element of DefArgs corresponding to Tag.
 *!--------------------------------------------------------------------*/

struct_lookup_subst([], [], _, []).
struct_lookup_subst([Tag | OrderedTags], [DefVal | DefArgs],
					 ArgSpecs, [Arg | ArgsList])

	:-
	(dmember(Tag=Arg, ArgSpecs) ->
		true
		;
		Arg = DefVal
	),
	struct_lookup_subst(OrderedTags, DefArgs, ArgSpecs, ArgsList).

/*!------------------------------------------------------
 |  check_default/4
 |  check_default(PList, Tag, Default, Value)
 |  check_default(+, +, +, -)
 |
 |  - looks up an equation on a list, with a default
 |
 |  PList is a list of equations of the form<br>
 |
 |      tag = value<br>
 |
 |  check_default(PList, Tag, Default, Value) succeeds if:<br>
 |  i)	 Tag=Value belongs to PList; or,<br>
 |  ii)	 if Default is of the form Value^DC and call(DC) succeeds, or<br>
 |  iii) if Default=Value<br>
 |  iv)  in cases ii,iii) it is assumed that there is at
 |       most one equation on PList with left side Tag
 *!-----------------------------------------------------*/
check_default(PList, Tag, Default, Value)
    :-
    dmember(Tag=Value, PList), !.
check_default(PList, Tag, Value^DefaultCall, Value)
	:-!,
	call(DefaultCall).
check_default(PList, Tag, Default, Default).

/*!------------------------------------------------------
 |  check_default_del/5
 |  check_default_del(PList,Tag,Default,Value,ReducedPList)
 |  check_default_del(+,+,+,+,-)
 |
 |  - looks up a tagged equation on a List, and deletes it
 |
 |  PList is a list of equations of the form<br>
 |
 |      tag = value<br>
 |
 |  check_default_del(PList, Tag, Default, Value,ReducedPList) 
 |  succeeds if:<br>
 |  i)	 Tag=Value does not belong to PList; or,<br>
 |  ii)	 Tag=Value belongs to PList, and the difference
 |	 between PList and ReducedPList is the removal
 |	 of the Tag=Value equation.<br>
 |  iii) it is assumed that there is at most one equation on 
 |       PList with left side Tag
 *!-----------------------------------------------------*/
check_default_del([],Tag,Default,Default,[]).
check_default_del([Tag=Value | Tail],Tag,Default,Value,Tail)
    :-!.
check_default_del([OTag=OVal | RestPList],Tag,Default,Value,
                    [OTag=OVal | ReducedPList])
    :-
    check_default_del(RestPList,Tag,Default,Value,ReducedPList).

/*!------------------------------------------------------
 |	remove_tagged/3
 |	remove_tagged(EqnList, TagsToRemove, ReducedEqnList)
 |	remove_tagged(+, +, -).
 |
 |	- removes tagged equations from a list
 |
 |	EqnList is a list of equations of the form<br>
 |
 |      tag = value<br>
 |
 |	and TagsToRemove is a list of atoms which are candidates
 |	to occur as tags in these equations.  ReducedEqnList is
 |	the result of removing all equations beginning with a 
 |	tag from TagsToRemove from the list EqnList.
 *!-----------------------------------------------------*/
remove_tagged([], _, []).
remove_tagged([Tag=Value | PList], TagsToRemove, RedPList)
    :-
    dmember(Tag, TagsToRemove),
    !,
    remove_tagged(PList, TagsToRemove, RedPList).
remove_tagged([Tag=Value | PList], TagsToRemove, [Tag = Value | RedPList])
    :-
    remove_tagged(PList, TagsToRemove, RedPList).

/*!------------------------------------------------------
 |	merge_plists/3
 |	merge_plists(LeftEqnList, RightEqnList, MergedLists)
 |	merge_plists(+, +, -).
 |
 |	- recursively merges two tagged equation lists
 |
 |	LeftEqnList and RightEqnList are lists of equations of the form<br>
 |
 |      tag = value<br>
 |
 |	MergedLists consists of all equations occurring in either
 |	LeftEqnList or RightEqnList, where if the equations<br>
 |	
 |		Tag=LVal    and Tag = RVal<br>
 |
 |	occur in LeftEqnList and RightEqnList, respectively, 
 |	MergedLists will contain the equation<br>
 |
 |		Tag = MVal<br>
 |
 |	where:<br>
 |	a)	If both of LVal and RVal are lists, then MVal is obtained by
 |		recursively calling merge_plists(LVal, RVal, MVal), or if
 |		that fails, checking that they are identical;<br>
 |	b)	Otherwise, MVal is LVal.
 *!-----------------------------------------------------*/
merge_plists(Left, [], Left) :-!.
merge_plists([], Right, Right) :-!.

merge_plists([Tag=LVal | RestLeft], Right, [Tag=MergeVal | RestResult])
    :-
    list_delete(Right, Tag=RVal, RestRight),
    ((nonvar(RVal), RVal = [_|_], nonvar(LVal), LVal = [_|_]) ->
        (merge_plists(LVal, RVal, MergeVal) ->
			true
			;
			LVal = RVal, MergeVal = LVal
		)
        ;
        MergeVal = LVal
    ),
    merge_plists(RestLeft, RestRight, RestResult).

/*!------------------------------------------------------
 |	merge_tagged_lists/3
 |	merge_tagged_lists(LeftEqnList, RightEqnList, MergedLists)
 |	merge_tagged_lists(+, +, -).
 |
 |	- recursively merges two tagged equation lists
 |
 |	LeftEqnList and RightEqnList are lists of equations of the form<br>
 |
 |      tag = value<br>
 |
 |	MergedLists consists of all equations occurring in either
 |	LeftEqnList or RightEqnList, where if the equations<br>
 |	
 |		Tag=LVal    and Tag = RVal<br>
 |
 |	occur in LeftEqnList and RightEqnList, respectively, 
 |	MergedLists will contain the equation<br>
 |
 |		Tag = LVal<br>
 *!-----------------------------------------------------*/
merge_tagged_lists(Left, [], Left) :-!.
merge_tagged_lists([], Right, Right) :-!.

merge_tagged_lists([Tag=LVal | RestLeft], Right, [Tag=LVal | RestResult])
    :-
    list_delete(Right, Tag=RVal, RestRight),
    merge_tagged_lists(RestLeft, RestRight, RestResult).

/*!------------------------------------------------------
 |	mangle_change_tagged/3.
 |	mangle_change_tagged(PList,Tag,NewValue)
 |	mangle_change_tagged(+,+,+)
 |
 |	- destructively changes the value of a tagged eqn
 |
 |	If Plist is a list of tagged equations, Tag is a tag, 
 |	and NewValue is an arbitrary prolog term, then:<br>
 |	i)	If an equation Tag=OldVal occurs on PList,
 |		destructively alters that eqn to become Tag=NewValue;<br>
 |	ii)	Has no effect otherwise.
 *!-----------------------------------------------------*/
mangle_change_tagged([],_,_).
mangle_change_tagged([TagEqn | WkngPList],Tag,NewValue)
    :-
    arg(1,TagEqn,Tag),
    !,
    mangle(2,TagEqn,NewValue).
mangle_change_tagged([_ | WkngPList],Tag,NewValue)
    :-
    mangle_change_tagged(WkngPList,Tag,NewValue).

/*!------------------------------------------------------
 |	subst_tagged/4.
 |	subst_tagged(PList,Tag,NewValue,NewPList)
 |	subst_tagged(+,+,+,-)
 |
 |	- NON-destructively changes the value of a tagged eqn
 |
 |	If Plist is a list of tagged equations, Tag is a tag, 
 |	and NewValue is an arbitrary prolog term, then:<br>
 |	i)	If an equation Tag=OldVal occurs on PList,
 |		then NewPList is the result of altering that eqn 
 |		to become Tag=NewValue;<br>
 |	ii)	Otherwise, NewPList = OldPList.
 *!-----------------------------------------------------*/
subst_tagged([],_,_, []).
subst_tagged([Tag=_ | PListTail],Tag,NewValue, [Tag=NewValue |PListTail])
    :-!.
subst_tagged([Eqn | PListTail],Tag,NewValue, [Eqn | NewPListTail])
    :-
    subst_tagged(PListTail,Tag,NewValue, NewPListTail).

/*!------------------------------------------------------
 |	merge_in_list/3
 |	merge_in_list(Left, Right, Result)
 |	merge_in_list(+, +, -)
 |
 |	- merges two list together
 |
 |	If Left and Right are lists, list Result consists of those
 |	elements of Left (in order, with duplicates preserved)
 |	which do not occur on Right, followed by list Right.
 *!-----------------------------------------------------*/
merge_in_list([], Right, Right).
merge_in_list([Item | Left], Right, Result)
	:-
	dmember(Item, Right),
	!,
	merge_in_list(Left, Right, Result).
merge_in_list([Item | Left], Right, [Item | Result])
	:-
	merge_in_list(Left, Right, Result).

/*!---------------------------------------------------------------------
 |	insert_item_in_list/3
 |	insert_item_in_list(In_List, Item, Out_List)
 |	insert_item_in_list(+, +, -).
 |
 | 	- interleaves an arbitrary term  between elements of a list
 |
 |	If In_List is a list of arbitrary terms, and Item is a term,
 |	then Out_List is that list obtained from In_List by interleaving
 |	Item between each pair of items of In_List.
 *!--------------------------------------------------------------------*/
insert_item_in_list([], _, []).
insert_item_in_list([Element], Item, [Element])
	:-!.
insert_item_in_list([Element | RestIn_List], Item, [Element, Item | RestOut_List])
	:-
	insert_item_in_list(RestIn_List, Item, RestOut_List).

/*!---------------------------------------------------------------------
 |	insert_spaces/2
 |	insert_spaces(In_List, Out_List)
 |	insert_spaces(+, -).
 |
 | 	- interleaves a quoted blank between elements of a list
 |
 |	If In_List is a list of arbitrary terms, then Out_List is that 
 |	list obtained from In_List by interleaving ' '  between each 
 |	pair of items of In_List.
 *!--------------------------------------------------------------------*/
insert_spaces(In_List, Out_List)
	:-
	insert_item_in_list(In_List, ' ', Out_List).

endmod.
