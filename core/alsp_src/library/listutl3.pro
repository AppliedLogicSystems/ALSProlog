/*======================================================================
 |			listutl3.pro
 |		Copyright (c) 1991 Applied Logic Systems, Inc.
 |
 |		Miscellaneous list predicates
 *=====================================================================*/
%! category(terms).
%! group(list).
%! subgroup(miscellaneous).

module builtins.

export nobind_member/2.
export output_prolog_list/1.
export output_prolog_list/5.
export flatten/2.
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

/*!---------------------------------------------------------------------
 |	nobind_member/2
 |	nobind_member(X, List)
 |	nobind_member(+, +)
 |
 |	- tests list membership without binding any variables
 |
 |	nobind_member(X, List) holds and only if X is a member of List;
 |	if the test is successful, no variables in either input are bound.
 *!--------------------------------------------------------------------*/
nobind_member(X, [Y | _])
	:-
	not(not(X=Y)),!.
nobind_member(X, [_ | T])
	:-
	nobind_member(X, T).

/*!---------------------------------------------------------------------
 |	output_prolog_list/1
 |	output_prolog_list(List)
 |	output_prolog_list(+)
 |
 |	- outputs items on a list, one to a line
 |
 |	Outputs (to the current output stream) each item on List, one item
 |	to a line, followed by a period.
 *!--------------------------------------------------------------------*/
output_prolog_list(List)
	:-
	current_output(Stream),
	output_prolog_list(Stream, List,'','.',' - ').

output_prolog_list(List,Indent,Term,Spacer)
	:-
	current_output(Stream),
	output_prolog_list(Stream,List,Indent,Term,Spacer).

output_prolog_list(Stream,[],Indent,Term,Spacer).
output_prolog_list(Stream,[Item | RestList],Indent,Term,Spacer)
	:-
	(Item = (Code-Content) ->
		printf(Stream,"%t%t%t%t%t\n",[Indent,Code,Spacer,Content,Term])
		;
		printf(Stream,"%t%t%t\n",[Indent,Item,Term])
	),
	output_prolog_list(Stream,RestList,Indent,Term,Spacer).

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
flatten([], []).
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
 |	Result is a list of length N all of whose elements are the entity
 |	Item.
 *!--------------------------------------------------------------------*/
n_of(0, _, []) :-!.
n_of(N, Item, [Item | RestItems])
   :-
   M is N -1,
   n_of(M, Item, RestItems).

/*!---------------------------------------------------------------------
 |	number_list/2
 |	number_list(List, NumberedList)
 |	number_list(+, -)
 |	
 |	- creates a numbered list from a source list
 |	
 |	If List is a list, NumberedList is a list of terms of the form
 |	N-Item, where the Item components are simply the elements of List
 |	in order, and N is a integer, sequentially numbered the elements
 |	of List.
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
 |	where N is the number of the position of X in Items.
 *!--------------------------------------------------------------------*/
number_list([], _, []).
number_list([Item | Items], CurNum, [CurNum-Item | NumberedItems])
	:-
	NextNum is CurNum+1,
	number_list(Items, NextNum, NumberedItems).

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
 |	OrderedTags and DefArgs are lists of the same length; so will be ArgsList.
 |	ArgSpecs is a list of equations of the form
 |		Tag = Value
 |	where each of the Tags in such an equation must be on the list
 |	OrderedTags (but not all OrderedTags elements must occur on ArgSpecs);
 |	in fact, ArgSpecs can be empty.  The elements X of ArgsList are defined
 |	as follows:  if X corresponds to Tag on OrderedTags, then: if Tag=Val
 |	occurs on ArgSpecs, X is Val; otherwise, X is the element of DefArgs
 |	corresponding to Tag.
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
 |  check_default/4.
 |  check_default(PList, Tag, Default, Value)
 |  check_default(+, +, +, -)
 |
 |  - looks up an equation on a list, with default
 |
 |  PList is a list of equations of the form
 |
 |      tag = value
 |
 |  check_default(PList, Tag, Default, Value) succeeds if:
 |  i)	 Tag=Value belongs to PList; or,
 |  ii)	 if Default is of the form Value^DC and call(DC) succeeds, or
 |  iii) if Default=Value
 *!-----------------------------------------------------*/
check_default(PList, Tag, Default, Value)
    :-
    dmember(Tag=Value, PList), !.
check_default(PList, Tag, Value^DefaultCall, Value)
	:-!,
	call(DefaultCall).
check_default(PList, Tag, Default, Default).

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
 |	-	removes tagged equations from a list
 |
 |	EqnList is a list of equations of the form
 |
 |      tag = value
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
 |	-	(recursively) merges two tagged equation lists
 |
 |	LeftEqnList and RightEqnList are lists of equations of the form
 |
 |      tag = value
 |
 |	MergedLists consists of all equations occurring in either
 |	LeftEqnList or RightEqnList, where if the equations
 |	
 |		Tag=LVal    and Tag = RVal
 |
 |	occur in LeftEqnList and RightEqnList, respectively, 
 |	MergedLists will contain the equation
 |
 |		Tag = MVal
 |
 |	where:
 |	a)	If both of LVal and RVal are lists, then MVal is obtained by
 |		recursively calling merge_plists(LVal, RVal, MVal), or if
		that fails, checking that they are identical;
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
 |	mangle_change_tagged/3.
 |	mangle_change_tagged(PList,Tag,NewValue)
 |	mangle_change_tagged(+,+,+)
 |
 |	-- destructively changes the value of a tagged eqn
 |
 |	If Plist is a list of tagged equations, Tag is a tag, 
 |	and NewValue is an arbitrary prolog term, then:
 |	i)	If an equation Tag=OldVal occurs on PList,
 |		destructively alters that eqn to become Tag=NewValue;
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
 |	-- NON-destructively changes the value of a tagged eqn
 |
 |	If Plist is a list of tagged equations, Tag is a tag, 
 |	and NewValue is an arbitrary prolog term, then:
 |	i)	If an equation Tag=OldVal occurs on PList,
 |		then NewPList is the result of altering that eqn 
 |		to become Tag=NewValue;
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

endmod.
