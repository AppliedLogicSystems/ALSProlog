---
title: 'number_list/3'
package: ALS Library
group: Lists
predicates:
- {sig: 'nobind_member/2', desc: 'tests list membership without binding any variables'}
- {sig: 'flatten/2', desc: 'flattens a nested list'}
- {sig: 'n_of/3', desc: 'creates a list of N copies of an item	'}
- {sig: 'is_length/2', desc: 'invertible length predicate'}
- {sig: 'number_list', args: {
    2: 'creates a numbered list from a source list',
    3: 'numbers the elements of a list'
  }}
- {sig: 'encode_list/3', desc: 'encodes the elements of a list with provided codes'}
- {sig: 'struct_lookup_subst/4', desc: 'performs substs for structs package constructors'}
- {sig: 'check_default/4', desc: 'looks up an equation on a list, with a default'}
- {sig: 'check_default_del/5', desc: 'looks up a tagged equation on a List, and deletes it'}
- {sig: 'remove_tagged/3', desc: 'removes tagged equations from a list'}
- {sig: 'merge_plists/3', desc: 'recursively merges two tagged equation lists'}
- {sig: 'merge_tagged_lists/3', desc: 'recursively merges two tagged equation lists'}
- {sig: 'mangle_change_tagged/3', desc: 'destructively changes the value of a tagged eqn'}
- {sig: 'subst_tagged/4', desc: 'NON-destructively changes the value of a tagged eqn'}
- {sig: 'merge_in_list/3', desc: 'merges two list together'}
- {sig: 'insert_item_in_list/3', desc: 'interleaves an arbitrary term  between elements of a list'}
- {sig: 'insert_spaces/2', desc: 'interleaves a quoted blank between elements of a list'}
---
## FORMS

`nobind_member(X, List)`

`flatten(List, FlatList)`

`n_of(N, Item, Result)`

`is_length(List, N)`

`number_list(List, NumberedList)`

`number_list(Items, StartNum, NumberedItems)`

`encode_list(Items, Codes, CodedItems)`

`struct_lookup_subst(OrderedTags, DefArgs, ArgSpecs, ArgsList)`

`check_default(PList, Tag, Default, Value)`

`check_default_del(PList,Tag,Default,Value,ReducedPList)`

`remove_tagged(EqnList, TagsToRemove, ReducedEqnList)`

`merge_plists(LeftEqnList, RightEqnList, MergedLists)`

`merge_tagged_lists(LeftEqnList, RightEqnList, MergedLists)`

`mangle_change_tagged(PList,Tag,NewValue)`

`subst_tagged(PList,Tag,NewValue,NewPList)`

`merge_in_list(Left, Right, Result)`

`insert_item_in_list(In_List, Item, Out_List)`

`insert_spaces(In_List, Out_List)`

## DESCRIPTION

**`nobind_member/2`** Logically, nobind_member(X, List) holds and only if X is a member
    of List; if the test is successful, no variables in either input are bound.

**`flatten/2`** If List is a list, some of whose elements may be nested lists,
    FlatList is the flattened version of List obtained by traversing
    the tree defining List in depth-first, left-to-right order;
    compound structures other than list structures are not flattened.

**`n_of/3`** Result is a list of length N all of whose elements are the entity Item.

**`is_length/2`** List is of length N - works in both directions in the sense
    that either List or N can be uninstantiated with the other
    variable of correct type, and is_length(List, N) will succeed.

**`number_list/2`** If List is a list, NumberedList is a list of terms of the form
    N-Item, where the Item components are simply the elements of List
    in order, and N is a integer, sequentially numbering the elements
    of List, beginning with 1.

**`number_list/3`** If Items is a list, and StartNum is an integer, NumberedItems is
    the list obtained by replacing each element X in Items by N-X,
    where N is the number of the position of X in Items; i.e., the
    list is numbered beginning with StartNum.

**`encode_list/3`** If Items is a list, and Codes is a list of atoms with
    length(Items) = length(Codes), then CodedItems is
    the list obtained by replacing each element X in Items by C-X,
    where C is the element of Codes in the position of X in Items.

**`struct_lookup_subst/4`** OrderedTags and DefArgs are lists of the same length.
    The output ArgsList will be the same length as OrderedTags.
    ArgSpecs is a list of equations of the form<br>
    Tag = Value<br>
    where each of the Tags in such an equation must be on the list
    OrderedTags (but not all OrderedTags elements must occur on ArgSpecs);
    in fact, ArgSpecs can be empty.  The elements X of ArgsList are defined
    as follows:  if X corresponds to Tag on OrderedTags, then:<br>
    if Tag=Val occurs on ArgSpecs, X is Val;<br>
    otherwise, X is the element of DefArgs corresponding to Tag.

**`check_default/4`** PList is a list of equations of the form<br>
    tag = value<br>
    check_default(PList, Tag, Default, Value) succeeds if:<br>
    i)	 Tag=Value belongs to PList; or,<br>
    ii)	 if Default is of the form Value^DC and call(DC) succeeds, or<br>
    iii) if Default=Value<br>
    iv)  in cases ii,iii) it is assumed that there is at
    most one equation on PList with left side Tag

**`check_default_del/5`** PList is a list of equations of the form<br>
    tag = value<br>
    check_default_del(PList, Tag, Default, Value,ReducedPList)
    succeeds if:<br>
    i)	 Tag=Value does not belong to PList; or,<br>
    ii)	 Tag=Value belongs to PList, and the difference
    between PList and ReducedPList is the removal
    of the Tag=Value equation.<br>
    iii) it is assumed that there is at most one equation on
    PList with left side Tag

**`remove_tagged/3`** EqnList is a list of equations of the form<br>
    tag = value<br>
    and TagsToRemove is a list of atoms which are candidates
    to occur as tags in these equations.  ReducedEqnList is
    the result of removing all equations beginning with a
    tag from TagsToRemove from the list EqnList.

**`merge_plists/3`** LeftEqnList and RightEqnList are lists of equations of the form<br>
    tag = value<br>
    MergedLists consists of all equations occurring in either
    LeftEqnList or RightEqnList, where if the equations<br>
    Tag=LVal    and Tag = RVal<br>
    occur in LeftEqnList and RightEqnList, respectively,
    MergedLists will contain the equation<br>
    Tag = MVal<br>
    where:<br>
    a)	If both of LVal and RVal are lists, then MVal is obtained by
    recursively calling merge_plists(LVal, RVal, MVal), or if
    that fails, checking that they are identical;<br>
    b)	Otherwise, MVal is LVal.

**`merge_tagged_lists/3`** LeftEqnList and RightEqnList are lists of equations of the form<br>
    tag = value<br>
    MergedLists consists of all equations occurring in either
    LeftEqnList or RightEqnList, where if the equations<br>
    Tag=LVal    and Tag = RVal<br>
    occur in LeftEqnList and RightEqnList, respectively,
    MergedLists will contain the equation<br>
    Tag = LVal<br>

**`mangle_change_tagged/3`** If Plist is a list of tagged equations, Tag is a tag,
    and NewValue is an arbitrary prolog term, then:<br>
    i)	If an equation Tag=OldVal occurs on PList,
    destructively alters that eqn to become Tag=NewValue;<br>
    ii)	Has no effect otherwise.

**`subst_tagged/4`** If Plist is a list of tagged equations, Tag is a tag,
    and NewValue is an arbitrary prolog term, then:<br>
    i)	If an equation Tag=OldVal occurs on PList,
    then NewPList is the result of altering that eqn
    to become Tag=NewValue;<br>
    ii)	Otherwise, NewPList = OldPList.

**`merge_in_list/3`** If Left and Right are lists, list Result consists of those
    elements of Left (in order, with duplicates preserved)
    which do not occur on Right, followed by list Right.

**`insert_item_in_list/3`** If In_List is a list of arbitrary terms, and Item is a term,
    then Out_List is that list obtained from In_List by interleaving
    Item between each pair of items of In_List.

**`insert_spaces/2`** If In_List is a list of arbitrary terms, then Out_List is that
    list obtained from In_List by interleaving ' '  between each
    pair of items of In_List.

## EXAMPLES

