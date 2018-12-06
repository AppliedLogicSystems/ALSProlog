---
title: 'number_list/3'
package: alslib
group: Lists
predicates:
- {sig: 'check_default/4', desc: 'looks up an equation on a list, with a default'}
- {sig: 'check_default_del/5', desc: 'looks up a tagged equation on a List, and deletes it'}
- {sig: 'encode_list/3', desc: 'encodes the elements of a list with provided codes'}
- {sig: 'flatten/2', desc: 'flattens a nested list'}
- {sig: 'is_length/2', desc: 'invertible length predicate'}
- {sig: 'mangle_change_tagged/3', desc: 'destructively changes the value of a tagged eqn'}
- {sig: 'merge_in_list/3', desc: 'merges two list together'}
- {sig: 'merge_plists/3', desc: 'recursively merges two tagged equation lists'}
- {sig: 'merge_tagged_lists/3', desc: 'recursively merges two tagged equation lists'}
- {sig: 'n_of/3', desc: 'creates a list of N copies of an item	'}
- {sig: 'nobind_member/2', desc: 'tests list membership without binding any variables'}
- {sig: 'number_list/2', desc: 'creates a numbered list from a source list'}
- {sig: 'number_list/3', desc: 'numbers the elements of a list'}
- {sig: 'remove_tagged/3', desc: 'removes tagged equations from a list'}
- {sig: 'struct_lookup_subst/4', desc: 'performs substs for structs package constructors'}
- {sig: 'subst_tagged/4', desc: 'NON-destructively changes the value of a tagged eqn'}
---
## FORMS

`check_default(PList, Tag, Default, Value)`

`check_default_del(PList,Tag,Default,Value,ReducedPList)`

`encode_list(Items, Codes, CodedItems)`

`flatten(List, FlatList)`

`is_length(List, N)`

`mangle_change_tagged(PList,Tag,NewValue)`

`merge_in_list(Left, Right, Result)`

`merge_plists(LeftEqnList, RightEqnList, MergedLists)`

`merge_tagged_lists(LeftEqnList, RightEqnList, MergedLists)`

`n_of(N, Item, Result)`

`nobind_member(X, List)`

`number_list(List, NumberedList)`

`number_list(Items, StartNum, NumberedItems)`

`remove_tagged(EqnList, TagsToRemove, ReducedEqnList)`

`struct_lookup_subst(OrderedTags, DefArgs, ArgSpecs, ArgsList)`

`subst_tagged(PList,Tag,NewValue,NewPList)`

## DESCRIPTION

**`check_default/4`** PList is a list of equations of the form  
    tag = value  
    check_default(PList, Tag, Default, Value) succeeds if:  
    i)	 Tag=Value belongs to PList; or,  
    ii)	 if Default is of the form Value^DC and call(DC) succeeds, or  
    iii) if Default=Value  
    iv)  in cases ii,iii) it is assumed that there is at  
    most one equation on PList with left side Tag  

**`check_default_del/5`** PList is a list of equations of the form  
    tag = value  
    check_default_del(PList, Tag, Default, Value,ReducedPList)  
    succeeds if:  
    i)	 Tag=Value does not belong to PList; or,  
    ii)	 Tag=Value belongs to PList, and the difference  
    between PList and ReducedPList is the removal  
    of the Tag=Value equation.  
    iii) it is assumed that there is at most one equation on  
    PList with left side Tag  

**`encode_list/3`** If Items is a list, and Codes is a list of atoms with  
    length(Items) = length(Codes), then CodedItems is  
    the list obtained by replacing each element X in Items by C-X,  
    where C is the element of Codes in the position of X in Items.  

**`flatten/2`** If List is a list, some of whose elements may be nested lists,  
    FlatList is the flattened version of List obtained by traversing  
    the tree defining List in depth-first, left-to-right order;  
    compound structures other than list structures are not flattened.  

**`is_length/2`** List is of length N - works in both directions in the sense  
    that either List or N can be uninstantiated with the other  
    variable of correct type, and is_length(List, N) will succeed.  

**`mangle_change_tagged/3`** If Plist is a list of tagged equations, Tag is a tag,  
    and NewValue is an arbitrary prolog term, then:  
    i)	If an equation Tag=OldVal occurs on PList,  
    destructively alters that eqn to become Tag=NewValue;  
    ii)	Has no effect otherwise.  

**`merge_in_list/3`** If Left and Right are lists, list Result consists of those  
    elements of Left (in order, with duplicates preserved)  
    which do not occur on Right, followed by list Right.  

**`merge_plists/3`** LeftEqnList and RightEqnList are lists of equations of the form  
    tag = value  
    MergedLists consists of all equations occurring in either  
    LeftEqnList or RightEqnList, where if the equations  
    Tag=LVal    and Tag = RVal  
    occur in LeftEqnList and RightEqnList, respectively,  
    MergedLists will contain the equation  
    Tag = MVal  
    where:  
    a)	If both of LVal and RVal are lists, then MVal is obtained by  
    recursively calling merge_plists(LVal, RVal, MVal), or if  
    that fails, checking that they are identical;  
    b)	Otherwise, MVal is LVal.  

**`merge_tagged_lists/3`** LeftEqnList and RightEqnList are lists of equations of the form  
    tag = value  
    MergedLists consists of all equations occurring in either  
    LeftEqnList or RightEqnList, where if the equations  
    Tag=LVal    and Tag = RVal  
    occur in LeftEqnList and RightEqnList, respectively,  
    MergedLists will contain the equation  
    Tag = LVal  

**`n_of/3`** Result is a list of length N all of whose elements are the entity Item.  

**`nobind_member/2`** Logically, nobind_member(X, List) holds and only if X is a member  
    of List; if the test is successful, no variables in either input are bound.  

**`number_list/2`** If List is a list, NumberedList is a list of terms of the form  
    N-Item, where the Item components are simply the elements of List  
    in order, and N is a integer, sequentially numbering the elements  
    of List, beginning with 1.  

**`number_list/3`** If Items is a list, and StartNum is an integer, NumberedItems is  
    the list obtained by replacing each element X in Items by N-X,  
    where N is the number of the position of X in Items; i.e., the  
    list is numbered beginning with StartNum.  

**`remove_tagged/3`** EqnList is a list of equations of the form  
    tag = value  
    and TagsToRemove is a list of atoms which are candidates  
    to occur as tags in these equations.  ReducedEqnList is  
    the result of removing all equations beginning with a  
    tag from TagsToRemove from the list EqnList.  

**`struct_lookup_subst/4`** OrderedTags and DefArgs are lists of the same length.  
    The output ArgsList will be the same length as OrderedTags.  
    ArgSpecs is a list of equations of the form  
    Tag = Value  
    where each of the Tags in such an equation must be on the list  
    OrderedTags (but not all OrderedTags elements must occur on ArgSpecs);  
    in fact, ArgSpecs can be empty.  The elements X of ArgsList are defined  
    as follows:  if X corresponds to Tag on OrderedTags, then:  
    if Tag=Val occurs on ArgSpecs, X is Val;  
    otherwise, X is the element of DefArgs corresponding to Tag.  

**`subst_tagged/4`** If Plist is a list of tagged equations, Tag is a tag,  
    and NewValue is an arbitrary prolog term, then:  
    i)	If an equation Tag=OldVal occurs on PList,  
    then NewPList is the result of altering that eqn  
    to become Tag=NewValue;  
    ii)	Otherwise, NewPList = OldPList.  

## EXAMPLES

