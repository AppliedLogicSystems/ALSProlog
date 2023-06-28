---
title: 'list_diff/3'
package: ALS Library
group: Lists
predicates:
- {sig: 'append/2', desc: 'appends a list of lists together'}
- {sig: 'list_diff/3', desc: 'returns the ordered difference of two lists'}
- {sig: 'list_diffs/4', desc: 'returns both ordered differences of two lists'}
- {sig: 'symmetric_diff/3', desc: 'returns the symmetric difference of two lists'}
- {sig: 'intersect', args: {
    2: 'returns the intersection of a list of lists',
    3: 'returns the intersection of two lists'
  }}
- {sig: 'int_diff/4', desc: 'returns the intersection and one difference of two lists'}
- {sig: 'union/3', desc: 'returns the ordered union of two lists'}
- {sig: 'sorted_merge', args: {
    2: 'returns the sorted union of a list of lists',
    3: 'returns the sorted union of two lists'
  }}
- {sig: 'subset/2', desc: 'determines if one list is a subset of another'}
- {sig: 'init_seg_list/2', desc: 'determines if one list is an initial segment of another'}
---
## FORMS

`append(ListOfLists, Result)`

`list_diff(A, B, A_NotB)`

`list_diffs(A,B,A_NotB,B_NotA)`

`symmetric_diff(A,B,A_symd_B)`

`intersect(A,B,AintB)`

`int_diff(A,B,AintB,AnotB)`

`intersect(L,IntsectL)`

`union(A,B, AuB)`

`sorted_merge(List1, List2, Union)`

`sorted_merge(ListOfLists, Union)`

`subset(LeftList, RightList)`

`init_seg_list(LeftList, RightList)`

## DESCRIPTION

**`append/2`** If ListOfLists if a list, each of whose elements is a list, Result
    is obtained by appending the members of ListOfLists together in order.

**`list_diff/3`** If A and B are lists, returns the difference A-B consisting of
    all items on A, but not on B.

**`list_diffs/4`** If A and B are lists, returns both the difference A-B together
    with the difference B-A.

**`symmetric_diff/3`** If A and B are lists, returns the symmetric difference of A and B,
    which is the union of A-B and B-A.

**`intersect/3`** If A and B are lists, returns the intersection AintB of A and B,
    which is the list of all items common to both lists, in order.

**`int_diff/4`** If A and B are lists, returns the intersection AintB of A and B,
    together with the difference A - B.

**`intersect/2`** If L is a list of lists, returns the intersection IntsectL of all
    of the lists appearing on L.

**`union/3`** If A and B are lists, returns the ordered union of A and B, consisting
    of all items occurring on either A or B, with all occurrences of items
    from A occurring before any items from B-A; equivalent to:<br>
    append(A,B-A,AuB);<br>
    If both lists have the property that each element occurs no more
    than once, then the union also has this property.

**`sorted_merge/3`** If List1 and List2 are lists of items, Union is
    the sorted merge (non-repetitive union) of List1 and List2.

**`sorted_merge/2`** If ListOfLists is a list of lists, Union is the sorted merge
    (non-repetitive union) of the members of ListsOfLists.

**`subset/2`** If LeftList and RightList are both lists, this predicate is
    true if and only if every element of LeftList is also an
    element of RightList

**`init_seg_list/2`** If LeftList and RightList are both lists, this predicate is
    true if and only if LeftList is an initial sublist of  RightList.

## EXAMPLES

