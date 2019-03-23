---
title: 'nth_tail/4'
package: ALS Library
group: Lists
module: builtins
predicates:
- {sig: 'at_most_n/3', desc: 'returns initial segment of list of length =< N'}
- {sig: 'change_nth/3', desc: 'destructively changes the Nth element of a list'}
- {sig: 'deleteNth/3', desc: 'deletes the Nth element of a list'}
- {sig: 'delete_1st/3', desc: 'deletes the left-most entry of Item in List'}
- {sig: 'get_list_tail/3', desc: 'returns the tail of a list determined by an element'}
- {sig: 'last/2', desc: 'returns last element of a list'}
- {sig: 'nonmember/2', desc: 'tests for the failure of membership'}
- {sig: 'nth_tail/4', desc: 'returns the nth head and tail of a list'}
- {sig: 'sublist/4', desc: 'extracts a sublist from a list'}
- {sig: 'subst_nth/4', desc: 'non-destructively changes the Nth element of a list'}
---
## FORMS

`at_most_n(List, N, Head)`

`change_nth(N, List, NewItem)`

`deleteNth(N, List, Remainder)`

`delete_1st(List, Item, Result)`

`get_list_tail(List, Item, Tail)`

`last(List, Item)`

`nonmember(List, Item)`

`nth_tail(N, List, Head, Tail)`

`sublist(List,Start,Length,Result)`

`subst_nth(N, List, NewItem, NewList)`

## DESCRIPTION

**`at_most_n/3`** If List is a list and N is a non-negative integer, Head is the  
    longest initial segment of List with length =< N.  

**`change_nth/3`** If N is a non-negative integer, List is list, and NewItem is any  
    non-var object, destructively changes the Nth element of List to become  
    NewItem. This predicate numbers the list beginning with 0.  

**`deleteNth/3`** If N is a non-negative integer and List is a list, then Remainder  
    is the result of deleting the Nth element of List.  
    This predicate numbers the list beginning with 1.  

**`delete_1st/3`** If Item occurs on List, deletes the left-most entry of Item on  
    List, returning in Result the tail of List beginning at that entry.  
    Fails if Item is not on List.  

**`get_list_tail/3`** If List is a list and Item is any object, Tail is the portion  
    of List extending from the leftmost occurrence of Item in List  
    to the end of List. Fails if Item does not belong to List.  

**`last/2`** If List is a non-empty list, Item is the last (right-most) element  
    of List.  

**`nonmember/2`** Succeeds iff member(List, Item) fails  

**`nth_tail/4`** If List is a list and N is a non-negative integer, then Head is  
    the portion of List up to and including the Nth element,  
    and tail is the portion of List from the Nth element to the end.  

**`sublist/4`** If List is an arbitrary list, Result is the sublist of  
    length Length, beginning at position Start in List.  

**`subst_nth/4`** If N is a non-negative integer, List is list, and NewItem is any  
    non-var object, NewList is the result of non-destructively changing  
    the Nth element of List to become NewItem.  
    This predicate numbers the list beginning with 0.  

{% comment %}
## EXAMPLES
{% endcomment %}

