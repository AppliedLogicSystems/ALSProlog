---
title: 'columns/1'
package: ALS Library
group: Formatting
predicates:
- {sig: 'columns', args: {
    1: 'Output ListOfRows as a minimal columnar array of terms',
    2: 'Like columns/1, outputting to a given stream',
    3: 'Output ListOfRows as a columnized rectangular array of terms'
  }}
---
## FORMS

`columns(ListOfRows)`

`columns(ListOfRows, OutputStream)`

`columns(ListOfRows, ColWidths, OutputStream)`

## DESCRIPTION

**`columns/1`** If ListOfRows is a list of lists of terms (Rows), and  
    if all of the Rows are of the same length, outputs a  
    rectangular array of the terms appearing in the  
    Rows to user_output. The array is split into columns of minimal  
    size to contain the appropriate terms, with one character  wide  
    blank separators  

**`columns/2`** If ListOfRows is a list of lists of terms (Rows), and  
    if all of the Rows are of the same length, outputs a  
    rectangular array of the terms appearing in the  
    Rows to OutputStream. The array is split into columns of minimal  
    size to contain the appropriate terms, with one character  wide  
    blank separators.  
    Note: if arg(2) is passed a list of integers(ColWidths), then  
    columns(ListOfRows, ColsWidths user_output)  
    is invoked.  

**`columns/3`** If ListOfRows is a list of lists of terms (Rows), and  
    if all of the Rows are of the same length, and if ColWidths  
    is a list of positive integers of the same length as the  
    common length as the individual lists in ListOfRows, outputs a  
    rectangular array of the terms appearing in the Rows to  
    OutputStream. The array is split into columns of the size of  
    the corresponding integer of ColWidths, with one character-wide  
    blank separators.  Terms in Rows which are of size > the  
    corresponding integer of ColWidths are trucated.  

## EXAMPLES

**`columns/1`**
```

?- listing(silly_list).

% user:silly_list/1
silly_list(
[[abc,rjtir,x,y],
[yrtbv,h8t,x,yacc],
[34,f(4,5,6),some_silly(55,yurht,123),thru]]).

?- listing(t1).
% user:t1/0
t1 :- silly_list(ListOfRows), columns(ListOfRows).

?- t1.
abc    rjtir     x                         y     
yrtbv  h8t       x                         yacc  
34     f(4,5,6)  some_silly(55,yurht,123)  thru  

?- listing(t1h2).
% user:t1h2/0
t1h2 :- silly_list(ListOfRows), ListOfRows = [Header|Rest], 
columns([u(Header,0'_)|Rest]).

?- t1h2.
abc    rjtir     x                         y     
___    _____     _                         _     
yrtbv  h8t       x                         yacc  
34     f(4,5,6)  some_silly(55,yurht,123)  thru  	
```

**`columns/3`**
```

?- listing(t2).
% user:t2/0
t2 :- silly_list(ListOfRows), columns(ListOfRows,[5,5,5,5]).

?- t2.
abc   rjtir x     y     
yrtbv h8t   x     yacc  
34    f(4,5 some_ thru  

?- listing(t2h).
% user:t2h/0
t2h :-  silly_list(ListOfRows),
columns([u([apple,orange,banana,kiwi],0'=)|ListOfRows],[5,5,5,5]).

?- t2h.
apple orang banan kiwi  
===== ===== ===== ===== 
abc   rjtir x     y     
yrtbv h8t   x     yacc  
34    f(4,5 some_ thru  
```

