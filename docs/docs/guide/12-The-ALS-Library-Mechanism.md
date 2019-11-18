---
---

# 12 The ALS Library Mechanism
{:.no_toc}

* TOC
{:toc}

The ALS Library mechanism provides a sophisticated device for managing large libraries of code in an efficient and flexible manner. Many files of potentially useful code can be available to a program without the cost of loading these files at the time the program is initially loaded. Only if program execution leads to a need for code from a particular library file is that file in fact loaded. Thereafter, execution proceeds as if the file had already been loaded.. The library mechanism is essentially invisibile to the programmer, except for a possible momentary pause when a particular group of library predicates is first loaded. Consequently, the line between the predicates which are called 'builtin' and those which are called 'library' is quite gray.
The files making up the library reside in the folder ...alsdir/library/.

## 12.1 Overview of the ALS Library Mechanism and Tools

Normally, the units making up the library are various sized (small to large) files
containing code defining certain useful predicates, or defining whole subsystems of
a large program.. Some of the predicates in such a file will be exported. These are
the predicates which are regarded as library predicates, and it is a call on one of
them which must cause the library file to be loaded.

Like most symbolic languages, ALS Prolog utilizes a name table which is a hash
table recording the association between names of predicates and the internal addresses at which their executable code is stored. Quite simply, the ALS library
mechanism replaces the normal name table entry for the library predicates by a special 'stub' name table entry which accomplishes three things:
* it indicates that the predicate in question is a library predicate;
* it indicates the file in which the library predicate resides;
* it issues an internal ALS Prolog interrupt which is regarded as a library interrupt.
In essence, execution is interrupted before execution of the called predicate, say p,
has actually commenced. During handling of the interrupt, the indicated file is
loaded (really, reconsulted, which is important), and the interrupt is released, resuming normal execution at the call to p. However, since the library file reconsulted during the interrupt contains a definition of p, the special name table entry
for p has been replaced by a normal name table entry p, so that execution proceeds
as if the code for p had always been loaded. Note that there is no interpretive overhead for this mechanism. The sole cost is born by the predicates which are stored
as library predicates. And the overhead for the library predicates is not measurably
greater that their own portion of the loading time at program initialization, were they
to be loaded with the rest of the the system.

The primitive mechanisms which implement this approach to libraries are to be
found in the builtins file blt_sys.pro. The actual loading mechanism is defined by
load_lib/2. The related predicate force_libload_all/2 can be used to force the
loading of a list of library files. This can be useful during construction of a stand-alone
package. The low level mechanism for installing a library-type name table entry is
libhide/3. The ALS Prolog system uses the file blt_lib.pro to record information
about files which are to be treated as library files. This allows great flexibility, and
in particular allows users and developers to add their own packages as library files.
A tool for managing this process is described in the ALS Development Tools
Guide.

The library includes
such facilities as the macro processing tools and the structure definition/abstraction
tools. These are described in their own sections of this manual or the ALS Tools
Guide. The survey below lists the remaining groups which have been installed as
of the date of writing of this chapter.

## 12.2 Lists: Algebraic List Predicates (listutl1.pro)
```
append/2
append(ListOfLists, Result)
append(+, -)
```
-- appends a list of lists together

If ListOfLists is a list, each of whose elements is a list, Result is obtained by appending the members of ListOfLists together in order.
```
intersect/2
intersect(L,IntsectL)
intersect(+,-)
```
-- returns the intersection of a list of lists

If L is a list of lists, returns the intersection IntsectL of all the lists appearing on L.
```
intersect/3
intersect(A,B,AintB)
intersect(+,+,-)
```
-- returns the intersection of two lists

If A and B are lists, returns the intersection AintB of A and B, which is the collection of all items common to both lists.
```
list_diff/3
list_diff(A, B, A_NotB)
list_diff(+, +, +)
```
-- returns the ordered difference of two lists

If A and B are lists, returns the difference A-B consisting of all items on A, but not
on B.
```
list_diffs/4
list_diffs(A,B,A_NotB,B_NotA)
list_diffs(+,+,-,-)
```
-- returns both ordered differences of two lists

If A and B are lists, returns both the difference A-B together with the difference B-A.
```
sorted_merge/2
sorted_merge(ListOfLists, Union)
sorted_merge(+, -)
```
-- returns the sorted union of a list of lists

If ListOfLists is a list of lists, Union is the sorted merge (non-repetitive union) of
the members of ListsOfLists.
```
sorted_merge/3
sorted_merge(List1, List2, Union)
sorted_merge(+, +, -)
```
-- returns the sorted union of two lists

If List1 and List2 are lists of items, Union is the sorted merge (non-repetitive union) of List1 and List2.
```
symmetric_diff/3
symmetric_diff(A,B,A_symd_B)
symmetric_diff(+,+,-)
```
-- returns the symmetric difference of two lists

If A and B are lists, returns the symmetric difference of A and B, which is the union
of A-B and B-A.
```
union/3
union(A,B, AuB)
union(+,+, -)
```
-- returns the ordered union of two lists

If A and B are lists, returns the ordered union of A and B, consisting of all items
occurring on either A or B, with all occurrences of items from A occurring before
any items from B-A; equivalent to:

    append(A,B-A,AuB);

If both lists have the property that each element occurs no more than once, then the
union also has this property.

## [12.3 Lists: Positional List Predicates (listutl2.pro)](id:positional-list-predicates)
```
at_most_n/3
at_most_n(List, N, Head)
at_most_n(+, +, -)
```
-- returns the initial segment of list of length =< N

If List is a list and N is a non-negative integer, Head is the longest initial segment
of List with length =< N.
```
change_nth/3
change_nth(N, List, NewItem)
change_nth(+, +, +)
```
-- destructively changes the Nth element of a list

If N is a non-negative integer, List is a list, and NewItem is any non-var object, destructively changes the Nth element of List to become NewItem; this predicate numbers the list beginning with 0.
```
deleteNth/3
deleteNth(N, List, Remainder)
deleteNth(+, +, -)
```
-- deletes the Nth element of a list

If N is a non-negative integer and List is a list, then Remainder is the result of deleting the Nth element of List; this predicate numbers the list beginning with 1.
```
get_list_tail/3
get_list_tail(List, Item, Tail)
get_list_tail(+, +, -)
```
-- returns the tail of a list determined by an element

If List is a list and Item is any object, Tail is the portion of List extending from the
leftmost occurrence of Item in List to the end of List; fails if Item does not belong to List.
```
list_delete/3
list_delete(List, Item, ResultList)
list_delete(+, +, -)
```
-- deletes all occurrences of an item from a list

If List is a list, and Item is any object, ResultList is obtained by deleting all occurrences of Item from List.
```
nth/3
nth(N, List, X)
nth(+, +, -)
```
-- returns the nth element of a list

If List is a list and N is a non-negative integer, then X is the nth element of List.
```
nth_tail/4
nth_tail(N, List, Head, Tail)
nth_tail(+, +, -, -)
```
-- returns the nth head and tail of a list

If List is a list and N is a non-negative integer, then Head is the portion of List up
to but not including the Nth element, and tail is the portion of List from the Nth element to the end.
```
position/3
position(List, Item, N)
position(+, +, -)
```
-- returns the position number of an item in a list

If List is a list and Item occurs in List, N is the number of the leftmost occurrence
of Item in List; fails if Item does not occur in List.
```
position/4
position(List, Item, M, N)
position(+, +, +, -)
```
-- returns the position number of an item in a list

If List is a list and Item occurs in List, N-M is the number of the leftmost occurrence
of Item in List; fails if Item does not occur in List.
```
sublist/4
sublist(List,Start,Length,Result)
sublist(+,+,+,-)
```
-- extracts a sublist from a list

If List is an arbitrary list, Result is the sublist of length Length beginning at position Start in List.
```
subst_nth/4
subst_nth(N, List, NewItem, NewList)
subst_nth(+, +, +, -)
```
-- non-destructively changes the Nth element of a list

If N is a non-negative integer, List is list, and NewItem is any non-var object,
NewList is the result of non-destructively changing the Nth element of List to become NewItem; this predicate numbers the list beginning with 0.

## 12.4 Lists: Miscellaneous List Predicates (listutl3.pro)
```
check_default/4
check_default(PList, Tag, Default, Value)
check_default(+, +, +, -)
```
-- looks up an equation on a list, with a default

PList is a list of equations of the form tag = value; check_default(PList, Tag, Default, Value) succeeds if Tag=Value belongs to PList; otherwise, applies Default=Value (which always succeeds if Value is a variable).
```
encode_list/3
encode_list(Items, Codes, CodedItems)
encode_list(+, +, -)
```
-- combines a list of items with a list of codes

If Items and Codes are lists of arbitrary terms of the same length, then CodedItems
is the list of corresponding pairs of the form Code-Item
```
flatten/2
flatten(List, FlatList)
flatten(+, -)
```
-- flattens a nested list

If List is a list, some of whose elements may be nested lists, FlatList is the flattened
version of List obtained by traversing the tree defining List in depth-first, left-to-right order; compound structures other than list structures are not flattened.
```
merge_plists/3
merge_plists(LeftEqnList, RightEqnList, MergedLists)
merge_plists(+, +, -).
```
-- (recursively) merges two tagged equation lists

LeftEqnList and RightEqnList are lists of equations of the form tag = value
MergedLists consists of all equations occurring in either LeftEqnList or RightEqnList, where if the equations 
Tag=LVal and Tag = RVal occur in LeftEqnList and RightEqnList, respectively, MergedLists will contain the 
equation Tag = MVal where: a)If both of LVal and RVal are lists, then MVal is obtained by recursively
calling merge_plists(LVal, RVal, MVal); b)Otherwise, MVal is LVal.
```
n_of/3
n_of(N, Item, Result)
n_of(+, +, -)
```
-- creates a list of N copies of an item

Result is a list of length N all of whose elements are the entity Item.
```
nobind_member/2
nobind_member(X, List)
nobind_member(+, +)
```
-- tests list membership without binding any variables

nobind_member(X, List) holds and only if X is a member of List; if the test is successful, no variables in either input are bound.
```
number_list/2
number_list(List, NumberedList)
number_list(+, -)
```
-- creates a numbered list from a source list

If List is a list, NumberedList is a list of terms of the form N-Item, where the Item
components are simply the elements of List in order, and N is a integer sequentially numbering the elements of List.
```
number_list/3
number_list(Items, StartNum, NumberedItems)
number_list(+, +, -)
```
-- numbers the elements of a list

If Items is a list, and StartNum is an integer, NumberedItems is the list obtained by
replacing each element X in Items by N-X, where N is the number of the position of X in Items.
```
output_prolog_list/1
output_prolog_list(List)
output_prolog_list(+)
```
-- outputs items on a list, one to a line

Outputs (to the current output stream) each item on List, one item to a line, followed by a period.
```
remove_tagged/3
remove_tagged(EqnList, TagsToRemove, ReducedEqnList)
remove_tagged(+, +, -).
```
-- removes tagged equations from a list

EqnList is a list of equations of the form Tag = Value and TagsToRemove is a list of
atoms which are candidates to occur as tags in these equations. ReducedEqnList is
the result of removing all equations from the list EqnList which begin with a tag from TagsToRemove.
```
struct_lookup_subst/4
struct_lookup_subst(OrderedTags, DefArgs, ArgSpecs, ArgsList)
struct_lookup_subst(+, +, +, -)
```
-- performs substs for structs package constructors

OrderedTags and DefArgs are lists of the same length; The resulting ArgsList will be of the same length.
ArgSpecs is a list of equations of the form Tag = Value where each of the Tags in such
an equation must be on the list OrderedTags (but not all OrderedTags elements must
occur on ArgSpecs); in fact, ArgSpecs can be empty. The elements X of ArgsList
are defined as follows: 

    if X corresponds to Tag on OrderedTags, 
    then: if Tag=Val occurs on ArgSpecs, X is Val; 
          otherwise, X is the element of DefArgs corresponding to Tag.

## 12.5 Tree Predicates (avl.pro)
```
avl_create/1
avl_create(Tree)
avl_create(-)
```
-- create an empty tree.

avl_create(Tree) creates an empty avl tree which is unified with Tree.
```
avl_inorder/2
avl_inorder(Tree,List)
avl_inorder(+,-)
```
-- returns list of keys in an avl tree in in-order traversal

If Tree is an avl tree, List is the ordered list of keys encountered during an inorder traversal of Tree.
```
avl_inorder_wdata/2
avl_inorder_wdata(Tree,List)
avl_inorder_wdata(+,-)
```
-- returns list of keys and data in an avl tree in in-order traversal

If Tree is an avl tree, then List is the ordered list of terms of the form Key-Data encountered during an inorder traversal of Tree.
```
avl_insert/4
avl_insert(Key,Data,InTree,OutTree)
avl_insert(+,+,+,-)
```
-- inserts a node in an avl tree

Inserts Key and Data into the avl-tree passed in through InTree yielding a tree which is unified with OutTree. 
If the Key is already present in the tree, then Data replaces the old data value associated with Key in the tree.
```
avl_search/3
avl_search(Key,Data,Tree)
avl_search(+,?,+)
```
-- searches for a key in an avl tree

Tree is searched in for Key. If Key is found, Data is unified with the data value corresponding to Key; 
If Key is not found, avl_search will fail.

## 12.6 Miscellaneous Predicates (commal.pro)]
```
flatten_comma_list/2
flatten_comma_list(SourceList, ResultList)
flatten_comma_list(+, -)
```
-- flattens nested comma lists and removes extraneous trues'

If SourceList is a comma list (i.e., (a,b,c,...) ), then ResultList is also a comma list
which is the result of removing all extraneous nesting and all extraneous occurrences of the atom true.

## 12.7 I/O Predicates (iolayer.pro)

To be added.

## 12.8 Control Predicates (lib_ctl.pro)
```
bagOf/3
bagOf(Pattern, Goal, Result)
bagOf(+, +, -)
```
-- Like bagof/3, but succeeds with empty list on no solutions

bagOf/3 is just like bagof/3, except that if Goal has no solutions, bagof/3 fails,
whereas bagOf/3 will succeed, binding Result to [].
```
max/3
max(A,B,M)
max(+,+,-)
```
-- computes the maximum of two numbers

If A and B are ground expressions which evaluate to numbers under is/2, then M will be their maximum value.
```
min/3
min(A,B,M)
min(+,+,-)
```
-- computes the minimum of two numbers

If A and B are ground expressions which evaluate to numbers under is/2, then M will be their minimum value.
```
setOf/3
setOf(Pattern, Goal, Result)
setOf(+, +, -)
```
-- Like setof/3, but succeeds with empty list on no solutions
setOf/3 is just like setof/3, except that if Goal has no solutions, setof/3 fails, whereas
setOf/3 will succeed, binding Result to [].

## 12.9 Prolog Database Predicates (misc_db.pro)
```
assert_all/1
assert_all(ClauseList)
assert_all(+)
```
-- asserts each clause on ClauseList in the current module

If ClauseList is a list of clauses, asserts each of these clauses in the current module.
```
assert_all0/2
assert_all0(ClauseList,Module)
assert_all0(+,+)
```
-- asserts each clause on ClauseList in module Module

If ClauseList is a list of clauses and Module is a Module,  asserts each of these clauses in module Module.
```
assert_all_refs/3
assert_all_refs(Module,ClauseList, RefsList)
assert_all_refs(+,+, -)
```
-- asserts a list of clauses, in a module, returning a list of refs

If Module is a module, and if ClauseList is a list of terms which can be asserted as
clauses, then assert_all_refs/3 causes each term on ClauseList to be asserted in
Module, and returns RefsList as the list of corresponding database references to these asserted clauses.
```
erase_all/1
erase_all(RefsList)
erase_all(+)
```
-- erases each clauses referenced by a list of clause references

If RefsList is a list of clause database references, causes each clause corresponding to one
of these references to be erased.

## 12.10 Miscellaneous I/O Predicates (misc_io.pro)
```
colwrite/4
colwrite(AtomList,ColPosList,CurPos,Stream)
colwrite(+,+,+,+)
```
-- writes atoms in AtomList at column positions in ColPosList

If AtomList is a list of atoms (symbols or UIAs), and if ColPosList is a list of
monotonically increasing positive integers of the same length as AtomList, and if
CurPos is a positive integer (normally 1), and Stream is valid output stream (in text
mode), this predicate outputs the items on AtomList to Stream, starting each element of AtomList at the postition indicated by the corresponding element of ColPosList. If a given item would overflow its column, it is truncated. Normally, CurPos = 1 and ColPosList begins with an integer greater than 1, so that the first column
position is implicit.
```
copyFiles/2
copyFiles(SourceFilesList, TargetSubDirPath)
copyFiles(+, +)
```
-- copies files to a directory

If SourceFilesList is a list of file names, and TargetSubDirPath is either an atom or
an internal form naming a directory, copies all of the indicated files to files with the
same names in TargetSubDirPath.
```
gen_file_header/[3,4]
gen_file_header(OutStream,SourceFile,TargetFile)
gen_file_header(OutStream,SourceFile,TargetFile,ExtraCall)
gen_file_header(+,+,+)
gen_file_header(+,+,+,+)
```
-- output a header suitable for a generated file

OutStream is a write stream, normally to file TargetFile. Given SourceFile = fooin,
and TargetFile = fooout, gen_file_header/3 outputs a header of the following format on OutStream:
```
/*================================================
        fooout
        --Generated from: fooin
        Date: 94/4/17
        Time: 9:52:53
*================================================*/
```
In gen_file_header/4, the argument ExtraCall is called just before the printing of the
lower comment line. Thus, if ExtraCall were

    printf(OutStream,'         -- by zipper_foo\n',[]),

the output would look like:
```
/*================================================
        fooout
        --Generated from: fooin
        Date: 94/4/17
        Time: 9:52:53
        -- by zipper_foo
*=================================================*/
```
```
putc_n_of/3
putc_n_of(Num, Char, Stream)
putc_n_of(+,+,+)
```
-- output Num copies of the char with code Char to Stream

Num should be a positive integer, and Char should be the code of a valid character;
Stream should be an output stream in text mode. Outputs, to Stream, Num copies
of the characater with code Char.
```
read_terms/1
read_terms(Term_List)
read_terms(-)
```
-- reads a list of Prolog terms from the default input stream

Reads a list (Term_List) of all terms which can be read from the default input
stream
```
read_terms/2
read_terms(Stream,Term_List)
read_terms(+,-)
```
-- reads a list of Prolog terms from stream Stream

Reads a list (Term_List) of all terms which can be read from the stream Stream.
```
read_lines/[1,2]
read_lines(Stream,Line_List)
read_lines(+,-)
```
Reads a list (Line_List) of all lines which can be read from the stream Stream.

## 12.11 Simple I/O Predicates (simplio.pro)

To be Added.

## 12.12 String Manipulation Predicates (strings.pro)
```
asplit/4
asplit(Atom,Splitter,LeftPart,RightPart)
asplit(+,+,-,-)
```
-- divides an atom as determined by a character

If Atom is any atom or UIA, and if Splitter is the character code of of a character,
then, if the character with code Splitter occurs in Atom, LeftPart is an atom consisting of that part of Atom from the left up to and including the leftmost occurrence of
the character with code Splitter, and RightPart is the atom consisting of that part of
Atom extending from immediately after the end of LeftPart to the end of Atom.
```
asplit0/4
asplit0(AtomCs,Splitter,LeftPartCs,RightPartCs)
asplit0(+,+,-,-)
```
-- divides a list of character codes as determined by a character code

If AtomCs is a list of character codes, and if Splitter is the character code of of a
character, then, if the character with code Splitter occurs in AtomCs, LeftPart is the
list consisting of that part of AtomCs from the left up to and including the leftmost
occurrence of Splitter, and RightPart is the atom consisting of that part of AtomCs
extending from immediately after the end of LeftPart to the end of AtomCs.
```
head/4
head(Atom,Splitter,Head,Tail)
head(+,+,-,-)
```
-- splits an list into segments determined by a character code

If Atom is a list of character codes, splits Atom into Head and tail the way asplit
would, using the first occurrence of Splitter; on successive retrys, usings the succeeding occurrences of Spliter as the split point.
```
head0/4
head0(List,Splitter,Head,Tail)
head0(+,+,-,-)
```
-- splits a character code list into segments determined by a code

If List is a list of character codes, splits List into Head and tail the way asplit0
would, using the first occurrence of Splitter; on successive retrys, usings the succeeding occurrences of Spliter as the split point.

## 12.13 Extensible List Utilities (xlists.pro)
```
xlist_append/2
xlist_append(ListOfXLists, Result)
xlist_append( +, -)
```
-- appends together an ordinary list of extensible lists

If ListOfXLists is an ordinary list of xtensible lists, then Result is obtained by serially xappending each of the xlists occurring on ListOfXLists.
```
xlist_append/3
xlist_append( Left, Right, Result)
xlist_append( +, +, -)
```
-- appends two extensible lists

```
xlist_head/2
xlist_head( XList, Result)
xlist_head( +, -)
```
-- returns the head of an extensible list

```
xlist_init/1
xlist_init(Result)
xlist_init(-)
```
-- creates a freshly initialized extensible list

xtensible lists are carried around in the form (Head, Tail) The actual list may be a
standard extensible list [a,b,c,d | T] or may be a comma separated list: (a, (b, (c, (d,
T)))) It is up to the routines using these tools to bind the tail variable to the correct
structure, or to createthe correct type of structure for xappending to another such xlist.
```
xlist_make/3
xlist_make( Head, Tail, Result)
xlist_make( +, +, -)
```
-- Makes an extenstible list data structure from Head,Tail

```
xlist_tail/2
xlist_tail( XList, Result)
xlist_tail( +, -)
```
-- returns the tail of an extensible list

```
xlist_unit_c/2
xlist_unit_c(First, Result)
xlist_unit_c(+, -)
```
-- creates a freshly initialized comma-type xlist with first elt

```
xlist_unit_l/2
xlist_unit_l(First, Result)
xlist_unit_l(+, -)
```
-- creates a freshly initialized ordinary xlist with first elt

## 12.14 CREF: Cross-Referencer (cref.pro)

CREF operates on suites of one or more prolog files, and outputs a report describing the program structure found, including modules, calls between predicates, numbers of clauses and facts for a predicate, facts and rules asserted, operator declarations, ALS Library files used, top-level (uncalled) predicates, and undefined predicates.

The main entry points, cref(SuiteName) {- runs "nonstop", with no cref shell} and cref(SuiteName, Opts)) {- can enter interactive cref shell after the initial analysis}.  Both of these calls are library calls, auto-loading the cref analysis files.

Neither of the following  auto-load cref.pro, but from the OS console, you can issue the command

```
alspro cref
```

to start alspro with cref loaded; then the convenience commands work:

```
c(SuiteName)        - equivalent to cref(SuiteName, []) {starts cref_shell}
d(SuiteName)        - equivalent to cref(SuiteName)     {runs "non-stop"}
 ````
 
Several test/example suites are built-in (recorded in cref_suite_db.pro:

```
suite_info(suite,hickory,
	'examples/als'+['hickory.pro','id.pro'],'hickory.xrf').
suite_info(suite,tc,
	'alsdir/library/tests'+['cref_test1.pro','cref_test2.pro',
			'cref_test3.pro'],'tc.xrf').
suite_info(suite,tc2,'alsdir/library/tests'+['cref_test_lib1.crf'],'tc2.xrf').
suite_info(suite,chat80,'examples/chat80'+['als_chat.pro','*.pl'],'chat80.xrf').
 ````
These can be invoked by cref(hickory), cref(tc), cref(tc2), and cref(chat80), respectively. 

When cref finishes writing the report, it also asserts
 
  ````
 	cref:lib_files_used(SuiteName, LibFilesList) 
  ````
  
 (in module cref, as indicated), where LibFilesList is the list of library files used by SuiteName.  This is then available for use by other tools, such as packaging.



 
 When cref finishes writing the report, it also asserts
 
  ````
 	cref:lib_files_used(SuiteName, LibFilesList) 
  ````
  
 (in module cref, as indicated), where LibFilesList is the list of library files used by SuiteName.  This is then available for use by other tools, such as packaging.
 
 
