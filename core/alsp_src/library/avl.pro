/*=====================================================================
 |		avl.pro		
 |	Copyright (c) 1990 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		-- avl tree program
 |
 | Author: Kevin A. Buettner
 | Created: 4/2/90
 | Revision History:
 |
 | Module Name:	avl
 |
 | Exported Procedures:
 |
 |	avl_create(Tree)	-- create an empty tree, Tree
 |
 |	avl_insert(Key,Data,InTree,OutTree)
 |				-- insert Key and Data into InTree
 |				   producing OutTree; If the Key is already
 |				   present in the tree, then Data replaces the 
 |				   old data value in the tree
 |
 |	avl_search(Key,Data,Tree)
 |				-- search Tree for Key and return Data
 |
 |	avl_inorder(Tree,List)	-- List is a list of the keys in the tree
 |				   found in an inorder traversal.
 |
 |
 | Data Structure:
 |
 |	Note:	The following comments are provided for informational purposes
 |		only.  The user of this module should not count on these
 |		structures being implemented in this manner nor attempt to
 |		access them directly.  Access should be made only thru
 |		the exported procedures.
 |
 |	The tree is represented by a structured term.  
 |
 |	empty denotes the empty tree.
 |
 |	bn(Key,Data,Left,Right) denotes a tree whose left and right subtrees
 |		have the same height.
 |
 |	ln(Key,Data,Left,Right) denotes a tree the height of whose left subtree
 |		is one larger than the height of the right subtree.
 |
 |	rn(Key,Data,Left,Right) denotes a tree the height of whose right
 |		subtree is one larger than the height of the left subtree.
 *==========================================================================*/

module avl.

export avl_create/1.
export avl_inorder/2.
export avl_inorder_wdata/2.
export avl_insert/4.
export avl_search/3.

/*!-------------------------------------------------------------
 * avl_create/1
 * avl_create(Tree)	
 * avl_create(-)	
 *	
 * -	create an empty tree.
 *
 * avl_create(Tree) creates an empty avl tree which is
 * unified with Tree.
 *!-------------------------------------------------------------*/
avl_create(empty).


/*
 * shell/0	-- shell for testing out the avl insertion and searching
 *		   routines.
 *
 */

shell :- shell(empty).
shell(Tree) :-
    write('avl: '),
    read(Command),
    act_on(Command,Tree).

act_on(quit,_) :- !.
act_on(halt,_) :- !.
act_on(exit,_) :- !.
act_on(insert(Key-Data),Tree) :-
    !,
    avl_insert(Key,Data,Tree,NewTree),
    shell(NewTree).
act_on(insert(Item),Tree) :-
    !,
    avl_insert(Item,d(Item),Tree,NewTree),
    shell(NewTree).
act_on(write, Tree) :-
    !,
%    pretty_print:pretty_print(Tree,50),nl,
    write(Tree),nl,
    shell(Tree).
act_on(height,Tree) :-
    !,
    height(Tree,Height),
    write(Height),nl,
    shell(Tree).
act_on(inorder,Tree) :-
    !,
    avl_inorder(Tree,List),
    write(List),
    nl,
    shell(Tree).
act_on(inorder_data,Tree) :-
    !,
    avl_inorder_wdata(Tree,List),
    write(List),
    nl,
    shell(Tree).
act_on(preorder,Tree) :-
    !,
    preorder(Tree,List),
    write(List),
    nl,
    shell(Tree).
act_on(count,Tree) :-
    !,
    nelems(Tree,Count),
    write(Count),
    nl,
    shell(Tree).
act_on(stats,Tree) :-
    !,
    height(Tree,Height),
    nelems(Tree,N),
    write('Height'=Height),write('  Element Count'=N),nl,
    shell(Tree).
act_on(search(Key),Tree) :-
    avl_search(Key,Data,Tree),
    !,
    write(Data),nl,
    shell(Tree).
act_on(search(Key),Tree) :-
    !,
    write('Key '),write(Key),write(' not found.'),nl,
    shell(Tree).
act_on(_,Tree) :-
    write('Unrecognized command.'),nl, shell(Tree).


height(empty,-1) :- !.
height(Tree,Height) :-
    avl_left(Tree,Left),
    avl_right(Tree,Right),
    height(Left,LH),
    height(Right,RH),
    max(LH,RH,H),
    Height is H+1.

/*
max(N1,N2,N1) :- N1 >= N2,!.
max(N1,N2,N2).
*/

nelems(empty,0) :- !.
nelems(Tree,N) :-
    avl_left(Tree,Left),
    avl_right(Tree,Right),
    nelems(Left,NL),
    nelems(Right,NR),
    N is NL+NR+1.

/*!----------------------------------------------------------------
 | avl_inorder/2
 | avl_inorder(Tree,List)
 | avl_inorder(+,-)
 |
 |	- returns list of keys in an avl tree in in-order traversal
 |
 | If Tree is an avl tree, List is the ordered list of keys 
 | encountered during an inorder traversal of Tree.
 *!---------------------------------------------------------------*/
avl_inorder(Tree,List) :-
    inorder(Tree,[],List).

inorder(empty,L,L) :- !.
inorder(Tree,InL,OutL) :-
    avl_key(Tree,Key),
    avl_left(Tree,Left),
    avl_right(Tree,Right),
    inorder(Right,InL,RL),
    inorder(Left,[Key|RL],OutL).

/*!----------------------------------------------------------------
 | avl_inorder_wdata/2
 | avl_inorder_wdata(Tree,List)
 | avl_inorder_wdata(+,-)
 |
 |	- returns list of keys and data in an avl tree in in-order traversal
 |
 | If Tree is an avl tree, List is the ordered list of terms of the
 | form Key-Data encountered during an inorder traversal of Tree.
 *!---------------------------------------------------------------*/
avl_inorder_wdata(Tree,List) :-
    inorder_wdata(Tree,[],List).

inorder_wdata(empty,L,L) :- !.
inorder_wdata(Tree,InL,OutL) :-
    avl_key(Tree,Key),
	avl_data(Tree,Data),
    avl_left(Tree,Left),
    avl_right(Tree,Right),
    inorder_wdata(Right,InL,RL),
    inorder_wdata(Left,[Key-Data|RL],OutL).

preorder(Tree,List) :-
    preorder(Tree,[],List).

preorder(empty,L,L) :- !.
preorder(Tree,InL,[Key|OutL]) :-
    avl_key(Tree,Key),
    avl_left(Tree,Left),
    avl_right(Tree,Right),
    preorder(Right,InL,RL),
    preorder(Left,RL,OutL).


/*
 * avl_key(Tree,Key)
 * avl_data(Tree,Data)
 * avl_left(Tree,LeftSubTree)
 * avl_right(Tree,RightSubTree)
 *
 * Each of these two argument procedures takes a non-empty avl-tree as
 * its first arguments and unifies the second argument with the obvious
 * component of the node.
 */

avl_key(Tree,Key) :- arg(1,Tree,Key).
avl_data(Tree,Data) :- arg(2,Tree,Data).
avl_left(Tree,Left) :- arg(3,Tree,Left).
avl_right(Tree,Right) :- arg(4,Tree,Right).


/*!------------------------------------------------------------------
 * avl_insert/4
 * avl_insert(Key,Data,InTree,OutTree)
 * avl_insert(+,+,+,-)
 *
 *	- inserts a node in an avl tree
 *
 * Inserts Key and Data into the avl-tree passed in through InTree
 * giving a tree which is unified with OutTree.  If the Key is already
 * present in the tree, then Data replaces the old data value in the
 * tree.
 *!------------------------------------------------------------------*/


avl_insert(Key,Data,InTree,OutTree) :-
    insert(InTree,Key,Data,OutTree,_),
    !.
avl_insert(Key,Data,T,T) :-
    telling(Current),
    tell(user),
    write('avl_insert failed on '),write(Key:Data),nl,
    height(T,Height),
    nelems(T,N),
    write('Height'=Height),write('  Element Count'=N),nl,
    avl_inorder(T,L),
    write('Keys:'),
    write(L),
    nl,
    tell(Current).


%% insert(InTree,Key,Data,OutTree,Longer)

% empty tree
insert(empty,Key,Data,bn(Key,Data,empty,empty),longer) :- !.

% balanced tree node
insert(bn(TreeKey,TreeData,Left,Right),Key,Data,OutTree,Longer) :-
    !,
    compare(Cmp,Key,TreeKey),
    insert_b(Cmp,TreeKey,TreeData,Left,Right,Key,Data,OutTree,Longer).

insert_b(=,_,_,Left,Right,Key,Data,bn(Key,Data,Left,Right),same) :- !.
insert_b(<,TreeKey,TreeData,Left,Right,Key,Data,OutTree,Longer) :- !,
    insert(Left,Key,Data,NewLeft,NewLonger),
    insert_bul(NewLonger,
	       TreeKey,TreeData,NewLeft,Right,
	       OutTree,Longer).
insert_b(>,TreeKey,TreeData,Left,Right,Key,Data,OutTree,Longer) :-
    insert(Right,Key,Data,NewRight,NewLonger),
    insert_bur(NewLonger,
	       TreeKey,TreeData,Left,NewRight,
	       OutTree,Longer).
    
insert_bul(same, Key,Data,Left,Right, bn(Key,Data,Left,Right),same) :- !.
insert_bul(longer, Key,Data,Left,Right, ln(Key,Data,Left,Right),longer).

insert_bur(same, Key,Data,Left,Right, bn(Key,Data,Left,Right),same) :- !.
insert_bur(longer, Key,Data,Left,Right, rn(Key,Data,Left,Right),longer).

% left subtree longer
insert(ln(TreeKey,TreeData,Left,Right),Key,Data,OutTree,same) :-
    !,
    compare(Cmp,Key,TreeKey),
    insert_l(Cmp,TreeKey,TreeData,Left,Right,Key,Data,OutTree).

insert_l(=,_,_,Left,Right,Key,Data,ln(Key,Data,Left,Right)) :- !.
insert_l(<,TreeKey,TreeData,Left,Right,Key,Data,OutTree) :- !,
    insert(Left,Key,Data,NewLeft,NewLonger),
    insert_lul(NewLonger,
	       TreeKey,TreeData,NewLeft,Right,
	       OutTree).
insert_l(>,TreeKey,TreeData,Left,Right,Key,Data,OutTree) :-
    insert(Right,Key,Data,NewRight,NewLonger),
    insert_lur(NewLonger,
	       TreeKey,TreeData,Left,NewRight,
	       OutTree).

insert_lul(same, Key,Data,Left,Right, ln(Key,Data,Left,Right)) :- !.
insert_lul(longer, Key,Data,Left,Right, OutTree) :-
    insert_lfix(Left,Key,Data,Right, OutTree).

insert_lur(same, Key,Data,Left,Right, ln(Key,Data,Left,Right)) :- !.
insert_lur(longer, Key,Data,Left,Right, bn(Key,Data,Left,Right)).

insert_lfix(ln(KeyL,DataL,LeftL,RightL), Key,Data,Right,
	    bn(KeyL,DataL,LeftL,bn(Key,Data,RightL,Right))) :- !.
insert_lfix(rn(KB,DB,LB,bn(KC,DC,LC,RC)), KA,DA,RA,
	    bn(KC,DC,bn(KB,DB,LB,LC),bn(KA,DA,RC,RA))) :- !.
insert_lfix(rn(KB,DB,LB,ln(KC,DC,LC,RC)), KA,DA,RA,
	    bn(KC,DC,bn(KB,DB,LB,LC),rn(KA,DA,RC,RA))) :- !.
insert_lfix(rn(KB,DB,LB,rn(KC,DC,LC,RC)), KA,DA,RA,
	    bn(KC,DC,ln(KB,DB,LB,LC),bn(KA,DA,RC,RA))).

% right subtree longer
insert(rn(TreeKey,TreeData,Left,Right),Key,Data,OutTree,same) :-
    !,
    compare(Cmp,Key,TreeKey),
    insert_r(Cmp,TreeKey,TreeData,Left,Right,Key,Data,OutTree).

insert_r(=,_,_,Left,Right,Key,Data,rn(Key,Data,Left,Right)) :- !.
insert_r(<,TreeKey,TreeData,Left,Right,Key,Data,OutTree) :- !,
    insert(Left,Key,Data,NewLeft,NewLonger),
    insert_rul(NewLonger,
	       TreeKey,TreeData,NewLeft,Right,
	       OutTree).
insert_r(>,TreeKey,TreeData,Left,Right,Key,Data,OutTree) :-
    insert(Right,Key,Data,NewRight,NewLonger),
    insert_rur(NewLonger,
	       TreeKey,TreeData,Left,NewRight,
	       OutTree).

insert_rul(same, Key,Data,Left,Right, rn(Key,Data,Left,Right)) :- !.
insert_rul(longer, Key,Data,Left,Right, bn(Key,Data,Left,Right)).

insert_rur(same, Key,Data,Left,Right, rn(Key,Data,Left,Right)) :- !.
insert_rur(longer, Key,Data,Left,Right, OutTree) :-
    insert_rfix(Right, Key,Data,Left, OutTree).

insert_rfix(rn(KB,DB,LB,RB), KA,DA,LA,
	    bn(KB,DB,bn(KA,DA,LA,LB),RB)) :- !.
insert_rfix(ln(KB,DB,bn(KC,DC,LC,RC),RB), KA,DA,LA,
	    bn(KC,DC,bn(KA,DA,LA,LC),bn(KB,DB,RC,RB))) :- !.
insert_rfix(ln(KB,DB,rn(KC,DC,LC,RC),RB), KA,DA,LA,
	    bn(KC,DC,ln(KA,DA,LA,LC),bn(KB,DB,RC,RB))) :- !.
insert_rfix(ln(KB,DB,ln(KC,DC,LC,RC),RB), KA,DA,LA,
	    bn(KC,DC,bn(KA,DA,LA,LC),rn(KB,DB,RC,RB))).

/*!---------------------------------------------------------------
 * avl_search/3
 * avl_search(Key,Data,Tree)
 * avl_search(+,?,+)
 *
 *	- searches for a key in an avl tree
 *
 * Tree is searched in for Key.  Data is unified with the corresponding
 * data value if found.  If Key is not found, avl_search will fail.
 *!--------------------------------------------------------------*/

avl_search(Key,Data,Tree) :-
    avl_key(Tree,TreeKey),
    compare(Cmp,Key,TreeKey),
    search(Cmp,Key,Data,Tree).

search(=,_,Data,Tree) :-
    !,
    avl_data(Tree,Data).
search(<,Key,Data,Tree) :-
    !,
    avl_left(Tree,Left),
    avl_search(Key,Data,Left).
search(>,Key,Data,Tree) :-
    avl_right(Tree,Right),
    avl_search(Key,Data,Right).

endmod.
