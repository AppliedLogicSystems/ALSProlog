/*
 * mktree1.pro
 *	Copyright (c) 1993 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 5/29/93
 *
 *
 * We define a little command language for building a binary tree.  The
 * node values are classified as major and minor which will be used later
 * to determine whether or not two trees belong in the same equivalence class.
 * We define two trees to be in the same equivalence class iff the trees
 * have the same shape with corresponding major nodes having the same values.
 * We will use the ALS-Prolog global variable mechanism in an ingenious way
 * to determine which trees are in the same equivalence class.
 *
 * A tree will be input as a list of commands which are described as follows:
 *
 *	major(Term)	-- define the value of the current node to be
 *			   a major node with value Term
 *	minor(Term)	-- define the value of the current node to be
 *			   a minor node with value Term
 *	left		-- descend into the left subtree of the current
 *			   node
 *	right		-- descend into the right subtree of the current
 *			   node
 *	up		-- make the current node the parent of the current
 *			   node.  If there is no parent, the current
 *			   node becomes the left subtree of the newly
 *			   created parent
 *	swap		-- swap right and left subtrees of current node
 *
 *
 * Nodes are represented as prolog terms of the form:
 *
 *	node(Value,LeftSubtree,RightSubtree)
 *
 * where the variables have the intuitively obvious meanings.  The atom
 *
 *	$
 *
 * is used for leaf nodes.  Value will be instantiated to major(Term) or
 * minor(Term) depending on which was specified.
 * 
 *
 * Examples:
 *	[]
 * yields
 *	$
 *
 *	[major(a)]
 * yields
 *	node(a,$,$)
 *
 *	[major(a),right,major(b)]
 * yields
 *	node(a,$,node(b,$,$))
 *
 *	[major(a),left,major(b),up,right,major(c),left,major(d)]
 * yields
 *	node(a,node(b,$,$),node(c,node(d,$,$),$))
 *
 *	[major(b),up,right,major(c),left,major(d),up,up,major(a)]
 * also yields
 *	node(a,node(b,$,$),node(c,node(d,$,$),$))
 *
 * -------
 *
 * A shell which reads commands and responds to the commands is invoked
 * from Prolog's top level via the procedure shell/0.  The shell reads
 * a command (a Prolog term) and responds accordingly.  A list with tree
 * building commands as described in the above section will build the
 * specified tree, print it out, and list the equivalence class id that
 * it belongs to.  Equivalence class ids are non-negative integers.  The
 * equivalence class will be determined by attempting to unify the current
 * tree against those trees saved on a list via the global variable
 * mechanism.  Should no matching tree be found, the new tree will be
 * assigned a new equivalence class id and added to the beginning of
 * the list of trees (pointed at by the global variable).
 *
 * As the tree was being built, the shape of the tree and the values of
 * the major nodes were constructed in a deterministic fashion, but the
 * values of the minor nodes (if any) were installed non-deterministically.
 * Being non-deterministic, the Prolog backtracking mechanism will be forced
 * to uninstantiate those non-deterministic portions upon failure.  Thus
 * when the shell backtracks over the call which set the global variable,
 * those portions of the structure which were constructed in a
 * non-deterministic fashion, namely the values of the minor nodes will become
 * uninstantiated.  This gives us the value for the tree representing a
 * particular equivalence class.  The tree is fully instantiated with respect
 * to its shape and values of the major nodes, but the minor nodes are
 * uninstantiated.  Thus another tree which is the same in all respects
 * except possibly for the values of the minor nodes will unify, giving
 * us an extremely simple (and I dare say clever) test for determining whether
 * or not a given tree is in a particular equivalence class.
 *
 * The shell commands are as follows:
 *
 *	[tree_cmd1,tree_cmd2,...]	-- build and print tree as described
 *					   above
 *	print				-- print out the equivalence classes
 *	help				-- print out short summary of commands
 *	quit				-- quit shell to Prolog top level
 *
 *
<<<<<<< 1.3
=======
 * Since this program is part of the test suite, there is a method for
 * automatically running a test.  Simply run the goal,
 *
 *	test
 *
 * at the top level.  This procedure will execute tree commands as described
 * above from the file mktree1.in and write its output to the file mktree1.out.
 * After execution, mktree1.out should be compared with mktree1.cmp.  If
 * the Prolog I/O system changes significantly, it is possible that these
 * files will fail the diff, but that the output is still correct.  Should
 * this happen, mktree1.cmp should be updated accordingly.
>>>>>>> /tmp/T4a000_3
 *	
 */


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%
%%%%% Tree building procedures
%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%
%% mktree(Commands,Tree)
%%
%%	This is the top level procedure used to make a tree from a command
%%	list as given in the description (above).  A choice point is left
%%	for instantiation of data values for minor nodes.
%%

mktree(Commands,OutTree) :-
<<<<<<< 1.3
	mktree(Commands,CurNode,ParentList,OutTree),
	fill_leaves(OutTree).
=======
	mktree(Commands,CurNode,ParentList,OutTree).
>>>>>>> /tmp/T4a000_3


%%
%% mktree(Commands,CurNode,ParentList,OutTree)
%%

mktree([],OutTree,[],OutTree) :-
<<<<<<< 1.3
	!.
=======
	!,
	fill_leaves(OutTree).
>>>>>>> /tmp/T4a000_3
mktree([],_,[P|Ps],OutTree) :-
	!,
<<<<<<< 1.3
	top_parent(Ps,P,OutTree).
=======
	top_parent(Ps,P,OutTree),
	fill_leaves(OutTree).
>>>>>>> /tmp/T4a000_3
mktree([C|Cs],Node,Parents,OutTree) :-
	mktree(C,Cs,Node,Parents,OutTree).


%%
%% mktree(Command,Commands,CurNode,ParentList,OutTree)
%%

mktree(left,Cs,Node,PL,OutTree) :-
	!,
	node_left(Node,Left),
	mktree(Cs,Left,[Node|PL],OutTree).
mktree(right,Cs,Node,PL,OutTree) :-
	!,
	node_right(Node,Right),
	mktree(Cs,Right,[Node|PL],OutTree).
mktree(up,Cs,Node,[],OutTree) :-
	!,
	mktree(Cs,node(_,Node,_),[],OutTree).
mktree(up,Cs,_,[P|Ps],OutTree) :-
	!,
	mktree(Cs,P,Ps,OutTree).
mktree(major(Val),Cs,Node,PL,OutTree) :-
	install_value(Node,major(Val)),
	!,				%% NB: cutting install_value choicept
	mktree(Cs,Node,PL,OutTree).
mktree(minor(Val),Cs,Node,PL,OutTree) :-
	install_value(Node,minor(_)),	%% install minor tag
	!,
	mktree(Cs,Node,PL,OutTree),
	install_value(Node,minor(Val)).	%% NB:	not cutting install_value
					%%	choice point
mktree(swap,Cs,node(Val,Left,Right),PL,OutTree) :-
	!,
	mktree(Cs,node(Val,Right,Left),PL,OutTree).
mktree(C,Cs,Node,PL,OutTree) :-
	printf("Tree building command %t not understood. Skipping it.\n"),
	mktree(Cs,Node,PL,OutTree).


%%
%% fill_leaves(Tree)
%%
%%	Instantiates all the leave nodes in the tree thus preventing it
%%	from taking any additional "shape" in the future.  It also instantiates
%%	unfilled value positions.
%%

fill_leaves($) :- !.
fill_leaves(node(Val,Left,Right)) :-
	(var(Val) -> Val = $ ; true),
	fill_leaves(Left),
	fill_leaves(Right).


%%
%% top_parent(ParentList,Parent,TopParent)
%%
%%	Determines the top parent in the parent list.  This is just the
%%	last element in the list.
%%

top_parent([],TopParent,TopParent) :-
	!.
top_parent([Parent|Parents],_,TopParent) :-
	top_parent(Parents,Parent,TopParent).


%%
%% node_left(Node,Left)
%% node_right(Node,Right)
%%
%%	These access the left and right subtrees of a node.
%%

node_left(node(_,Left,_),Left).
node_right(node(_,_,Right),Right).

%%
%% install_value(Node,Value)
%%
%%	This predicate attempts to instantiate the value portion of a node,
%%	succeeding even if the value has already been instantiated.  The old
%%	value is not changed if already instantiated.  Note that when it
%%	successfully instantiates a node, this predicate will leave a
%%	choice point and that this IS intentional.
%%

install_value(node(Value,_,_),Value).
install_value(_,_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%
%%%%%  Procedures for determining and extending the equivalence relation
%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%
%% Create global variable for equivalence relation and counter variable
%%

:-	make_gv('_equivalences'),
	make_gv('_eq_counter'),
	set_equivalences([]),
	set_eq_counter(0).

<<<<<<< 1.3
=======
init_equiv :-
	set_equivalences([]),
	set_eq_counter(0).

>>>>>>> /tmp/T4a000_3
%%
%% equiv_member(Tree,EqId)
%%
%% 	Determines id of equivalance class for Tree.  EqId will be the
%%	equivalance class Id.
%%
%% Note:
%%
%%	We could make this MUCH more efficent by using our builtin hashing
%%	procedures.  We are using a simple linear lookup for the sake of
%%	simplicity.  See mktree2.pro for a more efficient version.
%%
%%

equiv_member(Tree,EqId) :-
	get_equivalences(Eqs),
	member(eqClass(EqId,Tree),Eqs),
	!.
equiv_member(Tree,EqId) :-
	get_eq_counter(EqId),
	NextEqId is EqId+1,
	set_eq_counter(NextEqId),
	get_equivalences(Eqs),
	set_equivalences([eqClass(EqId,Tree) | Eqs]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%%  Tree writing procedures
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% print_tree(Tree)
%%

print_tree(Tree) :-
	print_tree(Tree,0).

%%
%% print_tree(Tree,Level)
%%

print_tree($,Level) :-
	!,
	print_val('<empty>',Level).
print_tree(node(Val,$,$),Level) :-
	!,
	print_val(Val,Level).
print_tree(node(Val,Left,Right),Level) :-
	NextLevel is Level+1,
	print_tree(Right,NextLevel),
	print_val(Val,Level),
	print_tree(Left,NextLevel).

print_val(Term,Level) :-
	TabLevel is Level*4,
	tab(TabLevel),
	print_val(Term),
	nl.

print_val(Var) :-
	var(Var),
	!,
	write('_').
print_val(minor(Term)) :-
	!,
	(var(Term) -> write('_') ; write(Term)).
print_val(major(Term)) :-
	!,
	write(Term).
print_val(Term) :-
	write(Term).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%
%%%%%  Shell which ties all of the above together
%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% shell
%%
%%	The top level query to run
%%

shell :-
<<<<<<< 1.3
=======
	init_equiv,
>>>>>>> /tmp/T4a000_3
	repeat,
<<<<<<< 1.3
	get_command(Command),
=======
	get_shell_command(Command),
>>>>>>> /tmp/T4a000_3
	do_command(Command).

<<<<<<< 1.3
get_command(Command) :-
=======
get_shell_command(Command) :-
>>>>>>> /tmp/T4a000_3
	write('Shell> '),
	read(Command).

<<<<<<< 1.3
=======
test :-
	see('mktree1.in'),
	tell('mktree1.out'),
	test1,
	seen,
	told,
	write('Performing diff...'),nl,
	system('diff mktree1.out mktree1.cmp'),
	write('...diff completed'),nl.

test1 :-
	init_equiv,
	repeat,
	get_test_command(Command),
	do_command(Command).

get_test_command(Command) :-
	read(Command),
	nl,write('Command: '),writeq(Command), write('.'),nl.

>>>>>>> /tmp/T4a000_3
do_command(print) :-
	!,
	print_eq,
	fail.
do_command(List) :-
	is_list(List),
	!,
	mktree(List,Tree),
	equiv_member(Tree,EqId),
	!,
	printf("Equivalence Class: %d\n",[EqId]),
	print_tree(Tree),
	nl,writeq(Tree),nl,
	fail.
do_command(help) :-
	printf("\nquit\t\t\t-- quit the shell\n"),
	printf("print\t\t\t-- print equivalence list\n"),
	printf("[treecmd1,treecmd2,...]\t-- build a tree\n"),
	printf("\tmajor(Val)\t-- current node gets preserved value Val\n"),
	printf("\tminor(Val)\t-- current node gets non-preserved value Val\n"),
	printf("\tleft\t\t-- make left subtree the current node\n"),
	printf("\tright\t\t-- make right subtree the current node\n"),
	printf("\tup\t\t-- make parent the current node\n"),
	printf("\tswap\t\t--swap right and left subtrees\n"),
	fail.
do_command(quit) :- 
	!.
<<<<<<< 1.3
=======
do_command(end_of_file) :-
	!.
>>>>>>> /tmp/T4a000_3

is_list([]).
is_list([_|_]).

print_eq :-
	get_equivalences(EqList),
	print_eq(EqList).

print_eq([]) :- !.
print_eq([eqClass(EqId,Tree)|T]) :-
	print_eq(T),
	printf("Equivalence Class: %d\n",[EqId]),
	print_tree(Tree),
	nl.
	
