/*
 * alabel.pro		-- label handling package
 *	Copyright (c) 1990 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 4/11/90
 * Revision History:
 *
 * Module Name:	labels
 *
 * Uses Module(s):
 *		avl
 *
 * Exported Procedures:
 *
 *	define_label(Label,LID,InDB,OutDB)
 *
 *	forward_label(Label,LID,InDB,OutDB)
 *
 *	backward_label(Label,LID,DB)
 *
 *	expression_label(Expression,LID,InDB,OutDB)
 *
 *	expression_labels(LabelList,ExpressionList,DB)
 *
 *	global_label(Label,InDB,OutDB)
 *
 *	is_global_label(Label,InDB)
 *
 *	In each case above, Label is some ground term.  For the application
 *	which this package is designed for Label is actually an integer, but
 *	it may be any ground term.  LID is an integer which will be returned
 *	as an output argument.  It will uniquely identify Label.  InDB and
 *	OutDB are avl trees created with avl_create.  InDB is an input argument
 *	and OutDB is an output argument (appropriately enough).  These
 *	trees form the label database.
 *
 *	At the source assembly language level, the programmer may write
 *	something of the form
 *
 *		N:
 *
 *	where N is a non-negative integer.  In such a situation, the translator
 *	will call define_label/4 with the integer N as the first argument. An
 *	integer will be unified with the second argument.  This integer form
 *	part of the new identifier for the output label.  The same integer
 *	N may appear a number of times as a label; each instance will be
 *	assigned a new label identifier.
 *
 *	When the label appears in an instruction, it will be suffixed with
 *	a 'b' or an 'f', e.g.,
 *
 *			bne	1b
 *
 *	In this case the label refers to a previous defining occurence the
 *	label 1.  Calling backward_label/3 will return the label identifier.
 *	backward_label/3 will fail if the label is not found.  This failure
 *	should be used for error detection.
 *
 *	When a label is suffixed with an f, the label identifier will refer
 *	to the future defining occurence of the label.  forward_label/4 is
 *	called to obtain the label identifier.
 *
 *	expression_label/4 is called in circumstances where an expression
 *	is encountered which is too complex (due to one or more of the
 *	components of the the expression being unknown).  It allocates
 *	a label identifier and puts the label in the database for future
 *	reference.
 *
 *	expression_labels/3 is called to retrieve all of the labels entered
 *	into the database with expression_label/4.  The labels are returned
 *	in the first argument and the corresponding expressions in the second.
 *
 *	global_label/3 is called to insert a global label into the database.
 *
 *	is_global_label/2 checks to see a label is a global one in the
 *	database.
 *	
 */

module labels.

use avl.

export define_label/4, forward_label/4, backward_label/3.


define_label(Label,LID,InDB,OutDB) :-
    avl_search(flab(Label),LID,InDB),
    LID \= invalid,
    !,
    avl_insert(flab(Label),invalid,InDB,IDB),
    avl_insert(blab(Label),LID,IDB,OutDB).
define_label(Label,LID,InDB,OutDB) :-
    next_label(LID,InDB,IDB),
    avl_insert(blab(Label),LID,IDB,OutDB).

forward_label(Label,LID,DB,DB) :-
    avl_search(flab(Label),LID,DB),
    LID \= invalid,
    !.
forward_label(Label,LID,InDB,OutDB) :-
    next_label(LID,InDB,IDB),
    avl_insert(flab(Label),LID,IDB,OutDB).

backward_label(Label,LID,DB) :-
    avl_search(blab(Label),LID,DB).


export expression_label/4, expression_labels/3.

expression_label(LID,Expression,InDB,OutDB) :-
    next_label(LID,InDB,IDB),
    avl_insert(elab(LID),Expression,IDB,OutDB).

expression_labels(LabelList,ExpressionList,DB) :-
    avl_inorder(DB,NodeList),
    el(NodeList,LabelList,ExpressionList,DB).

el([],[],[],_) :- !.
el([elab(LID)|T1],[LID|T2],[Exp|T3],DB) :-
    !,
    avl_search(elab(LID),Exp,DB),
    el(T1,T2,T3,DB).
el([_|T1],L2,L3,DB) :-
    el(T1,L2,L3,DB).

next_label(L,InDB,OutDB) :-
    avl_search(label(counter),L,InDB),
    !,
    NL is L+1,
    avl_insert(label(counter),NL,InDB,OutDB).
next_label(0,InDB,OutDB) :-
    avl_insert(label(counter),1,InDB,OutDB).


export global_label/3, is_global_label/2.

global_label(Lab,InDB,OutDB) :-
    avl_insert(glab(Lab),[],InDB,OutDB).

is_global_label(Lab,DB) :-
    avl_search(glab(Lab),_,DB),
    !.

endmod.		%% labels
