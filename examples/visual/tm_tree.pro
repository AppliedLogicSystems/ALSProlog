/*================================================================*
 | 		tm_tree.pro  
 | Copyright (c) 1999 by Applied Logic Systems, Inc.
 |
 |		Graphic representation of Prolog terms
 *================================================================*/

init_tree_demo
	:- 
	init_tk_alslib,
	tcl_call( tcli, [package,require,'Iwidgets'], _),
	tcl_call( tcli, [namespace,import,'itcl::*'], _),
	tcl_call( tcli, [source, 'tm_tree.itcl'], _),
	tcl_call( tcli, [init_demo_canvas, '.sc'], CName),
	assert(tree_canvas('.sc',CName)).

:- init_tree_demo.

treeloop
	:-
	write('Input a term:'),
	read(Term),
	disp_treeloop(Term).

disp_treeloop(quit)
	:-
	pix(quit).
disp_treeloop(Term)
	:-
	pix(Term),
	treeloop.


pix(Term)
	:-
	tree_canvas(_, Canvas),
	tcl_call(tcli, [Canvas, delete, demo], _),
	tcl_call(tcli, [wm,deiconify,'.'], _),
	tcl_call(tcli, [raise,'.'], _),
	draw_node(Canvas, Term, 190, 10, 200).

draw_node(Canvas, Term, X, Y, Width)
	:-
	atomic(Term),
	!,
	(atom(Term) -> T0 = Term ; sprintf(atom(T0),'%t',[Term])),
	tcl_call(tcli,['VisualRep','#auto',Canvas,default,T0],Visual),
	tcl_call(tcli,[Visual,draw,X,Y],_).

draw_node(Canvas, Term, X, Y, Width)
	:-
	Term =.. [Func | Args],
	length(Args, LA),
	(LA = 1 ->
		X0 = X, Delx = 0
		;
		X0 is X - (0.5*Width),
		Delx is 1.0*Width /(LA - 1)
	),
	Y0 is Y+50,
	Ex is 0.5*Delx,
	draw_kids(Args, Canvas, X0, X, Y0, Y, Delx,Ex),
	tcl_call(tcli,['VisualRep','#auto',Canvas,default,Func],Visual),
	tcl_call(tcli,[Visual,draw,X,Y],_).

draw_kids([], Canvas, X0, X, Y0, Y, Delx,Ex).
draw_kids([Arg | Args], Canvas, X0, X, Y0, Y, Delx,Ex)
	:-
	tcl_call(tcli,[Canvas,create,line,X,Y,X0,Y0,'-width',2, '-tags',demo],_),
	draw_node(Canvas, Arg, X0, Y0, Ex),
	NX0 is X0+Delx,
	draw_kids(Args, Canvas, NX0, X, Y0, Y, Delx,Ex).


