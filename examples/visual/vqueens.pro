/*================================================================*
 | 		vqueens.pro  
 | Copyright (c) 1987-1988, 1998 by Applied Logic Systems, Inc.
 |
 |		Visual version of:
 |			queens.pro
 |			-- placing queens on a chessboard benchmark
 *================================================================*/

/*----------------------------------------------------------------*
 |	Original Description:
 |
 | 	This is a benchmark which tests the speed of bagof (among 
 |	other things). It provides a different mix of operations than 
 |	the nrev benchmark.  This benchmark is invoked by entering the 
 |	following goal
 |
 |		?- all_queens.
 |
 | 	It will print out the number of solutions found and the time 
 |	it took to find these solutions.  There are 92 solutions 
 |	and ALS-Prolog takes about 78 seconds on a standard IBM-PC.
 |	It takes about 2 seconds on a Sun 3/260.
 |
 +---------------------------------------------------------------
 |	Visual version additions:
 |
 |	This version makes a minimal change to the original, as
 |	follows:
 |
 |	+	a chessboard is shown;
 |	+	after each solution is found, it is displayed on
 |		the chessboard, and the program waits for the
 |		user to click on the Next button;
 |	+	after clicking the Next button, the next solution is 
 |		sought;
 |	+	clicking the All button finds the rest without pausing.
 |
 |	The tcl/tk part of the code is in vq.tcl.
 *----------------------------------------------------------------*/

all_queens
	:- 
	abolish(display_mode,1),
	assert(display_mode(image)),
	all_queens0.

all_queens_none
	:- 
	abolish(display_mode,1),
	assert(display_mode(none)),
	all_queens0.

all_queens_text
	:- 
	abolish(display_mode,1),
	assert(display_mode(text)),
	all_queens0.

all_queens0
	:- 
	init_tk_alslib,
	tcl_call(tcli, [source, 'vq.tcl'], _),
	tcl_call(tcli, [image,create,photo,qcrown,'-file','queen_crown_icon.gif'],_),
	tcl_call(tcli, [image,create,photo,blank,'-file','blank_crown_icon.gif'],_),
	InitTime is cputime,
	tcl_call(tcli, [chessboard], _),
	generate_board(8),
	bagof(X,get_solutions(X),L),
	length(L,N),
	DeltaTime is cputime-InitTime,
	write('Number of Solutions'=N), nl,
	write('Time'=DeltaTime), nl.


size(8).
int(1).
int(2).
int(3).
int(4).
int(5).
int(6).
int(7).
int(8).

get_solutions(Soln) :- solve([], Soln).

	%%	newsquare generates legal positions for next queen:
newsquare([], square(1, X)) :- int(X).
newsquare([square(I, J) | Rest], square(X, Y)) :-
	X is I + 1,
	int(Y),
	not_threatened(I, J, X, Y),
	safe(Rest, X, Y).


	%%	safe checks whether square(X, Y) is threatened by any
	%%	existing queens:
safe([], X, Y).
safe([square(I,J) | L], X, Y) :-
	not_threatened(I, J, X, Y),
	safe(L, X, Y).


not_threatened(I, J, X, Y) :-
	I =\= X,
	J =\= Y,
	I-J =\= X-Y,
	I+J =\= X+Y.

	%%	solve accumulates the positions of occupied squares:
solve([square(Bs, Y) | L], [square(Bs, Y) | L]) 
	:- 
	size(Bs),
	show_soln([square(Bs, Y) | L]).

solve(Initial, Final) :-
	newsquare(Initial, Next),
	solve([Next | Initial], Final).

:- dynamic(wait_for_next/0).
	%% initial value:
wait_for_next.

show_soln(Soln)
	:-
	display_mode(M),
	(M = none -> true 
		; 
		display_soln(Soln, M),
		tcl_call(tcli, [update],_),
		update_soln_num,
		(wait_for_next ->
			tcl_call(tcli, [wait_on_button],_)
			;
			true
		),
		!,
		tcl_call(tcli, [clear_board,M], _)
	).

display_soln([], M).
display_soln([square(R,C) | Soln], M)
	:-
	R0 is R-1, C0 is C-1,
	sname(R0,C0,SN),
	(M = text ->
		tcl_call(tcli, [SN,configure,'-text','Q'],_)
		;
		tcl_call(tcli, [SN,configure,'-image',qcrown],_)
	),
	display_soln(Soln, M).

generate_board(N)
	:-
	generate_board(0,N),
	tcl_call(tcli,[raise,'.vq'],_).

generate_board(M,N)
	:-
	M < N, !,
	generate_row(0,N,M),
	NextM is M+1,
	generate_board(NextM,N).

generate_board(M,N).

generate_row(K,N,M)
	:-
	K >= N,
	!.
generate_row(K,N,M)
	:-
	sname(M,K,SN),
	tcl_call(tcli, [label,SN,'-relief',groove],_),
	tcl_call(tcli, [grid,SN,'-row',M,'-column',K,'-sticky','nsew'],_),
	NextK is K+1,
	generate_row(NextK,N,M).

sname(M,K,SN)
	:-
	catenate(['.vq.bd.s',M,'-',K],SN).

place_queen(square(R,C))
	:-
	sname(R,C,SN),
%	tcl_call(tcli, [SN,configure,'-text','Q'],_),
	tcl_call(tcli, [SN,configure,'-image',qcrown],_),
	tcl_call(tcli, [update],_).

place_queen(square(R,C))
	:-
	sname(R,C,SN),
	tcl_call(tcli, [SN,configure,'-text',''],_),
	tcl_call(tcli, [SN,configure,'-image',blank],_),
	tcl_call(tcli, [update],_).

update_soln_num
	:-
    tcl_call(tcli, ['.vq.info.soln_num','cget','-text'],Prev),
	atomread(Prev,PrevNum),
	NextNum is PrevNum+1,
    tcl_call(tcli, ['.vq.info.soln_num','configure','-text',NextNum],_).

find_all_the_rest
	:-
	retract(wait_for_next).
