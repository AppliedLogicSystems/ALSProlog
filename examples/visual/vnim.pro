/*===================================================================*
 | 		vnim.pro  
 |		-- a simple two player game
 |
 |		Visual Version of NIM
 |
 | Original Description: 
 |
 |	Plays the game Nim as shown in
 |      "The Art of Prolog", Leon Sterling, Ehud Shapiro, 
 |		The MIT Press, 1986.
 |
 |	[https://en.wikipedia.org/wiki/Nim]:
 |	Nim is a mathematical game of strategy in which two players 
 |	take turns removing objects from distinct heaps. On each turn, 
 |	a player must remove at least one object, and may remove any 
 |	number of objects provided they all come from the same heap. 
 |	The goal of the game is to be the player to remove the last object.
 *===================================================================*/

/*---------------------------------------------------------------------*
 |      Predicate:      play(Game)
 |      Normal usage:   play(nim)
 |
 |      Function:       plays the game with name Game.
 |      Reference:      [SS] page 297, Program 18.8.
 |
 |		opponent (human player) takes first move
 *---------------------------------------------------------------------*/

module vnim.
use tcltk.
use tk_alslib.

export vnim/0.
vnim
	:-
	initialize_nim(Position,Player).

initialize_nim([1,3,5,7],opponent)
	:-
	init_tk_alslib,
	tcl_call(shl_tcli, [source, 'vn.tcl'], _),
	tcl_call(shl_tcli, [setup_nim], _),
	assert(cur_pos(p(r(1),r(1,1,1),r(1,1,1,1,1),r(1,1,1,1,1,1,1)))).

export user_done_picking/0.
user_done_picking
	:-
	cur_pos(IPos),
	abolish(cur_pos,1),
	compact_cur_pos(IPos, Position),
	abolish(working_row,1),
	check_user_win(Position, IPos).

check_user_win([], _)
	:-!,
	info_dialog('You win!', 'End of Game'),
	quit.

check_user_win(Position, IPos)
	:-
	choose_move(Position,computer,Move),
	move(Move,Position,Position1),
	apply_move(Move, IPos),
	check_computer_win(Position1).

check_computer_win([])
	:-!,
	info_dialog('I, the computer, win!', 'End of Game'),
	quit.

check_computer_win(_).


export quit/0.
quit
	:-
	abolish(working_row,1),
	abolish(cur_pos,1),
	destroy_all_tcl_interpreters.

compact_cur_pos(IPos, List)
	:-
	IPos = p(r(A),r(B,C,D),r(E,F,G,H,I),r(J,K,L,M,N,O,P)),
	R1 is A,
	R2 is B+C+D,
	R3 is E+F+G+H+I,
	R4 is J+K+L+M+N+O+P,
	remove_zeros([R1,R2,R3,R4], List).

remove_zeros([], []).
remove_zeros([0 | Rs], L)
	:-!,
	remove_zeros(Rs, L).
remove_zeros([R | Rs], [R | L])
	:-!,
	remove_zeros(Rs, L).

export select_stick/2.
select_stick(Row, Stick)
	:-
	check_row_ok(Row),
	!,
	sprintf(atom(Cmd), 
		'.vn.p%t.b%t-%t configure  -text x -state disabled',
		[Row, Row, Stick]),
	tcl_call(shl_tcli, Cmd, _),
	cur_pos(P),
	arg(Row, P, RowRep),
	mangle(Stick, RowRep, 0),
	abolish(cur_pos,1),
	assert(cur_pos(P)).

:- dynamic(working_row/1).
    %% working_row records the row from which the user has stared removing 
    %% sticks for this user-round of removals;  So check_row_ok/1 determines
    %% if subsequent clicks in this round are still on that some initial row.
    %% When the user says "I'm done choosing", then in user_done_picking/0,
    %% working_row/1 is abolished.
check_row_ok(Row)
	:-
	working_row(WR),
	!,
	    %% Is selected row the same as previously?:
	fin_check_row_ok(WR, Row).

	%% No rows were previously clicked on
	%% during this user-round, so this
	%% is the first; record it:
check_row_ok(Row)
	:-
	assert(working_row(Row)).

	%% Yes, the selected row is the same as previously:
fin_check_row_ok(Row, Row) :-!.

	%% No, so ring the bell to alert the user:
fin_check_row_ok(_, _)
	:-
	tcl_call(shl_tcli, bell, _),
	!,
	fail.

apply_move((Row, N), IPos)
	:-
	arg(Row, IPos, PosRow),
	non_empty(PosRow),
	!,
	apply_to_row(PosRow, N, NPosRow, Row),
	mangle(Row, IPos, NPosRow),
	assert(cur_pos(IPos)).

apply_move((Row, N), IPos)
	:-
	Row =< 4,
	NRow is Row+1,
	apply_move((NRow, N), IPos).

non_empty(Row)
	:-
	functor(Row, _, Size),
	sum_args(1,Size,Row,0,Sum),
	!,
	Sum > 0.

sum_args(Lim,Lim,Term,CurSum,Sum)
	:-!,
	arg(Lim, Term, V),
	Sum is CurSum + V.
sum_args(N,Lim,Term,CurSum,Sum)
	:-
	arg(N, Term, V),
	NextSum is CurSum + V,
	M is N+1,
	sum_args(M,Lim,Term,NextSum,Sum).

apply_to_row(OR, PR, NR, Row)
	:-
	OR =.. [F | As],
	app_n_args(As, PR, NAs, Row, 1),
	NR =.. [F | NAs].

app_n_args([], _, [],_, _) :-!.
app_n_args(As, 0, As,_, _) :-!.
app_n_args([0 | As], PR, [0 | NAs], Row, StickN)
	:-!,
	StickM is StickN+1,
	app_n_args(As, PR, NAs, Row,StickM).
app_n_args([1 | As], PR, [0 | NAs], Row,StickN)
	:-
	sprintf(atom(Cmd), 
		'.vn.p%t.b%t-%t configure -text c -state disabled',
		[Row, Row, StickN]),
	tcl_call(shl_tcli, Cmd, _),
	NPR is PR-1,
	StickM is StickN+1,
	app_n_args(As, NPR, NAs, Row,StickM).


choose_move(Ns,computer,Move) :-
   !,
   unsafe(Ns,Sum), 
   safe_move(Ns,Sum,Move).
choose_move(Ns,computer,(1,1)) :-	% The computer's `arbitrary move'
   safe(Ns).

/*
 *      Predicate:      move(Move,Position,Position1)
 *      Normal usage:   move(in,in,out)
 *
 *      Function:       Position1 is the result of executing the move Move from
 *	                the current position
 *                     
 *      Reference:      [SS] page 340, Program 20.2.
 */

move((K,M),[N|Ns],[N|Ns1]) :-
   K > 1, K1 is K-1, move((K1,M),Ns,Ns1).
move((1,N),[N|Ns],Ns).
move((1,M),[N|Ns],[N1|Ns]) :-
   N>M, N1 is N-M.

next_player(computer,opponent).
next_player(opponent,computer).

game_over([],Player,Player).

announce(computer) :-
   write('You won!  Congratulations.'), nl.
announce(opponent) :-
   write('I won.'), nl.


/*
 *      Predicate:     unsafe(Position,Sum)
 *      Normal Usage:  unsafe(in,out)
 *
 *	Function:      checks to see if Position with nim-sum Sum is unsafe.
 *
 *	Reference:     [SS], page 340, Program 20.2.
 */

unsafe(Ns,Sum) :- nim_sum(Ns,[],Sum), not zero(Sum).
safe(Ns) :- not unsafe(Ns,Sum).

/*
 *      Predicate:     nim_sum(Position,SoFar,Sum)
 *	Normal Usage:  nim_sum(in,in,out)
 *
 *      Function:      Sum is the nim-sum of the current Position, and
 *                     SoFar is the accumulated value.
 *
 *      Reference:     [SS], page 340, Program 20.2.
 */

nim_sum([N|Ns],Bs,Sum) :-
   binary(N,Ds), nim_add(Ds,Bs,Bs1), nim_sum(Ns,Bs1,Sum).
nim_sum([],Sum,Sum).

nim_add(Bs,[],Bs).
nim_add([],Bs,Bs).
nim_add([B|Bs],[C|Cs],[D|Ds]) :-
   D is (B+C) mod 2, nim_add(Bs,Cs,Ds).

binary(1,[1]).
binary(N,[D|Ds]) :-
   N>1, D is N mod 2, N1 is N div 2, binary(N1,Ds).

decimal(Ds,N) :- decimal(Ds,0,1,N).
decimal([],N,T,N).
decimal([D|Ds],A,T,N) :- A1 is A+D*T, T1 is T*2, decimal(Ds,A1,T1,N).

zero([]).
zero([0|Zs]) :- zero(Zs).

/*
 *      Predicate:    safe_move(Position,NimSum,Move)
 *	Normal Usage: safe_move(in,in,out)
 *
 *      Function:     Move is a move from the current Postion with value
 *                    NimSum which leaves a safe position.
 *      Reference:    [SS], Page 341, Program 20.2.
 */

safe_move(Piles,NimSum,Move) :-
   safe_move(Piles,NimSum,1,Move).

safe_move([Pile|Piles],NimSum,K,(K,M)) :-
   binary(Pile,Bs), can_zero(Bs,NimSum,Ds,0), decimal(Ds,M).
safe_move([Pile|Piles],NimSum,K,Move) :-
   K1 is K+1, safe_move(Piles,NimSum,K1,Move).

can_zero([],NimSum,[],0) :-
   zero(NimSum).
can_zero([B|Bs],[0|NimSum],[C|Ds],C) :-
   can_zero(Bs,NimSum,Ds,C).
can_zero([B|Bs],[1|NimSum],[D|Ds],C) :-
   D is 1-B*C, C1 is 1-B, can_zero(Bs,NimSum,Ds,C1).

endmod.
