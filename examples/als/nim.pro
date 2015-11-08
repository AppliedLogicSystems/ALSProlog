/*---------------------------------------------------------------------------*
 | 		nim.pro  
 | 	Copyright (c) 1986-2015 by Applied Logic Systems, Inc.
 |
 |	A  simple two player game
 |
 |      Description:                                                          
 |
 |	Plays the game Nim as shown in
 |      The Art of Prolog, Leon Sterling, Ehud Shapiro, The MIT Press, 1986.
 |
 |      Predicate:      play(Game)
 |      Normal usage:   play(nim)
 |
 |      Function:       plays the game with name Game.
 |                     
 |      Reference:      [SS] page 297, Program 18.8.
 *---------------------------------------------------------------------------*/

nim :- play(nim).

play(Game) :-
   initialize(Game,Position,Player),
   display_game(Position,Player),
   play(Position,Player,Result).

play(Position,Player,Result) :-
   game_over(Position,Player,Result), !, announce(Result).
play(Position,Player,Result) :-
   choose_move(Position,Player,Move),
   move(Move,Position,Position1),
   display_game(Position1,Player),
   next_player(Player,Player1),
   !, play(Position1,Player1,Result).

/*
 * Choosing moves
 */

choose_move(Position,opponent,Move) :-
   !,
   repeat,
   write('Which pile do you want to take from? '), 
   read(Pile),
   verify_that_pile_exists(Position,Pile),
   choose_amount(Position, Pile, HowMany),
   Move = (Pile,HowMany).

choose_move(Ns,computer,Move) :-
   !,
   unsafe(Ns,Sum), safe_move(Ns,Sum,Move).
choose_move(Ns,computer,(1,1)) :-	% The computer's `arbitrary move'
   safe(Ns).

verify_that_pile_exists(Position,Pile) :- 
   length(Position,Len), 
   (Len < Pile ; Pile =< 0),  
   !, write('Enter a valid pile> '), nl, fail.
verify_that_pile_exists(_,_).

choose_amount(Position, Pile, HowMany)
   :-
   !,
   repeat,
   write('How many do you want to take? '), 
   read(HowMany),
   validate_amount(Position, Pile, HowMany).

validate_amount(Position, Pile, HowMany)
   :-
   nth(Pile, Position, PileAmt),
   PileAmt < HowMany,
   !,
   write('Amount' = HowMany), write(' is too large.'), nl, 
   fail.
validate_amount(_, _, _).
   




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

display_game(Position,X) :-
   write(Position), nl.

next_player(computer,opponent).
next_player(opponent,computer).

game_over([],Player,Player).

announce(computer) :-
   write('You won!  Congratulations.'), nl.
announce(opponent) :-
   write('I won.'), nl.

initialize(nim,[1,3,5,7],opponent).

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
