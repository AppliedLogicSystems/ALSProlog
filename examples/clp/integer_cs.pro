/*===================================================================*
 |		integer_cs.pro
 |
 |	Integer & finite-domain examples with CLP(BNR) interval constraints.
 *===================================================================*/

module clptest.
use rel_arith.

export s1/2.
s1(N,M) :-
	[N,M] :: integer(1,5),
	{ N < 4, N + M == 8 }.

export s2/2.
s2(N,M) :-
	[N,M] :: integer(1,15),
	{ 2*N + M == 11, N + 2*M ==13 }.

	%%-------------------------------------------------------------
	%%			Crypto-Arithmetic
	%%-------------------------------------------------------------

export sendmoremoney/0.
sendmoremoney  
	:-
	sendmoremoney(L),  
	enumerate(L),
	write('L'=L),nl.

export smm/0.
smm :-
	L = [S,E,N,D,M,O,R,Y],
	LNs = ['S','E','N','D','M','O','R','Y'],
	sendmoremoney(L),
	enumerate(L),
	show_v_list(L,LNs),
	check_smm(L).

export sendmoremoney/1.
sendmoremoney([S,E,N,D,M,O,R,Y])
	:-
	[S,M] :: integer(1,9),
	[E,N,D,O,R,Y] :: integer(0,9),
	distinct( [S,E,N,D,M,O,R,Y] ),
	{ 1000*(S+M) + 100*(E+O) + 10*(N+R) + (D+E)
		== 10000*M + 1000*O + 100*N + 10*E + Y}.

export sendmoremoney_x/1.
sendmoremoney_x(L)
	:-
	sendmoremoney(L),
	enumerate(L).

distinct([]).
distinct([X | Xs])
	:-
	distinct_from(Xs, X),
	distinct(Xs).

distinct_from([], _).
distinct_from([X | Xs], Y)
	:-
	{X <> Y},
	distinct_from(Xs, Y).

check_smm(L)
	:-
	L = [S,E,N,D,M,O,R,Y],
	Top is 1000*(S+M) + 100*(E+O) + 10*(N+R) + (D+E),
	Bot is 10000*M + 1000*O + 100*N + 10*E + Y,
	printf('\nTop Value=%t  Bottom Value=%d\n',[Top,Bot]).

/*
show_v_list([],[]).
show_v_list([X | L],[VN | LNs])
	:-
	show_variable(VN,X),
	show_v_list(L,LNs).

show_v_list([]).
show_v_list([X | L])
	:-
	show_variable(X),
	show_v_list(L).
*/

	%%-------------------------------------------------------------
	%%			Eight Queens
	%%-------------------------------------------------------------

export q/1.
q(N) :-
	n_queens(N, Positions),
	enumerate(Positions),
	printf('%t\n', [Positions]),
	fail.
	
export n_queens/2.
n_queens(N, Positions)
	:-
	is_length(Positions, N),
	Positions :: integer(1, N),
	queens(N, Positions).

queens(0, []) :-!.

queens(N, [P | Positions])
	:-
	N1 is N - 1,
	c_noattack(Positions, P, 1),
	queens(N1, Positions).

c_noattack([], _, _).
c_noattack([P | Positions], Pos, N)
	:-
	N1 is N + 1,
	{ Pos <> P, Pos <> P-N, Pos <> P+N },
	c_noattack(Positions, Pos, N1).


	%%-------------------------------------------------------------
	%%			Mastermind
	%%-------------------------------------------------------------

export mastermind/0.
mastermind 
	:-
	mastermind_1([], []).

mastermind_1(Gs, Ss)
	:-
	make_a_guess(Gs, Gs, G),
	!,
	get_score(G, S),
	not( S = [$ | _] ),
	mastermind_1([G | Gs], [S | Ss]).

make_a_guess( Previous_Guesses, Previous_Scores, G)
	:-
	newguess(G),
	matches_all( Previous_Guesses, Previous_Scores, G),
	enumerate(G).

matches_all([], [], _).
matches_all( [G | Gs], [S | Ss], New)
	:-
	score(G, New, S),
	matches_all(Gs, Ss, New).

get_score( G, S)
	:-
%	answer(A),
	printf('%t >>',[G]),flush_output,
	read(A),
	score(G, A, S).

	%% Defines the space of possible answers:

newguess(G) 
	:-
	G = [A, B, C, D],
	G::integer(1, 9),
	{A <> B, A <> C, A <> D, B <> D, C <> D}.

score(Answer, Guess, [B, C])
	:-
	bulls(Guess, Answer, B),
	cowsbulls(Guess, Answer, C).

bulls(Xs, Ys, N)
	:-
	count_equal(Xs, Ys, C),
	{N == (C)}.

count_equal([], [], 0).
count_equal([G | Gs], [A | As], (G==A) + S)
	:-
	count_equal(Gs, As, S).

cowsbulls(Guess, Answer, C)
	:-
	cows_and_bulls(Guess, Answer, S),
	{C == S}.

cows_and_bulls([], _, 0).
cows_and_bulls([X | Xs], Ys, N+J)
	:-
	in(X, Ys, J),
	cows_and_bulls(Xs, Ys, N).

in(X, Ys, N)
	:-
	N::integer(0,1),
	is_in(Ys, X, M),
	{N == M}.

is_in([], _, 0).
is_in([Y | Ys], X, (Y==X) + S)
	:-
	is_in(Ys, X, S).




endmod.
