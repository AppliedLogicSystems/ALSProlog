

prf(A,N,P)
	:-
	sub_atom(A,1,N,P).


sub_prf([], N, P, []).
sub_prf([A/_ | L], N, P, [A | SL])
	:-
	prf(A,N,P),
	!,
	sub_prf(L, N, P, SL).
sub_prf([_ | L], N, P, SL)
	:-
	sub_prf(L, N, P, SL).

sl(M) :-
	scrf(7,L),
	sub_prf(L,2, 'a_',M).

