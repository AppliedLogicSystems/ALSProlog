/*
 * Compiler  Tests
 */



/*
 * Too many subgoals
 */
compile_test1(N) :-
	create_nested_termcomma(N,T),
	nl, write('Trying to assert (c1(N) :- Term) where Term has '),
		write(N), write(' subgoals.'), !,
	assert((c1(N) :- T)), !,
	nl, write('Test is successful.').

create_nested_termcomma(1,q) :- !.
create_nested_termcomma(N,','(q,T)) :- 
	M is N - 1, 
	create_nested_termcomma(M,T).


/*
 * Too many variables
 */
compile_test2(N) :-
	create_nested_termcomma5(N,T),
	nl, write('Trying to assert (c2(N) :- Term) where Term has '),
		write(N), write(' variables.'), !,
	assert((c2(N) :- T)), !,
	nl, write('Test is successful.').

create_nested_termcomma5(1,q(_)) :- !.
create_nested_termcomma5(2,q(_,_)) :- !.
create_nested_termcomma5(3,q(_,_,_)) :- !.
create_nested_termcomma5(4,q(_,_,_,_)) :- !.
create_nested_termcomma5(5,q(_,_,_,_,_)) :- !.
create_nested_termcomma5(N,','(q(_,_,_,_,_),T)) :- 
	M is N - 5, 
	create_nested_termcomma5(M,T).




