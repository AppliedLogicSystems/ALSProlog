
test(Name) :-
	open(Name,read,S),
	!,
	read(S,Term),
	test(Term,S),
	close(S).
test(Name) :-
	write('Nope'),nl.

test(Term,_) :-
	nonvar(Term),
	Term = end_of_file,
	!.
test(Term,S) :-
	pp(Term),nl,
%	write(Term),nl,
	read(S,NextTerm),
	test(NextTerm,S).

testoldparser(Name) :-
	seeing(What),
	see(Name),
	read(X),
	processold(X),
	seen,
	see(What).

processold(Done) :-
	nonvar(Done),
	Done=end_of_file,
	!.
processold(Term) :-
	pp(Term),nl,
	read(X),
	processold(X).

