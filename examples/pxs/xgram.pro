/*---------------------------------------------------------------*
 *	xgram.pro
 *	Copyright (c) 1989-90 Applied Logic Systems, Inc.
 *
 *	Simple DCG rule expander
 *---------------------------------------------------------------*/

expand_grammar_rule( (Left --> Right) ,  ( Head :- Body ) )
	:-
	expand_single(Left, Head,  First,  Second),
	expand_sequence(Right, Body, First, Second).

expand_single( Term, Result,  Arg1,  Arg2)
	:-
	Term =.. [Functor | Term_Args],
	append(Term_Args, [Arg1, Arg2], New_Term_Args),
	Result =.. [Functor | New_Term_Args].

expand_sequence( ( Term,  Rest),   (Exp_Term, Exp_Rest),  InVar,  OutVar)
	:-!,
	expand_single(Term, Exp_Term,  InVar,  InterVar),
	expand_sequence( Rest,   Exp_Rest,  InterVar,  OutVar).


expand_sequence( Term,  Exp_Term,  InVar,  OutVar)
	:-
	expand_single(Term, Exp_Term,  InVar,  OutVar).

append([], Right, Right).
append([Head | Tail], Right, [Head | Result_Tail])
	:-
	append(Tail, Right, Result_Tail).


run(PrologRule)
	:-
	expand_grammar_rule( 
		(verb_phrase(vp(A,V,NP)) --> adverb(A), verb(V), noun_phrase(NP)),
		PrologRule).
