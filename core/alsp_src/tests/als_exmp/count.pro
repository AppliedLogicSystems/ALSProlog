/*
 * count(+ListOfFiles) or count(+SingleFile)
 *
 * For each file in the ListOfFiles input, or for the SingleFile input, a count
 * of the total number of predicates (per file), clauses, directives, goals and
 * total lines of code (LOC) is given.  Clause count includes predicate count.
 * LOC is the sum of clauses, directives, and goals.  
 *
 *  A filename extension of ".pro" is assumed, if not given explicitly.  If a
 *  filename does not end in ".pro", then preceed the filename (outside of any
 *  quotes, if needed) by a plus sign.  For example, the count/1 argument may
 *  be: +'foo/bar/file1.dat', or [..., +'foo/bar/file1.dat', ...].  (Make two
 *  indicated changes to use ".pl" instead of ".pro" as the default extension.)
 *
 *  If a file consults or reconsults another file, the re/consulted file is
 *  shown first with the "calling" file listed in braces.
 *
 *  This program should be run from the directory where the program being
 *  "counted" would run to avoid directory path problems within re/consults.
 *  If all re/consulted files have fully specified paths, then this program can
 *  be run from any directory.
 *
 * This program has been extended from the original by Peter Reintjes, who
 * can be reached at pbr@quine.sofla.ingr.com, or (407)498-2809.
 *
 * Changes include counting of directives and LOC, collecting stats from
 * consulted and reconsulted files, hanlding definite clause grammar notation,
 * modified output format, and a few bug fixes for pathological cases: handling
 * op/3 with uninstantiated arguments, accepting filenames without ".pro" or
 * ".pl" when needed, not handling periods in the file path specification, and
 * not counting multiple goals within a negation.
 *
 * This version of the program runs on ALS Prolog and each of the following
 * is used once, but may not run as is on other "standard" Prolog systems:  The
 * predicates dreverse/2 and dappend/3 are determinate forms of the standard
 * (common) reverse/2 and append/3, which will also work.  The predicate
 * length/2 is also an ALS builtin.  The builtin ttyflush/0 forces terminal
 * output, and may be removed without altering the final output of the program.
 * The predicate exists_file/1 determines if a file exists, and is used to avoid
 * OS type errors when trying to see/read non-existant files.  If exists_file/1 is
 * not availiable (or equivalent) replace it with atom/1, at a minimum.
 * Also note that "0'x" is the ISO standard for representing the character code
 * for "x", which may not be supported on all Prolog compilers. 
 *
 * Below is a typical usage:
 *
 *----------------------------------------------------------------------
 * ?- count([count, 'foo.pro', '../bar']).
 *
 * Counting: Predicates, Clauses, Directives, Goals, Lines Of Code
 *
 *       P      C      D      G    LOC   File name
 *
 *      17     44      0    145    189   count.pro
 *       7     22      0     35     57   subfoo.pro  {foo.pro}
 *      73    361      1   1043   1405   foo.pro
 *      43    132     14    398    544   ../bar.pro
 *  ------ ------ ------ ------ ------
 *     140    559     15   1621   2195   Total
 *
 * Yes.
 * ?-
 *----------------------------------------------------------------------
 *
 * M. Alan Newman, alan@geg.mot.com, (602)441-5713.
 * Motorola GSTG AI Laboratory, Scottsdale, Arizona
 * Last update: 9/3/93.
 */

count(File) :-
	File \= [_|_] ,
	count([File]) .
count(Files) :- nl ,
	write(
	  'Counting: Predicates, Clauses, Directives, Goals, and Lines Of Code') , 
	nl , column6(s('P','C','D','G','LOC'), 'File name') , nl , ! ,
	each(Files, s(0,0,0,0,0)) .

each([], S) :- 
	column6(s('------', '------', '------', '------', '------'), '') ,
	column6(S, 'Total') , nl .
each([File|Files], S0) :-
	one_file(File, S1, []) ,
	add_counts(S0, S1, S2) ,
	each(Files, S2) .
each([File|Files], S) :- 			% file error
	seen ,
	nl, tab(38) , write('Bad filename: ') , write(File) ,
	each(Files, S) .


one_file(File, S3, Consult) :-
	filename_extension(File, Name) ,
	exists_file(Name) , 				% use atom/1 if no exists/1
	see(Name) ,
	read(Clause) ,
	clauses(Clause, _,[], s(0,0,0,0,0),S1, s(0,0,0,0,0),S2, Name) ,
	seen ,
	column7(S1, Name, Consult) ,
	add_counts(S1, S2, S3) , ! .


/*
 * Count predicates and clauses (current file is INTernal count,
 *				 re/consulted files are EXTernal count)
 */

clauses(end_of_file, _,_, Int,Int, Ext,Ext,_) . 	  	% end of file 
clauses((:-Directive), _,_, Int0,Int2, Ext0,Ext2, File) :- ! ,	% directives
	goals(Directive, s(0,0,0,0,0),s(_,_,_,_,G), Ext0,Ext1, File) ,
	add_counts(s(0,0,G,0,G), Int0, Int1) ,	% (use goals/6, then transfer
	read(Next) ,				%  goals count to directives)
	clauses(Next, _,[], Int1,Int2, Ext1,Ext2, File) .
clauses((Rule:-Body), F,A, Int0,Int3, Ext0,Ext2, File) :- 	% normal clauses
	functor(Rule, F,A) , ! ,
	goals(Body, Int0,Int1, Ext0,Ext1, File) ,
	count_clause(Int1, Int2) ,
	read(Next) ,
	clauses(Next, F,A, Int2,Int3, Ext1,Ext2, File) .
clauses((DCG-->Body), F,A2, Int0,Int3, Ext0,Ext2, File) :-	% DGC clauses
	functor(DCG, F,A) , 
	A2 is A + 2 , ! ,  			% (allow mixing DCG & non-DCG)
	dcg_goals(Body, Int0,Int1, Ext0,Ext1, File) ,
	count_clause(Int1, Int2) ,
	read(Next) ,
	clauses(Next, F,A, Int2,Int3, Ext1,Ext2, File) .
clauses(Fact, F,A, Int0,Int2, Ext0,Ext1, File) :-		% facts
	functor(Fact, F,A) , ! ,
	count_clause(Int0, Int1) ,
	read(Next) ,
	clauses(Next, F,A, Int1,Int2, Ext0,Ext1, File) .
clauses(Predicate, _,_, Int0,Int2, Ext0,Ext1, File) :-		% next predicate
	count_predicate(Int0, Int1) ,
	clauses(Predicate, _,_, Int1,Int2, Ext0,Ext1, File) .


/*
 * Count goals
 */

goals(Call, Int0,Int1, Ext,Ext, _) :-			% implied call/1 with 
	var(Call) , ! ,					%	uninstantiated
	count_goal(Int0, Int1) .			%	goal
goals((A,B), Int0,Int2, Ext0,Ext2, File) :- ! , 	% conjunctions 
	goals(A, Int0,Int1, Ext0,Ext1, File) ,		%	(not counted
	goals(B, Int1,Int2, Ext1,Ext2, File) .		%	 as goals)
goals((A;B), Int0,Int2, Ext0,Ext2, File) :- ! , 	% disjunctions
	goals(A, Int0,Int1, Ext0,Ext1, File) ,		%	(not counted)
	goals(B, Int1,Int2, Ext1,Ext2, File) .
goals((A->B),Int0,Int3, Ext0,Ext2, File) :- ! , 	% if-then 
	count_goal(Int0, Int1) ,
	goals(A, Int1,Int2, Ext0,Ext1, File) ,
	goals(B, Int2,Int3, Ext1,Ext2, File) .
goals(\+(A), Int0,Int2, Ext0,Ext1, File) :- ! , 	% \+
	count_goal(Int0, Int1) ,
	goals(A, Int1,Int2, Ext0,Ext1, File) .
goals(not(A),Int0,Int2, Ext0,Ext1, File) :- ! , 	% not
	count_goal(Int0, Int1) ,
	goals(A, Int1,Int2, Ext0,Ext1, File) .		% single goals
goals(op(A,B,C), Int0,Int1, Ext,Ext, _) :- 		% op/3 with 
	nonvar(A) , nonvar(B) , nonvar(C) , ! ,		%	instantiated
	op(A,B,C) , 					%	arguments
	count_goal(Int0, Int1) .
goals(consult(External),      I0,I1, E0,E1, File) :- !, % consults
	external([External],  I0,I1, E0,E1, File) .
goals(reconsult(External),    I0,I1, E0,E1, File) :- !, % reconsults
	external([External],  I0,I1, E0,E1, File) .
goals([Ext|Files],	      I0,I1, E0,E1, File) :- !, % re/consult lists
	external([Ext|Files], I0,I1, E0,E1, File) .
goals(_, Int0,Int1, Ext,Ext, _) :- 
	count_goal(Int0, Int1) .


/*
 * Count external files consulted or reconsulted
 */

external([], Internal,Internal, External,External, File) :-
	see(File) .
external([External|Rest], Int0,Int2, Ext0,Ext2, File) :-
	one_file(External, Ext, File) ,
	count_directive(Int0, Int1) ,
	add_counts(Ext, Ext0, Ext1) ,
	external(Rest, Int1,Int2, Ext1,Ext2, File) .
external([_|Rest], Int0,Int2, Ext,Ext, File) :-		% error escape
	count_directive(Int0, Int1) ,
	external(Rest, Int1,Int2, Ext,Ext, File) .


/*
 * Count definite clause grammar clauses
 */

dcg_goals(Call, Int0,Int1, Ext,Ext, _) :-		% uninstantiated goal
	var(Call) , ! ,
	count_goal(Int0, Int1) .
dcg_goals((A,B), Int0,Int2, Ext0,Ext2, File) :- ! , 	% conjunctions
	dcg_goals(A, Int0,Int1, Ext0,Ext1, File) ,	%	(not counted)
	dcg_goals(B, Int1,Int2, Ext1,Ext2, File) .
dcg_goals((A;B), Int0,Int2, Ext0,Ext2, File) :- ! , 	% disjunctions
	dcg_goals(A, Int0,Int1, Ext0,Ext1, File) ,	%	(not counted)
	dcg_goals(B, Int1,Int2, Ext1,Ext2, File) .
dcg_goals({A}, Int0,Int1, Ext0,Ext1, File) :- ! , 	% non-DCG goals
	goals(A, Int0,Int1, Ext0,Ext1, File) .
dcg_goals(_, Int0,Int1, Ext,Ext, _) :- 			% single DCG goals
	count_goal(Int0, Int1) .			% 	(counted with 
							%	 non-DCG goals) 

/*
 * Allow implied ".pro" filename extensions
 */

filename_extension(+ File, File) :- ! , atom(File) .
filename_extension(File, File) :-
	name(File, List) ,
	dreverse(List, [0'o,0'r,0'p,0'.|_]) , ! . % "lp.", if you prefer!
filename_extension(File, Name) :-
	name(File, List) ,
	dappend(List, ".pro", Listpro) ,	  % '.pl', if you prefer!
	name(Name, Listpro) .


/*
 * Adjust appropriate tallies
 */

count_predicate(s(P,C,D,G,L), s(P1,C,D,G,L )) :- P1 is P+1 .

count_clause(   s(P,C,D,G,L), s(P,C1,D,G,L1)) :- C1 is C+1 , L1 is L+1 .

count_directive(s(P,C,D,G,L), s(P,C,D1,G,L1)) :- D1 is D+1 , L1 is L+1 .

count_goal(     s(P,C,D,G,L), s(P,C,D,G1,L1)) :- G1 is G+1 , L1 is L+1 .

add_counts(s(P0,C0,D0,G0,L0), s(P1,C1,D1,G1,L1), s(P2,C2,D2,G2,L2)) :-
	P2 is P0+P1 , C2 is C0+C1 , D2 is D0+D1 , G2 is G0+G1 , L2 is L0+L1 .


/*
 * Format output table rows
 */

column7(A,B,C) :- column6(A,B) , seventh_col(C) , ttyflush , ! .

seventh_col([]) .				% show calling file if current
seventh_col( C) :- tab(2) , write({C}) .	% one had been re/consulted

column6(s(A,B,C,D,E), F) :- nl , 
	right(A) , right(B) , right(C) , right(D) , right(E) , 
	tab(3) , write(F) .

right(A) :- 
	name(A, S) , 
	length(S, L) , 
	D is 7-L , 		% Over 1000000 LOC?  Then use "D is 8-L"!
	tab(D) , write(A) .
