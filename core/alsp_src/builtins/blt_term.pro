/*=====================================================================
 *		blt_term.pro
 *		Copyright (c) 1986-1992 Applied Logic Systems, Inc.
 *
 *	Builtin predicates for manipulating terms
 *
 *	Authors: Kevin A. Buettner, Ken Bowen, Chris White,
 *	         Keith Hughes, Ilyas Cicekli
 *	Original Creation Date: 3/20/86
 *====================================================================*/

module builtins.
 

/*
 * List Manipultation Predicates
 *
 *	append/3, dappend/3, member/2, dmember/2, reverse/2, dreverse/2,
 *	length/2
 *
 *	dappend/3 and dmember/2 are defined in builtins.pro since they
 *	are needed for certain initializations.
 */ 

export append/3.

append([],L,L).
append([H|T],L,[H|TL]) :- append(T,L,TL).
    
export member/2.

member(Item,[Item|_]).
member(Item,[_|Rest]) :- member(Item,Rest).

export reverse/2.
export dreverse/2.

reverse(List,Rev) :- reverse(List,[],Rev).

reverse([],Rev,Rev).
reverse([A|Rest],SoFar,Rev) :- reverse(Rest,[A|SoFar],Rev).

dreverse(List,Rev) :- dreverse(List,[],Rev).

dreverse([],Rev,Rev) :- !.
dreverse([A|Rest],SoFar,Rev) :- dreverse(Rest,[A|SoFar],Rev).

export length/2.

length(List,Length) :- length(List,0,Length).

length([],Length,Length) :- !.
length([_|Rest],Old,Length) :-
    New is Old+1,
    length(Rest,New,Length).

/*------ Moved to Library (strings.pro) - 11/11/91 - KAB -------
export asplit0/4.
asplit0([Char|Rest],Splitter,[Char|R1],String2) :-
	Char \= Splitter,!,
	asplit0(Rest,Splitter,R1,String2).
asplit0([Splitter|Rest],Splitter,[],Rest).
 *----------------------------------------------------------------*/

/*
 * Comparison predicates:
 *   @< /2
 *   @> /2
 *   @=< /2
 *   @>= /2
 *   == /2
 *   \== /2
 *
 * Note: noneq is a very fast way to test if two things are not literally
 *       (pointer) identical.  eq is a quick way to test if two things
 *       point to the same object.  eq and noneq both work properly on atoms
 *       and integers.
 */

export @< /2.
export @> /2.
export @=< /2.
export @>= /2.

T1 @< T2     :- compare(<,T1,T2).
T1 @> T2     :- compare(>,T1,T2).
T1 @=< T2    :- compare(R,T1,T2), noneq(R,'>').
T1 @>= T2    :- compare(R,T1,T2), noneq(R,'<').

/*----------  Defined in assembler:------------
export '=='/2.
export '\=='/2.
T1 == T2     :- compare(=,T1,T2).
T1 \== T2    :- compare(R,T1,T2), noneq(R,'=').
*----------------------------------------------*/


/*
 * does not unify (\=)
 */

export \= /2.
   
X \= X :- !, fail.
_ \= _.


/*
 * recorda/3, recordz/3, recorded/3.
 *
 *   recorda(Key,Term,Ref)
 *   recordz(Key,Term,Ref)
 *   recorded(Key,Term,Ref)
 */

export recorda/3, recordz/3, recorded/3.

recorda(Key,Term,Ref) :-
    rec_getkey(Key,KeyFunctor),
    asserta(recorded(KeyFunctor,Term),Ref).

recordz(Key,Term,Ref) :-
    rec_getkey(Key,KeyFunctor),
    assertz(recorded(KeyFunctor,Term),Ref).

recorded(Key,Term,Ref) :-
    rec_getkey(Key,KeyFunctor),
    clause(recorded(KeyFunctor,Term), true, Ref).

rec_getkey(Key, Key) :- atomic(Key), !.
rec_getkey(S,Key) :- functor(S,Key,_).


/*
 * sort/2
 *
 *	sort(List,SortedList)
 */

export sort/2.

sort(L,R) :- length(L,N), sort(N,L,_,R).

sort(2,[X1|L1],L,R) :- !, comprises(L1,X2,L),
     compare(Delta,X1,X2),
     sort_2(Delta,X1,X2,R).

sort_2('<',X1,X2,[X1,X2]).
sort_2('>',X1,X2,[X2,X1]).
sort_2('=',X1,X2,[X2]).

sort(1,[X|L],L,[X]) :- !.
sort(0,L,L,[]) :- !.
sort(N,L1,L3,R) :-
    N1 is N div 2, N2 is N-N1,
    sort(N1,L1,L2,R1),
    sort(N2,L2,L3,R2),
    merge(R1,R2,R).

merge([],R,R) :- !.
merge(R,[],R) :- !.
merge(R1,R2,[X|R]) :-
    comprises(R1,X1,R1a), comprises(R2,X2,R2a),
    compare(Delta,X1,X2),
    merge1(Delta,X,X1,X2,R1,R2,R1a,R2a,R).

% merge1(Delta,X,X1,X2,R1,R2,R1a,R2a,R)
merge1(  '<',  X,X, _, _, R2,R1a,_,  R  ) :- !, merge(R1a,R2,R).
merge1(  '>',  X,_, X, R1,_, _,  R2a,R  ) :- !, merge(R1,R2a,R).
merge1(  '=',  X,X, _, _, _, R1a,R2a,R  ) :-    merge(R1a,R2a,R).

comprises([X|L],X,L).


/*
 * univ (=..)
 *
 * 	Term =.. List
 */

export '=..'/2.

S =.. [F|Args] :-
    functor(S,F,A),
    !,
    univ_install(S,1,A,Args).
S =.. [F|Args] :-
    atomic(F),
    length(Args,Arity),
    !,
    functor(S,F,Arity),
    univ_install(S,1,Arity,Args).

univ_install(_,N,Arity,[]) :-
    N > Arity,
    !.
univ_install(S,N,Arity,[Arg|Args]) :-
    arg(N,S,Arg),
    !,
    NN is N+1,
    univ_install(S,NN,Arity,Args).


endmod.		%% blt_term.pro: Term Manipulation Builtins
