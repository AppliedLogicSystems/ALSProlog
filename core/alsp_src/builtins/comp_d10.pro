/*=====================================================================
 |		comp_d10.pro
 |		Copyright (c) 1986-1996 Applied Logic Systems, Inc.
 |		Distribution rights: Unrestricted.
 |
 |	Predicate definitions for compatibility with DEC-10 Prolog syntax
 |
 |	Authors: Kevin A. Buettner, Ken Bowen, Chris White,
 |	         Keith Hughes, Ilyas Cicekli
 |	Original Creation Date: 3/20/86
 *====================================================================*/

module builtins.

    /*
     * Compatibility Predicates:
     *   mode/1          -- nop
     *   public/1        -- nop
     *   dynamic/1       -- nop
     *   numbervars/3
	 *   multifile/1	-- nop
     */
   
export mode/1, public/1, multifile/1.
:- 	op(1150,fx,mode),
	op(1150,fx,public),
	op(1150,fx,multifile).

mode(_).
public(_).
multifile(_).

export numbervars/3.
 
numbervars(V,I0,I1) :- var(V), !, V = '$VAR'(I0), I1 is I0 + 1.
numbervars(A,I0,I1) :- atomic(A), !, I0=I1.
numbervars(T,I0,I1) :- functor(T,_,N), numbervars(N,T,I0,I1).
 
numbervars(0,_,I,I) :- !.
numbervars(K,T,I0,I2) :-
	arg(K,T,A),
	numbervars(A,I0,I1),
	K1 is K-1,
        numbervars(K1,T,I1,I2).

export memberchk/2.

memberchk(X,[X|_]) :- !.
memberchk(X,[_|T]) :- memberchk(X,T).
 
endmod.
