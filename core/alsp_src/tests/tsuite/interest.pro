/*-------------------------------------------------------------------*
 |		interest.pro
 |
 |	Playing with compound interest rate formulas
 |	with CLP(BNR) interval constraints.
 *-------------------------------------------------------------------*/

module clptest.

	%%--------------------------------------------------
	%% Continuously compounded future value:
	%% FV = future value
	%% PV = present value
	%% R  = rate of return per period
	%% N  = number of periods
	%%
	%%                                 R*N
	%%  cfv(FV,PV,R,N) iff FV = PV * e
	%%--------------------------------------------------

export cfv/4.
cfv(FV, PV, R, N) 
	:-
	[FV,PV,R,N]::real,
	{ FV == PV * exp( R * N) }.

export fv/2.
fv(1,FV) :- cfv(FV, 1000.0, 0.06, 7.0).
fv(2,PV) :- cfv(1521.961556, PV, 0.06, 7.0).
fv(3,R)  :- cfv(1521.961556, 1000.0, R, 7.0).
fv(4,N)  :- cfv(1521.961556, 1000.0, 0.06, N).

fv(r1, FV) :- PV::real(900.0,1100.0), cfv(FV, PV, 0.06, 7.0).

fv(r2, FV) :- R::real(0.05,0.07), cfv(FV, 1000.0, R, 7.0).

fv(r3, R) :- PV::real(900.0,1100.0), cfv(1500, PV, R, 7.0).

/*------- Example direct queries -------------

	% From Interest Rates in a nutshell, pI-11:
?-cfv(1000.0, PV, 0.08, 1.0).

?- cfv(FV, 234300.23, 0.06, 7.0).
?- cfv(1200.0, PV, 0.06, 7.0).
?- cfv(2200.0, 1000.0, R, 7.0).
?- cfv(2200.0, 1000.0, 0.063, N).
*/

	%%--------------------------------------------------
	%% Discrete compound interest / future value:
	%% FV = future value
	%% PV = present value
	%% R  = rate of return per period
	%% K  = number of compoundings per period
	%% N  = number of periods
	%%
	%%
	%%                                             K*N
	%% dfv(FV, PV, R, K, N) iff FV = PV * (1 + R/K)
	%%--------------------------------------------------

/*----------------------------------
   What we really want:
dfv(FV, PV, R, K, N) 
	:-
	[FV,PV,R,K,N]::real,
	{ FV == PV * power( (1+ R/K), K * N) ) }.

But at the moment, there are no primitives for ln(_) and power(_,_).
Later, we'll introduce more macro expansions which will handle introducing 
things like power(_,_) and ln(_); For now, we hand-expand things:
	 power(A, U) = exp( U * ln(A) )
	 Z = ln(V) iff exp(Z) = V
So:

dfv(FV, PV, R, K, N) 
	:-
	[FV,PV,R,K,N,Z]::real,
	{ FV == PV * power( (1+ R/K), K * N) ) }.
equiv to	{ FV == PV * exp( ln(1+ R/K) * K * N) }.
equiv to	{ FV == PV * exp( Z * K * N), exp(Z) == (1+ R/K) }.
 *--------------------------------------------------------------------*/

export dfv/5.
dfv(FV, PV, R, K, N) 
	:-
	[FV,PV,R,K,N,Z]::real,
	{FV == PV * exp( Z * K * N), exp(Z) == (1+ R/K) }.


/*
	%From Interest Rates in a nutshell, pI-11
?- dfv(1000.0, PV, 0.08, 1.0, 1.0).
?- dfv(1000.0, PV, 0.08, 4.0, 1.0).

?- dfv(FV, 925.9259259, 0.08, 1.0, 1.0).
?- dfv(FV, 923.845426, 0.08, 4.0, 1.0).

?- dfv(1000.0, 923.845426, R, 1.0, 1.0).

?- dfv(1000.0, 923.845426, 0.08, K, 1.0).


 */

	%% for automated testing:
clpt(i(1),[FV],['FV']) :- fv(1,FV).
clpt(i(2),[PV],['PV']) :- fv(2,PV).
clpt(i(3),[R],['R'])   :- fv(3,R).
clpt(i(4),[N],['N'])   :- fv(4,N).

clpt(i(5),[PV],['PV']) :- dfv(1000.0, PV, 0.08, 1.0, 1.0).
clpt(i(6),[PV],['PV']) :- dfv(1000.0, PV, 0.08, 4.0, 1.0).

clpt(i(7),[FV],['FV']) :- dfv(FV, 925.9259259, 0.08, 1.0, 1.0).
clpt(i(8),[FV],['FV']) :- dfv(FV, 923.845426, 0.08, 4.0, 1.0).

clpt(i(9),[R],['R'])   :- dfv(1000.0, 923.845426, R, 1.0, 1.0).
clpt(i(10),[K],['K'])  :- dfv(1000.0, 923.845426, 0.08, K, 1.0).

endmod.
