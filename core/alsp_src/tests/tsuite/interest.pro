/*-------------------------------------------------------------------*
 |		interest.pro
 |
 |	Playing with compound interest rate formulas
 |	with CLP(BNR) interval constraints.
 *-------------------------------------------------------------------*/

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

cfv(FV, PV, R, N) 
	:-
	[FV,PV,R,N]::real,
	{ FV == PV * exp( R * N) }.

fv1(FV) :- cfv(FV, 1000.0, 0.06, 7.0).
fv2(PV) :- cfv(1521.961556, PV, 0.06, 7.0).
fv3(R)  :- cfv(1521.961556, 1000.0, R, 7.0).
fv4(N)  :- cfv(1521.961556, 1000.0, 0.06, N).


/*------- Example direct queries -------------

	% From Interest Rates in a nutshell, pI-11:
:-cfv(1000.0, PV, 0.08, 1.0).

:- cfv(FV, 234300.23, 0.06, 7.0).
:- cfv(1200.0, PV, 0.06, 7.0).
:- cfv(2200.0, 1000.0, R, 7.0).
:- cfv(2200.0, 1000.0, 0.063, N).
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

dfv(FV, PV, R, K, N) 
	:-
	[FV,PV,R,K,N,Z]::real,
	{FV == PV * exp( Z * K * N), exp(Z) == (1+ R/K) }.


/*
	%From Interest Rates in a nutshell, pI-11
:- dfv(1000.0, PV, 0.08, 1.0, 1.0).
:- dfv(1000.0, PV, 0.08, 4.0, 1.0).

:- dfv(FV, 925.9259259, 0.08, 1.0, 1.0).
:- dfv(FV, 923.845426, 0.08, 4.0, 1.0).

:- dfv(1000.0, 923.845426, R, 1.0, 1.0).

:- dfv(1000.0, 923.845426, 0.08, K, 1.0).


 */

