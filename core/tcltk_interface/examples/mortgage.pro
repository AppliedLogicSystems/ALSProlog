/*===============================================================*
 |		mortgage.pro
 |
 *===============================================================*/


module mortgage.

/*------------------------------------------------------*
 |	m(A, R, I, M, N):
 |
 |	A = amount borrowed
 |	I = fixed interest rate
 |	M = number of payments per year
 |	N = number of years of loan
 |	R = amount of level periodic payment
 |
 |	m(A, R, I, M, N) iff
 |                              I/M
 |				R = A * -----------------------
 |                        1 - (1 + I/M) ^ (-N*M)
 |	iff
 |		J = I/M
 |
 |		K = (1 + J) ^ (-N*M)
 |
 |                 J
 |		R = A * --------
 |               1 - K
 |
 |	iff
 |		J = I/M
 |
 |		ln(K) = - N * M * ln(1 + J)
 |
 |		ln(R) = ln(A) + ln(J) - ln(1 - K)
 |
 *------------------------------------------------------*/



export m/5.
m(A, R, I, M, N)
	:-
	[A,R,J]::real(0.0, 100000000.0),
	I :: real(0.0, 1.0),
	[M,N] :: integer(0,100),
	{	    J == I/M,
		ln(K) == (-1) * N * M * ln(1 + J),
		R == A * ( J / (1 - K) ) },
	solve(R).

export t/2.
t(1,R) :- m(100000.0, R, 0.06, 1, 20).

pmts(A, I, M, N, Vals)
	:-
	NM is N * M,
	PeriodInt is I/M,
	is_length(Vals, NM),
	pay_off(1, NM, Vals, p(A,0), PeriodInt).

pay_off(Period, Limit, [], _, _)
	:-
	Period > Limit,
	!.
pay_off(Period, NM, [NextVal | Vals], p(PrevPrinc,_),Rate)
	:-
	NextVal = p(NextPrinc, ThisInterestPmt),
	{ ThisInterestPmt == Rate * PrevPrinc,
	  NextPrinc == PrevPrinc - ThisInterestPmt},

	ThisInterestPmt::real(IL,IH),
	NextPrinc::real(NL,NH),
	NextPeriod is Period + 1,
	pay_off(NextPeriod, NM, Vals, NextVal, Rate).

export tp/2.
tp(1, Vals)
	:-
	pmts(1000.0, 0.06, 3, 3, CVals),
	xvs(CVals, Vals).

xvs([], []).
xvs([p(P,I) | CVals], [p(PV,IV) | Vals])
	:-
	P::real(PL,PH), PV is (PH+PL)/2,
	I::real(IL,IH), IV is (IH+IL)/2,
	xvs(CVals, Vals).
xvs([p(N,P,I) | CVals], [p(N,PV,IV) | Vals])
	:-
	P::real(PL,PH), PV is (PH+PL)/2,
	I::real(IL,IH), IV is (IH+IL)/2,
	xvs(CVals, Vals).

xiv(Int, V)
	:-
	Int::real(L,H),
	!,
	V is (H+L)/2.

xiv(Int, V)
	:-
	Int::integer(L,H),
	!,
	V is (H+L)/2.

xiv(V, V)
	:-
	number(V).


export payoff/0.
payoff 
	:-
	[NYears, PmtsPerYear] :: integer(0,100),
	get_value(num_years, NYears),
	get_value(pmts_per_year,PmtsPerYear),
	get_value(interest_rate,Interest),
	get_value(principal,Principal),
	NumPmts is NYears * PmtsPerYear,
	[Interest, PeriodRate] :: real(0.0, 1.0),
	{PeriodRate == Interest / PmtsPerYear},
	Principal :: real(0.0, 1.0e12),
	m(Principal, PeriodicPmt, Interest, PmtsPerYear, NYears),
	payments_list_b(1,NumPmts,Principal,Interest,PmtsPerYear,PmtsList),

	PeriodicPmt::real(MPL,MPH), MPmt is (MPL + MPH)/2,
	printf('Periodic Payment = %t\n',[MPmt]),
	show_pmts(PmtsList).




payments_list_a(PN,NumPmts,_,_,[])
	:-
	PN > NumPmts,
	!.
payments_list_a(PN,NumPmts,PrevPrinc,PeriodRate,
				[p(PN,ThisInterestPmt,NextPrinc) | PmtsList])
	:-
write(PN),nl,
	[ThisInterestPmt,NextPrinc] :: real,
	{ ThisInterestPmt == PeriodRate * PrevPrinc,
	  NextPrinc == PrevPrinc - ThisInterestPmt},
	ThisInterestPmt::real(IL,IH),
	NextPrinc::real(NL,NH),
	NxtN is PN + 1,
	payments_list_a(NxtN,NumPmts,NextPrinc,PeriodRate,PmtsList).







/*
payments_list_a(PN,NumPmts,_,_,[])
	:-
	PN > NumPmts,
	!.
payments_list_a(PN,NumPmts,PrevPrinc,PeriodRate,
				[p(PN,ThisInterestPmt,NextPrinc) | PmtsList])
	:-
	[ThisInterestPmt,NextPrinc] :: real,
	{ ThisInterestPmt == PeriodRate * PrevPrinc,
	  NextPrinc == PrevPrinc - ThisInterestPmt},
	ThisInterestPmt::real(IL,IH),
	NextPrinc::real(NL,NH),
	NxtN is PN + 1,
	payments_list_a(NxtN,NumPmts,NextPrinc,PeriodRate,PmtsList).
*/


get_value(Tag,Val)
	:-
	printf('%t = ',[Tag]),
	read(IVal),
%	{Val == IVal}.
	Val :: real(IVal,IVal).

show_pmts(PmtsList,TclVals)
	:-
	xvs(PmtsList, Vals),
	disp_vals(Vals,TclVals).

disp_vals([],[]).
disp_vals([p(N,IP,CP) | Vals],[TN,TCP | RestTclVals])
	:-
	printf('%t.\t%t\t%t\n', [N,IP,CP]),
	tcl_plot_adjust(N,TN,CP,TCP),
	disp_vals(Vals,RestTclVals).

tcl_plot_adjust(N,TN,CP,TCP)
	:-
	TN is 30 * N,
	TCP is 1000.0 - CP.


:-
	load_slib(tclintf),
	tcl_new(i).

export run_mort/0.
run_mort
	:-
	tcl_eval(i, 'source mortgage.tcl', _),
	tk_main_loop.



export payoff/5.
payoff(NYears,PmtsPerYear,Interest,Principal,TclVals)
	:-
	NumPmts is NYears * PmtsPerYear,
	[NYears, PmtsPerYear] :: integer(0,100),
	[Interest, PeriodRate] :: real(0.0, 1.0),
	{PeriodRate == Interest / PmtsPerYear},
	Principal :: real(0.0, 1.0e12),
	m(Principal, PeriodicPmt, Interest, PmtsPerYear, NYears),
	payments_list_a(1,NumPmts,Principal,PeriodRate,PmtsList),

	PeriodicPmt::real(MPL,MPH), MPmt is (MPL + MPH)/2,
	printf('Periodic Payment = %t\n',[MPmt]),
	show_pmts(PmtsList,TclVals).


endmod.
