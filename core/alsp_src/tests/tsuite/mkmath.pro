/*
 * This program was used to partially build testmath.pro.  If main is run,
 * it will build testmath2.pro in order not to overwrite the real one.
 */

%% for(L,U,I)

for(I,I,I) :- !.	%% have arrived at upper bound
for(I,_,I).		%% not at upper bound, return I
for(L,U,I) :- NL is L+1, for(NL,U,I).

mf(exp(X),	X,	-39,800,10).
mf(exp10(X),	X,	-39,800,10).
mf(log(X),	X,	1,1000,10).
mf(log10(X),	X,	1,1000,10).
mf(sin(X),	X,	-314,315,100).
mf(cos(X),	X,	-314,315,100).
mf(tan(X),	X,	-314,315,100).
mf(asin(X),	X,	-10,10,10).
mf(acos(X),	X,	-10,10,10).
mf(atan(X),	X,	-80,80,10).
mf(round(X),	X,	-46,46,20).
mf(trunc(X),	X,	-46,46,20).
mf(floor(X),	X,	-46,46,20).
mf(sqrt(X),	X,	0,800,5).

main0 :-
	mf(F,V,L,U,D),
	functor(F,FF,_),
	name(FF,FFS),
	name(FFV,[~v,~_ | FFS]),
	functor(FV,FFV,2),
	arg(1,FV,V),
	arg(2,FV,Res),
	nl,
	nl,
	write(mf(F,FV,Res)), write('.'),
	nl,
	for(L,U,I),
	    V is I/D,
	    Res is F,
	    write(FV), write('.'), nl,
	    fail.
main0.

main :-
	tell('testmath2.pro'),
	main0,
	told.
