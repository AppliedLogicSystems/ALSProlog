files_386(L) :-
	setof(F, [P,PP]^tag_386(P,F,PP), L).

files_sparc(L) :-
	setof(F, [P,PP]^tag_sprc(P,F,PP), L).

comp_files(MM) :-
	files_386(L386),
	files_sparc(Lsparc),
	cmp_files(L386, Lsparc, MM).

cmp_files([], Lsparc, []).
cmp_files([F386 | L386], Lsparc, [f(F386,LSPCs) | MMTail]) :-
	cf(F386, Lsparc, LSPCs),
	!,
	cmp_files(L386, Lsparc, MMTail).
cmp_files([F386 | L386], Lsparc, MM) :-
	cmp_files(L386, Lsparc, MM).

/*
cf([], F386, MMTail, MMTail).
cf([Fsparc | Lsparc], F386, [p(F386,Fsparc,N) | RestMM], MMTail) :-
	count_common(F386,Fsparc,N),
	cf(Lsparc, F386, RestMM, MMTail).
*/

cf(F386, Lsparc, LSPCs) :-
	setof(ls(N,Fsparc, LLL), (member(Fsparc,Lsparc),
						 count_common(F386,Fsparc,N,LLL), N > 0), LSPCs).

count_common(F386,Fsparc,N, LLL) :-
	setof(P,[Pg1,Pg2]^(tag_386(P,F386,Pg1),tag_sprc(P,Fsparc,Pg2)),LLL),
	!,
	length(LLL,N).
count_common(F386,Fsparc,0, []).

ip(F386,Fsparc) :-
	tag_386(P, F386, _), tag_sprc(P, Fsparc, _).

dit :- comp_files(MM),
	swit(MM).

swit(L) :-
	tell(c386_sparc),
	swit0(L),
	told.

swit0([]).
swit0([f(F386, [ls(N,Fspc,LLL) | _]) | TailL]) :-
	write(F386),put(0'	'),write(Fspc),put(0'	'),write(N),nl,
	wlst(LLL),nl,
	swit0(TailL).

wlst([]).
wlst([I | L]) :-
	write(I),put(0'	'),
	wlst(L).

udef_386(L) :-
	setof(F1-P, [PP1]^tag_386(P,F1,PP1),L386),
	rem_spc_dfd(L386, L0),
	setof(F, [P,PP]^tag_386(P,F,PP), F386),
	sscreen(F386,L0,L).

rem_spc_dfd([], []).
rem_spc_dfd([F-P | RestL386], L) :-
	tag_sprc(P,FSpc,PP),
	!,
	rem_spc_dfd(RestL386, L).
rem_spc_dfd([F-P | RestL386], [F-P | L]) :-
	rem_spc_dfd(RestL386, L).


udef_sparc(L) :-
	setof(F1-P, [PP1]^tag_sprc(P,F1,PP1),Lsparc),
	rem_386_dfd(Lsparc, L0),
	setof(F, [P,PP]^tag_sprc(P,F,PP), SpF),
	sscreen(SpF,L0,L).

rem_386_dfd([], []).
rem_386_dfd([F-P | RestLsparc], L) :-
	tag_386(P,F386,PP),
	!,
	rem_386_dfd(RestLsparc, L).
rem_386_dfd([F-P | RestLsparc], [F-P | L]) :-
	rem_386_dfd(RestLsparc, L).

sscreen([],_,[]).
sscreen([F | SpF],L0,[F-FL | L]) :-
	(setof(P,member(F-P,L0),FL),!;FL=[]),
	sscreen(SpF,L0,L).

