/*============================================================*
 |		gen_serial.pro
 |
 |	Registration number structure:
 |
 |  XXXX-XXXX-XXXX-XXXX
 |     ^
 |     |---key to product type, according to:
 |
 |		9 (57=0'9): key_types(57, permanent-prof).
 |		8 (56=0'8): key_types(56, permanent-stud).
 |		7 (55=0'7): key_types(55, day30-prof).
 |		6 (54=0'6): key_types(54, day30-stud).
 |		[day30-stud will be unused]
 |
 |	X can be alpha-numeric; alphas are all UC.
 |
 *============================================================*/
sn :- write('Type [30,perm] = '), read(Type), dsn(Type).
dsn(30) :-
	date(D0),
	date_plus_30(D0, D),
	serial_nums:gen_30day(D, day30-prof, Key),
	write(key=Key),nl.

	%% Junk, for now (get library stuff from PM project):
date_plus_30(YY/MM0/DD, YY/MM/DD)
	:-
	MM0 < 12, !, MM is MM0+1.
date_plus_30(YY0/1/DD, N, YY/MM/DD)
	:-
	YY is YY0+1.

module serial_nums.

export xt/0.
xt :-
	gen_30day(1999/10/29, day30-prof, Key),
	write(Key),nl.

gen_30day(Y/M/D, Type, Key)
	:-
	gen_k_1(Y/M/D, Type, G1, G4),
	krspnd(G1, G2),
	krspnd(G4, G3),
	builtins:split4dash(KCs, G1,G2,G3,G4),
	atom_codes(Key, KCs).

gen_k_1(Y/M/D, Type, G1, G4)
	:-
	C1 is (Y - 1995) + 0'A,
	C2 is 0'Z - M,
	((1 =< D, D =< 5) -> 
		C0 is 0'8 - D
		;
		C0 is (0'Z - D) + 6
	),
	builtins:key_types(C3, Type),
	G1 = [C0,C1,C2,C3],
	G4 = [D2,D3,D0,D1],
	D0 is C0,
	D1 is C1,
	D2 is C2,
	D3 is C3 + 20.

krspnd([C1,C2,C3,C4],[D1,D2,D3,D4])
	:-
	[J2,J4,J3,J1] = [C1,C2,C3,C4],
	D1 is  (((J1 - 0'A) + 3) mod 26 ) + 0'A,
	D2 is  (((J2 - 0'A) + 3) mod 26 ) + 0'A,
	D3 is  (((J3 - 0'A) + 3) mod 26 ) + 0'A,
	D4 is  (((J4 - 0'A) + 3) mod 26 ) + 0'A.

endmod.

