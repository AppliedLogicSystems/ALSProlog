/*============================================================*
 |		gen_serial.pro
 |			-- requires serial_cmn.pro, which is contained
 |				in all images, but its routines are not
 |				exported from module builtins;
 |			-- generates serial numbers;
 |			-- serial_cmn.pro contains common routines 
 |				together with decoding routines; 
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

module serial_nums.

	%% Professional 30-day:
export p30/0.
p30 :-
	gen_30day(prof, Key),
	display_key(user_output,Key).

	%% Professional permanent:
export pp/0.
pp :-
	gen_perm(prof, Key),
	display_key(user_output,Key).

	%% Student permanent:
export sp/0.
sp :-
	gen_perm(stud, Key),
	display_key(user_output,Key).

display_key(user_output,Key)
	:-
	printf('key=%t\n',[Key]),
	builtins:check_key_type(Key,KeyType, KeyInfo, KeyPath),
	printf('type=%t info=%t path=%t\n',[KeyType, KeyInfo, KeyPath]).

gen_30day(Version, Key)
	:-
	date(Today),
	ddp30(Today, ND30),
	gen_the_key(ND30, day30-Version, G1, G4, Key).

ddp30(YY/MM/DD, YY30/MM30/DD30)
	:-
	epoch_secs(YY, 12, 31, EOY_ESS),
	epoch_secs(YY, MM, DD, Today_ESS),
  		%% SECS per DAY = 86400
	Today_ESS_Plus30 is Today_ESS + 30 * 86400,
	min(EOY_ESS, Today_ESS_Plus30, ND30ESS ),
	ess_to_datetime(ND30ESS,YY30,MM30,DD30,_,_,_).

gen_perm(Version, Key)
	:-
	date(Today),
	gen_the_key(Today, permanent-Version, G1, G4, Key).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Common key generation
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen_the_key(Date, Type, G1, G4, Key)
	:-
	gen_k_1(Date, Type, G1, G4),
	builtins:krspnd(G1, G2),
	builtins:krspnd(G4, G3),
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

endmod.

