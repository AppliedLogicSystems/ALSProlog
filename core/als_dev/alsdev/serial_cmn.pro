
module builtins.

check_key_type(KeyAtom, KeyType, KeyInfo, KeyPath)
	:-
	atom_codes(KeyAtom, KCs),
	length(KCs, Len),
	check_key_type0(Len, KCs, KeyType, KeyInfo).

check_key_type0(16, KCs, KeyType, KeyInfo)
	:-!,
	split4(KCs, G1,G2,G3,G4),
	key_type(G1, KeyType),
	fin_key_type0(G1,G2,G3,G4,KeyType,KeyInfo).

check_key_type0(19, KCs, KeyType, KeyInfo)
	:-!,
	split4dash(KCs, G1,G2,G3,G4),
	fin_key_type0(G1,G2,G3,G4,KeyType,KeyInfo).

fin_key_type0(G1,G2,G3,G4,KeyType,KeyInfo)
	:-
	krspnd(G1, G2),
	krspnd(G4, G3),
	key_type(G1, KeyType),
	key_info(KeyType, G1,G2,G3,G4, KeyInfo).

split4([C0,C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15], 
		[C0,C1,C2,C3],[C4,C5,C6,C7],[C8,C9,C10,C11],[C12,C13,C14,C15]). 

split4dash(
	[C0,C1,C2,C3,0'-,C4,C5,C6,C7,0'-,C8,C9,C10,C11,0'-,C12,C13,C14,C15], 
		[C0,C1,C2,C3],[C4,C5,C6,C7],[C8,C9,C10,C11],[C12,C13,C14,C15]). 

key_type([_,_,_,C3], KeyType)
	:-
	key_types(C3, KeyType).

key_types(57, permanent-prof).
key_types(56, permanent-stud).
key_types(55, day30-prof).
key_types(54, day30-stud).

key_info(_-_, G1,G2,G3,G4, KeyInfo)
	:-
	G1 = [C0,C1,C2,_],
	Y is C1 - 0'A +1995,
	M is (0'Z - C2),
	(dmember(C0, [0'3,0'4,0'5,0'6,0'7]) ->
		D is 0'8- C0
		;
		D is (0'Z - C0) + 6
	),
	KeyInfo = Y/M/D.

krspnd([C1,C2,C3,C4],[D1,D2,D3,D4])
	:-
	[J2,J4,J3,J1] = [C1,C2,C3,C4],
	D1 is  (((J1 - 0'A) + 3) mod 26 ) + 0'A,
	D2 is  (((J2 - 0'A) + 3) mod 26 ) + 0'A,
	D3 is  (((J3 - 0'A) + 3) mod 26 ) + 0'A,
	D4 is  (((J4 - 0'A) + 3) mod 26 ) + 0'A.

endmod.
