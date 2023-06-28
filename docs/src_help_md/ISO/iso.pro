
g :-	simpl_iso(ISOList).

simpl_iso(ISOList)
	:-
	open('iso_list_raw.pro', read, S),
	read(S, RawISOList),
	close(S),
	simplify(RawISOList, ISOList),
	sort(ISOList, SortedISOList),
	open('smpl_iso.txt', write, OS),
	write_clauses(OS, SortedISOList, [quoted(true)]),
	close(OS).

simplify([], []).
simplify([Items | RawISOListTail], ISOList)
	:-
	Items = [Sharp | RestItems],
	flatten_items(RestItems, Sharp, ISOList, TailISOList),
	simplify(RawISOListTail, TailISOList).

flatten_items([], Sharp, TailISOList, TailISOList).
flatten_items([Item | Items], Sharp, [[FlatItem, Sharp] | ISOList], TailISOList)
	:-
	sprintf(ItemCodes, '%t', [Item]),
	atom_codes(FlatItem, ItemCodes),
	flatten_items(Items, Sharp, ISOList, TailISOList).
