

reorg(Which)
	:-
	catenate(Which, '_api_basis.pro', BasisFile),
	catenate(Which, '_api.pro', APIFile),
	open(BasisFile, read, Source, []),
	read_term(Source, SourceTerm,[]),
	close(Source),
	(SourceTerm = api_basis(Which,Entries) ->
		open(APIFile, write, Target, []),
		reorg(Entries, Which, Target),
		close(Target)
		;
		printf('Error: Improper term in file %t\n', [BasisFile])
	).


reorg(Entries, Key, Target)
	:-
	reorder(Entries, FlatList, []),
	sort(FlatList, OrderedEntries),
	output_api(OrderedEntries, Key, Target).

reorder([], Tail, Tail).
reorder([Entry | Entries], List, Tail)
	:-
	Entry = ff(FileName, PredsList),
	redoit(PredsList, FileName, List, InterTail),
	reorder(Entries, InterTail, Tail).

redoit([], FileName, InterTail, InterTail).
redoit([Pred | PredsList], FileName, [Pred-FileName | List], InterTail)
	:-
	redoit(PredsList, FileName, List, InterTail).

output_api([], Key, Target)
	:-
	nl(Target),nl(Target).

output_api([Pred-File | OrderedEntries], Key, Target)
	:-
	printf(Target, 'api(%t, %t, \'%t\').\n', [Pred,Key,File]),
	output_api(OrderedEntries, Key, Target).




	%% Assumes the xxx_api.pro files are all consulted:

:-[m68k_api, sprc_api].

merge_archs(ArchList)
	:-
	setof(Pred, [Arch,File]^api(Pred,Arch,File),AllPreds),
	merge_lists(AllPreds, ArchList, List),
	open('mrg_archs.pro', write, Tgt, []),
	output_merger(List, Tgt),
	close(Tgt),
	open('mrg_archs.txt', write, Tgt2, []),
	nl(Tgt2),
	run_out(['Predicate' | ArchList],Tgt2),
	output_table(List, Tgt2),
	close(Tgt2).

merge_lists([], _, []).
merge_lists([Pred | Preds], ArchList, [p(Pred,PList) | List])
	:-
	pred_info(ArchList, Pred, PList),
	merge_lists(Preds, ArchList, List).

pred_info([], Pred, []).
pred_info([Arch | Archs], Pred, [Arch-File | PList])
	:-
	api(Pred, Arch, File),
	!,
	pred_info(Archs, Pred, PList).

pred_info([Arch | Archs], Pred, [Arch-null | PList])
	:-
	pred_info(Archs, Pred, PList).

output_merger([], Tgt)
	:-
	nl(Tgt).
output_merger([PI | List], Tgt)
	:-
	printf(Tgt, '%t.\n', [PI]),
	output_merger(List, Tgt).

output_table([], Tgt2)
	:-
	nl(Tgt2).
output_table([p(Pred,Entries) | List], Tgt2)
	:-
	run_out([Pred | Entries], Tgt2),
	output_table(List, Tgt2).

run_out([],Tgt2).
run_out([_-null],Tgt2)
	:-!,
	printf(Tgt2, '--\n', []).
run_out([_-Item],Tgt2)
	:-!,
	printf(Tgt2, '%t\n', [Item]).
run_out([Item],Tgt2)
	:-!,
	printf(Tgt2, '%t\n', [Item]).
run_out([_-null | Items],Tgt2)
	:-!,
	printf(Tgt2, '--\t', []),
	run_out(Items,Tgt2).
run_out([_-Item | Items],Tgt2)
	:-!,
	printf(Tgt2, '%t\t', [Item]),
	run_out(Items,Tgt2).
run_out([Item | Items],Tgt2)
	:-
	printf(Tgt2, '%t\t', [Item]),
	run_out(Items,Tgt2).





