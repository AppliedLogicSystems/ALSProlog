/*=========================================================*
 |			upd_toks.pro
 |
 |	Gets the most recent versions of generated tokens.h
 |	files from the builds directory, and stores the *.h
 |	files in generated/tok.h
 *=========================================================*/

		%% The builds to consider:
builds_dirs([
		'hppa_hpux9.05',
		'm68k_nextstep3.3',
		'powerpc_aix4.1.1',
		'sparc_sunos4.1.3'
	]).

gen_files([
		'icode.h',
		'lexinit.h',
		'magic.h',
		'siolex.h',
		'tokens.h',
		'tokini2.h'
	]).

top_build_dir('/mailbox3/builds').

upd_toks
	:-
	builds_dirs(BDs),
	most_rec_build(BDs,ChosenBD),
	store_from(ChosenBD).

most_rec_build(BDs,ChosenBD)
	:-	
	most_rec_build(BDs,no_file,0,ChosenBD).

most_rec_build([],BestSoFar,_,BestSoFar).

most_rec_build([BD | BDs],BestSoFar,BestSoFarDate,ChosenBD)
	:-
	check_rec(BD,BestSoFar,BestSoFarDate,NewBestSoFar,NewBestSoFarDate),
	most_rec_build(BDs,NewBestSoFar,NewBestSoFarDate,ChosenBD).

check_rec(BD,BestSoFar,BestSoFarDate,NewBestSoFar,NewBestSoFarDate)
	:-
	top_build_dir(TD),
	pathPlusFile(TD,BD,FullBD),
	extendPath(FullBD, 'bld-port', BDBldPort),
	pathPlusFile(BDBldPort, 'tokens.h', BDTokensH),
	file_status(BDTokensH, StatusList),
	dmember(mod_time=BDTokHTime, StatusList),
	(BestSoFarDate < BDTokHTime ->
		NewBestSoFar = BDBldPort,
		NewBestSoFarDate = BDTokHTime
		;
		NewBestSoFar = BestSoFar,
		NewBestSoFarDate = BestSoFarDate
	).

store_from(ChosenBDBldPort)
	:-
	gen_files(GenFiles),
	store_from(GenFiles, ChosenBDBldPort).

store_from([], _).
store_from([File | GenFiles], ChosenBDBldPort)
	:-
	pathPlusFile(ChosenBDBldPort, File, FullFile),
	catenate(['cp ',FullFile,' tok.h'], Cmnd),
	system(Cmnd),
	store_from(GenFiles, ChosenBDBldPort).

