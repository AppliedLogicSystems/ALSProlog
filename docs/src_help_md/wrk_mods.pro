
doit :- modsAndTitles(TLs).

ref_path('../ALSProlog/docs/docs/ref/').

modsAndTitles(TLs) :-
	ref_path(RefPath),
	atom_length(RefPath, RefPathLength),
	WrkTgt = './TmpDir/wrrk.txt',
	catenate([' grep "title: " ', RefPath, '*.md > ', WrkTgt], GrepCmd),
	system(GrepCmd),
	grab_lines(WrkTgt, Liness),
	processLines(Liness, RefPathLength, TLs),

/*
	length(TLs, NN),
	shwByFile(TLs,0,K),
	write(num=NN),nl,
	write(num_non_builtins=K),nl,
	open('./mods_nonBlt.txt', write, OS),
	set_output(OS),
	shwByFile(TLs,0,K),
	set_output(user),
	close(OS),
	write('num md pages'=NN),nl,
	write(num_non_builtins=K),nl,
*/
	catenate(' rm ', WrkTgt, RmWCmd),
	system(RmWCmd).

processLines([], _, []).
	%% skip line for 'index.md':
processLines([Line | Lines], RefPathLength, TrLs)
	:-
	sub_atom(Line, RefPathLength, 9, _, 'index.md:'),
	!,
	printf('Skipping index.md\n', []),
	processLines(Lines, RefPathLength, TrLs).
processLines([Line | Lines], RefPathLength, [TrL | TrLs])
	:-
	do_transform(Line, TrL),
	processLines(Lines, RefPathLength, TrLs).
	
	%% Transform grep output line to 
do_transform(Line, t(FName, Title, Module))
	:-
	transform(Line, t(Title, FName, Pred, Arity, ArityList, Module)).
	

transform(Line, t(Title, FName, Pred, Arity, ArityList, Module))
	:-
	atom_codes(Line, Codes),
	string_split_right(Codes, "title: ", XFileCs, QTitleCs),
	reverse(XFileCs, RevXFileCs),
	RevXFileCs = [0': | RevPathCodes],
	asplit0(RevPathCodes, 0'/, RevFNameCodes, _),
	reverse(RevFNameCodes, FNameCodes),
	atom_codes(FName, FNameCodes),

	strip_white(QTitleCs, QTCs1),
	QTCs1 = [0'' | Q1TitleCs],
	reverse(Q1TitleCs, RQ1TitleCs),
	strip_white(RQ1TitleCs, RQ1TitleCsW),
	RQ1TitleCsW = [0'' | RTitleCs],
	reverse(RTitleCs, TitleCs),
	atom_codes(Title, TitleCs),
	getPA(TitleCs, Pred, Arity, ArityList),
	(all_procedures(Module, Pred, Arity, _) ->
		true
		;
		Module = 'alsdev/tcl'
	).
	
getPA(TitleCs, Pred, Arity, AL)
	:-
	asplit0(TitleCs, 0'/, PredCs, ArityCs),
	!,
	atom_codes(Pred, PredCs),
	xarity(ArityCs, Arity, AL).

getPA(TitleCs, Pred, 0, [])
	:-
	atom_codes(Pred, TitleCs).

xarity([0'[ | RestArityCs], Arity, AL)
	:-!,
	term_codes(AL, [0'[ | RestArityCs]),
	AL = [Arity | _].

xarity(ArityCs, Arity, AL)
	:-
	asplit0(ArityCs, 0'(, NNCs, _),
	!,
	number_codes(Arity, NNCs),
	AL = [Arity].

xarity(ArityCs, Arity, AL)
	:-
	number_codes(Arity, ArityCs),
	AL = [Arity].

shwByFile([],K,K).
shwByFile([TT | TLs],N,K)
	:-
	TT = t(Title, FName, Pred, Arity, ArityList, Module),
	(Module \= builtins -> 
		sByF(TT),
		M is N+1
		; 
		M = N
	),
	shwByFile(TLs,M,K).

sByF(t(Title, FName, Pred, Arity, ArityList, Module))
	:-
	printf('%t\tmod = %t\ttitle = %t\n\n', [FName, Module, Title]).



string_split_left([],[],[],[]).
	% LeftPart = from head up to & including 1st occurrence of SplittingList
	% Code = A CL = [A B C | TT]     SL = [A B C]
string_split_left([Code | RestCodesList],[Code | RestSplittingList],[Code | LeftPartCs],RightPartCs)
	:-
	append(RestSplittingList, RightPartCs, RestCodesList),
	LeftPartCs = RestSplittingList,
	!.
string_split_left([Code | RestCodesList],SplittingList,[Code | LeftPartCs],RightPartCs)
	:-
	string_split_left(RestCodesList,SplittingList,LeftPartCs,RightPartCs).

string_split_mid([],[],[],[]).
	% LeftPart = from head up to but not including 1st occurrence of SplittingList
	% RightPart = from 1st occurrence of SplittingList to end of input list
string_split_mid([Code | RestCodesList],[Code | RestSplittingList],LeftPartCs,[Code | RightPartCs])
	:-
	append(RestSplittingList, _, RestCodesList),
	RightPartCs = RestCodesList,
	!,
	LeftPartCs = [].
string_split_mid([Code | RestCodesList],SplittingList,[Code | LeftPartCs],RightPartCs)
	:-
	string_split_mid(RestCodesList,SplittingList,LeftPartCs,RightPartCs).

/*
 string_split_mid("cdefg", "de", Left, Right).
	Left="c" Right = "defg"
*/

string_split_right([],[],[],[]).
	% LeftPart = from head up to but not including 1st occurrence of SplittingList
	% RightPart = after, but not including,  1st occurrence of SplittingList to end of input list
string_split_right([Code | RestCodesList],[Code | RestSplittingList],LeftPartCs,RightPartCs)
	:-
	append(RestSplittingList, RightPartCs, RestCodesList),
	!,
	LeftPartCs = [].
string_split_right([Code | RestCodesList],SplittingList,[Code | LeftPartCs],RightPartCs)
	:-
	string_split_right(RestCodesList,SplittingList,LeftPartCs,RightPartCs).

/*
 string_split_right("cdefg", "de", Left, Right).
	Left="c" Right = "fg"
*/





doit2 :-
	system(' grep "]<" alshelp/toc* > wrok.txt'),
	grab_lines('wrok.txt', Y),
	getix(Y, YIX),
	write(YIX),nl,
	length(YIX, NN),
	write(num=NN),nl,
	open('../many_ones_tgts.txt', write, OS),
	write_lines(YIX),nl.

getix([], []).
getix([Line | RestLines], [LIX | RestLIX])
	:-
	atom_codes(Line, Codes),
	string_split_right(Codes, "ix=\"", _, InitCodes),
	asplit0(InitCodes,0' ,LIXCodes, _),
	asplit0(LIXCodes, 0'/, FNameCodes, _),
	atom_codes(LIX0, LIXCodes),
	atom_codes(FName, FNameCodes),
	(all_procedures(Module, FName, Arity, _) -> true ; Module='alsdev?', Arity='?'),
	LIX = (LIX0, Module, FName/Arity),
	getix(RestLines, RestLIX).


