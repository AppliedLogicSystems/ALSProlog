
:-['ISO/iso_plus_md.pro'].

ref_path('../docs/ref/').
iso_base_url('https://www.deransart.fr/prolog/bips.html').

ii :-
	md_iso(MDISO),
	ref_path(RefPath),
	iso_base_url(ISOBase),
	do_ii(MDISO, RefPath, ISOBase).

do_ii([], _, _).
do_ii([Item | MDISO], RefPath, ISOBase)
	:-
	Item = [PA,_,_,ISO,ISOSharp,MDFile,_],
	pathPlusFile(RefPath, MDFile, MDPathAndFile),
	catenate(ISOBase, ISOSharp, ISOLink),
%(PA=='catch/3' -> trace ; true),
	process_page(MDPathAndFile, ISOLink),
	do_ii(MDISO, RefPath, ISOBase).

process_page(MDPathAndFile, ISOLink)
	:-
	grab_lines(MDPathAndFile, Lines),

	open(MDPathAndFile, write, OS),
	Lines = ['---' | RestLines],
	printf(OS, '---\n', []),
	cross_2nd_dashes(RestLines, OS, LinesAfterYAML),
		% [ISO Standard Predicate](https://www.deransart.fr/prolog/bips.html#abolish)
	sprintf(ISOLinkString, '[ISO Standard Predicate](%t)\n', [ISOLink]),
	atom_codes(ISOLinkAtom, ISOLinkString),

	LinesAfterYAML = [FirstLineAfterYAML | RestLinesAfterYAML],

	handlePostYAML(FirstLineAfterYAML, ISOLinkAtom, RestLinesAfterYAML, OS).

/*
	%% The ISO http line was earlier added in this file; replace it:
handlePostYAML(FirstLineAfterYAML, ISOLinkAtom, RestLinesAfterYAML, OS)
	:-
	sub_atom(FirstLineAfterYAML,0,32,_,'[ISO Standard Predicate](http://'),
	!,
	shovel_rest(RestLinesAfterYAML, OS),
	close(OS).
	
	%% The ISO https line was earlier added in this file; replace it:
handlePostYAML(FirstLineAfterYAML, ISOLinkAtom, RestLinesAfterYAML, OS)
	:-
	sub_atom(FirstLineAfterYAML,0,33,_,'[ISO Standard Predicate](https://'),
	!,
	shovel_rest(RestLinesAfterYAML, OS),
	close(OS).
*/
	
	%% No ISO line added yet in this file; add it & shovel:
handlePostYAML(FirstLineAfterYAML, ISOLinkAtom, RestLinesAfterYAML, OS)
	:-
	printf(OS, '%t\n', [ISOLinkAtom]),
	shovel_rest(RestLinesAfterYAML, OS),
	close(OS).
	
cross_2nd_dashes(Lines, OS, LinesTail)
	:-
	Lines = [Line | RemLines],
	cont_cross_2nd_dashes(Line, RemLines, OS, LinesTail).

cont_cross_2nd_dashes(Line, LinesTail, OS, LinesTail)
	:-
	Line = '---',
	!,
	printf(OS, '%t\n', [Line]).

cont_cross_2nd_dashes(Line, RemLines, OS, LinesTail)
	:-
	printf(OS, '%t\n', [Line]),
	cross_2nd_dashes(RemLines, OS, LinesTail).
	
shovel_rest([], OS).
shovel_rest([Line | LinesTail], OS)
	:-
	printf(OS, '%t\n', [Line]),
	shovel_rest(LinesTail, OS).
