
:-['ISO/iso_plus_md.pro'].

ref_path('../docs/ref/').
iso_base_url('http://www.deransart.fr/prolog/bips.html').

mi :-
	open('iso_inserts.txt', write, OS),
	md_iso(MDISO),
	iso_base_url(ISOBase),
	sort(MDISO, SortedMDISO),
	do_mi(MDISO, ISOBase, OS),
	close(OS).

do_mi([], ISOBase, OS).
		%% ['atom/1',builtins,'C_def','ISO','#atom','atom.md',''],
do_mi([Item | MDISO], ISOBase, OS)
	:-
	Item = [PA,_,_,ISO,ISOSharp,MDFile,_],
	catenate(ISOBase, ISOSharp, ISOLink),
	sprintf(ISOLinkString, '[ISO Standard Predicate](%t)\n', [ISOLink]),
	atom_codes(ISOLinkAtom, ISOLinkString),
	printf(OS, '%t (%t)\t%t\n',  [MDFile, PA, ISOLinkAtom]),
	do_mi(MDISO, ISOBase, OS).
