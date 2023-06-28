%% Force most recent:
%:-['../ALSProlog/core/alsp_src/library/html_tokens.pro'].
%:-['/Users/ken/ALS/GitHub/ParseHtml/parse_html.pro'].

parse_html 
	:- 
        sys_env(OS,_,_),
	NnPXML = 2,
	NnTagsVals1 = 11,
	NnTagsVals2 = 18,
	Fnc1 = div,
	Fnc2 = form,
	TgtTags1 = [h1],
	TgtTags2 = [table],
	NnPaths1 = 1,
	NnPaths2 = 3,
        (OS = mswin32 ->
                Path1 = '../alsp_src/tests/libtests/example.com',
                Path2 = '../alsp_src/tests/libtests/sample_awstats.html'
                ;
                Path1 = '../../alsp_src/tests/libtests/example.com',
                Path2 = '../../alsp_src/tests/libtests/sample_awstats.html'
        ),

	do_check_html_parsing(OS, Path1, NnPXML, NnTagsVals1, Fnc1, TgtTags1, NnPaths1),
	do_check_html_parsing(OS, Path2, NnPXML, NnTagsVals2, Fnc2, TgtTags2, NnPaths2).
	
do_check_html_parsing(OS, Path, NnPXML, NnTagsVals, Fnc, TgtTags, NnPaths)
	:-
	get_cwd(CWD),
	printf(user,'    In %t|%t: parsing path: %t\n', [OS, CWD, Path]),flush_output,
	(check_grab_pxml(Path, NnPXML) -> true ;
	    printf(user, 'check_grab_pxml FAILED on %t\n',[Path]), flush_output,
		fail
	),
	check_grab_pxml_tagged(Path, NnPXML, NnTagsVals, Fnc),
	check_grab_pxml_with_paths(Path, NnPXML, NnTagsVals, TgtTags, NnPaths).

check_grab_pxml(Path, NnPXML)
	:-
	grab_pxml(Path, PXML),
	length(PXML, LenPXML),
	LenPXML == NnPXML,
	PXML = [_, T2 | _],
	functor(T2, html, 2).

check_grab_pxml_tagged(Path, NnPXML, NnTagsVals, Fnc)
	:-
	grab_pxml_with_tagged(Path, PXML, TagsValsList),
	length(TagsValsList, LenTagsValsList),
	LenTagsValsList == NnTagsVals,
	dmember(body=RawBody, TagsValsList),
		% closed diff list:
	RawBody = ([Body | _], []),
	Body = body([], BodyContentList),
	BodyContentList = [BCL1 | _],
	functor(BCL1, FF, AA),		%% FF = div, AA = 2
	FF == Fnc.

check_grab_pxml_with_paths(Path, NnPXML, NnTagsVals, TgtTags, NnPaths)
	:-
 	grab_pxml_with_paths(Path, PXML, TagsValsList, TgtTags, Paths),
	length(TagsValsList, LenTagsValsList),
	LenTagsValsList == NnTagsVals,
	length(Paths, LenPaths),
	LenPaths == NnPaths.

