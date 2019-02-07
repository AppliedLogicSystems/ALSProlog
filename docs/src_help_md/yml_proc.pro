/*================================================================* 
 |			yml_proc.pro
 |
 |	- Process the yaml header in an md_help/___.md file
 |	  and insert the result in the corresponding
 |	  alshelp/___.html file. Default is to run 
 |	  with md_help and alshelp as subdirs, as in
 |	  running in src_help_md, but the paths can be specified
 *================================================================*/

default_path(md, './md_help').
default_path(html, './alshelp').

proc_all_yml
	:-
	default_path(md, MD_Path),
	default_path(html, HTML_Path),
	proc_all_yml(MD_Path, HTML_Path).

proc_all_yml(MD_Path, HTML_Path)
	:-
	files(MD_Path, '*.md', MD_FilesList),
	proc_all_yml_files(MD_FilesList, MD_Path, HTML_Path).

proc_all_yml_files([], HTML_Path)
	:-!.

proc_all_yml_files([MDFile | RestMD_FilesList], MD_Path, HTML_Path)
	:-
	path_directory_tail(FullMDPath, MD_Path, MDFile),
	proc_file_yml(FullMDPath, HTML_Path),
	!,
	proc_all_yml_files(RestMD_FilesList, MD_Path, HTML_Path).


pfy0 :- proc_file_yml('./md_help/member2.md', './alshelp').
pfy1 :- proc_file_yml('./md_help/putchar12.md', './alshelp').
pfy2 :- proc_file_yml('./md_help/Edit_menu.md', './alshelp').


proc_file_yml(MDFile, HTML_Path)
	:-
	grab_lines_begin_end(MDFile, YmlLines, '---', false, '---', false),
	yaml_tags(YamlTags),
	handle_header_lines(YamlTags, YmlLines, TagsVals, [], RestLines),
	cnvt_md_2_html_and_insert(RestLines, TagsVals, MDFile, HTML_Path).

/*
title: 'ALSDev Edit Menu'
package: ALSDev
group: Development Env.
module: alsdev
*/
%[TitleLine, GroupLine, ModuleOrPredicatesLine | RestYmlLines], HeaderVals)

yaml_tags(['title: '-7, 'package: '-9, 'group: '-7, 'module: '-8]).


	%% For tag title:
	%% If the tag occurs at the beginning 
	%% of the line, get it's value; there is no default.
get_tag_val('title: ', TagLen, Line, TagVal, _, true)
	:-
	Tag = 'title: ',
	sub_atom(Line, 0, TagLen, _, Tag),
	sub_atom(Line, TagLen, _, 0, TagVal0),
	strip_single_quotes_atom(TagVal0, TagVal).

	%% For tag package:
	%% If the tag occurs at the beginning 
	%% of the line, get it's value. The default is 'Core Prolog'
get_tag_val('package: ', TagLen, Line, TagVal, _, true)
	:-
	Tag = 'package: ',
	sub_atom(Line, 0, TagLen, _, Tag),
	!,
	sub_atom(Line, TagLen, _, 0, TagVal).

	% default_package: 'Core Prolog'
get_tag_val('package: ', TagLen, Line, TagVal, _, false)
	:-
	TagVal = 'Core Prolog'.


default_group('Core Prolog', 'Terms').
default_group('ALSDev', 'Development Env.').
default_group('ALS Library', 'Lists').
default_group('ALS C-Interf', 'C Data').

get_tag_val('group: ', TagLen, Line, TagVal, _, true)
	:-
	Tag = 'group: ',
	sub_atom(Line, 0, TagLen, _, Tag),
	!,
	sub_atom(Line, TagLen, _, 0, TagVal).

get_tag_val('group: ', TagLen, Line, TagVal, PrevVals, false)
	:-
	member('package: '-Package, PrevVals),
	default_group(Package, TagVal).
	

get_tag_val('module: ', TagLen, Line, TagVal, _, true)
	:-
	Tag = 'module: ',
	sub_atom(Line, 0, TagLen, _, Tag),
	!,
	sub_atom(Line, TagLen, _, 0, TagVal).

default_module('Core Prolog', builtins).
default_module('ALSDev', alsdev).
default_module('ALS Library', builtins).
default_module('ALS C-Interf', builtins).

get_tag_val('module: ', TagLen, Line, TagVal, PrevVals, false)
	:-
	member('package: '-Package, PrevVals),
	default_module(Package, TagVal).
	
handle_header_lines([], RestLines0, [], _, RestLines)
	:-
	RestLines0 = [Line | RestLines],
	sub_atom(Line, 0, 11, _, 'predicates:').
	
handle_header_lines([Tag-TagLen | RestYamlTags], [Line | RestYmlLines], [Tag-TagVal | TagsVals], PrevVals, RestLines)
	:-
	get_tag_val(Tag, TagLen, Line, TagVal, PrevVals, Present),
	(Present==true -> ContYmlLines = RestYmlLines ; ContYmlLines = [Line | RestYmlLines]),
	handle_header_lines(RestYamlTags, ContYmlLines, TagsVals, [Tag-TagVal | PrevVals], RestLines).

cnvt_md_2_html_and_insert(RestLines, TagsVals, MDPathAndFile, HTML_Path)
	:-
	PredicateLines = RestLines,
	get_pblocks(PredicateLines, PredBlocks),

	path_directory_tail(MDPathAndFile, MDDirectory, MDFileFullName),
	file_extension(MDFileFullName, Name, md),

	file_extension(HtmlFileFullName, Name, html),
	path_directory_tail(HtmlPathAndFile, HTML_Path, HtmlFileFullName),
	grab_lines(HtmlPathAndFile, OrigHtmlLines),

	file_extension(BackHtmlFileFullName, Name, html_back),
	path_directory_tail(BackHtmlPathAndFile, HTML_Path, BackHtmlFileFullName),
	move_file(HtmlPathAndFile, BackHtmlPathAndFile),


	open(HtmlPathAndFile, write, OutS),
%OutS = user_output,
		%yaml_tags(['title: '-7, 'package: '-9, 'group: '-7, 'module: '-8]).
	dmember('title: '-Title, TagsVals),
	dmember('group: '-Group, TagsVals),
	dmember('module: '-Module, TagsVals),

	create_headers(Title, Group, Module, PredBlocks, OutS),
	write_lines(OutS, OrigHtmlLines),
printf(user_output, '%t => %t\n', [MDPathAndFile, HtmlPathAndFile]),
	close(OutS).
	
create_headers(Title, Group, Module, PredBlocks, OutS)
	:-
	printf(OutS, '<h1>%t</h1>\n',[Title]),
	printf(OutS, '<h3>Module: %t</h3>\n',[Module]),
	printf(OutS, '<p>\n',[]),
	make_preds_entries(PredBlocks, OutS),
	printf(OutS, '</p>\n',[]).

make_preds_entries([], _).
make_preds_entries([Block | RestPredBlocks], OutS)
	:-!,
	block_entries(Block, OutS),
	make_preds_entries(RestPredBlocks, OutS).

block_entries(p(PA,Desc), OutS)
	:-!,
	printf(OutS, '<code>%t</code>\n &mdash; %t\n<br>\n',[PA,Desc]).

block_entries(mp(Block), Outs)	
	:- !,
	block_entries(Block, Outs).

block_entries([], _).
block_entries([p(PA,Desc) | RestBlock], OutS)
	:-
	block_entries(p(PA,Desc), OutS),
	block_entries(RestBlock, OutS).


	


get_pblocks([], []).
get_pblocks(PredicateLines, [PBlock | RestPredBlocks])
	:-
	get_pred_block(PredicateLines, PBlock, RestPredicateLines),
	get_pblocks(RestPredicateLines, RestPredBlocks).

get_pred_block([Line | RestPredicateLines], PBlock, RestPredicateLines)
	:-
	sub_atom(Line, D0, 7, D1, ', desc:'),
	!,
	make_single_block(Line, D0, D1, PBlock).

get_pred_block(PredicateLines, PBlock, RestPredicateLines)
	:-
	build_multi_block(PredicateLines, PBlock, RestPredicateLines).

	/*
              8  |  PA0     | D1A
	                D0  | 7   |  D1
                      D0A          |
	 [- {sig: 'member/2', desc: 'list membership'},
	            - {sig: 'dmember/2', desc: 'list membership'}],
	*/
make_single_block(Line, D0, D1, PBlock)
	:-
	D1A is 7 + D1,
	sub_atom(Line, 8,_,D1A, PA0),
	strip_single_quotes_atom(PA0, PA),

	D0A is D0 + 7 + 1,
	sub_atom(Line, D0A, _, 0, Desc0),
	atom_codes(Desc0, Desc0Codes),
	strip_white(Desc0Codes, Desc1Codes),
	dreverse(Desc1Codes, RevDesc1Codes),
	strip_white(RevDesc1Codes, RevDesc2Codes),
	RevDesc2Codes = [RC | RestRevDesc2Codes],
	(RC == 0'} ->
		RevDesc3Codes = RestRevDesc2Codes
		;
		RevDesc3Codes = RevDesc2Codes
	),
	dreverse(RevDesc3Codes, Desc3Codes),
	strip_front_back_chars(Desc3Codes, 0'', 0'', DescCodes),
	atom_codes(Desc, DescCodes),
	PBlock = p(PA, Desc).

	/*
	    - {sig: 'put_char', args: {
	        1: 'output a character to the current output stream',
	        2: 'output a character to a specific output stream'
	      }}
	*/
build_multi_block([Line0 | LinesTail], MPBlock, RestPredicateLines)
	:-
	sub_atom(Line0, 0, 8, A0, '- {sig: '),
	sub_atom(Line0, C0, 1, C1, ','),
	C1A is C1+1,
	sub_atom(Line0, 8,_,C1A, PN),
	get_np_lines(LinesTail, NLines, RestPredicateLines),
	make_multi_block(PN, NLines, MPBlock).

get_np_lines([Line | RestPredicateLines], [], RestPredicateLines)
	:-
	(
	sub_atom(Line, 2, 2, _, '}}');
	sub_atom(Line, 1, 2, _, '}}');
	sub_atom(Line, 0, 2, _, '}}');
	sub_atom(Line, 3, 2, _, '}}') ),
	!.
	
get_np_lines([Line | LinesTail], [Line | RestNLines], RestPredicateLines)
	:-
	get_np_lines(LinesTail, RestNLines, RestPredicateLines).
	

make_multi_block(PN, NLines, MPBlock)
	:-
	assemble_PADs(NLines, PN, MPLines),
	MPBlock = mp(MPLines).

assemble_PADs([], PN, []).
assemble_PADs([NL | RestNLines], PN, [MPL | RestMPLines])
	:-
	create_mpl(NL, PN, MPL),
	assemble_PADs(RestNLines, PN, RestMPLines).

	/*
	    1: 'output a character to the current output stream',
	    2: 'output a character to a specific output stream'
	*/
create_mpl(NL, InitPN, MPL)	
	:-
	atom_codes(NL, NLCodes),
	strip_white(NLCodes, NLC0),
	read_to(NLC0, [0':], BeforeColon, AfterColon, _),

	strip_both_white(AfterColon, AC0),
		%% kill trailing comma, if any:
	dreverse(AC0, AC1),
	AC1=[LastChar | RestChars],
	(LastChar==0', ->
		dreverse(RestChars, AC2)
		;
		AC2 = AC0
	),
	strip_front_back_chars(AC2, 0'', 0'', DescCodes),
	atom_codes(Desc, DescCodes),

	atom_codes(InitPN, InitPNCodes),
	strip_front_back_chars(InitPNCodes, 0'', 0'', PNCodes),

	append(PNCodes, [0'/ | BeforeColon], PACodes), 
	atom_codes(PA, PACodes),
	MPL = p(PA, Desc).


	



%%%%%%%%%%%%%%

tg_tt :-
	SrcF = './md_help/putchar12.md',
	Begin = '---',
	End = '---',
	grab_lines_begin_end(SrcF, Lines, Begin, true, End, true),
write_lines(Lines),nl.
tg_tf :-
	SrcF = './md_help/putchar12.md',
	Begin = '---',
	End = '---',
	grab_lines_begin_end(SrcF, Lines, Begin, true, End, false),
write_lines(Lines),nl.
tg_ft :-
	SrcF = './md_help/putchar12.md',
	Begin = '---',
	End = '---',
	grab_lines_begin_end(SrcF, Lines, Begin, false, End, true),
write_lines(Lines),nl.
tg_ff :-
	SrcF = './md_help/putchar12.md',
	Begin = '---',
	End = '---',
	grab_lines_begin_end(SrcF, Lines, Begin, false, End, false),
write_lines(Lines),nl.

%%%%%%%%%
grab_lines_begin_end(SrcF, Lines, Begin, IncB, End, IncE)
        :-
        open(SrcF,read,SS,[]),
        get_lines_b_e(SS, Lines, Begin, IncB, End, IncE),
        close(SS).

%% Begin,End are strings that (may) occur as the start
%% of lines in the stream; if they occur, Lines is the
%% list of all lines from the first line starting with
%% an occurrence of Begin, inclusive depending on IncB, 
%% to the first line starting with an occurrence of End, 
%% inclusive depending on IncE.  Values of IncB, IncE
%% are true/false

get_lines_b_e(SS, Lines, Begin, IncB, End, IncE)
	:-
	cross_lines_to_b(SS, FirstLine, Begin, EOS),
	(EOS==true ->
		Lines = []
		;
		(IncB==true -> 
			Lines = [FirstLine | RestLines]
			;
			Lines = RestLines
		),
		get_lines_to_e(SS, RestLines, End, IncE)
	).

cross_lines_to_b(SS, '', Begin, true)
	:-
	at_end_of_stream(SS),
	!.

cross_lines_to_b(SS, FirstLine, Begin, EOS)
	:-
	get_line(SS,FirstLine),
	disp_cross_lines_to_b(SS, FirstLine, Begin, EOS).

disp_cross_lines_to_b(SS, FirstLine, Begin, false)
	:-
	sub_atom(FirstLine, 0, _, _, Begin),
	!.

disp_cross_lines_to_b(SS, FirstLine, Begin, EOS)
	:-
	cross_lines_to_b(SS, FirstLine, Begin, EOS).

get_lines_to_e(SS, Lines, End, IncE)
	:-
	get_line(SS,Line),
	disp_get_lines_to_e(Line, SS, Lines, End, IncE).

disp_get_lines_to_e(Line, SS, List, End, IncE)
	:-
	sub_atom(Line, 0, _, _, End),
	!,
	(IncE ==true ->
		List = [Line]
		;
		List = []
	).

disp_get_lines_to_e(Line, SS, [Line | RestLines], End, IncE)
	:-
	get_lines_to_e(SS, RestLines, End, IncE).
	


strip_single_quotes_atom(AtomIn, AtomOut)
	:-
	atom_codes(AtomIn, AtomInCodes),
	strip_front_back_chars(AtomInCodes, 0'', 0'', AtomOutCodes),
	atom_codes(AtomOut, AtomOutCodes).

	%% for strings:
strip_front_back_chars(CharsIn, FChar, BChar, CharsOut)
	:-
	CharsIn = [C0 | RestCharsIn],
	(C0 == FChar ->
		CharsMid = RestCharsIn
		;
		CharsMid = CharsIn
	),
	dreverse(CharsMid, RevCharsMid),
	RevCharsMid = [C1 | RestRevCharsMid],
	(C1 == BChar ->
		dreverse(RestRevCharsMid, CharsFin)
		;
		CharsFin = CharsMid
	),
	CharsOut = CharsFin.





