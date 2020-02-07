/*================================================================* 
 |			md_yml_process.pro
 |
 |	- Predicates to process the yaml header in an 
 |	  /ALSProlog/docs/docs/ref/___.md file
 *================================================================*/

py :- proc_all_yml(Md_Groups, MdInfoByPreds, GPLTail).

proc_all_yml(Md_Groups, MdInfoByPreds, GPLTail)
	:-
		%% In blts_xprt_udoc.pro:
	ref_path(RefPath),
	files(RefPath, '*.md', MD_FilesList),
	proc_all_yml_files(MD_FilesList, RefPath, Md_Groups),
	list_md_by_pred(Md_Groups, MdInfoByPreds, GPLTail).

proc_all_yml_files([], _, []).
proc_all_yml_files([MD_File | MD_FilesList], MD_Path, Md_GroupsNext)
	:-
	path_directory_tail(FullPath, MD_Path, MD_File),
	(MD_File == 'index.md' -> 
		Md_GroupsNext = Md_Groups
		;
		proc_file_yml(FullPath, MD_File, TagsVals, PredBlocks, Group),
		Md_GroupsNext = [Group | Md_Groups]
	),
	proc_all_yml_files(MD_FilesList, MD_Path, Md_Groups).

proc_file_yml(FullPath, MD_File, TagsVals, PredBlocks,Group)
	:-
	grab_lines_begin_end(FullPath, YmlLines, '---', false, '---', false),
	yaml_tags(YamlTags),
	handle_header_lines(YamlTags, YmlLines, TagsVals, [], RestLines),
	get_pblocks(RestLines, PredBlocks),
	!,
	Group = g(MD_File, PredBlocks, TagsVals).
proc_file_yml(FullPath, MD_File, TagsVals, PredBlocks,Group)
	:-
	printf('proc_file_yml failed at: %t\n', [FullPath]).

/*
title: 'ALSDev Edit Menu'
package: ALSDev
group: Development Env.
module: alsdev
*/
	%% sub_atom length/offset of the tag + 2 ( = length of ': ' )
%yaml_tags(['title: '-7, 'package: '-9, 'group: '-7, 'module: '-8]).
yaml_tags(['title: '-7, 'package: '-9, 'group: '-7, 'module: '-8, 'iso: '-5]).


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
get_tag_val('module: ', _, Line, 'Cintf', _, false).

default_module('Core Prolog', builtins).
default_module('ALSDev', alsdev).
default_module('ALS Library', builtins).
default_module('ALS C-Interf', builtins).

get_tag_val('module: ', TagLen, Line, TagVal, PrevVals, false)
	:-
	member('package: '-Package, PrevVals),
	default_module(Package, TagVal).

        %% For tag iso:
        %% If the tag occurs at the beginning
        %% of the line, get it's value; there is no default.
get_tag_val('iso: ', TagLen, Line, TagVal, _, true)
        :-
        Tag = 'iso: ',
        sub_atom(Line, 0, TagLen, _, Tag),
	!,
        sub_atom(Line, TagLen, _, 0, TagVal).
get_tag_val('iso: ', _, Line, '', _, false).
	
handle_header_lines([], RestLines0, [], _, RestLines)
	:-
	RestLines0 = [Line | RestLines],
	sub_atom(Line, 0, 11, _, 'predicates:').
	
handle_header_lines([Tag-TagLen | RestYamlTags], [Line | RestYmlLines], 
			[Tag-TagVal | TagsVals], PrevVals, RestLines)
	:-
	get_tag_val(Tag, TagLen, Line, TagVal, PrevVals, Present),
	(Present==true -> ContYmlLines = RestYmlLines 
		; 
	 ContYmlLines = [Line | RestYmlLines]
	),
	handle_header_lines(RestYamlTags, ContYmlLines, 
				TagsVals, [Tag-TagVal | PrevVals], RestLines).

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



	%% Group = g(MD_File, PredBlocks, TagsVals).
	%% lmbp(Md_Groups, MdInfoByPreds, MdInfoByPredsTail).
	
list_md_by_pred([], GPLTail, GPLTail).
list_md_by_pred([Group | Md_GroupsTail], GroupPredList, FinalGPLTail)
	:-
	expnd_mdg(Group, GroupPredList, NextGPLTail),
	list_md_by_pred(Md_GroupsTail, NextGPLTail, FinalGPLTail).

expnd_mdg(g(MD_File, PredBlocks, TagsVals), GroupPredList, NextGPLTail)
	:-
/*
%nl,write(MD_File=PredBlocks),nl,
nl,write(MD_File),nl,
(MD_File == 'assert.md' -> trace ; true),
*/
	expnd_mdg_predblks(PredBlocks, MD_File, TagsVals, GroupPredList, NextGPLTail).

expnd_mdg_predblks([], MD_File, TagsVals, Tail, Tail).
expnd_mdg_predblks([Item | PredBlocksList], MD_File, TagsVals, GroupPredList, FinalGPLTail)
	:-
	xpnd_pred_item(Item, MD_File, TagsVals, GroupPredList, NextGPLTail),
	expnd_mdg_predblks(PredBlocksList, MD_File, TagsVals, NextGPLTail, FinalGPLTail).


xpnd_pred_item([], MD_File, TagsVals, Tail, Tail).

xpnd_pred_item(p(PA,Desc), MD_File, TagsVals, [p(PA,Desc,MD_File,TagsVals) | NextGPLTail], NextGPLTail).

xpnd_pred_item(mp(ItemList), MD_File, TagsVals, GroupPredList, NextGPLTail)
	:-!,
%write(mp(MD_File)>ItemList),nl,
	xpnd_pred_item(ItemList, MD_File, TagsVals, GroupPredList, NextGPLTail).

xpnd_pred_item([Item | ItemList], MD_File, TagsVals, GroupPredList, FinalGPLTail)
	:-
%write(MD_File > ItemList),nl,
	xpnd_pred_item(Item, MD_File, TagsVals, GroupPredList, InterGPLTail),
	xpnd_pred_item(ItemList, MD_File, TagsVals, InterGPLTail, FinalGPLTail).







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





