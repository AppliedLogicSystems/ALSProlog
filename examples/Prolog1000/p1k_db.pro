/*---------------------------------------------------------------------*
 |		p1k_db.pro
 |		Copyright (c) 1998-99 Applied Logic Systems, Inc.
 |		Copying per "Copying ALS"
 |
 |		Process and display the original Prolog 1000 database
 |		1.  Process the original text source into a file of
 |			lists of tagged values.
 |		2.	Process the previous file into a simple database
 |			file of application records.
 |		3.	Provide access to the records via a Tcl/Tk form.
 |		----------------------------------------------------------
 |		p1k_db/0 
 |			Browse the Prolog 1000 database (p1000.db) with a GUI form
 |		rd1000/0
 |			Read the original Prolog 1000 text source into a tagged 
 |			prolog term format and store the result in p1000.pro
 |		build_p1k_db
 |			Process p1000.pro into the database file p1000.pro
 |
 *---------------------------------------------------------------------*/

p1k_db
	:-
	set_prolog_flag(unknown, fail),
	init_global_db_access,
	init_tk_alslib,
	loadup_p1k_db('p1000.db').

init_global_db_access
	:-
	clause(set_db_access,_),
	!.
init_global_db_access
	:-
	make_gv('_db_access'),
	make_gv('_cur_rec').

loadup_p1k_db(File)
	:-
	open(File, read_write, DBStream, []),
	index1_idx(Index1HeaderPos),
	stream_position(DBStream, _, Index1HeaderPos),
	read(DBStream, Index1Start),
	stream_position(DBStream, _, Index1Start),
	read(DBStream, PositionList),
	PosVector =.. [pp | PositionList],
	findall(Tag,  field_type(Tag, _), Tags0),
	sort(Tags0, Tags),
	functor(PosVector, _, NumRecs),
	open(tk_win(tcli,'.dbf.main.purposetext'),write,_,[alias(purpose_win)]),
	open(tk_win(tcli,'.dbf.main.desc.text'),write,_,[alias(desc_win)]),
	open(tk_win(tcli,'.dbf.addl.text'),write,_,[alias(addl_win)]),
	set_db_access(dba(1, PosVector, NumRecs, Tags, DBStream, PurposeStream, DescStream)),
	tcl_call(tcli,[source,'dbform.tcl'], _),
	catenate('of ',NumRecs,TN),
	tcl_call(tcli, ['.dbf.main.control.total_num',configure,'-text',TN],_),
	lookup(1).


	/*
		dba(cur_pos, pos_vec, num_recs, tags, db_st, pur_st, desc_st)
	 */
dba_code(cur_pos,	1).
dba_code(pos_vec,	2).
dba_code(num_recs,	3).
dba_code(tags,		4).
dba_code(db_st,		5).
dba_code(pur_st,	6).
dba_code(desc_st,	7).

get_dba(Item, Value)
	:-
	dba_code(Item,	N),
	get_db_access(DBA),
	arg(N, DBA, Value).

set_dba(Item, Value)
	:-
	dba_code(Item,	N),
	get_db_access(DBA),
	mangle(N, DBA, Value).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Displaying records
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

show_next
	:-
	get_dba(cur_pos, N),
	M is N+1,
	lookup(M).

show_previous
	:-
	get_dba(cur_pos, N),
	M is N-1,
	lookup(M).

lookup(N) :-
	get_dba(num_recs, NumRecs),
	1 =< N,
	N =< NumRecs,
	!,
	get_dba(pos_vec, PosVector),
	arg(N, PosVector, RPos),
	get_dba(tags, Tags),
	get_dba(db_st, DB_Stream),
	read_db_rec_at(RPos, Tags, DB_Stream, Record),
	set_cur_rec(Record),
	display_record(Record, N),
	set_dba(cur_pos, N).

display_record(Record, N)
	:-
	get_pgm_name(Record, Name),
	check_default(Record, '<Domain>', '', Domain0),
	(atomic(Domain0) -> Domain = Domain0 ; catenate(Domain0, Domain)),
	get_pgm_developers(Record, Developers),
	check_default(Record, '<Organization>', '', Organization),
	tcl_call(tcli, [displ_head,Name,N],_),
	tcl_call(tcli, [displ_group2,Domain,Developers,Organization], _),
	tcl_call(tcli, ['.dbf.main.purposetext',delete,'1.0',end],_),
	tcl_call(tcli, ['.dbf.main.desc.text',delete,'1.0',end],_),
	check_default(Record, '<Purpose>', [], Purpose),
	check_default(Record, '<Description>', [], Description),
	write_lines(purpose_win, Purpose),
	tcl_call(tcli, ['.dbf.main.purposetext',see,'1.0'],_),
	write_lines(desc_win, Description),
	tcl_call(tcli, ['.dbf.main.desc.text',see,'1.0'],_),
	check_display_addl.

get_pgm_name(Record, Name)
	:-
	dmember('<Program Name>' = Name, Record),
	Name \= '',
	!.

get_pgm_name(Record, Name)
	:-
	dmember('<Program>' = Name, Record),
	Name \= '',
	!.

get_pgm_name(Record, 'Unknown Name').

get_pgm_developers(Record, Developers)
	:-
	dmember('<Developers>' = Developers, Record),
	Developers \= '',
	!.

get_pgm_developers(Record, Developers)
	:-
	dmember('<Developed by>' = Developers, Record),
	Developers \= '',
	!.

get_pgm_developers(Record, 'Unknown developers').


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Fields in the source application descriptions:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

field_type('<Program>',       	   line).		%what it is conventionally called in the documentation
field_type('<Program Name>',       line).		%what it is conventionally called in the documentation
field_type('<Number>',             integer).	%assigned receipt number
field_type('<Purpose>',            line).		%(up to 20 words)
field_type('<Developers>',         line).		%people mainly responsible
field_type('<Developed by>',       line).		%people mainly responsible
field_type('<Organization>',       line).		%directing or sponsoring the work
field_type('<Country>',            line).		%in which developed
field_type('<Domain>',   		   line_list).  %the work area and activity to which this is relevant.
field_type('<Contact>',            line).		%Name of someone who can answer questions, with
field_type('<Address>',            line).		%and
field_type('<Telephone>',          line).		%and
field_type('<Email>',              line).		%address (or fax number) for the contact (if known)
field_type('<Description>',        line_list).	%(up to150 words)
field_type('<Prolog>',             line_list).	%Name of interpreter/compiler(s) used
field_type('<Prolog system>',      line_list).	%Name of interpreter/compiler(s) used
field_type('<Platform>',           line).		%Hardware/operating system(s) (development/delivery)
field_type('<Other languages>',    line).		%or systems used, with approximate extent
field_type('<Lines>',              line).		%Approximate number of lines of Prolog
field_type('<Predicates>',         line).		%Approximate number of Prolog predicates
field_type('<Development effort>', line).		%Number of person-months for initial system
field_type('<Maintenance effort>', line).		%Number of person-months since, including enhancements
field_type('<Type of Use>', 	   line).		%In-house/out-of-house
field_type('<Current Status>',     line).	%1=Prototype, 2=Evaluation, 3=Released,
					 							%4=No longer supported, 5=Dead
field_type('<Use>',                line).	%1=Personal use, 2=in-House use, 3=Free/public domain,
					 							%4=Licensed, 5=Sold product
field_type('<No. of Users>',              line).	%Approximate number
field_type('<Users>',              line).	%Approximate number
field_type('<Date deployed>',      line).		%Month/year of inital practical use of program
field_type('<When Deployed>',      line).		%Month/year of inital practical use of program
field_type('<Dated>',              line).		%date to which this information refers
field_type('<Information Date>',   line).		%date to which this information refers
field_type('<References>',         line_list).	%Citations of published or available work, if any

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%  Read the entries from the original Prolog 1000 
	%%  text source into a "tagged list" format, and 
	%%  write the results to the file 'p1000.pro' .
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rd1000 
	:-
	rd1000(ProgramList),
	open('p1000.pro',write,OS,[]),
	write_clauses(OS, ProgramList, [quoted(true)]),
	close(OS).
	
source_file('prolog1000.v1').

rd1000(ProgramList)
	:-
	source_file(SourceFile),
	open(SourceFile, read, IS, []),
	rd1000_raw(IS, RawProgramList),
	close(IS),
	printf(user_output, 'Completed reading source file: %t\n\n', [SourceFile]),
	flush_output(user_output),
	refine_to_tags(RawProgramList, ProgramList).

rd1000_raw(IS, [RawPgmDesc | ProgramList])
	:-
	rd_1000_raw_pgm(IS, RawPgmDesc),
	!,
	rd1000_raw(IS, ProgramList).

rd1000_raw(IS, []).

rd_1000_raw_pgm(IS, [Ln1 | RestRawPgmDesc])
	:-
	locate_pgm_start_line(IS, Ln1),
	read_to_pgm_end(IS, RestRawPgmDesc).

read_to_pgm_end(IS, RestRawPgmDesc)
	:-
	get_line(IS, Ln),
	!,
	disp_read_to_pgm_end(Ln, IS, RestRawPgmDesc).

read_to_pgm_end(IS, []).

disp_read_to_pgm_end(Ln, IS, [Ln])
	:-
	is_last_pgm_line(Ln),
	!.

disp_read_to_pgm_end(Ln, IS, [Ln | RestRawPgmDesc])
	:-
	read_to_pgm_end(IS, RestRawPgmDesc).

locate_pgm_start_line(IS, Ln)
	:-
	get_line(IS, NxtLn),
	disp_locate_pgm_start_line(NxtLn, IS, Ln).

disp_locate_pgm_start_line(Ln, IS, Ln)
	:-
	sub_atom(Ln, 0,9,_,'<Program>'),
	!.
disp_locate_pgm_start_line(_, IS, Ln)
	:-
	locate_pgm_start_line(IS, Ln).

is_last_pgm_line(Ln)
	:-
	sub_atom(Ln, 0,10,_,'</Program>').

refine_to_tags([], []).
refine_to_tags([RawPgm | RawProgramList], [TggdPgm | TaggedLinePgms])
	:-
	parse_tags(RawPgm, InitTggdPgm),
	catch(refine_tagged_items(InitTggdPgm, TggdPgm), Ball, notify(InitTggdPgm,Ball)),
	(dmember('<Program>'=PP, TggdPgm); dmember('<Program Name>'=PP, TggdPgm) ),
printf(user_output, '%t\n', [PP]),
flush_output(user_output),
	refine_to_tags(RawProgramList, TaggedLinePgms).

parse_tags([], []).
parse_tags([Ln | RestRawPgm], [Tag=List | RestTggdPgm])
	:-
	extract_tag(Ln, Tag, RestLine),
	List = [RestLine | RestList],
	c2nxt_tag(RestRawPgm, RestList, TailRawPgm, NextTaggedLine),
	continue_parse_tags(NextTaggedLine, TailRawPgm, RestTggdPgm). 

extract_tag(Ln, Tag, RestLine)
	:-
	sub_atom(Ln, 0, 1, _, '<'),
	sub_atom(Ln, K, 1, _, '>'),
	K1 is K+1,
	sub_atom(Ln, 0,K1,_,Tag),
	sub_atom(Ln, K1, _, 0, RestLine).

c2nxt_tag([], [], [], '</Program>').
c2nxt_tag([Ln | TailRawPgm], [], TailRawPgm, Ln)
	:-
	sub_atom(Ln, 0, 1, _, '<'),
	!.

c2nxt_tag([Ln | RestRawPgm], [Ln | RestList], TailRawPgm, NextTaggedLine)
	:-
	c2nxt_tag(RestRawPgm, RestList, TailRawPgm, NextTaggedLine).

continue_parse_tags(NextTaggedLine, _, [])
	:-
	is_last_pgm_line(NextTaggedLine),
	!.

continue_parse_tags(NextTaggedLine, TailRawPgm, RestTggdPgm)
	:-
	parse_tags([NextTaggedLine | TailRawPgm], RestTggdPgm).

refine_tagged_items([], []).
refine_tagged_items([Tag=List | RestInitTggdPgm], [Tag = RefinedList | TaggdPgm])
	:-
	(field_type(Tag, Type) -> true ; throw(bad(Tag))),
	refine_by_type(Type, List, RefinedList),
	refine_tagged_items(RestInitTggdPgm, TaggdPgm).

refine_by_type(line, List, [L])
	:-
	length(List, N),
	fixup_line(N, List, LL),
	atom_codes(LL, LLCs),
	strip_white(LLCs, Mid),
	strip_tail_white(Mid, LCs),
	atom_codes(L, LCs).

refine_by_type(integer, [L | _], [N])
	:-
	atomread(L, N).

refine_by_type(line_list, List, RefinedList)
	:-
	clean_for_white(List, RefinedList).

fixup_line(0, List, '??') :-!.
fixup_line(1, [L], LL) 
	:-!,
	clean_for_white([L],[LL]).
fixup_line(_, List, LL) 
	:-
	catenate_ww(List, LL).

clean_for_white([], []).
clean_for_white([LL | List], [L | RefinedList])
	:-
	atom_codes(LL, LLCs),
	strip_white(LLCs, Mid),
	dreverse(Mid, RM),
	strip_white(RM, SRM),
	dreverse([0' | SRM], LCs),
	atom_codes(L, LCs),
	clean_for_white(List, RefinedList).

catenate_ww(List, LL)
	:-
	clean_for_white(List, CList),
	catenate(CList, LL).


notify(InitTggdPgm,Ball)
	:-
	printf(user_output,'\nBad Tag: %t\n',[Ball]),
	printf(user_output,'%t\n\n',[InitTggdPgm]).

rd_notes
	:-
	grab_lines('2check', NL),
	prologify(NL, PNL),
	open('2check.pro', write, OS, []),
	write_clauses(OS, PNL),
	close(OS).

prologify([], []).
prologify(['' | NL], PNL)
	:-!,
	prologify(NL, PNL).
prologify([L1,L2 | NL], [nt(Name,Num) | PNL])
	:-
	extract_tag(L1, T1, NameLine),
	extract_tag(L2, T2, NumLine),
	atom_codes(NameLine, LLCs),
	strip_white(LLCs, Mid),
	dreverse(Mid, RM),
	strip_white(RM, SRM),
	dreverse([0' | SRM], LCs),
	atom_codes(Name, LCs),
	atomread(NumLine, Num),
	prologify(NL, PNL).


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Build/Re-Build the database from a file of
	%% tagged-list versions of the p1000 records:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_p1k_db
	:-
	findall(Tag,  field_type(Tag, _), Tags0),
	sort(Tags0, Tags),
	open('p1000.pro', read, InS, []),
	open('p1000.db',read_write, OutS, []),
	build_p1k_db(InS, Tags, OutS).

build_p1k_db(InS, Tags, OutS)
	:-
	header_size(InitPos),
	stream_position(OutS, _, InitPos),
	catch( build_p1k_db(InS, Tags, OutS, PosList),
			Ball,
			bad_build(InS, OutS, Ball)
		   ),
	!,
	finish_build(OutS, PosList).

bad_build(InS, OutS, Ball)
	:-
	close(InS), 
	close(OutS),
	printf(user_output, 'Error during processing%\n\t%t\n', [Ball]),
	fail.

/*-------------------------------------------------------------------*
 	"Structure" of the database file:

	<File>		:= <Header><Records><Indicies>

	<Header> 	:= <IdxOffs>	:= <IdxOff> [ <IdxOffs> ]
	<Records>	:= <Record> [ <Records> ]
	<Indicies>	:= <Index>  [ <Indicies> ]

	<IdxOff>	:= <integer>.\n
	<Index>		:= <IndexTerm>.\n

	<Record>	:= <Fields>	:= <Field> [ <Fields> ]

	<Field>		:=	<string_field> | <string_list_field>
					Note: integers are stored as a string of digits

	<string_field> 		:= <string>\n

	<string_list_field>	:= <string_field> [<string_list_field>]\n
 *-------------------------------------------------------------------*/
header_size(20).
index1_idx(0).

build_p1k_db(InS, Tags, OutS, PosList)
	:-
	read(InS, Item0),
	disp_build_p1k_db(Item0, InS, Tags, OutS, PosList).

disp_build_p1k_db(end_of_file, InS, Tags, OutS, [])
	:-
	close(InS).

disp_build_p1k_db(Item0, InS, Tags, OutS, [CurPos | RestPosList])
	:-
	sort(Item0, Item),
	stream_position(OutS, CurPos, current_position),
	write_list_db_rec(Tags, Item, OutS),
	!,
put_code(user_output, 0'.), 
	build_p1k_db(InS, Tags, OutS, RestPosList).

disp_build_p1k_db(Item0, InS, Tags, OutS, [CurPos | RestPosList])
	:-
	printf(user_output, 'Bad item:\n%t\n\n', [Item0]),
	build_p1k_db(InS, Tags, OutS, RestPosList).

write_list_db_rec([], TaggedList, Stream)
	:-
	write_end_of_record(Stream).

write_list_db_rec([Tag | MasterOrder], [Tag = Value | TaggedList], Stream)
	:-!,
	field_type(Tag, Type),
	write_db_field(Type, Value, Stream),
	write_end_of_field(Stream),
	write_list_db_rec(MasterOrder, TaggedList, Stream).

write_list_db_rec([Tag | MasterOrder], TaggedList, Stream)
	:-!,
	write_end_of_field(Stream),
	write_list_db_rec(MasterOrder, TaggedList, Stream).

write_db_field(line, [Value], Stream)
	:-
	put_atom(Stream, Value).

write_db_field(line, Value, Stream)
	:-
	put_atom(Stream, Value).

write_db_field(integer, [Value], Stream)
	:-
	write(Stream, Value).

write_db_field(integer, Value, Stream)
	:-
	write(Stream, Value).

write_db_field(line_list, Value, Stream)
	:-
	write_db_list(Value, Stream).

write_db_list([], Stream).
write_db_list([Line | Lines], Stream)
	:-
	put_atom(Stream,Line),
	nl(Stream),
	write_db_list(Lines, Stream).

write_end_of_record(Stream)
	:-
	put_code(Stream, 23).

write_end_of_field(Stream)
	:-
	nl(Stream).

finish_build(OutS, PosList)
	:-
	stream_position(OutS, _, end_of_stream),
	index1_idx(Index1HeaderPos),
	stream_position(OutS, Index1Start, Index1HeaderPos),
	write(OutS, Index1Start),put_code(OutS, 0'.), nl(OutS),
	stream_position(OutS, _, Index1Start),
	write(OutS, PosList), put_code(OutS, 0'.), nl(OutS),
	close(OutS).


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Access the record at a given offset:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_db_rec_at(RecStart, Tags, IS, Record)
	:-
	stream_position(IS, _, RecStart),
	read_db_rec(Tags, IS, Record),
	!.

read_db_rec([], IS, [])
	:-
	get_code(IS, C),
	(C = 23 -> true ;
		printf('Warning: Record does not end with ^W\n', [])).

read_db_rec([Tag | Tags], IS, [Tag = Value | Record])
	:-
	field_type(Tag, Type),
	read_db_field(Type, IS, Value),
	read_db_rec(Tags, IS, Record).

read_db_field(line_list, IS, Value)
	:-
	read_db_line_list(IS, Value).

read_db_field(line, IS, Value)
	:-
	get_line(IS, Value).

read_db_field(integer, IS, Value)
	:-
	get_line(IS, Value0),
	atomread(Value0, Value).

read_db_line_list(IS, Value)
	:-
	get_line(IS, Line0),
	disp_read_db_line_list(Line0, IS, Value).

disp_read_db_line_list('', IS, []) :-!.
disp_read_db_line_list(Line, IS, [Line | Lines]) 
	:-
	read_db_line_list(IS, Lines).

display_addl
	:-
	get_cur_rec(Record),
	Record \= 0,
	!,
	Exclude = [
		'<Program Name>', '<Program>', '<Developers>', '<Developed by>',
		'<Domain>', '<Organization>', '<Purpose>', '<Description>' ],
	display_addl(Record, Exclude).
display_addl.

display_addl([], _).
display_addl([Field | Record], Exclude)
	:-
	display_addl_field(Field, Exclude),
	display_addl(Record, Exclude).

display_addl_field(_='', _)
	:-!.

display_addl_field(Tag=Value, Exclude)
	:-
	dmember(Tag, Exclude),
	!.

display_addl_field(Tag=Value, Exclude)
	:-
	atomic(Value),
	!,
	printf(addl_win, '%t: %t\n', [Tag, Value]).

display_addl_field(Tag=Value, Exclude)
	:-
	printf(addl_win, '%t:\n', [Tag]),
	write_lines(addl_win, Value).

check_display_addl
	:-
	tcl_call(tcli, [get_tcl_ga, app, 'Addl'], AddlStatus),
	(AddlStatus = open ->
		display_addl
		;
		true
	).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_record(Words, Searchmode, SelField, Start)
	:-
	get_dba(num_recs, NumRecs),
	get_dba(pos_vec, PosVector),
	get_dba(tags, Tags),
	get_dba(db_st, DB_Stream),
	Next is Start + 1,
	Next =< NumRecs,
	find_rec(Next, NumRecs, Start, Words,  Searchmode, 
				SelField, PosVector, Tags, DB_Stream),
	!.
find_record(Words, Searchmode, SelField, Start)
	:-
	tcl_call(tcli, [displ_rnum, Start], _),
	info_dialog('No (more) records found.', 'Search Results').
			
find_rec(N,NRs,Start,Words, Mode, SelField, PosVector, Tags, DB_Stream)
	:-
	arg(N, PosVector, RPos),
	read_db_rec_at(RPos, Tags, DB_Stream, Record),
	chk_rec(Mode,Record,Words, SelField),
	!,
	set_cur_rec(Record),
	display_record(Record, N),
	set_dba(cur_pos, N).

find_rec(N,NRs,Start,Words, Mode, SelField, PosVector, Tags, DB_Stream)
	:-
	N < NRs,
	M is N+1,
	tcl_call(tcli, [displ_rnum, M], _),
	find_rec(M,NRs,Start,Words, Mode, SelField, PosVector, Tags, DB_Stream).

chk_rec(some,Record,Words, SelField)
	:-
	tgt_fields_contents(Record, SelField, Contents),
%	check_default(Record, SelField, [], FieldVal),
	occurs_some(Words, Contents).

chk_rec(all,Record,Words, SelField)
	:-
	check_default(Record, SelField, [], FieldVal),
	occur_all(Words, FieldVal).

:- dynamic(related_field_name/2).

related_field_name('<Program>', '<Program Name>').
related_field_name('<Developers>', '<Developed by>').
related_field_name('<Prolog>', '<Prolog system>').

tgt_fields_contents(Record, SelField, Contents)
	:-
	findall(RFN, related_field_name(SelField, RFN), RelFields),
	extract_field_vals([SelField | RelFields], Record, Contents).

extract_field_vals([], Record, []).
extract_field_vals([Field | Fields], Record, Contents)
	:-
	extract_fld_val(Record, Field, [], FieldVal),
	append(FieldVal, ContentsTail, Contents),
	extract_field_vals(Fields, Record, ContentsTail).

extract_fld_val(Record, Field, [], FieldVal)
	:-
	dmember(Field=InitVal, Record),
	!,
	(InitVal = '' -> 
		FieldVal = []
		;
		(atomic(InitVal) -> 
			FieldVal = [InitVal]
			;
			FieldVal = InitVal
		)
	).
extract_fld_val(Record, Field, [], []).

occurs_some([Word | Words], FieldVal)
	:-
	occurs_among(FieldVal, Word),
	!.
occurs_some([_ | Words], FieldVal)
	:-
	occurs_some(Words, FieldVal).

occurs_among([FieldWord | FieldWords], Word)
	:-
	sub_atom(FieldWord, _,_,_, Word),
	!.

occurs_among([_ | FieldWords], Word)
	:-
	occurs_among(FieldWords, Word).

occurs_among(FieldWord, Word)
	:-
	atomic(FieldWord),
	sub_atom(FieldWord, _,_,_, Word),
	!.

occur_all([], FieldVal).
occur_all([Word | Words], FieldVal)
	:-
	occurs_among(FieldVal, Word),
	!,
	occur_all(Words, FieldVal).

