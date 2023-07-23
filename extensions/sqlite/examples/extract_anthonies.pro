/*============================================================================*
		extract_anthonies.pro

	Downloaded

	https://lincolnlibraries.org/bookguide/award-winners/anthony-awards/

	to
		AA_Lincoln.txt

	and discarded the first 3022 lines (no target data there).
	As usual, all the effort lies in scraping and cleaning the source data.

	Sample query at end of this file.
 *============================================================================*/
    	%% For dev: the searchdir fact finds sqlite3_intf.pro, 
	%% and then finds sqlite3_intf.psl:
module builtins.
searchdir('..').
endmod.

:-['sqlite3_intf.pro'].


xl :- setup_aa_db.

source_data_file('AA_Lincoln.txt').
sqlite_db_file('anthonies.sqlite3').
sqlite_table(aa).	%% anthony awards
column_names(['RowId', 'Author', 'Title', 'Year', 'Award', 'Read']).
types_list(['INT', 'TEXT', 'TEXT', 'INT', 'TEXT', 'INT']).
primary('RowId').

setup_aa_db
	:-
	extract_anthonies(OutLines),
	populate_db(OutLines).

extract_anthonies(OutLines)
	:-
	source_data_file(SourceHTMLFile),
	DASHCodes = [32,226,128,147,32],
	atom_codes(DASH, DASHCodes),
	assert(dash(DASH)),
	grab_lines(SourceHTMLFile, RawLines),
	process_lines(RawLines, 0, OutLines),
%write_lines(OutLines),
true.

populate_db(OutLines)
	:-
	sqlite_db_file(TargetSQLiteFile),
	sqlite_db_file(DBName),
	sqlite_table(TableName),
	column_names(ColNamesList),
	types_list(TypesList),
	drop_db(DBName),
	sqlite3_open(DBName, DBHandle),
	setup_table(DBHandle, TableName, ColNamesList, TypesList),
	insert_rows(OutLines, DBHandle, TableName, TypesList, 0),
	sqlite3_close(DBHandle).
	


process_lines([], _, []).
process_lines([L | RawLines], Year, [XInfo | OutLines])
	:-
%printf('L:   >> %t\n', [L]),
	filter_line(L, Ltype, LAdj, Awd),
	!,
	xinfo(Ltype, LAdj, Awd, Year, NextYear, XL),
	mki(Ltype, XL, Year, NextYear, Info, Awd, XInfo),
%printf('>> %t\n', [XInfo]),
	process_lines(RawLines, NextYear, OutLines).

process_lines([L | RawLines], Year, OutLines)
	:-
	process_lines(RawLines, Year, OutLines).

filter_line(L, 'D', L, ddd)
	:-
	sub_atom(L, 0, 11, _, '<p><strong>').

filter_line(L, 'pB', L2, Awd)
	:-
	sub_atom(L, 0, 8, _, '<p>Best '),
	sub_atom(L, 3, _, 0, L2),
	filter_line(L2, 'B', _, Awd).

	%% Don't want Novellas, but 'Novel' succeeds on 'Novella'
filter_line(L, 'B', L, Awd)
	:-
	Awd = 'Best Novella',
	sub_atom(L, 0, 12, _, Awd),
	!,
	fail.

filter_line(L, 'B', L, Awd)
	:-
	Awd = 'Best Novel',
	sub_atom(L, 0, 10, _, Awd).

filter_line(L, 'B', L, Awd)
	:-
	Awd = 'Best First Novel',
	sub_atom(L, 0, 16, _, Awd).

filter_line(L, 'B', L, Awd)
	:-
	AwdTag = 'Best Paperback',
	sub_atom(L, 0, 14, _, AwdTag),
	!,
	Awd = 'Best Paperback Original'.

xinfo('D', L, ddd, Year, NextYear, ddd)
	:-
	sub_atom(L, 11, _, 0, L1),
	sub_atom(L1,B,1,_, '>'),
	D is B + 1,
	sub_atom(L1, D,_,0,E),
	sub_atom(E,0,4,_,NextYear).

xinfo(Ltype, L, Awd, Year, Year, i(Author, Title))
	:-
	(Ltype = 'B', !; Ltype = 'pB'),
	!,
	get_title_author(L, Author, Title).


get_title_author(L, Author, Title)
	:-
	sub_atom(L, Bef, 1, Aft, '<'),
	cont_get_title_author(L, Bef, Aft, Author, Title).

cont_get_title_author(L, Bef, Aft, Author, Title)
	:-
	Aft =< 5,
	!,
	sub_atom(L, 0, Len, 6, AA),
	atom_codes(DASH, [32,226]),
	atom_split(AA, DASH, ZZ),
	ZZ = [ZZ1, ZZ2, ZZ3],
	sub_atom(ZZ2, 3, _, 0, Author),
	sub_atom(ZZ3, 3, _, 0, Title).

cont_get_title_author(L, Bef, Aft, Author, Title)
	:-
	sub_atom(L, 0, Bef,_, Awd_Auth),
	atom_codes(DASH, [226,128,147,32]),
	atom_split(Awd_Auth, DASH, ASegs),
	ASegs = [_, Author | _],

	sub_atom(L, Bef, _, 0, L2),
	sub_atom(L2, Bef2, 1, Aft2, '>'),
	Bef2A is Bef2+1,
	sub_atom(L2, Bef2A, LL2A, 0, TA),
	Bef0 is Bef2+ 1,
	sub_atom(L2, Bef0, _, 0, InitTitle),
	sub_atom(InitTitle, BT0, 1, _, '<'),
	sub_atom(InitTitle,0,BT0,_,Title).

mki('D', XL, _, NextYear, NextYear, Awd, NextYear)
	:-!.

mki(Ltype, i(Auth, Title), Year, NextYear, Info, Awd, i(Auth,Title,Year, Awd)).
	
strip_atom_ends_white(AtomIn, AtomOut)
	:-
	atom_codes(AtomIn, AtomInCodes),
	strip_both_white(AtomInCodes, StrippedAtomCodes),
	atom_codes(AtomOut, StrippedAtomCodes).

setup_table(DBHandle, TableName, ColNamesList, TypesList)
	:-
	primary(Primary),
	create_cols_spec(ColNamesList, TypesList, ColumnsList),
	sql_create_table(DBHandle, TableName, ColumnsList, Primary).

create_cols_spec([], [], []).
create_cols_spec([ColName | RestColNamesList], [ColType | RestTypesList], [ColSpec | RestColsList])
	:-
	create_col_spec(ColName, ColType, ColSpec),
	create_cols_spec(RestColNamesList, RestTypesList, RestColsList).

create_col_spec(ColName, ColType, [column_name=ColName, column_type=ColType]).






	%% i(S.A. Cosby,Razorblade Tears,2022,Best Novel),
	%% i(Author,Title,Year,Award),
	%% column_names(['RowId', 'Author', 'Title', 'Year', 'Award', 'Read']).
	%% types_list(   ['INT',   'TEXT',   'TEXT',  'INT',  'TEXT',  'INT']).
	%%	All Read entries default to 0 (unread)
insert_rows([], DBHabndle, TableName, TypesList, Counter)
	:-
	sqlite_db_file(DBName),
	printf('Finished inserting %t rows to table %t in database %t\n', [Counter, TableName, DBName]).

insert_rows([OLine | OutLines], DBHandle, TableName, TypesList, Counter)
	:-
	OLine = i(Author,Title,Year,Award),
	!,
	setup_insert_list([Counter,Author,Title,Year,Award,0], InsertsList),
	insert_one_row(DBHandle, TableName, InsertsList),
	NextCounter is Counter + 1,
	insert_rows(OutLines, DBHandle, TableName, TypesList, NextCounter).

insert_rows([OLine | OutLines], DBHabndle, TableName, TypesList, Counter)
	:-
	insert_rows(OutLines, DBHabndle, TableName, TypesList, Counter).

setup_insert_list([], []).
setup_insert_list([RawDataItem | RestLineList],  [ColInsertItem | RestInsertsList])
	:-
	open(atom(ColInsertItem), write, S),
	printf(S, '%t', [RawDataItem]),
	close(S),
	setup_insert_list(RestLineList, RestInsertsList).

/*
sqlite3 anthonies.sqlite3
SQLite version 3.39.5 2022-10-14 20:58:05
Enter ".help" for usage hints.

sqlite> pragma table_info(aa);
0|RowId|INT|0||1
1|Author|TEXT|0||0
2|Title|TEXT|0||0
3|Year|INT|0||0
4|Award|TEXT|0||0
5|Read|INT|0||0
*/

	%% You can issue queries against the database:
	%% This query utilizes the columns/1 predicate
	%% from the file rows_cols.pro in the ALS Prolog library
	%% to display the query result:
q2 :-
	sqlite_db_file(DBName),
	sqlite3_open(DBName, DBHandle),
	sqlite_table(TableName),
	select_all_table(DBHandle, TableName, Result),
	column_names(ColumnNames),
	rterms_2_lists(Result, ListOfLists),
	columns([u(ColumnNames) | ListOfLists]).

rterms_2_lists([], []).
rterms_2_lists([RTerm | ListOfRTerms], [List | ListOfLists])
	:-
	RTerm =.. [r | List],
	rterms_2_lists(ListOfRTerms, ListOfLists).

	


	
	
