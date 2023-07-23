/*============================================================================*
				csv_simple.pro

	An example of setting up a db from a csv file.
	The raw data source is:  

	https://www.kaggle.com/datasets/danbraswell/temporary-us-births

	The Kaggle file was downloaded to 

		us_births_2016_2021.csv  (from kaggle)

	The first line of the Kaggle file specifies the column names:

		State,State Abbreviation,Year,Gender,Education Level of Mother,\
		Education Level Code,Number of Births,\
		Average Age of Mother (years),Average Birth Weight (g)

	An additional INT (RowId) column is added as primary key.

	As is typical, most of the effort lies in cleaning and adjusting the
	data for insertion, including the column names.

	Below, unary facts specify the files and names:

	source_data_file: 
		a local path to a source csv file 
	sqlite_db_file: 
		a path to the target Sqlite3 data file (to be created if necessary)
		in which the table will be created
	sqlite_table: 
		name of the table
	types_list:
		a list sqlite3-appropriate types to be assigned  to the columns
		specified by the first line of the source_data_file
	primary:
		specifies the name of the additional primary key

	Sample query at end of this file.
 *============================================================================*/
    	%% For dev: the searchdir fact finds sqlite3_intf.pro, 
	%% and then finds sqlite3_intf.psl:
module builtins.
searchdir('..').
endmod.

:-['sqlite3_intf.pro'].

source_data_file('us_births_2016_2021.csv').
sqlite_db_file('db_us_births_2016_2021.sqlite3').
sqlite_table(births).
	%% initial 'INT' added for 'RowId':
types_list(['INT', 'TEXT', 'TEXT', 'INT', 'TEXT', 'TEXT', 'INT', 'INT', 'REAL', 'REAL']).
primary('RowId').

t :- setup_births_db.

	%% Takes 3 or 4 minutes on a fast mac.
	%% Uncomment the printf's below to get progress printing
	%% Once the db is built, the interface to it is
	%% fast (see q1 below) ; all the effort is in cleaning the data:

setup_births_db
	:-
	sqlite_db_file(DBName),
	sqlite_table(TableName),
	source_data_file(SrcPath),
	col_names_from_datfile(SrcPath, FileColNamesList),
	ColNamesList = ['RowId' | FileColNamesList],
	types_list(TypesList),
	drop_db(DBName),
	sqlite3_open(DBName, DBHandle),
	setup_table(DBHandle, TableName, ColNamesList, TypesList),
	load_data_lines(TableName, TypesList, DBHandle).

col_names_from_datfile(SrcPath, ColNamesList)
	:-
	open(SrcPath, read, ISS),
	get_line(ISS, FirstLine),
	atom_split(FirstLine, ',', InitFirstLineList),
	adjust_colnames_for_sql(InitFirstLineList, ColNamesList),
	close(ISS).

setup_table(DBHandle, TableName, ColNamesList, TypesList)
	:-
	primary(Primary),
	create_cols_spec(ColNamesList, TypesList, ColumnsList),
	sql_create_table(DBHandle, TableName, ColumnsList, Primary).

adjust_colnames_for_sql([], []).
adjust_colnames_for_sql([InitName | InitFirstLineList], [AdjustedName | FirstLineList])
	:-
	adjust_col_name(InitName, AdjustedName),
	adjust_colnames_for_sql(InitFirstLineList, FirstLineList).

adjust_col_name(InitName, AdjustedName)
	:-
	atom_codes(InitName, InitCodes),
	spaces2underbars(InitCodes, InitCodes0),
	rmv_adj_punct(InitCodes0, AdjInitCodes),
	atom_codes(AdjustedName, AdjInitCodes).

spaces2underbars([], []).
spaces2underbars([0'  | RestStringIn], [0'_ | RestStringOut])
	:- !,
	spaces2underbars(RestStringIn, RestStringOut).
spaces2underbars([C | RestStringIn], [C | RestStringOut])
	:-
	spaces2underbars(RestStringIn, RestStringOut).

rmv_adj_punct([], []).
rmv_adj_punct([0'" | RestStringIn], [0'", 0'" | RestStringOut])
	:- !,
	rmv_adj_punct(RestStringIn, RestStringOut).
rmv_adj_punct([C | RestStringIn], RestStringOut)
	:-
	C < 32,
	!,
	rmv_adj_punct(RestStringIn, RestStringOut).
rmv_adj_punct([C | RestStringIn], RestStringOut)
	:-
	(32 < C, C < 48),
	!,
	rmv_adj_punct(RestStringIn, RestStringOut).
rmv_adj_punct([C | RestStringIn], RestStringOut)
	:-
	(57 < C, C < 65),
	!,
	rmv_adj_punct(RestStringIn, RestStringOut).
rmv_adj_punct([C | RestStringIn], RestStringOut)
	:-
	(90 < C, C < 95,!; C=96),
	!,
	rmv_adj_punct(RestStringIn, RestStringOut).
rmv_adj_punct([C | RestStringIn], RestStringOut)
	:-
	(122 < C,  C < 128),
	!,
	rmv_adj_punct(RestStringIn, RestStringOut).
rmv_adj_punct([C | RestStringIn], [C | RestStringOut])
	:-
	rmv_adj_punct(RestStringIn, RestStringOut).

create_cols_spec([], [], []).
create_cols_spec([ColName | RestColNamesList], [ColType | RestTypesList], [ColSpec | RestColsList])
	:-
	create_col_spec(ColName, ColType, ColSpec),
	create_cols_spec(RestColNamesList, RestTypesList, RestColsList).

create_col_spec(ColName, ColType, [column_name=ColName, column_type=ColType]).

load_data_lines(TableName, TypesList, DBHandle)
	:-
	source_data_file(SrcPath),
	open(SrcPath, read, ISS),
		%% skip over first line (already used it for col names)
	get_line(ISS, _),	
	load_data_lines(DBHandle, TableName, TypesList, ISS, 0),
	close(ISS).

load_data_lines(DBHandle, TableName, TypesList, ISS, Counter)
	:-
	get_line(ISS, NextSrcLine),
	!,
	setup_data_line(NextSrcLine, InitNextLineList),
	setup_insert_list([Counter | InitNextLineList], InsertsList),

%printf('\n>>>> %t\n\n',[InsertsList]),
%put_code(0'.),
	insert_one_row(DBHandle, TableName, InsertsList),
	NextCounter is Counter + 1,
	!,
	load_data_lines(DBHandle, TableName, TypesList, ISS, NextCounter).
load_data_lines(ISS, TableName, Counter, TypesList, DBHandle)
	:-
	printf('\nFinshed loading %t data lines\n\n', [Counter]).

setup_data_line(SrcLine, InitLineList)
	:-
	atom_split(SrcLine, '"', QuotesSubs),
	QuotesSubs = [First, QMiddle, Last],
	!,
	adj_mid(QMiddle, Middle),
	atom_split(First, ',', FirstList),
	clean_trailing_spaces(FirstList, CleanFirstList),
	append(LFL, [_], FirstList),
	atom_split(Last, ',', LastList),
	LastList = [FLL | RLL],
	TheLastList = RLL,
	append(LFL, [Middle | TheLastList], InitLineList).
	
setup_data_line(SrcLine, InitLineList)
	:-
	atom_split(SrcLine, ',', InitLineList).

clean_trailing_spaces([], []).
clean_trailing_spaces([Atom | RestAtomList], [CleanAtom | RestCleanAtomList])
	:-
	clean_spaces(Atom, CleanAtom),
	clean_trailing_spaces(RestAtomList, RestCleanAtomList).

clean_spaces(Atom, CleanAtom)
	:-
	atom_codes(Atom, AtomCodes),
	strip_tail_white(AtomCodes, CleanAtomCodes),
	atom_codes(CleanAtom, CleanAtomCodes).

adj_mid(QMiddle, Middle)
	:-
	atom_codes(QMiddle, QCodes),
	fixm(QCodes, FXDQCodes),
	atom_codes(Middle, [0'" | FXDQCodes]).

fixm([], [0'"]).
fixm([0'' | RestQCodes], [0'', 0'' | RestFXDQCodes])
	:-
	fixm(RestQCodes, RestFXDQCodes).
fixm([C | RestQCodes], [C | RestFXDQCodes])
	:-
	fixm(RestQCodes, RestFXDQCodes).

setup_insert_list([], []).
setup_insert_list([RawDataItem | RestLineList],  [ColInsertItem | RestInsertsList])
	:-
	open(atom(ColInsertItem), write, S),
	printf(S, '%t', [RawDataItem]),
	close(S),
	setup_insert_list(RestLineList, RestInsertsList).

/*
You can use the commandline sqlite3 to examoine the result:
> sqlite3 db_us_births_2016_2021.sqlite3 
SQLite version 3.39.5 2022-10-14 20:58:05
Enter ".help" for usage hints.

sqlite> pragma table_info(births);
0|RowId|INT|0||1
1|State|TEXT|0||0
2|State_Abbreviation|TEXT|0||0
3|Year|INT|0||0
4|Gender|TEXT|0||0
5|Education_Level_of_Mother|TEXT|0||0
6|Education_Level_Code|INT|0||0
7|Number_of_Births|INT|0||0
8|Average_Age_of_Mother_years|REAL|0||0
9|Average_Birth_Weight_g|REAL|0||0
*/

	%% You can issue queries against the database:
	%% This query utilizes the columns/1 predicate
	%% from the file rows_cols.pro in the ALS Prolog library
	%% to display the query result:
q1 :-
	sqlite_db_file(DBName),
	sqlite3_open(DBName, DBHandle),
	sqlite_table(TableName),
	ColAtomsList = ['State','Year','Average_Birth_Weight_g'],
	WhereClauseList = ['Average_Birth_Weight_g > 3440','Year=2020'],
	select_where_lists(DBHandle, TableName, ColAtomsList, WhereClauseList, Result),
	rterms_2_lists(Result, ListOfLists),
	columns([u(ColAtomsList) | ListOfLists]).

rterms_2_lists([], []).
rterms_2_lists([RTerm | ListOfRTerms], [List | ListOfLists])
	:-
	RTerm =.. [r | List],
	rterms_2_lists(ListOfRTerms, ListOfLists).

	


	
	
