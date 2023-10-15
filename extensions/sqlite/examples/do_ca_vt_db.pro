/*============================================================================*
                                do_ca_vt_db.pro


 *============================================================================*/

:-['sqlite3_intf.pro'].
:-['load_csv.pro'].

xv :- setup_births_db_ca_vt.
t :- setup_births_db.

module ca_vt.
use sqlite3.
use csv.

export setup_births_db_ca_vt/0.
export setup_births_db/0.

	% 5,350 records
%source_data_file('us_births_2016_2021.csv').
%sqlite_db_file('db_us_births_2016_2021.sqlite3').

	% 214 records
source_data_file('ca_vt_births_2016_2021.csv').
sqlite_db_file('db_ca_vt_2016_2021.sqlite3').

sqlite_table(births).

	%% initial 'INT' added for 'RowId':
types_list(['INT', 'TEXT', 'TEXT', 'INT', 'TEXT', 'TEXT', 'INT', 'INT', 'REAL', 'REAL']).

primary('RowId').


	%% Takes 3 or 4 minutes on a fast mac.
	%% Uncomment the printf's below to get progress printing
	%% Once the db is built, the interface to it is
	%% fast (see q1 below) ; all the effort is in cleaning the data:

%setup_births_db
setup_births_db_ca_vt
	:-
	source_data_file(SrcPath),
	sqlite_db_file(DBName),
	cont_setup_births_db(SrcPath, DBName).

cont_setup_births_db(SrcPath, DBName)
        :-
        sqlite_table(TableName),
        types_list(TypesList),
        col_names_from_datfile(SrcPath, TypesList, ColNamesList, DataFileStream),
	adj_names_sql(ColNamesList, AdjColNamesList),
        drop_db(DBName),
        sqlite3_open(DBName, DBHandle),
        setup_table(DBHandle, TableName, AdjColNamesList, TypesList),
        load_data_lines(TableName, TypesList, DBHandle, DataFileStream, 0, FinalCount).

col_names_from_datfile(SrcPath, TypesList, ColNamesList, DataFileStream)
        :-
        open(SrcPath, read, DataFileStream),
        get_line(DataFileStream, FirstLine),
	input_csv_line(FirstLine, ',', FileNamesList),
        ColNamesList = ['RowId' | FileNamesList],
	assert(column_names(ColNamesList)).

adj_names_sql([], []).
adj_names_sql([FN | FileNamesList], [AdjFN | AdjFileNamesList])
	:-
	open(atom(AdjFN), write, S),
	printf(S, '''%t''', [FN]),
	close(S),
	adj_names_sql(FileNamesList, AdjFileNamesList).

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

load_data_lines(TableName, TypesList, DBHandle, DataFileStream, FinalCount, FinalCount)
	:-
	at_end_of_stream(DataFileStream),
	!,
	close(DataFileStream).

load_data_lines(TableName, TypesList, DBHandle, DataFileStream, CurCount, FinalCount)
	:-
	get_line(DataFileStream, LineAtom),
	input_csv_line(LineAtom, ',', FieldsAtomsList),
	TypesList = [_ | TypesListTail],
	adj_items_for_sql(FieldsAtomsList, TypesListTail, AdjFieldsAtomsList),
	RowList = [CurCount | AdjFieldsAtomsList],
	insert_one_row(DBHandle, TableName, RowList),
%write(row=RowList),nl,
	NextCount is CurCount + 1,
	load_data_lines(TableName, TypesList, DBHandle, DataFileStream, NextCount, FinalCount).
	
	
adj_items_for_sql([], [], []).
adj_items_for_sql([FieldAtom | FieldsAtomsList], [Type | TypesList], [FieldVal | AdjFieldsAtomsList])
	:-
	adj_for_sql(Type, FieldAtom, FieldVal),
	adj_items_for_sql(FieldsAtomsList, TypesList, AdjFieldsAtomsList).

adj_for_sql('TEXT', FN, AdjFN)
	:-!,
	atom_codes(FN, FNCs),
	open(atom(AdjFN), write, S),
	dbl_sng_w(FNCs, S),
	close(S).

adj_for_sql(_, FN, AdjFN)
	:-!,
	open(atom(FN),read,InS),
	read_term(InS,AdjFN, [attach_fullstop(true)] ),
	close(InS).

adj_for_sql(_, FN, FN).

dbl_sng_w([], S).

dbl_sng_w([0'' | FNCs], S)
	:-!,
	put_code(S, 0''),
	put_code(S, 0''),
	dbl_sng_w(FNCs, S).

dbl_sng_w([C | FNCs], S)
	:-
	put_code(S, C),
	dbl_sng_w(FNCs, S).



%column_names(ColNamesList)

export query_births0/0.
query_births0 
	:-
        sqlite_db_file(DBName),
        sqlite3_open(DBName, DBHandle),
        sqlite_table(TableName),
        printf('\n\n>>> Births query #0: select_all_table\n', []),
        select_all_table(DBHandle, TableName, Result),
        column_names(ColumnNames),
        display_result(Result, ColumnNames).
  
display_result(Result, ColNames)
        :-
        rterms_2_lists(Result, ListOfLists),
                %% columns is from the library:
        columns([u(ColNames) | ListOfLists]).
        
rterms_2_lists([], []).
rterms_2_lists([RTerm | ListOfRTerms], [List | ListOfLists])
        :-
        RTerm =.. [r | List],
        rterms_2_lists(ListOfRTerms, ListOfLists).

         



endmod.
