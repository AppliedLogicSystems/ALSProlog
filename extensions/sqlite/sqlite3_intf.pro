/*=======================================================================*
 |                      sqlite3_intf.pro
 |		Copyright (c) 2023 Applied Logic Systems, Inc.
 |
 |              -- Prolog side of the Prolog <--> Sqlite interface
 |
 |		See psql3_doc.txt faor documentaion and examples
 |		See also tests_sqlite3_intf.pro
 |
 |    In the following, DBAccess is either a DBName or a DBHandle.
 | If DBAccess is a DBHandle, the statement is executed  against the 
 | DBHandle database..  If DBAccess is a DBName, sqlite3_open is called 
 | (on the C side) to obtain a DBHandle against which the the SQL 
 | statement in question is executed.
 *=======================================================================*/
:-['./sqlite3_intf.psl'], write('LOCAL LOADED sqlite3_intf.psl'), nl,nl.

module sqlite3.

export sql_create_table/4.
export sql_create_table/5.
export sql_create_table_string/5.
export sql_drop_table/2.
export insert_one_row/3.
export insert_one_row/4.
export insert_one_row_string/3.
export insert_rows/3.
export select_one_table/3.
export select_all_table/3.
export sql_query_table_all_string/2.
export sql_select_string/4.
export select_where_lists/5.
export build_sql_select_string/4.
export sql_update/4.
export sql_update_string/4.
export sql_delete/3.
export sql_delete_string/3.
export sql_clear_table/2.
export sql_create_index/4.
export sql_create_index/7.
export sql_index_string/6.
export sql_drop_index/2.
export query_foreign_keys/2.
export set_foreign_keys_on/1.
export set_foreign_keys_off/1.
	
	/*----------------------------------------------------------------*
	  sql_create_table/4
	  sql_create_table(DBAccess, TableName, ColumnsList, Primary)
	  sql_create_table(+, +, +, +)
	 *----------------------------------------------------------------*/
sql_create_table(DBAccess, TableName, ColumnsList, Primary)
	:-
	sql_create_table(DBAccess, TableName, ColumnsList, Primary, []).

sql_create_table(DBAccess, TableName, ColumnsList, Primary, ForeignKeys)
	:-
	sql_create_table_string(TableName, ColumnsList, Primary, ForeignKeys, CreateTableString),
	sqlite3_exec_norows(DBAccess, CreateTableString).

sql_create_table_string(TableName, ColumnsList, Primary, ForeignKeyData, CreateTableString)
	:-
	open(atom(CreateTableString), write, S),
	printf(S, 'CREATE TABLE %t (', [TableName]),
	write_sql_create_table_columns(ColumnsList, S, Primary, ForeignKeyData),
	printf(S, ');', []),
	close(S).

write_sql_create_table_columns([], S, Primary, FKLine)
	:-
	((FKLine \= nothing, FKLine \= []) -> 
		printf(S, ', %t ', [FKLine])
		; 
		true
	).

write_sql_create_table_columns([ColumnSpec], S, Primary, ForeignKeys)
	:-
	write_sql_column(ColumnSpec, S, Primary),
write_sql_create_table_columns([], S, Primary, ForeignKeys).

write_sql_create_table_columns([ColumnSpec | RestColumnSpecs], S, Primary, ForeignKeys)
	:-
	write_sql_column(ColumnSpec, S, Primary),
	printf(S, ', ', []),
	write_sql_create_table_columns(RestColumnSpecs, S, Primary, ForeignKeys).



write_sql_column(ColumnSpec, S, Primary)
	:-
	member(column_name=ColumnName, ColumnSpec),
	member(column_type=IncomingColumnType, ColumnSpec),
	sql_type(IncomingColumnType, ColumnType),
	(member(constraints=ConstraintsList, ColumnSpec) -> true ; ConstraintsList = []),
	printf(S, '%t %t ', [ColumnName, ColumnType]),
	(ColumnName = Primary -> printf(S, ' PRIMARY KEY', []) 
		; 
		print_constraints(ConstraintsList, S)
	).

sql_type(integer, 'INT').
sql_type(int, 'INT').
sql_type('INT', 'INT').
sql_type('INTEGER', 'INT').
sql_type(text, 'TEXT').
sql_type('TEXT', 'TEXT').
sql_type('REAL', 'REAL').       
sql_type(double, 'REAL').
sql_type(float, 'REAL').

write_foreign_keys([], S).
write_foreign_keys([ForeignKey], S)
	:-!,
	write_out_foreign_key(ForeignKey, S, last).
write_foreign_keys([ForeignKey | ForeignKeys], S)
	:-
	write_out_foreign_key(ForeignKey, S, notlast),
	write_foreign_keys(ForeignKeys, S).

	%fk(<local column name>, <foreign table identifying db functor>, <foreign table column>)
write_out_foreign_key(fk(LocalColName, ForeignTable, ForeignColumn), S, Where)
	:-
	printf(S, 'FOREIGN KEY(%t) REFERENCES %t(%t) ', [LocalColName, ForeignTable, ForeignColumn]),
	(Where = last -> printf(S, '', []) ; printf(S, ', ', []) ).

print_constraints([], _).

print_constraints([Constraint], S)
	:-!,
	printf(S, '%t', [Constraint]).

print_constraints([Constraint | RestConstraints], S)
	:-
	printf(S, '%t ', [Constraint]),
	print_constraints(RestConstraints, S).

/* -------------------------------------------------------------- *
	sql_drop_table/2
	sql_drop_table(DBAccess, TableName)
	sql_drop_table(+, +)
 * -------------------------------------------------------------- */
sql_drop_table(DBAccess, TableName)
	:-
	catenate(['DROP TABLE IF EXISTS ', TableName, ';'], Cmd),
	sqlite3_exec_norows(DBAccess, Cmd).
	

	/* -------------------------------------------- *
	    Make various INSERT strings

	    -- ListOfSingles (InsertList)  should condsist i
	    of strings of the argument vector of a 
	    conplete insert, such as

		'4, \'Volvo\', 29000'
	    for

	    INSERT INTO Cars VALUES(4, \'Volvo\', 29000);

	    Each individual vector is fleshed out and
	    the resulting list is assenbled into a
	    single string such as this:

	    'INSERT INTO Cars VALUES(1, \'Audi\', 52642); \
             INSERT INTO Cars VALUES(2, \'Mercedes\', 57127); \
             INSERT INTO Cars VALUES(3, \'Skoda\', 9000); \
             INSERT INTO Cars VALUES(4, \'Volvo\', 29000);'
	 * -------------------------------------------- */

	/*----------------------------------------------------------------*
	      insert_one_row/3
	      insert_one_row(DBAccess, TableName, RowList)
	      insert_one_row(+, +, +)

	   RowList is a list making up the values for a single row of
	   TablqeName, for example:

			[ 3, 'Skoda', 9000 ]

	   Notice that Skoda carries ssingle-quotes because it is a prolog
	   UIA, not (yet) because of SQL text quoting.  insert_one_row/3
	   will combine all the individual values into a single UIA "vector"

		'3, \'Skoda\', 9000'
	   
	   which then becomes part 0f a comlete SQL statement

		'INSERT INTO Cars VALUES(3, \'Skoda\', 9000);' 

	   whiach can be executed by sqlite3_exec_norows/2.
	 *----------------------------------------------------------------*/
insert_one_row(DBAccess, TableName, RowList)
	:-
	insert_one_row(DBAccess, TableName, RowList, false).

insert_one_row(DBAccess, TableName, RowList, Q4Boolean)
	:-
	insert_one_row_string(TableName, RowList, Q4Boolean, InsertString),
        sqlite3_exec_norows(DBAccess, InsertString).

insert_one_row_string(TableName, RowList, Q4Boolean, InsertString)
	:-
	open(atom(InsertString), write, S),
	printf(S, 'INSERT INTO %t VALUES (', [TableName]), 
	(RowList = [_|_] -> 
		compose_row(RowList, Q4Boolean, S)
		;
		RowList =.. [_ | Args],
		compose_row(Args, Q4Boolean, S)
	),
	printf(S, ');', []),
	close(S).

compose_row([], Q4Boolean, S).
compose_row([Value | RowList], Q4Boolean, S)
	:-
	number(Value),
        !,
	(RowList = [] -> 
		printf(S, '%t ', [Value])
		;
		printf(S, '%t, ', [Value]),
		compose_row(RowList, Q4Boolean, S)
	).

compose_row([Value | RowList], Q4Boolean, S)
	:-
	atom(Value),
        !,
	(RowList = [] -> 	% whether there's a terminating comma output:
		(Q4Boolean=true ->
			atom_codes(Value, VCs),
		%	put_single_quotes(3, S),
			put_single_quotes(1, S),
			q4_write(VCs, S),
		%	put_single_quotes(3, S)
			put_single_quotes(1, S)
			;
			printf(S, '\'%t\' ', [Value])
		)
		;
		(Q4Boolean=true ->
			atom_codes(Value, VCs),
			put_single_quotes(1, S),
			q4_write(VCs, S),
			put_single_quotes(1, S),
			put_code(S, 0',),
			compose_row(RowList, Q4Boolean, S)
			;
			printf(S, '\'%t\', ', [Value]),
			compose_row(RowList, Q4Boolean, S)
		)
	).

put_single_quotes(0, S) :-!.
put_single_quotes(N, S)
	:-
	put_code(S, 0''),
	M is N-1,
	put_single_quotes(M, S).

q4_write([], S).
q4_write([0'' | CCs], S)
        :-
	put_single_quotes(4, S),
        q4_write(CCs, S).
q4_write([CC | CCs], S)
        :-
	put_code(S, CC),
        q4_write(CCs, S).


insert_rows([], DBAccess, TableName).
insert_rows([RowList | ListOfRows], DBAccess, TableName)
	:-
	insert_one_row(DBAccess, TableName, RowList),
	insert_rows(ListOfRows, DBAccess, TableName).









	/* -------------------------------------------- *
	    Simple SELECT * FROM TABLE SQL queries
	 * -------------------------------------------- */

select_one_table(DBAccess, TableName, Result)
	:-
	sql_select_string('*', TableName, '', SelectString),
        Limit = 1,
        sqlite3_exec_rows(DBAccess, SelectString, Limit,  Result).

select_all_table(DBAccess, TableName, Result)
	:-
	sql_select_string('*', TableName, '', SelectString),
        Limit = all,
        sqlite3_exec_rows(DBAccess, SelectString, Limit,  Result).


	/* -------------------------------------------- *
	    Make various SELECT FROM TABLE SQL strings
	 * -------------------------------------------- */

sql_query_table_all_string(TableName, QueryString)
	:-
	open(atom(QueryString), write, S),
	printf(S, 'SELECT * FROM %t;', [TableName]),
	close(S).

	/* Simple case where Cols and Wheres are both strings: 
	   ColsString cab be explicit col names (e.g. 'Name,Price')
	   or can be '*' or the atom all;
	 */

	%% Component args are strings
sql_select_string(ColsString, Table, WhereClauseString, SelectString)
	:-
	ck_sql_cols_str(ColsString, OKColsString),
	(WhereClauseString = '' ->
		catenate(['SELECT ', OKColsString, ' FROM ', Table], SelectString)
		;
		catenate(['SELECT ', OKColsString, ' FROM ', Table, ' WHERE ', WhereClauseString], 
			SelectString)
	).

ck_sql_cols_str('*', '*').
ck_sql_cols_str(' * ', '*').
ck_sql_cols_str(' all_cols ', '*').
ck_sql_cols_str('all_cols', '*').

/*-----------------------------------------------------------------------------------*
select_where_lists/5
select_where_lists(DBHandle, TableName, ColAtomsList, WhereClauseList, Result)
select_where_lists(+, +, +, +, -)

	* ColAtomsList is a list of atoms naming a subset of the columns of TableName
	* WhereClauseList is a list of atoms (UIAs) expressing sql conditons on
	  those columns of TableName

 *-----------------------------------------------------------------------------------*/

select_where_lists(DBAccess, TableName, ColAtomsList, WhereClauseList, Result)
	:-
        build_sql_select_string(ColAtomsList, TableName, WhereClauseList, SelectString),
        Limit = all,
        sqlite3_exec_rows(DBAccess, SelectString, Limit,  Result).

build_sql_select_string(ColAtomsList, TableName, WhereClauseList, SelectString)
	:-
	open(atom(SelectString), write, S),
	printf(S, 'SELECT ', []),
	write_select_cols(ColAtomsList, S),
	printf(S, ' FROM %t ', [TableName]),
	printf(S, ' WHERE  ', []),
	write_where_clauses(WhereClauseList, S),
	printf(S, '; ', []),
	close(S).

write_select_cols([], S).
write_select_cols('*', S)
        :-!,
        printf(S, '%t ', ['*']).
write_select_cols([ColId], S)
	:-!,
	printf(S, '%t ', [ColId]).
write_select_cols([ColId | ColAtomsList], S)
	:-
	printf(S, '%t, ', [ColId]),
	write_select_cols(ColAtomsList, S).

write_where_clauses([], S).
write_where_clauses([Clause], S)
	:-!,
	printf(S, '%t ', [Clause]).
write_where_clauses([Clause | ClauseList], S)
	:-
	printf(S, '%t AND ', [Clause]),
	write_where_clauses(ClauseList, S).

write_out([], S).
write_out([Item], S)
	:-!,
	printf(S, ' %t', [Item]).
write_out([Item | XClauseList], S)
	:-
	printf(S, ' %t', [Item]),
	write_out(XClauseList, S).

	/* -------------------------------------------- *
	    Make various UPDATE TABLE SQL strings
	 * -------------------------------------------- */

sql_update(DBAccess, TableName, SetList, WhereClauseList)
	:-
	sql_update_string(TableName, SetList, WhereClauseList, UpdateString),
        sqlite3_exec_norows(DBAccess, UpdateString).

sql_update_string(TableName, SetList, WhereClauseList, UpdateString)
	:-
	open(atom(UpdateString), write, S),
	printf(S,'UPDATE %t SET ', [TableName]),
	write_sql_sets(SetList, S),
	printf(S, ' WHERE ', []),
	write_where_clauses(WhereClauseList, S),
	printf(S, ';', []),
	close(S).

write_sql_sets([], S).
write_sql_sets([Set], S)
	:-!,
	printf(S, '%t ', [Set]).
write_sql_sets([Set | SetList], S)
	:-
	printf(S, '%t, ', [Set]),
	write_sql_sets(SetList, S).


	/* -------------------------------------------- *
	    		Delete rows
	 * -------------------------------------------- */

sql_delete(DBAccess, TableName, WhereClauseList)
	:-
	sql_delete_string(TableName, WhereClauseList, DeleteString),
        sqlite3_exec_norows(DBAccess, DeleteString).

sql_delete_string(TableName, WhereClauseList, DeleteString)
	:-
	open(atom(DeleteString), write, S),
	printf(S, 'DELETE FROM %t WHERE ', [TableName]),
	write_where_clauses(WhereClauseList, S),
	printf(S, ';', []),
	close(S).

	/* -------------------------------------------- *
	    		Clear table
	 * -------------------------------------------- */

sql_clear_table(DBAccess, TableName)
	:-
	open(atom(ClearString), write, S),
	printf(S, 'DELETE FROM %t;', [TableName]),
	close(S),
        sqlite3_exec_norows(DBAccess, ClearString).


	/* -------------------------------------------- *
	    		Create index
		See https://www.sqlite.org/lang_createindex.html

	   ColExpr is either a ColName, or
		asc(ColName)  or  desc(ColName)
	   WhereClause: as in select, involving ColExpr
	 * -------------------------------------------- */
sql_create_index(ColExpr, TableName, IndexName, DBHandle)
	:-
	sql_create_index(ColExpr, '', TableName, IndexName, '', [], DBHandle).

	%%Primary automatically gets an index when table is created:
sql_create_index(ColExpr, Modifier, TableName, IndexName, Primary, WhereClause, DBHandle)
	:-
	( ColExpr = Primary ; ColExpr = asc(Primary), ColExpr = desc(Primary) ),
	!.

sql_create_index(ColExpr, Modifier, TableName, IndexName, Primary, WhereClause, DBHandle)
	:-
	sql_index_string(ColExpr, Modifier, TableName, IndexName, WhereClause, CreateIndexString),
        sqlite3_exec_norows(DBHandle, CreateIndexString).
	
sql_create_index(ColExprsList, Modifier, TableName, IndexName, Primary, WhereClause, DBHandle)
	:-
	sql_index_string(ColExprsList, Modifier, TableName, IndexName, WhereClause, CreateIndexString),
        sqlite3_exec_norows(DBHandle, CreateIndexString).
	
sql_index_string(ColExpr, Modifier, TableName, IndexName, WhereClause, IndexString)
	:-
	open(atom(IndexString), write, S),
	begin_index_string(TableName, IndexName, Modifier, IndexString, WhereClause, S),
	printf(S, ' ( ', []),
	add_index_cols(ColExpr, S),
	printf(S, ' ) ', []),
	add_where_clause(WhereClause, S),
	printf(S, ';', []),
	close(S).
		
begin_index_string(TableName, IndexName, Modifier, IndexString, WhereClause, S)
	:-
	(nonvar(Modifier), Modifier = unique),
	!,
	printf(S, 'CREATE UNIQUE INDEX IF NOT EXISTS %t ON %t ', [IndexName, TableName]).
begin_index_string(TableName, IndexName, Modifier, IndexString, WhereClause, S)
	:-
	printf(S, 'CREATE INDEX IF NOT EXISTS %t ON %t ', [IndexName, TableName]).

add_index_cols([ColExpr | ColExprs], S)
	:-!,
	add_index_col(ColExpr, S),
	(ColExprs = [] -> true 
		;
		printf(S, ' , ', []),
		add_index_cols(ColExprs, S)
	).

add_index_cols(ColExpr, S)
	:-
	add_index_col(ColExpr, S).

add_index_col(asc(ColName), S)
	:-!,
	printf(S, ' %t ASC ', [ColName]).
add_index_col(desc(ColName), S)
	:-!,
	printf(S, ' %t DESC ', [ColName]).
add_index_col(ColName, S)
	:-
	printf(S, ' %t ', [ColName]).

add_where_clause([], _)
	:-!.

add_where_clause([WhereClause], S)
	:-!,
	add_where_clause(WhereClause, S).

	%% Assumes WhereClause is properly formed,
	%% syntacticallyl & semantically
add_where_clause(WhereClause, S)
	:-
	printf(S, ' WHERE %t ', [WhereClause]).

	/* -------------------------------------------- *
	   		Drop an Index
	 * -------------------------------------------- */
sql_drop_index(IndexName, DBHandle)
	:-
	open(atom(DropString), write, S),
	printf(S, 'DROP INDEX %t;', [IndexName]),
	close(S),
        sqlite3_exec_norows(DBHandle, DropString).


	/* -------------------------------------------- *
	   	Turn foreign key constraint support
		on/off, query status
		See:
		https://www.sqlite.org/foreignkeys.html
		https://www.sqlite.org/pragma.html

Note from https://www.sqlite.org/foreignkeys.html:
	"Foreign key constraints are disabled by default (for backwards compatibility), so must be enabled separately for each database connection. (Note, however, that future releases of SQLite might change so that foreign key constraints enabled by default. Careful developers will not make any assumptions about whether or not foreign keys are enabled by default but will instead enable or disable them as necessary.)"
	Consequence:  if foreign key constraint is required (e.g. performing inserts, deletes,
	updates, etc.), caution must be exercise about how you make calls which connect to the db.  
	Separate calls which use the db NAME are separate connections to the db.  So, for example,
		set_foreign_keys_on(DBName),
		insert_one_row(DBName, TableName, RowList)
	invoke separate connections.  So even though the set_foreign_keys_on(DBName) call turns
	foreign key constraint support, the 2nd call, insert_one_row(...) occurs through a
	separate connection, and foreign key constraint support will NOT be on for that
	connection.  One needs to separately open a connection, recieving a DBHandle, and
	then pass that DBHandle to the two calls:
		sqlite3_open(DBName, DBHandle),
		    ...
		set_foreign_keys_on(DBHandle),
		insert_one_row(DBHandle, TableName, RowList)
	This discipline is only necessary when foreign key constraint support is required,
	e.g. inserts, deletes, updates, etc.  It is not necessary for selects, etc.
	 * -------------------------------------------- */
query_foreign_keys(DBAccess, Result)
	:-
        sqlite3_exec_rows(DBAccess, 'SELECT * FROM pragma_foreign_keys();', 1,  SelectResult),
	SelectResult = [r(Result)].
	
set_foreign_keys_on(DBHandle)
	:-
        sqlite3_exec_norows(DBHandle, 'PRAGMA foreign_keys = 1;').

set_foreign_keys_off(DBHandle)
	:-
        sqlite3_exec_norows(DBHandle, 'PRAGMA foreign_keys = 0;').

	/* ============================================ *
	    		Utilities
	 * ============================================ */

	/* -------------------------------------------- *
	   Double all occurrences of single quotes in a 
	   prolog string (sequence of codes)
	 * -------------------------------------------- */
export dbl_sgn_q/2.
dbl_sgn_q([], []).
dbl_sgn_q([0'' | Cs], [0'', 0'' | DCs])
        :-
        dbl_sgn_q(Cs, DCs).
dbl_sgn_q([C | Cs], [C | DCs])
        :-
        dbl_sgn_q(Cs, DCs).
export quad_sgn_q/2.
quad_sgn_q([], []).
quad_sgn_q([0'' | Cs], [0'', 0'', 0'', 0'' | DCs])
        :-
        quad_sgn_q(Cs, DCs).
quad_sgn_q([C | Cs], [C | DCs])
        :-
        quad_sgn_q(Cs, DCs).

quad_sgn_q_a(Atom, DCs).


export drop_db/1.
drop_db(DBName) :- 
	(exists_file(DBName) ->
		remove_file(DBName) ; true ).

export check_db_exists/1.
check_db_exists(DBName) :- 
	((DBName \= ':memory:', DBName \= '') -> 
		exists_file(DBName) ; true).

endmod.
