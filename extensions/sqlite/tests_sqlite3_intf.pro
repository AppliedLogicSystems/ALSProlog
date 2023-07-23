/*=======================================================================*
 |                      tests_sqlite3_intf.pro
 |		Copyright (c) 2023 Applied Logic Systems, Inc.
 |
 |              Tests for the Prolog <--> Sqlite interface
 |
 |	All tests utilize the sqlite3 database testsq3.db
 *=======================================================================*/
:-['sqlite3_intf.pro'].
:-['../../core/alsp_src/tests/test.pro'].

	/* Cf. https://www.sqlite.org/inmemorydb.html */
test_db(':memory:').
%test_db('').
%test_db('testsq3.db').

test_sqlite3
	:-
	test_db(DBName),
	drop_db(DBName),
	test([
	test_open(DBName),
	test_open_close(DBName),
	test_create_table_string('Cars'),
	test_create_table_string(members),

	test_create_table_by_name(DBName, 'Cars'),
	test_create_table_by_handle(DBName, 'Cars'),
	test_create_table_by_name(DBName, members),
	test_create_table_by_handle(DBName, members),

	test_insert_rows(DBName, 'Cars'),
	test_insert_rows(DBName, members),

	test_create_insert_table(DBName, 'Cars'),
	test_create_insert_table(DBName, members),

	test_select_one_table(DBName, 'Cars'),
	test_select_one_empty_table(DBName),
	test_select_one_table(DBName, members),

	test_select_all_table(DBName, 'Cars'),
	test_select_all_table(DBName, members),
	test_select_all_table_2(DBName, 'Cars'),
	test_select_all_table_2(DBName, members),

	test_select_all_where(DBName, 'Cars'),
	test_select_all_where(DBName, members),

	test_update(DBName, 'Cars'),
	test_update(DBName, members),

	test_delete(DBName, 'Cars'),
	test_delete(DBName, members),

	test_foreign_key_constraint_manip(DBName),

	true]).

/*
drop_db(DBName) :- 
	(exists_file(DBName) ->
		remove_file(DBName) ; true ).

check_db_exists(DBName) :- 
	((DBName \= ':memory:', DBName \= '') -> 
		exists_file(DBName) ; true).
*/


test_open(DBName)
	:-
	sqlite3_open(DBName, DBHandle), 
	check_db_exists(DBName).

test_open_close(DBName)
	:-
	drop_db(DBName),
	sqlite3_open(DBName, DBHandle),
	check_db_exists(DBName),
	sqlite3_close(DBHandle).
 

table_spec('Cars', ColumnsList, Primary)
	:-
	TableName = 'Cars',
        ColumnsList = [
                [column_name='Id', column_type='INT'],
                [column_name='Name', column_type='TEXT'],
                [column_name='Price', column_type='INT'] ],
        Primary = 'Id'.

table_spec(members, ColumnsList, Primary)
	:-
	TableName = 'members',
        ColumnsList = [
		[column_name=contact_id, column_type='INTEGER'],
		[column_name=first_name, column_type='TEXT', constraints=['NOT NULL']],
		[column_name=last_name, column_type='TEXT', constraints=['NOT NULL']],
		[column_name=email, column_type='TEXT', constraints=['NOT NULL', 'UNIQUE']],
		[column_name=balance, column_type='REAL', constraints=['NOT NULL']] ],
	Primary = contact_id.

expected_create_table_string_for('Cars', 
		'CREATE TABLE Cars (Id INT PRIMARY KEY, Name TEXT, Price INT);').
expected_create_table_string_for(members, 
		'CREATE TABLE members (contact_id INTEGER PRIMARY KEY, first_name TEXT NOT NULL, last_name TEXT NOT NULL, email TEXT NOT NULL UNIQUE, balance REAL NOT NULL);').

test_create_table_string(TableName)
	:-
	table_spec(TableName, ColumnsList, Primary),
	expected_create_table_string_for(TableName, TgT),
	sql_create_table_string(TableName, ColumnsList, Primary, CreateTableString),
	TgT = CarsCreateTableString.

	/* ------------------------------------------ *
	   Create table; access to db is via db name
	   in arg 1 of sqlite3_exec_norows
	 * ------------------------------------------ */
test_create_table_by_name(DBName, TableName)
	:-
	drop_db(DBName),
	table_spec(TableName, ColumnsList, Primary),
	sql_create_table(DBName, TableName, ColumnsList, Primary).

	/* ------------------------------------------ *
	   Create table; access to db is via dbHandle
	   in arg 1 of sqlite3_exec_norows
	 * ------------------------------------------ */
test_create_table_by_handle(DBName, TableName)
	:-
	drop_db(DBName),
	table_spec(TableName, ColumnsList, Primary),
	sql_create_table_string(TableName, ColumnsList, Primary, CreateTableString),
	sqlite3_open(DBName, DBHandle),
	sqlite3_exec_norows(DBHandle, CreateTableString).

	/* ------------------------------------------------------------------------------------- *
		Inserts

	   Cars
                [column_name='Id', column_type='INT'],
                [column_name='Name', column_type='TEXT'],
                [column_name='Price', column_type='INT'] ],

	   members
		[column_name=contact_id, column_type='INTEGER'],
		[column_name=first_name, column_type='TEXT', constraints=['NOT NULL']],
		[column_name=last_name, column_type='TEXT', constraints=['NOT NULL']],
		[column_name=email, column_type='TEXT', constraints=['NOT NULL', 'UNIQUE']],
		[column_name=balance, column_type='REAL', constraints=['NOT NULL']] ],
	 * ------------------------------------------------------------------------------------- */
setup_table(DBHandle, TableName)
	:-
	table_spec(TableName, ColumnsList, Primary),
	sql_create_table(DBHandle, TableName, ColumnsList, Primary),
	insert_list_for(TableName, ListOfRows),
	insert_rows(ListOfRows, DBHandle, TableName).

insert_list_for('Cars', [ 
		   [3, 'Skoda', 9000],
                   [4, 'Volvo', 29000],
                   [5, 'Bentley', 350000]]    ).
	
insert_list_for(members, [
                [99,  'Mark', 'Slotnik', 'mark.s@gmail.com', 90.00],
                [100, 'Jane', 'Mason', 'mason@greenday.com', 20.89],
                [101, 'Grace', 'Jones', 'grace@gjones.com', 351.87],
                [102, 'Bill', 'Brown', 'bb@roads.org', 0.00],
                [103, 'Sally', 'Ainsley', 'ainsley@connects.net', 14.03]   ]).

test_insert_rows(DBName, TableName)
	:-
	drop_db(DBName),
	sqlite3_open(DBName, DBHandle),
	setup_table(DBHandle, TableName).

test_create_insert_table(DBName, TableName)
	:-
	drop_db(DBName),
	sqlite3_open(DBName, DBHandle),
	setup_table(DBHandle, TableName).

	/* ------------------------------------------ *
			SELECTS
	     The following SELECT tests run 
		    setup_table/2
	     to create a table and populate it.
	 * ------------------------------------------ */

expected_select_one_list_for('Cars', [r(3,'Skoda',9000)]).
expected_select_one_list_for(members, [r(99,  'Mark', 'Slotnik', 'mark.s@gmail.com', 90.00)]).

expected_select_all_list_for('Cars', [r(3,'Skoda',9000),r(4,'Volvo',29000),r(5,'Bentley',350000)]).
expected_select_all_list_for(members, 
                [r(99,  'Mark', 'Slotnik', 'mark.s@gmail.com', 90.00),
                r(100, 'Jane', 'Mason', 'mason@greenday.com', 20.89),
                r(101, 'Grace', 'Jones', 'grace@gjones.com', 351.87),
                r(102, 'Bill', 'Brown', 'bb@roads.org', 0.00),
                r(103, 'Sally', 'Ainsley', 'ainsley@connects.net', 14.03)]).

test_select_one_table(DBName, TableName)
	:-
	drop_db(DBName),
	sqlite3_open(DBName, DBHandle),
	setup_table(DBHandle, TableName),
	select_one_table(DBHandle, TableName, Result),
	expected_select_one_list_for(TableName, Tgt),
	Result = Tgt.

test_select_one_empty_table(DBName)
	:-
	drop_db(DBName),
	sqlite3_open(DBName, DBHandle),
	TableName = 'Cars',
	table_spec('Cars', ColumnsList, Primary),
	sql_create_table(DBHandle, TableName, ColumnsList, Primary),
		%% No inserts, so Result = []:
	select_one_table(DBHandle, TableName, Result),
	Result = [].

test_select_all_table(DBName, TableName)
	:-
	drop_db(DBName),
	sqlite3_open(DBName, DBHandle),
	setup_table(DBHandle, TableName),
	select_all_table(DBHandle, TableName, Result),
	expected_select_all_list_for(TableName, Tgt),
	Result = Tgt.

test_select_all_table_2(DBName, TableName)
	:-
	drop_db(DBName),
	sqlite3_open(DBName, DBHandle),
	setup_table(DBHandle, TableName),
	sql_query_table_all_string(TableName, SelectString),
	Limit = all,
	sqlite3_exec_rows(DBHandle, SelectString, Limit,  Result),
	expected_select_all_list_for(TableName, Tgt),
	Result = Tgt.

	/* ------------------------------------------------------------------------------------- *
		SubSelects

	   Cars
                [column_name='Id', column_type='INT'],
                [column_name='Name', column_type='TEXT'],
                [column_name='Price', column_type='INT'] ],

	   members
		[column_name=contact_id, column_type='INTEGER'],
		[column_name=first_name, column_type='TEXT', constraints=['NOT NULL']],
		[column_name=last_name, column_type='TEXT', constraints=['NOT NULL']],
		[column_name=email, column_type='TEXT', constraints=['NOT NULL', 'UNIQUE']],
		[column_name=balance, column_type='REAL', constraints=['NOT NULL']] ],
	 * ------------------------------------------------------------------------------------- */


where_clauses_for('Cars', ['Name', 'Price'], ['Price > 10000']).

expected_select_all_where_for('Cars', [r('Volvo',29000),r('Bentley',350000)]).

where_clauses_for(members, [last_name, balance, email, contact_id], [ 'balance > 50', 'contact_id<101' ]).
%where_clauses_for(members, [last_name, balance, email, contact_id], [ 'contact_id<101', 'balance > 50' ]).
%where_clauses_for(members, [last_name, balance, email, contact_id], [ 'balance > 50' ]).

expected_select_all_where_for(members, [r('Slotnik',90.0,'mark.s@gmail.com',99)]).


test_select_all_where(DBName, TableName)
	:-
	drop_db(DBName),
	sqlite3_open(DBName, DBHandle),
	setup_table(DBHandle, TableName),
	where_clauses_for(TableName, ColStringsList, WhereClauseList),
	select_where_lists(DBHandle, TableName, ColStringsList, WhereClauseList, Result),
	expected_select_all_where_for(TableName, Tgt),
	Result = Tgt.


	/* ------------------------------------------------------------------------------------- *
		Updates

	   Cars
                [column_name='Id', column_type='INT'],
                [column_name='Name', column_type='TEXT'],
                [column_name='Price', column_type='INT'] ],
 		   3, Skoda, 9000,
                   4, Volvo, 29000,
                   5, Bentley, 350000] ).

	   members
		[column_name=contact_id, column_type='INTEGER'],
		[column_name=first_name, column_type='TEXT', constraints=['NOT NULL']],
		[column_name=last_name, column_type='TEXT', constraints=['NOT NULL']],
		[column_name=email, column_type='TEXT', constraints=['NOT NULL', 'UNIQUE']],
		[column_name=balance, column_type='REAL', constraints=['NOT NULL']] ],
                   99,  Mark, Slotnik, mark.s@gmail.com, 90.00,
                   100, Jane, Mason, mason@greenday.com, 20.89,
                   101, Grace, Jones, grace@gjones.com, 351.87,
                   102, Bill, Brown, bb@roads.org, 0.00,
                   103, Sally, Ainsley, ainsley@connects.net, 14.03]).

	 * ------------------------------------------------------------------------------------- */

update_comps_for('Cars', ['Price' = 32500], ['Id' = 4]).

update_comps_for(members, [email = '\'jm@holiday.org\'', balance = 37.88], [last_name= '\'Mason\'', first_name = '\'Jane\'']).

expected_update_result_for('Cars', [r(4,'Volvo',32500)]).

expected_update_result_for(members, [r(100,'Jane','Mason','jm@holiday.org',37.88)]).

test_update(DBName, TableName)
	:-
	drop_db(DBName),
	sqlite3_open(DBName, DBHandle),
	setup_table(DBHandle, TableName),
	update_comps_for(TableName, SetList, WhereClauseList),
	select_where_lists(DBHandle, TableName, ['*'], WhereClauseList, Result0),
	sql_update(DBHandle, TableName, SetList, WhereClauseList),
	select_where_lists(DBHandle, TableName, ['*'], WhereClauseList, Result1),
	expected_update_result_for(TableName, Tgt),
	write((Result0 -> Result1)),nl,
	Result1 = Tgt.
	

	/* ------------------------------------------------------------------------------------- *
			Delete rows
	 * ------------------------------------------------------------------------------------- */
delete_comps_for('Cars', ['Id' = 4]).
delete_comps_for(members, [last_name= '\'Mason\'', first_name = '\'Jane\'']).

	%expected_delete_results(TableName, Before, After).
expected_delete_results('Cars', [r(4,'Volvo',29000)], []).
expected_delete_results(members,  [r(100,'Jane','Mason','mason@greenday.com',20.89)], []).

test_delete(DBName, TableName)
	:-
	drop_db(DBName),
	sqlite3_open(DBName, DBHandle),
	setup_table(DBHandle, TableName),
	delete_comps_for(TableName, WhereClauseList),

	expected_delete_results(TableName, Before, After),
	select_where_lists(DBHandle, TableName, ['*'], WhereClauseList, Before),
	sql_delete(DBHandle, TableName, WhereClauseList),
	select_where_lists(DBHandle, TableName, ['*'], WhereClauseList, After),
	write((Before -> After)),nl.

	/* ------------------------------------------------------------------------------------- *
			Enabling foreign key constraint support
	 * ------------------------------------------------------------------------------------- */
expected_fk_results(members,  [0, 1, 0]).

test_foreign_key_constraint_manip(DBName)
	:-
	sqlite3_open(DBName, DBHandle), 
	expected_fk_results(members,  [Result1, Result2, Result3]),

	query_foreign_keys(DBHandle, Result1),
	set_foreign_keys_on(DBHandle),
	query_foreign_keys(DBHandle, Result2),
	set_foreign_keys_off(DBHandle),
	query_foreign_keys(DBHandle, Result3).
