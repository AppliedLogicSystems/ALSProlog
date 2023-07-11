:-['sqlite3_intf.pro'].

ex_1(CreateTableString)
	:- 
	TableName = 'Cars', Primary = 'Id',
        ColumnsList = [
                [column_name='Id', column_type='INT'],
                [column_name='Name', column_type='TEXT'],
                [column_name='Price', column_type='INT'] ],
    sql_create_table_string(TableName, ColumnsList, Primary, CreateTableString).

ex_2 :-
        DBAccess = 'my_sqlite3.db',
        TableName = 'Cars', Primary = 'Id',
        ColumnsList = [
                [column_name='Id', column_type='INT'],
                [column_name='Name', column_type='TEXT'],
                [column_name='Price', column_type='INT'] ],
    sql_create_table(DBAccess, TableName, ColumnsList, Primary).

ex_3 :-
        sqlite3_open('my_sqlite3.db', DBHandle),
        TableName = 'Cars', Primary = 'Id',
        ColumnsList = [
                [column_name='Id', column_type='INT'],
                [column_name='Name', column_type='TEXT'],
                [column_name='Price', column_type='INT'] ],
    sql_create_table(DBHandle, TableName, ColumnsList, Primary).

ex_4 :-
	sqlite3_exec_norows('my_sqlite3.db', 'INSERT INTO Cars VALUES(3, \'Skoda\', 9000);' ).

ex_5 :-
	insert_one_row('my_sqlite3.db', 'Cars',  [ 3, 'Skoda', 9000 ] ).

ex_6 :- ListOfRows =
	   [	[ 3, 'Skoda', 9000],
                [ 4, 'Volvo', 29000],
                [ 5, 'Bentley', 350000]   ],
	insert_rows(ListOfRows, 'my_sqlite3.db', 'Cars').

ex_7(R) :-
	select_one_table('my_sqlite3.db', 'Cars', R).

ex_8(R) :-
	ColStringsList = ['Price', 'Name'],
	WhereClauseList = ['Price > 10000'],
	select_where_lists('my_sqlite3.db', 'Cars', ColStringsList, WhereClauseList, R).

ex_9 :-   DBAccess = 'my_sqlite3.db',
	TableName = 'Cars',
	WhereClauseList = ['Id = 4'],
	select_where_lists(DBAccess, TableName, ['*'], WhereClauseList, R1),
	sql_update(DBAccess, TableName, ['Price = 31500'], WhereClauseList),
	select_where_lists(DBAccess, TableName, ['*'], WhereClauseList, R2),
	write((R1 -> R2)), nl.

ex_10 :-   DBAccess = 'my_sqlite3.db',
	TableName = 'Cars',
	WhereClauseList = ['Id = 4'],
	sql_delete(DBAccess, TableName, WhereClauseList).

ex_11 :-   DBAccess = 'my_sqlite3.db',
	TableName = 'Cars',
	sql_clear_table(DBAccess, TableName).

ex_12 :-   DBAccess = 'my_sqlite3.db',
	TableName = 'Cars',
	ColName = 'Name', 
	IndexName = cars_name,
	sql_create_index(ColName, TableName, IndexName, DBAccess).

ex_13 :-   DBAccess = 'my_sqlite3.db',
	TableName = 'Cars',
	Primary = 'Id',
	ColName = 'Price', 
	Modifier = unique,
	IndexName = cars_name,
	WhereClauseList = ['Price > 10000'],
	sql_create_index(ColName, Modifier, TableName, IndexName, Primary,  WhereClauseList, DBAccess).

%   [column_name='Price', column_type='INT'] ],

ex_14 :-   DBAccess = 'my_sqlite3.db',
	IndexName = cars_name,
	sql_drop_index(IndexName, DBAccess).
	
