/* ==============================================================*
 |				odbc_samples.pro
 |		Copyright (c) 1998-99 Applied Logic Systems, Inc.
 |
 *-------------------------------------------------------------*/


cpi_row_view(Row) :-
	sql_open_connection('Economics', '', '', C),
	printf('Connected to: %s\n', ['Economics']),
 	sql_open_statement(C, 'select "Expenditure Category","RelImpDec-99","UadjPctChg3-98to3-99","SeasAdjPctJan-Feb" from cpi', S),
 	sql_execute_statement(S),
	get_a_row(S, Row).


cpi_view(Cat,RelImp,UPCMarch99,SeasPctFeb) :-
	sql_open_connection('Economics', '', '', C),
 	sql_open_statement(C, 
		'select "Expenditure Category","RelImpDec-99","UadjPctChg3-98to3-99","SeasAdjPctJan-Feb" from cpi', S),
 	sql_execute_statement(S),
	get_a_row(S, [Cat,RelImp,UPCMarch99,SeasPctFeb]).

module zap.
use odbc.

:-
	Dfn = [
		ds = 'Economics',
		table = 'cpi',
		cols_list = [
			'Expenditure category','RelImpDec-99',
			'UadjPctChg3-98to3-99','SeasAdjPctJan-Feb'],
		pred=my_cpi
	],
	defSQLview(Dfn).

endmod.

do_insert1(Text,Num) :-
	sql_open_connection('Economics', '', '', C),
	printf('Connected to: %s\n', ['Economics']),
	sprintf(atom(SQLCmd), 
			'INSERT INTO datatable1(text1,value1) VALUES (\'%t\', %t)', 
			[Text, Num] ),
	sql_open_statement(C, SQLCmd, S),
 	sql_execute_statement(S).

do_retrieve1(Rows) :-
	sql_open_connection('Economics', '', '', C),
	sql_open_statement(C, 'SELECT ALL * FROM datatable1', S),
 	sql_execute_statement(S),
	sql_fetch_all_rows(S, Rows).

do_retrieve1a(X,Y) :-
	sql_open_connection('Economics', '', '', C),
	sql_open_statement(C, 'SELECT ALL text1,value1 FROM datatable1', S),
 	sql_execute_statement(S),
	!,
	get_a_row(S, [X,Y]).

:-
	Dfn = [
		ds = 'Economics',
		table = 'datatable1',
		cols_list = [text1, value1],
		pred=my_view1
	],
	defSQLview(Dfn).


