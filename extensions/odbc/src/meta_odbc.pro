/* meta-odbc interface */


/*
example: 

sql_init.

sql_open_connection(C, 'sdk21-Access32', '', ''),

sql_open_statement(C, 'select * from customer', S),

sql_execute_statement(S),

sql_fetch_row(S, R),

sql_close_statement(S),

sql_close_connection(C).

sql_shutdown.

R = ['bob', '203 Main St.', ...]

*/

/*
init - sets a global for the enironment - books says it is global for each app.
*/

:- make_gv('_odbc_environment').

sql_init :-
	sql_alloc_env(Environment),
	set_odbc_environment(Environment).

sql_shutdown :-
	get_odbc_environment(Environment),
	sql_free_env(Environment),
	set_odbc_environment(0).

/* sql_alloc_connect/2 is already defined */

sql_open_connection(DataSource, User, Password, Connection) :-
	get_odbc_environment(Environment),
	sql_alloc_connect(Environment, Connection),
	catch ((
		sql_connect(Connection, DataSource, User, Password)
	), ConnectError, (
		sql_free_connect(Connection),
		throw(ConnectError)
	)).

sql_open_connection(ConString, OutString, Connection) :-
	get_odbc_environment(Environment),
	sql_alloc_connect(Environment, Connection),
	catch ((
		sql_driver_connect(Connection, 0, ConString, OutString, 512, 'SQL_DRIVER_NOPROMPT')
	), ConnectError, (
		sql_free_connect(Connection),
		throw(ConnectError)
	)).

sql_open_connection(Connection, ConnectionString) :-
	get_odbc_environment(Environment),
	sql_alloc_connect(Environment, Connection),
	catch ((
		o_GetForegroundWindow(X),
		sql_driver_connect(Connection, X, '', ConnectionString, 512, 'SQL_DRIVER_PROMPT')
	), ConnectError, (
		sql_free_connect(Connection),
		throw(ConnectError)
	)).

sql_close_connection(Connection) :-
	catch ((
		sql_disconnect(Connection)
	), DisconnectError, (
		sql_free_connect(DBConnection),
		throw(DisconnectError)
	)),
	sql_free_connect(Connection).

sql_commit(Connection) :-
	get_odbc_environment(Environment),
	sql_transact(Environment, Connection, 'SQL_COMMIT').
	
sql_open_statement(Connection, SQLQuery, StatementTerm) :-
	sql_alloc_stmt(Connection, StatementHandle),
	catch ((
		sql_prepare(StatementHandle, SQLQuery),
		sql_num_result_cols(StatementHandle, NumColumns),
		alloc_col_info(StatementHandle, 1, NumColumns, ColumnInfoList),
		sql_num_params(StatementHandle, NumParams),
		alloc_param_info(StatementHandle, 1, NumParams, ParamInfoList),
		StatementTerm = statement(StatementHandle, ColumnInfoList, ParamInfoList)
	), StatementError, (
		sql_free_stmt(StatementHandle, 'SQL_DROP'),
		throw(StatementError)
	)).

length_from_percision(Percision, Length) :-
	Percision > 0,
	Length is Percision + 1.
length_from_percision(Percision, 256).

alloc_col_info(Statement, N, MaxCols, []) :- N > MaxCols.
alloc_col_info(Statement, N, MaxCols, [Col | RestData]) :-
	sql_describe_col(Statement, N, Name, 255, ODBCType, Percision, _, _),
	length_from_percision(Percision, Length),
	simple_ctype(ODBCType, CType),
	alloc_datum(CType, Datum, Length),
	c_alloc_abs(int, Len),
	sql_bind_col(Statement, N, CType, Datum, Length, Len),
	Col = col_info(Name, CType, Datum, Len),
	NextN is N + 1,
	alloc_col_info(Statement, NextN, MaxCols, RestData).

alloc_param_info(Statement, N, MaxParams, []) :- N > MaxParams.
alloc_param_info(Statement, N, MaxParams, [Param | RestData]) :-
	sql_describe_param(Statement, N, Name, 255, ODBCType, Percision, Scale, _),
	Length is Percision + 1,
	simple_ctype(ODBCType, CType),
	alloc_datum(CType, Datum, Length),
	c_alloc_abs(int, Len),
	ctype_sql_type(CType, SQLType),
	sql_bind_parameters(Statement, N, 'SQL_PARAM_INPUT_OUTPUT', CType, SQLType, Percision, Scale, Datum, Length, Len),
	Param = param_info(Name, CType, Datum, Len),
	NextN is N + 1,
	alloc_param_info(Statement, NextN, MaxParams, RestData).

simple_ctype('SQL_BIGINT', 'SQL_C_DOUBLE').
simple_ctype('SQL_BIT', 'SQL_C_DOUBLE').
simple_ctype('SQL_DECIMAL', 'SQL_C_DOUBLE').
simple_ctype('SQL_DOUBLE', 'SQL_C_DOUBLE').
simple_ctype('SQL_FLOAT', 'SQL_C_DOUBLE').
simple_ctype('SQL_INTEGER', 'SQL_C_DOUBLE').
simple_ctype('SQL_NUMERIC', 'SQL_C_DOUBLE').
simple_ctype('SQL_REAL', 'SQL_C_DOUBLE').
simple_ctype('SQL_SMALLINT', 'SQL_C_DOUBLE').
simple_ctype('SQL_VARCHAR', 'SQL_C_CHAR').
simple_ctype('SQL_LONGVARCHAR', 'SQL_C_CHAR').
simple_ctype('SQL_CHAR', 'SQL_C_CHAR').
simple_ctype('SQL_DATE', 'SQL_C_DATE').
simple_ctype('SQL_TIME', 'SQL_C_TIME').
simple_ctype('SQL_TIMESTAMP', 'SQL_C_TIMESTAMP').

ctype_sql_type('SQL_C_DOUBLE', 'SQL_DOUBLE').
ctype_sql_type('SQL_C_CHAR', 'SQL_CHAR').
ctype_sql_type('SQL_C_DATE', 'SQL_DATE').
ctype_sql_type('SQL_C_TIME', 'SQL_TIME').
ctype_sql_type('SQL_C_TIMESTAMP', 'SQL_TIMESTAMP').

alloc_datum('SQL_C_CHAR', Datum, Length) :-
	c_allocn_abs(char, Length, Datum).
alloc_datum('SQL_C_DOUBLE', Datum, _) :-
	c_alloc_abs(double, Datum).
alloc_datum('SQL_C_DATE', Datum, _) :-
	c_alloc_abs('DATE_STRUCT', Datum).
alloc_datum('SQL_C_TIME', Datum, _) :-
	c_alloc_abs('TIME_STRUCT', Datum).
alloc_datum('SQL_C_TIMESTAMP', Datum, _) :-
	c_alloc_abs('TIMESTAMP_STRUCT', Datum).


sql_close_statement(statement(StatementHandle, ColumnInfoList, ParamInfoList)) :-
		catch ((
			sql_free_stmt(StatementHandle, 'SQL_DROP')
		), FreeError, (
			free_col_info(ColumnInfoList),
			free_param_info(ParamInfoList),
			throw(FreeError)
		)),
		free_col_info(ColumnInfoList),
		free_param_info(ParamInfoList).

free_col_info([]).
free_col_info([col_info(_, _, Datum, Len) | Rest]) :-
	c_free(Datum), c_free(Len),
	free_col_info(Rest).

free_param_info([]).
free_param_info([param_info(_, _, Datum, Len) | Rest]) :-
	c_free(Datum), c_free(Len),
	free_param_info(Rest).
	
sql_execute_statement(statement(StatementHandle, _, _)) :-
	sql_execute(StatementHandle).

sql_bind_row(statement(StatementHandle, ColumnInfoList, ParamInfoList), Row) :-
	bind_item(Row, ColumnInfoList).

bind_item(_, []).
bind_item([Item | ItemTail], [col_info(_, Type, Datum, _) | InfoTail]) :-
	put_data(Type, Item, Datum),
	bind_item(ItemTail, InfoTail).

put_data('SQL_C_CHAR', PrologData, Datum) :-
	c_set(Datum, str, PrologData).
put_data('SQL_C_DOUBLE', PrologData, Datum) :-
	c_set(Datum, double, PrologData).
put_data('SQL_C_DATE', date(Year, Month, Day), Datum) :-
	c_set(Datum, 'DATE_STRUCT', [year, Year, month, Month, day, Day]).
put_data('SQL_C_TIME', time(Hour, Minute, Second), Datum) :-
	c_set(Datum, 'TIME_STRUCT', [hour, Hour, minute, Minute, second, Second]).
put_data('SQL_C_TIMESTAMP', timestamp(Year, Month, Day, Hour, Minute, Second, Fraction), Datum) :-
	c_set(Datum, 'TIMESTAMP_STRUCT',
		[year, Year, month, Month, day, Day,
	    hour, Hour, minute, Minute, second, Second, fraction, Fraction]).

sql_fetch_row(statement(StatementHandle, [], _), Row) :- fail.
sql_fetch_row(statement(StatementHandle, ColumnInfoList, _), Row) :-
	sql_fetch(StatementHandle),
	collect_results(ColumnInfoList, Row).

collect_results([], []).
collect_results([col_info(_, Type, Datum, _) | ColRest], [Result | ResultRest]) :-
	get_data(Type, Datum, Result),
	collect_results(ColRest, ResultRest).
	
get_data('SQL_C_CHAR', Datum, PrologData) :-
	!,
	c_examine(Datum, str, PrologData).
get_data('SQL_C_DOUBLE', Datum, PrologData) :-
	!,
	c_examine(Datum, double, PrologData).
get_data('SQL_C_DATE', Datum, PrologData) :-
	!,
	c_examine(Datum, 'DATE_STRUCT', [year, Year, month, Month, day, Day]),
	PrologData = date(Year, Month, Day).
get_data('SQL_C_TIME', Datum, PrlogData) :-
	!,
	c_examine(Datum, 'TIME_STRUCT', [hour, Hour, minute, Minute, second, Second]),
	PrologData = time(Hour, Minute, Second).
get_data('SQL_C_TIMESTAMP', Datum, PrologData) :-
	!,
	c_examine(Datum, 'TIMESTAMP_STRUCT',
	   [year, Year, month, Month, day, Day,
	    hour, Hour, minute, Minute, second, Second, fraction, Fraction]),
	PrologData = timestamp(Year, Month, Day, Hour, Minute, Second, Fraction).
