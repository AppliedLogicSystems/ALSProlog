/* prolog glue level for odbc */

/*
sql_alloc_env/1

Description

sql_alloc_env(Environment) is true.

Procedurally, sql_alloc_env(Environment) is executed as follows:

a) Allocates an environment and initializes the ODBC call level interface.

b) Instatiates Environment with the environment-term which is to be associated
with this environment.

c) The goal succeeds.
 
Template and modes

open(-Environment)

Errors

a) Environment is not a variable
- type_error(variable, Environment).

b) SQL error
- sql_error(...)

*/

/*
error strategy:

Assumptions

- ISO Standard Prolog calls work as defined in the standard (ie they produce all the
  expected errors).  For example, I'm assuming that atom_length(X, Y) is always true if
  Y is a variable.
  
- ALS functions work in a reasonable way (ie they produce errors in the spirit of the
  standard).  For example, c_alloc is always true and c_examine(X, ptr, Y) is
  always true if Y is a variable.
  
- C-level predicates are always true if return values are variables.  For example
  c_SQLAllocEnv(X, Y) is always true if Y is a variable.


These assumptions allow me to write staightforward code, with only a little bit
of error checking for required variable arguments, and return codes.

*/

/*

Things I'd like to see changed in the Prolog-C interface.

- It would be nice to have pointers to 8/16/32-bit values (chars, ints, pointer)
  handled directly by the low level stuff via unbound variables. For example:
  
  void get_num(int *num);
  
  could be called with get_num(X), where X is a unbound variable.  X would be
  bound to the resulting integer.
  
- Prolog level binding to shared libraries and DLLs

*/

/* Connecting to a Data Source */

sql_alloc_env(Environment) :-
	check_var(Environment),
	c_alloc(ptr, EnvrHandle),
	o_SQLAllocEnv(EnvrHandle, Result),
	check_sql_result(Result, environment, Environment),
	c_examine(EnvrHandle, ptr, Environment).

sql_alloc_connect(Environment, Connection) :-
	check_var(Connection),
	c_alloc(ptr, ConHandle),
	o_SQLAllocConnect(Environment, ConHandle, Result),
	check_sql_result(Result, connection, Connection),
	c_examine(ConHandle, ptr, Connection).

sql_connect(Connection, DataSourceName, UserID, AuthenticationString) :-
	atom_length(DataSourceName, DataSourceNameLength),
	atom_length(UserID, UserIDLength),
	atom_length(AuthenticationString, AuthStringLength),
	o_SQLConnect(Connection,
		     DataSourceName, DataSourceNameLength,
		     UserID, UserIDLength,
		     AuthenticationString, AuthStringLength,
		     Result),
	check_sql_result(Result, connection, Connection).

sql_driver_connect(Connection, Window, ConnectionString,
		   ConnectionStringOutput, OutputLengthMax, DriverCompletion) :-
	atom_length(ConnectionString, ConStringLength),
	c_allocn(char, OutputLengthMax, ConnectionStringOutput),
	c_alloc(short, X),
	must_c_const(DriverCompletion, DriverCompletionNumber),
	o_SQLDriverConnect(Connection, Window,
			      ConnectionString, ConStringLength,
			      ConnectionStringOutput, OutputLengthMax, X,
			      DriverCompletionNumber, Result),
	check_sql_result(Result, connection, Connection),
	c_examine(X, short, ConStringOutputLength),
	'$uia_clip'(ConnectionStringOutput, ConStringOutputLength).

sql_browse_connect(Connection, ConnectionString,
		   ConnectionStringOutput, OutputLengthMax) :-
	atom_length(ConnectionString, ConStringLength),
	c_allocn(char, OutputLengthMax, ConnectionStringOutput),
	c_alloc(short, ConStringOutLengthPtr),
	o_SQLBrowseConnect(Connection,
			      ConnectionString, ConStringLength,
			      ConnectionStringOutput, OutputLengthMax, ConStringOutLengthPtr,
			      Result),
	check_sql_result(Result, connection, Connection),
	c_examine(ConStringOutLenPtr, short, ConStringOutputLength),
	'$uia_clip'(ConnectionStringOutput, ConStringOutputLength).

/* Obtaining Information about a Driver and Data Source */

sql_data_sources(Environment, Direction, DataSourceName, DataSourceNameMax,
		 Description, DescriptionMax) :-
	must_c_const(Direction, DirectionNumber),
	c_allocn(char, DataSourceNameMax, DataSourceName),
	c_alloc(short, DataSourceLenPtr),
	c_allocn(char, DescriptionMax, Description),
	c_alloc(short, DescriptionLenPtr),
	o_SQLDataSource(Environment,  DirectionNumber,
			DataSourceName, DataSourceNameMax, DataSourceLenPtr,
			Description, DescriptionMax, DescriptionLenPtr, Result),
	check_sql_result(Result, environment, Environment),
	c_examine(DataSourceLenPtr, short, DataSourceLength),
	'$uia_clip'(DataSourceName, DataSourceLength),
	c_examine(DescriptionLenPtr, short, DescriptionLength),
	'$uia_clip'(Description, DescriptionLength).

sql_drivers(Environment, Direction, DriverDesc, DriverDescMax,
		 DriverAttrib, DriverAttribMax) :-
	must_c_const(Direction, DirectionNumber),
	c_allocn(char, DriverDescMax, DriverDesc),
	c_alloc(short, DriverDescLenPtr),
	c_allocn(char, DriverAttribMax, DriverAttrib),
	c_alloc(short, DriverAttribLenPtr),
	o_SQLDataSource(Environment,  DirectionNumber,
			DriverDesc, DriverDescMax, DriverDescLenPtr,
			DriverAttrib, DriverAttribMax, DriverAttribLenPtr, Result),
	check_sql_result(Result, environment, Environment),
	c_examine(DriverDescLenPtr, short, DriverDescLength),
	'$uia_clip'(DriverDesc, DriverDescLength),
	c_examine(DriverAttribLenPtr, short, DriverAttribLength),
	'$uia_clip'(DriverAttrib, DriverAttribLength).

sql_get_info(Connection, InfoType, InfoValue, InfoMax) :-
	must_c_const(InfoType, InfoTypeNumber),
	c_allocn(char, InfoMax, InfoValue),
	c_alloc(short, InfoLenPtr),
	o_SQLGetInfo(Connection, InfoTypeNumber, InfoValue, InfoMax, InfoLenPtr, Result),
	check_sql_result(Result, connection, Connection),
	c_examine(InfoLenPtr, short, InfoLength),
	'$uia_clip'(InfoValue, InfoLength).
	
/* sql_get_functions, should probably be made more prolog-y.  have one function to
   test the existence of a single function, and another to get the full list of
   functions. */
sql_get_functions(Connection, Function, ExistsPtr) :-
	must_c_const(Function, FuncNumber),
	c_alloc(ptr, ExistsPtr),
	o_SQLGetFunction(Connection, FuncNumber, ExistsPtr, Result),
	check_sql_result(Result, connection, Connection).

sql_get_type_info(Statement, Type) :-
	must_c_const(Type, TypeNumber),
	o_SQLGetTypeInfo(Statement, TypeNumber, Result),
	check_sql_result(Result, statement, Statement).

/* Setting and Retrieving Driver Options */

/* sql_set_connection_option should be smarter - take either an int or atom as Param and
   do the right thing with it. */
sql_set_connect_option(Connection, Option, Param) :-
	must_c_const(Option, OptionNumber),
	o_SQLSetConnectOption(Connection, OptionNumber, Param, Result),
	check_sql_result(Result, connection, Connection).

/* Ditto as above */
sql_get_connect_option(Connection, Option, ParamPtr) :-
	must_c_const(Option, OptionNumber),
	c_alloc(ptr, ParamPtr),
	o_SQLGetConnectOption(Connection, OptionNumber, ParamPtr, Result),
	check_sql_result(Result, connection, Connection).
	
sql_set_stmt_option(Statement, Option, Param) :-
	must_c_const(Option, OptionNumber),
	o_SQLSetStmttOption(Statement, OptionNumber, Param, Result),
	check_sql_result(Result, statement, Statement).

sql_get_stmt_option(Statement, Option, ParamPtr) :-
	must_c_const(Option, OptionNumber),
	c_alloc(ptr, ParamPtr),
	o_SQLGetStmtOption(Statement, OptionNumber, ParamPtr, Result),
	check_sql_result(Result, statement, Statement).


/* Preparing SQL Requests */

sql_alloc_stmt(Connection, Statement) :-
	c_alloc(ptr, StmtHandle),
	o_SQLAllocStmt(Connection, StmtHandle, Result),
	check_sql_result(Result, connection, Connection),
	c_examine(StmtHandle, ptr, Statement).

sql_prepare(Statement, String) :-
	must_c_const('SQL_NTS', SQL_NTS),
	o_SQLPrepare(Statement, String, SQL_NTS, Result),
	check_sql_result(Result, statement, Statement).
	
sql_bind_parameters(Statement, ParamNumber, ParamType, CType, SQLType,
		    ColDef, Scale, Value, ValueMax, ValueLenPtr) :-
	must_c_const(ParamType, ParamTypeNumber),
	must_c_const(CType, CTypeNumber),
	must_c_const(SQLType, SQLTypeNumber),
	o_SQLBindParameters(Statement, ParamNumber, ParamType, CType, SQLType,
		    	    ColDef, Scale, Value, ValueMax, ValueLenPtr, Result),
	check_sql_result(Result, statement, Statement).

sql_param_options(Statement, CRow, RowNumberPtr) :-
	o_SQLParamOptions(Statement, CRow, RowNumberPtr, Result),
	check_sql_result(Result, statement, Statement).

sql_get_cursor_name(Statement, CursorName, CursorMax) :-
	c_allocn(char, CursorName),
	c_alloc(long, CursorNameLenPtr),
	o_SQLGetCursorName(Statement, CursorName, CursorMax, CursorNameLenPtr, Result),
	check_sql_result(Result, statement, Statement),
	c_examine(CursorNameLenPtr, short, CursorNameLength),
	'$uia_clip'(CursorName, CursorNameLength).
	
sql_set_cursor_name(Statement, CursorName) :-
	must_c_const('SQL_NTS', SQL_NTS),
	o_SQLSetCursorName(Statement, CursorName, SQL_NTS, Result),
	check_sql_result(Result, statement, Statement).
	
/* something has to be done about the fact that KeysetCount can be a symbolic
   constant or an actual count. */
sql_set_scroll_options(Statement, Concurrency, KeysetCount, RowsetCount) :-
	must_c_const(Concurrency, ConcurrencyNumber),
	o_SQLSetScrollOptions(Statement, ConcurrencyNumber, KeysetCount, RowsetCount, Result),
	check_sql_result(Result, statement, Statement).

/* Submitting Requests */

sql_execute(Statement) :-
	o_SQLExecute(Statement, Result),
	check_sql_result(Result, statement, Statement).

sql_exec_direct(Statement, Query) :-
	atom_length(Query, QueryLen),
	o_SQLExecDirect(Statement, Query, QueryLen, Result),
	check_sql_result(Result, statement, Statement).

sql_native_sql(Connection, SQLStrIn, SQLStrOut, SQLStrMax) :-
	c_allocn(char, SQLStrMax, SQLStrOut),
	c_alloc(long, SQLStrOutLenPtr),
	o_SQLNativeSql(Connection, SQLStrIn, SQL_NTS, SQLStrOut, SQLStrMax, SQLStrOutLenPtr, Result),	
	check_sql_result(Result, connection, Connection),
	c_examine(SQLStrOutLenPtr, short, SQLStrOutLength),
	'$uia_clip'(SQLStrOut, SQLStrOutLength).
	
sql_describe_param(Statement, ParamMarkerNumber, SQLType, ColDef, Scale, Nullable) :-
	c_alloc(short, SQLTypePtr),
	c_alloc(short, ScalePtr),
	c_alloc(short, NullablePtr),
	o_SQLDescribeParam(Statement, ParamMarkerNumber, SQLTypePtr, ColDef,
			   ScalePtr, NullablePtr, Result),
	check_sql_result(Result, statement, Statement),
	c_examine(SQLTypePtr, short, SQLTyleNumber),
	sql_type(SQLType, SQLTyleNumber),
	c_examine(ScalePtr, short, Scale),
	c_examine(NullablePtr, short, NullableNumber),
	nullable_type(Nullable, NullableNumber).
	
sql_num_params(Statement, ParamCount) :-
	c_alloc(short, ParamCountPtr),
	o_SQLNumParams(Statement, ParamCountPtr, Result),
	check_sql_result(Result, statement, Statement),
	c_examine(ParamCountPtr, short, ParamCount).

sql_param_data(Statement, ValuePtr) :-
	o_SQLParamData(Statement, ValuePtr, Result),
	check_sql_result(Result, statement, Statement).
	
sql_put_data(Statement, Value) :-
	atom_length(Value, ValueLength),
	o_SQLPutData(Statement, Value, ValueLength, Result),
	check_sql_result(Result, statement, Statement).

/* Retrieving Results and Information about Results */

sql_row_count(Statement, RowCount) :-
	c_alloc(long, RowCountPtr),
	o_SQLRowCount(Statement, RowCountPtr, Result),
	check_sql_result(Result, statement, Statement),
	c_examine(RowCountPtr, long, RowCount).

sql_num_result_cols(Statement, NumResultCols) :-
	c_alloc(short, NumResultColsPtr),
	o_SQLNumResultCols(Statement, NumResultColsPtr, Result),
	check_sql_result(Result, statement, Statement),
	c_examine(NumResultColsPtr, short, NumResultCols).


sql_type('SQL_CHAR', 1).
sql_type('SQL_NUMERIC', 2).
sql_type('SQL_DECIMAL', 3).
sql_type('SQL_INTEGER', 4).
sql_type('SQL_SMALLINT', 5).
sql_type('SQL_FLOAT', 6).
sql_type('SQL_REAL', 7).
sql_type('SQL_DOUBLE', 8).
sql_type('SQL_VARCHAR', 12).

sql_type('SQL_DATE', 9).
sql_type('SQL_TIME', 10).
sql_type('SQL_TIMESTAMP', 11).
sql_type('SQL_LONGVARCHAR', -1).
sql_type('SQL_BINARY', -2).
sql_type('SQL_VARBINARY', -3).
sql_type('SQL_LONGVARBINARY', -4).
sql_type('SQL_BIGINT', -5).
sql_type('SQL_TINYINT', -6).
sql_type('SQL_BIT', -7).
sql_type(X,Y) :- throw_error(domain_error(sql_constant, [X,Y])).
	
nullable_type('SQL_NO_NULLS', 0).
nullable_type('SQL_NULLABLE', 1).
nullable_type('SQL_NULLABLE_UNKNOWN', 2).
nullable_type(X,Y) :- throw_error(domain_error(sql_constant, [X,Y])).

sql_describe_col(Statement, ColNum, ColName, NameMax, SQLType, Percision, Scale, Nullable) :-
	c_allocn(char, NameMax, ColName),
	c_alloc(short, NameLenPtr),
	c_alloc(short, SQLTypePtr),
	c_alloc(int, PercisionPtr),
	c_alloc(short, ScalePtr),
	c_alloc(short, NullablePtr),
	o_SQLDescribeCol(Statement, ColNum, ColName, NameMax,
		NameLenPtr, SQLTypePtr, PercisionPtr, ScalePtr, NullablePtr, Result),
	check_sql_result(Result, statement, Statement),
	c_examine(NameLenPtr, short, NameLength),
	c_examine(SQLTypePtr, short, SQLTyleNumber),
	sql_type(SQLType, SQLTyleNumber),
	c_examine(PercisionPtr, int, Percision),
	c_examine(ScalePtr, short, Scale),
	c_examine(NullablePtr, short, NullableNumber),
	nullable_type(Nullable, NullableNumber),
	'$uia_clip'(ColName, NameLength).

sql_col_attributes(Statement, Col, DescType, Desciption, DescMax, DescNumber) :-
	must_c_const(DescType, DescTypeNumber),
	c_allocn(char, DescMax, Description),
	c_alloc(short, DescLenPtr),
	c_alloc(int, DescNumPtr),
	o_SQLColAttributes(Statement, Col, DescTypeNumber, Desciption, DescMax, DescLenPtr,
			   DescNumPtr, Result),
	check_sql_result(Result, statement, Statement),
	c_examine(DescLenPtr, short, DescLength),
	'$uia_clip'(Desciption, DescLength),
	c_examine(DescNumPtr, int, DescNumber).

sql_bind_col(Statement, ColNum, Type, ValuePtr, ValueMax, ValueLenPtr) :-
	must_c_const(Type, TypeNum), 
	o_SQLBindCol(Statement, ColNum, TypeNum, ValuePtr, ValueMax, ValueLenPtr, Result),
	check_sql_result(Result, statement, Statement).

sql_fetch(Statement) :-
	must_c_const('SQL_NO_DATA_FOUND', NoDataFoundResult),
	o_SQLFetch(Statement, Result),
	(
		Result = NoDataFoundResult -> fail
	;
		check_sql_result(Result, statement, Statement)
	).
	
sql_extended_fetch(Statement, FetchType, NumRows, RowsFetched, StatusList) :-
	SQLExtendedFetch.
sql_get_data :- SQLGetData.
sql_set_pos :- SQLSetPos.
sql_more_results :- SQLMoreResults.
sql_error :- SQLError.

/* Obtaining Information about the Data Source's System Tables (Catalog Functions) */

sql_colum_privileges :- SQLColumnPrivileges.
sql_columns :- SQLColumns.
sql_foreign_keys :- SQLForeignKeys.
sql_primary_keys :- SQLPrimaryKeys.
sql_procedure_columns :- SQLProcedureColumns.
sql_procedures :- SQLProcedures.
sql_special_columns :- SQLSpecialColumns.
sql_statistics :- SQLStatistics.
sql_table_privileges :- SQLTablePrivileges.
sql_tables :- SQLTables.


/* Terminating a Statement */

sql_free_stmt(Statement, Option) :-
	must_c_const(Option, OptionNumber),
	o_SQLFreeStmt(Statement, OptionNumber, Result),
	check_sql_result(Result, statement, Statement).
	
sql_cancel(Statement) :-
	o_SQLCancel(Statement, Result),
	check_sql_result(Result, statement, Statement).
	
sql_transact(Environment, Connection, Type) :- 
	must_c_const(Type, TypeNumber),
	o_SQLTransact(Environment, Connection, TypeNumber, Result),
	check_sql_result(Result, connection, Connection).

/* Terminating a Connection */

sql_disconnect(Connection) :-
	o_SQLDisconnect(Connection, Result),
	check_sql_result(Result, connection, Connection).

sql_free_connect(Connection) :-
	o_SQLFreeConnect(Connection, Result),
	check_sql_result(Result, connection, Connection).
	
sql_free_env(Environment) :-
	o_SQLFreeEnv(Environment, Result),
	check_sql_result(Result, environment, Environment).

/* Utility functions. */

must_c_const(X, Y) :- c_const(X, Y), !.
must_c_const(X, Y) :- throw_error(domain_error(sql_constant, X)).

check_var(X) :- var(X), !.
check_var(X) :- throw_error(type_error(variable, X)).

check_sql_result(Result, _, _) :-
	c_const('SQL_SUCCESS', Result), !.
check_sql_result(Result, Type, Object) :-
	collect_errors(Type, Object, Errors),
	throw_error(sql_error(Result, Errors)).

error_params(environment, E, E, C, S) :-
	must_c_const('SQL_NULL_HDBC', C),
	must_c_const('SQL_NULL_HSTMT', S).
error_params(connection, C, 0, C, S) :-
	must_c_const('SQL_NULL_HSTMT', S).
error_params(statement, S, 0, 0, S).

collect_errors(Type, Object, [error(State, NativeError, ErrorMessage) | Rest]) :-
	error_params(Type, Object, E, C, S),
	c_allocn(char, 6, State),	
	c_alloc(float, NativeErrorPtr),
	c_allocn(char, 512, ErrorMessage),
	c_alloc(short, ErrorMsgLenPtr),
	o_SQLError(E, C, S, State, NativeErrorPtr,
		ErrorMessage, 512, ErrorMsgLenPtr, Result),
	c_const('SQL_SUCCESS', Result),
	c_examine(NativeErrorPtr, float, NativeError),
	c_examine(ErrorMsgLenPtr, short, ErrorMsgLen),
	'$uia_clip'(ErrorMessage, ErrorMsgLen),
	collect_errors(Type, Object, Rest).
collect_errors(_, _, []).

throw_error(Error) :- throw(error(Error, [])).


