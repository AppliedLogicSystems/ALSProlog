/* ==============================================================*
 |					meta_odbc.pro
 |		Copyright (c) 1998-99 Applied Logic Systems, Inc.
 |
 |					Meta-ODBC Interface 
 |
 |	Author: Chuck Houpt
 |			Contributions from Ken Bowen
 |
 |	Schematic example: 
 |	
 |	sql_init.
 |	sql_open_connection('sdk21-Access32', '', '', C),
 |	sql_open_statement(C, 'select * from customer', S),
 |	sql_execute_statement(S),
 |	sql_fetch_row(S, R),
 |	sql_close_statement(S),
 |	sql_close_connection(C).
 |	sql_shutdown.
 |	R = ['bob', '203 Main St.', ...]
 *-------------------------------------------------------------*/

module odbc.

/*!--------------------------------------------------------------*
 |	sql_init/0
 |	sql_init
 |	sql_init
 |
 |	- set a global environment for ODBC 
 |
 |	Defn: sql_init :- sql_init(_).
 *--------------------------------------------------------------*/
/*!--------------------------------------------------------------*
 |	sql_init/1
 |	sql_init(Env)
 |	sql_init(-)
 |
 |	- set and return a global environment for ODBC 
 |
 |	Books say this is global for each app;
 |	Calls sql_alloc_env(Environment) to set this global, obtains
 |	a handle Environment to it, and stores this in the prolog
 |	global variable "_odbc_environment":
 |
 |			set_odbc_environment(Environment)
 |			get_odbc_environment(Environment)
 |
 |	If there is an existing non-zero value for Environment in
 |
 |			get_odbc_environment(Environment),
 |
 |	does nothing.
 *--------------------------------------------------------------*/

:- 	clause(set_odbc_environment(_),true), ! ; 
		make_gv('_odbc_environment').

export sql_init/0.
export sql_init/1.
sql_init 
	:-
	sql_init(_).

sql_init(Environment) 
	:-
	get_odbc_environment(Environment),
	Environment \= 0,
	!.
sql_init(Environment) 
	:-
	sql_alloc_env(Environment),
	set_odbc_environment(Environment).

/*!--------------------------------------------------------------*
 |	sql_shutdown/0
 |	sql_shutdown
 |	sql_shutdown
 |	
 |	deallocate an odbc global environment
 *--------------------------------------------------------------*/
export sql_shutdown/0.
sql_shutdown 
	:-
	get_odbc_environment(Environment),
	sql_free_env(Environment),
	set_odbc_environment(0).

/*!--------------------------------------------------------------*
 |	sql_open_connection/4
 |	sql_open_connection(DataSource, User, Password, Connection)
 |	sql_open_connection(+, +, +, -)
 |
 |	- open a connection to a data source
 |
 |	Note that: sql_alloc_connect/2 is already defined
 *--------------------------------------------------------------*/
export sql_open_connection/4.
sql_open_connection(DataSource, User, Password, Connection) 
	:-
	get_odbc_environment(Environment),
	sql_alloc_connect(Environment, Connection),
	catch(
		sql_connect(Connection, DataSource, User, Password),
		ConnectError, 
		( sql_free_connect(Connection), throw(ConnectError) )
		 ).

/*!--------------------------------------------------------------*
 |	sql_open_connection/3
 |	sql_open_connection(ConString, OutString, Connection)
 |	sql_open_connection(+, -, -)
 |
 |	- open a connection to a data source
 |
 |	Note that: sql_alloc_connect/2 is already defined
 *--------------------------------------------------------------*/
export sql_open_connection/3.
sql_open_connection(ConString, OutString, Connection) 
	:-
	get_odbc_environment(Environment),
	sql_alloc_connect(Environment, Connection),
	catch(
		sql_driver_connect(Connection, 0, ConString, OutString, 
										512, 'SQL_DRIVER_NOPROMPT'),
		ConnectError, 
		( sql_free_connect(Connection), throw(ConnectError) )
	     ).

/*!--------------------------------------------------------------*
 |	sql_open_connection/2
 |	sql_open_connection(Connection, ConnectionString)
 |	sql_open_connection(-, -)
 |
 |	- open a connection to a data source
 |
 |	Note that: sql_alloc_connect/2 is already defined
 *--------------------------------------------------------------*/
export sql_open_connection/2.
sql_open_connection(Connection, ConnectionString) 
	:-
	get_odbc_environment(Environment),
	sql_alloc_connect(Environment, Connection),
	catch(
		( o_GetForegroundWindow(X),
		  sql_driver_connect(Connection, X, '', ConnectionString, 
		  								512, 'SQL_DRIVER_PROMPT')), 
		ConnectError, 
		( sql_free_connect(Connection), throw(ConnectError) )
	     ).

/*!--------------------------------------------------------------*
 |	sql_close_connection/1
 |	sql_close_connection(Connection)
 |	sql_close_connection(+)
 |
 |	-deallocate an SQL/ODBC connection
 *--------------------------------------------------------------*/
export sql_close_connection/1.
sql_close_connection(Connection) :-
	catch ((
		sql_disconnect(Connection)
	), DisconnectError, (
		sql_free_connect(DBConnection),
		throw(DisconnectError)
	)),
	sql_free_connect(Connection).

/*!--------------------------------------------------------------*
 |	sql_commit
 |	sql_commit(Connection)
 |	sql_commit(+)
 |
 |	- submit a statement 'SQL_COMMIT' to a data source
 *--------------------------------------------------------------*/
export sql_commit/1.
sql_commit(Connection) 
	:-
	get_odbc_environment(Environment),
	sql_transact(Environment, Connection, 'SQL_COMMIT').
	
/*!--------------------------------------------------------------*
 |	sql_open_statement/3
 |	sql_open_statement(Connection, SQLQuery, StatementTerm) 
 |	sql_open_statement(+, +, -) 
 |
 |	- allocate a statement data structure
 *--------------------------------------------------------------*/
export sql_open_statement/3.
sql_open_statement(Connection, SQLQuery, StatementTerm) 
	:-
	sql_alloc_stmt(Connection, StatementHandle),
	catch (
		(
		sql_prepare(StatementHandle, SQLQuery),
		sql_num_result_cols(StatementHandle, NumColumns),
		alloc_col_info(StatementHandle, 1, NumColumns, ColumnInfoList),
		sql_num_params(StatementHandle, NumParams),
		alloc_param_info(StatementHandle, 1, NumParams, ParamInfoList),
		StatementTerm = statement(StatementHandle, ColumnInfoList, ParamInfoList)
		), 
		StatementError, 
		( sql_free_stmt(StatementHandle, 'SQL_DROP'), throw(StatementError) )
	).

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

/*!--------------------------------------------------------------*
 |	sql_close_statement/1
 |	sql_close_statement(StatementTerm)
 |	sql_close_statement(+)
 |
 |	- deallocate an SQL statement data structure
 |
 |	StatementTerm must be a term returned by a call to
 |
 |			sql_open_statement/3
 |
 |	and is of the form
 |
 |		statement(StatementHandle, ColumnInfoList, ParamInfoList)
 *--------------------------------------------------------------*/
export sql_close_statement/1.
sql_close_statement(statement(StatementHandle, ColumnInfoList, ParamInfoList)) 
	:-
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
	
/*!--------------------------------------------------------------*
 | sql_execute_statement/1
 | sql_execute_statement(StatementTerm)
 | sql_execute_statement(+)
 |
 |	-	Executes an SQL/ODBC statement term
 |
 |	StatementTerm must be a term returned by a call to
 |
 |			sql_open_statement/3
 |
 |	and is of the form
 |
 |		statement(StatementHandle, ColumnInfoList, ParamInfoList)
 *--------------------------------------------------------------*/
export sql_execute_statement/1.
sql_execute_statement(statement(StatementHandle, _, _)) 
	:-
	sql_execute(StatementHandle).

/*--------------------------------------------------------------*
 *--------------------------------------------------------------*/
export sql_bind_row/2.
sql_bind_row(statement(StatementHandle, ColumnInfoList, ParamInfoList), Row) 
	:-
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

/*!--------------------------------------------------------------*
 |	sql_fetch_row/2
 |	sql_fetch_row(StatementTerm, Row)
 |	sql_fetch_row(+, -)
 |
 |	- fetch a row of returned data from a StatementTerm
 *--------------------------------------------------------------*/
export sql_fetch_row/2.
sql_fetch_row(statement(StatementHandle, [], _), Row) 
	:- 
	fail.
sql_fetch_row(statement(StatementHandle, ColumnInfoList, _), Row) 
	:-
	sql_fetch(StatementHandle),
	collect_results(ColumnInfoList, Row).

collect_results([], []).
collect_results([col_info(_, Type, Datum, _) | ColRest], [Result | ResultRest]) 
	:-
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

/*!-------------------------------------------------------------*
 *--------------------------------------------------------------*/
export sql_fetch_all_rows/2.
sql_fetch_all_rows(StatementTerm, [R | RowsList])
	:-
	sql_fetch_row(StatementTerm, R),
	!,
	sql_fetch_all_rows(StatementTerm, RowsList).

sql_fetch_all_rows(StatementTerm, []).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Obtaining Information about the Data Source's 
	%%%% System Tables (Catalog Functions)
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*!-------------------------------------------------------------*
 *--------------------------------------------------------------*/
export sql_tables/2.
sql_tables(Connection, TablesList) 
	:-
	sql_alloc_stmt(Connection, StatementHandle),
	o_SQLTables(StatementHandle,0,0,0,0,0,0,0,0,Result),
	check_sql_result(Result, statement, StatementHandle),
	sql_fetch_the_rows(StatementHandle, TablesList).

/*!-------------------------------------------------------------*
 *--------------------------------------------------------------*/
sql_fetch_the_rows(StatementHandle, TablesList)
	:-
	sql_num_result_cols(StatementHandle, NumColumns),
	alloc_col_info(StatementHandle, 1, NumColumns, ColumnInfoList),
	StatementTerm = statement(StatementHandle, ColumnInfoList, []),
	sql_fetch_all_rows(StatementTerm, TablesList).

/*!-------------------------------------------------------------*
 *--------------------------------------------------------------*/
export get_a_row/2.
get_a_row(statement(StatementHandle, [], _), Row)
	:-!,
 	fail.

get_a_row(statement(StatementHandle, ColumnInfoList, _), Row)
	:-
	sql_fetch(StatementHandle,Outcome),
	disp_get_a_row(Outcome,statement(StatementHandle, ColumnInfoList, _), Row).

disp_get_a_row('SQL_NO_DATA_FOUND',_, _) :-!, fail.

disp_get_a_row(_,statement(StatementHandle, ColumnInfoList, _), Row)
	:-
	collect_results(ColumnInfoList, Row).

disp_get_a_row(_,statement(StatementHandle, ColumnInfoList, _), Row)
	:-
	get_a_row(statement(StatementHandle, ColumnInfoList, _), Row).




/*
sql_fetch_row(statement(StatementHandle, [], _), Row) 
	:- 
	fail.
sql_fetch_row(statement(StatementHandle, ColumnInfoList, _), Row) 
	:-
	sql_fetch(StatementHandle),
	collect_results(ColumnInfoList, Row).
*/

/*!-------------------------------------------------------------*
 *--------------------------------------------------------------*/
export ds_tables/2.
ds_tables(DS, TablesList)
	:-
	sql_open_connection(DS, '', '', C),
	sql_tables(C, TablesList).

/*!-------------------------------------------------------------*
 *--------------------------------------------------------------*/
export sql_columns/3.
sql_columns(Connection, Table, ColsList) 
	:-
	sql_alloc_stmt(Connection, StatementHandle),
	atom_length(Table, TLen),
	o_SQLColumns(StatementHandle,0,0,0,0,Table,TLen,0,0,Result),
	check_sql_result(Result, statement, StatementHandle),
	sql_fetch_the_rows(StatementHandle, ColsList).
	
/*!-------------------------------------------------------------*
 *--------------------------------------------------------------*/
export ds_table_cols/3.
ds_table_cols(DS, Table, ColsList)
	:-
	sql_open_connection('Economics', '', '', C),
	sql_columns(C, Table, ColsList0),
	cvrt_to_tms(ColsList0, c, ColsList).

cvrt_to_tms([], Func, []).
cvrt_to_tms([List | ListOfLists], Func, [Term | TermsList])
	:-
	Term =.. [Func | List],
	cvrt_to_tms(ListOfLists, Func, TermsList).


/*
sql_column_privileges :- SQLColumnPrivileges.
sql_foreign_keys :- SQLForeignKeys.
sql_primary_keys :- SQLPrimaryKeys.
sql_procedure_columns :- SQLProcedureColumns.
sql_procedures :- SQLProcedures.
sql_special_columns :- SQLSpecialColumns.
sql_statistics :- SQLStatistics.
sql_table_privileges :- SQLTablePrivileges.
*/


/*!-----------------------------------------------------------*
 |	sql_all_drivers/1
 |	sql_all_drivers(DriversList)
 |	sql_all_drivers(-)
 |
 |	- Obtain list of ODBC drivers
 |
 |	DriversList is the list of descriptions of all ODBC drivers
 |	known by the ODBC Driver Manager.  The elements of the list 
 |	are of the form [Desc, Attrib], where Desc and Attrib are 
 |	both atoms.  Based on the SQLDrivers call.
 *-----------------------------------------------------------*/
export sql_all_drivers/1.
sql_all_drivers(DriversList)
	:-
	get_odbc_environment(Environment),
	Direction = 'SQL_FETCH_FIRST',
	DriverDescMax = 256,
	DriverAttribMax = 256,
	sql_all_drivers(Environment, Direction, 
					 DriverDescMax, DriverAttribMax, DriversList).

sql_all_drivers(Environment, Direction, 
				 DriverDescMax, DriverAttribMax, DriversList)
	:-
	must_c_const(Direction, DirectionNumber),
	c_allocn(char, DriverDescMax, DriverDesc),
	c_alloc(short, DriverDescLenPtr),
	c_allocn(char, DriverAttribMax, DriverAttrib),
	c_alloc(short, DriverAttribLenPtr),
	o_SQLDrivers(Environment,  DirectionNumber,
			DriverDesc, DriverDescMax, DriverDescLenPtr,
			DriverAttrib, DriverAttribMax, DriverAttribLenPtr, Result),
	sql_all_drivers0(Result, Environment, Direction, 
				DriverDesc, DriverDescMax, DriverDescLenPtr,
				DriverAttrib, DriverAttribMax, DriverAttribLenPtr, 
				DriversList).

sql_all_drivers0(Result, Environment, Direction, 
		DriverDesc, DriverDescMax, DriverDescLenPtr,
		DriverAttrib, DriverAttribMax, DriverAttribLenPtr, [])
	:-
	c_const('SQL_NO_DATA_FOUND', Result),
	!.

sql_all_drivers0(Result, Environment, Direction, 
		DriverDesc, DriverDescMax, DriverDescLenPtr,
		DriverAttrib, DriverAttribMax, DriverAttribLenPtr, 
		DriversList)
	:-
	(c_const('SQL_SUCCESS', Result) ; 
		c_const('SQL_SUCCESS_WITH_INFO', Result)),
	!,
	c_examine(DriverDescLenPtr, short, DriverDescLength),
	clip_uia(DriverDesc, DriverDescLength),
	c_examine(DriverAttribLenPtr, short, DriverAttribLength),
	clip_uia(DriverAttrib, DriverAttribLength),
	DriversList = [[DriverDesc,DriverAttrib] | RestDriversList],
	NextDirection = 'SQL_FETCH_NEXT',
	sql_all_drivers(Environment, NextDirection, 
				 DriverDescMax, DriverAttribMax, RestDriversList).

sql_all_drivers0(Result, Environment, _, _, _, _, _, _, _, _)
	:-
	collect_errors(environment, Environment, Errors),
	throw_error(sql_error(Result, Errors)).

/*!-----------------------------------------------------------*
 |	sql_all_data_sources/1
 |	sql_all_data_sources(SourcesList)
 |	sql_all_data_sources(-)
 |	
 |	- obtain list of data sources
 |
 |	SourcesList is the list of descriptions of all data
 |	sources known by the ODBC Driver Manager.  The elements of
 |	the list are of the form [Name, Desc], where Name and
 |	Desc are both atoms.  Based on the SQLDataSources call.
 *-----------------------------------------------------------*/
export sql_all_data_sources/1.
sql_all_data_sources(SourcesList)
	:-
	get_odbc_environment(Environment),
	Direction = 'SQL_FETCH_FIRST',
	DataSourceNameMax = 256,
	DescriptionMax = 256,
	sql_all_data_sources(Environment, Direction, 
						DataSourceNameMax, DescriptionMax, SourcesList).

sql_all_data_sources(Environment, Direction, 
						DataSourceNameMax, DescriptionMax, SourcesList)
	:-
	must_c_const(Direction, DirectionNumber),
	c_allocn(char, DataSourceNameMax, DataSourceName),
	c_alloc(short, DataSourceLenPtr),
	c_allocn(char, DescriptionMax, Description),
	c_alloc(short, DescriptionLenPtr),
	o_SQLDataSources(Environment,  DirectionNumber,
			DataSourceName, DataSourceNameMax, DataSourceLenPtr,
			Description, DescriptionMax, DescriptionLenPtr, Result),
	sql_all_data_sources0(Result,Environment, Direction, 
		DataSourceName, DataSourceNameMax, DataSourceLenPtr,
		Description, DescriptionMax, DescriptionLenPtr, SourcesList).

sql_all_data_sources0(Result, Environment, Direction, 
	DataSourceName, DataSourceNameMax, DataSourceLenPtr,
	Description, DescriptionMax, DescriptionLenPtr, [])
	:-
	c_const('SQL_NO_DATA_FOUND', Result),
	!.

sql_all_data_sources0(Result, Environment, Direction, 
	DataSourceName, DataSourceNameMax, DataSourceLenPtr,
	Description, DescriptionMax, DescriptionLenPtr, SourcesList)
	:-
	(c_const('SQL_SUCCESS', Result) ; 
		c_const('SQL_SUCCESS_WITH_INFO', Result)),
	!,
	c_examine(DataSourceLenPtr, short, DataSourceLength),
	clip_uia(DataSourceName, DataSourceLength),
	c_examine(DescriptionLenPtr, short, DescriptionLength),
	clip_uia(Description, DescriptionLength),
	SourcesList = [[DataSourceName,Description] | RestSourcesList],
	NextDirection = 'SQL_FETCH_NEXT',
	sql_all_data_sources(Environment, NextDirection, 
						DataSourceNameMax, DescriptionMax, RestSourcesList).

sql_all_data_sources0(Result, Environment, _, _, _, _, _, _, _, _)
	:-
	collect_errors(environment, Environment, Errors),
	throw_error(sql_error(Result, Errors)).

/*!-----------------------------------------------------------*
 *-----------------------------------------------------------*/

:- module_closure(defSQLview,1).
defSQLview(ExecMod, Dfn)
	:-
	defSQLview(ExecMod, Dfn, Result),
	subset([mod=Mod, simple_access=SimpleAccess, 
				access=Access, insert=Insert], Result),
	init_mod_for_odbc(Mod),
	do_clause_record(SimpleAccess, Mod),
	do_clause_record(Access, Mod),
	do_clause_record(Insert, Mod).

do_clause_record(Clause, Mod)
	:-
	Clause = (Head :- _),
	functor(Head, Pred, Arity),
	Mod:abolish(Pred/Arity),
	Mod:assert(Clause).

init_mod_for_odbc(Mod)
	:-
	modules(Mod, ML),
	!,
	(dmember(odbc,ML) -> true ; Mod:douse(odbc)).

init_mod_for_odbc(Mod)
	:-
	create_new_module(Mod),
	Mod:douse(odbc).

defSQLview(ExecMod, Dfn, Result)
	:-
		%% Temporary:
	Result = 
	   [mod=Mod, simple_access=SimpleAccess, access=Access, insert=Insert],
	dmember(ds=DSName, Dfn),
	dmember(table=Table, Dfn),
	dmember(cols_list=ViewColsList, Dfn),
	(dmember(mod=Mod, Dfn) -> true ; Mod = ExecMod),
	dmember(pred=Pred, Dfn),
	length(ViewColsList, NCols),
	simple_select_pat(ViewColsList, Table, SSelStmt),
	catenate('simple_',Pred,SAPred),
	functor(SAHead, SAPred, NCols),
	SAHead =.. [_|SAArgVars],
	SimpleAccess = (SAHead :- 
		sql_open_connection(DSName, '', '', C),
 		sql_open_statement(C, SSelStmt, S),
 		sql_execute_statement(S),
		!,
		get_a_row(S, SAArgVars)
		),

	functor(AHead, Pred, NCols),
	AHead =.. [_|AArgVars],
	ds_table_cols(DSName,Table,AllColsList),
	xtract_col_types(ViewColsList, AllColsList, ViewColTypes),
	select_pat(ViewColsList, Table, AllColsList, SelPat),
	Access = (AHead :- 
		sql_open_connection(DSName, '', '', C),
		odbc:where_conds(AArgVars, ViewColsList, ViewColTypes, SelPat, SelStmt),
 		sql_open_statement(C, SelStmt, S),
 		sql_execute_statement(S),
		!,
		get_a_row(S, AArgVars)
		),

	catenate('insert_',Pred,IPred),
	functor(IHead, IPred, NCols),
	IHead =.. [_|IArgVars],
	bld_insrt_pattern(ViewColsList, Table, NCols, IArgVars, AllColsList, InsrtPat),
	Insert = (IHead :- 
		sql_open_connection(DSName, '', '', C1),
		sprintf(atom(InsrtStmt), InsrtPat, IArgVars),
 		sql_open_statement(C1, InsrtStmt, S1),
		!,
 		sql_execute_statement(S1)
		).

xtract_col_types([], _, []).
xtract_col_types([VC | ViewColsList], AllColsList, [Type | ViewColTypes])
	:-
	member(CD, AllColsList),
	arg(4, CD, VC), 
	arg(6, CD, Type),
	xtract_col_types(ViewColsList, AllColsList, ViewColTypes).

	%'INSERT INTO datatable1(text1, value1) VALUES (\'%t\', %t)', 
bld_insrt_pattern(ViewColsList, Table, NCols, IArgVars, AllColsList, InsrtPat)
	:-
	atom_codes(Table, TableCs),	
	append("(" , Rest0, X0s),
	append(TableCs, X0s, X1s),
	append("INSERT INTO ",X1s, PatCs),
	insrt_list_of_atoms(ViewColsList, Rest0, Rest1),
	append(") VALUES (", Rest2, Rest1),
	n_pctts(ViewColsList, AllColsList, ")", Rest2),
	atom_codes(InsrtPat, PatCs).

n_pctts([ColName], AllColsList, TerminalCs, PatCs)
	:-
	member(CD, AllColsList),
	arg(4, CD, ColName),
	arg(6, CD, 'TEXT'),
	!,
	append("'%t'",TerminalCs,PatCs).

n_pctts([ColName], AllColsList, TerminalCs, PatCs)
	:-!,
	append("%t",TerminalCs,PatCs).

n_pctts([ColName | RestViewCols], AllColsList, TerminalCs, PatCs)
	:-
	member(CD, AllColsList),
	arg(4, CD, ColName),
	arg(6, CD, 'TEXT'),
	!,
	append("'%t',", PatCsTail, PatCs),
	n_pctts(RestViewCols, AllColsList, TerminalCs, PatCsTail).

n_pctts([ColName | RestViewCols], AllColsList, TerminalCs, PatCs)
	:-
	append("%t, ", PatCsTail, PatCs),
	n_pctts(RestViewCols, AllColsList, TerminalCs, PatCsTail).

simple_select_pat(ColsList, Table, Pattern)
	:-
	atom_codes(Table, TableCs),
	append(" FROM ", TableCs, TerminalCs),
	insrt_list_of_atoms(ColsList, Rest0, TerminalCs),
	append("SELECT ALL ", Rest0, PatternCs),
	atom_codes(Pattern, PatternCs).

insrt_list_of_atoms([], Tail, Tail).
insrt_list_of_atoms([Atom], CurTail, FinalTail)
	:-!,
	atom_codes(Atom, AtomCs),
	append(AtomCs, FinalTail,  CurTail).

insrt_list_of_atoms([Atom | Atoms], CurTail, FinalTail)
	:-
	atom_codes(Atom, AtomCs),
	append(",", NewTail,  XYYs),
	append(AtomCs, XYYs,  CurTail),
	insrt_list_of_atoms(Atoms, NewTail, FinalTail).

select_pat(ColsList, Table, AllColsList, SelPat)
	:-
	append("SELECT ", Rest0, PatternCs),
	insrt_list_of_atoms(ColsList, Rest0, Rest1),
	atom_codes(Table, TableCs),
	append(" FROM ", TableCs, STCs),
	append(STCs, " %t", XCs),
	XCs = Rest1,
	atom_codes(SelPat, PatternCs).

where_conds(AArgs, ColsList, ColTypes, SelPat, SelStmt)
	:-
	nvacts(AArgs, ColsList, ColTypes, NVL),
	(NVL = [] -> WCX = '' ;
		wc_cs(NVL, WCCs),
		append(" WHERE ", WCCs, YCs),
		atom_codes(WCX, YCs)
	),
	sprintf(atom(SelStmt), SelPat, [WCX]).

nvacts([], [], [], []).
nvacts([A | AArgs], [_ | ColsList], [_ | ColTypes], NVL)
	:-
	var(A),
	!,
	nvacts(AArgs, ColsList, ColTypes, NVL).
nvacts([A | AArgs], [C | ColsList], [T | ColTypes], [(A,C,T) | NVL])
	:-
	nvacts(AArgs, ColsList, ColTypes, NVL).

wc_cs([], []).
wc_cs([(AA,Col,ColType) | NVL], WCCs)
	:-
	atom_codes(AA, AACs),
	atom_codes(Col, CCs),
	(ColType = 'TEXT' ->
		append([0'' | AACs], "'", VCs)
		;
		VCs = AACs
	),
	(NVL = [] -> UVCs = VCs ; append(VCs, ", ", UVCs)),
	append(UVCs, RestWCCs, XCs),
	append(" = ", XCs, YCs),
	append(CCs, YCs, WCCs),
	wc_cs(NVL, RestWCCs).


endmod.

