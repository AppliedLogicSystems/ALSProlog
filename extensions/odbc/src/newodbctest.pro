/* * odbctest.pro	-- test of the Open Database Connectivity interface for ALS Prolog. *		   This program is losely based on the QueryDemo program that is part *		   of ODBC SDK. * *	Copyright (c) 1996 Applied Logic Systems, Inc. * * Author: Chuck Houpt * Creation:	3/15/96 */:- [odbc, 'prolog odbc'].test :-	query('select * from customer').query(Query) :-	/* Allocate an Environment. */	sql_alloc_env(Environment),		catch ((		/* Allocate a connection. */		sql_alloc_connect(Environment, DBConnection),				catch ((			/* Connect the connection to the ODBC 2.1 SDK Access 32-bit sample database.			   (Note: The DBase samples don't seem to work properly with the SDK's 32-bit			    demo programs - maybe a bug in the 32-bit DBase drivers. */			sql_connect(DBConnection, 'sdk21-Access32', '', ''),						catch ((				/* Allocate a statement. */				sql_alloc_stmt(DBConnection, Statement),								catch ((					/* Execute the query. */					sql_exec_direct(Statement, Query),										/* Print out the number of columns in the result. */					sql_num_result_cols(Statement, NumResultCols),					printf('Number of Result columms: %t\n', [NumResultCols]),										/* Print the column names and data. */					printColumnNames(Statement, NumResultCols),					printColumnData(Statement, NumResultCols)				),				ExecError,				(					/* Close and Unbind the Statement. */					sql_free_stmt(Statement, 'SQL_CLOSE'),					sql_free_stmt(Statement, 'SQL_UNBIND'),					throw(ExecError)				)),								/* Close and Unbind the Statement. */				sql_free_stmt(Statement, 'SQL_CLOSE'),				sql_free_stmt(Statement, 'SQL_UNBIND')						),			StatementError,			(				/* Disconnect and free the connection. */				sql_disconnect(DBConnection),				throw(StatementError)			)),						/* Disconnect and free the connection. */			sql_disconnect(DBConnection)		),		ConnectError,		(			sql_free_connect(DBConnection),			throw(ConnectError)		)),				sql_free_connect(DBConnection)	),	EnvError,	(		/* Free the environment if there was an exception. */		sql_free_env(Environment),		throw(EnvError)	)),			/* Free the environment. */	sql_free_env(Environment)./* * printColumnNames(Statement, NumCols) */printColumnNames(Statement, NumCols) :-	NumCols > 0,	MaxCols is NumCols + 1,	printColumnNames0(Statement, 1, MaxCols).	printColumnNames0(_, MaxCols, MaxCols) :-	printf('\n').printColumnNames0(Statement, N, MaxCols) :-	sql_describe_col(Statement, N, ColName, 512, _, _, _, _),	printf('%t\t', [ColName]),	NextN is N + 1,	printColumnNames0(Statement, NextN, MaxCols)./* * printColumnData(Statement, NumCols) */printColumnData(Statement, NumCols) :-	NumCols > 0,	MaxCols is NumCols + 1,	bindColumns(Statement, 1, MaxCols, DataList, DataLenList),	catch ((		printColumnData0(Statement, DataList)	),	Error,	(		freeAbsList(DataList),		freeAbsList(DataLenList),		throw(Error)	)),	freeAbsList(DataList),	freeAbsList(DataLenList)./* * bindColumns/5 * Binds each column of Statement to a C string and C integer for storing * of fetch results. */bindColumns(Statement, MaxCols, MaxCols, [], []).bindColumns(Statement, N, MaxCols, [Datum | RestData], [Len | RestLen]) :-	c_allocn_abs(char, 255, Datum),	c_alloc_abs(int, Len),	sql_bind_col(Statement, N, 'SQL_C_CHAR', Datum, 255, Len),	NextN is N + 1,	bindColumns(Statement, NextN, MaxCols, RestData, RestLen)./*  * printColumnData0(Statement, DataList) * * Fetches results from Statement and prints them until Fetch fails. * DataList is the list of C strings where the fetched results are placed. * */printColumnData0(Statement, DataList) :-	sql_fetch(Statement),	printDataList(DataList),	printColumnData0(Statement, DataList).printColumnData0(Statement, _)./* * printDataList(List) * * Prints a list of C strings as a tab-delimited line. */printDataList([]) :-	printf('\n').printDataList([D | Rest]) :-	c_examine(D, str, Data),	printf('%t\t', [Data]),	printDataList(Rest)./* * freeAbsList(List)  *  * Frees a list of malloc-ed objects created with c_alloc_abs/2 or c_allocn_abs/3. */freeAbsList([]).freeAbsList([E | Rest]) :-	c_free_abs(E),	freeAbsList(Rest).