/*
 * odbctest.pro	-- test of the Open Database Connectivity interface for ALS Prolog.
 *		   This program is losely based on the QueryDemo program that is part
 *		   of ODBC SDK.
 *
 *	Copyright (c) 1996 Applied Logic Systems, Inc.
 *
 * Author: Chuck Houpt
 * Creation:	3/15/96
 */


test :-
	query('select * from customer').

query(Query) :-
	c_const('SQL_SUCCESS', SQL_Success),
	
	/* Allocate an Environment. */
	c_alloc(ptr, EnvrHandle),
	o_SQLAllocEnv(EnvrHandle, SQL_Success),
	c_examine(EnvrHandle, ptr, Environment),
	
	/* Allocate a connection. */
	c_alloc(ptr, DBConnHandle),
	o_SQLAllocConnect(Environment, DBConnHandle, SQL_Success),
	c_examine(DBConnHandle, ptr, DBConnection),
	
	/* Connect the connection to the ODBC 2.1 SDK Access 32-bit sample database.
	   (Note: The DBase samples don't seem to work properly with the SDK's 32-bit
	    demo programs - maybe a bug in the 32-bit DBase drivers. */
	c_const('SQL_NTS', SQL_NTS),
	o_SQLConnect(DBConnection, 'sdk21-Access32', '', ''),
	
	/* Allocate a statement. */
	c_alloc(ptr, StmntHandle),	
	o_SQLAllocStmt(DBConnection, StmntHandle, SQL_Success),
	c_examine(StmntHandle, ptr, Statement),
	
	/* Execute the query. */
	atom_length(Query, QueryLen),
	o_SQLExecDirect(Statement, Query, QueryLen, SQL_Success),
	
	/* Print out the number of columns in the result. */
	c_alloc(short, NumResColPtr),
	o_SQLNumResultCols(Statement, NumResColPtr, SQL_Success),
	c_examine(NumResColPtr, short, NumResultCols),
	printf('Number of Result columms: %t\n', [NumResultCols]),
	
	/* Print the column names and data. */
	printColumnNames(Statement, NumResultCols),
	printColumnData(Statement, NumResultCols),
	
	/* Close and Unbind the Statement. */
	c_const('SQL_CLOSE', SQL_CLOSE),
	c_const('SQL_UNBIND', SQL_UNBIND),
	o_SQLFreeStmt(Statement, SQL_CLOSE, SQL_Success),
	o_SQLFreeStmt(Statement, SQL_UNBIND, SQL_Success),
	
	/* Disconnect and free the connection. */
	o_SQLDisconnect(DBConnection, SQL_Success),
	o_SQLFreeConnect(DBConnection, SQL_Success),
	
	/* Free the environment. */
	o_SQLFreeEnv(Environment, SQL_Success).

/*
 * printColumnNames(Statement, NumCols)
 */
printColumnNames(Statement, NumCols) :-
	NumCols > 0,
	MaxCols is NumCols + 1,
	printColumnNames0(Statement, 1, MaxCols).
	
printColumnNames0(_, MaxCols, MaxCols) :-
	printf('\n').
printColumnNames0(Statement, N, MaxCols) :-
	c_const('SQL_SUCCESS', SQL_Success),
	c_allocn(char, 255, Buffer),
	c_alloc(short, ColLenPtr),
	c_alloc(short, ColTypePtr),
	c_alloc(int, ColDefPtr),
	c_alloc(short, ColScalePtr),
	c_alloc(short, ColNullPtr),
	o_SQLDescribeCol(Statement, N, Buffer, 255,
		ColLenPtr, ColTypePtr, ColDefPtr, ColScalePtr, ColNullPtr, SQL_Success),
	printf('%t\t', [Buffer]),
	NextN is N + 1,
	printColumnNames0(Statement, NextN, MaxCols).

/*
 * printColumnData(Statement, NumCols)
 */
printColumnData(Statement, NumCols) :-
	NumCols > 0,
	MaxCols is NumCols + 1,
	bindColumns(Statement, 1, MaxCols, DataList, DataLenList),
	printColumnData0(Statement, DataList),
	freeAbsList(DataList),
	freeAbsList(DataLenList).

/*
 * bindColumns/5
 * Binds each column of Statement to a C string and C integer for storing
 * of fetch results.
 */
bindColumns(Statement, MaxCols, MaxCols, [], []).
bindColumns(Statement, N, MaxCols, [Datum | RestData], [Len | RestLen]) :-
	c_const('SQL_SUCCESS', SQL_Success),
	c_const('SQL_C_CHAR', SQL_C_CHAR),
	c_allocn_abs(char, 255, Datum),
	c_alloc_abs(int, Len),
	o_SQLBindCol(Statement, N, SQL_C_CHAR, Datum, 255, Len, SQL_Success),
	NextN is N + 1,
	bindColumns(Statement, NextN, MaxCols, RestData, RestLen).

/* 
 * printColumnData0(Statement, DataList)
 *
 * Fetches results from Statement and prints them until Fetch fails.
 * DataList is the list of C strings where the fetched results are placed.
 *
 */
printColumnData0(Statement, DataList) :-
	c_const('SQL_SUCCESS', SQL_Success),
	o_SQLFetch(Statement, SQL_Success),
	printDataList(DataList),
	printColumnData0(Statement, DataList).
printColumnData0(Statement, _).

/*
 * printDataList(List)
 *
 * Prints a list of C strings as a tab-delimited line.
 */
printDataList([]) :-
	printf('\n').
printDataList([D | Rest]) :-
	c_examine(D, str, Data),
	printf('%t\t', [Data]),
	printDataList(Rest).


/*
 * freeAbsList(List) 
 * 
 * Frees a list of malloc-ed objects created with c_alloc_abs/2 or c_allocn_abs/3.
 */
freeAbsList([]).
freeAbsList([E | Rest]) :-
	c_free_abs(E),
	freeAbsList(Rest).
	



