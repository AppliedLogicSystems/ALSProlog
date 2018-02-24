module builtins.

export o_SQLExtendedFetch/6.
o_SQLExtendedFetch(A,B,C,D,E,F) :-
	odbc0(0,A,B,C,D,E,F).
export o_SQLGetInfo/6.
o_SQLGetInfo(A,B,C,D,E,F) :-
	odbc0(1,A,B,C,D,E,F).
export o_SQLSetScrollOptions/5.
o_SQLSetScrollOptions(A,B,C,D,E) :-
	odbc1(0,A,B,C,D,E).
export o_SQLSetPos/5.
o_SQLSetPos(A,B,C,D,E) :-
	odbc1(1,A,B,C,D,E).
export o_SQLGetCursorName/5.
o_SQLGetCursorName(A,B,C,D,E) :-
	odbc2(0,A,B,C,D,E).
export o_SQLParamOptions/4.
o_SQLParamOptions(A,B,C,D) :-
	odbc3(0,A,B,C,D).
export o_SQLSetStmtOption/4.
o_SQLSetStmtOption(A,B,C,D) :-
	odbc3(1,A,B,C,D).
export o_SQLSetConnectOption/4.
o_SQLSetConnectOption(A,B,C,D) :-
	odbc3(2,A,B,C,D).
export o_SQLPutData/4.
o_SQLPutData(A,B,C,D) :-
	odbc3(3,A,B,C,D).
export o_SQLGetStmtOption/4.
o_SQLGetStmtOption(A,B,C,D) :-
	odbc3(4,A,B,C,D).
export o_SQLGetFunctions/4.
o_SQLGetFunctions(A,B,C,D) :-
	odbc3(5,A,B,C,D).
export o_SQLGetConnectOption/4.
o_SQLGetConnectOption(A,B,C,D) :-
	odbc3(6,A,B,C,D).
export o_SQLTransact/4.
o_SQLTransact(A,B,C,D) :-
	odbc3(7,A,B,C,D).
export o_SQLSetCursorName/4.
o_SQLSetCursorName(A,B,C,D) :-
	odbc4(0,A,B,C,D).
export o_SQLPrepare/4.
o_SQLPrepare(A,B,C,D) :-
	odbc4(1,A,B,C,D).
export o_SQLExecDirect/4.
o_SQLExecDirect(A,B,C,D) :-
	odbc4(2,A,B,C,D).
export o_SQLStatistics/10.
o_SQLStatistics(A,B,C,D,E,F,G,H,I,J) :-
	odbc5(0,A,B,C,D,E,F,G,H,I,J).
export o_SQLProcedureColumns/10.
o_SQLProcedureColumns(A,B,C,D,E,F,G,H,I,J) :-
	odbc6(0,A,B,C,D,E,F,G,H,I,J).
export o_SQLColumnPrivileges/10.
o_SQLColumnPrivileges(A,B,C,D,E,F,G,H,I,J) :-
	odbc6(1,A,B,C,D,E,F,G,H,I,J).
export o_SQLTables/10.
o_SQLTables(A,B,C,D,E,F,G,H,I,J) :-
	odbc6(2,A,B,C,D,E,F,G,H,I,J).
export o_SQLColumns/10.
o_SQLColumns(A,B,C,D,E,F,G,H,I,J) :-
	odbc6(3,A,B,C,D,E,F,G,H,I,J).
export o_SQLDescribeCol/10.
o_SQLDescribeCol(A,B,C,D,E,F,G,H,I,J) :-
	odbc7(0,A,B,C,D,E,F,G,H,I,J).
export o_SQLNumParams/3.
o_SQLNumParams(A,B,C) :-
	odbc8(0,A,B,C).
export o_SQLParamData/3.
o_SQLParamData(A,B,C) :-
	odbc8(1,A,B,C).
export o_SQLGetTypeInfo/3.
o_SQLGetTypeInfo(A,B,C) :-
	odbc8(2,A,B,C).
export o_SQLRowCount/3.
o_SQLRowCount(A,B,C) :-
	odbc8(3,A,B,C).
export o_SQLNumResultCols/3.
o_SQLNumResultCols(A,B,C) :-
	odbc8(4,A,B,C).
export o_SQLFreeStmt/3.
o_SQLFreeStmt(A,B,C) :-
	odbc8(5,A,B,C).
export o_SQLAllocStmt/3.
o_SQLAllocStmt(A,B,C) :-
	odbc8(6,A,B,C).
export o_SQLAllocConnect/3.
o_SQLAllocConnect(A,B,C) :-
	odbc8(7,A,B,C).
export o_SQLMoreResults/2.
o_SQLMoreResults(A,B) :-
	odbc9(0,A,B).
export o_SQLFreeEnv/2.
o_SQLFreeEnv(A,B) :-
	odbc9(1,A,B).
export o_SQLFreeConnect/2.
o_SQLFreeConnect(A,B) :-
	odbc9(2,A,B).
export o_SQLFetch/2.
o_SQLFetch(A,B) :-
	odbc9(3,A,B).
export o_SQLExecute/2.
o_SQLExecute(A,B) :-
	odbc9(4,A,B).
export o_SQLDisconnect/2.
o_SQLDisconnect(A,B) :-
	odbc9(5,A,B).
export o_SQLCancel/2.
o_SQLCancel(A,B) :-
	odbc9(6,A,B).
export o_SQLAllocEnv/2.
o_SQLAllocEnv(A,B) :-
	odbc9(7,A,B).
export o_SQLForeignKeys/14.
o_SQLForeignKeys(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-
	odbc10(0,A,B,C,D,E,F,G,H,I,J,K,L,M,N).
export o_SQLBindParameter/11.
o_SQLBindParameter(A,B,C,D,E,F,G,H,I,J,K) :-
	odbc11(0,A,B,C,D,E,F,G,H,I,J,K).
export o_SQLSpecialColumns/11.
o_SQLSpecialColumns(A,B,C,D,E,F,G,H,I,J,K) :-
	odbc12(0,A,B,C,D,E,F,G,H,I,J,K).
export o_SQLNativeSql/7.
o_SQLNativeSql(A,B,C,D,E,F,G) :-
	odbc13(0,A,B,C,D,E,F,G).
export o_SQLBrowseConnect/7.
o_SQLBrowseConnect(A,B,C,D,E,F,G) :-
	odbc13(1,A,B,C,D,E,F,G).
export o_SQLDescribeParam/7.
o_SQLDescribeParam(A,B,C,D,E,F,G) :-
	odbc14(0,A,B,C,D,E,F,G).
export o_SQLGetData/7.
o_SQLGetData(A,B,C,D,E,F,G) :-
	odbc14(1,A,B,C,D,E,F,G).
export o_SQLBindCol/7.
o_SQLBindCol(A,B,C,D,E,F,G) :-
	odbc14(2,A,B,C,D,E,F,G).
export o_SQLSetParam/9.
o_SQLSetParam(A,B,C,D,E,F,G,H,I) :-
	odbc15(0,A,B,C,D,E,F,G,H,I).
export o_SQLDrivers/9.
o_SQLDrivers(A,B,C,D,E,F,G,H,I) :-
	odbc16(0,A,B,C,D,E,F,G,H,I).
export o_SQLDataSources/9.
o_SQLDataSources(A,B,C,D,E,F,G,H,I) :-
	odbc16(1,A,B,C,D,E,F,G,H,I).
export o_SQLDriverConnect/9.
o_SQLDriverConnect(A,B,C,D,E,F,G,H,I) :-
	odbc17(0,A,B,C,D,E,F,G,H,I).
export o_SQLError/9.
o_SQLError(A,B,C,D,E,F,G,H,I) :-
	odbc18(0,A,B,C,D,E,F,G,H,I).
export o_SQLTablePrivileges/8.
o_SQLTablePrivileges(A,B,C,D,E,F,G,H) :-
	odbc19(0,A,B,C,D,E,F,G,H).
export o_SQLProcedures/8.
o_SQLProcedures(A,B,C,D,E,F,G,H) :-
	odbc19(1,A,B,C,D,E,F,G,H).
export o_SQLPrimaryKeys/8.
o_SQLPrimaryKeys(A,B,C,D,E,F,G,H) :-
	odbc19(2,A,B,C,D,E,F,G,H).
export o_SQLConnect/8.
o_SQLConnect(A,B,C,D,E,F,G,H) :-
	odbc19(3,A,B,C,D,E,F,G,H).
export o_SQLColAttributes/8.
o_SQLColAttributes(A,B,C,D,E,F,G,H) :-
	odbc20(0,A,B,C,D,E,F,G,H).

endmod.     %  builtins
