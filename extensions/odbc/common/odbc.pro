module builtins.

export o_SQLGetDescRec/12.
o_SQLGetDescRec(A,B,C,D,E,F,G,H,I,J,K,L) :-
	odbc0(0,A,B,C,D,E,F,G,H,I,J,K,L).
export o_SQLTablePrivileges/8.
o_SQLTablePrivileges(A,B,C,D,E,F,G,H) :-
	odbc1(0,A,B,C,D,E,F,G,H).
export o_SQLProcedures/8.
o_SQLProcedures(A,B,C,D,E,F,G,H) :-
	odbc1(1,A,B,C,D,E,F,G,H).
export o_SQLPrimaryKeys/8.
o_SQLPrimaryKeys(A,B,C,D,E,F,G,H) :-
	odbc1(2,A,B,C,D,E,F,G,H).
export o_SQLConnect/8.
o_SQLConnect(A,B,C,D,E,F,G,H) :-
	odbc1(3,A,B,C,D,E,F,G,H).
export o_SQLColAttributes/8.
o_SQLColAttributes(A,B,C,D,E,F,G,H) :-
	odbc2(0,A,B,C,D,E,F,G,H).
export o_SQLGetDiagField/8.
o_SQLGetDiagField(A,B,C,D,E,F,G,H) :-
	odbc2(1,A,B,C,D,E,F,G,H).
export o_SQLColAttribute/8.
o_SQLColAttribute(A,B,C,D,E,F,G,H) :-
	odbc2(2,A,B,C,D,E,F,G,H).
export o_SQLSetScrollOptions/5.
o_SQLSetScrollOptions(A,B,C,D,E) :-
	odbc3(0,A,B,C,D,E).
export o_SQLSetPos/5.
o_SQLSetPos(A,B,C,D,E) :-
	odbc3(1,A,B,C,D,E).
export o_SQLSetStmtAttr/5.
o_SQLSetStmtAttr(A,B,C,D,E) :-
	odbc3(2,A,B,C,D,E).
export o_SQLSetEnvAttr/5.
o_SQLSetEnvAttr(A,B,C,D,E) :-
	odbc3(3,A,B,C,D,E).
export o_SQLSetConnectAttr/5.
o_SQLSetConnectAttr(A,B,C,D,E) :-
	odbc3(4,A,B,C,D,E).
export o_SQLGetCursorName/5.
o_SQLGetCursorName(A,B,C,D,E) :-
	odbc4(0,A,B,C,D,E).
export o_SQLDrivers/9.
o_SQLDrivers(A,B,C,D,E,F,G,H,I) :-
	odbc5(0,A,B,C,D,E,F,G,H,I).
export o_SQLDataSources/9.
o_SQLDataSources(A,B,C,D,E,F,G,H,I) :-
	odbc5(1,A,B,C,D,E,F,G,H,I).
export o_SQLDriverConnect/9.
o_SQLDriverConnect(A,B,C,D,E,F,G,H,I) :-
	odbc6(0,A,B,C,D,E,F,G,H,I).
export o_SQLSetParam/9.
o_SQLSetParam(A,B,C,D,E,F,G,H,I) :-
	odbc7(0,A,B,C,D,E,F,G,H,I).
export o_SQLBindParam/9.
o_SQLBindParam(A,B,C,D,E,F,G,H,I) :-
	odbc7(1,A,B,C,D,E,F,G,H,I).
export o_SQLGetDiagRec/9.
o_SQLGetDiagRec(A,B,C,D,E,F,G,H,I) :-
	odbc8(0,A,B,C,D,E,F,G,H,I).
export o_SQLError/9.
o_SQLError(A,B,C,D,E,F,G,H,I) :-
	odbc8(1,A,B,C,D,E,F,G,H,I).
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
export o_SQLCloseCursor/2.
o_SQLCloseCursor(A,B) :-
	odbc9(6,A,B).
export o_SQLCancel/2.
o_SQLCancel(A,B) :-
	odbc9(7,A,B).
export o_SQLAllocEnv/2.
o_SQLAllocEnv(A,B) :-
	odbc9(8,A,B).
export o_SQLExtendedFetch/6.
o_SQLExtendedFetch(A,B,C,D,E,F) :-
	odbc10(0,A,B,C,D,E,F).
export o_SQLSetDescField/6.
o_SQLSetDescField(A,B,C,D,E,F) :-
	odbc10(1,A,B,C,D,E,F).
export o_SQLGetStmtAttr/6.
o_SQLGetStmtAttr(A,B,C,D,E,F) :-
	odbc10(2,A,B,C,D,E,F).
export o_SQLGetInfo/6.
o_SQLGetInfo(A,B,C,D,E,F) :-
	odbc10(3,A,B,C,D,E,F).
export o_SQLGetEnvAttr/6.
o_SQLGetEnvAttr(A,B,C,D,E,F) :-
	odbc10(4,A,B,C,D,E,F).
export o_SQLGetConnectAttr/6.
o_SQLGetConnectAttr(A,B,C,D,E,F) :-
	odbc10(5,A,B,C,D,E,F).
export o_SQLAllocHandleStd/4.
o_SQLAllocHandleStd(A,B,C,D) :-
	odbc11(0,A,B,C,D).
export o_SQLParamOptions/4.
o_SQLParamOptions(A,B,C,D) :-
	odbc11(1,A,B,C,D).
export o_SQLTransact/4.
o_SQLTransact(A,B,C,D) :-
	odbc11(2,A,B,C,D).
export o_SQLSetStmtOption/4.
o_SQLSetStmtOption(A,B,C,D) :-
	odbc11(3,A,B,C,D).
export o_SQLSetConnectOption/4.
o_SQLSetConnectOption(A,B,C,D) :-
	odbc11(4,A,B,C,D).
export o_SQLPutData/4.
o_SQLPutData(A,B,C,D) :-
	odbc11(5,A,B,C,D).
export o_SQLGetStmtOption/4.
o_SQLGetStmtOption(A,B,C,D) :-
	odbc11(6,A,B,C,D).
export o_SQLGetFunctions/4.
o_SQLGetFunctions(A,B,C,D) :-
	odbc11(7,A,B,C,D).
export o_SQLGetConnectOption/4.
o_SQLGetConnectOption(A,B,C,D) :-
	odbc11(8,A,B,C,D).
export o_SQLFetchScroll/4.
o_SQLFetchScroll(A,B,C,D) :-
	odbc11(9,A,B,C,D).
export o_SQLEndTran/4.
o_SQLEndTran(A,B,C,D) :-
	odbc11(10,A,B,C,D).
export o_SQLAllocHandle/4.
o_SQLAllocHandle(A,B,C,D) :-
	odbc11(11,A,B,C,D).
export o_TraceOpenLogFile/1.
o_TraceOpenLogFile(A) :-
	odbc12(0,A).
export o_SQLSetCursorName/4.
o_SQLSetCursorName(A,B,C,D) :-
	odbc13(0,A,B,C,D).
export o_SQLPrepare/4.
o_SQLPrepare(A,B,C,D) :-
	odbc13(1,A,B,C,D).
export o_SQLExecDirect/4.
o_SQLExecDirect(A,B,C,D) :-
	odbc13(2,A,B,C,D).
export o_SQLBindParameter/11.
o_SQLBindParameter(A,B,C,D,E,F,G,H,I,J,K) :-
	odbc14(0,A,B,C,D,E,F,G,H,I,J,K).
export o_SQLSetDescRec/11.
o_SQLSetDescRec(A,B,C,D,E,F,G,H,I,J,K) :-
	odbc14(1,A,B,C,D,E,F,G,H,I,J,K).
export o_SQLSpecialColumns/11.
o_SQLSpecialColumns(A,B,C,D,E,F,G,H,I,J,K) :-
	odbc15(0,A,B,C,D,E,F,G,H,I,J,K).
export o_SQLStatistics/10.
o_SQLStatistics(A,B,C,D,E,F,G,H,I,J) :-
	odbc16(0,A,B,C,D,E,F,G,H,I,J).
export o_SQLProcedureColumns/10.
o_SQLProcedureColumns(A,B,C,D,E,F,G,H,I,J) :-
	odbc17(0,A,B,C,D,E,F,G,H,I,J).
export o_SQLColumnPrivileges/10.
o_SQLColumnPrivileges(A,B,C,D,E,F,G,H,I,J) :-
	odbc17(1,A,B,C,D,E,F,G,H,I,J).
export o_SQLTables/10.
o_SQLTables(A,B,C,D,E,F,G,H,I,J) :-
	odbc17(2,A,B,C,D,E,F,G,H,I,J).
export o_SQLColumns/10.
o_SQLColumns(A,B,C,D,E,F,G,H,I,J) :-
	odbc17(3,A,B,C,D,E,F,G,H,I,J).
export o_SQLDescribeCol/10.
o_SQLDescribeCol(A,B,C,D,E,F,G,H,I,J) :-
	odbc18(0,A,B,C,D,E,F,G,H,I,J).
export o_SQLForeignKeys/14.
o_SQLForeignKeys(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-
	odbc19(0,A,B,C,D,E,F,G,H,I,J,K,L,M,N).
export o_SQLNumParams/3.
o_SQLNumParams(A,B,C) :-
	odbc20(0,A,B,C).
export o_SQLBulkOperations/3.
o_SQLBulkOperations(A,B,C) :-
	odbc20(1,A,B,C).
export o_SQLRowCount/3.
o_SQLRowCount(A,B,C) :-
	odbc20(2,A,B,C).
export o_SQLParamData/3.
o_SQLParamData(A,B,C) :-
	odbc20(3,A,B,C).
export o_SQLNumResultCols/3.
o_SQLNumResultCols(A,B,C) :-
	odbc20(4,A,B,C).
export o_SQLGetTypeInfo/3.
o_SQLGetTypeInfo(A,B,C) :-
	odbc20(5,A,B,C).
export o_SQLFreeStmt/3.
o_SQLFreeStmt(A,B,C) :-
	odbc20(6,A,B,C).
export o_SQLFreeHandle/3.
o_SQLFreeHandle(A,B,C) :-
	odbc20(7,A,B,C).
export o_SQLCopyDesc/3.
o_SQLCopyDesc(A,B,C) :-
	odbc20(8,A,B,C).
export o_SQLAllocStmt/3.
o_SQLAllocStmt(A,B,C) :-
	odbc20(9,A,B,C).
export o_SQLAllocConnect/3.
o_SQLAllocConnect(A,B,C) :-
	odbc20(10,A,B,C).
export o_SQLNativeSql/7.
o_SQLNativeSql(A,B,C,D,E,F,G) :-
	odbc21(0,A,B,C,D,E,F,G).
export o_SQLBrowseConnect/7.
o_SQLBrowseConnect(A,B,C,D,E,F,G) :-
	odbc21(1,A,B,C,D,E,F,G).
export o_SQLDescribeParam/7.
o_SQLDescribeParam(A,B,C,D,E,F,G) :-
	odbc22(0,A,B,C,D,E,F,G).
export o_SQLGetDescField/7.
o_SQLGetDescField(A,B,C,D,E,F,G) :-
	odbc22(1,A,B,C,D,E,F,G).
export o_SQLGetData/7.
o_SQLGetData(A,B,C,D,E,F,G) :-
	odbc22(2,A,B,C,D,E,F,G).
export o_SQLBindCol/7.
o_SQLBindCol(A,B,C,D,E,F,G) :-
	odbc22(3,A,B,C,D,E,F,G).
export o_VOID/1.
o_VOID(Value) :- 
	odbc_gv(0,Value).
export o_DWORD/1.
o_DWORD(Value) :- 
	odbc_gv(1,Value).

endmod.     %  builtins
