/*================================================================
          odbc0.c
          --Generated from: odbc.src
          Date: 96/4/25   Time: 14:41:48
		--by ALS Interface Generator

 *===============================================================*/

#include "alspi.h"
#include "cinterf.h"
#include "odbc.h"


static int odbc_dispatch0(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;
	PWord arg5; int type5;
	PWord arg6; int type6;
	PWord arg7; int type7;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg5,type5))
			PI_FAIL;
	PI_getan(&arg6,&type6,6);
	if (type6 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg6,type6))
			PI_FAIL;
	PI_getan(&arg7,&type7,7);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) SQLExtendedFetch(((HSTMT  ) arg2),((UWORD  ) arg3),((SDWORD  ) arg4),((UDWORD * ) arg5),((UWORD * ) arg6));
			break;
		case 1:
			retval = (unsigned long) SQLGetInfo(((HDBC  ) arg2),((UWORD  ) arg3),((PTR  ) arg4),((SWORD  ) arg5),((SWORD * ) arg6));
			break;
		default:
			PI_FAIL;
	}
	PI_makedouble(&rval,&rtype,(double) retval);
	if (PI_unify(arg7,type7,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int odbc_dispatch1(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;
	PWord arg5; int type5;
	PWord arg6; int type6;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg5,type5))
			PI_FAIL;
	PI_getan(&arg6,&type6,6);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) SQLSetScrollOptions(((HSTMT  ) arg2),((UWORD  ) arg3),((SDWORD  ) arg4),((UWORD  ) arg5));
			break;
		case 1:
			retval = (unsigned long) SQLSetPos(((HSTMT  ) arg2),((UWORD  ) arg3),((UWORD  ) arg4),((UWORD  ) arg5));
			break;
		default:
			PI_FAIL;
	}
	PI_makedouble(&rval,&rtype,(double) retval);
	if (PI_unify(arg6,type6,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int odbc_dispatch2(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;
	PWord arg5; int type5;
	PWord arg6; int type6;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 == PI_SYM)
		arg3 = (unsigned long) PI_getsymname(0,arg3,0);
	else if (!CI_get_integer((unsigned long *)&arg3,type3))
		PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg5,type5))
			PI_FAIL;
	PI_getan(&arg6,&type6,6);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) SQLGetCursorName(((HSTMT  ) arg2),((UCHAR * ) arg3),((SWORD  ) arg4),((SWORD * ) arg5));
			break;
		default:
			PI_FAIL;
	}
	PI_makedouble(&rval,&rtype,(double) retval);
	if (PI_unify(arg6,type6,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int odbc_dispatch3(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;
	PWord arg5; int type5;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) SQLParamOptions(((HSTMT  ) arg2),((UDWORD  ) arg3),((UDWORD * ) arg4));
			break;
		case 1:
			retval = (unsigned long) SQLSetStmtOption(((HSTMT  ) arg2),((UWORD  ) arg3),((UDWORD  ) arg4));
			break;
		case 2:
			retval = (unsigned long) SQLSetConnectOption(((HDBC  ) arg2),((UWORD  ) arg3),((UDWORD  ) arg4));
			break;
		case 3:
			retval = (unsigned long) SQLPutData(((HSTMT  ) arg2),((PTR  ) arg3),((SDWORD  ) arg4));
			break;
		case 4:
			retval = (unsigned long) SQLGetStmtOption(((HSTMT  ) arg2),((UWORD  ) arg3),((PTR  ) arg4));
			break;
		case 5:
			retval = (unsigned long) SQLGetFunctions(((HDBC  ) arg2),((UWORD  ) arg3),((UWORD * ) arg4));
			break;
		case 6:
			retval = (unsigned long) SQLGetConnectOption(((HDBC  ) arg2),((UWORD  ) arg3),((PTR  ) arg4));
			break;
		case 7:
			retval = (unsigned long) SQLTransact(((HENV  ) arg2),((HDBC  ) arg3),((UWORD  ) arg4));
			break;
		default:
			PI_FAIL;
	}
	PI_makedouble(&rval,&rtype,(double) retval);
	if (PI_unify(arg5,type5,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int odbc_dispatch4(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;
	PWord arg5; int type5;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 == PI_SYM)
		arg3 = (unsigned long) PI_getsymname(0,arg3,0);
	else if (!CI_get_integer((unsigned long *)&arg3,type3))
		PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) SQLSetCursorName(((HSTMT  ) arg2),((UCHAR * ) arg3),((SWORD  ) arg4));
			break;
		case 1:
			retval = (unsigned long) SQLPrepare(((HSTMT  ) arg2),((UCHAR * ) arg3),((SDWORD  ) arg4));
			break;
		case 2:
			retval = (unsigned long) SQLExecDirect(((HSTMT  ) arg2),((UCHAR * ) arg3),((SDWORD  ) arg4));
			break;
		default:
			PI_FAIL;
	}
	PI_makedouble(&rval,&rtype,(double) retval);
	if (PI_unify(arg5,type5,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int odbc_dispatch5(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;
	PWord arg5; int type5;
	PWord arg6; int type6;
	PWord arg7; int type7;
	PWord arg8; int type8;
	PWord arg9; int type9;
	PWord arg10; int type10;
	PWord arg11; int type11;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 == PI_SYM)
		arg3 = (unsigned long) PI_getsymname(0,arg3,0);
	else if (!CI_get_integer((unsigned long *)&arg3,type3))
		PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 == PI_SYM)
		arg5 = (unsigned long) PI_getsymname(0,arg5,0);
	else if (!CI_get_integer((unsigned long *)&arg5,type5))
		PI_FAIL;
	PI_getan(&arg6,&type6,6);
	if (type6 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg6,type6))
			PI_FAIL;
	PI_getan(&arg7,&type7,7);
	if (type7 == PI_SYM)
		arg7 = (unsigned long) PI_getsymname(0,arg7,0);
	else if (!CI_get_integer((unsigned long *)&arg7,type7))
		PI_FAIL;
	PI_getan(&arg8,&type8,8);
	if (type8 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg8,type8))
			PI_FAIL;
	PI_getan(&arg9,&type9,9);
	if (type9 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg9,type9))
			PI_FAIL;
	PI_getan(&arg10,&type10,10);
	if (type10 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg10,type10))
			PI_FAIL;
	PI_getan(&arg11,&type11,11);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) SQLStatistics(((HSTMT  ) arg2),((UCHAR * ) arg3),((SWORD  ) arg4),((UCHAR * ) arg5),((SWORD  ) arg6),((UCHAR * ) arg7),((SWORD  ) arg8),((UWORD  ) arg9),((UWORD  ) arg10));
			break;
		default:
			PI_FAIL;
	}
	PI_makedouble(&rval,&rtype,(double) retval);
	if (PI_unify(arg11,type11,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int odbc_dispatch6(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;
	PWord arg5; int type5;
	PWord arg6; int type6;
	PWord arg7; int type7;
	PWord arg8; int type8;
	PWord arg9; int type9;
	PWord arg10; int type10;
	PWord arg11; int type11;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 == PI_SYM)
		arg3 = (unsigned long) PI_getsymname(0,arg3,0);
	else if (!CI_get_integer((unsigned long *)&arg3,type3))
		PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 == PI_SYM)
		arg5 = (unsigned long) PI_getsymname(0,arg5,0);
	else if (!CI_get_integer((unsigned long *)&arg5,type5))
		PI_FAIL;
	PI_getan(&arg6,&type6,6);
	if (type6 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg6,type6))
			PI_FAIL;
	PI_getan(&arg7,&type7,7);
	if (type7 == PI_SYM)
		arg7 = (unsigned long) PI_getsymname(0,arg7,0);
	else if (!CI_get_integer((unsigned long *)&arg7,type7))
		PI_FAIL;
	PI_getan(&arg8,&type8,8);
	if (type8 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg8,type8))
			PI_FAIL;
	PI_getan(&arg9,&type9,9);
	if (type9 == PI_SYM)
		arg9 = (unsigned long) PI_getsymname(0,arg9,0);
	else if (!CI_get_integer((unsigned long *)&arg9,type9))
		PI_FAIL;
	PI_getan(&arg10,&type10,10);
	if (type10 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg10,type10))
			PI_FAIL;
	PI_getan(&arg11,&type11,11);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) SQLProcedureColumns(((HSTMT  ) arg2),((UCHAR * ) arg3),((SWORD  ) arg4),((UCHAR * ) arg5),((SWORD  ) arg6),((UCHAR * ) arg7),((SWORD  ) arg8),((UCHAR * ) arg9),((SWORD  ) arg10));
			break;
		case 1:
			retval = (unsigned long) SQLColumnPrivileges(((HSTMT  ) arg2),((UCHAR * ) arg3),((SWORD  ) arg4),((UCHAR * ) arg5),((SWORD  ) arg6),((UCHAR * ) arg7),((SWORD  ) arg8),((UCHAR * ) arg9),((SWORD  ) arg10));
			break;
		case 2:
			retval = (unsigned long) SQLTables(((HSTMT  ) arg2),((UCHAR * ) arg3),((SWORD  ) arg4),((UCHAR * ) arg5),((SWORD  ) arg6),((UCHAR * ) arg7),((SWORD  ) arg8),((UCHAR * ) arg9),((SWORD  ) arg10));
			break;
		case 3:
			retval = (unsigned long) SQLColumns(((HSTMT  ) arg2),((UCHAR * ) arg3),((SWORD  ) arg4),((UCHAR * ) arg5),((SWORD  ) arg6),((UCHAR * ) arg7),((SWORD  ) arg8),((UCHAR * ) arg9),((SWORD  ) arg10));
			break;
		default:
			PI_FAIL;
	}
	PI_makedouble(&rval,&rtype,(double) retval);
	if (PI_unify(arg11,type11,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int odbc_dispatch7(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;
	PWord arg5; int type5;
	PWord arg6; int type6;
	PWord arg7; int type7;
	PWord arg8; int type8;
	PWord arg9; int type9;
	PWord arg10; int type10;
	PWord arg11; int type11;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 == PI_SYM)
		arg4 = (unsigned long) PI_getsymname(0,arg4,0);
	else if (!CI_get_integer((unsigned long *)&arg4,type4))
		PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg5,type5))
			PI_FAIL;
	PI_getan(&arg6,&type6,6);
	if (type6 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg6,type6))
			PI_FAIL;
	PI_getan(&arg7,&type7,7);
	if (type7 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg7,type7))
			PI_FAIL;
	PI_getan(&arg8,&type8,8);
	if (type8 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg8,type8))
			PI_FAIL;
	PI_getan(&arg9,&type9,9);
	if (type9 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg9,type9))
			PI_FAIL;
	PI_getan(&arg10,&type10,10);
	if (type10 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg10,type10))
			PI_FAIL;
	PI_getan(&arg11,&type11,11);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) SQLDescribeCol(((HSTMT  ) arg2),((UWORD  ) arg3),((UCHAR * ) arg4),((SWORD  ) arg5),((SWORD * ) arg6),((SWORD * ) arg7),((UDWORD * ) arg8),((SWORD * ) arg9),((SWORD * ) arg10));
			break;
		default:
			PI_FAIL;
	}
	PI_makedouble(&rval,&rtype,(double) retval);
	if (PI_unify(arg11,type11,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int odbc_dispatch8(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) SQLNumParams(((HSTMT  ) arg2),((SWORD * ) arg3));
			break;
		case 1:
			retval = (unsigned long) SQLParamData(((HSTMT  ) arg2),((PTR * ) arg3));
			break;
		case 2:
			retval = (unsigned long) SQLGetTypeInfo(((HSTMT  ) arg2),((SWORD  ) arg3));
			break;
		case 3:
			retval = (unsigned long) SQLRowCount(((HSTMT  ) arg2),((SDWORD * ) arg3));
			break;
		case 4:
			retval = (unsigned long) SQLNumResultCols(((HSTMT  ) arg2),((SWORD * ) arg3));
			break;
		case 5:
			retval = (unsigned long) SQLFreeStmt(((HSTMT  ) arg2),((UWORD  ) arg3));
			break;
		case 6:
			retval = (unsigned long) SQLAllocStmt(((HDBC  ) arg2),((HSTMT * ) arg3));
			break;
		case 7:
			retval = (unsigned long) SQLAllocConnect(((HENV  ) arg2),((HDBC * ) arg3));
			break;
		default:
			PI_FAIL;
	}
	PI_makedouble(&rval,&rtype,(double) retval);
	if (PI_unify(arg4,type4,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int odbc_dispatch9(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) SQLMoreResults(((HSTMT  ) arg2));
			break;
		case 1:
			retval = (unsigned long) SQLFreeEnv(((HENV  ) arg2));
			break;
		case 2:
			retval = (unsigned long) SQLFreeConnect(((HDBC  ) arg2));
			break;
		case 3:
			retval = (unsigned long) SQLFetch(((HSTMT  ) arg2));
			break;
		case 4:
			retval = (unsigned long) SQLExecute(((HSTMT  ) arg2));
			break;
		case 5:
			retval = (unsigned long) SQLDisconnect(((HDBC  ) arg2));
			break;
		case 6:
			retval = (unsigned long) SQLCancel(((HSTMT  ) arg2));
			break;
		case 7:
			retval = (unsigned long) SQLAllocEnv(((HENV * ) arg2));
			break;
		default:
			PI_FAIL;
	}
	PI_makedouble(&rval,&rtype,(double) retval);
	if (PI_unify(arg3,type3,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int odbc_dispatch10(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;
	PWord arg5; int type5;
	PWord arg6; int type6;
	PWord arg7; int type7;
	PWord arg8; int type8;
	PWord arg9; int type9;
	PWord arg10; int type10;
	PWord arg11; int type11;
	PWord arg12; int type12;
	PWord arg13; int type13;
	PWord arg14; int type14;
	PWord arg15; int type15;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 == PI_SYM)
		arg3 = (unsigned long) PI_getsymname(0,arg3,0);
	else if (!CI_get_integer((unsigned long *)&arg3,type3))
		PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 == PI_SYM)
		arg5 = (unsigned long) PI_getsymname(0,arg5,0);
	else if (!CI_get_integer((unsigned long *)&arg5,type5))
		PI_FAIL;
	PI_getan(&arg6,&type6,6);
	if (type6 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg6,type6))
			PI_FAIL;
	PI_getan(&arg7,&type7,7);
	if (type7 == PI_SYM)
		arg7 = (unsigned long) PI_getsymname(0,arg7,0);
	else if (!CI_get_integer((unsigned long *)&arg7,type7))
		PI_FAIL;
	PI_getan(&arg8,&type8,8);
	if (type8 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg8,type8))
			PI_FAIL;
	PI_getan(&arg9,&type9,9);
	if (type9 == PI_SYM)
		arg9 = (unsigned long) PI_getsymname(0,arg9,0);
	else if (!CI_get_integer((unsigned long *)&arg9,type9))
		PI_FAIL;
	PI_getan(&arg10,&type10,10);
	if (type10 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg10,type10))
			PI_FAIL;
	PI_getan(&arg11,&type11,11);
	if (type11 == PI_SYM)
		arg11 = (unsigned long) PI_getsymname(0,arg11,0);
	else if (!CI_get_integer((unsigned long *)&arg11,type11))
		PI_FAIL;
	PI_getan(&arg12,&type12,12);
	if (type12 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg12,type12))
			PI_FAIL;
	PI_getan(&arg13,&type13,13);
	if (type13 == PI_SYM)
		arg13 = (unsigned long) PI_getsymname(0,arg13,0);
	else if (!CI_get_integer((unsigned long *)&arg13,type13))
		PI_FAIL;
	PI_getan(&arg14,&type14,14);
	if (type14 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg14,type14))
			PI_FAIL;
	PI_getan(&arg15,&type15,15);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) SQLForeignKeys(((HSTMT  ) arg2),((UCHAR * ) arg3),((SWORD  ) arg4),((UCHAR * ) arg5),((SWORD  ) arg6),((UCHAR * ) arg7),((SWORD  ) arg8),((UCHAR * ) arg9),((SWORD  ) arg10),((UCHAR * ) arg11),((SWORD  ) arg12),((UCHAR * ) arg13),((SWORD  ) arg14));
			break;
		default:
			PI_FAIL;
	}
	PI_makedouble(&rval,&rtype,(double) retval);
	if (PI_unify(arg15,type15,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int odbc_dispatch11(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;
	PWord arg5; int type5;
	PWord arg6; int type6;
	PWord arg7; int type7;
	PWord arg8; int type8;
	PWord arg9; int type9;
	PWord arg10; int type10;
	PWord arg11; int type11;
	PWord arg12; int type12;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg5,type5))
			PI_FAIL;
	PI_getan(&arg6,&type6,6);
	if (type6 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg6,type6))
			PI_FAIL;
	PI_getan(&arg7,&type7,7);
	if (type7 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg7,type7))
			PI_FAIL;
	PI_getan(&arg8,&type8,8);
	if (type8 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg8,type8))
			PI_FAIL;
	PI_getan(&arg9,&type9,9);
	if (type9 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg9,type9))
			PI_FAIL;
	PI_getan(&arg10,&type10,10);
	if (type10 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg10,type10))
			PI_FAIL;
	PI_getan(&arg11,&type11,11);
	if (type11 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg11,type11))
			PI_FAIL;
	PI_getan(&arg12,&type12,12);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) SQLBindParameter(((HSTMT  ) arg2),((UWORD  ) arg3),((SWORD  ) arg4),((SWORD  ) arg5),((SWORD  ) arg6),((UDWORD  ) arg7),((SWORD  ) arg8),((PTR  ) arg9),((SDWORD  ) arg10),((SDWORD * ) arg11));
			break;
		default:
			PI_FAIL;
	}
	PI_makedouble(&rval,&rtype,(double) retval);
	if (PI_unify(arg12,type12,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int odbc_dispatch12(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;
	PWord arg5; int type5;
	PWord arg6; int type6;
	PWord arg7; int type7;
	PWord arg8; int type8;
	PWord arg9; int type9;
	PWord arg10; int type10;
	PWord arg11; int type11;
	PWord arg12; int type12;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 == PI_SYM)
		arg4 = (unsigned long) PI_getsymname(0,arg4,0);
	else if (!CI_get_integer((unsigned long *)&arg4,type4))
		PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg5,type5))
			PI_FAIL;
	PI_getan(&arg6,&type6,6);
	if (type6 == PI_SYM)
		arg6 = (unsigned long) PI_getsymname(0,arg6,0);
	else if (!CI_get_integer((unsigned long *)&arg6,type6))
		PI_FAIL;
	PI_getan(&arg7,&type7,7);
	if (type7 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg7,type7))
			PI_FAIL;
	PI_getan(&arg8,&type8,8);
	if (type8 == PI_SYM)
		arg8 = (unsigned long) PI_getsymname(0,arg8,0);
	else if (!CI_get_integer((unsigned long *)&arg8,type8))
		PI_FAIL;
	PI_getan(&arg9,&type9,9);
	if (type9 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg9,type9))
			PI_FAIL;
	PI_getan(&arg10,&type10,10);
	if (type10 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg10,type10))
			PI_FAIL;
	PI_getan(&arg11,&type11,11);
	if (type11 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg11,type11))
			PI_FAIL;
	PI_getan(&arg12,&type12,12);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) SQLSpecialColumns(((HSTMT  ) arg2),((UWORD  ) arg3),((UCHAR * ) arg4),((SWORD  ) arg5),((UCHAR * ) arg6),((SWORD  ) arg7),((UCHAR * ) arg8),((SWORD  ) arg9),((UWORD  ) arg10),((UWORD  ) arg11));
			break;
		default:
			PI_FAIL;
	}
	PI_makedouble(&rval,&rtype,(double) retval);
	if (PI_unify(arg12,type12,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int odbc_dispatch13(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;
	PWord arg5; int type5;
	PWord arg6; int type6;
	PWord arg7; int type7;
	PWord arg8; int type8;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 == PI_SYM)
		arg3 = (unsigned long) PI_getsymname(0,arg3,0);
	else if (!CI_get_integer((unsigned long *)&arg3,type3))
		PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 == PI_SYM)
		arg5 = (unsigned long) PI_getsymname(0,arg5,0);
	else if (!CI_get_integer((unsigned long *)&arg5,type5))
		PI_FAIL;
	PI_getan(&arg6,&type6,6);
	if (type6 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg6,type6))
			PI_FAIL;
	PI_getan(&arg7,&type7,7);
	if (type7 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg7,type7))
			PI_FAIL;
	PI_getan(&arg8,&type8,8);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) SQLNativeSql(((HDBC  ) arg2),((UCHAR * ) arg3),((SDWORD  ) arg4),((UCHAR * ) arg5),((SDWORD  ) arg6),((SDWORD * ) arg7));
			break;
		case 1:
			retval = (unsigned long) SQLBrowseConnect(((HDBC  ) arg2),((UCHAR * ) arg3),((SWORD  ) arg4),((UCHAR * ) arg5),((SWORD  ) arg6),((SWORD * ) arg7));
			break;
		default:
			PI_FAIL;
	}
	PI_makedouble(&rval,&rtype,(double) retval);
	if (PI_unify(arg8,type8,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int odbc_dispatch14(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;
	PWord arg5; int type5;
	PWord arg6; int type6;
	PWord arg7; int type7;
	PWord arg8; int type8;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg5,type5))
			PI_FAIL;
	PI_getan(&arg6,&type6,6);
	if (type6 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg6,type6))
			PI_FAIL;
	PI_getan(&arg7,&type7,7);
	if (type7 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg7,type7))
			PI_FAIL;
	PI_getan(&arg8,&type8,8);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) SQLDescribeParam(((HSTMT  ) arg2),((UWORD  ) arg3),((SWORD * ) arg4),((UDWORD * ) arg5),((SWORD * ) arg6),((SWORD * ) arg7));
			break;
		case 1:
			retval = (unsigned long) SQLGetData(((HSTMT  ) arg2),((UWORD  ) arg3),((SWORD  ) arg4),((PTR  ) arg5),((SDWORD  ) arg6),((SDWORD * ) arg7));
			break;
		case 2:
			retval = (unsigned long) SQLBindCol(((HSTMT  ) arg2),((UWORD  ) arg3),((SWORD  ) arg4),((PTR  ) arg5),((SDWORD  ) arg6),((SDWORD * ) arg7));
			break;
		default:
			PI_FAIL;
	}
	PI_makedouble(&rval,&rtype,(double) retval);
	if (PI_unify(arg8,type8,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int odbc_dispatch15(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;
	PWord arg5; int type5;
	PWord arg6; int type6;
	PWord arg7; int type7;
	PWord arg8; int type8;
	PWord arg9; int type9;
	PWord arg10; int type10;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg5,type5))
			PI_FAIL;
	PI_getan(&arg6,&type6,6);
	if (type6 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg6,type6))
			PI_FAIL;
	PI_getan(&arg7,&type7,7);
	if (type7 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg7,type7))
			PI_FAIL;
	PI_getan(&arg8,&type8,8);
	if (type8 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg8,type8))
			PI_FAIL;
	PI_getan(&arg9,&type9,9);
	if (type9 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg9,type9))
			PI_FAIL;
	PI_getan(&arg10,&type10,10);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) SQLSetParam(((HSTMT  ) arg2),((UWORD  ) arg3),((SWORD  ) arg4),((SWORD  ) arg5),((UDWORD  ) arg6),((SWORD  ) arg7),((PTR  ) arg8),((SDWORD * ) arg9));
			break;
		default:
			PI_FAIL;
	}
	PI_makedouble(&rval,&rtype,(double) retval);
	if (PI_unify(arg10,type10,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int odbc_dispatch16(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;
	PWord arg5; int type5;
	PWord arg6; int type6;
	PWord arg7; int type7;
	PWord arg8; int type8;
	PWord arg9; int type9;
	PWord arg10; int type10;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 == PI_SYM)
		arg4 = (unsigned long) PI_getsymname(0,arg4,0);
	else if (!CI_get_integer((unsigned long *)&arg4,type4))
		PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg5,type5))
			PI_FAIL;
	PI_getan(&arg6,&type6,6);
	if (type6 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg6,type6))
			PI_FAIL;
	PI_getan(&arg7,&type7,7);
	if (type7 == PI_SYM)
		arg7 = (unsigned long) PI_getsymname(0,arg7,0);
	else if (!CI_get_integer((unsigned long *)&arg7,type7))
		PI_FAIL;
	PI_getan(&arg8,&type8,8);
	if (type8 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg8,type8))
			PI_FAIL;
	PI_getan(&arg9,&type9,9);
	if (type9 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg9,type9))
			PI_FAIL;
	PI_getan(&arg10,&type10,10);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) SQLDrivers(((HENV  ) arg2),((UWORD  ) arg3),((UCHAR * ) arg4),((SWORD  ) arg5),((SWORD * ) arg6),((UCHAR * ) arg7),((SWORD  ) arg8),((SWORD * ) arg9));
			break;
		case 1:
			retval = (unsigned long) SQLDataSources(((HENV  ) arg2),((UWORD  ) arg3),((UCHAR * ) arg4),((SWORD  ) arg5),((SWORD * ) arg6),((UCHAR * ) arg7),((SWORD  ) arg8),((SWORD * ) arg9));
			break;
		default:
			PI_FAIL;
	}
	PI_makedouble(&rval,&rtype,(double) retval);
	if (PI_unify(arg10,type10,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int odbc_dispatch17(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;
	PWord arg5; int type5;
	PWord arg6; int type6;
	PWord arg7; int type7;
	PWord arg8; int type8;
	PWord arg9; int type9;
	PWord arg10; int type10;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 == PI_SYM)
		arg4 = (unsigned long) PI_getsymname(0,arg4,0);
	else if (!CI_get_integer((unsigned long *)&arg4,type4))
		PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg5,type5))
			PI_FAIL;
	PI_getan(&arg6,&type6,6);
	if (type6 == PI_SYM)
		arg6 = (unsigned long) PI_getsymname(0,arg6,0);
	else if (!CI_get_integer((unsigned long *)&arg6,type6))
		PI_FAIL;
	PI_getan(&arg7,&type7,7);
	if (type7 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg7,type7))
			PI_FAIL;
	PI_getan(&arg8,&type8,8);
	if (type8 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg8,type8))
			PI_FAIL;
	PI_getan(&arg9,&type9,9);
	if (type9 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg9,type9))
			PI_FAIL;
	PI_getan(&arg10,&type10,10);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) SQLDriverConnect(((HDBC  ) arg2),((HWND  ) arg3),((UCHAR * ) arg4),((SWORD  ) arg5),((UCHAR * ) arg6),((SWORD  ) arg7),((SWORD * ) arg8),((UWORD  ) arg9));
			break;
		default:
			PI_FAIL;
	}
	PI_makedouble(&rval,&rtype,(double) retval);
	if (PI_unify(arg10,type10,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int odbc_dispatch18(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;
	PWord arg5; int type5;
	PWord arg6; int type6;
	PWord arg7; int type7;
	PWord arg8; int type8;
	PWord arg9; int type9;
	PWord arg10; int type10;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 == PI_SYM)
		arg5 = (unsigned long) PI_getsymname(0,arg5,0);
	else if (!CI_get_integer((unsigned long *)&arg5,type5))
		PI_FAIL;
	PI_getan(&arg6,&type6,6);
	if (type6 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg6,type6))
			PI_FAIL;
	PI_getan(&arg7,&type7,7);
	if (type7 == PI_SYM)
		arg7 = (unsigned long) PI_getsymname(0,arg7,0);
	else if (!CI_get_integer((unsigned long *)&arg7,type7))
		PI_FAIL;
	PI_getan(&arg8,&type8,8);
	if (type8 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg8,type8))
			PI_FAIL;
	PI_getan(&arg9,&type9,9);
	if (type9 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg9,type9))
			PI_FAIL;
	PI_getan(&arg10,&type10,10);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) SQLError(((HENV  ) arg2),((HDBC  ) arg3),((HSTMT  ) arg4),((UCHAR * ) arg5),((SDWORD * ) arg6),((UCHAR * ) arg7),((SWORD  ) arg8),((SWORD * ) arg9));
			break;
		default:
			PI_FAIL;
	}
	PI_makedouble(&rval,&rtype,(double) retval);
	if (PI_unify(arg10,type10,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int odbc_dispatch19(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;
	PWord arg5; int type5;
	PWord arg6; int type6;
	PWord arg7; int type7;
	PWord arg8; int type8;
	PWord arg9; int type9;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 == PI_SYM)
		arg3 = (unsigned long) PI_getsymname(0,arg3,0);
	else if (!CI_get_integer((unsigned long *)&arg3,type3))
		PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 == PI_SYM)
		arg5 = (unsigned long) PI_getsymname(0,arg5,0);
	else if (!CI_get_integer((unsigned long *)&arg5,type5))
		PI_FAIL;
	PI_getan(&arg6,&type6,6);
	if (type6 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg6,type6))
			PI_FAIL;
	PI_getan(&arg7,&type7,7);
	if (type7 == PI_SYM)
		arg7 = (unsigned long) PI_getsymname(0,arg7,0);
	else if (!CI_get_integer((unsigned long *)&arg7,type7))
		PI_FAIL;
	PI_getan(&arg8,&type8,8);
	if (type8 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg8,type8))
			PI_FAIL;
	PI_getan(&arg9,&type9,9);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) SQLTablePrivileges(((HSTMT  ) arg2),((UCHAR * ) arg3),((SWORD  ) arg4),((UCHAR * ) arg5),((SWORD  ) arg6),((UCHAR * ) arg7),((SWORD  ) arg8));
			break;
		case 1:
			retval = (unsigned long) SQLProcedures(((HSTMT  ) arg2),((UCHAR * ) arg3),((SWORD  ) arg4),((UCHAR * ) arg5),((SWORD  ) arg6),((UCHAR * ) arg7),((SWORD  ) arg8));
			break;
		case 2:
			retval = (unsigned long) SQLPrimaryKeys(((HSTMT  ) arg2),((UCHAR * ) arg3),((SWORD  ) arg4),((UCHAR * ) arg5),((SWORD  ) arg6),((UCHAR * ) arg7),((SWORD  ) arg8));
			break;
		case 3:
			retval = (unsigned long) SQLConnect(((HDBC  ) arg2),((UCHAR * ) arg3),((SWORD  ) arg4),((UCHAR * ) arg5),((SWORD  ) arg6),((UCHAR * ) arg7),((SWORD  ) arg8));
			break;
		default:
			PI_FAIL;
	}
	PI_makedouble(&rval,&rtype,(double) retval);
	if (PI_unify(arg9,type9,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int odbc_dispatch20(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;
	PWord arg5; int type5;
	PWord arg6; int type6;
	PWord arg7; int type7;
	PWord arg8; int type8;
	PWord arg9; int type9;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg5,type5))
			PI_FAIL;
	PI_getan(&arg6,&type6,6);
	if (type6 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg6,type6))
			PI_FAIL;
	PI_getan(&arg7,&type7,7);
	if (type7 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg7,type7))
			PI_FAIL;
	PI_getan(&arg8,&type8,8);
	if (type8 != PI_INT)
		if (!CI_get_integer((unsigned long *)&arg8,type8))
			PI_FAIL;
	PI_getan(&arg9,&type9,9);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) SQLColAttributes(((HSTMT  ) arg2),((UWORD  ) arg3),((UWORD  ) arg4),((PTR  ) arg5),((SWORD  ) arg6),((SWORD * ) arg7),((SDWORD * ) arg8));
			break;
		default:
			PI_FAIL;
	}
	PI_makedouble(&rval,&rtype,(double) retval);
	if (PI_unify(arg9,type9,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}



PI_BEGIN
	PI_PDEFINE("odbc0",7,odbc_dispatch0,"_odbc_dispatch0")
	PI_PDEFINE("odbc1",6,odbc_dispatch1,"_odbc_dispatch1")
	PI_PDEFINE("odbc2",6,odbc_dispatch2,"_odbc_dispatch2")
	PI_PDEFINE("odbc3",5,odbc_dispatch3,"_odbc_dispatch3")
	PI_PDEFINE("odbc4",5,odbc_dispatch4,"_odbc_dispatch4")
	PI_PDEFINE("odbc5",11,odbc_dispatch5,"_odbc_dispatch5")
	PI_PDEFINE("odbc6",11,odbc_dispatch6,"_odbc_dispatch6")
	PI_PDEFINE("odbc7",11,odbc_dispatch7,"_odbc_dispatch7")
	PI_PDEFINE("odbc8",4,odbc_dispatch8,"_odbc_dispatch8")
	PI_PDEFINE("odbc9",3,odbc_dispatch9,"_odbc_dispatch9")
	PI_PDEFINE("odbc10",15,odbc_dispatch10,"_odbc_dispatch10")
	PI_PDEFINE("odbc11",12,odbc_dispatch11,"_odbc_dispatch11")
	PI_PDEFINE("odbc12",12,odbc_dispatch12,"_odbc_dispatch12")
	PI_PDEFINE("odbc13",8,odbc_dispatch13,"_odbc_dispatch13")
	PI_PDEFINE("odbc14",8,odbc_dispatch14,"_odbc_dispatch14")
	PI_PDEFINE("odbc15",10,odbc_dispatch15,"_odbc_dispatch15")
	PI_PDEFINE("odbc16",10,odbc_dispatch16,"_odbc_dispatch16")
	PI_PDEFINE("odbc17",10,odbc_dispatch17,"_odbc_dispatch17")
	PI_PDEFINE("odbc18",10,odbc_dispatch18,"_odbc_dispatch18")
	PI_PDEFINE("odbc19",9,odbc_dispatch19,"_odbc_dispatch19")
	PI_PDEFINE("odbc20",9,odbc_dispatch20,"_odbc_dispatch20")
PI_END

void odbc_compact_init(void)
{
	PI_INIT;
}
