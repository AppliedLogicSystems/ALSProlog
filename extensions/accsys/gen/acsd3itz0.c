/*================================================================
          acsd3itz0.c
          --Generated from: acsd3itz.src
          Date: 95/12/6   Time: 19:53:13
		--by ALS Interface Generator

 *===============================================================*/

#include "defs.h"
#include "cinterf.h"
#include "accsys.h"


static int acc_dispatch0(void)
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
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer(&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 == PI_SYM)
		arg3 = (unsigned long) PI_getsymname(0,arg3,0);
	else if (!CI_get_integer(&arg3,type3))
		PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer(&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 != PI_INT)
		if (!CI_get_integer(&arg5,type5))
			PI_FAIL;
	PI_getan(&arg6,&type6,6);
	if (type6 == PI_SYM)
		arg6 = (unsigned long) PI_getsymname(0,arg6,0);
	else if (!CI_get_integer(&arg6,type6))
		PI_FAIL;
	PI_getan(&arg7,&type7,7);
	if (type7 != PI_INT)
		if (!CI_get_integer(&arg7,type7))
			PI_FAIL;
	PI_getan(&arg8,&type8,8);
	if (type8 != PI_INT)
		if (!CI_get_integer(&arg8,type8))
			PI_FAIL;
	PI_getan(&arg9,&type9,9);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) dDfldnm(((DBF  ) arg2),((CHAR_PTR  ) arg3),((INT_PTR  ) arg4),((INT_PTR  ) arg5),((CHAR_PTR  ) arg6),((INT_PTR  ) arg7),((INT_PTR  ) arg8));
			break;
		default:
			PI_FAIL;
	}
	make_number(&rval,&rtype,(double) retval);
	if (PI_unify(arg9,type9,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int acc_dispatch1(void)
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
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer(&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer(&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer(&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 == PI_SYM)
		arg5 = (unsigned long) PI_getsymname(0,arg5,0);
	else if (!CI_get_integer(&arg5,type5))
		PI_FAIL;
	PI_getan(&arg6,&type6,6);
	if (type6 == PI_SYM)
		arg6 = (unsigned long) PI_getsymname(0,arg6,0);
	else if (!CI_get_integer(&arg6,type6))
		PI_FAIL;
	PI_getan(&arg7,&type7,7);
	if (type7 != PI_INT)
		if (!CI_get_integer(&arg7,type7))
			PI_FAIL;
	PI_getan(&arg8,&type8,8);
	if (type8 != PI_INT)
		if (!CI_get_integer(&arg8,type8))
			PI_FAIL;
	PI_getan(&arg9,&type9,9);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) dDfldno(((DBF  ) arg2),((int  ) arg3),((INT_PTR  ) arg4),((CHAR_PTR  ) arg5),((CHAR_PTR  ) arg6),((INT_PTR  ) arg7),((INT_PTR  ) arg8));
			break;
		default:
			PI_FAIL;
	}
	make_number(&rval,&rtype,(double) retval);
	if (PI_unify(arg9,type9,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int acc_dispatch2(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer(&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) dUleap(((int  ) arg2));
			break;
		case 1:
			retval = (unsigned long) dTclose(((DBT  ) arg2));
			break;
		case 2:
			retval = (unsigned long) dDfields(((DBF  ) arg2));
			break;
		case 3:
			retval = (unsigned long) dDreclen(((DBF  ) arg2));
			break;
		case 4:
			retval = (unsigned long) dDflush(((DBF  ) arg2));
			break;
		case 5:
			retval = (unsigned long) dDclose(((DBF  ) arg2));
			break;
		default:
			PI_FAIL;
	}
	make_number(&rval,&rtype,(double) retval);
	if (PI_unify(arg3,type3,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int acc_dispatch3(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 == PI_SYM)
		arg2 = (unsigned long) PI_getsymname(0,arg2,0);
	else if (!CI_get_integer(&arg2,type2))
		PI_FAIL;
	PI_getan(&arg3,&type3,3);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) dTmemuse(((CHAR_PTR  ) arg2));
			break;
		case 1:
			retval = (unsigned long) dTcreat(((CHAR_PTR  ) arg2));
			break;
		case 2:
			retval = (unsigned long) dTchkmm(((CHAR_PTR  ) arg2));
			break;
		default:
			PI_FAIL;
	}
	make_number(&rval,&rtype,(double) retval);
	if (PI_unify(arg3,type3,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int acc_dispatch4(void)
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
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer(&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 == PI_SYM)
		arg3 = (unsigned long) PI_getsymname(0,arg3,0);
	else if (!CI_get_integer(&arg3,type3))
		PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer(&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) dTmemosz(((DBT  ) arg2),((CHAR_PTR  ) arg3),((LONG_PTR  ) arg4));
			break;
		default:
			PI_FAIL;
	}
	make_number(&rval,&rtype,(double) retval);
	if (PI_unify(arg5,type5,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int acc_dispatch5(void)
{
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	double float3;
	PWord arg4; int type4;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer(&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (!CI_get_double(&float3,arg3,type3))
		PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 == PI_SYM)
		arg4 = (unsigned long) PI_getsymname(0,arg4,0);
	else if (!CI_get_integer(&arg4,type4))
		PI_FAIL;


	switch(arg1)
	{
		case 0:
			dUdtonk(((int  ) arg2),((double  ) float3),((CHAR_PTR  ) arg4));
			break;
		default:
			PI_FAIL;
	}
	PI_SUCCEED;
}

static int acc_dispatch6(void)
{
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer(&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer(&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer(&arg4,type4))
			PI_FAIL;


	switch(arg1)
	{
		case 0:
			dUtoday(((INT_PTR  ) arg2),((INT_PTR  ) arg3),((INT_PTR  ) arg4));
			break;
		default:
			PI_FAIL;
	}
	PI_SUCCEED;
}

static int acc_dispatch7(void)
{
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 == PI_SYM)
		arg2 = (unsigned long) PI_getsymname(0,arg2,0);
	else if (!CI_get_integer(&arg2,type2))
		PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 == PI_SYM)
		arg3 = (unsigned long) PI_getsymname(0,arg3,0);
	else if (!CI_get_integer(&arg3,type3))
		PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 == PI_SYM)
		arg4 = (unsigned long) PI_getsymname(0,arg4,0);
	else if (!CI_get_integer(&arg4,type4))
		PI_FAIL;


	switch(arg1)
	{
		case 0:
			dUexpnm(((CHAR_PTR  ) arg2),((CHAR_PTR  ) arg3),((CHAR_PTR  ) arg4));
			break;
		default:
			PI_FAIL;
	}
	PI_SUCCEED;
}

static int acc_dispatch8(void)
{
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 == PI_SYM)
		arg2 = (unsigned long) PI_getsymname(0,arg2,0);
	else if (!CI_get_integer(&arg2,type2))
		PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer(&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 == PI_SYM)
		arg4 = (unsigned long) PI_getsymname(0,arg4,0);
	else if (!CI_get_integer(&arg4,type4))
		PI_FAIL;


	switch(arg1)
	{
		case 0:
			dUatocf(((CHAR_PTR  ) arg2),((int  ) arg3),((CHAR_PTR  ) arg4));
			break;
		default:
			PI_FAIL;
	}
	PI_SUCCEED;
}

static int acc_dispatch9(void)
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
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer(&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer(&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 == PI_SYM)
		arg4 = (unsigned long) PI_getsymname(0,arg4,0);
	else if (!CI_get_integer(&arg4,type4))
		PI_FAIL;
	PI_getan(&arg5,&type5,5);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) dDgetrec(((DBF  ) arg2),((long  ) arg3),((CHAR_PTR  ) arg4));
			break;
		case 1:
			retval = (unsigned long) dDupdrec(((DBF  ) arg2),((long  ) arg3),((CHAR_PTR  ) arg4));
			break;
		case 2:
			retval = (unsigned long) dDinsrec(((DBF  ) arg2),((long  ) arg3),((CHAR_PTR  ) arg4));
			break;
		default:
			PI_FAIL;
	}
	make_number(&rval,&rtype,(double) retval);
	if (PI_unify(arg5,type5,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int acc_dispatch10(void)
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
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 == PI_SYM)
		arg2 = (unsigned long) PI_getsymname(0,arg2,0);
	else if (!CI_get_integer(&arg2,type2))
		PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer(&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer(&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) dDopen(((CHAR_PTR  ) arg2),((int  ) arg3),((int  ) arg4));
			break;
		case 1:
			retval = (unsigned long) dDcreat(((CHAR_PTR  ) arg2),((int  ) arg3),(*(CHAR_PTR **) & arg4));
			break;
		default:
			PI_FAIL;
	}
	make_number(&rval,&rtype,(double) retval);
	if (PI_unify(arg5,type5,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int acc_dispatch11(void)
{
	double retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 == PI_SYM)
		arg2 = (unsigned long) PI_getsymname(0,arg2,0);
	else if (!CI_get_integer(&arg2,type2))
		PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer(&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) dUnftod(((CHAR_PTR  ) arg2),((int  ) arg3));
			break;
		default:
			PI_FAIL;
	}
	make_number(&rval,&rtype,(double) retval);
	if (PI_unify(arg4,type4,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int acc_dispatch12(void)
{
	double retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer(&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 == PI_SYM)
		arg3 = (unsigned long) PI_getsymname(0,arg3,0);
	else if (!CI_get_integer(&arg3,type3))
		PI_FAIL;
	PI_getan(&arg4,&type4,4);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) dUnktod(((int  ) arg2),((CHAR_PTR  ) arg3));
			break;
		default:
			PI_FAIL;
	}
	make_number(&rval,&rtype,(double) retval);
	if (PI_unify(arg4,type4,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int acc_dispatch13(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 == PI_SYM)
		arg2 = (unsigned long) PI_getsymname(0,arg2,0);
	else if (!CI_get_integer(&arg2,type2))
		PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 == PI_SYM)
		arg3 = (unsigned long) PI_getsymname(0,arg3,0);
	else if (!CI_get_integer(&arg3,type3))
		PI_FAIL;
	PI_getan(&arg4,&type4,4);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) dUdftodk(((CHAR_PTR  ) arg2),((CHAR_PTR  ) arg3));
			break;
		case 1:
			retval = (unsigned long) dDcopy(((CHAR_PTR  ) arg2),((CHAR_PTR  ) arg3));
			break;
		default:
			PI_FAIL;
	}
	make_number(&rval,&rtype,(double) retval);
	if (PI_unify(arg4,type4,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int acc_dispatch14(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer(&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer(&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) dDrecsta(((DBF  ) arg2),((long  ) arg3));
			break;
		case 1:
			retval = (unsigned long) dDrclrec(((DBF  ) arg2),((long  ) arg3));
			break;
		case 2:
			retval = (unsigned long) dDrmrec(((DBF  ) arg2),((long  ) arg3));
			break;
		case 3:
			retval = (unsigned long) dDdelrec(((DBF  ) arg2),((long  ) arg3));
			break;
		case 4:
			retval = (unsigned long) dDreccnt(((DBF  ) arg2),((LONG_PTR  ) arg3));
			break;
		default:
			PI_FAIL;
	}
	make_number(&rval,&rtype,(double) retval);
	if (PI_unify(arg4,type4,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int acc_dispatch15(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 == PI_SYM)
		arg2 = (unsigned long) PI_getsymname(0,arg2,0);
	else if (!CI_get_integer(&arg2,type2))
		PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer(&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) dTopen(((CHAR_PTR  ) arg2),((int  ) arg3));
			break;
		case 1:
			retval = (unsigned long) dDbuffs(((CHAR_PTR  ) arg2),((int  ) arg3));
			break;
		default:
			PI_FAIL;
	}
	make_number(&rval,&rtype,(double) retval);
	if (PI_unify(arg4,type4,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int acc_dispatch16(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer(&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 == PI_SYM)
		arg3 = (unsigned long) PI_getsymname(0,arg3,0);
	else if (!CI_get_integer(&arg3,type3))
		PI_FAIL;
	PI_getan(&arg4,&type4,4);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) dDapprec(((DBF  ) arg2),((CHAR_PTR  ) arg3));
			break;
		default:
			PI_FAIL;
	}
	make_number(&rval,&rtype,(double) retval);
	if (PI_unify(arg4,type4,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int acc_dispatch17(void)
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
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer(&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer(&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer(&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 == PI_SYM)
		arg5 = (unsigned long) PI_getsymname(0,arg5,0);
	else if (!CI_get_integer(&arg5,type5))
		PI_FAIL;
	PI_getan(&arg6,&type6,6);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) dU3itodk(((int  ) arg2),((int  ) arg3),((int  ) arg4),((CHAR_PTR  ) arg5));
			break;
		case 1:
			retval = (unsigned long) dU3itodf(((int  ) arg2),((int  ) arg3),((int  ) arg4),((CHAR_PTR  ) arg5));
			break;
		default:
			PI_FAIL;
	}
	make_number(&rval,&rtype,(double) retval);
	if (PI_unify(arg6,type6,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int acc_dispatch18(void)
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
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 == PI_SYM)
		arg2 = (unsigned long) PI_getsymname(0,arg2,0);
	else if (!CI_get_integer(&arg2,type2))
		PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer(&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer(&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 == PI_SYM)
		arg5 = (unsigned long) PI_getsymname(0,arg5,0);
	else if (!CI_get_integer(&arg5,type5))
		PI_FAIL;
	PI_getan(&arg6,&type6,6);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) dUatonf(((CHAR_PTR  ) arg2),((int  ) arg3),((int  ) arg4),((CHAR_PTR  ) arg5));
			break;
		default:
			PI_FAIL;
	}
	make_number(&rval,&rtype,(double) retval);
	if (PI_unify(arg6,type6,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int acc_dispatch19(void)
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
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer(&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 == PI_SYM)
		arg3 = (unsigned long) PI_getsymname(0,arg3,0);
	else if (!CI_get_integer(&arg3,type3))
		PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer(&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 == PI_SYM)
		arg5 = (unsigned long) PI_getsymname(0,arg5,0);
	else if (!CI_get_integer(&arg5,type5))
		PI_FAIL;
	PI_getan(&arg6,&type6,6);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) dUnftonk(((int  ) arg2),((CHAR_PTR  ) arg3),((int  ) arg4),((CHAR_PTR  ) arg5));
			break;
		default:
			PI_FAIL;
	}
	make_number(&rval,&rtype,(double) retval);
	if (PI_unify(arg6,type6,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int acc_dispatch20(void)
{
	unsigned long retval;
	PWord rval; int rtype;
	PWord arg1; int type1;
	PWord arg2; int type2;
	double float2;
	PWord arg3; int type3;
	PWord arg4; int type4;
	PWord arg5; int type5;
	PWord arg6; int type6;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (!CI_get_double(&float2,arg2,type2))
		PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer(&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer(&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 == PI_SYM)
		arg5 = (unsigned long) PI_getsymname(0,arg5,0);
	else if (!CI_get_integer(&arg5,type5))
		PI_FAIL;
	PI_getan(&arg6,&type6,6);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) dUdtonf(((double  ) float2),((int  ) arg3),((int  ) arg4),((CHAR_PTR  ) arg5));
			break;
		default:
			PI_FAIL;
	}
	make_number(&rval,&rtype,(double) retval);
	if (PI_unify(arg6,type6,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int acc_dispatch21(void)
{
	PWord arg1; int type1;
	PWord arg2; int type2;
	PWord arg3; int type3;
	PWord arg4; int type4;
	PWord arg5; int type5;

	PI_getan(&arg1,&type1,1);
	if (type1 != PI_INT)
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 == PI_SYM)
		arg2 = (unsigned long) PI_getsymname(0,arg2,0);
	else if (!CI_get_integer(&arg2,type2))
		PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer(&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer(&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 != PI_INT)
		if (!CI_get_integer(&arg5,type5))
			PI_FAIL;


	switch(arg1)
	{
		case 0:
			dUdkto3i(((CHAR_PTR  ) arg2),((INT_PTR  ) arg3),((INT_PTR  ) arg4),((INT_PTR  ) arg5));
			break;
		case 1:
			dUdfto3i(((CHAR_PTR  ) arg2),((INT_PTR  ) arg3),((INT_PTR  ) arg4),((INT_PTR  ) arg5));
			break;
		default:
			PI_FAIL;
	}
	PI_SUCCEED;
}

static int acc_dispatch22(void)
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
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer(&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 != PI_INT)
		if (!CI_get_integer(&arg3,type3))
			PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer(&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 != PI_INT)
		if (!CI_get_integer(&arg5,type5))
			PI_FAIL;
	PI_getan(&arg6,&type6,6);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) dDdate(((DBF  ) arg2),((INT_PTR  ) arg3),((INT_PTR  ) arg4),((INT_PTR  ) arg5));
			break;
		default:
			PI_FAIL;
	}
	make_number(&rval,&rtype,(double) retval);
	if (PI_unify(arg6,type6,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int acc_dispatch23(void)
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
		if (!CI_get_integer(&arg1,type1))
			PI_FAIL;
	PI_getan(&arg2,&type2,2);
	if (type2 != PI_INT)
		if (!CI_get_integer(&arg2,type2))
			PI_FAIL;
	PI_getan(&arg3,&type3,3);
	if (type3 == PI_SYM)
		arg3 = (unsigned long) PI_getsymname(0,arg3,0);
	else if (!CI_get_integer(&arg3,type3))
		PI_FAIL;
	PI_getan(&arg4,&type4,4);
	if (type4 != PI_INT)
		if (!CI_get_integer(&arg4,type4))
			PI_FAIL;
	PI_getan(&arg5,&type5,5);
	if (type5 != PI_INT)
		if (!CI_get_integer(&arg5,type5))
			PI_FAIL;
	PI_getan(&arg6,&type6,6);
	if (type6 == PI_SYM)
		arg6 = (unsigned long) PI_getsymname(0,arg6,0);
	else if (!CI_get_integer(&arg6,type6))
		PI_FAIL;
	PI_getan(&arg7,&type7,7);


	switch(arg1)
	{
		case 0:
			retval = (unsigned long) dUnktoa(((int  ) arg2),((CHAR_PTR  ) arg3),((int  ) arg4),((int  ) arg5),((CHAR_PTR  ) arg6));
			break;
		case 1:
			retval = (unsigned long) dTgetmm(((DBT  ) arg2),((CHAR_PTR  ) arg3),((long  ) arg4),((long  ) arg5),((CHAR_PTR  ) arg6));
			break;
		default:
			PI_FAIL;
	}
	make_number(&rval,&rtype,(double) retval);
	if (PI_unify(arg7,type7,rval,rtype))
		PI_SUCCEED;
	PI_FAIL;
}



PI_BEGIN
	PI_PDEFINE("acc0",9,acc_dispatch0,"_acc_dispatch0")
	PI_PDEFINE("acc1",9,acc_dispatch1,"_acc_dispatch1")
	PI_PDEFINE("acc2",3,acc_dispatch2,"_acc_dispatch2")
	PI_PDEFINE("acc3",3,acc_dispatch3,"_acc_dispatch3")
	PI_PDEFINE("acc4",5,acc_dispatch4,"_acc_dispatch4")
	PI_PDEFINE("acc5",4,acc_dispatch5,"_acc_dispatch5")
	PI_PDEFINE("acc6",4,acc_dispatch6,"_acc_dispatch6")
	PI_PDEFINE("acc7",4,acc_dispatch7,"_acc_dispatch7")
	PI_PDEFINE("acc8",4,acc_dispatch8,"_acc_dispatch8")
	PI_PDEFINE("acc9",5,acc_dispatch9,"_acc_dispatch9")
	PI_PDEFINE("acc10",5,acc_dispatch10,"_acc_dispatch10")
	PI_PDEFINE("acc11",4,acc_dispatch11,"_acc_dispatch11")
	PI_PDEFINE("acc12",4,acc_dispatch12,"_acc_dispatch12")
	PI_PDEFINE("acc13",4,acc_dispatch13,"_acc_dispatch13")
	PI_PDEFINE("acc14",4,acc_dispatch14,"_acc_dispatch14")
	PI_PDEFINE("acc15",4,acc_dispatch15,"_acc_dispatch15")
	PI_PDEFINE("acc16",4,acc_dispatch16,"_acc_dispatch16")
	PI_PDEFINE("acc17",6,acc_dispatch17,"_acc_dispatch17")
	PI_PDEFINE("acc18",6,acc_dispatch18,"_acc_dispatch18")
	PI_PDEFINE("acc19",6,acc_dispatch19,"_acc_dispatch19")
	PI_PDEFINE("acc20",6,acc_dispatch20,"_acc_dispatch20")
	PI_PDEFINE("acc21",5,acc_dispatch21,"_acc_dispatch21")
	PI_PDEFINE("acc22",6,acc_dispatch22,"_acc_dispatch22")
	PI_PDEFINE("acc23",7,acc_dispatch23,"_acc_dispatch23")
PI_END

void acsd3itz_compact_init(void)
{
	PI_INIT;
}
