/*================================================================
          acsd3itz3.c
          --Generated from: acsd3itz.src
          Date: 95/12/6   Time: 19:53:16
		--by ALS Interface Generator

 *===============================================================*/

#include "defs.h"
#include "cinterf.h"
#include "accsys.h"


static int acc_gv_dispatch(void)
{
	PWord dispatchVal,dispatchType;
	PWord argVal; int argType;
	PWord retVal; int retType;

	PI_getan(&dispatchVal,&dispatchType,1);
	PI_getan(&argVal,&argType,2);

	if (dispatchType != PI_INT)  PI_FAIL;

	switch( dispatchVal )
	{
		case 0:
		PI_makedouble(&retVal,&retType,(double) (long) dversion);
		break;
		case 1:
		PI_makedouble(&retVal,&retType,(double) (long) d_report);
		break;
		case 2:
		PI_makedouble(&retVal,&retType,(double) (long) dretcode);
		break;
		case 3:
		PI_makedouble(&retVal,&retType,(double) (long) d_blksiz);
		break;
		case 4:
		PI_makedouble(&retVal,&retType,(double) (long) d_request);
		break;
		case 5:
		PI_makedouble(&retVal,&retType,(double) (long) d_recno);
		break;
		default:
			PI_FAIL;
	}

if (!PI_unify(argVal,argType,retVal,retType))
		PI_FAIL;

	PI_SUCCEED;
}

PI_BEGIN
	PI_PDEFINE("acc_gv",2,acc_gv_dispatch,"_acc_gv_dispatch")
PI_END

void acsd3itz4_init(void)
{
	PI_INIT;
}

void acsd3itz_init(void)
{
	acsd3itz_compact_init();
	acsd3itz2_init();
	acsd3itz3_init();
	acsd3itz4_init();

}
