/*================================================================ 
          odbc3.c
          --Generated from: odbc.src
          Date: 1999/4/9   Time: 19:15:53
		--by ALS Interface Generator

 *===============================================================*/

#include "alspi.h"
#include "cinterf.h"
#include "odbc.h"


static int odbc_gv_dispatch(void)
{
	PWord dispatchVal; int dispatchType;
	PWord argVal; int argType;
	PWord retVal; int retType;

	PI_getan(&dispatchVal,&dispatchType,1);
	PI_getan(&argVal,&argType,2);

	if (dispatchType != PI_INT)  PI_FAIL;

	switch( dispatchVal )
	{
		case 0:
		case 1:
		default:
			PI_FAIL;
	}

if (!PI_unify(argVal,argType,retVal,retType))
		PI_FAIL;

	PI_SUCCEED;
}

PI_BEGIN
	PI_PDEFINE("odbc_gv",2,odbc_gv_dispatch,"_odbc_gv_dispatch")
PI_END

void odbc4_init(void)
{
	PI_INIT;
}

extern void odbc_compact_init(void);
extern void odbc2_init(void);
extern void odbc3_init(void);
void odbc_init(void)
{
	odbc_compact_init();
	odbc2_init();
	odbc3_init();
	odbc4_init();

}
