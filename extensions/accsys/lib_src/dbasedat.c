/**************
 * DBASEDAT.C *
 **************/

/************************************************************
*                                                           *
* Copyright 1989 - 1991, Billy Shakespeare & Company, Inc.  *
* All rights reserved.                                      *
*                                                           *
*************************************************************
*                                                           *
* Published by                                              *
*        Copia International, Inc.                          *
*        Wheaton, Illinois                                  *
*        U. S. A.                                           *
*                                                           *
*************************************************************/

#include <stdlib.h>
#include "db4.h"
#include "d4def.h"

/**** ILYAS ****/
#define D_VERSION "1.03"  /* version control variable: was 1.03A */
/**** ILYAS ****/

#ifndef ACDLL
char dversion[16] = D_VERSION; /* version control variable: was 1.03A */
int  d_report = 0; /* AccSys/dBASE error reporter */
int dretcode = 0;  /* error code from
                        dDopen, dNopen, dXopen, dXactidx, dTopen, dTopen3 */
unsigned short d_request = 0;
int d_blksiz = 1;
long d_recno;
char _dtmpdir[ACTMPLEN]; /* [85] */
char _dminbuf[120];
#ifdef _WINDOWS
char _dWINbuf[256];
#endif

#else /* ACDLL */

DBGDAT dbgdata =
{
	D_VERSION, /* version control variable: was 1.03A */
	0, 	/* int  d_report = 0; -- AccSys/dBASE error reporter */
	0,	/* int dretcode = 0;  -- error code from
                        dDopen, dNopen, dXopen, dXactidx, dTopen, dTopen3 */
	0,	/* unsigned short d_request = 0; */
	1,	/* int d_blksiz = 1; */
	0L,	/* long d_recno; */
};
#endif  /* end of #ifndef ACDLL.... */
