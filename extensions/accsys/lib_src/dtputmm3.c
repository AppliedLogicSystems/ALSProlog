/**************
 * dTputmm3.c *
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

#include "db4.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "d4def.h"
 
#ifdef unix
#include <sys/types.h>
#include <unistd.h>
#endif  /* unix */

#ifdef STANDARD
#include <io.h>
#else
#include <fcntl.h>
#endif

#ifdef ACDLL
#undef d_report
#define d_report pdbt->pdbg->d_report
#endif

int DECLARE dTputmm3(
#ifndef unix 
  /* d_report arrangement: 2280 */
	/* inputs */
	DBT  dbtptr,
	CHAR_PTR memobuff,

	/* output */
	CHAR_PTR memofield)
#else  /* unix */ 
	
 	dbtptr, memobuff, memofield ) 
	DBT dbtptr; 
	CHAR_PTR memobuff; 
	CHAR_PTR memofield; 
#endif /* unix */ 

{
#define K16 16384
int rc;
unsigned wtlen, memosize, memolen;
int iii;
CHAR_PTR bufptr;

if (pdbt->flag != OPEN)
{
    d_report = 2281;
    return(dNOOPEN);
}
if (pdbt->version != 3)
{
    d_report = 2282;
    return(dILLEGAL);
}

iii = pdbt->handle;   /* short hand setup */

memosize = memolen = ACstrlen(memobuff);
if (!memolen)
{
    d_report = 2283;
    return(dOUTRANGE);
}


if (lseek(iii, (long) pdbt->freehead << SNODSHIFT, SEEK_SET) == (long) -1)
{
	d_report = 2284;
	rc = dIOERR;
	goto done;
}

while(memolen)
{
    wtlen = (memolen > K16) ? K16 : memolen;
    if (_ACwrite(iii, memobuff, wtlen) != wtlen)
    {
	d_report = 2285;
	rc = dIOERR;
	goto done;
    }
    memobuff += wtlen;
    memolen -= wtlen;
} /* end of while-loop */

bufptr = pdbt->dbtbuf; /* short hand setup */
*bufptr++ = 0x1a;
*bufptr++ = 0x1a;
if (_ACwrite(iii, pdbt->dbtbuf, 2) != 2)
{
	d_report = 2286;
	rc = dIOERR;
	goto done;
}
memosize++;

/* copy the memo field pointer */
bufptr = pdbt->dbtbuf;
(void) ACltoa(pdbt->freehead, bufptr, 10);
iii = ACstrlen(bufptr);
if (iii < 10)
{
    for (rc = 10 - iii; rc; rc--) *memofield++ = '0';
}
while (iii-- > 0) *memofield++ = *bufptr++;

pdbt->freehead += (memosize / SNODSIZ) + 1;

rc = SUCCESS;

done:


return(rc);

} /* end of dTputmm3() */

