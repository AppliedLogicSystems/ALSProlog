/**************
 * dTmemsz3.c *
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
#include <stdlib.h>
#include "d4def.h"
#ifdef STANDARD
#include <io.h>
#else
#include <fcntl.h>
#endif

#ifdef ACDLL
#undef d_report
#define d_report pdbt->pdbg->d_report
#endif

int DECLARE dTmemsz3(
#ifndef unix 
  /* d_report arrangement: 2240 */
	/* input */
	DBT   dbtptr,
	CHAR_PTR memofield,

	/* output */
	LONG_PTR size)
#else  /* unix */ 
	
 	dbtptr, memofield, size ) 
	DBT dbtptr; 
	CHAR_PTR memofield; 
	LONG_PTR size; 
#endif /* unix */ 

{
CHAR_PTR buff;
int i, rc;
int readlen;
long memosize;
int loop;

if (pdbt->flag != OPEN)
{
    d_report = 2241;
    return(dNOOPEN);
}
if (pdbt->version != 3)
{
    d_report = 2242;
    return(dILLEGAL);
}

rc = _dmmseek(dbtptr, memofield);
if (rc != SUCCESS) goto done;

memosize = (long) 0;
loop = 1;
while(loop)
{
    readlen = _ACread(pdbt->handle, pdbt->dbtbuf, (unsigned) SNODSIZ);
    if (readlen == 0) break; /* done */
    if (readlen < 0)
    {
	d_report = 2243;
	rc = dIOERR;
	goto done;
    }
    for (i = 1, buff = pdbt->dbtbuf; i <= readlen; i++, buff++)
    {
    	if (*buff == 0x1a || !(*buff))
	{
	    loop = 0;
	    break;     /* done */
	}
	memosize++;
    }
}

*size = memosize;

done:


return(rc);
} /* end of dTmemsz3() */

