/**************
 * dTgetmm3.c *
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

int DECLARE dTgetmm3(
#ifndef unix 

				/* d_report arrangement: 2220 */
	/* inputs */
	DBT   dbtptr,
	CHAR_PTR memofield,

	/* output */
	CHAR_PTR memobuff)
#else  /* unix */ 
	
 	dbtptr, memofield, memobuff ) 
	DBT dbtptr; 
	CHAR_PTR memofield; 
	CHAR_PTR memobuff; 
#endif /* unix */ 

{
int i, rc;
int readlen;
CHAR_PTR buff;
int loop;

if (pdbt->flag != OPEN)
{
    d_report = 2221;
    return(dNOOPEN);
}

if (pdbt->version != 3)
{
    d_report = 2222;
    return(dILLEGAL);
}


rc = _dmmseek(dbtptr, memofield);
if (rc != SUCCESS) goto done;

loop = 1;
while(loop)
{
    readlen = _ACread(pdbt->handle, pdbt->dbtbuf, (unsigned) SNODSIZ);
    if (readlen == 0) break; /* done */
    if (readlen < 0)
    {
	d_report = 2223;
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
	*memobuff++ = *buff;
    }
}

*memobuff = (char) 0;

rc = SUCCESS;

done:


return(rc);

} /* end of dTgetmm3() */

