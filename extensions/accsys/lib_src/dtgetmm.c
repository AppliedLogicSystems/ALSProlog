/*************
 * dTgetmm.c *
 *************/

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

int DECLARE dTgetmm(
#ifndef unix 

				/* d_report arrangement: 2060 */
	/* inputs */
	DBT   dbtptr,
	CHAR_PTR memofield,
	long offset,
	long length, /* if > physical size, ignored */

	/* output */
	CHAR_PTR memobuff)
#else  /* unix */ 
	
 	dbtptr, memofield, offset, length, memobuff ) 
	DBT dbtptr; 
	CHAR_PTR memofield; 
	long offset; 
	long length; 
	CHAR_PTR memobuff; 
#endif /* unix */ 

{
int rc;
long size;
unsigned rdsize;

if (pdbt->flag != OPEN)
{
    d_report = 2061;
    return(dNOOPEN);
}

if (pdbt->version != 4)
{
    d_report = 2062;
    return(dILLEGAL);
}

if (offset < (long) 0)
{
    d_report = 2063;
    return(dOUTRANGE);
}


rc = _dTmmsz(dbtptr, memofield, &size);
if (rc != SUCCESS) goto done;
if (!size) goto done;
if (offset >= size)
{
    d_report = 2064;
    rc = dOUTRANGE;
    goto done;
}

/* at this point, the file pointer is placed at the very beginning
   of the memo */

if (offset)
{
    if (lseek(pdbt->handle, offset, SEEK_CUR) == (long) -1)
    {
	d_report = 2065;
	rc = dIOERR;
	goto done;
    }
    size -= offset; /* actual size to read */
}

if (length < size) size = length; /* requested size < actual size */

while (size > (long) 0)
{
    rdsize = ( (long) pdbt->blksiz <= size ) ?
    			pdbt->blksiz : (unsigned) size;
    if (_ACread(pdbt->handle, memobuff, rdsize) != rdsize)
    {
	d_report = 2066;
	rc = dIOERR;
	goto done;
    }
    size -= (long) rdsize;
    memobuff += rdsize;
}

*memobuff = (char) 0;

done:


return(rc);
} /* end of dTgetmm() */

