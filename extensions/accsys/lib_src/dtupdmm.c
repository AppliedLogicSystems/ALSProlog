/*************
 * dTupdmm.c *
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

int DECLARE dTupdmm(
#ifndef unix 

				/* d_report arrangement: 2160 */
	/* inputs */
	DBT   dbtptr,
	long  length,
	CHAR_PTR memobuff,

	/* input & output */
	CHAR_PTR memofield,

	/* output */
	INT_PTR setnew)
#else  /* unix */ 
	
 	dbtptr, length, memobuff, memofield, setnew ) 
	DBT dbtptr; 
	long length; 
	CHAR_PTR memobuff; 
	CHAR_PTR memofield; 
	INT_PTR setnew; 
#endif /* unix */ 
 /* 1: memofield changed  0: no changed */
{
int rc;
unsigned iosize;
long size;
long origused, newneeds; /* # of nodes required for memo */
long orignode;
int handle;
CHAR_PTR bufptr;
long lll;
CHAR_PTR memoptr;

*setnew = 0;

if (pdbt->flag != OPEN)
{
    d_report = 2161;
    return(dNOOPEN);
}
if (pdbt->version != 4)
{
    d_report = 2162;
    return(dILLEGAL);
}

handle = pdbt->handle;   /* short hand setup */
bufptr = pdbt->dbtbuf; /* short hand setup */

if (length <= (long) 0)
{
    d_report = 2163;
    return(dOUTRANGE);
}

rc = _dTmmsz(dbtptr, memofield, &size);
if (rc != SUCCESS) goto done;

origused = ((size + 7) / pdbt->blksiz) + 1; /* size + 7 == (size + 8) - 1 */
newneeds = ((length + 7) / pdbt->blksiz) + 1; /* x + 7 == x + 8 - 1 */
if (origused == newneeds)
{
    /* use the original node */
    _bytes4(length + 8, bufptr);
    if (lseek(handle, (long) -4, SEEK_CUR) == (long) -1)
    {
	d_report = 2164;
	rc = dIOERR;
	goto done;
    }
    if (_ACwrite(handle, bufptr, 4) != 4)
    {
	d_report = 2165;
	rc = dIOERR;
	goto done;
    }
    while (length > 0)
    {
	iosize = ( (long) MEMOWTSZ <= length ) ? MEMOWTSZ : (unsigned) length;
	if (_ACwrite(handle, memobuff, iosize) != iosize)
	{
	    d_report = 2166;
	    rc = dIOERR;
	    goto done;
	}
	length -= (long) iosize;
	memobuff += iosize;
    }

    rc = SUCCESS;
    goto done;
}

/* this noded too big or too small:  find another one */
/* (1) save original node */
memoptr = memofield;
orignode = (long) 0;
for (rc = 1; rc <= 10; rc++, memoptr++)
{
    orignode = orignode * 10 + (*memoptr - '0');
}

/* (2) put the new contents */
rc = _dtptmem(dbtptr, memobuff, length, length, memofield, &lll);
if (rc != SUCCESS) goto done;

/* (3) put the original node in the free-list */
rc = _dtmmnhd(dbtptr, orignode, origused);
if (rc == SUCCESS) *setnew = 1;

done:


return(rc);
} /* end of dTupdmm() */

