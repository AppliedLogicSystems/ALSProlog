/*************
 * dTaddmm.c *
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

int DECLARE dTaddmm(
#ifndef unix 

				/* d_report arrangement: 2000 */
	/* inputs */
	DBT   dbtptr,
	long  length,
	CHAR_PTR memobuff,

	/* input and output */
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
long from, to;
int handle;
CHAR_PTR bufptr;
long lll;
CHAR_PTR memoptr;

*setnew = 0;

if (pdbt->flag != OPEN)
{
    d_report = 2001;
    return(dNOOPEN);
}
if (pdbt->version != 4)
{
    d_report = 2002;
    return(dILLEGAL);
}

handle = pdbt->handle;   /* short hand setup */
bufptr = pdbt->dbtbuf;   /* short hand setup */

if (length <= (long) 0)
{
    d_report = 2003;
    return(dOUTRANGE);
}
rc = _dTmmsz(dbtptr, memofield, &size);
if (rc != SUCCESS) goto done;

origused = ((size + 7) / pdbt->blksiz) + 1; /* size + 7 == (size + 8) - 1 */
newneeds = ((size + length + 7) / pdbt->blksiz) + 1; /* x + 7 == x + 8 - 1 */

if (origused == newneeds)
{
    /* use the original node */
    _bytes4(size + length + 8, bufptr);
    if (lseek(handle, (long) -4, SEEK_CUR) == (long) -1)
    {
	d_report = 2004;
	rc = dIOERR;
	goto done;
    }
    if (_ACwrite(handle, bufptr, 4) != 4)
    {
	d_report = 2005;
	rc = dIOERR;
	goto done;
    }
    if (lseek(handle, size, SEEK_CUR) == (long) -1)
    {
	d_report = 2006;
	rc = dIOERR;
	goto done;
    }
    while (length > 0)
    {
	iosize = ( (long) MEMOWTSZ <= length ) ? MEMOWTSZ : (unsigned) length;
	if (_ACwrite(handle, memobuff, iosize) != iosize)
	{
	    d_report = 2007;
	    rc = dIOERR;
	    goto done;
	}
	length -= (long) iosize;
	memobuff += iosize;
    }

    rc = SUCCESS;
    goto done;
}

/* original node too small or too big :  find another one */
/* (1) put the original node in the free-list */
memoptr = memofield;
from = (long) 0;
for (rc = 1; rc <= 10; rc++, memoptr++)
{
    from = from * 10 + (*memoptr - '0');
}

rc = _dtmmnhd(dbtptr, from, origused);
if (rc != SUCCESS) goto done;

/* (2) reserve a space */
rc = _dtptmem(dbtptr, memobuff, size + length, (long) 0, memofield, &to);
if (rc != SUCCESS) goto done;

/* (3) copy the original memo contents */
from = from * pdbt->blksiz + 8;
to = to * pdbt->blksiz + 8;
lll = size;
while (lll)
{
    iosize = ( (long) pdbt->blksiz <= lll) ?
		pdbt->blksiz : (unsigned) lll;
    if (_dseekrd(handle, from, bufptr, iosize) != SUCCESS)
    {
	d_report = 2008;
	rc = dIOERR;
    }
    if (_dseekwt(handle, to, bufptr, iosize) != SUCCESS)
    {
	d_report = 2009;
	rc = dIOERR;
	goto done;
    }
    lll -= (long) iosize;
    from += iosize;
    to += iosize;
}

/* (4) add the requested contents */
while (length > 0)
{
    iosize = ( (long) MEMOWTSZ <= length ) ? MEMOWTSZ : (unsigned) length;
    if (_ACwrite(handle, memobuff, iosize) != iosize)
    {
	d_report = 2010;
	rc = dIOERR;
	goto done;
    }
    length -= (long) iosize;
    memobuff += iosize;
    to += iosize;
}

if (to > pdbt->dbteof) pdbt->dbteof = to;

*setnew = 1;

done:


return(rc);
} /* end of dTaddmm() */
