/*************
 * dTnewmm.c *
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

int DECLARE dTnewmm(
#ifndef unix 
  /* d_report arrangement: 2100 */
	/* inputs */
	DBT   dbtptr,
	long  length,
	CHAR_PTR memobuff,

	/* output */
	CHAR_PTR memofield)
#else  /* unix */ 
	
 	dbtptr, length, memobuff, memofield ) 
	DBT dbtptr; 
	long length; 
	CHAR_PTR memobuff; 
	CHAR_PTR memofield; 
#endif /* unix */ 

{
long dummy;
long memoloc;
int i, rc;

memoloc = (long) 0;
for (i=1; i<=10; i++)
{
    if (*memofield < '0' || '9' < *memofield) break;
    memoloc = memoloc * 10 + (*memofield - '0');
    memofield++;
}
if (memoloc)
{
    /* already has some memo */
    d_report = 2101;
    return(dILLEGAL);
}

rc = _dtptmem(dbtptr, memobuff, length, length, memofield, &dummy);

return(rc);
} /* end of dTnewmm() */


int _DECLARE _dtptmem(
#ifndef unix 
  /* d_report arrangement: 2110 */
	/* inputs */
	DBT   dbtptr,
	CHAR_PTR memobuff,
	long  length, /* length to reserve */
	long  writelen, /* actual length to write */
		/* must be: length >= writelen */

	/* output */
	CHAR_PTR memofield,
	LONG_PTR located)
#else  /* unix */ 
	
 	dbtptr, memobuff, length, writelen, memofield, located ) 
	DBT dbtptr; 
	CHAR_PTR memobuff; 
	long length; 
	long writelen; 
	CHAR_PTR memofield; 
	LONG_PTR located; 
#endif /* unix */ 

{
int rc;
unsigned wtsize;
long nextnode;
long nextloc;
long size;
int hiteof;
long origused, newneeds; /* # of nodes requred for memo */
long prevnode, curnode; /* previous & current nodes */
long curloc;   /* current location */
int handle;
CHAR_PTR bufptr;
int i;

if (pdbt->flag != OPEN)
{
    d_report = 2111;
    return(dNOOPEN);
}
if (pdbt->version != 4)
{
    d_report = 2112;
    return(dILLEGAL);
}

handle = pdbt->handle; /* short hand setup */
bufptr = pdbt->dbtbuf; /* short hand setup */

hiteof = 0;
newneeds = ((length + 7) / pdbt->blksiz) + 1;
				/* length + 7 == length + 8 - 1 */
nextnode = pdbt->freehead;
curnode = nextnode;
prevnode = (long) 0;
wtsize = 0;
i = 0; /* i: loop count */
while (1)
{
    i++;

    nextloc = nextnode * pdbt->blksiz;

    if (nextloc >= pdbt->dbteof)
    {
	hiteof = i;
	wtsize = (unsigned) (nextloc - pdbt->dbteof); /* filler size */
	curnode = nextnode;
	break;
    }

    curloc = lseek(handle, nextloc, SEEK_SET);
    if (curloc == (long) -1)
    {
	d_report = 2113;
	return(dIOERR);
    }
    if (_ACread(handle, bufptr, 8) != 8)
    {
	d_report = 2114;
	return(dIOERR);
    }
    /* Note: curloc is not incremented by 8 */

    size = _4bytes(bufptr + 4) - 8;
    origused = ((size + 7) / pdbt->blksiz) + 1;
    				/* size + 7 == (size + 8) - 1 */
    if (origused >= newneeds)
    {
        wtsize = 0; /* no need to fill the gap */
        break; /* found the spot */
    }

    prevnode = nextnode;
    nextnode = _4bytes(bufptr);

} /* end of while-loop */

if (hiteof)
{
    curloc = lseek(handle, (long) 0, SEEK_END);
    if (curloc == (long) -1)
    {
	d_report = 2115;
	return(dIOERR);
    }
    if (wtsize)
    {
	(void) ACmemset(bufptr, 0, wtsize);
	if (_ACwrite(handle, bufptr, wtsize) != wtsize)
	{
	    d_report = 2116;
	    return(dIOERR);
	}
	curloc += wtsize;
    }
}
	/* at this point, the file pointer is placed at the very beginning
	   of the memo */
else
{   /* found a node in the free list */
    /* adjust the free-list chain */
    curnode = nextnode;
    nextnode = _4bytes(bufptr);

    if (lseek(handle, (long) prevnode * pdbt->blksiz,
    	SEEK_SET) == (long) -1)
    {
	d_report = 2117;
	return(dIOERR);
    }
    origused -= newneeds;
    if (origused)
    {   /* last part of current node will be placed in free-list */
        /* update the previous node */
	_bytes4(curnode + newneeds, bufptr);
	if (_ACwrite(handle, bufptr, 4) != 4)
	{
	    d_report = 2118;
	    return(dIOERR);
	}

	/* then, update the left-over of the current node */
	_bytes4(nextnode, bufptr);
	_bytes4(origused, bufptr + 4);
	if (_dseekwt(handle,
		(long) (curnode + newneeds) * pdbt->blksiz,
		bufptr, 8) != SUCCESS)
	{
	    d_report = 2119;
	    return(dIOERR);
	}
    }
    else
    {   /* use the entire section of the previous memo */
        /* needs to update the pointer only in the previous node */
        _bytes4(nextnode, bufptr);
	if (_ACwrite(handle, bufptr, 4) != 4)
	{
	    d_report = 2120;
	    return(dIOERR);
	}
    }
}

/* reposition the file to the new memo location and write the memo header */
*bufptr = 0xff;
*(bufptr + 1) = 0xff;
*(bufptr + 2) = 0x08;
*(bufptr + 3) = 0x00;
_bytes4(length + 8, bufptr + 4);
if (_dseekwt(handle, curloc, bufptr, 8) != SUCCESS)
{
    d_report = 2121;
    return(dIOERR);
}
curloc += 8;

while (writelen > 0)
{
    wtsize = ((long) MEMOWTSZ <= writelen) ? MEMOWTSZ : (unsigned) writelen;
    if (_ACwrite(handle, memobuff, wtsize) != wtsize)
    {
	d_report = 2122;
	return(dIOERR);
    }
    writelen -= (long) wtsize;
    memobuff += wtsize;
    curloc += wtsize;
}

if (curloc > pdbt->dbteof) pdbt->dbteof = curloc;

/* copy the memo field pointer */
#ifndef unix
#ifdef STANDARD
(void) ACltoa(curnode, bufptr, 10);
#else
(void) stcl_d(bufptr, curnode);
#endif
#else /* unix */
(void) ACltoa(curnode, bufptr, 10);
#endif /* unix */

i = ACstrlen(bufptr);

if (i < 10)
{
    for (rc = 10 - i; rc; rc--) *memofield++ = '0';
}
while (i-- > 0) *memofield++ = *bufptr++;

*located = curnode;

/* finally adjust the free list */
if (hiteof)
{
    _bytes4(curnode + newneeds, pdbt->dbtbuf);
    pdbt->freehead = curnode + newneeds;
    if (_dseekwt(handle,
    		 (long) prevnode * pdbt->blksiz,
		 pdbt->dbtbuf, 4) != SUCCESS)
    {
        d_report = 2123;
        return(dIOERR);
    }
}

return(SUCCESS);

} /* end of _dtptmem() */

