/**************
 * _dtmmutl.c *
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

#ifdef ACDLL
#undef d_report
#define d_report pdbt->pdbg->d_report
#endif

/* put original node in free list */

int _DECLARE _dtmmnhd(dbtptr,newhead,nheadhas)
				/* d_report arrangement: 10320 */
	/* inputs */
	DBT   dbtptr;
	long  newhead;  /* new head node */
	long  nheadhas; /* # of nodes of new head */

	/* output: none */
{
int handle;
CHAR_PTR bufptr;
int rc;

handle = pdbt->handle;   /* short hand setup */
bufptr = pdbt->dbtbuf; /* short hand setup */

/* (1) head-of-free-list <= orignal field location */
rc = _dseekrd(handle, (long) 0, bufptr, 4); /* save old head of free list */
if (rc != SUCCESS)
{
    d_report = 10321;
    return(dIOERR);
}

_bytes4(newhead, bufptr + 4);

rc = _dseekwt(handle, (long) 0, bufptr + 4, 4);
if (rc != SUCCESS)
{
    d_report = 10322;
    return(dIOERR);
}

/* (2) original field location <= old head-of-free-list */
_bytes4(nheadhas, bufptr + 4);
rc = _dseekwt(handle, newhead * pdbt->blksiz, bufptr, 8);
if (rc != SUCCESS)
{
    d_report = 10323;
    return(dIOERR);
}

pdbt->freehead = newhead;

return(SUCCESS);

} /* end of _dtmmnhd() */

