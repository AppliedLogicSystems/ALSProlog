/*************
* _dxgetnd.c *
**************/

/***********************************************************
*                                                          *
* Copyright 1989, 1990, Billy Shakespeare & Company, Inc.  *
* All rights reserved.                                     *
*                                                          *
************************************************************
*                                                          *
* Published by                                             *
*        Copia International, Inc.                         *
*        Wheaton, Illinois                                 *
*        U. S. A.                                          *
*                                                          *
************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "db4.h"
#include "d4def.h"
#ifdef STANDARD
#include <io.h>
#else
#include <fcntl.h>
#endif

/* Abstract: Get next MDX node */
/* _dxgetnd() gets the next available node.
   At the same time, it updates the available-list. */

long _DECLARE _dxgetnd(fn,pavailist)
			/* d_report arrangement: 10340 */
	/* inputs */
#ifdef ACDLL
	DBGDAT_PTR dbgptr,
#endif
	int	fn;
	PAVAILIST pavailist;

	/* output: none */
{
#define MINI12 12
char minibuf[MINI12];
long nextnode;
long avail;
int rc;

if (!(pavailist->ghead))
{
    /* no garbage list available; get from eof; no I/O */
    nextnode = pavailist->mdxeof;
    pavailist->mdxeof += pavailist->blksiz;
    pavailist->updated = 1;
    return(nextnode);
}

rc = _dseekrd(fn, (pavailist->ghead << SNODSHIFT) + 4, minibuf, 4);
if (rc != SUCCESS)
{
    d_report = 10341;
    return( (long) rc);
}
nextnode = pavailist->ghead;
pavailist->ghead = _4bytes(minibuf);

rc = _dseekrd(fn, (long) MDXGHEAD, minibuf + 4, 8);
if (rc != SUCCESS)
{
    d_report = 10342;
    return((long) rc);
}
avail = _4bytes(minibuf + 8);
avail -= pavailist->blksiz;
pavailist->gnums = avail;
_bytes4(avail, minibuf + 8);
(void) ACmemcpy(minibuf + 4, minibuf, 4);

rc = _dseekwt(fn, (long) MDXGHEAD, minibuf + 4, 8);
if (rc != SUCCESS)
{
    d_report = 10343;
    return((long) rc);
}

return(nextnode);

} /* end of _dxgetnd() */
