/**************
 * _dgrbnod.c *
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

/* general routine to grab a node from either existing buffer or
   disk.
   <outputs>
	1. return code; SUCCESS or failure code
	2. work->nodeid <- nodenum
	3. node buffer (*retnode)
*/
#ifndef ACDLL
int _DECLARE _dgrbnod(pindex,pio,work,nodenum,retnode)
#else
int _DECLARE _dgrbnod(DBGDAT_PTR dbgptr,
#endif
	/* inputs */		/* d_report arrangement: 10080 */
	PINDEX pindex;
	PADIOBUFF pio;
	PXDXVEC work;
	long   nodenum;
	/* output */
	pPXDXNOD retnode;
{
PCOMDAT comptr;
PPOTATO potatptr;
PXDXNOD nodeptr;
int i, j;

comptr = &(pindex->xdxcdata);
/* check existing buffer */
for (i = -1; i < (int) pio->potatnum; i++)
{
    if (i == -1)
    {
	if (!work->nodeid) continue;
        j = (int) work->potatoid;
	if (j == (int) NIL) continue;
    }
    else
    {
	j = i;
    }
    potatptr = pio->potatoes + j;
    nodeptr = (PXDXNOD) potatptr->bufptr;
    if ( nodenum == nodeptr->nodeid)
    {
	_dcycptt( potatptr, &(pio->mru), &(pio->lru));
	work->nodeid = nodenum;
	*retnode = nodeptr;
	return(SUCCESS);
    }
} /* end of for-loop */

potatptr = _dgetptt( &(pio->mru), &(pio->lru));
nodeptr = (PXDXNOD) potatptr->bufptr;

if (nodeptr->bufflag & ACWRITE)
{
    i = _dseekwt(comptr->handle, (long) nodeptr->nodeid << SNODSHIFT,
		 nodeptr->nodebuf, (unsigned) comptr->nodesz);
    if (i != SUCCESS)
    {
	d_report = 10081;
	return(i);
    }
    if (nodeptr->nodeid > pio->lstpnode) pio->lstpnode = nodeptr->nodeid;
}
if (nodenum <= pio->lstpnode)
{
    i = _dseekrd(comptr->handle, (long) nodenum << SNODSHIFT,
		 nodeptr->nodebuf, (unsigned) comptr->nodesz);
    if (i != SUCCESS)
    {
	d_report = 10082;
	return(i);
    }
    nodeptr->has = (int) _2bytes(nodeptr->nodebuf);
}
else
{
    nodeptr->has = 0;
}
work->nodeid = nodenum;
work->potatoid = potatptr - pio->potatoes;
/* work->offset: set by calling function */

nodeptr->nodeid = nodenum;
nodeptr->bufflag = ACREAD;

*retnode = nodeptr;
return(SUCCESS);
} /* end of _dgrbnod() */
