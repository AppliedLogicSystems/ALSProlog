/**************
 * _drewfwd.c *
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

#ifndef ACDLL
int _DECLARE _drewfwd(pindex,pio,which)
#else
int _DECLARE _drewfwd(DBGDAT_PTR dbgptr,
#endif
		/* d_report arrangement: 10280 */
	PINDEX  pindex;
	PADIOBUFF pio;
	int    which;
{
unsigned short offset;
long nodenum;
PXDXNOD nodeptr;
PCOMDAT comptr;
int rc;
CHAR_PTR thisnode, kptr;
int add8or4;
long isnonleaf;

add8or4 = pindex->indexid ? 8 : 4; /* MDX 8; NDX 4 */
comptr = &(pindex->xdxcdata);

pindex->work = pindex->xdxacc;

nodenum = pindex->root;

do
{
    /* pindex->work->nodeid = nodenum; -- done by _grabnod() */

    /* grab node in buffer */
    rc = _dgrbnod(
#ifdef ACDLL
		dbgptr,
#endif
		pindex, pio, pindex->work, nodenum, &nodeptr);
    if (rc != SUCCESS) return(rc);

    if (!(nodeptr->has))
    {
        d_report = 10281;
	rc = (which == REWIND) ? dEOF : dBOF;
		/* This return code looks reversed.  However,
		   d?nxtkey or d?prvkey takes this code better
		   than the other way around. */
	return(rc);
    }

    thisnode = nodeptr->nodebuf;
    offset = comptr->itemlen * (int) _2bytes(thisnode);
    isnonleaf = _4bytes(thisnode + add8or4 + offset);

    if (which == REWIND)
    {
	kptr = thisnode + add8or4;
	pindex->work->offset = isnonleaf ? 0 : -(comptr->itemlen);
    }
    else
    {
	kptr = thisnode + add8or4 + offset;
	pindex->work->offset = offset;
    }

    nodenum = _4bytes(kptr);

    pindex->work++;

} while(isnonleaf);

pindex->work--;
pindex->leaf = pindex->work;

pindex->position = (which == REWIND) ? TOP : BOTTOM;
return(SUCCESS);
} /* end of _drewfwd() */
