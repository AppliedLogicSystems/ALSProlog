/**************
 * _dnxtkey.c *
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
 
#include <string.h>
#include "db4.h"
#include "d4def.h"

#ifndef ACDLL
int _DECLARE _dnxtkey(pindex,pio,nextkey,recno)
#else
int _DECLARE _dnxtkey(DBGDAT_PTR dbgptr,
#endif
			/* d_report arrangement: 10200 */
	/* inputs */
	PINDEX  pindex;
	PADIOBUFF pio;

	/* outputs */
	CHAR_PTR nextkey;
	LONG_PTR recno;
{
PCOMDAT pxcom;
PXDXNOD nodeptr;
unsigned short nodeofst, offset;
long ixnodeid;
int rc;
int add8or4;
CHAR_PTR kptr;
long isnonleaf;

add8or4 = pindex->indexid ? 8 : 4;
pxcom = &(pindex->xdxcdata);
pindex->work = pindex->leaf;

while (1)
{
    rc = _dgrbnod(
#ifdef ACDLL
		dbgptr,
#endif
		pindex, pio, pindex->work, pindex->work->nodeid, &nodeptr);
    if (rc != SUCCESS) return(rc);

    pindex->work->offset += pxcom->itemlen;
    
    offset = pindex->work->offset;

    nodeofst = pxcom->itemlen * nodeptr->has;

    if (offset < nodeofst)
    {
	kptr = nodeptr->nodebuf + offset + 12;
	(void) ACmemcpy(pindex->curkey, kptr, pindex->complen);
	pindex->currecno = _4bytes(kptr - 4);
	(void) ACmemcpy(nextkey, pindex->curkey, pindex->complen);
	*recno = pindex->currecno; 
	return(SUCCESS);
    }

    /* should be : pindex->work->offset == nodeofst */

    /* climb up in the tree */
    while (2)
    {
	if (--(pindex->work) < pindex->xdxacc)
	{
	    pindex->position = BOTTOM;
            d_report = 10201;
	    return(dEOF);
	}

        /* climbing up to grab... */
	rc = _dgrbnod(
#ifdef ACDLL
			dbgptr,
#endif
			pindex, pio, pindex->work, pindex->work->nodeid,
			&nodeptr);
	if (rc != SUCCESS) return(rc);

	nodeofst = pxcom->itemlen * nodeptr->has;

	pindex->work->offset += pxcom->itemlen;

	if (pindex->work->offset <= nodeofst) break; /* got it */

	/* climb up another step */
    } /* end of while(2) */

    /* go down thru the tree */
    ixnodeid = _4bytes(nodeptr->nodebuf + add8or4 + pindex->work->offset);
    while (3)
    {
	pindex->work++;
	pindex->work->nodeid = ixnodeid;

	rc = _dgrbnod(
#ifdef ACDLL
			dbgptr,
#endif
			pindex, pio, pindex->work, ixnodeid, &nodeptr);
	if (rc != SUCCESS) return(rc);

	rc = pxcom->itemlen * nodeptr->has;

	isnonleaf = _4bytes(nodeptr->nodebuf + add8or4 + rc);

	pindex->work->offset = isnonleaf ? 0 : -(pxcom->itemlen);

	if (!isnonleaf) break;

	ixnodeid = _4bytes(nodeptr->nodebuf + add8or4);
    } /* end of while(3) */

    pindex->leaf = pindex->work;

    /* go back to read the next key from index */
} /* end of while(1) loop */

return(SUCCESS);

} /* end of _dnxtkey() */
