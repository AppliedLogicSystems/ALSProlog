/**************
 * _dputkey.c *
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

#include  <string.h>
#include "db4.h"
#include "d4def.h"

/*
 * The following code inserts a record in the middle of the
 * keys list.
 */
#ifndef ACDLL
int _DECLARE _dputkey(pindex,pio,pfreelst,ndptr,key,recno,offset,
						key1,node1,key2,node2)
#else
int _DECLARE _dputkey(DBGDAT_PTR dbgptr,
#endif
	/* inputs */  /* d_report arrangement: 10240 */
	PINDEX    pindex;
	PADIOBUFF pio;
	PAVAILIST pfreelst; /* null for NDX */
	PXDXNOD   ndptr;
	CHAR_PTR  key;
	long	  recno;
	unsigned short offset;
	/* outputs */
	pCHAR_PTR key1;
	LONG_PTR  node1; /* 1st node number when split */
	pCHAR_PTR key2;
	LONG_PTR  node2; /* 2nd node number when split */
{
PCOMDAT comptr;
CHAR_PTR kptr; /* pointer to key in index */
int rc;
PXDXNOD ndptr2;
PPOTATO potatptr;
unsigned shiftlen;
unsigned half1, half2, whichalf;
int itemsize;
int ismdx, condadd;

*key1 = *key2 = (CHAR_PTR) 0;

ismdx = pindex->indexid;
condadd = ismdx ? 8 : 4;
comptr = &(pindex->xdxcdata);
itemsize = comptr->itemlen;

whichalf = 0; /* no split yet; original buffer */

while (1)
{
    if (ndptr->has < comptr->maxitems)
    {
	pindex->work->offset = itemsize * offset;
	pindex->work->nodeid = ndptr->nodeid;

	kptr = ndptr->nodebuf + 8 + pindex->work->offset;
	if (!ismdx) kptr -= 4;
	if (ndptr->has > 0)
	{
	    /* (1) make a space by shifting down some records */
	    shiftlen = (ndptr->has - (unsigned) offset) * itemsize + 4;
	    (void) ACmemmove(kptr + itemsize, kptr, shiftlen);
	}
	else
	{
	    (void) ACmemset(ndptr->nodebuf, 0, comptr->nodesz);
	}

	/* (2) finally copy the key to buffer */
	(void) ACmemcpy(kptr + (ismdx ? 4 : 8), key,
			(unsigned) pindex->complen);

	/* (3) set node number to zero */
	if (!ismdx) (void) ACmemset(kptr, 0, 4);

	/* (4) set the record number */
	if (ismdx) _bytes4(recno, kptr);
	else	 _bytes4(recno, kptr + 4);

	/* (5) increment key count */
	_bytes4((long) ++(ndptr->has), ndptr->nodebuf);

	ndptr->bufflag = ACWRITE;

	comptr->curnode = ndptr->nodeid;

	kptr = ndptr->nodebuf + condadd
	    	    + (ndptr->has - 1) * itemsize;
	if (whichalf == 2)
	{ /* split occurred */
	    *key2 = kptr;
	    *node2 = ndptr->nodeid;
	}
	else
	{
	    if (whichalf)
	    		/** && ndptr->has == (offset + 1)) **/
	    {
	        *key1 = kptr;
	        *node1 = ndptr->nodeid;
	    }
	}
	return(SUCCESS);
    }

    /*
     * no more space in this node: time to split
     */
    potatptr = _dgetptt( &(pio->mru), &(pio->lru));
    ndptr2 = (PXDXNOD) potatptr->bufptr;

    if (ndptr2->bufflag & ACWRITE)
    {
        rc = _dseekwt(comptr->handle, (long) ndptr2->nodeid << SNODSHIFT,
			ndptr2->nodebuf, (unsigned) comptr->nodesz);
	if (rc != SUCCESS)
	{
	    d_report = 10241;
	    return(rc);
	}
	if (ndptr2->nodeid > pio->lstpnode) pio->lstpnode = ndptr2->nodeid;
    }

    /* initialize new node elements (ndptr->....) */
    if (pfreelst)
    {
	ndptr2->nodeid = _dxgetnd(
#ifdef ACDLL
				dbgptr,
#endif
				comptr->handle, pfreelst);
	if (!(ndptr2->nodeid)) return(dIOERR);
    }
    else
    {
	ndptr2->nodeid = comptr->eofnode++;
    }
    comptr->changed = 1;

    /* split an existing buffer in half */

    half1 = ndptr->has >> 1;
    half2 = ndptr->has - half1;

    if ( (unsigned) offset <= half1)
    {
	whichalf = 1; /* 1st half */
    }
    else
    {
	if (ndptr->has & 1)
	{
	    whichalf = half2; /* whichalf => temp. var */
	    half2 = half1;
	    half1 = whichalf;
	}
	whichalf = 2; /* 2nd half */
	offset -= half1;
    }

    /* adjust count/ptr values in buffers */
    /* (1) 2nd buffer */
    _bytes4((long) half2, ndptr2->nodebuf);
    ndptr2->has = half2;

    /* (2) 1st buffer */
    _bytes4((long) half1, ndptr->nodebuf);
    ndptr->has = half1;

    /* (3) if MDX, fix node chain */
    if (ismdx)
    {
	(void) ACmemcpy(ndptr2->nodebuf + 4, ndptr->nodebuf + 4, 4);
	_bytes4(ndptr2->nodeid, ndptr->nodebuf + 4);
	if (!_4bytes(ndptr2->nodebuf + 4)) pindex->tail = ndptr2->nodeid;
	pindex->bnumglst += pfreelst->blksiz;
    }
    /* (4) copy 2nd half of 1st buffer to 2nd buffer */
    (void) ACmemcpy(ndptr2->nodebuf + condadd,
		  ndptr->nodebuf + condadd + half1 * itemsize,
		  (unsigned) half2 * itemsize);

    /* (5) zero out the end */
    (void) ACmemset(ndptr->nodebuf + condadd + half1 * itemsize,
    		  0, 4);
    (void) ACmemset(ndptr2->nodebuf + condadd + half2 * itemsize,
    		  0, 4);

    ndptr->bufflag = ACWRITE;
    ndptr2->bufflag = ACWRITE;

    if (whichalf == 2)
    {
	*key1 = ndptr->nodebuf + condadd
		+ (ndptr->has - 1) * itemsize;
	*node1 = ndptr->nodeid;

	ndptr = ndptr2; /* go back to top of the
			   while-loop to finish it up */
    }
    else
    {
	*key2 = ndptr2->nodebuf + condadd
		+ (ndptr2->has - 1) * itemsize;
	*node2 = ndptr2->nodeid;
    }

} /* end of while loop */

} /* end of _dputkey() */

