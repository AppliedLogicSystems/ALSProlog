/*************
 * _drmkey.c *
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
 
#include <string.h>
#include "db4.h"
#include "d4def.h"

/*
 * The following code deletes a record in the middle of the
 * records list.
 */
#ifndef ACDLL
int _DECLARE _drmkey(key,recno,pindex,pio,nodeptr,updkey,rmvnode)
#else
int _DECLARE _drmkey(DBGDAT_PTR dbgptr,
#endif
	/* inputs */      /* d_report arrangement: 10300 */
	CHAR_PTR key;
	long recno;
	PINDEX pindex;
	PADIOBUFF pio;
	PXDXNOD nodeptr;

	/* outputs */
	pCHAR_PTR updkey;
	INT_PTR rmvnode;
{
PCOMDAT comptr;
CHAR_PTR kptr; /* pointer to key in NDX */
long   lnum;  /* long integer temporarily used */
unsigned short offset;
int itemsize;
int rc;
unsigned shiftlen;
int add8or4;

*updkey = (CHAR_PTR) 0;
*rmvnode = 0;

add8or4 = (pindex->indexid) ? 8 : 4;
comptr = &(pindex->xdxcdata);
itemsize = comptr->itemlen;

kptr = nodeptr->nodebuf + 8;

lnum = _4bytes(kptr + pindex->work->offset); /* record number */

offset = 0; /* enable first check */
while (1)
{
    if (lnum == recno)
    {
        if (offset) break;
	if ((*(pindex->keycmp))
            (kptr + 4 + pindex->work->offset, key, pindex->complen) == 0)
	{
	    break;
	}
    }

    rc = _dprvkey(
#ifdef ACDLL
		dbgptr,
#endif
		REMOVE, pindex, pio, (CHAR_PTR) 0, &nodeptr, &lnum);
    if (rc == dBOF)
    {
	d_report = 10301;
	return(dNOTFOUND);
    }
    if (rc != SUCCESS) return(rc);

    kptr = nodeptr->nodebuf + 8;
    if ((*(pindex->keycmp))
        (kptr + 4 + pindex->work->offset, key, pindex->complen) != 0)
    {
        d_report = 10302;
	return(dNOTFOUND);
    }
    offset = 0; /* disable first check */
} /* end of while (1) */

offset = pindex->work->offset / itemsize;

nodeptr->has--;

if (nodeptr->has)
{
    /* (1) shift up some keys */
    kptr = nodeptr->nodebuf + add8or4 + (itemsize * offset);
    shiftlen = (nodeptr->has - (unsigned) offset) * itemsize + 4;
    (void) ACmemcpy(kptr, kptr + itemsize, shiftlen);

    /* (2) decrement key count */
    _bytes4((long) nodeptr->has, nodeptr->nodebuf);

    nodeptr->bufflag = ACWRITE;

    comptr->curnode = nodeptr->nodeid;

    if (offset == nodeptr->has)
    {
        (void) ACmemcpy(pindex->curkey,
		      nodeptr->nodebuf + add8or4
		      + (itemsize * (offset - 1)),
		      itemsize);

	*updkey = pindex->curkey;
    }
    return(SUCCESS);
}

/* this node was totally deleted */

comptr->curnode = 0;
(void) ACmemset(pindex->curkey, 0, itemsize);
pindex->currecno = (long) 0;

(void) ACmemset(nodeptr->nodebuf, 0, 4);
rc = _dseekwt(comptr->handle, nodeptr->nodeid << SNODSHIFT,
		nodeptr->nodebuf, 4);
if (rc != SUCCESS)
{
    d_report = 10303;
    return(dIOERR);
}

lnum = nodeptr->nodeid;

nodeptr->nodeid = 0;
nodeptr->bufflag = UNUSED;
/* nodeptr->has = 0; */

if (lnum != pindex->root)
{
	*rmvnode = 1;
	return(SUCCESS);
}

/*** the last key has been removed: clean up & reinitialize ***/
/* reset XDXNOD instances */
for (nodeptr = pio->xnodes, rc = 1;
     rc <= pio->potatnum; nodeptr++, rc++)
{
    nodeptr->nodeid = 0;
    nodeptr->bufflag = UNUSED;
    nodeptr->has = 0;
}

/* reset index entry hash vector */
for (rc = 0; rc < ACCESSSZ; rc++)
{
    pindex->xdxacc[rc].nodeid = (unsigned short) 0;
    pindex->xdxacc[rc].potatoid = NIL;
    pindex->xdxacc[rc].offset = (unsigned short) 0;
}

if (!(pindex->indexid))
{
	rc = _dseekwt(comptr->handle, (long) XDXHDRSZ, pindex->curkey, 4);
	if (rc != SUCCESS)		/* pindex->curkey has 0's */
	{
	    d_report = 10304;
	    return(dIOERR);
	}
	pindex->root = (long) 1;
	comptr->eofnode = (long) 2;
}

pindex->hasix = 0;
comptr->changed = 1;

return(SUCCESS);

} /* end of _drmkey() */

