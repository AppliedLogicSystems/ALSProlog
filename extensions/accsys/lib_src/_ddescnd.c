/*******************
*    _ddescnd.c	   *
********************/

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

/*
 * _ddescnd() descends an NDX or MDX file
 * from its root.
 * It eventually locates a node number of the file where the
 * key resides.
 */

#ifndef ACDLL
int _DECLARE _ddescnd(pindex,pio,key,iomode,pnode,exactrec)
#else
int _DECLARE _ddescnd(DBGDAT_PTR dbgptr,
#endif
	/* inputs */      /* d_report arrangement: N/A */
	PINDEX  pindex;
	PADIOBUFF pio;
	CHAR_PTR key;
	int    iomode; /* ACREAD, PUTKEY or ACWRITE */

	/* output */
	pPXDXNOD pnode;
	LONG_PTR exactrec; /* non-zero: exact match  0: else */
{
	long nodenum;
	unsigned short offset;
	PXDXNOD nodptr;
	PCOMDAT comptr;
	int rc;
	CHAR_PTR kptr;
	unsigned int compsize, itemsize;
	int nextlevel;
	int howmany;
	int add8or4, add4or8;
	int hitonce;

	comptr = &(pindex->xdxcdata);
	compsize = pindex->complen;
	itemsize = comptr->itemlen;
	if (pindex->indexid)
	{
    	add8or4 = 8;
    	add4or8 = 4;
	}
	else
	{
    	add8or4 = 4;
    	add4or8 = 8;
	}
	pindex->work = pindex->xdxacc;
	pindex->work--;
	
	nodenum = pindex->root;

	*exactrec = (long) 0;

	while (nodenum)
	{
    	pindex->work++;
    	/*pindex->work->nodeid = nodenum;*/
	
    	/* grab node in buffer */
    	rc = _dgrbnod(
#ifdef ACDLL
			dbgptr,
#endif
    		pindex, pio, pindex->work, nodenum, &nodptr);

    	if (rc != SUCCESS) return(rc);

    	howmany = (int) _2bytes(nodptr->nodebuf);
    	if (howmany)
    	{
			offset = (howmany - 1) * itemsize;
    		/* offset : set to the second last key in the node */
			kptr = nodptr->nodebuf + offset + add8or4;
    	}
    	else
    	{
			pindex->work->offset = 0;
			*pnode = nodptr;
			pindex->leaf = pindex->work;
			switch (iomode)
			{
				case ACREAD:
					pindex->position = (int) BOTTOM;
					rc = dEOF;
					break;
				case ACWRITE: /* remove */
					pindex->position = (int) UNINIT;
					rc = dNOTFOUND;
					break;
				default: /* PUTKEY */
					pindex->position = (int) INPROGRESS;
					rc = SUCCESS;
			}
			return(rc);
    	}
    	nextlevel = (int) _4bytes(kptr + itemsize);
    	kptr += add4or8; /* kptr now points at the key that is compared */

    	*exactrec = (long) 0;
    	hitonce = 0;
    	while ( (int) offset >= 0)
    	{ /* This loop has been designed to look for a key in this node. */
			rc = (*(pindex->keycmp))(key, kptr, compsize);
			if (rc < 0)
			{   /* key < kptr */
	    		offset -= itemsize;
	    		kptr   -= itemsize;
	    		hitonce++;
	    		continue;
			}
			if (rc == 0)
			{
	    		/* key == kptr */
	    		*exactrec = _4bytes(kptr - 4);
	    		if (comptr->ftype & UNIQUE) goto outloop2;
	    		if (iomode == ACREAD)
	    		{
					offset -= itemsize;
					kptr   -= itemsize;
					continue;
	    		}
	    		/* iomode == PUTKEY or ACWRITE */
	    		if (!nextlevel && (iomode == (int) ACWRITE)) goto outloop2;
	    		break; /* iomode == ACWRITE || iomode == PUTKEY */
			}
	
			/* (rc > 0) : key > kptr */
			break;

    	} /* end of while (offset >= 0) loop */

    	offset += itemsize;
    	kptr   += itemsize;

outloop2:

    	pindex->work->offset = offset;
    	if (nextlevel)
		nodenum = _4bytes(kptr - add4or8);
    	else
    		nodenum = (long) 0;

	} /* end of while (nodenum) loop */

	*pnode = nodptr;

	if (iomode == ACREAD)
	{
    	if (*exactrec)
    	{
			pindex->position = INPROGRESS;
    	}
    	else
    	{
			pindex->position = hitonce ? INPROGRESS : BOTTOM;
    	}
	}
	else
	{
    	pindex->position = INPROGRESS;
	}

	pindex->leaf = pindex->work;
	return(SUCCESS);

} /* end of _ddescnd() */
