/*************
 * _dkeyrm.c *
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

#include <stdio.h>

#include <string.h>
#include "db4.h"
#include "d4def.h"

#ifndef unix
#ifdef __HIGHC__
int _DECLARE _ascend2(
#ifdef ACDLL
		DBGDAT_PTR dbgptr,
#endif
		PINDEX pindex, PADIOBUFF pio, PAVAILIST pfreelst);
#endif 	/* __HIGHC__ */
#else 	/* unix */
int _DECLARE _ascend2();
#endif 	/* unix */


#ifndef ACDLL
int _DECLARE _dkeyrm(pindex,pio,pfreelst,key,recno)
#else
int _DECLARE _dkeyrm(DBGDAT_PTR dbgptr,
#endif
			/* d_report arrangement: N/A */
	/* inputs */
	PINDEX pindex;
	PADIOBUFF pio;
	PAVAILIST pfreelst;
	CHAR_PTR key;
	long  recno;

	/* output: none */
{
	long exact;
	int rc, i;
	PXDXNOD nodeptr;
	CHAR_PTR ptr;

/*
printf("\n_dkeyrm: START "); fflush(stdout);
*/

#ifndef unix
#ifndef __HIGHC__
int _DECLARE _ascend2(
#ifdef ACDLL
		DBGDAT_PTR dbgptr,
#endif
		PINDEX pindex, PADIOBUFF pio, PAVAILIST pfreelst);
#endif 	/* __HIGHC__ */
#endif 	/* unix */

	rc = _ddescnd(
#ifdef ACDLL
			dbgptr,
#endif
			pindex, pio, key, ACWRITE, &nodeptr, &exact);

/*
printf("\n_dkeyrm: After _ddescnd rc=%x nodeptr=%x exact=%x ",
rc,nodeptr,exact); fflush(stdout);
*/

	if (rc != SUCCESS) return(rc);

	rc = _drmkey(
#ifdef ACDLL
			dbgptr,
#endif
			key, recno, pindex, pio, nodeptr, &ptr, &i);

/*
printf("\n_dkeyrm: After _drmkey  rc=%x ptr=%x i=%x ",
rc,ptr,i); fflush(stdout);
*/

	if (rc != SUCCESS) return(rc);
	
	if (!ptr && !i) return(SUCCESS);

	if (ptr)
	{
    	/* the last element in the node was removed; the second last became
       	the last => update the tree upward */
    	rc = _dascnd(
#ifdef ACDLL
				dbgptr,
#endif
				REMOVE, pindex, pio, pfreelst, ptr, pindex->work->nodeid,
    			(CHAR_PTR) 0, (long) 0);

/*
printf("\n_dkeyrm: After _dascnd  rc=%x ", rc); fflush(stdout);
*/

    	if (rc != SUCCESS) return(rc);
	}

	if (i)
	{
    	/* A node was completely removed */
    	rc = _ascend2(
#ifdef ACDLL
				dbgptr,
#endif
				pindex, pio, pfreelst);

/*
printf("\n_dkeyrm: After _ascend2  rc=%x ", rc); fflush(stdout);
*/

	}

	return(rc);

} /* end of _dkeyrm() */


/*
 * _ascend2() ascends an index file from a leaf.
 * While ascending, it deletes a key from the index file.
 */

static int _DECLARE _ascend2(pindex,pio,pfreelst)
	  /* d_report arrangement: N/A */
	/* input */
#ifdef ACDLL
	DBGDAT_PTR dbgptr,
#endif
	PINDEX pindex;
	PADIOBUFF pio;
	PAVAILIST pfreelst;
	/* output : none */
{
unsigned short shiftlen;
PXDXNOD nodeptr;
PCOMDAT comptr;
unsigned short offset;
int haslen;
CHAR_PTR bufptr;
int itemsize;
long nodenum, dummy;
CHAR_PTR hiddenkey;
int rc;
int ismdx;

ismdx = pindex->indexid;
comptr = &(pindex->xdxcdata);
itemsize = comptr->itemlen;

/* pindex->work == pindex->leaf */
/* pindex->work++;  ???? */

for (pindex->work--; pindex->xdxacc <= pindex->work; pindex->work--)
{
    /* grab node in buffer */
    rc = _dgrbnod(
#ifdef ACDLL
		dbgptr,
#endif
		pindex, pio, pindex->work, pindex->work->nodeid, &nodeptr);
    if (rc != SUCCESS) return(rc);

    offset = pindex->work->offset;

    if (nodeptr->has >= 2)
    {
		haslen = itemsize * nodeptr->has;
		shiftlen = haslen - offset + 4;
		_bytes4((long) --(nodeptr->has), nodeptr->nodebuf);
		nodeptr->bufflag = ACWRITE;
		if ((int) pindex->work->offset != haslen)
		{
	    	bufptr = nodeptr->nodebuf + (ismdx ? 8 : 4) + offset;
	    	(void) ACmemcpy(bufptr, bufptr + itemsize, shiftlen);
	    	return(SUCCESS);
		}
		/* trying to remove the last 'incomplete' key... */
		pindex->work->offset -= itemsize;
		nodenum = _4bytes(nodeptr->nodebuf + (ismdx ? 8 : 4)
			  			+ pindex->work->offset);
		rc = _dhdnkey(
#ifdef ACDLL
				dbgptr,
#endif
				pindex, pio, nodenum, &dummy, &hiddenkey);
		if (rc != SUCCESS) return(rc);

		return(_dascnd(
#ifdef ACDLL
					dbgptr,
#endif
					REMOVE, pindex, pio, pfreelst, hiddenkey,
					nodeptr->nodeid, (CHAR_PTR) 0, (long) 0));
    }

    /* This node now has zero keys (not totally empty yet!);
       it will be eliminated. */
    nodeptr->bufflag = ACREAD; /* avoid disk-write */

    nodenum  = _4bytes(nodeptr->nodebuf + (ismdx ? 8 : 4)
		       + (pindex->work->offset ? 0 :itemsize));

    if (pindex->work == pindex->xdxacc)
    {
		/* root node will come down 1 level */
    		pindex->root = nodenum;
		comptr->changed = 1;
		break;
    }

    rc = _dhdnkey(
#ifdef ACDLL
		dbgptr,
#endif
		pindex, pio, nodenum, &dummy, &hiddenkey);
    if (rc != SUCCESS) return(rc);

    return(_dascnd(
#ifdef ACDLL
		dbgptr,
#endif
		REMOVE, pindex, pio, pfreelst, hiddenkey,
		   nodenum, (CHAR_PTR) 0, (long) 0));

} /* end of for-loop */

return(SUCCESS);

} /* end of _ascend2() */

