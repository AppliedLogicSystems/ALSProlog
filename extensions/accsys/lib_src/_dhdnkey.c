/**************
 * _dhdnkey.c *
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

int _DECLARE _dhdnkey(pindex,pio,node,lastnode,hiddenkey)
				/* d_report arrangement: 10100 */
	/* inputs */
#ifdef ACDLL
	DBGDAT_PTR dbgptr,
#endif
	PINDEX pindex;
	PADIOBUFF pio;
	long  node; /* initial node to look for */

	/* outputs */
	LONG_PTR lastnode;
	pCHAR_PTR hiddenkey;
{
PCOMDAT comptr;
PXDXNOD nodptr, ionodep;
PPOTATO potatptr;
int  itemsize;
CHAR_PTR bufptr;
int  i, found;
#define ismdx (pindex->indexid)

comptr = &(pindex->xdxcdata);
itemsize = comptr->itemlen;

ionodep = (PXDXNOD) 0;
while (node)
{
    *lastnode = node;
    found = 0;
    for (i = 0; i < (int) pio->potatnum; i++)
    {
	potatptr = pio->potatoes + i;
	nodptr = (PXDXNOD) potatptr->bufptr;
        if ( node == nodptr->nodeid)
        {
		found = 1;
		ionodep = (PXDXNOD) 0;
		break;
	}
    } /* end of for-loop */

    if (!found)
    {
	if (ionodep)
	{
	    nodptr = ionodep;
	}
	else
	{
	    /* potatptr = _dgetptt( &(pio->mru), &(pio->lru)); */
	    /* nodptr = (PXDXNOD) potatptr->bufptr; */
	    nodptr = (PXDXNOD) (pio->lru)->bufptr;
	    			/* get the least freq. used buffer */
	    ionodep = nodptr;
	}

	if (nodptr->bufflag & ACWRITE)
	{
	    i = _dseekwt(comptr->handle,
			 (long) nodptr->nodeid << SNODSHIFT,
			 nodptr->nodebuf, (unsigned) comptr->nodesz);
	    if (i != SUCCESS)
	    {
		d_report = 10101;
		return(i);
	    }
	    if (nodptr->nodeid > pio->lstpnode)
	    	pio->lstpnode = nodptr->nodeid;
	}

	i = _dseekrd(comptr->handle, (long) node << SNODSHIFT,
		      nodptr->nodebuf, (unsigned) comptr->nodesz);
	if (i != SUCCESS)
	{
	    d_report = 10102;
	    return(i);
	}
	nodptr->has = (int) _2bytes(nodptr->nodebuf);
	nodptr->nodeid = node;
	nodptr->bufflag = ACREAD;
    } /* end of if (!found)-clause */
    bufptr = nodptr->nodebuf + (ismdx ? 8 : 4) + itemsize * nodptr->has;
    node = _4bytes(bufptr);
} /* end of while(node)-loop */

/*_dcycptt( potatptr, &(pio->mru), &(pio->lru));*/

/*
*hiddenkey = bufptr - itemsize;
*/

ACmemcpy(_dminbuf, bufptr - itemsize, itemsize);
*hiddenkey = _dminbuf;

return(SUCCESS);

} /* end of _dhdnkey() */

