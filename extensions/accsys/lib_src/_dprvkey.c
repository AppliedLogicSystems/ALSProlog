/**************
 * _dprvkey.c *
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

int _DECLARE _dprvkey(opcode,pindex,pio,prevkey,pnodeptr,recno)
			/* d_report arrangement: 10220 */
	/* input */
#ifdef ACDLL
	DBGDAT_PTR dbgptr,
#endif
	/* input */
	int    opcode;  /* PREVIOUS or REMOVE */
	PINDEX pindex;
	PADIOBUFF pio;

	/* outputs */
	CHAR_PTR prevkey;   /* when opcode == PREVIOUS  */
	pPXDXNOD pnodeptr;  /* when opcode == REMOVE */
	LONG_PTR recno;
{
	PCOMDAT pxcom;
	PXDXNOD nodeptr;
	unsigned short offset;
	long ixnodeid;
	int rc;
	int add8or4;

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

    	if (rc != SUCCESS) goto done;

    	pindex->work->offset -= pxcom->itemlen;
    	offset = pindex->work->offset;

    	if (0 <= (short) offset)
    	{
			(void) ACmemcpy(pindex->curkey, nodeptr->nodebuf + offset + 12,
		      			pindex->complen);
			pindex->currecno = _4bytes(nodeptr->nodebuf + 8 + offset);
			if (opcode == PREVIOUS)
				ACmemcpy(prevkey, pindex->curkey, pindex->complen);
			else
				*pnodeptr = nodeptr;
			*recno = pindex->currecno; 
			rc = SUCCESS;
			goto done;
    	}

    	/* climb up in the tree */
    	while (2)
    	{

			if (--(pindex->work) < pindex->xdxacc)
			{
	    		pindex->position = TOP;
            	d_report = 10221;
	    		rc = dBOF;
	    		goto done;
			}

        	/* climbing up to grab... */
			rc = _dgrbnod(
#ifdef ACDLL
					dbgptr,
#endif
					pindex, pio, pindex->work, pindex->work->nodeid,
					&nodeptr);

			if (rc != SUCCESS) goto done;

			pindex->work->offset -= pxcom->itemlen;

			if (0 <= (short) pindex->work->offset) break; /* got it */

			/* climb up another step */
    	} /* end of while(2) */

    	/* go down thru the tree */
    	while (3)
    	{
			ixnodeid
	    		= _4bytes(nodeptr->nodebuf + add8or4 + pindex->work->offset);
			pindex->work++;
			pindex->work->nodeid = ixnodeid;

			rc = _dgrbnod(
#ifdef ACDLL
					dbgptr,
#endif
					pindex, pio, pindex->work, pindex->work->nodeid,
					&nodeptr);

			if (rc != SUCCESS) goto done;

			rc = pxcom->itemlen * nodeptr->has;
			pindex->work->offset = rc;

			if (!(_4bytes(nodeptr->nodebuf + add8or4 + rc))) break;
    	} /* end of while(3) */
    
    	pindex->leaf = pindex->work;

    	/* go back to read the next record from table */
	} /* end of while(1) loop */

done:

	return(rc);

} /* end of _dprvkey() */
