/**************
 * _dxput.c *
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

#include "db4.h"
#include <string.h>
#include "d4def.h"

#ifndef ACDLL
int _DECLARE _dxput(pindex,pio,pfreelst,key,recno)
#else
int _DECLARE _dxput(DBGDAT_PTR dbgptr,
#endif
			/* d_report arrangement: 10380 */
	/* inputs */
	PINDEX pindex;
	PADIOBUFF pio;
	PAVAILIST pfreelst;
	CHAR_PTR  key;
	long  recno;

{
long node1, node2;
int rc;
long found;
PXDXNOD nodeptr;
PCOMDAT pxcom;
CHAR_PTR key1, key2;

rc = _ddescnd(
#ifdef ACDLL
		dbgptr,
#endif
		pindex, pio, key, PUTKEY, &nodeptr, &found);
if (rc != SUCCESS) return(rc);

pxcom = &(pindex->xdxcdata);

if (found)
{
    if (pxcom->ftype & UNIQUE)
    { 
	d_report = 10382;
	return(dKEYVIOL);
    }

    if (found == recno)
    {
        pindex->leaf->offset -= pxcom->itemlen;
	(void) ACmemcpy(pindex->curkey, key, pxcom->itemlen);
	pindex->currecno = recno;
	pindex->position = PUTKEY;
	return(SUCCESS);
    }
}

/*  key1 = key2 = (CHAR_PTR) 0; --- done by _dputkey() */

rc =_dputkey(
#ifdef ACDLL
		dbgptr,
#endif
		pindex, pio, pfreelst, nodeptr, key, recno,
		pindex->work->offset / pxcom->itemlen,
		&key1, &node1, &key2, &node2);
if (rc != SUCCESS) return(rc);

if (key1 || key2)
{
    rc = _dascnd(
#ifdef ACDLL
		dbgptr,
#endif
		ACWRITE, pindex, pio, pfreelst, key1, node1, key2, node2);
    if (rc == SUCCESS)
	rc = _ddescnd(
#ifdef ACDLL
		dbgptr,
#endif
		pindex, pio, key, ACWRITE, &nodeptr, &found);
}

pxcom->size++;

if (rc == SUCCESS)
{
    /* keep key of current node and offset */

    (void) ACmemcpy(pindex->curkey, key, pxcom->itemlen);
    pindex->currecno = recno;
    pxcom->changed = 1;
}

pindex->position = PUTKEY;

return(rc);

} /* end of _dxput() */
