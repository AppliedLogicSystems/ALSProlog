/**************
 * dXgetrno.c *
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

#ifdef ACDLL
#undef d_report
#define d_report iptr->amdx->pdbg->d_report
#endif

/*
 *  dXgetrno obtains a record number from an index file based on a key.
 */
int DECLARE dXgetrno(
#ifndef unix 
 /* d_report arrangement: 1160 */
	/* inputs */
	IDX idxptr,
	CHAR_PTR key,

	/* output */
	LONG_PTR recno)
#else  /* unix */ 
	
 	idxptr, key, recno ) 
	IDX idxptr; 
	CHAR_PTR key; 
	LONG_PTR recno; 
#endif /* unix */ 

{
int rc;
PINDEX pindex;
PADIOBUFF pio; 

if (iptr->amdx->flag != OPEN)
{   /* file not open */
    d_report = 1161;
    return(dNOOPEN);
}

pindex = &(iptr->idx);
if ((pindex->xdxcdata).flag != OPEN)
{
    d_report = 1162;
    return(dNOOPEN);
}

pio = &(iptr->amdx->mdxbuff);
rc = _dXgrec(
#ifdef ACDLL
		iptr->amdx->pdbg,
#endif
		pindex, pio, key, recno);
return(rc);
} /* end of dXgetrno() */

#ifdef ACDLL
#undef  d_report
#define d_report dbgptr->d_report
#endif

int _DECLARE _dXgrec(
#ifndef unix 
 /* d_report arrangement: 1170 */
	/* inputs */
#ifdef ACDLL
	DBGDAT_PTR dbgptr,
#endif
	PINDEX pindex,
	PADIOBUFF pio,
	CHAR_PTR key,

	/* output */
	LONG_PTR recno)
#else  /* unix */ 
	
 	pindex, pio, key, recno ) 
	PINDEX pindex; 
	PADIOBUFF pio; 
	CHAR_PTR key; 
	LONG_PTR recno; 
#endif /* unix */ 

{
unsigned short offset;
long exact;
int rc;
PXDXNOD nodeptr;

rc = _ddescnd(
#ifdef ACDLL
	dbgptr,
#endif
	pindex, pio, key, ACREAD, &nodeptr, &exact);

if (rc != SUCCESS) return(rc);

offset = pindex->work->offset;

*recno = _4bytes(nodeptr->nodebuf + offset + 8); 
		/* nodeptr->nodebuf + 4 + offset + 4 */

(void) ACmemcpy(pindex->curkey, nodeptr->nodebuf + offset + 12,
	      pindex->complen);
pindex->currecno = *recno;

if (!exact)
{
    if (pindex->position == (int) BOTTOM)
    {
	d_report = 1171;
        return(dEOF);
    }
    else
    {  
	d_report = 1172;
	return(dNOTFOUND);
    }
}

return(SUCCESS);
} /* end of _dXgrec() */
