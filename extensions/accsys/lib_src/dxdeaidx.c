/*************
* dXdeaidx.c *
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
#include <stdlib.h>
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report iptr->amdx->pdbg->d_report
#endif

int DECLARE dXdeaidx(
#ifndef unix 
IDX idxptr)
#else  /* unix */ 
	idxptr ) 
	IDX idxptr; 
#endif /* unix */ 

		/*** d_report assignment: 1100 ***/
	/* inputs : idxptr  --  index-in-MDX pointer */
{
PCOMDAT comptr;
int rc;

comptr = &(iptr->idx.xdxcdata);

if (comptr->flag != OPEN)
{
    d_report = 1101;
    return(dNOOPEN);
}

rc = _dXiflsh(idxptr);

if (iptr->nextidx) iptr->nextidx->previdx = iptr->previdx;
else		   iptr->amdx->tailiidx   = iptr->previdx;
if (iptr->previdx) iptr->previdx->nextidx = iptr->nextidx;
else		   iptr->amdx->headiidx   = iptr->nextidx;

comptr->flag = CLOSED;

_ACfree( (CHAR_PTR) iptr);

return(rc);
} /* end of dXdeaidx() */
