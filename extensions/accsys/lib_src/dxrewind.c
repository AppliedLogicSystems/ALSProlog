/*******************
*    dXrewind.c	   *
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

#ifdef ACDLL
#undef d_report
#define d_report iptr->amdx->pdbg->d_report
#endif

/*
 * dXrewind() descends a primary index from its root.
 * It eventually locates the first node number of the table file where the
 * key is associated with.
 */

int DECLARE dXrewind(
#ifndef unix 
IDX idxptr)
#else  /* unix */ 
	idxptr ) 
	IDX idxptr; 
#endif /* unix */ 
 /* d_report arrangement: 1360 */
{
PINDEX pindex;
int rc;

if (iptr->amdx->flag != OPEN)
{   /* file not open */
    d_report = 1361;
    return(dNOOPEN);
}

pindex = &(iptr->idx);

if ((pindex->xdxcdata).flag != OPEN)
{
    d_report = 1362;
    return(dNOOPEN);
}


rc = _drewfwd(
#ifdef ACDLL
		iptr->amdx->pdbg,
#endif
		pindex, &(iptr->amdx->mdxbuff), REWIND);
       /* if _drewfwd() returns dEOF, it means that MDX's IDX is empty */
return(rc);
} /* end of dXrewind() */

/*
 * dXforwrd() descends a primary index from its root.
 * It eventually locates the last node number of the table file where the
 * key is associated with.
 */

int DECLARE dXforwrd(
#ifndef unix 
IDX idxptr)
#else  /* unix */ 
	idxptr ) 
	IDX idxptr; 
#endif /* unix */ 
 /* d_report arrangement: 1370 */
{
int rc;
PINDEX pindex;

if (iptr->amdx->flag != OPEN)
{   /* file not open */
    d_report = 1371;
    return(dNOOPEN);
}

pindex = &(iptr->idx);

if ((pindex->xdxcdata).flag != OPEN)
{
    d_report = 1372;
    return(dNOOPEN);
}


rc = _drewfwd(
#ifdef ACDLL
		iptr->amdx->pdbg,
#endif
		pindex, &(iptr->amdx->mdxbuff), FORWARD);
       /* if _drewfwd() returns dBOF, it means that MDX's IDX is empty */
return(rc);
} /* end of dXforwrd() */
