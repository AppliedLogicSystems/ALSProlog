/**************
 * dXputkey.c *
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

int DECLARE dXputkey(
#ifndef unix 
 /* d_report arrangement: 1340 */
	/* inputs */
	IDX      idxptr,
	CHAR_PTR key,
	long     recno)
#else  /* unix */ 
	
 	idxptr, key, recno ) 
	IDX idxptr; 
	CHAR_PTR key; 
	long recno; 
#endif /* unix */ 

{
int rc;

if (iptr->amdx->flag != OPEN)
{   /* file not open */
    d_report = 1341;
    return(dNOOPEN);
}
if (iptr->idx.xdxcdata.flag != OPEN)
{
    d_report = 1342;
    return(dNOOPEN); /* IDX not activated */
}


/* index open status will be checked by _dxput() */
/* enter key in MDX */
rc = _dxput(
#ifdef ACDLL
	iptr->amdx->pdbg,
#endif
	&(iptr->idx), &(iptr->amdx->mdxbuff), &(iptr->amdx->freelist),
	key, recno);
if (rc == SUCCESS) iptr->idx.hasix = HASIDX;
return(rc);

} /* end of dXputkey() */
