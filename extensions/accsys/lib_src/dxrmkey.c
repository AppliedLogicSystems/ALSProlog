/*************
 * dXrmkey.c *
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
 
#include <stdlib.h>
#include "db4.h"
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report iptr->amdx->pdbg->d_report
#endif

/*
 *  dXrmkey removes a key from an index.
 */
int DECLARE dXrmkey(
#ifndef unix 
 /* d_report arrangement: 1380 */
	/* inputs */
	IDX  idxptr,
	CHAR_PTR key,
	long recno)
#else  /* unix */ 
	
 	idxptr, key, recno ) 
	IDX idxptr; 
	CHAR_PTR key; 
	long recno; 
#endif /* unix */ 


	/* output : none */
{
int rc;

if (iptr->amdx->flag != OPEN)
{   /* file not open */
    d_report = 1381;
    return(dNOOPEN);
}

if ((iptr->idx).xdxcdata.flag != OPEN)
{
    d_report = 1382;
    return(dNOOPEN);
}


rc = _dkeyrm(
#ifdef ACDLL
	iptr->amdx->pdbg,
#endif
	&(iptr->idx), &(iptr->amdx->mdxbuff), &(iptr->amdx->freelist),
	key, recno);
if (rc != SUCCESS)
{
    if (rc == dEOF) rc = dNOTFOUND;
}


return(rc);

} /* end of dXrmkey() */
