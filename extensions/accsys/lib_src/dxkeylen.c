/**************
 * dXkeylen.c *
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
 
#include <stdlib.h>
#include "db4.h"
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report iptr->amdx->pdbg->d_report
#endif

int DECLARE dXkeylen(
#ifndef unix 
IDX idxptr)
#else  /* unix */ 
	idxptr ) 
	IDX idxptr; 
#endif /* unix */ 
 /* d_report arrangement: 1200 */
{
    if ((iptr->idx).xdxcdata.flag != OPEN)
    {
        d_report = 1201;
        return(dNOOPEN);
    }
    return((iptr->idx).complen);
} /* end of dXkeylen */
