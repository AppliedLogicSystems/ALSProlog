/************
 * dNexpr.c *
 ************/

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
#define d_report nptr->pdbg->d_report
#endif

int DECLARE dNexpr(
#ifndef unix 
 /* d_report arrangement: 3080 */
	/* inputs */
	NDX ndxptr,

	/* output: none */
	CHAR_PTR kexpr,   /* key expression */
	INT_PTR  unique)
#else  /* unix */ 
	
 	ndxptr, kexpr, unique ) 
	NDX ndxptr; 
	CHAR_PTR kexpr; 
	INT_PTR unique; 
#endif /* unix */ 
  /* 1: unique   0=>non-unique */
{
    PINDEX pindex;

    pindex = &(nptr->ndx);

    if ((pindex->xdxcdata).flag != OPEN)
    {
        d_report = 3081;
        return(dNOOPEN);
    }

    (void) ACmemcpy(kexpr, pindex->express, pindex->exprlen + 1);
    *unique = (pindex->xdxcdata.ftype & UNIQUE) ? 1 : 0;

    return(SUCCESS);

} /* end of dNexpr() */

