/**************
 * dNkeylen.c *
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
#define d_report nptr->pdbg->d_report
#endif

int DECLARE dNkeylen(
#ifndef unix 
 /* d_report arrangement: 3160 */
	NDX ndxptr /* primary index file descriptor */
		    )
#else  /* unix */ 
	ndxptr ) 
	NDX ndxptr; 
#endif /* unix */ 

{
    if ((nptr->ndx).xdxcdata.flag != OPEN)
    {
        d_report = 3161;
        return(dNOOPEN);
    }
    return((nptr->ndx).complen);
} /* end of dNkeylen */
