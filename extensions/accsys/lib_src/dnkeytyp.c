/**************
 * dNkeytyp.c *
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

int DECLARE dNkeytyp(
#ifndef unix 
NDX ndxptr)
#else  /* unix */ 
	ndxptr ) 
	NDX ndxptr; 
#endif /* unix */ 
 /* d_report arrangement: 3180 */
{
    if ((nptr->ndx).xdxcdata.flag != OPEN)
    {
        d_report = 3181;
        return(dNOOPEN);
    }
    return((nptr->ndx).keytype);
} /* end of dNkeytyp */
