/************
 * dNexplen *
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

#include <stdlib.h>
#include "db4.h"
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report nptr->pdbg->d_report
#endif

int DECLARE dNexplen(
#ifndef unix 
 /* d_report arrangement: 3060 */
	/* inputs */
	NDX ndxptr,

	/* output: */
	INT_PTR length)
#else  /* unix */ 
	
 	ndxptr, length ) 
	NDX ndxptr; 
	INT_PTR length; 
#endif /* unix */ 

{
    PINDEX pindex;

    pindex = &(nptr->ndx);

    if ((pindex->xdxcdata).flag != OPEN)
    {
        d_report = 3061;
        return(dNOOPEN);
    }

    *length = pindex->exprlen;

    return(SUCCESS);

} /* end of dNexplen() */

