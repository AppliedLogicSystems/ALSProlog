/***********
* dXtags.c *
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
#define d_report mptr->pdbg->d_report
#endif

/*
 *   dXtags() list all tag names in the order of tag id numbers in MDX.
 */
int DECLARE dXtags(
#ifndef unix 
MDX mdxptr)
#else  /* unix */ 
	mdxptr ) 
	MDX mdxptr; 
#endif /* unix */ 

		/*** d_report arrangement: 1440 ***/
	/* input : mdxptr  --  MDX file pointer */

	/* outputs: none */

	/* returns: tagno */
{
    if (mptr->flag != OPEN)
    {
        d_report = 1441;
	return(dNOOPEN);
    }
    return(mptr->indexes);
} /* end of dXtags() */
