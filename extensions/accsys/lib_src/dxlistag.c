/*************
* dXlistag.c *
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
#include <string.h>
#include "db4.h"
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report mptr->pdbg->d_report
#endif

/*
 *   dXlistag() list all tag names in the order of tag id numbers in MDX.
 */
int DECLARE dXlistag(
#ifndef unix 

		/*** d_report arrangement: 1240 ***/
	/* inputs */
	MDX  mdxptr,     /* MDX file pointer */

	/* outputs */
	TAG_PTR  tagptr)
#else  /* unix */ 
	
 	mdxptr, tagptr ) 
	MDX mdxptr; 
	TAG_PTR tagptr; 
#endif /* unix */ 
  /* pointer to an array of TAG structures */
{
    int rc;
    CHAR_PTR memory, tptr;
    unsigned msize;

    if (mptr->flag != OPEN)
    {
        d_report = 1241;
	return(dNOOPEN);
    }

    msize = mptr->indexes * TDATASZ;
    memory = (CHAR_PTR) _ACalloc((unsigned long) msize);
    if (!memory)
    {
	d_report = 1242;
	return(dMEMERR);
    }
    rc = _dseekrd(mptr->handle, (long) ACTTAGS, memory, msize);
    if (rc != SUCCESS)
    {
	_ACfree(memory);
	d_report = 1243;
	return(rc);
    }

    for (rc=1, tptr = memory; rc <= mptr->indexes;
    	 rc++, tptr += TDATASZ, tagptr++)
    {
	tagptr->tagid = rc;
	ACstrcpy(tagptr->tagnm, tptr + TAGNAME);
	tagptr->keytype = *(tptr + TAGTYPE);
    } /* end of for-loop */

    _ACfree(memory);

    return(SUCCESS);

} /* end of dXlistag() */
