/*************
* dXsortag.c *
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
 *   dXsortag() lists all tag names in alphabetical order.
 */
int DECLARE dXsortag(
#ifndef unix 

		/*** d_report arrangement: 1420 ***/
	/* input */
	MDX  mdxptr,     /* MDX file pointer */

	/* output */
	TAG_PTR tagptr)
#else  /* unix */ 
	
 	mdxptr, tagptr ) 
	MDX mdxptr; 
	TAG_PTR tagptr; 
#endif /* unix */ 
	   /* pointer to an array of TAG structures */
{
    int rc;
    CHAR_PTR memory;
    unsigned msize;
    extern void _DECLARE _dxtnms
			(
#ifndef unix 
int tagno, CHAR_PTR memory, pTAG_PTR tagptr)
#else  /* unix */ 
	 ) 
#endif /* unix */ 
;

    if (mptr->flag != OPEN)
    {
        d_report = 1421;
	return(dNOOPEN);
    }
    msize = (mptr->indexes + 1) * TDATASZ;
    memory = (CHAR_PTR) _ACalloc((unsigned long) msize);
    if (!memory)
    {
	d_report = 1422;
	return(dMEMERR);
    }
    rc = _dseekrd(mptr->handle, (long) TAGSTART, memory, msize);
    if (rc != SUCCESS)
    {
	_ACfree(memory);
	d_report = 1423;
	return(rc);
    }

    _dxtnms( *(memory + HIGH), memory, &tagptr);

    _ACfree(memory);

    return(SUCCESS);

} /* end of dXsortag() */

void _DECLARE _dxtnms(
#ifndef unix 

		/*** d_report arrangement: N/A ***/
	/* inputs */
	int tagno,
	CHAR_PTR memory,

	/* output */
	pTAG_PTR tagptr)
#else  /* unix */ 
	
 	tagno, memory, tagptr ) 
	int tagno; 
	CHAR_PTR memory; 
	pTAG_PTR tagptr; 
#endif /* unix */ 
  /* pointer to an array of TAG structures */
{
    CHAR_PTR tptr;

    if (!tagno) return;
    _dxtnms( *(memory + tagno * TDATASZ + LOW), memory, tagptr);
    (*tagptr)->tagid = tagno;
    tptr = memory + tagno * TDATASZ;
    ACstrcpy( (*tagptr)->tagnm, tptr + TAGNAME);
    (*tagptr)->keytype = *(tptr + TAGTYPE);
    (*tagptr)++;
    _dxtnms( *(memory + tagno * TDATASZ + HIGH), memory, tagptr);
} /* end of _dxtnms() */
