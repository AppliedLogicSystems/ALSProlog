/************
* _dxstag.c *
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
#include <string.h>
#include "db4.h"
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report mptr->pdbg->d_report
#endif

/*
 *   _dxstag() searches the tag name in MDX.
 */
int _DECLARE _dxstag(mdxptr,tagname,simple,indexid,hdrnode,memsize,
	exprlen,expression,FORcond,unique,order)
		/*** d_report arrangement: 10400 ***/
	/* inputs */
	MDX	 mdxptr;   /* MDX file pointer */
	CHAR_PTR tagname;  /* tag name */
	int	simple;    /* 1: 'memsize' not included */
			   /* 0: 'memsize' included */
	/* outputs */
		/* for simple != 0 */
	INT_PTR indexid;   /* index number (poisitional) */
	LONG_PTR hdrnode;  /* header node */
		/* for simple == 0 */
	UINT_PTR memsize; /* memory size */
	INT_PTR  exprlen; /* index expression length */
	CHAR_PTR expression; /* index expression (if pointer is non-zero) */
	CHAR_PTR FORcond;  /* FOR condition (if pointer is non-zero) */
	INT_PTR  unique;   /* 1: unique   0=>non-unique */
	CHAR_PTR order;    /* 'A'scending or 'D'escending */
{
    int rc;
    CHAR_PTR tagupper;
    CHAR_PTR memory, cptr, tptr;
    unsigned msize;
    int  nexttag;
    unsigned uuu;
    int elen, Fcondlen;

    tagupper = ACstrupr(tagname);
    msize = (mptr->indexes + 1) * TDATASZ;
    if (msize < DNODSIZ) msize = DNODSIZ;

    memory = (CHAR_PTR) _ACalloc((unsigned long) msize);
    if (!memory)
    {
	d_report = 10401;
	return(dMEMERR);
    }
    rc = _dseekrd(mptr->handle, (long) TAGSTART, memory, msize);
    if (rc != SUCCESS)
    {
	_ACfree(memory);
	d_report = 10402;
	return(rc);
    }

    nexttag = *(memory + HIGH);
    while (1)
    {
	if (nexttag > mptr->indexes)
	{
	    d_report = 10403;
	    rc = dOUTRANGE;
	    goto done;
	}
	tptr = memory + nexttag * TDATASZ;
	rc = ACstrcmp(tagupper, tptr + TAGNAME);
	if (rc == 0)
	{
	    *indexid = nexttag;
	    *hdrnode = _4bytes(tptr);

	    rc = SUCCESS;
	    goto done;
	}
	cptr = (rc < 0) ? tptr + LOW : tptr + HIGH;
	if (*cptr)
	{
	    nexttag = *cptr;
	    continue; /* goback to top of while(2) loop */
	}
	    
	/* not found */
	d_report = 10404;
	rc = dNOTFOUND;
	goto done;

    } /* end of while(1) loop */

done:

    if (rc != SUCCESS || simple)
    {
	_ACfree(memory);
	return(rc);
    }

    rc = _dseekrd(mptr->handle, *hdrnode << SNODSHIFT, memory, msize);
    if (rc != SUCCESS)
    {
	d_report = 10405;
	return(rc);
    }
    elen = ACstrlen(memory + KEXPRESS);
    Fcondlen = ACstrlen(memory + FORcndtn);
    if (expression) ACstrcpy(expression, memory + KEXPRESS);
    if (FORcond) ACstrcpy(FORcond, memory + FORcndtn);
    *unique = (*(memory + ORDERBYTE) & UNIQUE) ? 1 : 0;
    *order  = (*(memory + ORDERBYTE) & DESCEND) ? 'D' : 'A';
    uuu = elen + Fcondlen + _2bytes(memory + KEYLEN) + ACstrlen(tagname) + 3;
    	  /* elen, Fcondlen, and ACstrlen(tagname) are followed by
	     a null character; 3 is added for the computation */
    *exprlen = elen;
    *memsize = SZIIDXMDX + uuu;
    _ACfree(memory);
    return(SUCCESS);
} /* end of _dxstag() */
