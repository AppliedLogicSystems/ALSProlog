/************
* dXrmtag.c *
*************/

/************************************************************
*                                                           *
* Copyright 1989 - 1990, Billy Shakespeare & Company, Inc.  *
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
#include <stdio.h>
#include <string.h>
#include "db4.h"
#include "d4def.h"
#ifdef STANDARD
#include <io.h>
#else
#include <fcntl.h>
#endif

#define SZALLOC XDXHDRSZ

#ifdef __HIGHC__
extern void _DECLARE fixup
		(
CHAR_PTR memory, int partag, int prevval, int newval)
;
#endif 	/* __HIGHC__ */

/*
 *   Example)  dXrmtag("DATABASE", "DATABASE", "tagname");
 */
#ifndef unix 
#ifndef ACDLL
int DECLARE dXrmtag(
#else
int DECLARE DdXrmtag(DBGDAT_PTR dbgptr,
#endif
		/*** d_report arrangement: 1400 ***/
	CHAR_PTR dbfname,  /* DBF name */
	CHAR_PTR mdxname,  /* MDX name */
	CHAR_PTR tagname)
#else  /* unix */ 
int DECLARE dXrmtag(
 	dbfname, mdxname, tagname ) 
	CHAR_PTR dbfname; 
	CHAR_PTR mdxname; 
	CHAR_PTR tagname; 
#endif /* unix */ 
  /* tag name */
{
MDX mdxptr;
int fn;
int rc;
CHAR_PTR memory, premove;
int tags;
long eofnode, garbhead, hdrnode, freenum;
long headnode;
#define tailnode eofnode
int  rmtag;
unsigned tagtblsz;
int lefttag, righttag, partag;
int dummy;
char cdummy;

#ifndef __HIGHC__
extern void _DECLARE fixup
		(
#ifndef unix 
CHAR_PTR memory, int partag, int prevval, int newval)
#else  /* unix */ 
	 ) 
#endif /* unix */ 
;
#endif 	/* __HIGHC__ */

if ((mdxname == (CHAR_PTR) 0) || (dbfname &&
    !ACstrcmp(dbfname, mdxname) && ACstrchr(dbfname, (int) '.')))
{
    d_report = 1401;
    return(dBADNAME);
}

#ifndef ACDLL
mdxptr = dXopen(mdxname, d_SINGLE, 4);
#else
mdxptr = DdXopen(dbgptr, mdxname, d_SINGLE, 4);
#endif

if (!mdxptr) return(dretcode);
tags = mptr->indexes;
rc = _dxstag(mdxptr, (CHAR_PTR) tagname, 1,
	     (INT_PTR) &rmtag, (LONG_PTR) &hdrnode, (UINT_PTR) &tagtblsz,
	     (INT_PTR) &tagtblsz, (CHAR_PTR) 0,
	     (CHAR_PTR) 0, (INT_PTR) &dummy, (CHAR_PTR) &cdummy);
				/* tagtblsz --> dummy */
fn = dXclose(mdxptr);

if (rc != SUCCESS) return(rc);
if (fn != SUCCESS) return(rc);

memory = (CHAR_PTR) _ACalloc((unsigned long) SZALLOC);
if (!memory)
{
	d_report = 1402;
	return(dMEMERR);
}
dUexpnm(mdxname, "MDX", memory);

if (tags == 1)
{	/* this is the only tag in MDX */
    (void) _ACremove(memory);    

    if (!dbfname)
    {
    	_ACfree(memory);
	return(SUCCESS);
    }

    /* Check the associated DBF */
    dUexpnm(dbfname, "DBF", memory);
    fn = _dopn(
#ifdef ACDLL
		dbgptr,
#endif
    		memory, d_SINGLE);
    if (fn < 0)
    {
	_ACfree(memory);
	return(fn);
    }
    rc = _dseekrd(fn, (long) 0, memory, 0x20);
    if (rc != SUCCESS)
    {
        d_report = 1403;
	_ACfree(memory);
	(void) _dcls(fn);
	return(rc);
    }
    if (!(*memory & isDBF))
    {
	d_report = 1404;
	_ACfree(memory);
	(void) _dcls(fn);
	return(dBADFILE);
    }
    *memory = (char) 0;
    rc = _dseekwt(fn, (long) MDXFLAG, memory, 1);
    if (rc != SUCCESS)
    {
        d_report = 1405;
    }
    _ACfree(memory);
    (void) _dcls(fn);
    return(rc);
}

/* there are more than 1 tag */
fn = _dopn(
#ifdef ACDLL
		dbgptr,
#endif
		memory, d_SINGLE);
if (fn < 0)
{
	_ACfree(memory);
	return(fn);
}

tagtblsz = (tags + 1) * TDATASZ;
if (tagtblsz > SZALLOC)
{
	_ACfree(memory);
        memory = (CHAR_PTR) _ACalloc((unsigned long) tagtblsz);
	if (!memory)
	{
		(void) _dcls(fn);
		d_report = 1406;
		return(dMEMERR);
	}
}

rc = _dseekrd(fn, (long) 0, memory, 0x30);
if (rc != SUCCESS)
{
	d_report = 1407;
	goto closefree;
}
garbhead = _4bytes(memory + MDXGHEAD);
freenum  = _4bytes(memory + MAVLNODE);

/* remove the tag from the tag table */
rc = _dseekrd(fn, (long) TAGSTART, memory, tagtblsz);
if (rc != SUCCESS)
{
	d_report = 1408;
	goto closefree;
}

premove = memory + rmtag * TDATASZ;

lefttag = *(premove + LOW);
righttag = *(premove + HIGH);
partag   = *(premove + TOCHAN);

for (;;)
{
    if (!lefttag && !righttag)
    {
	/* leaf */
	fixup(memory, partag, rmtag, 0);
	if (tags != rmtag)
	{
	    ACmemcpy(premove, memory + tags * TDATASZ, TDATASZ);
	    fixup(memory, (int) *(memory + tags * TDATASZ + TOCHAN),
			  tags, rmtag);
	}
	break;
    }
    if (righttag)
    {
        rmtag = righttag;
	lefttag = *(memory + righttag * TDATASZ + LOW);
	while (lefttag)
	{
	    rmtag = lefttag;
	    lefttag = *(memory + lefttag * TDATASZ + LOW);
	}

	ACmemcpy(premove, memory + rmtag * TDATASZ, 16);
	partag = *(memory + rmtag * TDATASZ + TOCHAN);
	premove = memory + rmtag * TDATASZ;
	lefttag = *(premove + LOW);
	righttag = *(premove + HIGH);
	continue;
    }
    ACmemcpy(premove, memory + lefttag * TDATASZ, 18);
    partag = rmtag;
    rmtag = lefttag;
    premove = memory + rmtag * TDATASZ;
    lefttag = *(premove + LOW);
    righttag = *(premove + HIGH);
    if (lefttag)  *(memory + lefttag * TDATASZ + TOCHAN) = partag;
    if (righttag) *(memory + righttag * TDATASZ + TOCHAN) = partag;

    partag = *(memory + tags * TDATASZ + TOCHAN);
    if (tags != rmtag)
    {
	ACmemcpy(premove, memory + tags * TDATASZ, TDATASZ);
	fixup(memory, partag, tags, rmtag);
    }
    break;
} /* end of for(;;) loop */

rc = _dseekwt(fn, (long) TAGSTART, memory, tagtblsz);
if (rc != SUCCESS)
{
	d_report = 1409;
	goto closefree;
}

/** fix up the garbage collection chain */
rc = _dseekrd(fn, hdrnode << SNODSHIFT, memory, XDXHDRSZ);
if (rc != SUCCESS)
{
	d_report = 1410;
	goto closefree;
}
headnode = _4bytes(memory + ACTHEAD);
tailnode = _4bytes(memory + ACTTAIL);
freenum  += _4bytes(memory + BNOGLIST);
(void) ACmemset(memory, 0, 4);
_bytes4(headnode, memory + BNOGLIST);
(void) ACmemset(memory + ACTTAIL, 0, 8);

rc = _dseekwt(fn, hdrnode << SNODSHIFT, memory, XDXHDRSZ);
if (rc != SUCCESS)
{
	d_report = 1411;
	goto closefree;
}

if (garbhead)
{
	_bytes4(garbhead, memory);
	rc = _dseekwt(fn, (tailnode << SNODSHIFT) + BNOGLIST, memory, 4);
	if (rc != SUCCESS)
	{
		d_report = 1412;
		goto closefree;
	}
}

*memory = tags - 1;
rc = _dseekwt(fn, (long) TAGNUMS, memory, 1);
if (rc != SUCCESS)
{
    d_report = 1413;
    goto closefree;
}


_bytes4(hdrnode, memory);
_bytes4(freenum, memory + 4);
rc = _dseekwt(fn, (long) MDXGHEAD, memory, 8);
if (rc != SUCCESS)  d_report = 1414;

closefree:

(void) _dcls(fn);
_ACfree(memory);

return(rc);

} /* end of dXrmtag() */

static void _DECLARE fixup(
#ifndef unix 

	CHAR_PTR memory,
	int partag, int prevval, int newval)
#else  /* unix */ 
	
 	memory, partag, prevval, newval ) 
	CHAR_PTR memory; 
	int partag; 
	int prevval; 
	int newval; 
#endif /* unix */ 

{
CHAR_PTR lowptr;

lowptr  = memory + partag * TDATASZ + LOW;

if (*lowptr == prevval)
{
	*lowptr = newval;
}
else
{
	*(lowptr + 1) = newval;
}
} /* end of fixup */

