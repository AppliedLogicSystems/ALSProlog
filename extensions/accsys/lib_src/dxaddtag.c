/*************
* dXaddtag.c *
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

#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#ifdef MSC
#include <sys\types.h>
#endif
#include "db4.h"
#include "d4def.h"
#ifdef STANDARD
#include <sys\stat.h>
#include <io.h>
#endif
#include <stdio.h>

#ifdef __HIGHC__
int _DECLARE _dxcre8(
#ifndef unix 
#ifdef ACDLL
		DBGDAT_PTR dbgptr,
#endif
		CHAR_PTR mdxname)
#else  /* unix */ 
	 ) 
#endif /* unix */ 
;
#endif	/* __HIGHC__ */

/*
 *   Example)  dXaddtag("DATABASE", "DATABASE.MDX",
 *				"tagname", "LASTNAME", "C40");
 */
#ifndef unix 
#ifndef ACDLL
int DECLARE dXaddtag(
#else
int DECLARE DdXaddtag(DBGDAT_PTR dbgptr,
#endif
		/*** d_report arrangement: 1020 ***/
	CHAR_PTR dbfname,  /* DBF name */
	CHAR_PTR mdxname,  /* MDX name */
	CHAR_PTR tagname,  /* tag name */
	CHAR_PTR iexpr,    /* index expression  */
	CHAR_PTR typelen,  /* type and length */
	CHAR_PTR FORcond)
#else  /* unix */ 
int DECLARE dXaddtag(
 	dbfname, mdxname, tagname, iexpr, typelen, FORcond ) 
	CHAR_PTR dbfname; 
	CHAR_PTR mdxname; 
	CHAR_PTR tagname; 
	CHAR_PTR iexpr; 
	CHAR_PTR typelen; 
	CHAR_PTR FORcond; 
#endif /* unix */ 
  /* FOR-condition */
{
int dbfhasmdx;
int dbffd;
int mdxfd;
int newmdx;
int rc;
CHAR_PTR memory, cptr;
int tags;
int blksz;
AVAILIST freelist;
long initeof;
long nextnode;
int  nexttag;
long locate;
int keylen, itype;
unsigned tagtblsz;
unsigned char order;

#ifndef __HIGHC__
int _DECLARE _dxcre8(
#ifndef unix 
#ifdef ACDLL
		DBGDAT_PTR dbgptr,
#endif
		CHAR_PTR mdxname)
#else  /* unix */ 
	 ) 
#endif /* unix */ 
;
#endif	/* __HIGHC__ */

if ((mdxname == (CHAR_PTR) 0) || (dbfname &&
    !ACstrcmp(dbfname, mdxname) && ACstrchr(dbfname, (int) '.')))
{
    d_report = 1021;
    return(dBADNAME);
}
    
if ((ACstrlen(iexpr) > MAXKEYEX) ||
    (FORcond && (ACstrlen(FORcond) > MAXKEYEX)))
{
    d_report = 1022;    /* key-expression or FOR-condition too long */
    return(dOUTRANGE);
}

itype = toupper(*typelen);
typelen++;
switch(itype)
{
case 'C':
    keylen = ACatoi(typelen);
    if (keylen <= 0 || MAXKEYLEN < keylen)
    {
	d_report = 1023;
	return(dOUTRANGE);
    }
    while (isdigit(*typelen)) typelen++;
    break;

case 'N':
case 'F':
    keylen = NKEYLEN;
    typelen++;
    break; /* OK */

case 'D':
    keylen = NDKEYLEN;
    typelen++;
    break; /* OK */

default:
    d_report = 1024;
    return(dILLEGAL);
}

order = isIDX;
if (toupper(*typelen) == 'D')
{
    order |= DESCEND;
    typelen++;
}
if (toupper(*typelen) == 'U')
{
    order |= UNIQUE;
}

memory = (CHAR_PTR) _ACalloc((unsigned long) DNODSIZ);
if (!memory)
{
    d_report = 1025;
    return(dMEMERR);
}

if (dbfname)
{
    /* Check the associated DBF */
    dUexpnm(dbfname, "DBF", memory);
    dbffd = _dopn(
#ifdef ACDLL
		dbgptr,
#endif
		memory, d_SINGLE);
    if (dbffd < 0)
    {
	_ACfree(memory);
	return(dbffd);
    }
    rc = _dseekrd(dbffd, (long) 0, memory, 0x20);
    if (rc != SUCCESS)
    {
	d_report = 1026;
	goto byedbf;
    }
    if (!(*memory & isDBF))
    {
	d_report = 1027;
	rc = dBADFILE;
	goto byedbf;
    }
    dbfhasmdx = (memory[MDXFLAG] == 0x01); 
}

dUexpnm(mdxname, "MDX", memory);

newmdx = 0;
mdxfd = _dopn(
#ifdef ACDLL
		dbgptr,
#endif
		memory, d_SINGLE);
if (mdxfd < 0)
{
    if (mdxfd != dNOTFOUND)
    {
	rc = mdxfd;
	goto byedbf;    /* open() failed */
    }
    mdxfd = _dxcre8(
#ifdef ACDLL
		dbgptr,
#endif
    		memory); /* no MDX exists; create it */
    if (mdxfd < 0)
    {
	rc = mdxfd;
	goto byedbf;
    }
    newmdx = 1;
}
rc = _dseekrd(mdxfd, (long) 0, memory, 0x30);
if (rc != SUCCESS)
{
    d_report = 1028;
    goto byemdx;
}

tags = *(memory + TAGNUMS);
if (tags >= MAXTAGS)
{
    d_report = 1029;
    rc = dOUTRANGE;
    goto byemdx;
}

blksz = *(memory + BLKSIZ);
if (blksz < 2) blksz = 2;
else if (blksz > 32) blksz = 32;
freelist.blksiz = blksz;

if (!tags)
{
    /* brand new or empty MDX */
    initeof = (long) blksz + 4;
    freelist.mdxeof = initeof;
    freelist.ghead  = 0;
    freelist.gnums  = 0;

    nextnode = (long) 4;
    (void) ACmemset(memory, 0, 16);
    *memory = 1;       /* # indexes (tags) */
    *(memory + 4) = blksz + 4; /* eof */
    rc = _dseekwt(mdxfd, (long) TAGNUMS, memory, 16);
    if (rc != SUCCESS)
    {
	d_report = 1030;
	goto byemdx;
    }

    (void) ACmemset(memory, 0, 0x40);

    *(memory + HIGH) = 1; /* first tag */
    cptr = memory + TDATASZ;
    *cptr = 0x04;
    ACstrcpy(cptr + TAGNAME, ACstrupr(tagname));
    *(cptr + TAG10) = 0x10;
    *(cptr + TAG2)  = 0x02;
    *(cptr + TAGTYPE) = itype;

    rc = _dseekwt(mdxfd, (long) XDXHDRSZ, memory, 0x40);
    if (rc != SUCCESS)
    {
	d_report = 1031;
	goto byemdx;
    }
}
else
{
    /* existing MDX */
    initeof = _4bytes(memory + MDXENODE);
    freelist.mdxeof = initeof;
    freelist.ghead  = _4bytes(memory + MDXGHEAD);
    freelist.gnums  = _4bytes(memory + MAVLNODE);

    tagtblsz = (tags + 1) * TDATASZ;
    if (tagtblsz > DNODSIZ)
    {
	_ACfree(memory);
        memory = (CHAR_PTR) _ACalloc((unsigned long) tagtblsz);
	if (!memory)
	{
	    (void) _dcls(mdxfd); /* newmdx == 0 (existing MDX) */
	    (void) _dcls(dbffd);
	    d_report = 1032;
	    return(dMEMERR);
	}
    }
    rc = _dseekrd(mdxfd, (long) TAGSTART, memory, tagtblsz);
    if (rc != SUCCESS)
    {
	d_report = 1033;
	goto byemdx;
    }

    nexttag = *(memory + HIGH);

    while (1)
    {
	cptr = memory + nexttag * TDATASZ;
	if (tagname) rc = ACstrcmp(ACstrupr(tagname), cptr + TAGNAME);
	if (!tagname || rc == 0)
	{
	    d_report = 1034;
	    rc = dBADNAME;
	    goto byemdx;
	}
	cptr = (rc < 0) ? cptr + LOW : cptr + HIGH;
	if (!(*cptr)) break; /* found the spot */
	nexttag = *cptr;
    } /* end of while(1) loop */
	    
    /* found the spot */
    *cptr = (char) tags + 1;
    locate = (long) TAGSTART + (cptr - memory);

    rc = _dseekwt(mdxfd, locate, cptr, 1); /* update */
    if (rc != SUCCESS)
    {
	d_report = 1035;
	goto byemdx;
    }
    /* rearrange the garbage node */
    nextnode = _dxgetnd(
#ifdef ACDLL
			dbgptr,
#endif
			mdxfd, &freelist);
    if (!nextnode)
    {
	rc = dIOERR;
	goto byemdx;
    }
    (void) ACmemset(memory, 0, TDATASZ);

    _bytes4(nextnode, memory);
    ACstrcpy(memory + TAGNAME, ACstrupr(tagname));
    *(memory + TAG10) = 0x10;
    *(memory + TOCHAN) = (char) nexttag;
    *(memory + TAG2) = 2;
    *(memory + TAGTYPE) = itype;
    locate = (long) ACTTAGS + tags * TDATASZ;
    rc = _dseekwt(mdxfd, locate, memory, TDATASZ);
    if (rc != SUCCESS)
    {
	d_report = 1036;
	goto byemdx;
    }

    /* increment tag numbers */
    *memory = tags + 1;
    rc = _dseekwt(mdxfd, (long) TAGNUMS, memory, 1);
    if (rc != SUCCESS)
    {
	d_report = 1037;
	goto byemdx;
    }

} /* end of if-else.....*/

/** write the individual index header **/
(void) ACmemset(memory, 0, DNODSIZ); /* clear buffer */
locate = nextnode << SNODSHIFT;
nextnode = _dxgetnd(
#ifdef ACDLL
			dbgptr,
#endif
			mdxfd, &freelist);
if (!nextnode)
{
	rc = dIOERR;
	goto byemdx;
}
_bytes4(nextnode, memory);
*(memory + 4) = blksz; /* initially blksz (e.g., 2) nodes used */
*(memory + ORDERBYTE) = order;
*(memory + XDXKTYPE) = (char) itype;
_bytes2(keylen, memory + KCOMPLEN);

_bytes2( ((blksz << SNODSHIFT) - 12) / (keylen + 4), memory + MAXKEYS);
_bytes2( keylen + 4, memory + KEYLEN);
ACstrcpy(memory + KEXPRESS, iexpr);
if (FORcond && *FORcond)
{
    *(memory + FORMARK) = (char) FORsig;
    ACstrcpy(memory + FORcndtn, FORcond);
}
_bytes4(nextnode, memory + ACTTAIL);
_bytes4(nextnode, memory + ACTHEAD);
rc = _dseekwt(mdxfd, locate, memory, DNODSIZ);
if (rc != SUCCESS)
{
    d_report = 1038;
    goto byemdx;
}

(void) ACmemset(memory, 0, SNODSIZ); /* clear buffer */

/* write rest of individual index header */
for (rc = 1; rc < blksz; rc++)
{
    if (_ACwrite(mdxfd, memory, SNODSIZ) != SNODSIZ)
    {
	d_report = 1039;
	rc = dIOERR;
	goto byemdx;
    }
}

/* zero out the first blocks for this individual index */
locate = nextnode << SNODSHIFT;
rc = _dseekwt(mdxfd, locate, memory, SNODSIZ);
if (rc != SUCCESS)
{
    d_report = 1040;
    goto byemdx;
}
for (rc = 1; rc < blksz; rc++)
{
    if (_ACwrite(mdxfd, memory, SNODSIZ) != SNODSIZ)
    {
	d_report = 1041;
	rc = dIOERR;
	goto byemdx;
    }
}

if (freelist.mdxeof != initeof)
{
	_bytes4(freelist.mdxeof, memory); /* clear buffer */
	rc = _dseekwt(mdxfd, (long) MDXENODE, memory, 4);
	if (rc != SUCCESS)
	{
	    d_report = 1042;
	    goto byemdx;
	}
}

if (dbfname)
{
    if (!dbfhasmdx)
    {
	*memory = 0x01;
	rc = _dseekwt(dbffd, (long) MDXFLAG, memory, 1);
	if (rc != SUCCESS) d_report = 1043;
    }
    else
        rc = SUCCESS;
}

byemdx:
	if (newmdx) (void) close(mdxfd); /* new file created */
	else	    (void) _dcls(mdxfd); /* existing MDX */
byedbf:
	(void) _dcls(dbffd);

	_ACfree(memory);

	return(rc);
} /* end of dXaddtag() */

#ifndef ACDLL
static int _DECLARE _dxcre8(
#ifndef unix 
CHAR_PTR mdxname)
#else  /* unix */ 
	mdxname ) 
	CHAR_PTR mdxname; 
#endif /* unix */ 

#else
static int _DECLARE _dxcre8(
#ifndef unix 
DBGDAT_PTR dbgptr, CHAR_PTR mdxname)
#else  /* unix */ 
	
 	dbgptr, mdxname ) 
	DBGDAT_PTR dbgptr; 
	CHAR_PTR mdxname; 
#endif /* unix */ 

#endif
		/*** d_report arrangement: 1050 ***/
{
CHAR_PTR memory;
int fn;
int blksz;
int month, day, year;
int i;

memory = (CHAR_PTR) _ACalloc((unsigned long) XDXHDRSZ);
if (!memory)
{
    d_report = 1051;
    return(dMEMERR);
}

(void) ACmemset(memory, 0, XDXHDRSZ);

fn = _dcre8new(mdxname, FORCE);
if (fn < 0)
{
	d_report = 1052;
	_ACfree(memory);
	return(dIOERR);    /* file creation by open() failed */
}

/* write the header envelope */
for (day = 1; day <= 4; day++)
{
    if (_ACwrite(fn, memory, XDXHDRSZ) != XDXHDRSZ)
    {
	close(fn);
	(void) _ACremove(mdxname);
	_ACfree(memory);
	d_report = 1053;
	return(dIOERR);
    }
}

*memory = MDXTYPE;
dUtoday(&month, &day, &year);
*(memory + MONTH)  = (char) month;
*(memory + DAY)    = (char) day;
*(memory + YEAR)   = (char) ((year - 1900) & 0x00ff);
*(memory + MONTH2) = (char) month;
*(memory + DAY2)   = (char) day;
*(memory + YEAR2)  = (char) *(memory + YEAR);
_dbasenm(mdxname, memory + MDXNAME);
blksz = d_blksiz;
if (blksz < 2) blksz = 2;
else if (blksz > 32) blksz = 32;
*(memory + BLKSIZ) = (char) blksz;
*(memory + 0x17) = (char) (blksz << 1);
*(memory + 0x18) = (char) 1;
*(memory + 0x19) = (char) 48;
*(memory + 0x1a) = (char) 32;
*(memory + 0x20) = (char) (blksz << 1) + 4; /* will be interpreted
				wrong shortly (intentionally) */

i = _dseekwt(fn, (long) 0, memory, 0x30);
if (i != SUCCESS)
{
    close(fn);
    (void) _ACremove(mdxname);
    _ACfree(memory);
    d_report = 1054;
    return(i);
}

_ACfree(memory);

return(fn);

} /* end of _dxcre8() */

