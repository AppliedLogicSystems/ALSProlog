/**********
 * dXcopy *
 **********/

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

#include <errno.h>
#include <stdio.h>
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

#ifndef unix 
#ifndef ACDLL
int DECLARE dXcopy(
 /* d_report arrangement: 1060 */
#else
int DECLARE DdXcopy(DBGDAT_PTR dbgptr, /* d_report arrangement: 1060 */
#endif
	/* input */
	CHAR_PTR src,    /* original MDX name */
	/* output */
	CHAR_PTR dest)
#else  /* unix */ 
int DECLARE dXcopy(
 	src, dest ) 
	CHAR_PTR src; 
	CHAR_PTR dest; 
#endif /* unix */ 
   /* new MDX name */
{
int infh, outfh;
CHAR_PTR mdxheader;
CHAR_PTR idxheader;
char srcname[ACTMPLEN];
char destname[ACTMPLEN];
int i;  /* general purpose integers */
int indexes, blksz;
CHAR_PTR cptr;
long oldhdr, newhdr, newroot;
int month, day, year;
int rc;

if (!src || !dest || !ACstrcmp(src, dest))
{
    d_report = 1061;
    return(dILLEGAL);
}

dUexpnm(src, "MDX", srcname);
dUexpnm(dest, "MDX", destname);

mdxheader = (CHAR_PTR) _ACalloc((unsigned long) MDXHDRSZ);
if (!mdxheader)
{
    d_report = 1062;
    return(dMEMERR);
}
idxheader = (CHAR_PTR) _ACalloc((unsigned long) DNODSIZ);
if (!idxheader)
{
    d_report = 1063;
    return(dMEMERR);
}

infh = _dopn(
#ifdef ACDLL
		dbgptr,
#endif
		srcname, d_READONLY);
if (infh < 0)
{
    _ACfree(idxheader);
    _ACfree(mdxheader);
    return(infh);
}

rc = _ACread(infh, mdxheader, MDXHDRSZ);
if (rc != MDXHDRSZ)
{
    (void) _dcls(infh);
    _ACfree(idxheader);
    _ACfree(mdxheader);
    d_report = 1064;
    return(dIOERR);
}

outfh = _dcre8new((CHAR_PTR) destname, FORCE);
if (outfh < 0)
{
    (void) _dcls(infh);
    _ACfree(idxheader);
    _ACfree(mdxheader);
    d_report = 1065;
    return(dIOERR);
}

indexes = (int) *(mdxheader + TAGNUMS);
blksz = (int) *(mdxheader + BLKSIZ) & 0x00ff;
if (blksz < 2) blksz = 2;
else if (blksz > 32) blksz = 32;

newhdr = (long) 4;
newroot = (long) 4 + (long) (blksz * indexes);
cptr = mdxheader + ACTTAGS;
for (i=1; i <= indexes;
     i++, cptr += TDATASZ, newhdr += (long) blksz, newroot += (long) blksz)
{
    oldhdr = _4bytes(cptr);
    _bytes4(newhdr, cptr);
    rc = _dseekrd(infh, oldhdr << SNODSHIFT, idxheader, DNODSIZ);
    if (rc != SUCCESS)
    {
	(void) close(outfh);
	(void) _dcls(infh);
	(void) _ACremove(destname);
	_ACfree(idxheader);
	_ACfree(mdxheader);
	d_report = 1066;
	return(dIOERR);
    }
    _bytes4(newroot, idxheader);
    _bytes4((long) blksz, idxheader + BNOGLIST);

    _bytes2(0, idxheader + IDXMARK);
    _bytes4(newroot, idxheader + ACTTAIL);
    _bytes4(newroot, idxheader + ACTHEAD);
    rc = _dseekwt(outfh, newhdr << SNODSHIFT, idxheader, DNODSIZ);
    if (rc != SUCCESS)
    {
	(void) close(outfh);
	(void) _dcls(infh);
	(void) _ACremove(destname);
	_ACfree(idxheader);
	_ACfree(mdxheader);
	d_report = 1067;
	return(dIOERR);
    }
}

newroot = (long) 4 + (long) (blksz * indexes);
indexes *= blksz;
(void) ACmemset(idxheader, 0, SNODSIZ);
if (lseek(outfh, newroot << SNODSHIFT, 0) == (long) -1)
{
	(void) close(outfh);
	(void) _dcls(infh);
	(void) _ACremove(destname);
	_ACfree(idxheader);
	_ACfree(mdxheader);
	d_report = 1069;
	return(dIOERR);
}
while(indexes--)
{
    if (_ACwrite(outfh, idxheader, SNODSIZ) != SNODSIZ)
    {
	(void) close(outfh);
	(void) _dcls(infh);
	(void) _ACremove(destname);
	_ACfree(idxheader);
	_ACfree(mdxheader);
	d_report = 1070;
	return(dIOERR);
    }
    newroot++;
} /* end of while-loop */
	
dUtoday(&month, &day, &year);
*(mdxheader + MONTH)  = (char) month;
*(mdxheader + DAY)    = (char) day;
*(mdxheader + YEAR)   = (char) ((year - 1900) & 0x00ff);
*(mdxheader + MONTH2) = (char) month;
*(mdxheader + DAY2)   = (char) day;
*(mdxheader + YEAR2)  = (char) *(mdxheader + YEAR);
(void) ACmemset(mdxheader + MDXNAME, 0, 12);
_dbasenm(dest, mdxheader + MDXNAME);
(void) ACmemset(mdxheader + MDXENODE, 0, 12);
_bytes4(newroot, mdxheader + MDXENODE);

/* write the new header */
rc = _dseekwt(outfh, (long) 0, mdxheader, MDXHDRSZ);
if (rc != SUCCESS)
{
    (void) close(outfh);
    (void) _ACremove(destname);
    d_report = 1068;
}

if (rc == SUCCESS) (void) close(outfh); /* already closed if not SUCCESS */
(void) _dcls(infh);
_ACfree(idxheader);
_ACfree(mdxheader);
if (rc != SUCCESS) return(rc);

/* set DBF's mdx flag on */
ACstrcpy(destname, dest);
cptr = ACstrrchr(destname, (int) '.'); /* last dot */
if (cptr) *cptr = (char) 0;
dUexpnm(destname, "DBF", srcname);
outfh = _dopn(
#ifdef ACDLL
		dbgptr,
#endif
		srcname, d_SINGLE);
if (outfh < 0) return(SUCCESS); /* probably, nothing to do with DBF */

rc = _dseekrd(outfh, (long) 0, destname, 0x20);
if (rc != SUCCESS) goto byebye; /* not valid DBF; ignore it */
if (!(*destname & isDBF)) goto byebye; /* not valid DBF; ignore it */
if (destname[MDXFLAG] == 0x01) goto byebye; /* already MDX recognized */
*srcname = 0x01;
rc = _dseekwt(outfh, (long) MDXFLAG, srcname, 1);
if (rc != SUCCESS) d_report = 1071;

byebye:
(void) _dcls(outfh);
return(rc);
} /* end of dXcopy() */

#ifndef unix 
#ifndef ACDLL
int DECLARE dXrename(
 /* d_report arrangement: 1072+ */
#else
int DECLARE DdXrename(DBGDAT_PTR dbgptr, /* d_report arrangement: 1072+ */
#endif
	/* inputs */
	CHAR_PTR mdxname,    /* original MDX filename */
	CHAR_PTR newname)
#else  /* unix */ 
int DECLARE dXrename(
 	mdxname, newname ) 
	CHAR_PTR mdxname; 
	CHAR_PTR newname; 
#endif /* unix */ 
    /* new name */
{
#define FromDBF _dminbuf
char ToDBF[ACTMPLEN];
char RENBUF[12];
int handle;
int rc;

dUexpnm(mdxname, "MDX", FromDBF);
handle = _dopn(
#ifdef ACDLL
		dbgptr,
#endif
		FromDBF, d_SINGLE);

#ifdef MSC
#define MSCLONE
#endif
#ifdef ZORTECH
#define MSCLONE
#endif

if (handle < 0)
{
#ifdef MSCLONE
    if (errno == ENOENT) /* file not found */
    {
#endif
#ifdef TURBOC
    if (errno == ENOFILE) /* file not found */
    {
#endif
#ifdef __HIGHC__
    if (errno == ENOENT) /* file not found */
    {
#endif /* __HIGHC__ */
#ifdef unix
    if (errno == ENOENT) /* file not found */
    {
#endif /* unix */
	d_report = 1075;
	return(dNOTFOUND);
    }
    d_report = 1076;
    return(dIOERR);
}

ACmemset(RENBUF, 0, 12);
_dbasenm(newname, RENBUF);
rc = _dseekwt(handle, (long) MDXNAME, RENBUF, 12);
if (rc != SUCCESS)
{
    d_report = 1077;
    rc = dIOERR;
}

(void) _dcls(handle);
if (rc != SUCCESS) return(rc);

dUexpnm(newname, "MDX", ToDBF);
if (_Arename(
#ifdef ACDLL
		dbgptr,
#endif
		FromDBF, ToDBF))
{
    d_report = 1078;
    rc = dIOERR;
}
return(rc);
} /* end of dXrename() */

