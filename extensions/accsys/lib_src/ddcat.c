/**************
 * dDcat.c *
 **************/

/************************************************************
*                                                           *
* Copyright 1990 - 1991, Billy Shakespeare & Company, Inc.  *
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

/*
#include <ctype.h>
#include <string.h>
*/
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "db4.h"
#include "d4def.h"

 
#ifdef unix
#include <sys/types.h>
#include <unistd.h>
#endif  /* unix */
 
#ifdef STANDARD
#include <io.h>
#else
#include <fcntl.h>
#endif

#define IOSIZE 0x4000
#define MINISIZE 16

int _DECLARE _ACcopyfile(
#ifndef unix 

#ifdef ACDLL
		DBGDAT_PTR dbgptr,
#endif
		int from, int to,
		CHAR_PTR memory, unsigned int memsize, long copylen)
#else  /* unix */ 
	 ) 
#endif /* unix */ 
;
#ifndef ACDLL
int DECLARE dDcat(
#ifndef unix 
CHAR_PTR origdbf, CHAR_PTR adddbf)
#else  /* unix */ 
	
 	origdbf, adddbf ) 
	CHAR_PTR origdbf; 
	CHAR_PTR adddbf; 
#endif /* unix */ 

			/* d_report arrangement: 340 */
#else
int DECLARE DdDcat(
#ifndef unix 
DBGDAT_PTR dbgptr, CHAR_PTR origdbf, CHAR_PTR adddbf)
#else  /* unix */ 
	
 	dbgptr, origdbf, adddbf ) 
	DBGDAT_PTR dbgptr; 
	CHAR_PTR origdbf; 
	CHAR_PTR adddbf; 
#endif /* unix */ 

			/* d_report arrangement: 340 */
#endif
{
char exname[ACTMPLEN];
int  rc, fd1, fd2, save;
CHAR_PTR memory;
long size1, size2;
unsigned int stadr1, stadr2, reclen;

dUexpnm(adddbf, "DBF", exname);
fd2 = _dopn(
#ifdef ACDLL
		dbgptr,
#endif
		exname, d_SINGLE);
if (fd2 < 0)
{
    d_report = 341;
    return(dNOTFOUND);
}

memory = (CHAR_PTR) _ACalloc((unsigned long) IOSIZE);
if (!memory)
{
    _dcls(fd2);
    d_report = 342;
    return(dMEMERR);
}

dUexpnm(origdbf, "DBF", exname);
fd1 = _dopn(
#ifdef ACDLL
		dbgptr,
#endif
		exname, d_SINGLE);
if (fd1 < 0)
{
    if (errno != ENOENT)
    {
	/* not (file not found) */
	_ACfree(memory);
	_dcls(fd2);
	d_report = 343;
	return(fd1);
    }
    /* origdbf does not exist */
    rc = _dseekrd(fd2, (long) 0, memory, MINISIZE);
    if (rc != SUCCESS)
    {
	_ACfree(memory);
	_dcls(fd2);
	d_report = 344;
	return(rc);
    }
    (void) lseek(fd2, (long) 0, SEEK_SET); /* this can't fail */
    stadr2 = _2bytes(memory + TBLADRS);
    size2  = _4bytes(memory + RECORD);
    reclen = _2bytes(memory + RECLEN);

    fd1 = _dcre8new(exname, ~FORCE);
    if (fd1 < 0)
    {
	(void) _dcls(fd2);
	d_report = 345;
	return(dIOERR);
    }

    rc = _ACcopyfile(
#ifdef ACDLL
		dbgptr,
#endif
		fd2, fd1, memory, IOSIZE, size2 * reclen + stadr2 + 1);
    save = errno;
    _ACfree(memory);
    _dcls(fd1);
    _dcls(fd2);
    if (rc != SUCCESS)
    {
        (void) _ACremove(exname);
	errno = save;
    }
    return(rc);
}

/* origdbf exists */
_dcls(fd1);
_dcls(fd2);
#ifndef ACDLL
rc = dDcmpstr(        origdbf, adddbf);
#else
rc = DdDcmpstr(dbgptr, origdbf, adddbf);
#endif
if (rc != SUCCESS)
{
    _ACfree(memory);
    return(rc);
}

/* reopen origdbf and adddbf */
dUexpnm(origdbf, "DBF", exname);
fd1 = _dopn(
#ifdef ACDLL
		dbgptr,
#endif
		exname, d_SINGLE);
if (fd1 < 0)
{
    _ACfree(memory);
    d_report = 346;
    return(fd1);
}
dUexpnm(adddbf, "DBF", exname);
fd2 = _dopn(
#ifdef ACDLL
		dbgptr,
#endif
		exname, d_SINGLE);
if (fd2 < 0)
{
    _ACfree(memory);
    _dcls(fd1);
    d_report = 347;
    return(fd2);
}
rc = _dseekrd(fd1, (long) 0, memory, MINISIZE);
if (rc != SUCCESS)
{
    _ACfree(memory);
    _dcls(fd2);
    _dcls(fd1);
    d_report = 348;
    return(rc);
}
stadr1 = _2bytes(memory + TBLADRS);
size1  = _4bytes(memory + RECORD);
reclen = _2bytes(memory + RECLEN);

rc = _dseekrd(fd2, (long) 0, memory, MINISIZE);
if (rc != SUCCESS)
{
    _ACfree(memory);
    _dcls(fd2);
    _dcls(fd1);
    d_report = 349;
    return(rc);
}
stadr2 = _2bytes(memory + TBLADRS);
size2  = _4bytes(memory + RECORD);

if (lseek(fd1, size1 * reclen + stadr1, SEEK_SET) == (long) -1)
{
    _ACfree(memory);
    _dcls(fd2);
    _dcls(fd1);
    d_report = 350;
    return(dIOERR);
}
if (lseek(fd2, (long) stadr2, SEEK_SET) == (long) -1)
{
    _ACfree(memory);
    _dcls(fd2);
    _dcls(fd1);
    d_report = 351;
    return(dIOERR);
}

/* append adddbf to origdbf */
rc = _ACcopyfile(
#ifdef ACDLL
		dbgptr,
#endif
		fd2, fd1, memory, IOSIZE, size2 * reclen + 1);
save = errno;
_dcls(fd2); /* close adddbf */
if (rc != SUCCESS)
{
    _ACfree(memory);
    /* perform recovery action on origdbf */
    *memory = (char) ENDMARK;
    (void) _dseekwt(fd1, size1 * reclen + stadr1, memory, 1);
    (void) _ACwrite(fd1, memory, 0); /* truncate the rest */
    (void) _dcls(fd1);
    d_report = 352;
    errno = save;
    return(rc);
}

_bytes4(size1 + size2, memory);
rc = _dseekwt(fd1, (long) RECORD, memory, 4);
_ACfree(memory); /* free memory */
_dcls(fd1);      /* close origdbf */
if (rc != SUCCESS) d_report = 353; /* dIOERR */

return(SUCCESS);
} /* end of dDcat() */

static int _DECLARE _ACcopyfile(
#ifndef unix 

#ifdef ACDLL
			DBGDAT_PTR dbgptr,
#endif
			int from, int to,
			CHAR_PTR memory, unsigned int memsize, long copylen)
#else  /* unix */ 
	
 	from, to, memory, memsize, copylen ) 
	int from; 
	int to; 
	CHAR_PTR memory; 
	unsigned int memsize; 
	long copylen; 
#endif /* unix */ 

{
int rlen, wlen;

while (copylen > (long) 0)
{
    rlen = _ACread(from, memory, memsize);
    if (rlen == 0) break;
    if (rlen < 0)
    {
        d_report = 356;
	return(dIOERR);
    }

    wlen = _ACwrite(to, memory, rlen);
    if (wlen != rlen)
    {
        d_report = 357;
        return(dIOERR);
    }
    copylen -= wlen;
} /* end of while(1) loop */
if (copylen > (long) 0)
{
    d_report = 358;
    return(dBADFILE);
}
return(SUCCESS);
} /* end of _ACcopyfile() */
