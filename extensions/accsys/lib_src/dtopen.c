/***********
* dTopen.c *
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

#include "db4.h"
#include <stdio.h>
#include <stdlib.h>
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

#ifndef unix 
#ifndef ACDLL
DBT DECLARE dTopen(
#else
DBT DECLARE DdTopen(
	DBGDAT_PTR dbgptr,
#endif
				/*** d_report assignment: 2140 ***/
	/* inputs */
	CHAR_PTR dbtname,  /* character string that represents the name of
			   memo file to open */
	int   mode)
#else  /* unix */ 
DBT DECLARE dTopen(
 	dbtname, mode ) 
	CHAR_PTR dbtname; 
	int mode; 
#endif /* unix */ 
  /* file open mode */
{
char exname[ACTMPLEN];
int fh;
int blocksiz;
PDBTFILE pmemof;
long lll;

dUexpnm(dbtname, "DBT", exname);
fh = _dopn(
#ifdef ACDLL
		dbgptr,
#endif
		exname, mode);
if (fh < 0)
{
    dretcode = fh;
    return((DBT) 0);
}
lll = _dTmemus(
#ifdef ACDLL
		dbgptr,
#endif
		fh, &blocksiz);
if (lll <= (long) 0)
{
    (void) _dcls(fh);
    dretcode = (int) lll;
    return((DBT) 0);
}
pmemof = (PDBTFILE) _ACalloc((unsigned long) SZDBTFILE);
if (pmemof) pmemof->dbtbuf = (CHAR_PTR) _ACalloc((unsigned long) blocksiz);
if (!pmemof || !(pmemof->dbtbuf))
{
    if (pmemof) _ACfree(pmemof);
    (void) _dcls(fh);
    d_report = 2143;
    dretcode = dMEMERR;
    return( (DBT) 0);
}
pmemof->flag = CLOSED; /* will be opened shortly */

pmemof->handle = fh;
pmemof->dbtmode = mode;
lll = lseek(fh, (long) 0, SEEK_END);
if (lll == (long) -1)
{
    _ACfree((CHAR_PTR) pmemof->dbtbuf);
    _ACfree((CHAR_PTR) pmemof);
    (void) _dcls(fh);
    d_report = 2144;
    dretcode = dIOERR;
    return( (DBT) 0);
}
pmemof->dbteof = lll;
lll = lseek(fh, (long) 0, SEEK_SET);
if (lll == (long) -1)
{
    _ACfree((CHAR_PTR) pmemof->dbtbuf);
    _ACfree((CHAR_PTR) pmemof);
    (void) _dcls(fh);
    d_report = 2145;
    dretcode = dIOERR;
    return( (DBT) 0);
}

if (_ACread(fh, pmemof->dbtbuf, (unsigned) DBTMINSZ) != DBTMINSZ)
{
    _ACfree((CHAR_PTR) pmemof->dbtbuf);
    _ACfree((CHAR_PTR) pmemof);
    (void) _dcls(fh);
    d_report = 2146;
    dretcode = dBADFILE;
    return( (DBT) 0);
}
pmemof->freehead = _4bytes(pmemof->dbtbuf);
pmemof->blksiz = _2bytes(pmemof->dbtbuf + 0x14);
pmemof->version = 4; /* dBASE IV version */

#ifdef ACDLL
pmemof->pdbg = dbgptr;
#endif

pmemof->flag = OPEN;

return((DBT) pmemof);
} /* end of dTopen() */

#ifdef ACDLL
#undef d_report
#define d_report pdbt->pdbg->d_report
#endif

int DECLARE dTclose(
#ifndef unix 
DBT dbtptr)
#else  /* unix */ 
	dbtptr ) 
	DBT dbtptr; 
#endif /* unix */ 
 /*** d_report arrangement: 2150 ***/
{
int rc;

if (pdbt->flag != OPEN)
{
    d_report = 2151;
    return(dNOOPEN);
}
if (pdbt->version != 4)
{
    d_report = 2152;
    return(dILLEGAL);
}

rc = SUCCESS;

/* close the file */
if (_dcls(pdbt->handle) != 0)
{
    /* if errno == EBADF then invalid file-handle argument */
    d_report = 2153;
    rc = dIOERR;
}
pdbt->flag = CLOSED;

_ACfree((CHAR_PTR) pdbt->dbtbuf);
_ACfree ((CHAR_PTR) dbtptr);

return(rc);
} /* end of dTclose() */
