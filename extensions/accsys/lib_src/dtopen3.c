/************
* dTopen3.c *
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

#include "db4.h"
#include <stdio.h>
#include <stdlib.h>
#include "d4def.h"
#ifdef STANDARD
#include <io.h>
#else
#include <fcntl.h>
#endif

#ifndef unix 
#ifndef ACDLL
DBT DECLARE dTopen3(
#else
DBT DECLARE DdTopen3(DBGDAT_PTR dbgptr,
#endif
				/*** d_report assignment: 2260 ***/
	/* inputs */
	CHAR_PTR dbtname,  /* character string that represents the name of
			      memo file to open */
	int   mode)
#else  /* unix */ 
DBT DECLARE dTopen3(
 	dbtname, mode ) 
	CHAR_PTR dbtname; 
	int mode; 
#endif /* unix */ 
  /* file open mode */
{
char exname[ACTMPLEN];
int fh;
PDBTFILE pmemof;
int rc;

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

pmemof = (PDBTFILE) _ACalloc((unsigned long) SZDBTFILE);
if (pmemof) pmemof->dbtbuf = (CHAR_PTR) _ACalloc((unsigned long) SNODSIZ);
if (!pmemof || !(pmemof->dbtbuf))
{
    (void) _dcls(fh);
    d_report = 2263;
    dretcode = dMEMERR;
    return( (DBT) 0);
}
pmemof->flag = CLOSED; /* will be opened shortly */

pmemof->handle = fh;
pmemof->dbtmode = mode;

rc = _dseekrd(fh, (long) 0, pmemof->dbtbuf, (unsigned) DBTMINSZ);
if (rc != SUCCESS)
{
    (void) _dcls(fh);
    d_report = 2264;
    dretcode = dBADFILE;
    return( (DBT) 0);
}
pmemof->dbteof = pmemof->freehead = _4bytes(pmemof->dbtbuf);
pmemof->blksiz = SNODSIZ;
pmemof->version = 3; /* dBASE III (PLUS) version */

#ifdef ACDLL
pmemof->pdbg = dbgptr;
#endif

pmemof->flag = OPEN;

return((DBT) pmemof);
} /* end of dTopen3() */

#ifdef ACDLL
#undef d_report
#define d_report pdbt->pdbg->d_report
#endif

int DECLARE dTclose3(
#ifndef unix 
DBT dbtptr)
#else  /* unix */ 
	dbtptr ) 
	DBT dbtptr; 
#endif /* unix */ 
 /*** d_report arrangement: 2270 ***/
{
int rc;

if (pdbt->flag != OPEN)
{
    d_report = 2271;
    return(dNOOPEN);
}
if (pdbt->version != 3)
{
    d_report = 2272;
    return(dILLEGAL);
}

rc = SUCCESS;

if (pdbt->freehead != pdbt->dbteof)
{
    _bytes4(pdbt->freehead, pdbt->dbtbuf);
    rc = _dseekwt(pdbt->handle, (long) 0, pdbt->dbtbuf, 4);
}

/* close the file */
if (_dcls(pdbt->handle) != 0)
{
    /* if errno == EBADF then invalid file-handle argument */
    d_report = 2273;
    if (rc == SUCCESS) rc = dIOERR;
}
pdbt->flag = CLOSED;

_ACfree((CHAR_PTR) pdbt->dbtbuf);
_ACfree ( (CHAR_PTR) dbtptr);

return(rc);

} /* end of dTclose3() */

