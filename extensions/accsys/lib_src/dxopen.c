/***********
* dXopen.c *
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
#include <string.h>
#include "db4.h"
#include "d4def.h"
#ifdef STANDARD
#include <io.h>
#else
#include <fcntl.h>
#endif

#ifndef unix 
#ifndef ACDLL
MDX DECLARE dXopen(
#else
MDX DECLARE DdXopen(DBGDAT_PTR dbgptr,
#endif
				/*** d_report assignment: N/A ***/
	/* inputs */
	CHAR_PTR mdxname,  /* character string that represents the name of
			   MDX file to open */
	int   mode,  /* file open mode */
        int   buffs)
#else  /* unix */ 
MDX DECLARE dXopen(
 	mdxname, mode, buffs ) 
	CHAR_PTR mdxname; 
	int mode; 
	int buffs; 
#endif /* unix */ 
 /* number of buffers to be used
                            for the MDX file */
{
char exname[ACTMPLEN];

dUexpnm(mdxname, "MDX", exname);
return(_dXopen(
#ifdef ACDLL
	dbgptr,
#endif
	exname, mode, buffs));
} /* end of dXopen */

#ifdef ACDLL
#undef d_report
#define d_report mptr->pdbg->d_report
#endif

int DECLARE dXclose(
#ifndef unix 
MDX mdxptr)
#else  /* unix */ 
	mdxptr ) 
	MDX mdxptr; 
#endif /* unix */ 
 /*** d_report arrangement: 1280 ***/
{
if (mptr->flag != OPEN)
{
    d_report = 1281;
    return(dNOOPEN);
}

return(_dXclose(mdxptr));

} /* end of dXclose() */


int _DECLARE _dXclose(
#ifndef unix 
MDX mdxptr)
#else  /* unix */ 
	mdxptr ) 
	MDX mdxptr; 
#endif /* unix */ 
 /*** d_report arrangement: 1290 ***/
{
int rc, rc2, rc3;
PIIDXMDX  pidx;
PADIOBUFF pio;
CHAR_PTR erase;

rc = dXflush(mdxptr);

rc2 = SUCCESS;
while (mptr->tailiidx)
{
    rc3 = dXdeaidx((IDX) mptr->tailiidx);
    if (rc2 == SUCCESS) rc2 = rc3;
}

/* close the file */
if (_dcls(mptr->handle) != 0)
{
    /* if errno == EBADF then invalid file-handle argument */
    if (rc == SUCCESS && rc2 == SUCCESS)
    {
	d_report = 1291;
	rc = dIOERR;
    }
}

pidx = mptr->headiidx;
while (pidx)
{
    erase = (CHAR_PTR) pidx;
    pidx = pidx->nextidx;
    _ACfree(erase);    
}

mptr->flag = CLOSED;

/* Finally, deallocate the MDX file descriptor. */
pio = &(mptr->mdxbuff);
_dfrexm(pio->xnodes + pio->potatnum - 1, pio->potatnum);
_ACfree ( (CHAR_PTR) mdxptr );

if (rc == SUCCESS) rc = rc2;
return(rc);

} /* end of _dXclose() */

#ifdef ACDLL
#undef d_report
#define d_report dbgptr->d_report
#endif

#ifndef unix 
#ifndef ACDLL
long DECLARE dXbuffs(
 /*** d_report arrangement: N/A ***/
#else
long DECLARE DdXbuffs(DBGDAT_PTR dbgptr, /*** d_report arrangement: N/A ***/
#endif
	/* inputs */
	CHAR_PTR mdxname,  /* character string that represents
                              the name of MDX file to open */
        int   buffs)
#else  /* unix */ 
long DECLARE dXbuffs(
 	mdxname, buffs ) 
	CHAR_PTR mdxname; 
	int buffs; 
#endif /* unix */ 
 /* number of buffers used */

	/* returns */
	        /* long: size of memory allocated */
{
int fh;
char exname[ACTMPLEN];
long lrc;
int i;

dUexpnm(mdxname, "MDX", exname);

fh = _dopn(
#ifdef ACDLL
		dbgptr,
#endif
		exname, d_READONLY);
if (fh < 0) return( (long) fh);

if (buffs < 4) buffs = 4;

lrc = _dXbuffs(
#ifdef ACDLL
		dbgptr,
#endif
		fh, buffs, (UINT_PTR) &i); /* i == dummy */

(void) _dcls(fh);

return(lrc);
} /* end of dXbuffs() */

long _DECLARE _dXbuffs(
#ifndef unix 

#ifdef ACDLL
	DBGDAT_PTR dbgptr,
#endif
			/*** d_report arrangement: 1310 ***/
	/* inputs */
	int fh, /* file hanle of an open MDX file */
        int buffs, /* number of buffers used */

        /* output */
        UINT_PTR basicsiz)
#else  /* unix */ 
	
 	fh, buffs, basicsiz ) 
	int fh; 
	int buffs; 
	UINT_PTR basicsiz; 
#endif /* unix */ 
 /* basic size */

	/* returns */
	        /* long: total size of memory allocated */
{
int bsiz;
int blksz;
#define MINISIZ 0x20
char minibuf[MINISIZ];

*basicsiz = 0; /* for later checking by calling routine */

bsiz = _dseekrd(fh, (long) 0, minibuf, MINISIZ);
if (bsiz != SUCCESS)
{
	d_report = 1311;
	return( (long) bsiz); /* read() failed: not too much to do */
}

bsiz = (unsigned) SZMDXFILE + ( (SZXDXNOD + SZPOTATO) * buffs );

bsiz += ACstrlen(minibuf + KEXPRESS) + 1; /* MDX name */

*basicsiz = bsiz;

blksz = d_blksiz;
if (blksz < 2) blksz = 2;
else if (blksz > 32) blksz = 32;
blksz <<= SNODSHIFT;

return( (long) bsiz + (blksz * buffs) );
} /* end of _dXbuffs() */

