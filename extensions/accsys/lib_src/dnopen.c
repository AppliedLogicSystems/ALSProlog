/***********
* dNopen.c *
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
NDX DECLARE dNopen(
				/*** d_report assignment: N/A ***/
#else
NDX DECLARE DdNopen(
				/*** d_report assignment: N/A ***/
	DBGDAT_PTR dbgptr,
#endif
	/* inputs */
	CHAR_PTR ndxname,  /* character string that represents the name of
			   NDX file to open */
	/*** d_report assignment: N/A ***/ 
	int   mode,  /* file open mode */
        int   buffs)
#else  /* unix */ 
	
NDX DECLARE dNopen(
 	ndxname, mode, buffs ) 
	CHAR_PTR ndxname; 
	int mode; 
	int buffs; 
#endif /* unix */ 
 /* number of buffers to be used
                            for the table file */
{
char exname[ACTMPLEN];

dUexpnm(ndxname, "NDX", exname);
#ifndef ACDLL
return(_dNopen(exname, mode, buffs));
#else
return(_dNopen(dbgptr, exname, mode, buffs));
#endif
} /* end of dNopen */


/*************************************************
*                  dNclose                       *
**************************************************/
#ifdef ACDLL
#undef d_report
#define d_report nptr->pdbg->d_report
#endif

int DECLARE dNclose(
#ifndef unix 
NDX ndxptr)
#else  /* unix */ 
	ndxptr ) 
	NDX ndxptr; 
#endif /* unix */ 
 /*** d_report arrangement: 3220 ***/
{
PCOMDAT comptr;

comptr = &((nptr->ndx).xdxcdata);
if (comptr->flag != OPEN)
{
    d_report = 3221;
    return(dNOOPEN);
}

return(_dNclose(ndxptr));
} /* end of dNclose() */

int _DECLARE _dNclose(
#ifndef unix 
NDX ndxptr)
#else  /* unix */ 
	ndxptr ) 
	NDX ndxptr; 
#endif /* unix */ 
 /*** d_report arrangement: 3230 ***/
{
int rc;
PCOMDAT comptr;
PADIOBUFF pio;

comptr = &((nptr->ndx).xdxcdata);

rc = dNflush(ndxptr);

/* close the file */
if (_dcls(comptr->handle) != 0)
{
    /* if errno == EBADF then invalid file-handle argument */
    if (rc == SUCCESS)
    {
	d_report = 3231;
	rc = dIOERR;
    }
}
comptr->flag = CLOSED;

pio = &(nptr->ndxbuff);

/* Finally, deallocate the NDX file descriptor. */
_dfrexm(pio->xnodes + pio->potatnum - 1, pio->potatnum);

_ACfree ( (CHAR_PTR ) ndxptr );

return(rc);
} /* end of _dNclose() */

#ifdef ACDLL
#undef d_report
#define d_report dbgptr->d_report
#endif

#ifndef ACDLL
long DECLARE dNbuffs( ndxname, buffs)
 /*** d_report arrangement: N/A ***/
	/* inputs */
#else
long DECLARE DdNbuffs( dbgptr, ndxname, buffs)
 /*** d_report arrangement: N/A ***/
	/* inputs */
	DBGDAT_PTR dbgptr;
#endif
	CHAR_PTR ndxname;  /* character string that represents
                           the name of table file to open */
        int   buffs; /* number of buffers used */

	/* returns */
	        /* long: size of memory allocated */
{
int fh;
char exname[ACTMPLEN];
long lrc;
int i;

dUexpnm(ndxname, "NDX", exname);

fh = _dopn(
#ifdef ACDLL
		dbgptr,
#endif
		(CHAR_PTR) exname, d_READONLY);
if (fh < 0) return( (long) fh);

if (buffs < 4) buffs = 4;

lrc = _dNbuffs(
#ifdef ACDLL
	dbgptr,
#endif
	fh, buffs, (UINT_PTR) &i); /* i == dummy */

(void) _dcls(fh);

return(lrc);
} /* end of dNbuffs() */

#ifndef unix 
#ifndef ACDLL
long _DECLARE _dNbuffs(
#else
long _DECLARE _dNbuffs(
	DBGDAT_PTR dbgptr,
#endif
			/*** d_report arrangement: 3250 ***/
	/* inputs */
	int fh, /* file hanle of an open NDX file */
        int buffs, /* number of buffers used */

        /* output */
        UINT_PTR basicsiz /* basic size */
		     )
#else  /* unix */ 
long _DECLARE _dNbuffs(
 	fh, buffs, basicsiz ) 
	/*** d_report arrangement: 3250 **/ 
	int fh; 
	int buffs; 
	UINT_PTR basicsiz; 
#endif /* unix */ 

	/* returns */
	        /* long: total size of memory allocated */
{
int bsiz;
int keylen;
#define MINISIZ 0x100
char minibuf[MINISIZ];

*basicsiz = 0; /* for later checking by calling routine */

bsiz = _dseekrd(fh, (long) 0, minibuf, MINISIZ);
if (bsiz != SUCCESS)
{
	(void) _dcls(fh);
	d_report = 3251;
	return( (long) bsiz); /* _ACread() failed: not too much to do */
}

keylen  = (int) _2bytes(minibuf + KEYLEN);

bsiz = (unsigned) SZNDXFILE + ((SZXDXNOD + SZPOTATO) * buffs) + keylen;

bsiz += ACstrlen(minibuf + KEXPRESS) + 1; /* key expression */

*basicsiz = bsiz;

return( (long) bsiz + (NDXNODSZ * buffs) );

} /* end of _dNbuffs() */

void DECLARE dN3datek(
#ifndef unix 
NDX ndxptr)
#else  /* unix */ 
	ndxptr ) 
	NDX ndxptr; 
#endif /* unix */ 

{
if ((nptr->ndx).xdxcdata.flag != OPEN) return; /* not open, do nothing */
nptr->ndx.keytype = 'D';
} /* end of dNdatek() */
