/***********
* dDopen.c *
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

#include <stdio.h>

#include <stdlib.h>
#include "db4.h"
#include "d4def.h"

#ifdef STANDARD
#include <io.h>
#else
#include <fcntl.h>
#endif

DBF DECLARE dDopen(
#ifndef unix 
#ifndef ACDLL

		/*** d_report assignment: N/A ***/
		/* inputs */
#else
DBF DECLARE DdDopen(
		/*** d_report assignment: N/A ***/
		/* inputs */
	DBGDAT_PTR dbgptr,
#endif
	CHAR_PTR dbfname,  	/* character string that represents the name of
			      			table file to open */
	/*** d_report assignment: N/A ***/ 
	int   mode,  		/* file open mode */
    int   buffs)
#else  /* unix */ 
	
 	dbfname, mode, buffs ) 
	CHAR_PTR dbfname; 
	int mode; 
	int buffs; 
#endif /* unix */ 
 		/* number of buffers to be used
                            for the table file */
{
	char exname[ACTMPLEN];

	dUexpnm(dbfname, "DBF", exname);

#ifndef ACDLL
	return(_dDopen(exname, mode, buffs));
#else
	return(_dDopen(dbgptr, exname, mode, buffs));
#endif
} /* end of dDopen */


/*************************************************
*                  dDclose                       *
**************************************************/

#ifdef ACDLL
#undef d_report
#define d_report tblptr->pdbg->d_report
#endif

int DECLARE dDclose(
#ifndef unix 
DBF dbfptr)
#else  /* unix */ 
	dbfptr ) 
	DBF dbfptr; 
#endif /* unix */ 
 /*** d_report arrangement: 300 ***/
{
	PCOMDAT comptr;

	comptr = &(tblptr->tblcdata);
	if (comptr->flag != OPEN)
	{
    	d_report = 301;
    	return(dNOOPEN);
	}

	return(_dDclose(dbfptr));
} /* end of dDclose() */

int _DECLARE _dDclose(
#ifndef unix 
DBF dbfptr)
#else  /* unix */ 
	dbfptr ) 
	DBF dbfptr; 
#endif /* unix */ 
 /*** d_report arrangement: 306 ***/
{
	int rc;
	PCOMDAT comptr;

	comptr = &(tblptr->tblcdata);

	rc = dDflush(dbfptr);

	/* close the file */
	if (_dcls(comptr->handle) != 0)
	{
    	/* if errno == EBADF then invalid file-handle argument */
    	if (rc == SUCCESS)
    	{
			d_report = 306;
			rc = dIOERR;
    	}
	}
	comptr->flag = CLOSED;

	/* Finally, deallocate the table file descriptor. */
	_dfredm(tblptr->nodes + tblptr->potatnum - 1, tblptr->potatnum);
	_ACfree ( (CHAR_PTR) dbfptr );

	return(rc);

} /* end of _dDclose() */


#ifdef ACDLL
#undef d_report
#define d_report dbgptr->d_report
#endif


long DECLARE dDbuffs(
#ifndef unix 
#ifndef ACDLL
 /*** d_report arrangement: N/A ***/
		/* inputs */
#else
long DECLARE DdDbuffs( /*** d_report arrangement: N/A ***/
		/* inputs */
	DBGDAT_PTR dbgptr,
#endif
	CHAR_PTR dbfname,  	/* character string that represents
			      			the name of table file to open */
	/*** d_report arrangement: N/A ***/ 
    int   buffs)
#else  /* unix */ 
	
 	dbfname, buffs ) 
	CHAR_PTR dbfname; 
	int buffs; 
#endif /* unix */ 
 		/* number of buffers used */

		/* returns */
	        /* long: size of memory allocated */
{
	int i;  /* general purpose integer */
	int fh;
	char exname[ACTMPLEN];
	long lrc;

	dUexpnm(dbfname, "DBF", exname);

	fh = _dopn(
#ifdef ACDLL
			dbgptr,
#endif
			exname, d_READONLY);

	if (fh < 0) return((long) fh);

	if (buffs < 1) buffs = 1;

#ifndef ACDLL
	lrc = _dDbuffs(fh, buffs, (UINT_PTR) &i, &i); /* i == dummy */
#else
	lrc = _dDbuffs(dbgptr, fh, buffs, (UINT_PTR) &i, &i); /* i == dummy */
#endif

	(void) _dcls(fh);

	return(lrc);

} /* end of dDbuffs() */


long _DECLARE _dDbuffs(
#ifndef unix 

#ifdef ACDLL
	DBGDAT_PTR dbgptr,
#endif
			/*** d_report arrangement: 310 ***/
		/* inputs */
	int fh, 		/* file hanle of an open table file */
    int buffs, 		/* number of buffers used */

        /* output */
	/*** d_report arrangement: 310 ***/ 
    UINT_PTR basicsiz, 	/* basic size */
	INT_PTR  nofields)
#else  /* unix */ 
	
 	fh, buffs, basicsiz, nofields ) 
	int fh; 
	int buffs; 
	UINT_PTR basicsiz; 
	INT_PTR nofields; 
#endif /* unix */ 
 	/* no. of fields */

	/* returns */
	        /* long: total size of memory allocated */
{
	int bsiz, noflds, bufsize;
#define MINISIZ 0x20
	char minibuf[MINISIZ];
	unsigned int hdrsize, reclen;
	CHAR_PTR cptr;

	*basicsiz = 0; /* for later check by calling routine */

	bsiz = _dseekrd(fh, (long) 0, minibuf, MINISIZ);

	if (bsiz != SUCCESS)
	{
		d_report = 311;
		return( (long) bsiz); /* _ACread() failed: not too much to do */
	}

	cptr = minibuf + TBLADRS;
	hdrsize = (int) _2bytes(cptr);
	cptr += 2;
	reclen  = (int) _2bytes(cptr);

	if (hdrsize < 0x41)
	{
		/* probably the file is too small to qualify for a DBF */
		d_report = 312;
		return( (long) dBADFILE);
		/* read failure; or
		   file's length is shorter than expected .DB size */
	}

	noflds = (int) (hdrsize - 0x20) / 0x20; /* no. of fields */

	bufsize  = _Dnodesiz(reclen) + 1; /* 1 byte for end-marker */

	bsiz = (unsigned) SZTBLFILE + (unsigned) (SZFLDTL + SZFLDNM) * noflds;

	bsiz += (SZTBLNOD + SZPOTATO) * buffs;

	*basicsiz = bsiz;
	*nofields = noflds;

	return( (long) bsiz + ( bufsize * buffs ) );
} /* end of _dDbuffs() */


int _DECLARE _Dnodesiz(
#ifndef unix 
int reclen)
#else  /* unix */ 
	reclen ) 
	int reclen; 
#endif /* unix */ 

{
    int ccc;

    if   (reclen <= 0x0100)    ccc = 0x400;
    else if (reclen <= 0x0200) ccc = 0x800;
    else if (reclen <= 0x0400) ccc = 0x1000;
    else 		       ccc = 0x2000;

    return(ccc - (ccc % reclen));

} /* end of _Dnodesiz */

