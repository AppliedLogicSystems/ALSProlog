/************
* dTcreat.c *
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
 
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include "db4.h"
#include "d4def.h"
#ifdef STANDARD
#include <sys\types.h>
#include <sys\stat.h>
#include <io.h>
#endif

/*
 *   Example)  dTcreat("memofile");
 */
#ifndef ACDLL
int DECLARE dTcreat(
#ifndef unix 
CHAR_PTR dbtname)
#else  /* unix */ 
	dbtname ) 
	CHAR_PTR dbtname; 
#endif /* unix */ 

#else
int DECLARE DdTcreat(
#ifndef unix 
DBGDAT_PTR dbgptr, CHAR_PTR dbtname)
#else  /* unix */ 
	
 	dbgptr, dbtname ) 
	DBGDAT_PTR dbgptr; 
	CHAR_PTR dbtname; 
#endif /* unix */ 

#endif
		/*** d_report arrangement: 2040 ***/
	/* input:  dbtname; -- DBT name */
	
	/* output: none */
{
int fh;
int i;  /* general purpose integer */
int blksz;
char name[80];
char buff[DBTMINSZ];

dUexpnm(dbtname, "DBT", name);

fh = _dcre8new((CHAR_PTR) name, FORCE);
if (fh < 0)
{
	d_report = 2041;
	return(dIOERR);    /* file creation by open() failed */
}

(void) ACmemset(buff, 0, DBTMINSZ); /* clear buffer */

/* dBASE IV format */
/* initialze buffer */
*buff = 0x01;
*(buff + 0x12) = 0x02;
*(buff + 0x13) = 0x01;
blksz = d_blksiz;
if (blksz < 1) blksz = 1;
else if (blksz > 32) blksz = 32;
_bytes2((unsigned short) blksz << SNODSHIFT, buff + 0x14);

_dbasenm(name, buff + 0x08); /* file base name */
i = DBTMINSZ;

/* write the header */
if (_ACwrite(fh, buff, (unsigned) i) != (unsigned) i)
{
    close(fh);
    (void) _ACremove(name);
    d_report = 2042;
    return(dIOERR);
}

(void) close(fh); /* close DBT */

#ifndef ACDLL
return(_dtchdbf(name, isDBT4)); /* touch DBF */
#else
return(_dtchdbf(dbgptr, name, isDBT4)); /* touch DBF */
#endif

} /* end of dTcreat() */

int _DECLARE _dtchdbf(
#ifndef unix 

		/*** d_report arrangement: 2050 ***/
#ifdef ACDLL
	DBGDAT_PTR dbgptr,
#endif
	/*** d_report arrangement: 2050 ***/ 
	CHAR_PTR name,   /* calling routine must have the storage for this */
	int  product)
#else  /* unix */ 
	
 	name, product ) 
	CHAR_PTR name; 
	int product; 
#endif /* unix */ 
    /* isDBT4 or ~isDBT4 */
{
int fh;
char buf[1];
int ronly;
int rc;
CHAR_PTR dotptr;

/* convert the name from DBT to DBF */
dotptr = ACstrstr(name, ".");
*(dotptr + 1) = (char) 0;
(void) ACstrcat(name, "DBF");

fh = _dopn(
#ifdef ACDLL
		dbgptr,
#endif
		name, d_SINGLE);
if (fh < 0) return(fh);

if (_ACread(fh, buf, 1) != 1)
{
	d_report = 2051;
	(void) _dcls(fh);
	return(dIOERR);
}
if (!(buf[0] & isDBF))
{
	d_report = 2052;
	(void) _dcls(fh);
	return(dBADFILE);
}

if (product == isDBT4 && buf[0] & hasDBT && buf[0] & isDBT4) goto done;
if (product != isDBT4 && buf[0] & hasDBT && !(buf[0] & isDBT4)) goto done;

/* Warining! if DBF had a DBT of different product, i.e., IV v.s. III,
             the 'product' will overwrite it */
ronly = (buf[0] & DBRONLY) ? 1 : 0;

buf[0] = (product == isDBT4) ? (char) DBFwDBT4 : (char) DBFwDBT3;
if (ronly) buf[0] |= DBRONLY;
rc = _dseekwt(fh, (long) 0, buf, 1);
if (rc != SUCCESS)
{
	d_report = 2053;
	(void) _dcls(fh);
	return(dIOERR);
}

done:

(void) _dcls(fh);

return(SUCCESS);

} /* end of _dtchdbf() */

