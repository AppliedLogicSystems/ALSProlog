/************
* dNindex.c *
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
#include <string.h>
#include "db4.h"
#include "d4def.h"


/*
    example:

	dNindex("dbfname", "ndxname", "tagname"
		"PART_NO", "C30CD", 0, keygen, 'E', "next.pgm");
			       ||__Descending
			       |___Case non-sensitive
*/
#ifndef unix 
#ifndef ACDLL
int DECLARE dNindex(
			/*** d_report arrangement: 3140 ***/
#else
int DECLARE DdNindex(DBGDAT_PTR dbgptr,
			/*** d_report arrangement: 3140 ***/
#endif
	/* inputs */
	CHAR_PTR dbfname,  /* DBF name */
	CHAR_PTR ndxname,  /* name of NDX to be created */
	CHAR_PTR kexpr,  /* key expression */
	CHAR_PTR typelen,   /* key type and length */
	int  unique,     /* 0: duplicate keys allowed  : unique keys only */
#ifndef __HIGHC__
#ifndef ACDLL
	int  (aPOINTER keygen) (CHAR_PTR record, CHAR_PTR key)
			/* pointer to key generator function */
#else
	FARPROC keygen
#endif
#else 
	int (*keygen)()
#endif 	/* __HIGHC__ */
	/* output: none */
		    )

#else  /* unix */ 
int DECLARE dNindex( dbfname, ndxname, kexpr, typelen, unique, keygen ) 
	/*** d_report arrangement: 3140 ***/ 
	CHAR_PTR dbfname; 
	CHAR_PTR ndxname; 
	CHAR_PTR kexpr; 
	CHAR_PTR typelen; 
	int unique; 
	int (*keygen)();
#endif /* unix */ 
{
int rc;
char ascii[ACTMPLEN];
DBF  dbfptr;
int order, type, keylen;

if (ACstrlen(kexpr) > MAXKEYEX)
{
	d_report = 3141;
	return(dOUTRANGE);
}

rc = _dchktl(
#ifdef ACDLL
		dbgptr,
#endif
		typelen, &type, &keylen, &order);
if (rc != SUCCESS) return(rc);
switch(type)
{
case 'N':
case 'F':
case 'D':
	keylen = 8;
}

if (order == 'D') order = 'A'; /* changed to ascending */

#ifndef ACDLL
rc = dNcreat(ndxname, kexpr, typelen);
#else
rc = DdNcreat(dbgptr, ndxname, kexpr, typelen);
#endif

if (rc != SUCCESS) return(rc);

dUexpnm(dbfname, "DBF", ascii);
dbfptr = _dDopen(
#ifdef ACDLL
		dbgptr,
#endif
		ascii, d_SINGLE, 1);
if (!dbfptr) return(dretcode);

dUexpnm(ndxname, "NDX", ascii);

rc = _dindex(dbfptr, (CHAR_PTR) ascii, (CHAR_PTR ) 0, order, type, 0,
	     keylen, unique, keygen, 0);
order = d_report;
(void) dDclose(dbfptr);
d_report = order;

if (rc != SUCCESS)
{
	keylen = d_report;  /* keylen : used as temp. var. */
	(void) _ACremove(ascii); /* remove NDX created */
	d_report = keylen;
}

return(rc);

} /* end of dNindex() */

