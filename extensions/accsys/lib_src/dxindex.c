/************
* dXindex.c *
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
 
#include <string.h>
#include "db4.h"
#include "d4def.h"

/*
    example:

	dXindex("dbfname", "mdxname", "tagname"
		"PART_NO", "C30D", 0, keygen, "", 'E', "next.pgm");
			       |__Descending
*/

#ifndef unix 
#ifndef ACDLL
int DECLARE dXindex(
 /*** d_report arrangement: N/A ***/
#else
int DECLARE DdXindex(DBGDAT_PTR dbgptr, /*** d_report arrangement: N/A ***/
#endif
	/* inputs */
	CHAR_PTR dbfname,  /* DBF name */
	CHAR_PTR mdxname,  /* name of MDX to work on */
	CHAR_PTR tagname,  /* tag name */
	CHAR_PTR iexpr,    /* index expression */
	CHAR_PTR typelen,   /* key type and length */
	int  unique,	/* 1: uniq key only  0: duplicate keys allowed */

#ifndef __HIGHC__
#ifndef ACDLL
	int  (aPOINTER keygen)
(CHAR_PTR record, CHAR_PTR key),
			/* pointer to key generator function */
#else
	FARPROC keygen,
#endif
#else 	/* __HIGHC__ */
	int (*keygen)(),
#endif 	/* __HIGHC__ */
	CHAR_PTR FORcond,  /* FOR condition */

#ifndef __HIGHC__
#ifndef ACDLL
	int  (aPOINTER condcheck)(CHAR_PTR record)
			/* pointer to condition-checker function */
#else
	FARPROC condcheck
#endif
#else 	/* __HIGHC__ */
	int (*condcheck)()
#endif 	/* __HIGHC__ */

        )
	/* output: none */
#else  /* unix */ 
int DECLARE dXindex(
 	dbfname, mdxname, tagname, iexpr, typelen, unique, keygen, 
	FORcond, condcheck ) 
	CHAR_PTR dbfname; 
	CHAR_PTR mdxname; 
	CHAR_PTR tagname; 
	CHAR_PTR iexpr; 
	CHAR_PTR typelen; 
	int unique; 
	int (*keygen)();
	CHAR_PTR FORcond;
	int (*condcheck)();
#endif /* unix */ 
{
	int rc;
#define MINILEN ACTMPLEN
	char minibuf[MINILEN];
	DBF  dbfptr;
	int order, type, keylen;

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
			keylen = 12;
			break;
		case 'D':
			keylen = 8;
	}

	(void) ACstrncpy(minibuf, typelen, MINILEN - 5);
	if (unique) (void) ACstrcat(minibuf, "U");

#ifndef ACDLL
	rc = dXaddtag(
#else
	rc = DdXaddtag(dbgptr,
#endif
		dbfname, mdxname, tagname, iexpr, minibuf, FORcond);

	if (rc != SUCCESS) return(rc);

	dUexpnm(dbfname, "DBF", minibuf);

	dbfptr = _dDopen(
#ifdef ACDLL
				dbgptr,
#endif
				minibuf, d_SINGLE, 1);

	if (!dbfptr) return(dretcode);

	dUexpnm(mdxname, "MDX", minibuf);

	rc = _dindex(dbfptr, (CHAR_PTR) minibuf, (CHAR_PTR) tagname, order, type, 0,
		keylen, unique, keygen,
		(FORcond && (*FORcond)) ? condcheck : 0);
	order = d_report;
	(void) dDclose(dbfptr);
	d_report = order;

	if (rc != SUCCESS)
	{
		keylen = d_report;  /* keylen : used as temp. var. */
#ifndef ACDLL
		(void) dXrmtag(dbfname, mdxname, tagname); /* remove index added */
#else
		(void) DdXrmtag(dbgptr, dbfname, mdxname, tagname);
						   /* remove index added */
#endif
		d_report = keylen;
	}

	return(rc);

} /* end of dXindex() */
