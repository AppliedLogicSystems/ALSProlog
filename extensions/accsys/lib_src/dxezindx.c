/*************
* dXezindx.c *
**************/

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

/*
    example:

	dXezindx("dbfname",
		 3, 0, 'E', "next.pgm");
		 |  |
		 |  |__Order && Unique
		 |
		 |_____Field Number

*/

#ifndef unix 
#ifndef ACDLL
int DECLARE dXezindx(
#else
int DECLARE DdXezindx(DBGDAT_PTR dbgptr,
#endif
			/*** d_report arrangement: 1120 ***/
	/* inputs */
	CHAR_PTR name,     /* database name */
	int  fieldno,   /* field number */
	int  orderuniq  /* bit 0: 1: uniq key only  0: duplicate keys allowed
			   bit 1: 1: Descending     0: Ascending */
		     )
#else  /* unix */ 
int DECLARE dXezindx(
 	name, fieldno, orderuniq ) 
	CHAR_PTR name; 
	int fieldno; 
	int orderuniq; 
#endif /* unix */ 

	/* output: none */
{
int rc;
char ascii[ACTMPLEN];
DBF  dbfptr;
int offset, length, decimal;
int order;
char type;
char fieldnm[22]; /* can handle nihongo as well */
CHAR_PTR cptr;

dUexpnm(name, "DBF", ascii);
dbfptr = _dDopen(
#ifdef ACDLL
	dbgptr,
#endif
	ascii, d_SINGLE, 1);
if (!dbfptr) return(dretcode);
rc = dDfldno(dbfptr, fieldno, &offset, fieldnm, &type, &length, &decimal);
if (rc !=SUCCESS)
{
    (void) dDclose(dbfptr);
    return(rc);
}
if (type == 'M' || type == 'L')
{
    (void) dDclose(dbfptr);
    d_report = 1121;
    return(dILLEGAL);
}

cptr = ascii;
*cptr++ = type;
(void) ACitoa(length, cptr, 10);
cptr = ascii + ACstrlen(ascii);
if (orderuniq & 0x02)
{
	*cptr++ = 'D';
	order = 'D';
}
else
{
	*cptr++ = 'A';
	order  = 'A';
}
if (orderuniq & 0x01) *cptr++ = 'U';
*cptr = (char) 0;

(void) dDclose(dbfptr); /* don't bother dXaddtag when it opens DBF */

#ifndef ACDLL
rc = dXaddtag(
#else
rc = DdXaddtag(dbgptr,
#endif
		name, name, fieldnm, fieldnm, ascii, (CHAR_PTR) 0);
if (rc != SUCCESS) return(rc);

switch(type)
{
case 'N':
case 'F':
	length = 12;
	break;
case 'D':
	length = 8;
}

dUexpnm(name, "DBF", ascii);
dbfptr = _dDopen(
#ifdef ACDLL
		dbgptr,
#endif
		ascii, d_SINGLE, 1);
if (!dbfptr) return(dretcode);

dUexpnm(name, "MDX", ascii);

rc = _dindex(dbfptr, (CHAR_PTR) ascii, (CHAR_PTR) fieldnm,
		order, (int) type, offset,
		length, orderuniq & 0x01, 0, 0);
order = d_report;
(void) dDclose(dbfptr);
d_report = order;

if (rc != SUCCESS)
{
	length = d_report;  /* length : used as temp. var. */
#ifndef ACDLL
	(void) dXrmtag(name, ascii, fieldnm); /* remove index added */
#else
	(void) DdXrmtag(dbgptr, name, ascii, fieldnm); /* remove index added */
#endif
	d_report = length;
}

return(rc);

} /* end of dXezindx() */
