/***********
* dDsort.c *
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

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "db4.h"
#include "d4def.h"
#include "dzinclud.h"
#ifdef STANDARD
#include <process.h>
#endif

/*
    example:                position    order
                            --------    -----
	char *specifier[] = { "1A", "2A", "3AC", "8D" };

	dDsort("ORIGTBL", "DESTTBL", 4, specifier,
               'E', "next.pgm");
*/

void _DECLARE sortorder(
#ifndef unix 
CHAR_PTR string,
		INT_PTR position, CHAR_PTR sorder, CHAR_PTR uplow)
#else  /* unix */ 
	 ) 
#endif /* unix */ 
;

#ifndef ACDLL
int DECLARE dDsort( src, dest, sortfields, specifier)
			/*** d_report arrangement: 440 ***/
	/* inputs */
#else
int DECLARE DdDsort( dbgptr, src, dest, sortfields, specifier)
			/*** d_report arrangement: 440 ***/
	/* inputs */
	DBGDAT_PTR dbgptr;
#endif
	CHAR_PTR src;  /* character string that represents the name of
		       original table file */
	CHAR_PTR dest; /* character string that represents the name of
		       the sorted table file */

	int  sortfields;  /* # of fields for sorting */
#ifndef _WINDOWS
	CHAR_PTR specifier[]; /* sort specifier */
#else
	pLPSTR   specifier;   /* sort specifier */
#endif
	/* output: none */
{
static char sortqual[120];
/* 0123456789012345678901234567890123456789 */
char sorder; /* sort order: 'A'scending or 'D'escending */
char uplow;  /* 'C'  => Not case sensitive
		else => Case sensitive or non-character type */
int position;
int offset;
int sqcnt; /* # of char's in sort-qualifier */
int rc;
char ascii[ACTMPLEN];
char fldtype;
int  length;
int  decimal;
DBF  dbfptr;
int allchars; /* # of all char's in a command line */

dUexpnm(src, "DBF", ascii);

dbfptr = _dDopen(
#ifdef ACDLL
		dbgptr,
#endif
		ascii, d_SINGLE, 1);
if (!dbfptr) return(dretcode);

allchars = ACstrlen(src);
allchars += ACstrlen("\001");
allchars += ACstrlen(dest);
allchars += 1; /* ACstrlen("0") == 1 */

/* number of sortfields */
if (sortfields <= 0)
{
    _dDclose(dbfptr);
    d_report = 441;
    return(dOUTRANGE);
}

(void) itoa(sortfields, sortqual, 10);

sqcnt = ACstrlen(sortqual); /* no space after key # */

while (sortfields--)
{
    /* format sort qualifier */

    sortorder(*specifier, &position, (CHAR_PTR) &sorder, &uplow);
    rc = dDfldno(dbfptr, position,
    	         &offset, ascii, &fldtype, &length, &decimal);
    if (rc != SUCCESS)
    {
        _dDclose(dbfptr);
        return(rc);
    }

    *(sortqual + sqcnt) = sorder;
    sqcnt++;

    if (fldtype == 'D')			     fldtype = 'T';
    else if (fldtype == 'C' && uplow == 'C') fldtype = 'I';

    *(sortqual + sqcnt) = fldtype;
    sqcnt++;

    (void) itoa(offset, sortqual + sqcnt, 10);
    sqcnt += ACstrlen(sortqual + sqcnt); /* no space after key position */
    *(sortqual + sqcnt) = ',';
    sqcnt++;
    (void) itoa(length, sortqual + sqcnt, 10);

    sqcnt += ACstrlen(sortqual + sqcnt); /* no space after key length */

    if (allchars + sqcnt >= 128)
    {
        _dDclose(dbfptr);
        d_report = 442;
        return(dOUTRANGE);
    }

    specifier++; /* get ready to read next key, if any */

} /* end of while-loop */
*(sortqual + sqcnt) = 0; /* terminating NULL */

if (allchars + sqcnt >= 128)
{
    _dDclose(dbfptr);
    d_report = 443;
    return(dOUTRANGE);
}

rc = _dDclose(dbfptr);
if (rc != SUCCESS) return(rc);

/******* OLD Scheme *****
#ifndef _WINDOWS
calltype &= 0x7f;
if (calltype == 'e' || calltype == 'E')
{
    (void) execlp("accsysd.exe", "accsysd",
                  src, "\001", dest, "0",
                  sortqual, progfile, (CHAR_PTR ) 0);
    /--* execlp should not return as soon as ACCSYSD starts running *--/

    d_report = 444;
    rc = dERROR;
}
else
{
#ifdef STANDARD
    rc = spawnlp(P_WAIT, "accsysd.exe", "accsysd",
                  src, "\001", dest, "0",
                  sortqual, progfile, (CHAR_PTR ) 0);
    if (rc == -1)
    {
        d_report = 445;
        rc = dERROR;
    }
#else
    rc = forklp("accsysd.exe", "accsysd.exe",
                  src, "\001", dest, "0",
                  sortqual, progfile, (CHAR_PTR ) 0);
    if (rc == -1)
    {
        d_report = 445;
        rc = dERROR;
    }
#endif
}
#else
ACstrcpy(_dWINbuf, "ACCSYSD ");
ACstrcat(_dWINbuf, src);
ACstrcat(_dWINbuf, " \001 ");
ACstrcat(_dWINbuf, dest);
ACstrcat(_dWINbuf, " 0 ");
ACstrcat(_dWINbuf, sortqual);
ACstrcat(_dWINbuf, " ");
ACstrcat(_dWINbuf, progfile);

rc = WinExec((LPSTR) _dWINbuf, 0);
if (rc > 32) rc = 0;
else
{
    d_report = rc;
    rc = -1;
}
#endif
******* End of OLD Scheme */

rc = _dZmain(
#ifdef ACDLL
	dbgptr,
#endif
	src, dest, OP0, sortqual);
return(rc);

} /* end of dDsort() */

static void _DECLARE sortorder(
#ifndef unix 

	/* input parameters */
        CHAR_PTR string,
        
        /* output parameters */
        INT_PTR position,
        CHAR_PTR sorder,
	CHAR_PTR uplow)
#else  /* unix */ 
	
 	string, position, sorder, uplow ) 
	CHAR_PTR string; 
	INT_PTR position; 
	CHAR_PTR sorder; 
	CHAR_PTR uplow; 
#endif /* unix */ 

{
int i;

i = 0;
while (*string)
{
    if (*string < '0' || '9' < *string) break;
    i = i * 10 + (*string - '0');
    string++;
}
*position = i;
*sorder = (toupper(*string) == 'D')  ?  'D' : 'A';
string++;
*uplow = (toupper(*string) == 'C') ? 'C' : (int) 0;
} /* end of sortorder() */
