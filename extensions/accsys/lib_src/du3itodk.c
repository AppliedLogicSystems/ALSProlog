/**************
 * dU3itodk.c *
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
 
/****************************************************************
 * Gregorian Calendar Definition:                               *
 *      Every fourth year is a leap year except century years.  *
 *      If a century year is divisible by 400, e.g., 2000,      *
 *      the century year is a leap year.                        *
 ****************************************************************/      

#include <stdlib.h>
#include <string.h>
#include "db4.h"
#include "d4def.h"

static char blankdate[] = {0x7d, 0xc3, 0x94, 0x25, 0xad, 0x49, 0xb2, 0x54};

#ifndef unix 
#ifndef ACDLL
int DECLARE dU3itodk(
#else
int DECLARE DdU3itodk(DBGDAT_PTR dbgptr,
#endif
        /* inputs */    /*** d_report arrangement: 4020 ***/
        int month,
        int day,
        int year,

        /* output */
        CHAR_PTR datekey)
#else  /* unix */ 
int DECLARE dU3itodk(
 	month, day, year, datekey ) 
	int month; 
	int day; 
	int year; 
	CHAR_PTR datekey; 
#endif /* unix */ 

{
static char checktbl[] = {0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
union d
{
    double julian;
    char juldate[8];
} dddd;
CHAR_PTR dptr;
int i;
int leapyear;

if (day == 9999 && month == 9999 && year == 9999)
{
	ACmemcpy(datekey, (CHAR_PTR) blankdate, 8);
	return (SUCCESS);
}
leapyear = dUleap(year);

if (day <= 0 || month <= 0 || year <= 0) goto error;
if (month > 12 || month < 1) goto error;
if (month == 2)
{
    if (day > 29) goto error;
    if (day == 29 && !leapyear) goto error;
}
else
{
    if (day > checktbl[month]) goto error;
}

dddd.julian = _ACjulian(year - 1);  /* compute for past years */
dddd.julian += _daystbl[month];
if (leapyear && month >= 3) dddd.julian++;
dddd.julian += day;

dddd.julian += dDATECONST;

dptr = dddd.juldate;
for (i=1; i<=8; i++) *datekey++ = *dptr++;

return (SUCCESS);

error:
    d_report = 4021;
    return(dOUTRANGE);

} /* end of dU3itodk() */
 
