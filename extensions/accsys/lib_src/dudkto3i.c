/**************
 * dUdkto3i.c *
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
#include "db4.h"
#include "d4def.h"

#ifdef ZORTECH
#define INFINITY 1.0e+99
#else
#define INFINITY 1.0e+100
#endif

static int _day4tbl[14] = 
/* 0, 1  2   3   4    5    6    7    8    9   10   11   12   13 */
{  0, 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366};
/*       1   2   3    4    5    6    7    8    9   10   11   12 */

void DECLARE dUdkto3i(
#ifndef unix 

        /* input */     /*** d_report arrangement: N/A ***/
        CHAR_PTR datekey, /* date key */

        /* outputs */
        INT_PTR month,
        INT_PTR day,
        INT_PTR year)
#else  /* unix */ 
	
 	datekey, month, day, year ) 
	CHAR_PTR datekey; 
	INT_PTR month; 
	INT_PTR day; 
	INT_PTR year; 
#endif /* unix */ 

{
int mm;   /* month */
int dd;   /* day */
int yyyy; /* year */
union d
{
    double julian;
    char   juldate[8];
} dddd;
CHAR_PTR dptr;
INT_PTR daysptr; /* ptr to _daystbl[] */

dptr = dddd.juldate;
for (mm = 1; mm <= 8; mm++) *dptr++ = *datekey++;
if (dddd.julian >= INFINITY)
{
	*month = 9999;
	*day = 9999;
	*year = 9999;
	return;		/* blank date key */
}
dddd.julian -= dDATECONST;
dptr = dddd.juldate;
yyyy = (int)
	((((double) 400 * dddd.julian) + (double) JULIAN) / (double) JULIAN);
dd  = (int) (dddd.julian - _ACjulian(yyyy - 1));

if (dUleap(yyyy)) daysptr = _day4tbl + 2;
else              daysptr = _daystbl + 2;

for (mm = 1; mm <= 12; mm++)
{
    if (*daysptr >= dd) break;
    daysptr++;
}

dd -= *(daysptr - 1);

if (dd == 0 && mm == 1)
{
    dd = 31;
    mm = 12;
    yyyy--;
}

*month = mm;
*day = dd;
*year = yyyy;

} /* end of dUdkto3i() */
