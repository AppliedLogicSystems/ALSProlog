/*************
 * dUdtonf.c *
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
#include <string.h>
#include "db4.h"
#include "d4def.h"
#ifndef STANDARD
#include <math.h>
#endif

#ifdef _WINDOWS
static int  decpt, sign;
#endif

#ifndef unix 
#ifndef ACDLL
int DECLARE dUdtonf(
#else
int DECLARE DdUdtonf(DBGDAT_PTR dbgptr,
#endif
        /* inputs */       /*** d_report arrangement: 4060 ***/
        double number,
	int    width,
	int    decimal,

        /* output */
        CHAR_PTR nfield)
#else  /* unix */ 
int DECLARE dUdtonf(
 	number, width, decimal, nfield ) 
	double number; 
	int width; 
	int decimal; 
	CHAR_PTR nfield; 
#endif /* unix */ 

{
CHAR_PTR digitptr;
#ifndef _WINDOWS
int  decpt, sign;
#endif

if ( (decimal < 0) || (decimal && width <= (decimal + 1)) )
{
    d_report = 4061;
    return(dOUTRANGE);
}
/**** ILYAS **** Type casting ****/
digitptr = (char *) ecvt(number, 25, &decpt, &sign);

if (number == (double) 0) sign = 0;
_Atonf(digitptr, width, decimal, decpt, sign, nfield);
return(SUCCESS);
} /* end of dUdtonf() */

void _DECLARE _Atonf(
#ifndef unix 

        /* inputs */
        CHAR_PTR digits,
	int    width,
	int    decimal,
	int    decpt,
	int    sign,

        /* output */
        CHAR_PTR nfield)
#else  /* unix */ 
	
 	digits, width, decimal, decpt, sign, nfield ) 
	CHAR_PTR digits; 
	int width; 
	int decimal; 
	int decpt; 
	int sign; 
	CHAR_PTR nfield; 
#endif /* unix */ 

{
CHAR_PTR digptr;
int places, blanks;

places = width;
if (decimal) places -= decimal + 1;
blanks = (decpt > 0) ? places - decpt : places - 1;
if (blanks < 0) blanks = 0;
if (sign && blanks > 0) blanks--;
digptr = digits;
if (decpt > places) digptr += decpt - places;
places -= blanks;
if (sign) 
{
    places--;
    if (decpt > places) digptr++;
}
while (blanks--) *nfield++ = ' ';
if (sign)	 *nfield++ = '-';
if (decpt <= 0)
{
    if (places > 0) *nfield++ = '0';
}
else
{
    while(places--) *nfield++ = (*digptr) ? *digptr++ : '0';
}
if (!decimal) return; /* done */
/* work on '.' and fractional part */
*nfield++ = '.';
while(decpt < 0 && decimal)
{
    *nfield++ = '0';
    decpt++;
    decimal--;
}
while(decimal)
{
    *nfield++ = (*digptr) ? *digptr++ : '0';
    decimal--;
}
} /* end of dUdtonf() */
