/**************
 * dUnftonk.c *
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
 
#include <string.h>
#include <stdlib.h>
#include "db4.h"
#include "d4def.h"

#ifndef unix 
#ifndef ACDLL
int DECLARE dUnftonk(
#else
int DECLARE DdUnftonk(DBGDAT_PTR dbgptr,
#endif
        /* inputs */       /*** d_report arrangement: 4080 ***/
	int  style,	/* MDXstyle or NDXstyle */
        CHAR_PTR nfield,
	int  width,

        /* output */
        CHAR_PTR key)
#else  /* unix */ 
int DECLARE dUnftonk(
 	style, nfield, width, key ) 
	int style; 
	CHAR_PTR nfield; 
	int width; 
	CHAR_PTR key; 
#endif /* unix */ 
 /* ptr to numeric/float key buffer
				 8 bytes for NDX
				12 bytes for IDX in MDX */
{
double number;
unsigned char buffer[21];
CHAR_PTR cannon, anchor, ptr, dotptr, lastptr;
int pwrstart;
int  power, fraction, negpower;
unsigned char negative;
#define length pwrstart
#define byte   fraction

if (width > MXNKLEN)
{
    d_report = 4081;
    return(dOUTRANGE);
}

if (style & NDXstyle)
{
    ACmemcpy(buffer, nfield, width);
    *(buffer + width) = (char) 0;
    nfield = (CHAR_PTR) buffer;
    while (*nfield == ' ') nfield++;
    number = ACatof(nfield);
    dUdtonk(NDXstyle, number, key);
    return(SUCCESS);
}

/* NKEYidx */
power = 0;
pwrstart = 0;
negative = 0;
anchor = (CHAR_PTR ) 0;

cannon = nfield;
while (*cannon == ' ') cannon++;
if (*cannon == '-')
{
    negative = 0x80;
    cannon++;
}

for (ptr = cannon; *ptr != '.' && ptr < nfield + width; ptr++)
{
    if (*ptr != '0')
    {
     	pwrstart++;
    	if (!anchor) anchor = ptr;
    }
    if (pwrstart) power++;
}

fraction = 0;
negpower = 0;
lastptr = dotptr = ptr; /* possibly on '.' */
ptr++; /* skip possible '.' */
for (; ptr < nfield + width; ptr++)
{
    if (*ptr != '0')
    {
	fraction = ptr - dotptr;
	if (!negpower) negpower = fraction;
	if (!anchor) anchor = ptr;
	lastptr = ptr;
    }
}

ACmemset(key, 0, 12);
if (!power)
{
    if (!fraction)
    {
	*key++ = (char) 0x34;
	*key = (char) 0x01;
	length = 0;
    }
    else
    {
	length = lastptr - anchor + 1;
	*key++ = (char) (0x35 - negpower);
	*key = (char) ((length << 2) + 1);
    }
}
else
{
    *key++ = (char) (0x34 + power);
    if (!fraction)
    {
	*key = (char) 0x29;
	length = power;
    }
    else
    {
        length = power + fraction;
	*key = (char) ((length << 2) + 1);
    }
}

*key++ |= negative;

/***
#define length pwrstart
#define byte   fraction
***/
power = 0;
while(length--)
{
    if (*anchor == '.') anchor++;
    byte = *anchor++ - '0';
    power ^= 1;
    if (power)
    {
	*key = byte << 4;
    }
    else
    {
        *key |= byte;
	key++;
    }
}

return(SUCCESS);

} /* end of dUnftonk() */
