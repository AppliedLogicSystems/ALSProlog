/*************
 * dUdtonk.c *
 *************/

/*****************************************************
*                                                    *
* Copyright 1989, Billy Shakespeare & Company, Inc.  *
* All rights reserved.                               *
*                                                    *
******************************************************
*                                                    *
* Published by                                       *
*        Copia International, Inc.                   *
*        Wheaton, Illinois                           *
*        U. S. A.                                    *
*                                                    *
******************************************************/

#include <string.h>
#include "db4.h"
#include "d4def.h"
#ifdef STANDARD
#include <stdlib.h>
#else
#include <math.h>
#endif

#ifdef _WINDOWS
static int  dec, sign;
#endif
void DECLARE dUdtonk(
#ifndef unix 

        /* input */       /*** d_report arrangement: N/A ***/
	int    style,
        double number,
        /* output */
        CHAR_PTR key)
#else  /* unix */ 
	
 	style, number, key ) 
	int style; 
	double number; 
	CHAR_PTR key; 
#endif /* unix */ 
 /* ptr to numeric key buffer */
{
CHAR_PTR digptr;
CHAR_PTR lastdig;
#ifndef _WINDOWS
int  dec, sign;
#endif

if (style & NDXstyle)
{
	ACmemcpy(key, (CHAR_PTR) &number, 8);
	return;
}

/* MDXstyle for complex numeric key */
ACmemset(key, 0, 12);
if (!number)
{
	*key++ = (char) 0x34;
	*key   = (char) 1;
	return;
}

/**** ILYAS **** Type casting ****/
digptr = (char *)ecvt(number, 25, &dec, &sign);
*key++ = (char) 0x34 + dec;
for (lastdig = digptr + 24;
     *lastdig == '0' && digptr <= lastdig;
     lastdig--);
*key++ = 41 | (sign ? 0x80 : 0);

sign = 0;
for (dec = 1; dec <= 16 && digptr <= lastdig; dec++, digptr++)
{
    sign ^= 1;
    if (sign)
    {
	*key = (*digptr - '0') << 4;
    }
    else
    {
	*key |= (*digptr - '0') & 0x0f;
	key++;
    }
}

} /* end of dUdtonk() */
