/*************
 * dUnktod.c *
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
 

#include "db4.h"

double DECLARE dUnktod(
#ifndef unix 

        /* inputs */       /*** d_report arrangement: N/A ***/
	int	 style,
        CHAR_PTR key)
#else  /* unix */ 
	
 	style, key ) 
	int style; 
	CHAR_PTR key; 
#endif /* unix */ 
 /* ptr to numeric key */

	/* output: none */
{
double number;
int i, j;
int power;
int digits;
int sign;

if (style & NDXstyle) return(*((DOUBLE_PTR) key));

/* NKEYidx */
power = *key++;
digits = *key++;
if (power == 0x34 && digits == 1) return((double) 0);

sign  = digits & 0x80;
number = 0;
i = 0;
digits = ((digits & 0x3f) - 1) >> 2;
for (j = digits; j; j--)
{
    i ^= 1;
    if (i)
    {
        number = number * 10 + ((*key >> 4) & 0x0f);
    }
    else
    {
	number = number * 10 + (*key & 0x0f);
	key++;
    }
}

power -= 0x34 + digits;
if (power > 0)
{
    while(power--) number *= 10;
}
else
{
    while(power++) number /= 10;
}

return(sign ? -number : number);

} /* end of dUnktod() */

