/**************
 * dUatocf.c *
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
#include "db4.h"
#include "d4def.h"

void DECLARE dUatocf(
#ifndef unix 

        /* inputs */      /*** d_report arrangement: N/A ***/
        CHAR_PTR string,
        int   width, /* width of alphanumeric field */

        /* output */
        CHAR_PTR cfield)
#else  /* unix */ 
	
 	string, width, cfield ) 
	CHAR_PTR string; 
	int width; 
	CHAR_PTR cfield; 
#endif /* unix */ 

{
int len, copylen, padlen;

len = ACstrlen(string);

if (len >= width)
{
    copylen = width;
    padlen  = 0;
}
else
{
    copylen = len;
    padlen  = width - len;
}

(void) ACmemmove(cfield, string, copylen);
if (padlen) (void) ACmemset(cfield + copylen, ' ', padlen);

} /* end of dUatocf() */

