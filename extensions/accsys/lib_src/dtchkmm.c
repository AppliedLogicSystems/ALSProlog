/*************
 * dTchkmm.c *
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
 

#include <stdlib.h>
#include "db4.h"

#define BLANK   0x01
#define ZERO    0x02
#define NONZERO 0x04
#define NUMBER  0x06

int DECLARE dTchkmm(
#ifndef unix 
CHAR_PTR memofield)
#else  /* unix */ 
	memofield ) 
	CHAR_PTR memofield; 
#endif /* unix */ 
  /* d_report arrangement: N/A */
	/* input:  memofield */

	/* output : none */

	/* return value:  -1  --  bad memo field
			   0  --  no memo exists
			   1  --  there is a memo */
{
int i;
int status;

status = 0;
for (i=1; i<=10; i++, memofield++)
{
    if ('0' <= *memofield && *memofield <= '9')
    {
        /*if (status & BLANK) return(-1); -- not applicable for non-dBASE4*/
       	if (*memofield == '0')
	{
	    if (!(status & NONZERO)) status |= ZERO;
	}
	else
	{
	    status = NONZERO;
	}
	continue;
    }
    if (*memofield == ' ')
    {
        if (status & NUMBER) return(-1); /* bad memofield format */
	status |= BLANK;
	continue;
    }
    return(-1); /* bad memo field format */
}

if (status & BLANK) return(0); /* has memo: blank memo field */

/* status must be a number */
if (!(status & NONZERO)) return(-1); /* all 0's */

return(1); /* there is a memo */

} /* end of dTchkmm() */
