/*************
 * dUinitmf.c *
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

void DECLARE dUinitmf(
#ifndef unix 

	/* input and output */
	CHAR_PTR memofield
		     )
#else  /* unix */ 
	memofield ) 
	CHAR_PTR memofield; 
#endif /* unix */ 

{
	(void) ACmemset(memofield, ' ', 10);
} /* end of dUinitmf() */
