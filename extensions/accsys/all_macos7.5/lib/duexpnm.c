/*************
 * dUexpnm.c *
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

#include <ctype.h>

#ifdef NIHONGO
#ifdef MSC
#include <jctype.h>
#endif
#ifndef iskanji
/***** #error iskanji() macro not defined *****/
#endif
#endif

#include <string.h>
#include "db4.h"
#include "d4def.h"
#define DOT 0x2e

void DECLARE dUexpnm(
#ifndef unix 

	/* inputs */    /*** d_report arrangement: N/A ***/
	CHAR_PTR orig,
	CHAR_PTR add,

	/* output */
	CHAR_PTR result)
#else  /* unix */ 
	
 	orig, add, result ) 
	CHAR_PTR orig; 
	CHAR_PTR add; 
	CHAR_PTR result; 
#endif /* unix */ 

{
	CHAR_PTR pointer, ptr2;

	ACstrcpy(result, orig);
	pointer = ACstrrchr(result, (int) DOT); /* last dot */
	ptr2 = result + ACstrlen(result) - 1;
	if (pointer && pointer == ptr2) goto done; /* orig ending with '.' */
	if (*ptr2 != (char) DOT)
	{  /* last character is not a dot */
		for (pointer = ptr2; result <= pointer; pointer--)
		{
		     if (*pointer == DOT) goto done;
		     if (*pointer == BACKSLASH) break;
		}
		ptr2++;
		*ptr2++ = (char) DOT;
		*ptr2   = (char) 0;
		ACstrcat(result, add);
	}
done:
	pointer = result;
#if !defined(unix) && !defined(MacOS) 
	while (*pointer)
	{
#ifdef NIHONGO
	    if (iskanji(*pointer) && iskanji2(*(pointer+1)))
		pointer++;
	    else
		*pointer = toupper(*pointer);
#else
	    *pointer = toupper(*pointer);
#endif
	    pointer++;
	}
#endif /* unix */ 

} /* end of dUexpnm() */
