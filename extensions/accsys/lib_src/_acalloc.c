/*************
* _ACalloc.c *
**************/

/*****************************************************
*                                                    *
* Copyright 1990, Billy Shakespeare & Company, Inc.  *
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

#ifdef MSC
#ifndef _WINDOWS
#include <malloc.h>
#endif
#endif

#ifdef TURBOC
#include <alloc.h>
#endif

#include <stdlib.h>
#include "db4.h"
#include "d4def.h"


#ifndef _WINDOWS

VOID_PTR _DECLARE _ACalloc(memsize)
	unsigned long memsize;
{
	return(malloc((size_t) memsize));
} /* end of _ACalloc() */

void _DECLARE _ACfree(memory)
	VOID_PTR memory;
{
	free((VOID_PTR) memory);
} /* end _ACfree() */

#else

	/*** WINDOWS ***/
VOID_PTR _DECLARE _ACalloc(unsigned long memsize)
{
HANDLE hmem;
LPSTR  lpmem;

hmem = GlobalAlloc(GMEM_MOVEABLE, (DWORD) (memsize + sizeof(HANDLE)));
if (hmem == NULL) goto errrtn;
lpmem = GlobalLock(hmem);
if (lpmem == (LPSTR) NULL) goto errrtn;
*((HANDLE far *) lpmem) = hmem;
return((VOID_PTR) (lpmem + sizeof(HANDLE)));

errrtn:
	return((VOID_PTR) NULL);
} /* end of _ACalloc() */

void _DECLARE _ACfree(VOID_PTR memory)
{
HANDLE mhandle;

mhandle = (HANDLE) *((HANDLE far *) ( (DWORD) memory - sizeof(HANDLE)));
(void) GlobalUnlock(mhandle);
(void) GlobalFree(mhandle);
} /* end _ACfree() */

#endif

