/***********************************************************
*                                                          *
* Copyright 1991, Billy Shakespeare & Company, Inc.	   *
* All rights reserved.                                     *
*                                                          *
************************************************************
*                                                          *
* Published by                                             *
*        Copia International, Inc.                         *
*        Wheaton, Illinois                                 *
*        U. S. A.                                          *
*                                                          *
************************************************************/

#include "db4.h"
#ifdef ACDLL
#undef dversion
#endif

#ifdef ILYAS

/**** ILYAS ****/
#ifdef __HIGHC__
#define FAR 
#define PASCAL
#endif
/**** ILYAS ****/

char dversion[16] = D_VERSION; /* version control variable: was 1.03A */

/* DLL initialization */
int FAR PASCAL LibMain()
	HANDLE hInstance, WORD wDataSeg, WORD cbHeapSize,
	LPSTR lpszCmdLine;
{
/* No specific DLL initialization necesary */

if (cbHeapSize != 0) UnlockData(0);
	/* if DLL data seg is MOVEABLE, unlock it */

return(1); /* sucessful initialization */
} /* end of /* LibMain() */

/* DLL Exit Procedure */
VOID FAR PASCAL WEP()
	int nParameter;
{
	return;
} /* end of WEP() */

void FAR PASCAL _ACPident()
	LPSTR identifier; 
{
	lstrcpy(identifier, (LPSTR) dversion);
} /* end of Indentify() */

#endif 	/* ILYAS */
