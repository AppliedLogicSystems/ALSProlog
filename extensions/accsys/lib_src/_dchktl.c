/************
* _dchktl.c *
*************/

/***********************************************************
*                                                          *
* Copyright 1989, 1990, Billy Shakespeare & Company, Inc.  *
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
 
#include <ctype.h>
#include <stdlib.h>
#include "db4.h"
#include "d4def.h"

/*
	Example)
			   "C30CD"
			       ||__Descending
			       |___Case insensitive
*/

#ifndef ACDLL
int _DECLARE _dchktl(typelen,type,keylen,order)
#else
int _DECLARE _dchktl(DBGDAT_PTR dbgptr,
#endif
			/* d_report arrangement: 10020 */
	/* input */
	CHAR_PTR typelen;   /* key type and length */

	/* outputs */
	INT_PTR type;
	INT_PTR keylen;
	INT_PTR order;
{
	CHAR_PTR cptr;
	int ttt, ooo, klen;

	cptr = typelen;
	ooo = 'A'; /* default is 'A'scending */
	ttt = toupper(*cptr);
	cptr++; /* could not do ++ in the toupper() macro */

	switch(ttt)
	{
		case 'C':
		case 'N':
		case 'F':
		case 'D':
			break; /* right type */
		default:
			d_report = 10021;
			return(dILLEGAL); /* bad request */
	}

	klen = _datoi(cptr, &cptr);
	if ( ttt == 'C')
	{
		if (klen <= 0 || MAXKEYLEN < klen)
		{
			d_report = 10022;
			return(dOUTRANGE);
		}
	}
	if (*cptr)
	{ /* case sensitivity and/or indexing order specified */
		if (toupper(*cptr) == 'C')
		{
			ttt  = 'I'; /* case insensitivity */
			cptr++;
			if (!(*cptr)) goto done; /* only case sensitivity specified */
		}
		/* here, order character must be specified */
		ooo = toupper(*cptr);
		if (ooo != 'A' && ooo != 'D')
		{
			d_report = 10023;
			return(dILLEGAL); /* neither 'A'scending nor 'D'escending */
		}
	}

done:
	*order = ooo;
	*type = ttt;
	*keylen = klen;

	return(SUCCESS);

} /* end of _dchktl() */


int _DECLARE _datoi(sptr,newsptr)
    /* input */
    CHAR_PTR sptr;

    /* outputs */
    pCHAR_PTR newsptr;

    /* returns: number from qualifier */
{
	register int num;

	num = 0;
	while ('0' <= *sptr && *sptr <= '9')
    	num = (num << 3) + num + num  /* num * 10 */
          	+ (*sptr++ - '0');

	*newsptr = sptr;

	return(num);
} /* end of _datoi() */

