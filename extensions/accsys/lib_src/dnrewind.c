/*******************
*    dNrewind.c	   *
********************/

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
 
#include <stdlib.h>
#include "db4.h"
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report nptr->pdbg->d_report
#endif

/*
 * dNrewind() descends a primary index from its root.
 * It eventually locates the first node number of the table file where the
 * key is associated with.
 */

int DECLARE dNrewind(
#ifndef unix 
NDX ndxptr)
#else  /* unix */ 
	ndxptr ) 
	NDX ndxptr; 
#endif /* unix */ 
 /* d_report arrangement: 3280 */
{
int rc;

if ((nptr->ndx).xdxcdata.flag != OPEN)
{
        d_report = 3281;
        return(dNOOPEN);
}

rc = _drewfwd(
#ifdef ACDLL
	nptr->pdbg,
#endif
	&(nptr->ndx), &(nptr->ndxbuff), REWIND);
       /* if _drewfwd() returns dEOF, it means that NDX is empty */
return(rc);
} /* end of dNrewind() */

/*
 * dNforwrd() descends a primary index from its root.
 * It eventually locates the last node number of the table file where the
 * key is associated with.
 */

int DECLARE dNforwrd(
#ifndef unix 
NDX ndxptr)
#else  /* unix */ 
	ndxptr ) 
	NDX ndxptr; 
#endif /* unix */ 
 /* d_report arrangement: 3290 */
{
int rc;

if ((nptr->ndx).xdxcdata.flag != OPEN)
{
        d_report = 3291;
        return(dNOOPEN);
}
rc = _drewfwd(
#ifdef ACDLL
		nptr->pdbg,
#endif
		&(nptr->ndx), &(nptr->ndxbuff), FORWARD);
	       /* if _drewfwd() returns dBOF, it means that NDX is empty */
return(rc);
} /* end of dNforwrd() */
