/**************
 * dDcmpstr.c *
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

#include <string.h>
#include "db4.h"
#include "d4def.h"

#ifndef ACDLL
int DECLARE dDcmpstr(
#ifndef unix 
CHAR_PTR dbf1, CHAR_PTR dbf2)
#else  /* unix */ 
	
 	dbf1, dbf2 ) 
	CHAR_PTR dbf1; 
	CHAR_PTR dbf2; 
#endif /* unix */ 

		/* d_report arrangement: 190 */
#else
int DECLARE DdDcmpstr(
#ifndef unix 
DBGDAT_PTR dbgptr, CHAR_PTR dbf1, CHAR_PTR dbf2)
#else  /* unix */ 
	
 	dbgptr, dbf1, dbf2 ) 
	DBGDAT_PTR dbgptr; 
	CHAR_PTR dbf1; 
	CHAR_PTR dbf2; 
#endif /* unix */ 

		/* d_report arrangement: 190 */
#endif
{
PTBLFILE dd1, dd2;
char exname[ACTMPLEN];
int  flds1, flds2;
PFLDTL pfldtl1, pfldtl2;
PFLDNM pfldnm1, pfldnm2;
int  rc;

dUexpnm(dbf1, "DBF", exname);
dd1 = (PTBLFILE) _dDopen(
#ifdef ACDLL
			dbgptr,
#endif
			exname, d_SHARE_READ, 0);
if (!dd1) return(dretcode);

dUexpnm(dbf2, "DBF", exname);
dd2 = (PTBLFILE) _dDopen(
#ifdef ACDLL
			dbgptr,
#endif
			exname, d_SHARE_READ, 0);
if (!dd2) return(dretcode);

flds1 = dd1->fldsnum;
flds2 = dd2->fldsnum;

if (flds1 != flds2) goto mismatch;

for (pfldtl1 = dd1->fltyplen,
     pfldnm1 = dd1->fldnames,
     pfldtl2 = dd2->fltyplen,
     pfldnm2 = dd2->fldnames,
     flds1 = 1; flds1 <= flds2; flds1++,
				pfldtl1++, pfldnm1++, pfldtl2++, pfldnm2++)
{
   if (pfldtl1->type != pfldtl2->type) goto mismatch;
   if (pfldtl1->length != pfldtl2->length) goto mismatch;
   if (pfldtl1->decimal != pfldtl2->decimal) goto mismatch;
   if (ACstricmp(pfldnm1->fieldnm, pfldnm2->fieldnm)) goto mismatch;
}
rc = SUCCESS;
goto done;

mismatch:
    rc = dMISMATCH;
    d_report = 191;

done:

    (void) _dDclose((DBF) dd2);
    (void) _dDclose((DBF) dd1);

    return(rc);
} /* end of dDcmpstr() */

