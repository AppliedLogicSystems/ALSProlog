/**************
 * dTmemosz.c *
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

#include "db4.h"
#include <stdlib.h>
#include "d4def.h"
#ifdef STANDARD
#include <io.h>
#else
#include <fcntl.h>
#endif

#ifdef ACDLL
#undef d_report
#define d_report pdbt->pdbg->d_report
#endif

int DECLARE dTmemosz(
#ifndef unix 

	/* input */		/* d_report arrangement: 2081, 2082 */
	DBT   dbtptr,
	CHAR_PTR memofield,

	/* output */
	LONG_PTR size)
#else  /* unix */ 
	
 	dbtptr, memofield, size ) 
	DBT dbtptr; 
	CHAR_PTR memofield; 
	LONG_PTR size; 
#endif /* unix */ 

{
int rc;

if (pdbt->flag != OPEN)
{
    d_report = 2081;
    return(dNOOPEN);
}
if (pdbt->version != 4)
{
    d_report = 2082;
    return(dILLEGAL);
}

rc = _dTmmsz(dbtptr, memofield, size);

return(rc);

} /* end of dTmemosz() */

int _DECLARE _dTmmsz(
#ifndef unix 
  /* d_report arrangement: 2083 */
	/* input */
	DBT   dbtptr,
	CHAR_PTR memofield,

	/* output */
	LONG_PTR size)
#else  /* unix */ 
	
 	dbtptr, memofield, size ) 
	DBT dbtptr; 
	CHAR_PTR memofield; 
	LONG_PTR size; 
#endif /* unix */ 

{
int rc;
CHAR_PTR buff;

rc = _dmmseek(dbtptr, memofield);
if (rc != SUCCESS) return(rc);

if (_ACread(pdbt->handle, pdbt->dbtbuf, (unsigned) 8) != 8)
{
    d_report = 2083;
    return(dIOERR);
}
buff = pdbt->dbtbuf;

if ((unsigned char) *buff != (unsigned char) 0xff)
{
    d_report = 2084;
    return(dBADFILE); /* not DBT of dBASE IV */
}

*size = _4bytes(buff + 4) - 8;

return(SUCCESS);
} /* end of _dTmmsz() */

