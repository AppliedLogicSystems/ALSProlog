/**************
 * _dmmseek.c *
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

#include <stdio.h>
#include <stdlib.h>

#include "d4def.h"

#ifdef STANDARD
#include <io.h>
#else
#include <fcntl.h>
#endif

#ifdef unix
#include <sys/types.h>
#include <unistd.h>
#endif 	/* unix */

#ifdef ACDLL
#undef d_report
#define d_report pdbt->pdbg->d_report
#endif

int _DECLARE _dmmseek(dbtptr,memofield)  /* d_report arrangement: 10160 */
	/* input */
	DBT  dbtptr;
	CHAR_PTR memofield;

	/* output: none */
{
long memoloc;
int i;

if (pdbt->flag != OPEN)
{
    d_report = 10161;
    return(dNOOPEN);
}

memoloc = (long) 0;
for (i=1; i<=10; i++)
{
    if (*memofield == ' ')
    {
	memofield++;
	continue;
    }
    if (*memofield < '0' || '9' < *memofield) break;
    memoloc = memoloc * 10 + (*memofield - '0');
    memofield++;
}

if (memoloc == (long) 0)
{
    d_report = 10162;
    return(dOUTRANGE); /* unused memofield */
}

if (lseek(pdbt->handle, memoloc * pdbt->blksiz, SEEK_SET) == (long) -1)
{
    d_report = 10163;
    return(dIOERR);
}

return(SUCCESS);
} /* end of _dmmseek() */

