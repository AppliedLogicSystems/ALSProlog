/**************
 * _dbfndwt.c *
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
 
#include <stdlib.h>
#include <stdio.h>
#include "db4.h"
#include "d4def.h"

#ifndef unix
#ifdef STANDARD
#include <io.h>
#else
#include <fcntl.h>
#endif
#else 	/* unix */
#include <sys/types.h>
#include <unistd.h>
#endif 	/* unix */

/*
 * _dbfndwt() writes a node buffer for DBF.
 * If the node is the last node, it puts the end-marker.
 */
int _DECLARE _dbfndwt(dbfptr,nodptr,comptr)
		/* inputs */  /* d_report arrangement: N/A */
	DBF dbfptr; PTBLNOD nodptr; PCOMDAT comptr;
		/* outputs : none */
{
	unsigned wlen;

    if (lseek(comptr->handle,
	    (long) tblptr->stadrs
	    + (long) (nodptr->nodeid - 1) * comptr->nodesz, SEEK_SET)
	    == (long) -1)
	{
	    return(dIOERR);
	}
	wlen = nodptr->has * comptr->itemlen;

	if (nodptr->nodeid == (comptr->eofnode - 1))
	{	/* last node */
		*(nodptr->nodebuf + wlen) = ENDMARK;
		wlen++;
	}

	if (_ACwrite(comptr->handle, nodptr->nodebuf, wlen) != wlen)
	{
	    return(dIOERR);
	}
	nodptr->bufflag &= ~ACWRITE;
	if (nodptr->nodeid > tblptr->lstpnode)
			tblptr->lstpnode = nodptr->nodeid;
	return(SUCCESS);
} /* end of _dbfndwt() */
