/************
* dNflush.c *
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
 
#include <stdlib.h>
#include "db4.h"
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report nptr->pdbg->d_report
#endif

int DECLARE dNflush(
#ifndef unix 
NDX ndxptr)
#else  /* unix */ 
	ndxptr ) 
	NDX ndxptr; 
#endif /* unix */ 
 /*** d_report arrangement: 3100 ***/
{
#define MBUFSIZE 8
char minibuf[MBUFSIZE];
PXDXNOD nodeptr;
int i, rc;
PINDEX  pindex;
PCOMDAT pxcom;
PADIOBUFF pio;

pindex = &(nptr->ndx);
pio = &(nptr->ndxbuff);
pxcom = &(pindex->xdxcdata);
if (pxcom->flag != OPEN)
{
    d_report = 3101;
    return(dNOOPEN);
}

for (i=1, nodeptr=pio->xnodes; i <= pio->potatnum; i++, nodeptr++)
{
    if (!(nodeptr->bufflag & ACWRITE)) continue;

    rc = _dseekwt(pxcom->handle, (long) nodeptr->nodeid << SNODSHIFT,
		  nodeptr->nodebuf, (unsigned) NDXNODSZ);
    if (rc != SUCCESS)
    {
	d_report = 3102;
	return(dIOERR);
    }

    nodeptr->bufflag = ACREAD;
    if (nodeptr->nodeid > pio->lstpnode) pio->lstpnode = nodeptr->nodeid;
} /* end of for loop */

if (pxcom->changed)
{ /* current rec. # != original # */
    _bytes4(pindex->root, minibuf);
    _bytes4(pxcom->eofnode, minibuf + EOFNODE);

    rc = _dseekwt(pxcom->handle, (long) 0, minibuf, MBUFSIZE);
    if (rc != SUCCESS)
    {
	d_report = 3103;
	return(dIOERR);
    }

    pxcom->changed = 0;
}

return(SUCCESS);
} /* end of dNflush() */
