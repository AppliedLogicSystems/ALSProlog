/************
* dXflush.c *
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
 
#include <stdio.h>
#include "db4.h"
#include "d4def.h"
#ifdef STANDARD
#include <io.h>
#else
#include <fcntl.h>
#endif

#ifdef ACDLL
#undef d_report
#define d_report mptr->pdbg->d_report
#endif

int DECLARE dXflush(
#ifndef unix 
MDX mdxptr)
#else  /* unix */ 
	mdxptr ) 
	MDX mdxptr; 
#endif /* unix */ 
 /*** d_report arrangement: 1140 ***/
{
PIIDXMDX pidx;
int i;
PADIOBUFF pio;
int changed;
unsigned nodesize;

pio = &(mptr->mdxbuff);
if (mptr->flag != OPEN)
{
    d_report = 1141;
    return(dNOOPEN);
}

nodesize = mptr->freelist.blksiz << SNODSHIFT;
i = _dxwtnod(
#ifdef ACDLL
	mptr->pdbg,
#endif
	mptr->handle, pio, nodesize, &changed);
if (i != SUCCESS) return(i);

pidx = mptr->headiidx;
while (pidx)
{
    i = _dxwtIhd((IDX) pidx);
    if (i != SUCCESS) return(i);
    if (pidx == mptr->tailiidx) break;
    pidx = pidx->nextidx;
}

if (changed || mptr->freelist.updated) i = _dxwtGhd(mdxptr);

return(i);
} /* end of dXflush() */

#ifdef ACDLL
#undef d_report
#define d_report iptr->amdx->pdbg->d_report
#endif

int _DECLARE _dXiflsh(
#ifndef unix 
IDX idxptr)
#else  /* unix */ 
	idxptr ) 
	IDX idxptr; 
#endif /* unix */ 
 /*** d_report arrangement: N/A ***/
{
int i;
PMDXFILE pmdx;
PADIOBUFF pio;
int changed;
unsigned nodesize;

pmdx = iptr->amdx;
pio = &(pmdx->mdxbuff);

nodesize = pmdx->freelist.blksiz << SNODSHIFT;
i = _dxwtnod(
#ifdef ACDLL
	pmdx->pdbg,
#endif
	pmdx->handle, pio, nodesize, &changed);
if (i != SUCCESS) return(i);

i = _dxwtIhd(idxptr);
if (i != SUCCESS) return(i);

if (changed || pmdx->freelist.updated) i = _dxwtGhd((MDX) pmdx);

return(i);
} /* end of _dXiflsh() */

#ifdef ACDLL
#undef d_report
#define d_report mptr->pdbg->d_report
#endif

/* write MDX global header */
int _DECLARE _dxwtGhd(
#ifndef unix 
MDX mdxptr)
#else  /* unix */ 
	mdxptr ) 
	MDX mdxptr; 
#endif /* unix */ 

{
#define MINI16 16
char minibuf[MINI16];
int i;
int year, month, day;

*minibuf = MDXTYPE;
dUtoday(&month, &day, &year);
*(minibuf + YEAR) = (char) ((year - 1900) & 0x00ff);
*(minibuf + MONTH) = (char) month;
*(minibuf + DAY) = (char) day;
i = _dseekwt(mptr->handle, (long) 0, minibuf, 4);
if (i != SUCCESS)
{
	d_report = 1142;
	return(dIOERR);
}

_bytes4( (long) mptr->indexes, minibuf);
_bytes4(mptr->freelist.mdxeof, minibuf + 4);
_bytes4(mptr->freelist.ghead, minibuf + 8);
_bytes4(mptr->freelist.gnums, minibuf + 12);
i = _dseekwt(mptr->handle, (long) TAGNUMS, minibuf, MINI16);
if (i != SUCCESS)
{
	d_report = 1143;
	return(dIOERR);
}
return(SUCCESS);
} /* end of _dxwtGhd() */

#ifdef ACDLL
#undef d_report
#define d_report iptr->amdx->pdbg->d_report
#endif

/* write MDX individual IDX header */
int _DECLARE _dxwtIhd(
#ifndef unix 
IDX idxptr)
#else  /* unix */ 
	idxptr ) 
	IDX idxptr; 
#endif /* unix */ 

{
int i;
#define MINI8   8
#define MINI10 10
char minibuf[MINI10];
PCOMDAT comptr;
long   ioposition;

comptr = &(iptr->idx.xdxcdata);
if (comptr->changed)
{
    _bytes4(iptr->idx.root, minibuf);
    _bytes4(iptr->idx.bnumglst, minibuf + BNOGLIST);
    ioposition = iptr->hdrnode << SNODSHIFT;
    i = _dseekwt(comptr->handle, ioposition, minibuf, MINI8);
    if (i != SUCCESS)
    {
	d_report = 1146;
	return(i);
    }
   
    _bytes2(iptr->idx.hasix, minibuf);
    _bytes4(iptr->idx.tail, minibuf + 2);	/* ACTTAIL */
    _bytes4(iptr->idx.head, minibuf + 6);	/* ACTHEAD */
    ioposition = (iptr->hdrnode << SNODSHIFT) + IDXMARK;
    i = _dseekwt(comptr->handle, ioposition, minibuf, MINI10);
    if (i != SUCCESS)
    {
	d_report = 1147;
	return(i);
    }
    comptr->changed = 0;
}
return(SUCCESS);
} /* end of _dxwtIhd() */

#ifdef ACDLL
#undef d_report
#define d_report dbgptr->d_report
#endif

int _DECLARE _dxwtnod(
#ifndef unix 

#ifdef ACDLL
	DBGDAT_PTR dbgptr,
#endif
	/* inputs */
	int	 fn,
	PADIOBUFF pio,
	unsigned nodesize,
	/* output */
	INT_PTR changed)
#else  /* unix */ 
	
 	fn, pio, nodesize, changed ) 
	int fn; 
	PADIOBUFF pio; 
	unsigned nodesize; 
	INT_PTR changed; 
#endif /* unix */ 

{
PXDXNOD nodeptr;
int i;

*changed = 0;
for (i=1, nodeptr=pio->xnodes; i <= pio->potatnum; i++, nodeptr++)
{
    if (!(nodeptr->bufflag & ACWRITE)) continue;
    *changed = 1;
    if (_dseekwt(fn, (long) nodeptr->nodeid << SNODSHIFT,
    		 nodeptr->nodebuf, nodesize) != SUCCESS)
    {
	d_report = 1148;
	return(dIOERR);
    }
    nodeptr->bufflag = ACREAD;
    if (nodeptr->nodeid > pio->lstpnode) pio->lstpnode = nodeptr->nodeid;
} /* end of for loop */
return(SUCCESS);
} /* end of _dxwtnod() */
