/************
* _dNopen.c *
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
 

#include "db4.h"
#include <stdlib.h>
#include <string.h>
#include "d4def.h"
#ifdef STANDARD
#include <io.h>
#else
#include <fcntl.h>
#endif

#ifndef ACDLL
NDX _DECLARE _dNopen(filename,mode,buffs)
#else
NDX _DECLARE _dNopen(DBGDAT_PTR dbgptr,
#endif
		/*** d_report assignment: 10180 ***/
	/* inputs */
	CHAR_PTR filename; /* character string that represents the name of
			   table or Xnn file to open */
	int  mode;   /* file open mode */
        int  buffs;   /* number of buffers to be used for the table file */
{
PNDXFILE pndx; /* NDX descriptor */
PINDEX   pindex;
int i;  /* general purpose integer */
int fh; /* file handle number */
#define MINSIZE 0x100
char minibuf[MINSIZE];
CHAR_PTR memory; /* memory allocated for .DB or .Xnn */
unsigned int memsize; /* memory size */
PXDXNOD nodeptr;
PPOTATO potptr;
PCOMDAT comptr;
long lrc;
PADIOBUFF pio;


#ifdef sun4

    int asun4int;

#define EXTRA_MEMORY_FOR_ALLIGNMENTS    16

#define MEMORY_IN_LONG_BOUNDARY(m,s)                \
    {                                               \
        asun4int=((int)m & 0x03);                   \
        if ( asun4int != 0 ) {                      \
            m += (sizeof(long) - asun4int);         \
            s += (sizeof(long) - asun4int) ;        \
        }                                           \
    }

#endif  /* sun4 */

fh = _dopn(
#ifdef ACDLL
		dbgptr,
#endif
		filename, mode);
if (fh < 0)
{
    dretcode = fh;
    return((NDX) 0);
}

if (buffs < 4) buffs = 4;

lrc = _dNbuffs(
#ifdef ACDLL
		dbgptr,
#endif
		fh, buffs, (UINT_PTR) &memsize);
if (!memsize)
{
    (void) _dcls(fh);
    dretcode = (int) lrc;
    return( (NDX) 0);
}

i = _dseekrd(fh, (long) 0, minibuf, MINSIZE);
if (i != SUCCESS)
{
    (void) _dcls(fh);
    d_report = 10183;
    dretcode = i;
    return((NDX) 0); /* _ACread() failed: not too much to do */
}
/* Header was read.  Let's start to fill in the NDXFILE structure. */

#ifdef sun4
    memsize += EXTRA_MEMORY_FOR_ALLIGNMENTS;
#endif  /* sun4 */

memory = (CHAR_PTR) _ACalloc((unsigned long) memsize);
if (!memory)
{
    (void) _dcls(fh);
    d_report = 10184;
    dretcode = dMEMERR;
    return( (NDX) 0);
}

#ifdef sun4
    MEMORY_IN_LONG_BOUNDARY(memory,memsize)
#endif  /* sun4 */

pndx = (PNDXFILE) memory;
memory += SZNDXFILE;
memsize -= SZNDXFILE;

pndx->atbl = (PTBLFILE) 0; /* no DBF is attached */
pndx->next = (PNDXFILE) 0; /* no other NDX attached */
pndx->prev = (PNDXFILE) 0; /* no other NDX attached */

/* fill in INDEX structure */
pindex = &(pndx->ndx);
comptr = &(pindex->xdxcdata); /* set up 'comptr' right now */

comptr->ftype = minibuf[ORDERBYTE];

#ifdef sun4
    MEMORY_IN_LONG_BOUNDARY(memory,memsize)
#endif  /* sun4 */

pindex->indexid = 0; /* NDX */
pindex->position = UNINIT;
pindex->root = _4bytes(minibuf);
pindex->keytype = (int) *(minibuf + XDXKTYPE) & 0x00ff;
pindex->exprlen = ACstrlen(minibuf + KEXPRESS);
pindex->express = memory;
ACmemcpy(memory, minibuf + KEXPRESS, pindex->exprlen + 1);
memory  += pindex->exprlen + 1;
memsize -= pindex->exprlen + 1;

if (!(pindex->keytype))
    pindex->keytype = (*(minibuf + NDXKTYPE)) ? 'N' : 'C';

switch(pindex->keytype)
{
case 'N':
case 'F':
case 'D':
    pindex->keycmp = _dncmp;
    break;

default:
    pindex->keycmp = ACmemCMP;
}

#ifdef sun4
    MEMORY_IN_LONG_BOUNDARY(memory,memsize)
#endif  /* sun4 */

pindex->complen = (int) *(minibuf + KCOMPLEN) & 0x00ff;
pio = &(pndx->ndxbuff);
pio->xnodes = (PXDXNOD) memory;
/* the following 2 elements need to be initialized right now */
comptr->itemlen = (int) _2bytes(minibuf + KEYLEN);
comptr->nodesz = SNODSIZ;

for (nodeptr = (PXDXNOD) memory, i = 1;  i <= buffs; nodeptr++, i++)
{
	nodeptr->indexid = 0; /* NDX */
	nodeptr->nodeid = (long) 0;
	nodeptr->bufflag = UNUSED;
	nodeptr->has = 0;
	nodeptr->nodebuf
	    = (CHAR_PTR) _ACalloc((unsigned long) (comptr->nodesz));
	if (!(nodeptr->nodebuf))
	{
            _dfrexm(--nodeptr, --i);
            _ACfree( (CHAR_PTR) pndx);
	    (void) _dcls(fh);
	    d_report = 10185;
	    dretcode = dMEMERR;
	    return( (NDX) 0);
	}
	memory += SZXDXNOD;
        memsize -= SZXDXNOD;
}
for (i = 0; i < ACCESSSZ; i++)
{
    pindex->xdxacc[i].nodeid = (long) 0;
    pindex->xdxacc[i].potatoid = NIL;
    pindex->xdxacc[i].offset = (unsigned short) 0;
}

#ifdef sun4
    MEMORY_IN_LONG_BOUNDARY(memory,memsize)
#endif  /* sun4 */

pindex->leaf = pindex->work = (PXDXVEC) 0;
pindex->currecno = (long) 0;
pindex->curkey = memory;
memory += comptr->itemlen;
memsize -= comptr->itemlen;

/* fill in INDEX structure */

/* fill in the COMDAT structure */
comptr->flag = CLOSED; /* will be open shortly */
comptr->handle = fh;
comptr->special = 0; /* nothing special, yet */
/* comptr->ftype = minibuf[ORDERBYTE];  -- done earlier */
comptr->fmode = mode; /* initially open for reading */
comptr->size = (long) 0;
comptr->eofnode = _4bytes(minibuf + EOFNODE);
/* comptr->itemlen = (int) _2bytes(minibuf + KEYLEN); -- already done */
/* comptr->nodesz = SNODSIZ; -- already done */
comptr->maxitems = (int) *(minibuf + MAXKEYS) & 0x00ff;
comptr->changed = 0; /* nothing changed yet */
comptr->curnode = (long) 0;

/* set up POTATO instances */

#ifdef sun4
    MEMORY_IN_LONG_BOUNDARY(memory,memsize)
#endif  /* sun4 */

pio->potatnum = buffs;
pio->lstpnode = comptr->eofnode - 1;
pio->potatoes = (PPOTATO) memory;
pio->mru   = (PPOTATO) memory;
nodeptr = pio->xnodes;
for (potptr = (PPOTATO) memory; buffs > 0; potptr++, buffs--)
{
	potptr->next = potptr + 1;
	potptr->prev = potptr - 1;
	potptr->bufptr = (CHAR_PTR) nodeptr;

	memory += SZPOTATO;
        memsize -= SZPOTATO;
	nodeptr++;
}
pio->lru = --potptr;
pio->potatoes->prev = (PPOTATO) 0;
potptr->next = (PPOTATO) 0;

#ifdef ACDLL
pndx->pdbg = dbgptr;
#endif

comptr->flag = OPEN; /* Now, this NDX is open */

return((NDX) pndx);
} /* end of _dNopen() */
