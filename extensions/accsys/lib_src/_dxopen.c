/************
* _dXopen.c *
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

#include "db4.h"
#include <stdlib.h>
#include "d4def.h"
#ifdef STANDARD
#include <io.h>
#else
#include <fcntl.h>
#endif

MDX _DECLARE _dXopen(filename,mode,buffs)
		/*** d_report assignment: 10360 ***/
	/* inputs */
#ifdef ACDLL
	DBGDAT_PTR dbgptr,
#endif
	CHAR_PTR filename; /* character string that represents the name of
			   MDX to open */
	int  mode;     /* file open mode */
        int  buffs; /* number of buffers to be used for the table file */
{
PMDXFILE pmdx; /* MDX descriptor */
PADIOBUFF pio;
int i;  /* general purpose integer */
int blksz; /* block size */
int fh; /* file handle number */
#define MINSIZE 0x30
char minibuf[MINSIZE];
CHAR_PTR memory; /* memory allocated for .DB or .Xnn */
unsigned int memsize; /* memory size */
PXDXNOD nodeptr;
PPOTATO potptr;
long lrc;


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
    return((MDX) 0);
}

if (buffs < 4) buffs = 4;

lrc = _dXbuffs(
#ifdef ACDLL
		dbgptr,
#endif
		fh, buffs, &memsize);
if (!memsize)
{
    (void) _dcls(fh);
    dretcode = (int) lrc;
    return( (MDX) 0);
}
 
#ifdef sun4
    memsize += EXTRA_MEMORY_FOR_ALLIGNMENTS;
#endif  /* sun4 */

i = _dseekrd(fh, (long) 0, minibuf, MINSIZE);
if (i != SUCCESS)
{
    (void) _dcls(fh);
    d_report = 10363;
    dretcode = i;
    return((MDX) 0); /* read() failed: not too much to do */
}
/* Header was read.  Let's start to fill in the MDXFILE structure. */

memory = (CHAR_PTR) _ACalloc((unsigned long) memsize);
if (!memory)
{
    (void) _dcls(fh);
    d_report = 10364;
    dretcode = dMEMERR;
    return( (MDX) 0);
}

 
#ifdef sun4
    MEMORY_IN_LONG_BOUNDARY(memory,memsize)
#endif  /* sun4 */
 
pmdx = (PMDXFILE) memory;
memory += SZMDXFILE;
memsize -= SZMDXFILE;

pmdx->handle = fh; /* file handle */
pmdx->flag   = CLOSED; /* will be OPENed later */
pmdx->fmode = mode;
pmdx->atbl = (PTBLFILE) 0; /* no DBF is attached */
pmdx->indexes = (int) _4bytes(minibuf + TAGNUMS); /* # indexes */
pmdx->freelist.mdxeof = _4bytes(minibuf + MDXENODE);
pmdx->freelist.ghead =  _4bytes(minibuf + MDXGHEAD);
pmdx->freelist.gnums =  _4bytes(minibuf + MAVLNODE);
pmdx->freelist.updated = 0; /* no global header change, yet */

/* fill in I/O buffer structure */
blksz = (int) *(minibuf + BLKSIZ) & 0x00ff;
if (blksz < 2) blksz = 2;
else if (blksz > 32) blksz = 32;
pmdx->freelist.blksiz = blksz;
blksz <<= SNODSHIFT;

#ifdef sun4
    MEMORY_IN_LONG_BOUNDARY(memory,memsize)
#endif  /* sun4 */
 
pio = &(pmdx->mdxbuff);
pio->xnodes = (PXDXNOD) memory;
pio->xnodes = (PXDXNOD) memory;
for (nodeptr = (PXDXNOD) memory, i = 1;  i <= buffs; nodeptr++, i++)
{
	nodeptr->indexid = NIL; /* unused */
	nodeptr->nodeid = (long) 0;
	nodeptr->bufflag = UNUSED;
	nodeptr->has = 0;
	nodeptr->nodebuf = (CHAR_PTR) _ACalloc((unsigned long) blksz);
	if (!(nodeptr->nodebuf))
	{
            _dfrexm(--nodeptr, --i);
            _ACfree( (CHAR_PTR) pmdx);
	    (void) _dcls(fh);
	    d_report = 10365;
	    dretcode = dMEMERR;
	    return( (MDX) 0);
	}
	memory += SZXDXNOD;
        memsize -= SZXDXNOD;
}

/* set up POTATO instances */
pio->potatnum = buffs;
pio->lstpnode = pmdx->freelist.mdxeof - pmdx->freelist.blksiz;
						/* actually -1 */

#ifdef sun4
    MEMORY_IN_LONG_BOUNDARY(memory,memsize)
#endif  /* sun4 */
 
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
pmdx->headiidx = (PIIDXMDX) 0;
pmdx->tailiidx = (PIIDXMDX) 0;

#ifdef ACDLL
pmdx->pdbg = dbgptr;
#endif

pmdx->flag = OPEN;

return((MDX) pmdx);

} /* end of _dXopen() */
