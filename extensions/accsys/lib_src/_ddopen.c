/************
* _dDopen.c *
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

#include <stdio.h>

#include <string.h>
#include <stdlib.h>
#include "db4.h"
#include "d4def.h"

#ifdef STANDARD
#include <io.h>
#else
#include <fcntl.h>
#endif

#ifdef ZORTECH 
void * __CDECL memccpy
	(void *to, const void *from, int value, unsigned int len);
#endif
#ifdef MacOS
void *memccpy(void *to, const void *from, int value, unsigned int len);
#endif


#ifndef ACDLL
DBF _DECLARE _dDopen(filename,mode,buffs)
		/*** d_report assignment: 10040 ***/
		/* inputs */
#else
void FAR PASCAL _ACPident(CHAR_PTR identifier);
DBF _DECLARE _dDopen(
		/*** d_report assignment: 10040 ***/
		/* inputs */
	DBGDAT_PTR dbgptr,
#endif
	CHAR_PTR filename; 	/* character string that represents the name of
			      			table or Xnn file to open */
	int  mode;  		/* file open mode */
    int  buffs; 		/* number of buffers to be used for the table file */
{
	PTBLFILE ptbl; 		/* table descriptor (for .DB file) */
	int i;  			/* general purpose integer */
	unsigned uuu; 		/* general purpose unsigned integer */
	int fh;				/* file handle number */
	CHAR_PTR imem;  	/* input memory */
	CHAR_PTR memory; 	/* memory allocated for .DB or .Xnn */
	unsigned int memsize; /* memory size */
	unsigned int isize; /* input size */
	int strat;
	PFLDTL fldtlptr;
	PFLDNM  fldnmptr;
	CHAR_PTR bufptr;
	PTBLNOD nodeptr;
	PPOTATO potptr;
	PCOMDAT comptr;
	long lrc;

#ifdef sun4

	int asun4int;

#define EXTRA_MEMORY_FOR_ALLIGNMENTS 	16

#define MEMORY_IN_LONG_BOUNDARY(m,s) 				\
	{												\
		asun4int=((int)m & 0x03); 					\
		if ( asun4int != 0 ) {						\
			m += (sizeof(long) - asun4int);			\
			s += (sizeof(long) - asun4int) ;		\
		}											\
	}

#endif 	/* sun4 */

#ifdef ACDLL
	char ver[20];
	_ACPident((CHAR_PTR) ver);
	if (strcmp(ver, dversion))
	{
    	d_report = 10042;
    	dretcode = dMISMATCH;
    	return( (DBF) 0);
	}
#endif

	fh = _dopn(
#ifdef ACDLL
			dbgptr,
#endif
			filename, mode);

	if (fh < 0)
	{
    	dretcode = fh;
    	return((DBF) 0);
	}

	if (buffs < 1) buffs = 1;
	strat = d_BUF;

#ifndef ACDLL
	lrc = _dDbuffs(fh, buffs, &memsize, &i);
#else
	lrc = _dDbuffs(dbgptr, fh, buffs, &memsize, &i);
#endif

	if (!memsize)
	{
    	(void) _dcls(fh);
    	dretcode = (int) lrc;
    	return( (DBF) 0);
	}

#ifdef sun4
	memsize += EXTRA_MEMORY_FOR_ALLIGNMENTS;
#endif 	/* sun4 */

	isize = 0x20 * (i + 1);
	imem = (CHAR_PTR) _ACalloc((unsigned long) isize);
	memory = (CHAR_PTR ) _ACalloc((unsigned long) memsize);

	if (!imem || !memory)
	{
    	(void) _dcls(fh);
    	d_report = 10043;
    	dretcode = dMEMERR;
    	return( (DBF) 0);
	}

#ifdef sun4
	MEMORY_IN_LONG_BOUNDARY(memory,memsize)
#endif 	/* sun4 */

	ptbl = (PTBLFILE) memory;
	memory += SZTBLFILE;
	memsize -= SZTBLFILE;

	ptbl->fldsnum = i; /* i contains no. of fields */

	comptr = &(ptbl->tblcdata);
	comptr->flag = CLOSED; /* will be open shortly */

	comptr->handle = fh; /* file handle */

	i = _dseekrd(fh, (long) 0, imem, isize);


	if (i != SUCCESS)
	{
        _ACfree((CHAR_PTR ) ptbl);
		_ACfree(imem);
		(void) _dcls(fh);
		d_report = 10044;
        dretcode = i;
		return((DBF) 0); /* read() failed: not too much to do */
	}

	/* Header was read.  Let's start to fill in the TBLFILE structure. */
	if (imem[ENCRYPT])
	{
        _ACfree((CHAR_PTR ) ptbl);
		_ACfree(imem);
		(void) _dcls(fh);
		d_report = 10045;
        dretcode = dENCRYPT;
		return((DBF) 0); /* DBF encrypted; don't violate the security */
	}

	bufptr = imem;

	comptr->ftype = (int) *bufptr++ & 0x00ff;
	ptbl->year = *bufptr++;
	ptbl->month = *bufptr++;
	ptbl->day = *bufptr++;
	comptr->size = _4bytes(bufptr);
	bufptr += 4;
	ptbl->stadrs = (int) _2bytes(bufptr);
	bufptr += 2;
	comptr->itemlen = (int) _2bytes(bufptr);

	comptr->fmode = mode;
	comptr->special = 0;
	if (imem[MDXFLAG]) comptr->special = HASMDX;
	if ((comptr->ftype & isDBF) != isDBF)
	{
    	_ACfree((CHAR_PTR ) ptbl);
    	_ACfree(imem);
    	(void) _dcls(fh);
    	d_report = 10046;
    	dretcode = dBADFILE;
    	return( (DBF) 0);
	}

	/* set node size */
	comptr->nodesz = _Dnodesiz(comptr->itemlen);
	comptr->maxitems = comptr->nodesz / comptr->itemlen;
	comptr->eofnode = (comptr->size - 1) / comptr->maxitems + 2;

	comptr->changed = 0;
	comptr->curnode = (long) 0;

#ifdef sun4
	MEMORY_IN_LONG_BOUNDARY(memory,memsize)
#endif 	/* sun4 */

	/* construct array of fields */
	uuu = (unsigned) SZFLDNM * ptbl->fldsnum;
	ptbl->fldnames = (PFLDNM) memory;
	memory  += uuu;
	memsize -= uuu;

#ifdef sun4
	MEMORY_IN_LONG_BOUNDARY(memory,memsize)
#endif 	/* sun4 */

	uuu = (unsigned) SZFLDTL * ptbl->fldsnum;
	ptbl->fltyplen = (PFLDTL) memory;
	memory  += uuu;
	memsize -= uuu;

	bufptr = imem + FIELDS;
	/* read fields info (type and length) */
	for (fldnmptr=ptbl->fldnames, fldtlptr=ptbl->fltyplen, i=1;
     	i<=ptbl->fldsnum;
     	fldnmptr++, fldtlptr++, bufptr += NXTFLD, i++)
	{
		ACmemccpy(fldnmptr->fieldnm, bufptr, 0, MAXFLDNM + 1);
		fldtlptr->type = *(bufptr + FLDTYP);
		fldtlptr->length = *(bufptr + FLDLEN);
		fldtlptr->decimal = *(bufptr + FLDDEC);
	}

	/** reset table hash vector **/
	for (i = 0; i < ACCESSSZ; i++) ptbl->tblacc[i] = NIL;

	/*** set up the potato buffer chain ***/
	ptbl->potatnum = buffs;
	ptbl->lstpnode = comptr->eofnode - 1;

	/* set up TBLNOD instances */
	if (buffs)
	{

#ifdef sun4
	MEMORY_IN_LONG_BOUNDARY(memory,memsize)
#endif 	/* sun4 */

    	ptbl->nodes = (PTBLNOD) memory;
    	for (nodeptr = (PTBLNOD) memory; buffs > 0; nodeptr++, buffs--)
    	{
			nodeptr->bufflag = UNUSED;
			nodeptr->fstrec  = (long) 0;
        	nodeptr->has = 0;
			/* nodeptr->nodebuf = (CHAR_PTR ) 0; */

			memory += SZTBLNOD;
        	memsize -= SZTBLNOD;
    	}
	}

	ptbl->iostrat = strat;

	/* attach node buffer to TBLNOD instances */
	if (ptbl->potatnum)
	{
    	buffs = ptbl->potatnum;
    	for (nodeptr = ptbl->nodes, i=1; i <= buffs; nodeptr++, i++)
    	{
			nodeptr->nodebuf
				= (CHAR_PTR ) _ACalloc((unsigned long) (comptr->nodesz + 1));
					 /* 1 byte for end-marker */
        	if (!(nodeptr->nodebuf))
        	{
            	_dfredm(--nodeptr, --i);
            	_ACfree( (CHAR_PTR ) ptbl);
	    		_ACfree(imem);
	    		(void) _dcls(fh);
            	d_report =10047;
            	dretcode = dMEMERR;
            	return( (DBF) 0);
			}
    	} /* end of for-loop */

#ifdef sun4
	MEMORY_IN_LONG_BOUNDARY(memory,memsize)
#endif 	/* sun4 */

    	/* set up POTATO instances */
    	ptbl->potatoes = (PPOTATO) memory;
    	ptbl->mru   = (PPOTATO) memory;
    	buffs = ptbl->potatnum;
    	nodeptr = ptbl->nodes;
    	for (potptr = (PPOTATO) memory; buffs > 0; potptr++, buffs--)
    	{
			potptr->next = potptr + 1;
			potptr->prev = potptr - 1;
			potptr->bufptr = (CHAR_PTR ) nodeptr;

			memory += SZPOTATO;
        	memsize -= SZPOTATO;
			nodeptr++;
    	}
    	ptbl->lru = --potptr;
    	ptbl->potatoes->prev = (PPOTATO) 0;
    	potptr->next = (PPOTATO) 0;
	} /* end of if (ptbl->potatnum) ...... */


#ifdef ACDLL
	ptbl->pdbg = dbgptr;
#endif

	comptr->flag = OPEN; /* Now, this DBF is open */

	_ACfree(imem);

	return((DBF) ptbl);
} /* end of _dDopen() */



#if defined(ZORTECH) || defined(MacOS)
#ifdef ZORTECH
void * __CDECL memccpy(void *to, const void *from, int value, unsigned int len)
#else
void *memccpy(void *to, const void *from, int value, unsigned int len)
#endif
{
    char *orig, *dest;
    int found = 0;

    orig = (char *) from;
    dest = (char *) to;
    if (len <= 0) return(NULL);
    while(len--)
    {
	*dest++ = (char) *orig;
	if (*orig == value)
	{
	    found = 1;
	    break;
	}
	orig++;
    }
    return(found ? dest : (void *) 0);
} /* end of memccpy() */
#endif

