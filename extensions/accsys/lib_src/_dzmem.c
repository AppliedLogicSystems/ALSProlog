/************
 * _dzmem.c *
 ************/

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
#include <stdlib.h>
#include "db4.h"
#include "d4def.h"
#include "dzinclud.h" /* this must be before <alloc.h> (for TURBO) */

#ifndef unix
#ifndef __HIGHC__
#ifndef _WINDOWS
#ifdef MSC
#include <malloc.h>
#define _ACmemavl _memavl
#endif
#ifdef TURBOC
#include <alloc.h>
unsigned int _DECLARE _ACmemavl();
#endif
#ifdef ZORTECH
#define WMEMSIZE 60000
unsigned int _DECLARE _ACmemavl();
#endif
#ifdef LATTICE
unsigned int _DECLARE _ACmemavl();
#endif
#else
#define WMEMSIZE 60000
unsigned int _DECLARE _ACmemavl();
#endif
#else 	/* __HIGHC__ */
#define _ACmemavl _memavl
#endif 	/* __HIGHC__ */
#else /* unix */
#define _ACmemavl _memavl
#endif 	/* unix */

#define MINSIZE 8192
#define PTRLEN (sizeof(CHAR_PTR))

long _DECLARE attchmem(
#ifndef unix 
PSORTGDAT sgp, unsigned int memsize)
#else  /* unix */ 
	 ) 
#endif /* unix */ 
;

/*
   Getismem() first attempts to get memory for internal sort.
   If it fails, it attemps to get memory for external sort.
   Getismem() also indicates it set up for internal or external sort.
*/

int _DECLARE getismem(
#ifndef unix 

        /* input */  /* d_report: 10640+ */
#ifdef ACDLL
	DBGDAT_PTR dbgptr,
#endif
	PSORTGDAT sgp,
        long filelen,  /* file length */
        long items,    /* number of items in original file */

        /* output */
        INT_PTR external,
        UINT_PTR blocks   /* number of blocks */
		)
#else  /* unix */ 
	
 	sgp, filelen, items, external, blocks ) 
	PSORTGDAT sgp; 
	long filelen; 
	long items; 
	INT_PTR external; 
	UINT_PTR blocks; 
#endif /* unix */ 

{
unsigned int memsize;
CHAR_PTR memory;
long lll;
int tryagain = 0;

memsize = _ACmemavl() & ~0x0f;

#ifdef DEBUG
printf("\ngetismem() first _ACmemavl()::   memsize=%u", memsize);
#endif
if (memsize < MINSIZE) 
{
    d_report = 10641;
    return(dMEMERR);
}

*external = 0; /* let's pray for internal sort */
*blocks = K;

if (filelen > (long) memsize)
{
    *external = 1; /* no chance of sorting in memory */
}
else
{
#ifdef DEBUG
printf("\A");
#endif
    /* there is a good chance of simple internal sort */

    memsize = (unsigned int) filelen;
    memory = _ACalloc((unsigned long) memsize);
    if (memory)
    {
#ifdef DEBUG
printf(" B");
#endif
        leafs[0].leafmem = memory;
        leafs[0].msize = memsize;
        sortbuf = _ACalloc((unsigned long) memsize);
        if (!sortbuf)
        {
#ifdef DEBUG
printf(" C");
#endif
            _ACfree(memory);
            tryagain = 1;
        }
        else
        {   /* 'sortbuf' successfully obtained memory */
#ifdef DEBUG
printf(" D");
#endif
            sortptr = (pCHAR_PTR) _ACalloc( (unsigned long)
                                ( (memsize / PTRLEN) + 1 ) * PTRLEN);
            if (!sortptr)
            {
#ifdef DEBUG
printf(" E");
#endif
                _ACfree(sortbuf);
                _ACfree(memory);
                tryagain = 1;
            }
            else
            {
#ifdef DEBUG
printf(" F");
#endif
                *blocks = 1;
                return(SUCCESS);
            }
        }
    } /* end of if (memory).... */

    if (tryagain)
    {
#ifdef DEBUG
printf(" G");
#endif
        /* Simple internal sort cannot be done.
           Still try sort in memory */
        
        memsize  = (unsigned int) (((items / 9) + 8) * itemlen);
                        /* (divide by 9) + (max. mod value) */
        if ( memsize < (itemlen << 2) )
        {
#ifdef DEBUG
printf(" H");
#endif
            /* give up; use disk */
            *external = 1;
        }
        else
        {
#ifdef DEBUG
printf(" I");
#endif
            while (1) /* dummy loop */
            {
                lll = attchmem(sgp, memsize);
		if (lll)
                {
                    /* give up; use disk */
                    *external = 1;
                    break;
                }
#ifdef DEBUG
printf(" J");
#endif
                sortbuf = _ACalloc((unsigned long) memsize);
                if (sortbuf)
                {
#ifdef DEBUG
printf(" K");
#endif
                    sortptr = _ACalloc(
		    	(unsigned long) ( (memsize / PTRLEN) + 1 ) * PTRLEN);
                    if (sortptr) break; /* will be sorted in memory only */

                    _ACfree(sortbuf);
                }

#ifdef DEBUG
printf(" L");
#endif
                freeallm(sgp, K - 1);
                *external = 1;
                break;
            } /* end of while (1) loop */

        } /* end if if-else.... */

    } /* end of if (tryagain) ...... */
} /* end of if (filelen <= (long) memsize) else  ..... */

if (*external)
{
#ifdef DEBUG
printf("\nM");
#endif

    memsize = _ACmemavl();

    while (1)
    {
        if (memsize < (itemlen * 4))
	{
	    d_report = 10642;
            return(dMEMERR); /* severe memory shortage */
	}

#ifdef DEBUG
printf(" N");
#endif
        memory = _ACalloc((unsigned long) memsize);
	if (!memory)
        {
#ifdef DEBUG
printf("O");
#endif
            memsize = memsize - ((memsize >> 1) & 0x7fff);
            continue;
        }
#ifdef DEBUG
printf(" P");
#endif
        leafs[0].leafmem = memory;
        leafs[0].msize = memsize;
        sortbuf = _ACalloc((unsigned long) memsize);

        if (!sortbuf)
        {
#ifdef DEBUG
printf(" Q");
#endif
            _ACfree(memory);
            memsize = memsize - ((memsize >> 1) & 0x7fff);
            continue;
        }
        
#ifdef DEBUG
printf(" R");
#endif

        /* 'memory' && 'sortbuf' successfully obtained memory */
        sortptr = _ACalloc
		((unsigned long) ( (memsize / PTRLEN) + 1 ) * PTRLEN);
        if (!sortptr)
        {
#ifdef DEBUG
printf(" S");
#endif
                _ACfree(sortbuf);
                _ACfree(memory);
                memsize = memsize - ((memsize >> 1) & 0x7fff);
                continue;
        }
        break;

    } /* end of while (1) loop */

#ifdef DEBUG
printf(" T");
#endif

} /* end of if (*external) ..... */

return(SUCCESS);
} /* end of getismem() */


int _DECLARE getxsmem(
#ifndef unix 
		/* d_report: 10650+ */
#ifdef ACDLL
		DBGDAT_PTR dbgptr,
#endif
		PSORTGDAT sgp)
#else  /* unix */ 
	sgp ) 
	PSORTGDAT sgp; 
#endif /* unix */ 

        /* input: use global 'itemlen ' */

        /* output : none */
{
unsigned int memsize;
long lll;

memsize = _ACmemavl();

#ifdef DEBUG
printf("\ngetxsmem() pt. A::   memsize=%u", memsize);
#endif

if (memsize < TOUTMSIZ ||
	!(sortbuf = (CHAR_PTR) _ACalloc((unsigned long) TOUTMSIZ)))
{
    d_report = 10651;
    return(dMEMERR);
}

while (1)
{
#ifdef DEBUG
printf("\ngetxsmem() pt. B::   request memsize=%u", memsize);
#endif
    if (memsize < itemlen)
    {
	d_report = 10652;
        return(dMEMERR);
    }

    lll = attchmem(sgp, memsize);
    if (!lll) break; /* done */

    memsize = (unsigned int) (lll / 9);
#ifdef DEBUG
printf("\n   new memsize=%u lll=%ld", memsize, lll);
#endif
}

return(SUCCESS);
} /* end of getxsmem() */


void _DECLARE freeallm(
#ifndef unix 
PSORTGDAT sgp, int cnt)
#else  /* unix */ 
	
 	sgp, cnt ) 
	PSORTGDAT sgp; 
	int cnt; 
#endif /* unix */ 

{
    while (cnt >= 0)
    {
        _ACfree(leafs[cnt].leafmem);
        cnt--;
    }
} /* end of freeallm() */

static long _DECLARE attchmem(
#ifndef unix 
PSORTGDAT sgp, unsigned int memsize)
#else  /* unix */ 
	
 	sgp, memsize ) 
	PSORTGDAT sgp; 
	unsigned int memsize; 
#endif /* unix */ 

        /* input */
        /* unsigned int memsize; */

        /* returns: (long) 0 if successful
                    memory left, otherwise */
{
CHAR_PTR memory;
long totalmem;
int i;

totalmem = (long) 0; 
for (i=0; i < K; i++)
{
    if ((memsize > (unsigned int) _ACmemavl()) ||
    	!(memory = (CHAR_PTR) _ACalloc((unsigned long) memsize)))
    {
        freeallm(sgp, --i);
        return(totalmem);
    }
    leafs[i].leafmem = memory;
    leafs[i].msize = (memsize / itemlen) * itemlen;
                          /* normalization */
    totalmem += memsize;
} /* end of for-loop */
return( (long) 0);
} /* end of attchmem() */

#ifdef _WINDOWS
unsigned int _DECLARE _ACmemavl(
#ifndef unix 
)
#else  /* unix */ 
	 ) 
#endif /* unix */ 

{
    return((unsigned int) WMEMSIZE);
}

#else

#ifdef TURBOC
unsigned int _DECLARE _ACmemavl(
#ifndef unix 
)
#else  /* unix */ 
	 ) 
#endif /* unix */ 

{
long memleft;

memleft = coreleft();

if (memleft > (long) 0x0ffff) return(0x0ffff);

return( (unsigned int) memleft);

}
#endif

#ifdef ZORTECH
unsigned int _DECLARE _ACmemavl(
#ifndef unix 
)
#else  /* unix */ 
	 ) 
#endif /* unix */ 

{
    return((unsigned int) WMEMSIZE);
}
#endif

#ifdef LATTICE
unsigned int _DECLARE _ACmemavl(
#ifndef unix 
)
#else  /* unix */ 
	 ) 
#endif /* unix */ 

{
long memleft;

memleft = sizmem();

if (memleft > (long) 0x0ffff) return(0x0ffff);

return( (unsigned int) memleft);
}
#endif

#endif
