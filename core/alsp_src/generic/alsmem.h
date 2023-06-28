/*=============================================================*
 |			alsmem.h	
 |		Copyright (c) 1993-5, Applied Logic Systems, Inc.
 |
 |			-- header information for the als_mem variable.
 |
 | Author: Kevin A. Buettner
 | Created: 11/30/93
 |
 | Note:  This file should be included by mem.c and by any als-mics
 | programs having need for the information in the header.  It should
 | not be needed by the other files in the Prolog system.
 *=============================================================*/
#ifndef ALSMEM_H_INCLUDED
#define ALSMEM_H_INCLUDED

#ifdef KERNAL
#define AM_BLOCKSIZE		0x00038000
#define AM_MALLOCSPACE		1
#define AM_MAXBLOCKS		32
#define AM_MAXGLOBALS		64
#else
/* AM_BLOCKSIZE is the default block size to allocate */
#define AM_BLOCKSIZE		0x00200000	/* about 2 meg */

/* AM_MALLOCSPACE is the space we attempt to leave malloc with at 
   initialization */
#define AM_MALLOCSPACE		65536

/* Maximum number of blocks which we will permit to be allocated.  
   Note that each block is huge.  The number below should be more
   than ample. */
#define AM_MAXBLOCKS		128

/* Maximum number of global variables which we may register to be
   saved.  These should not refer to dynamically allocated memory.  */
#define AM_MAXGLOBALS		64
#endif /* KERNAL */

struct am_header {

	/* number of blocks allocated */
	long	nblocks;

	/* pointer to the free list */
	long	*freelist;

	/* total space allocated (all blocks) */
	long	totsize;

	/* number of globals */
	long	nglobals;


	/* Integrity information to make sure saved state matches image */

	long **integ_als_mem;		/* address of als_mem variable */
	int (*integ_als_mem_init) (const char *, long);
					/* address of als_mem_init */
	int (*integ_w_unify) ( PWord, int, PWord, int );
					/* address of w_unify */
	char integ_version_num[128];	/* version number information */
	char integ_processor[128];	/* name of processor */
	char integ_minor_os[128];	/* OS name */

	/* the block information */
	struct {
	    long *start;		/* start of block */
	    long asize;			/* allocated size in memory */
	    long fsize;			/* size of block in file */
	} blocks[AM_MAXBLOCKS];

	/* the globals information */
	struct {
	    long *addr;
	    long value;
	} globals[AM_MAXGLOBALS];
};

#define FB_SIZE(p)  (* (long *) p)
#define FB_NEXT(p)  (* ((long **) p + 1))

#endif /* ALSMEM_H_INCLUDED */
