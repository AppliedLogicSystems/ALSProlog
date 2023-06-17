/*===========================================================*
 |		wintcode.c
 |	Copyright (c) 1986-1995 Applied Logic Systems
 |
 |		-- Functions for manipulating the code area & relatives
 |
 | Author: Kevin A. Buettner
 | Creation Date: 8/14/86
 | 10/26/94 - C. Houpt -- Added prototypes for alignSystemStack() and
 |				fixSystemStack().  Also Various UCHAR* casts.
 |		 	-- Added pragmas to force some pointer returning functions
 |		 	   to use DO instead of A0 register under MetroWerks.
 *===========================================================*/
#include "defs.h"
#include <limits.h>
#include "wintcode.h"
#include "module.h"
#include "cutmacro.h"
#include "main.h"
#include "rinfo.h"
#include "icodegen.h"

#ifdef MacOS
#include <OSUtils.h>		/* For the FlushInstructionCache function */
#endif /* MacOS */

#if	defined(HAVE_MMAP) && (defined(HAVE_DEV_ZERO) || defined(HAVE_MMAP_ZERO))
#include <sys/mman.h>

#elif	defined(arch_m88k)  /* !HAME_MMAP */
extern	int	memctl		( void *, int, int );

#endif /* HAVE_MMAP */

#if 0
#ifndef MaxFunc
#define max(a,b) ((a) < (b) ? (b) : (a))
#endif /* MaxFunc */
#endif

#undef round
#define round(x,s) (((((long) x)-1) & ~(long)((s)-1)) + (s))

extern int system_debugging;

/*-----------------------------------------------------------------*
 * wm_aborted indicates whether an abort was done or not.
 *-----------------------------------------------------------------*/

PWord wm_aborted = 0;

long *aib_clause_addr;		/* used by assertz */

/*-----------------------------------------------------------------*
 * w_nametable is allocated to be an array of pointers to procedure entries
 *-----------------------------------------------------------------*/

/*//ntbl_entry **w_nametable = (ntbl_entry **) 0;*/

/*-----------------------------------------------------------------*
 | w_freeptr points to a chain of free code space blocks.
 | w_totspace keeps track of the total amount of space available
 | for allocation.  At present, this is used only for statistics.
 *-----------------------------------------------------------------*/

/*//static long *w_freeptr = (long *) 0;*/
/*//static long  w_totspace = 0;*/

/*//unsigned long w_timestamp = 0;*/
/*//unsigned long w_reconstamp = 0;*/

/*//static long *w_tofreelist = (long *) 0;*/	/* list of blocks to be freed */
/*//static long  w_tofreesize = 0;*/
				/* size (in longwords) of the blocks on the
				 * tofreelist
				 */

/*//static struct codeblock *codeblock_list = (struct codeblock *) 0;*/

static	long *	code_alloc	( size_t, int );
static	void	makerunable	( void );
static	void	makewritable	( void );
static	ntbl_entry * alloc_name_entry ( void );
static	long *	cbsffa		( Code * );
static	long *	clause_start_from_retaddr ( Code *, long * );
static	void	mark_fromretaddr ( Code * );
static	void	mark_clause	( long * );

/*-----------------------------------------------------------------*
 | code_alloc
 |
 | This function allocates a block of aligned memory and keeps track of
 | the starting address and the amount of space allocated in the block
 | so that makerunable and makewritable calls will be able to run.
 *-----------------------------------------------------------------*/

static long *
code_alloc(size, fe_num)
    size_t size;
    int fe_num;
{
    struct codeblock *newcb;

    newcb = (struct codeblock *) ss_malloc(sizeof (struct codeblock), fe_num);

    newcb->next = codeblock_list;
    codeblock_list = newcb;
    
    newcb->addr = ss_pmalloc(size, fe_num, &newcb->size);
	
	memset(newcb->addr, -1, newcb->size);
    return newcb->addr;
}

void dump_codespace(void);
void dump_codespace(void)
{
  struct codeblock *b;
  
  for (b = codeblock_list; b ; b = b->next) {
    long *p, size;
    printf("\nCode Block %p\n", b->addr);
    
    if (*(b->addr) == -1) {
      p = b->addr + 1;
      
      for (p = b->addr+1 ; *p != -1 ; p += size) {
	size = *p > 0 ? *p : -*p;
	if (*p > 0) printf("Block %p Length %ld\n", p, size);
      }
    }
  }
}



#ifdef PACKAGE
int
pckg_addto_codeblocks(addr, len)
    long *addr;
    int   len;
{
    struct codeblock *newcb;

    newcb = (struct codeblock *) ss_malloc(sizeof (struct codeblock),
					   FE_CODESPACE_INIT);

    newcb->next = codeblock_list;
    codeblock_list = newcb->next;

    newcb->addr = addr;
    newcb->size = len;

    return (1);
}
#endif /* PACKAGE */

/*-----------------------------------------------------------------*
 * makerunable calls memctl on each code block to make it executable.
 *-----------------------------------------------------------------*/

static void
makerunable()
{
#if !defined(Portable) && !defined(arch_sparc)	
			/* Portable doesn't need any of this stuff */
/* #if	defined(HAVE_MMAP) || defined(arch_m88k) */
#if	(defined(HAVE_MMAP) && (defined(HAVE_DEV_ZERO) || defined(HAVE_MMAP_ZERO))) || defined(arch_m88k)
    register struct codeblock *cbp;

    for (cbp = codeblock_list; cbp; cbp = cbp->next) {
#ifdef	HAVE_MMAP
	if ( mprotect((caddr_t)cbp->addr, (size_t)cbp->size,
	              PROT_READ | PROT_EXEC)) {
	    fprintf(stderr, "makerunable: mprotect error\n");
	    perror("mprotect");
	}
#else	/* inner !HAVE_MMAP */
	if (memctl(cbp->addr, cbp->size, 1) != 0) {
	    fprintf(stderr, "makerunable: memctl error\n");
	    perror("mctl");
	}
#endif 	/* inner HAVE_MMAP */
    }
#elif	defined(arch_m68k)
#ifdef NeXT					/* m68k OS's */
    /* Note: NeXT (if defined) will be predefined by the C-compiler. */
    asm("trap #2");
#elif defined(MacOS)		/* m68k OS's */
    FlushInstructionCache();
#else 						/* m68k OS's */
    flush_cache();
#endif						/* m68k OS's */
#elif	defined(arch_i386)
#ifdef DOS
    refresh_code_window();
#endif			/* DOS */
#endif			/* (defined(HAVE_MMAP) && (de.... */
#endif 				/* not Portable and not arch_sparc */
}

/*-----------------------------------------------------------------*
 | makewritable calls memctl on each code block to make it writable
 *-----------------------------------------------------------------*/

static void
makewritable()
{
#if !defined(Portable) && !defined(arch_sparc)	
			/* Portable doesn't need any of this stuff */
/*#if	defined(HAVE_MMAP) || defined(arch_m88k) */
#if	(defined(HAVE_MMAP) && (defined(HAVE_DEV_ZERO) || defined(HAVE_MMAP_ZERO))) || defined(arch_m88k)
    register struct codeblock *cbp;

    for (cbp = codeblock_list; cbp; cbp = cbp->next) {
#ifdef 	HAVE_MMAP
	if (mprotect((caddr_t)cbp->addr, (size_t) cbp->size,
	             PROT_READ | PROT_WRITE) != 0) {
	    fprintf(stderr, "makewritable: mprotect error\n");
	    perror("mprotect");
	}
#else	/* !HAVE_MMAP */
	if (memctl(cbp->addr, cbp->size, 2) != 0) {
	    fprintf(stderr, "makewritable: memctl error\n");
	    perror("mctl");
	}
#endif 	/* HAVE_MMAP */
    }
#endif				/* (defined(HAVE_MMAP) && ....) || arch_m88k */
#endif 				/* not Portable and not arch_sparc */
}


dbprot_t
w_dbprotect(newstate)
    dbprot_t newstate;
{
    static dbprot_t state = DBRS_WRITABLE;
    dbprot_t oldstate;

    oldstate = state;
    if (state != newstate) {
	switch (newstate) {
	    case DBRS_WRITABLE :
		makewritable();
		break;
	    case DBRS_RUNABLE :
		makerunable();
		break;
	}
	state = newstate;
    }
    return oldstate;
}

/*-----------------------------------------------------------------*
 | alloc_name_entry returns a pointer to a new (uninitialized) name entry.
 *-----------------------------------------------------------------*/

#ifdef KERNAL
#define NENT_ALLOCQUANT 64
#else
#define NENT_ALLOCQUANT 1024
#endif /* KERNAL */
/*//static ntbl_entry *ane_entbase = (ntbl_entry *) 0;*/
/*//static ntbl_entry *ane_entptr = (ntbl_entry *) 0;*/

static ntbl_entry *
alloc_name_entry()
{

    if (ane_entbase == ane_entptr) {

	ane_entbase = (ntbl_entry *) 
		code_alloc(NENT_ALLOCQUANT * sizeof (ntbl_entry), FE_XMEM_NTBL);

	memset((char *) ane_entbase, -1 , NENT_ALLOCQUANT * sizeof (ntbl_entry));
	ane_entptr = ane_entbase + NENT_ALLOCQUANT;
    }

    return (--ane_entptr);
}

/*-----------------------------------------------------------------*
 | Look up an entry in the name table. If the entry is there, the index
 | into the name table is returned and the user can use that to get the
 | pointer to the name table entry. An index is returned if the item
 | was not found, and this is the place to put a new entry. If a -1 
 | is returned, it is not there and there is no room for anything else.
 *-----------------------------------------------------------------*/

int
nameprobe(modid, tokid, arity)
    PWord modid, tokid;
    int   arity;
{
    register int i, s;
    register ntbl_entry *n;
    PWord tokid_arity;

    tokid_arity = MMK_FUNCTOR(tokid, arity);

    i = s = ((modid << 2) + tokid_arity + (tokid << 4)) & (NTBL_SIZE - 1);

    while ((n = w_nametable[i]) &&
	   !(n->modid == modid && n->tokid_arity == tokid_arity)) {
	i = (i + 1) & (NTBL_SIZE - 1);
	if (i == s) {
	    i = -1;
	    break;
	}
    }

    return i;
}

/*-----------------------------------------------------------------*
 | w_namelookup(modid,tokid,arity)
 | int modid, tokid, arity;
 |
 | Looks up and returns the procedure table index for the given
 | module, token index, and arity.  If an entry is not present,
 | one is created.
 *-----------------------------------------------------------------*/

int
w_namelookup(modid, tokid, arity)
    PWord modid, tokid;
    int   arity;
{
    int   i;
    ntbl_entry *n;

    /* If can find with the proper arity, return that */
    if ((i = nameprobe(modid, tokid, arity)) != -1 && w_nametable[i])
	return (i);

    /* If original call came out with a full table, then we must die, since
     * there is no room for a new entry.
     */
    if (i == -1)
	fatal_error(FE_FULL_NTBL, 0);

    /* Well, install it. */
    w_nametable[i] = n = alloc_name_entry();
    n->flags = NFLG_UNDEFINED;
    n->modid = modid;
    n->tokid_arity = MMK_FUNCTOR(tokid, arity);
    n->index_block = (long *) 0;
    n->timestamp = 0;
    n->lo_id = 0;
    n->hi_id = 0;
    if (isCutMacro(tokid, arity))
	n->nargs = arity + 1;
    else
	n->nargs = arity;

    n->first_clause = n->last_clause = (long *) 0;

    ic_install_overflow_call(n);
    ic_install_call_entry(n);
    ic_install_normal_exec_entry(n);
    ic_install_resolve_ref(n);

    return (i);
}

/*-----------------------------------------------------------------*
 | w_nameprobe
 |
 | Performs a similar function as w_namelookup, but will not create
 | an entry if the given procedure is not present.  It also returns
 | the entry pointer if the procedure was found, a null pointer if not.
 *-----------------------------------------------------------------*/

ntbl_entry *
w_nameprobe(modid, tokid, arity)
    PWord modid, tokid;
    int   arity;
{
    int   i;
    ntbl_entry *n;

    /* If can find with the proper arity, return that */
    if ((i = nameprobe(modid, tokid, arity)) != -1 && w_nametable[i])
	n = w_nametable[i];
    /* If call came out with a full table, then we must die, since there is
     * no room for a new entry.
     */
    else
	n = (ntbl_entry *) 0;

    return n;
}

ntbl_entry *
w_nameentry(modid, tokid, arity)
    PWord modid, tokid;
    int   arity;

{
    return w_nametable[w_namelookup(modid, tokid, arity)];
}

/*-----------------------------------------------------------------*
 | w_initcode builds the header block and initializes w_freeptr
 | to point to it.
 *-----------------------------------------------------------------*/

#define WC_HEADERSIZE 4

void
w_initcode()
{
    if (!w_freeptr) {
	long *headerblock;

	headerblock = ss_malloc(WC_HEADERSIZE * sizeof (long),
				FE_CODESPACE_INIT);

	/*
	 * initialize header block.  This block is a block which never
	 * actually gets allocated (because it is smaller than any
	 * request), but which is always present even when all of the
	 * space is exhausted.
	 */

	headerblock[WCI_SIZE] = WC_HEADERSIZE;
	*((long **) headerblock + WCI_BLINK) = headerblock;
	*((long **) headerblock + WCI_FLINK) = headerblock;
	headerblock[WC_HEADERSIZE - 1] = WC_HEADERSIZE;

	w_freeptr = headerblock;

	w_nametable = (ntbl_entry **)
		ss_malloc(NTBL_SIZE * sizeof (ntbl_entry *),
			  FE_CODESPACE_INIT);
	memset((char *) w_nametable, 0 , NTBL_SIZE * sizeof(ntbl_entry *));

	ss_register_global((long *) &w_freeptr);
	ss_register_global((long *) &w_totspace);
	ss_register_global((long *) &w_timestamp);
	ss_register_global((long *) &w_reconstamp);
	ss_register_global((long *) &w_tofreelist);
	ss_register_global((long *) &w_tofreesize);
	ss_register_global((long *) &w_nametable);
	ss_register_global((long *) &codeblock_list);
	ss_register_global((long *) &ane_entbase);
	ss_register_global((long *) &ane_entptr);
    }
}

void
w_freecount(tot, used)
    long *tot, *used;

{
    long  f;
    long *p;

    f = *(w_freeptr + WCI_SIZE);
    p = *((long **) w_freeptr + WCI_FLINK);

    while (p != w_freeptr) {
	f += *(p + WCI_SIZE);
	p = *((long **) p + WCI_FLINK);
    }

    *tot = w_totspace * 4;
    *used = (*tot - 4 * f);
}

/*-----------------------------------------------------------------*
 | w_alloccode(size)
 |
 | Allocates a block of at least size+WC_OVERHEAD longwords using
 | first fit rover.  A pointer to the beginning of the block is returned.
 | Care should be taken to avoid overwriting the memory management
 | longwords at the beginning and end of the block.
 *-----------------------------------------------------------------*/

/*  #define MDEBUG 1 */

long *
w_alloccode(size)
    int   size;
{
    long *p;			/* search pointer */
    long  codeSize = size;	/* Keep real size of usable data */
    long  s;			/* size of block found */

#ifdef MDEBUG
    if (size > WC_AREASIZE - WC_OVERHEAD)
#ifdef __LP64__
	printf("w_alloccode: Need block of size %d\n", sizeof(Code) * size);
#else
	printf("w_alloccode: Need block of size %d\n", 4 * size);
#endif
#endif

    p = w_freeptr;
    size += WC_OVERHEAD;

    while ((s = *(p + WCI_SIZE)) < size) {

		p = *((long **) p + WCI_FLINK);

		if (p == w_freeptr) {
	    	long *new;
	    	long  len = max(size + 2, WC_AREASIZE);

#ifdef __LP64__
#ifdef MDEBUG
	    	if (len > WC_AREASIZE + sizeof(Code))
			printf("w_alloccode: allocating block of size %ld\n", sizeof(Code) * len);
#endif
	    	new = (long *) code_alloc((size_t)(sizeof(Code) * len), FE_XMEM_CLAUSE);
#else
#ifdef MDEBUG
	    	if (len > WC_AREASIZE + 4)
			printf("w_alloccode: allocating block of size %ld\n", 4 * len);
#endif
	    	new = (long *) code_alloc((size_t)(4 * len), FE_XMEM_CLAUSE);
#endif

	    		/* update record of total space */
	    	w_totspace += len;

	    		/* make it look like there is a used block at the */
	    		/* beginning and end */
	    	new[0] = new[len - 1] = -1;   

	    		/* mark ends of the free block      */
	    	new[1] = new[len - 2] = len - 2;

	    	new += 1;

	    		/* get next block on chain */
	    	p = *((long **) p + WCI_FLINK);
	    	*((long **) new + WCI_BLINK) = w_freeptr;
	    	*((long **) new + WCI_FLINK) = p;
	    	*((long **) p + WCI_BLINK) = new;
	    	*((long **) w_freeptr + WCI_FLINK) = new;
	    	p = w_freeptr;
		}
    }	/* while */

    if (s < size + WC_EPSILON) {
	long *b = *((long **) p + WCI_BLINK);

	w_freeptr = *((long **) p + WCI_FLINK);
	*((long **) b + WCI_FLINK) = w_freeptr;
	*((long **) w_freeptr + WCI_BLINK) = b;
	*(p + WCI_SIZE) = -s;	/* fill in size and mark as used */
	*(p + s - 1) = -s;
    }
    else {
	w_freeptr = p;
	*(p + WCI_SIZE) = s - size;
	p += (s - size);
	*(p - 1) = s - size;
	*(p + WCI_SIZE) = -size;
	*(p + size - 1) = -size;
    }

    /* We know nothing about the clause block yet. */
    *(p + WCI_MASK) = 0;

    /* Size of data area */
    *(p + WCI_SIZECODE) = codeSize;

    return p;
}

/*-----------------------------------------------------------------*
 | w_freecode returns a block to the pool of free blocks.
 *-----------------------------------------------------------------*/

void
w_freecode(p)
    long *p;
{
    long *q;
    long  s;

    s = sizeUsedBlock(p);	/* pull out the size */

    if (*(p+s) > 0 && *(p-1) > 0) {
      long *f,*b;
      q = p + s;
      p-= *(p-1);
	f = *((long **) q + WCI_FLINK);
	b = *((long **) q + WCI_BLINK);
	*((long **) f + WCI_BLINK) = b;
	*((long **) b + WCI_FLINK) = f;
      s += sizeFreeBlock(p) + sizeFreeBlock(q);
      if (w_freeptr == q) w_freeptr = p;
    }
    else 
if (*(p + s) > 0) {		/* if next block is free... */
      long *f,*b;
	q = p + s;
	f = *((long **) q + WCI_FLINK);
	b = *((long **) q + WCI_BLINK);
	*((long **) f + WCI_BLINK) = p;
	*((long **) b + WCI_FLINK) = p;
	*((long **) p + WCI_FLINK) = f;
	*((long **) p + WCI_BLINK) = b;
	s += sizeFreeBlock(q);
	if (w_freeptr == q) w_freeptr = p;
    } else

    if (*(p - 1) > 0) {		/* if previous block is free .. */
	p -= *(p - 1);		/* set p back to start of previous block */
	s += sizeFreeBlock(p);	/* update total size            */
    }
    else {			/* previous block not free */
      long *b;
      b = *((long **)w_freeptr + WCI_BLINK);
	*((long **) p + WCI_FLINK) = w_freeptr;
	*((long **) p + WCI_BLINK) = b;
	*((long **) w_freeptr + WCI_BLINK) = p;
	*((long **) b + WCI_FLINK) = p;
	w_freeptr = p;
    }
    *(p + WCI_SIZE) = *(p + s - 1) = s;
}

/*-----------------------------------------------------------------*
 | w_installcode 
 |
 | grabs some space for the code in the icode buffer and
 | moves the contents of the icode buffer to this area.  The pointer
 | to the start of the area allocated for the code is returned.  Thus
 | the actual code may be found at WCI_CLAUSECODE longwords into the
 | area.
 |
 | The extra parameter is the amount of space to allocate in addition
 | to the space required by the clause code.  This space is used by
 | the query code.
 |
 | The actual size of the allocated buffer is stored in the memory
 | location pointed at by the parameter actualsize (if this parameter
 | is non-zero).
 *-----------------------------------------------------------------*/

long *
w_installcode(buffer, bufsize, extra, actualsize)
    Code *buffer;		/* pointer to buffer of Code */
    int   bufsize;		/* size of the buffer (in Code words) */
    int   extra;		/* amount of extra space to allocate */
    int  *actualsize;		/* the actual amount of space allocated */
{
    long *areastart;
    int   copysize;
    int   asize;
#ifdef PACKAGE
    int   risize;
#endif

    /* Need size in terms of long words */
    copysize = round(bufsize, sizeof (long) / sizeof (Code)) * sizeof (Code) / sizeof (long);
    asize = copysize + round(extra, sizeof (long)) * sizeof (Code) / sizeof (long);

#ifdef PACKAGE
    if (rinfo_flag) {
	risize = rinfo_size();
	asize += risize;
    }
#endif /* PACKAGE  */

    areastart = w_alloccode(asize);

#ifdef PACKAGE
    /* Put relocation information into clause */
    if (rinfo_flag) {
	*(areastart + WCI_RINFO) = (long) (WCI_CLAUSECODE + copysize);
	copy_rinfo(clsRInfoBuf(areastart));
    }
    else {
	*(areastart + WCI_RINFO) = 0;
    }
#endif /* PACKAGE  */

    copy_code((long *)buffer, areastart + WCI_CLAUSECODE, copysize);

    if (actualsize)
	*actualsize = asize * sizeof (long) / sizeof (Code);

    return areastart;
}

#ifdef arch_m88k

/*-----------------------------------------------------------------*
 | copy_code procedure for the 88k.
 *-----------------------------------------------------------------*/

extern	void	wm_g_uia	( void );
extern	void	wm_p_uia	( void );
extern	void	mth_pushdbl0	( void );

void
copy_code(buffer, to, bufsize)
    long *buffer, *to;
    int   bufsize;
{
    register long *s, *t;
    register long v;
    register long *targ;
    register long offs;
    register int size;

    s = to;
    t = (long *) buffer;
    size = bufsize;
    while (size-- > 0) {

	/*
	 * If we have an 88K br, br.n, bsr, or bsr.n, then get the target
	 * and re-install it fixed up with a new offset.
	 */

	if (((v = *t) & 0xf0000000) == 0xc0000000) {
	    offs = v & 0x3ffffff;
	    if (offs & 0x2000000)
		offs |= 0xfc000000;
	    targ = t + offs;
	    if (targ < buffer || targ >= buffer + bufsize) {
		*s = (v & 0xfc000000) | (0x3ffffff & (targ - s));
	    }
	    else
		*s = v;		/* branch is within the code we are copying */

	    if (targ == (long *) wm_g_uia || targ == (long *) wm_p_uia) {
		int   fv;

		if (size > 0) {
		    fv = MFENCE_VAL(*(t + 1));	/* get the fence */
		    size -= fv;
		    if (size > 0)
			for (; fv; fv--)	/* copy uia fence and data */
			    *++s = *++t;
		}
	    }
	    else if (targ == (long *) mth_pushdbl0) {
		if (size > 0) {	/* copy two words after call */
		    *++s = *++t;
		    size--;
		}
		if (size > 0) {
		    *++s = *++t;
		    size--;
		}
	    }

	}
	else {
	    *s = v;		/* copy the code as is */
	}
	t++;
	s++;
    }

}

#else  /* arch_m88k */
#ifdef arch_sparc

extern	void	wm_g_uia	( void );
extern	void	wm_p_uia	( void );
extern	void	mth_pushdbl0	( void );

/*-----------------------------------------------------------------*
 * copy_code procedure for the Sparc.
 *-----------------------------------------------------------------*/

void
copy_code(buffer, to, bufsize)
    long *buffer, *to;
    int   bufsize;
{
    register long *s, *t, *targ;
    register long v, offs;
    register int size;

    for (s = to, t = buffer, size = bufsize; size-- > 0; t++, s++) {

	/*
	 * If we have a Sparc call outside of the buffer, then get target
	 * and re-install it fixed up with new offset
	 */

	if (((v = *t) & 0xc0000000) == 0x40000000) {
	    offs = v & 0x3fffffff;
	    if (offs & 0x20000000)
		offs |= 0xc0000000;
	    targ = t + offs;
	    if (targ < buffer || targ >= buffer + bufsize)
		*s = 0x40000000 | (0x3fffffff & (targ - s));
	    else
		*s = v;		/* call is within the code we are copying */

	    if (targ == (long *) wm_g_uia || targ == (long *) wm_p_uia) {
		int   fv;

		if (size > 0) {
		    *++s = *++t;	/* copy word after call */
		    size--;	/* decrement size */
		    fv = MFENCE_VAL(*(t + 1));	/* get the fence */
		    size -= fv;
		    if (size > 0)
			for (; fv; fv--)	/* copy uia fence and data */
			    *++s = *++t;
		}
	    }
	    else if (targ == (long *) mth_pushdbl0) {
		if (size > 0) {	/* copy three words after call */
		    *++s = *++t;
		    size--;
		}
		if (size > 0) {
		    *++s = *++t;
		    size--;
		}
		if (size > 0) {
		    *++s = *++t;
		    size--;
		}
	    }
	}
	else if ((v & 0xc1c00000) == 0x00800000) {
	    offs = v & 0x3fffff;	/* 22 bit displ */
	    if (offs & 0x200000)
		offs |= 0xffc00000;
	    targ = t + offs;
	    if (targ < buffer || targ >= buffer + bufsize)
		*s = (v & 0xffc00000) | (0x3fffff & (targ - s));
	    else
		*s = v;		/* branch is within code */
	}
	else
	    *s = v;		/* copy code as is */
    }
}

#else  /* otherwise */

/*-----------------------------------------------------------------*
 | Generic copy_code procedure.  
 |
 | This is the procedure to use on machines which don't require the use 
 | of relative branches to get outside of the clause.  (When possible, 
 | we should strive to generate relative branches for targets within 
 | the clause and absolute branches for targets outside of the clause.)
 |
 | If we do another machine which requires the sort of fixups that the 88k
 | requires, we would probably be better off putting copy_code into a file
 | of its own in each of the system specific directories.
 *-----------------------------------------------------------------*/

void
copy_code(from, to, count)
    register long *from;
    register long *to;
    register int count;
{
    while (count--)
	*to++ = *from++;
}

#endif /* arch_sparc */
#endif /* arch_m88k */

/*-----------------------------------------------------------------*
 | w_nukeindexing 
 |
 | is used to wipe out the indexing code for a procedure
 | if it exists and replace it with the jump to the naive chain of
 | choice points.  This procedure also checks the time stamp to see
 | if all of the clauses should be thrown away (in the case of a 
 | reconsult operation).  If w_reconstamp is bigger than the 
 | entry's timestamp, all clauses currently associated with the 
 | procedure are trashed and the entry is made undefined.
 *-----------------------------------------------------------------*/

void
w_nukeindexing(ent)
    ntbl_entry *ent;
{
    long *p;

    switch (ent->flags & NMSK_USAGE) {
	case NFLG_SINGLE:
	case NFLG_MULTIPLE:
	    p = ent->first_clause;
	    break;
	case NFLG_SWITCH:
	    p = ent->first_clause;

	    /* If there is an index block */
	    if (ent->index_block) {
		/* Any choice points into index block must be changed. */
		w_fixchoicepoints(ent->index_block);

		/* Free it up. */
		w_freecode(ent->index_block);
		ent->index_block = (long *) 0;
	    }

	    if (nextClauseAddr(p) == 0) {
		ic_install_jmp(ent,
			       clauseCode(p) + dstartCode(p),
			       emaskCode(p));
		ent->flags = (ent->flags & ~NMSK_USAGE) | NFLG_SINGLE;
	    }
	    else {
		ic_install_try_me_jmp(ent,
				      clauseCode(p),
				      (long)choiceEntry(nextClauseAddr(p)));
		ic_install_retry_me(choiceCode(p),
				    (long)choiceEntry(nextClauseAddr(p)),
				    ent->nargs,
				    emaskCode(p));

		ent->flags = (ent->flags & ~NMSK_USAGE) | NFLG_MULTIPLE;
	    }
	    break;
	default:
	    return;
	    break;
    }
}

/*-----------------------------------------------------------------*
 | w_fixchoicepoints: 
 |
 | called by w_nukeindexing (above) to fix up the next clause 
 | addresses in each choice point.
 |
 | If a next clause address points into an index block, we expect the code
 | there to take the following form:
 |
 | i386:
 |      Size    What
 |      ----    ----
 |      1       mov eax,
 |      4               wm_retry or wm_trust
 |      2       call eax
 |      1       nop
 |      4       pointer to start of clause code or pointer to something else
 |              in this block
 |
 | m68k:
 |      2       nop
 |      2       jsr
 |      4       wm_retry or wm_trust
 |      4       pointer to start of clause code or pointer to something else
 |              in this block
 |
 | m88k:
 |      4       bsr.n   wm_trustN or wm_trust_uN
 |      4       addu    E, SPB, 0
 |      4       br.n    offset to the start of clause code or offset to
 |                      something else in this block
 |      4       addu    SP, E, 0
 |
 |
 | Fortunately, the macros provided by chpt.h (IDXPATCH_) make the code for
 | each of these three schemes the same.
 *-----------------------------------------------------------------*/

void
w_fixchoicepoints(ib)
    long *ib;
{
    long *ib_end;
    register PWord *b;
    PWord *prev_spb;
    register long *ptr;

    /* set ib_end to the last long word in the block */
    ib_end = ib + sizeUsedBlock(ib) - 1;

    /* Start with the top choice point and no valid SPB value */
    b = wm_B;
    prev_spb = (long *) 0;

    /* While there are choice points to look at */
    while (b != 0) {
	/* Get the next clause address */
	if (b < wm_heapbase || b > wm_trailbase) {
		printf("b problem\n");
	}
	ptr = (long *) chpt_NextClause(b);

	/* If the clause address points into the indexing block, there is
	 * work to do.
	 */
	if (ib <= ptr && ptr <= ib_end) {
	    /* Only two adjacent choice points both pointing into the index
	     * block can have equivalent SPBs. Is this the case?
	     */
	    if (prev_spb == (PWord *) ((PWord) chpt_SPB(b) & ~3L)) {
		/* Yep. The first choice point scanned pointed at a try/retry
		 * patch from a switch_on instruction. This current one, the
		 * one that put the code into the switch_on patch must be
		 * made a failure, since the previous choice point is now
		 * pointing into the naive try/retry chain of the proc
		 */
		chpt_NextClause(b) = (Code *) wm_trust_fail;
	    }
	    /* Otherwise, see if the next clause in the try/retry patch will
	     * be in the index block.
	     */
	    else {
		long *targ = IDXPATCH_TARGET(ptr);

		if (ib <= targ && targ <= ib_end) {
		    /* We have to look back at the previous instruction,
		     * which is pointing at a clause not into the code block,
		     * and point where that clause would have failed to in
		     * the naive try/retry chain.
		     */
		    ptr -= IDXPATCH_SIZE;	/* back up to previous */
		    chpt_NextClause(b) = choiceEntry(
		     nextClauseAddr(IDXPATCH_TARGET(ptr) - WCI_CLAUSECODE));
		}
		/* Next clause in the try/retry patch points outside of the
		 * indexing block into somewhere else.
		 */
		else {
		    /* Make choice point point to this somewhere elses choice
		     * point entry.
		     */
		    chpt_NextClause(b) = choiceEntry(cbsffa((Code *)targ));
		}
	    }
	    /* We want the SPB value for this choice point sans the
	     * compaction bit
	     */
	    prev_spb = (PWord *) ((PWord) chpt_SPB(b) & ~3L);
	}
	b = chpt_B(b);		/* Move to next choice point */
    }
}


/*-----------------------------------------------------------------*
 | cbsffa       -- clause block start from failure address
 *-----------------------------------------------------------------*/

static long *
cbsffa(addr)
    Code *addr;
{
    long *ret, size;
    int   i;


    ret = (long *) (((long) addr) & ~3);	/* align address */
    ret -= WCI_CLAUSECODE;	/* back up to probable starting place */

    for (i = 20; i > 0; i--, ret--) {
	size = *ret;		/* get supposed size field */
	if (size < 0 && size > (signed) -(MAX_ICBUFSIZE * sizeof (Code) / sizeof (long)) &&
	*(ret - size - 1) == size && sizeCode(ret) <= -size - WC_OVERHEAD) {
	    return ret;
	}
    }

    fatal_error(FE_REMIDX, 0);

    /* will not return */
    return (long *) 0;
}


/*-----------------------------------------------------------------*
 * w_abolish abolishes the procedure corresponding to ent.
 *-----------------------------------------------------------------*/

void
w_abolish(ent)
    ntbl_entry *ent;
{
    w_abolish_cg(ent, 0, 0);
}

/*-----------------------------------------------------------------*
 * w_erase erases the clause in procedure number n with address a
 *-----------------------------------------------------------------*/

int
w_erase(n, a)
    int   n;
    long *a;
{
    ntbl_entry *ent;
    long *p, *q;

    if (!(ent = w_nametable[n]))
	return (0);

    w_nukeindexing(ent);

    p = first_clause(n);

    if (p == a) {		/* See if deleting the first clause */
	/* Deleting the first clause. Is there only one? */
	if (nextClauseAddr(p) == 0) {
	    /* Only one clause. Strip everything out. */
	    ent->first_clause = ent->last_clause = (long *) 0;

	    if (ent->flags & NMSK_DYNAMIC) {
		ent->flags = (ent->flags & ~NMSK_USAGE) | NFLG_BUILTIN;
		ic_install_fail(ent);
	    }
	    else {
		ent->flags = (ent->flags & ~NMSK_USAGE) | NFLG_UNDEFINED;
		ic_install_resolve_ref(ent);
		w_relink(ent);
	    }
	}
	else {			/* More than one clause */
	    /* Find out where the next one is */
	    q = nextClauseAddr(p);

	    /* Tell name table entry that the second clause is now the first
	     * one
	     */
	    ent->first_clause = q;

	    /* Is the new first clause the only other one */
	    if (nextClauseAddr(q) == 0) {
		/* Will only be one clause in new procedure */
		ent->flags = (ent->flags & ~NMSK_USAGE) | NFLG_SINGLE;

		ic_install_jmp(ent,
			       clauseCode(q) + dstartCode(q),
			       emaskCode(q));
	    }
	    else {
		/* Will be other stuff. */
		ic_install_try_me_jmp(ent,
				      clauseCode(q),
				      (long)choiceEntry(nextClauseAddr(q)));
	    }
	}
    }
    else {			/* Not deleting the first clause */
	q = p;			/* Search for the clause to delete */
	do {
	    p = q;
	    q = next_clause(q);
	} while (q != (long *) 0 && q != a);


	if (q == (long *) 0)
	    return (0);		/* If there was no clause to delete, return
				 * nothing
				 */

	/* Otherwise, move to the clause after the one being deleted */
	q = next_clause(q);

	/* Are we at the last clause in the procedure? */
	if (q == (long *) 0) {
	    /* Yep. In the last clause. */

	    /* Will there only be one in the procedure? */
	    if (ent->first_clause == p) {
		/* Only one clause left in the procedure. Mark it as such and
		 * fix name table.
		 */
		ent->flags = (ent->flags & ~NMSK_USAGE) | NFLG_SINGLE;

		ic_install_jmp(ent, clauseCode(p) + dstartCode(p), emaskCode(p));
	    }
	    /* Make penultimate clause not point to deleted clause. */
	    nextClauseAddr(p) = 0;

	    /* Fix up last clause pointer */
	    ent->last_clause = p;

	    /* Penultimate is now the last. Put in a trust. */
	    ic_install_trust_me(choiceCode(p),
				(PWord) (clauseCode(p) + dstartCode(p)),
				ent->nargs,
				emaskCode(p));
	}
	else {
	    /* Make previous clause point after deleted clause */
	    *(p + WCI_NEXTCLAUSEADDR) = (long) q;
	    ic_install_retry_me(choiceCode(p), (long)choiceEntry(q),
				ent->nargs, emaskCode(p));

	    /* If previous clause is first in the procedure, the try_me in
	     * the head is pointing at the deleted clause. Fix it if
	     * necessary.
	     */
	    if (p == ent->first_clause)
		ic_install_try_me_jmp(ent, clauseCode(p), (long)choiceEntry(q));
	}
    }


    w_freeclause(a);		/* free up the space */

    seticount(ent);

    return 1;			/* And return a success */
}

/*-----------------------------------------------------------------*
 * w_freeclause is called by w_erase and w_nukeindexing to free up clauses.
 * If code space garbage collection is implemented, it will put the clause
 * on the tofree list.  Otherwise, w_freecode is called immediately.
 *-----------------------------------------------------------------*/

void
w_freeclause(a)
    long *a;
{
#ifndef CodeGC
    w_freecode(a);		/* Actually free up the code */
#else
    /*
     * Put the block on the tofree list, mark it as free and update info
     * on amount of space existing on tofree list.
     */

    nextClauseAddr(a) = w_tofreelist;
    w_tofreelist = a;

    *(a + WCI_MASK) |= WCMSK_TOBEFREED;
    w_tofreesize += sizeUsedBlock(a);

    ic_install_next_choice_in_a_deleted_clause(choiceEntry(a));

    /* Enough to warrant a collection? */
    if (w_tofreesize >= WC_FREELIMIT)
	w_collect();
#endif
}


/*-----------------------------------------------------------------*
 | w_assertz 
 |
 | takes the given procedure name and arity and asserts the
 | contents of the icode buffer to the end of the clauses associated
 | with the procedure.
 |
 | Parameters:
 | p               -- token id of the procedure
 | a               -- arity of the procedure
 | buffer          -- buffer holding the clause code
 | bufsize         -- amount of stuff in the buffer in units of Code
 | firstargkey     -- key of the first argument; this parameter is
 |                    used by the indexer.
 | fstartoffset    -- offset with respect to the start of the clause code
 |                    for the indexer to cause code to branch to which
 |                    can bypass the dereference loop (because it has
 |                    been done already).
 | dstartoffset    -- offset with respect to the start of the clause code
 |                    to branch to when we know that things are
 |                    determinate (so far).  In fact, it is an error to
 |                    not use the offset when the procedure has has a
 |                    choice point.
 | envsavemask     -- mask corresponding to the frame position of registers
 |                    to save on the frame.  Bit 0 is set to indicate that
 |                    the OldE register should be stored.  Bit 1
 |                    will be set to indicate that the CP register should
 |                    be stored.  Bits 2-4 correspond to A1, A2, and A3
 |                    respectively.
 *-----------------------------------------------------------------*/

void
w_assertz(p, a, buffer, bufsize, firstargkey, fstartoffset, dstartoffset, envsavemask)
    PWord p;			/* token id */
    int   a;			/* arity */
    Code *buffer;		/* temp buffer which has clause code */
    int   bufsize;		/* size of buffer */
    long  firstargkey;		/* first argument key */
    int   fstartoffset;		/* first argument start after dereference
				 * loop
				 */
    int   dstartoffset;		/* determinate start offset */
    long  envsavemask;		/* environment save mask */

{
    w_addclause(
		   p, a, CGI_ASSERTZ,
		   buffer, bufsize,
		   firstargkey, fstartoffset, dstartoffset,
		   envsavemask);
}

/*-----------------------------------------------------------------*
 | w_asserta 
 |
 | takes the given procedure name and arity and asserts the
 | contents of the icode buffer to the beginning of the clauses 
 | associated with the procedure.
 |
 | See the comment for w_assertz for a description of the parameters.
 *-----------------------------------------------------------------*/

void
w_asserta(p, a, buffer, bufsize, firstargkey, fstartoffset, dstartoffset, envsavemask)
    PWord p;
    int   a;
    Code *buffer;
    int   bufsize;
    long  firstargkey;
    int   fstartoffset;
    int   dstartoffset;
    long  envsavemask;
{
    w_addclause(
		   p, a, CGI_ASSERTA,
		   buffer, bufsize,
		   firstargkey, fstartoffset, dstartoffset,
		   envsavemask);
}

void
w_addclause(p, a, cg_id,
	    buffer, bufsize,
	    firstargkey, fstartoffset, dstartoffset, envsavemask)
    PWord p;
    int   a;
    int   cg_id;
    Code *buffer;
    int   bufsize;
    long  firstargkey;
    int   fstartoffset;
    int   dstartoffset;
    long  envsavemask;
{
    int   procid;
    ntbl_entry *ent;
    long *newclause;
    long *nextclause, *prevclause;

    ent = w_nametable[procid = w_namelookup((PWord) cur_mod, p, a)];

    if (ent->flags & NMSK_PERMANENT) {
	fprintf(stderr,
	  "Warning: Attempt made to change %s:'%s'/%d.  No action taken.\n",
		TOKNAME(cur_mod), TOKNAME(p), a);
	aib_clause_addr = (long *) 0;
	return;
    }


    if (ent->timestamp < w_reconstamp)
	w_abolish_cg(ent, cg_id, -1);
    else
	w_nukeindexing(ent);

    /* Get clause space from the clause area and copy the code into it */
    aib_clause_addr = newclause = w_installcode(buffer, bufsize, 0, (int *) 0);

#ifndef KERNAL
    if (system_debugging) {
	list_asm(clauseCode(newclause), bufsize);
    }
#endif /* KERNAL */

    /* Fill in the entries in the clause data structure */
    *(newclause + WCI_PROCIDX) = procid;
    *(newclause + WCI_FIRSTARGKEY) = firstargkey;
    *(newclause + WCI_FSTART) = fstartoffset;
    *(newclause + WCI_DSTART) = dstartoffset;
    *(newclause + WCI_EMASK) = envsavemask;

    ent->timestamp = w_timestamp++;

    if (!ent->first_clause) {	/* there are no clauses */

	/*
	 | There are no clauses in the procedure.  We need to install a jump
	 | to newclause in the procedure entry table, and put a trust
	 | into the choice point field of the one and only clause (the
	 | trust code should never be run, but it will be there for future
	 | additions).  Other bookkeeping in the procedure table entry and
	 | the new clause needs to be taken care of also.
	 */

	ic_install_jmp(ent, clauseCode(newclause) + dstartoffset, envsavemask);
	ic_install_trust_me(choiceCode(newclause),
			    (PWord) (clauseCode(newclause) + dstartoffset),
			    ent->nargs,
			    envsavemask);
	clauseId(newclause) = cg_id << CGI_OFFSET;
	nextClauseAddr(newclause) = (long *) 0;
	ent->first_clause = ent->last_clause = newclause;
	ent->flags = (ent->flags & ~NMSK_USAGE) | NFLG_SINGLE;
	if (cg_id == CGI_ASSERTA || cg_id == CGI_ASSERTZ)
	    ent->flags |= NMSK_DYNAMIC;
	w_relink(ent);
    }		/* --------------- end no clauses exist -------  */
    else if (cg_id != CGI_ASSERTA && cg_id >= cgId(ent->last_clause)) {

	/* 
	 | Previous clauses exist, and:
	 | The clause which we want to add should come after all existing clauses
	 | in the procedure.  The first thing we do is determine the clause id
	 | for the clause we are adding.
	 */

	if (cg_id == CGI_ASSERTZ)
	    clauseId(newclause) = (cg_id << CGI_OFFSET) + ++(ent->hi_id);
	else if (cg_id == cgId(ent->last_clause))
	    clauseId(newclause) = clauseId(ent->last_clause) + 1;
	else
	    clauseId(newclause) = cg_id << CGI_OFFSET;


	/*
	 * In the following block of code, we set prevclause to the
	 * clause which will come before the one we are adding.  We
	 * then set the next clause link in prevclause to point at the
	 * new one.  We then change the last_clause field in the procedure
	 * entry to point at the clause which we are adding. We also set
	 * the choice point code in the newclause to perform a trust_me
	 * operation.
	 */

	prevclause = ent->last_clause;
	nextClauseAddr(prevclause) = newclause;
	nextClauseAddr(newclause) = (long *) 0;
	ent->last_clause = newclause;
	ic_install_trust_me(choiceCode(newclause),
			    (PWord)(clauseCode(newclause) + dstartoffset),
			    ent->nargs,
			    envsavemask);

	/*
	 * Check to see if the procedure has only one clause in it
	 * thus far or multiple clauses.  We then set the choice point
	 * code appropriately.
	 */

	if ((ent->flags & NMSK_USAGE) == NFLG_SINGLE) {		/* one clause */
	     ent->flags = (ent->flags & ~NMSK_USAGE) | NFLG_MULTIPLE;

	    	/* Install try_me_else jump to first clause */
	    ic_install_try_me_jmp(ent,
				  clauseCode(prevclause),
				  (long)choiceEntry(newclause));

	    ic_install_retry_me(choiceCode(prevclause),
				(long)choiceEntry(newclause),
				ent->nargs,
				emaskCode(prevclause));
	}
	else {			/* multiple clauses */
	    ic_install_retry_me(choiceCode(prevclause),
				(long)choiceEntry(newclause),
				ent->nargs,
				emaskCode(prevclause));
	}
    }	/* End adding clause at very end */
    else {

	/*
	 * We know that newclause should not be added at the end of the
	 * procedure, so we must determine where it is to be added. The
	 * following if-then-else statement determines where the clause
	 * is to be added and sets the clause id field for the clause
	 * to the appropriate value.  After the if-then-else statement
	 * finishes, prevclause will be set to the clause just before
	 * newclause is to be added and nextclause will be set to just after
	 * the new clause is to be added.  nextclause should not be zero,
	 * though prevclause may be.  When prevclause is zero, this indicates
	 * that the clause should be added before any other clauses in the
	 * procedure.
	 */

	if (cg_id == CGI_ASSERTA) {
	    prevclause = (long *) 0;
	    nextclause = ent->first_clause;
	    clauseId(newclause) = (cg_id << CGI_OFFSET) + --(ent->lo_id);
	}
	else {
	    register long key;

	    prevclause = (long *) 0;
	    nextclause = ent->first_clause;
	    key = (cg_id << CGI_OFFSET) | CGI_VMASK;
	    while (nextclause && key > clauseId(nextclause)) {
		prevclause = nextclause;
		nextclause = next_clause(nextclause);
	    }
	    if (prevclause && cg_id == cgId(prevclause))
		clauseId(newclause) = clauseId(prevclause) + 1;
	    else
		clauseId(newclause) = cg_id << CGI_OFFSET;
	}

	/*
	 * Set next_clause field of newclause
	 */
	nextClauseAddr(newclause) = nextclause;
	ic_install_retry_me(
			       choiceCode(newclause),
			       (long)choiceEntry(nextclause),
			       ent->nargs,
			       envsavemask);

	/*
	 * Now we must determine if we are adding to the front or
	 * somewhere in the middle.  Note that if we are adding to
	 * the middle, there must be more than one clause already in
	 * the procedure.
	 */

	if (!prevclause) {	/* We are adding to the front */
	    if (nextClauseAddr(nextclause) == (long *) 0) {
		/* nextclause is only clause.  Install trust_me in nextclause
		 */
		ic_install_trust_me(
			choiceCode(nextclause),
			(PWord)(clauseCode(nextclause) + dstartCode(nextclause)),
			ent->nargs,
			emaskCode(nextclause));
		ent->flags = (ent->flags & ~NMSK_USAGE) | NFLG_MULTIPLE;
	    }

	    ent->first_clause = newclause;
	    ic_install_try_me_jmp(
				     ent,
				     clauseCode(newclause),
				     (long)choiceEntry(nextclause));
	}
	else {			/* We are adding to the middle */
	    nextClauseAddr(prevclause) = newclause;
		if (ent->first_clause == prevclause) {
				/* prev clause IS the very first clause */
			ic_install_try_me_jmp(ent,
						clauseCode(prevclause) + dstartoffset,
						(long)choiceEntry(newclause));
		};
	    ic_install_retry_me(
				   choiceCode(prevclause),
				   (long)choiceEntry(newclause),
				   ent->nargs,
				   emaskCode(prevclause));
	}
    }

    seticount(ent);
}

	/*--------------------------------------------------*
	 |	w_abolish_cg(ent, cg_id, cg_mask)
	 |
	 |	ent     = name table entry
	 |	cg_id   = (integer) clause group id
	 |	cg_mask =
	 |
	 |  In the clauses for ent, the clauses identified
	 |	by cg_id form a block:
	 |
	 |	....non-cg_id...|...cg_id...|...non-cg_id...
	 |
	 |	Either or both of the head or tail blocks could
	 |	be empty.  w_abolish_cg wipes out the cg_id block,
	 |	leaving:
	 |
	 |	....non-cg_id...||...non-cg_id...
	 *--------------------------------------------------*/

void
w_abolish_cg(ent, cg_id, cg_mask)
    ntbl_entry *ent;
    int   cg_id;
    int   cg_mask;
{
    long *p, *q, *eb1;

    w_nukeindexing(ent);

    p = ent->first_clause;

    if (!p)
		return;

    q = (long *) 0;

    /* skip over non-matching clauses */
    while (p && (cgId(p) & cg_mask) != cg_id) {
		q = p;
		p = next_clause(p);
    }

    if (!p)
		return;

    eb1 = q;	/* eb1 is end of initial block of clauses to
				 * leave
				 */

    /* remove clauses which match cg_id after mask is and'd */
    while (p && (cgId(p) & cg_mask) == cg_id) {
		q = next_clause(p);
		w_freeclause(p);
		p = q;
    }

    if (eb1) {			/* There are initial clause(s) to stay */
		nextClauseAddr(eb1) = p;

		if (p) {		/* There are final clause(s) to stay */
	    	ic_install_retry_me(choiceCode(eb1),
								(long)choiceEntry(p),
								ent->nargs,
								emaskCode(eb1));
		}
		else {			/* There are no final clauses */
	    	ent->last_clause = eb1;	/* eb1 is the last clause now */
	    	if (eb1 == ent->first_clause) {	/* eb1 is the only clause */
			ent->flags = (ent->flags & ~NMSK_USAGE) | NFLG_SINGLE;
			ic_install_jmp(ent,
			       clauseCode(eb1) + dstartCode(eb1),
			       emaskCode(eb1));
	    	}
	    	else {		/* eb1 is last clause */
			ic_install_trust_me(choiceCode(eb1),
				    (PWord) (clauseCode(eb1) + dstartCode(eb1)),
				    ent->nargs,
				    emaskCode(eb1));
	    	}
		}
    }
    else {			/* There are no initial clauses */
		ent->first_clause = p;	/* p is now first clause */
		if (p) {		/* There are final clauses */
	    	if (nextClauseAddr(p) == 0) {	/* p is the only clause */
				ent->flags = (ent->flags & ~NMSK_USAGE) | NFLG_SINGLE;
				ic_install_jmp(ent,
			       clauseCode(p) + dstartCode(p),
			       emaskCode(p));
	    	}
	    	else {
			ic_install_try_me_jmp(ent,
				      clauseCode(p),
				      (long)choiceEntry(nextClauseAddr(p)));
	    	}
		}
	else {			/* There are no final clauses */
	    ent->last_clause = (long *) 0;
	    ent->flags = (ent->flags & ~NMSK_USAGE) | NFLG_UNDEFINED;
	    ic_install_resolve_ref(ent);
	    w_relink(ent);
	}
    }

}

void
w_execquery(buffer, bufsize)
    Code *buffer;
    int   bufsize;
{
    w_exec(buffer, bufsize, "print_no");
}

void
w_execcommand(buffer, bufsize)
    Code *buffer;
    int   bufsize;
{
    w_exec(buffer, bufsize, "print_warning");
}



#define QOVERHEAD 128

#ifdef MacOS
extern long alignSystemStack(void);
extern fixSystemStack(long);
#endif

void
w_exec(buffer, bufsize, nocatcher)
    Code *buffer;
    int   bufsize;
    const char *nocatcher;
{
    long *qbuf;
    Code *execCode;
    int   ibufsize;		/* installed buffer size */
    dbprot_t odbrs;

#if defined(MacOS) && !defined(Portable)
    long   stackOffBy;

    /* See note below on why we need this... */
#endif

    qbuf = w_installcode(buffer, bufsize, QOVERHEAD, &ibufsize);

    execCode = ic_install_no(clauseCode(qbuf) + ibufsize - QOVERHEAD,
			     clauseCode(qbuf), nocatcher);

    wm_aborted = 0;
    odbrs = w_dbprotect(DBRS_RUNABLE);
#if defined(MacOS) && !defined(Portable)
    /* MPW C does not guarantee that the stack (a7) will be  longword aligned
     * upon each C call, as apparently the Unix C compilers do.  I don't
     * think I've ever seen that documented in the M68000 books, but I could
     * be wrong.   The Native Code WAM does make this assumption, though, so
     * if you are porting to a 68000 system, you may have to do something
     * similar to what I do below...
     */
    stackOffBy = alignSystemStack();
    wm_exec(execCode);
    fixSystemStack((long) stackOffBy);
#else
    wm_exec(execCode);
#endif
    w_dbprotect(odbrs);

    /*
     * If Control-C is pushed when nested commands (or queries)
     * are being executed, reissue Control-C interrupt.
     */

#if 0
    //if (wm_aborted && (wm_regidx != 0))
#endif
    if (wm_aborted && (current_engine.reg_stack_top != current_engine.reg_stack_base))
	reissue_cntrlc();
    else
	wm_aborted = 0;


    w_freecode(qbuf);
}

void
w_assert_builtin(name, arity, builtin)
    const char *name;
    int   arity;
    int   (*builtin) ( void );
{
    ntbl_entry *ent;

    ent = w_nametable[w_namelookup(
		 (PWord) MODULE_BUILTINS, (PWord) find_token((UCHAR *)name), arity)];
    ent->flags = NFLG_BUILTIN | NMSK_EXPORT | NFLG_BLT_BUILTIN;
    ic_install_builtin(ent, builtin);
}


void
w_assert_built2(name, arity, installer, p1, p2)
    const char *name;
    int   arity;
    void  (*installer) ( ntbl_entry *, PWord, PWord );
    PWord p1, p2;
{
    ntbl_entry *ent;
    short bflg;

    ent = w_nametable[w_namelookup(
		 (PWord) MODULE_BUILTINS, (PWord) find_token((UCHAR *)name), arity)];

    if ((long) installer == (long) ic_install_jmp)
	bflg = NFLG_BLT_JMP;
    else if ((long) installer == (long) ic_install_call)
	bflg = NFLG_BLT_CALL;
    else if ((long) installer == (long) ic_install_equal)
	bflg = NFLG_BLT_EQUAL;
    else if ((long) installer == (long) ic_install_true)
	bflg = NFLG_BLT_TRUE;
    else {
	bflg = NFLG_BLT_UNKNOWN;
    }

    ent->flags = NFLG_BUILTIN | NMSK_EXPORT | bflg;
    (*installer) (ent, p1, p2);
}

void
w_assert_foreign(modid, name, arity, builtin)
    PWord modid;
    const char *name;
    int   arity;
    int   (*builtin) ( void );
{
    ntbl_entry *ent;

    ent = w_nametable[w_namelookup(modid, (PWord) find_token((UCHAR *)name), arity)];

    ent->flags = NFLG_BUILTIN | NMSK_EXPORT | NFLG_BLT_BUILTIN;

    ic_install_builtin(ent, builtin);

    w_relink(ent);

}

void
w_dynamic(m, p, a)
    PWord m, p;
    int a;
{
    ntbl_entry *ent = w_nametable[w_namelookup(m, p, a)];

    ent->flags |= NMSK_DYNAMIC;
    if ((ent->flags & NMSK_USAGE) == NFLG_UNDEFINED)
		ic_install_fail(ent);
}

int
w_spy(m, p, a)
    PWord m, p;
    int   a;
{
    ntbl_entry *ent;

    if ((ent = w_nameentry(m, p, a)) != (ntbl_entry *) 0) {
		ent->flags |= NMSK_SPYSET;
		ic_install_spy(ent);
		return 1;
    }
    else
		return 0;

}

int
w_nospy(m, p, a)
    PWord m, p;
    int   a;
{
    ntbl_entry *ent;

    if ((ent = w_nameprobe(m, p, a)) != (ntbl_entry *) 0) {
		ent->flags &= ~NMSK_SPYSET;
		ic_install_normal_exec_entry(ent);
		return 1;
    }
    else
		return 0;

}


void
w_libbreak(m, p, a, i)
    PWord m, p;
    int   a, i;
{
    ntbl_entry *ent;

    ent = w_nametable[w_namelookup(m, p, a)];
    if ((ent->flags & NMSK_USAGE) == NFLG_UNDEFINED) {
		ent->flags = (ent->flags & ~NMSK_USAGE) | NFLG_LIBBREAK;
		ic_install_libbreak(ent, i);
    }
}


PWord
nextproc(n, f)
    register PWord n;
    int   f;
{
    register ntbl_entry *ent;

    for (n++; n < NTBL_SIZE; n++) {
	if ( (ent = w_nametable[n]) ) {	/* don't want == */
	    switch (ent->flags & NMSK_USAGE) {
		case NFLG_SINGLE:
		case NFLG_MULTIPLE:
		case NFLG_SWITCH:
		    return (n);
		    break;

		case NFLG_BUILTIN:
		    if (f)	/* all procs */
			return (n);
		    break;

		case NFLG_LIBBREAK:
		case NFLG_IMPORTED:
		case NFLG_UNDEFINED:
		    if (f == 2)
			return (n);
		    break;

		case NFLG_UNUSED:
		default:
		    break;
	    }
	}
    }

    return (PWord) (-1);	/* no applicable procedures found */
}

/*-----------------------------------------------------------------*
 | first_clause 
 |
 | is given the index of a procedure and returns the address of
 | the first clause in the procedure if any.  If anything goes wrong
 | in finding the clause (like the procedure not existing), a coerced
 | 0 is returned.
 *-----------------------------------------------------------------*/

long *
first_clause(n)
    int   n;			/* name table index */
{
    register ntbl_entry *ent;

    if ((ent = w_nametable[n]) == (ntbl_entry *) 0)
	return (long *) 0;

    switch (ent->flags & NMSK_USAGE) {
	case NFLG_SINGLE:
	case NFLG_MULTIPLE:
	case NFLG_SWITCH:
	    return ent->first_clause;
	default:
	    return (long *) 0;
	    break;
    }
}

/*-----------------------------------------------------------------*
 | next_clause 
 |
 | is given the address of a clause and returns the address
 | of the next clause in the chain.  A coerced zero is returned if
 | the given clause is the last.
 *-----------------------------------------------------------------*/

long *
next_clause(a)
    long *a;			/* address of current clause */
{
    if (a == (long *) 0)
	return ((long *) 0);
    else if (*(a + WCI_MASK) & WCMSK_TOBEFREED) {
	register long *c =
	w_nametable[(*(a + WCI_PROCIDX))]->first_clause;
	register long i = *(a + WCI_CLAUSEID);

	while (c && i > *(c + WCI_CLAUSEID))
	    c = nextClauseAddr(c);

	return c;
    }
    else
	return nextClauseAddr(a);
}

/*-----------------------------------------------------------------*
 | make_dbref 
 |
 | is given the address of a clause to make a data base reference
 | for.  It builds a structure on the Prolog heap and returns the
 | address of this structure (as a long).  The structure is $dbref/4 where
 | each argument is an integer.  The first argument is the low 16 bits
 | of the address of the clause.  The second argument is the high 16 bits
 | of this address.  This address will not alway be sufficient to find
 | a clause since the clause may move.  The third argument is the
 | procedure index which the clause belongs to.  Finally the last argument
 | is the clause id.  When the clause moves, it can still be located
 | in validate_dbref by scanning the clauses in the given procedure
 | until the unique clause id is found.
 *-----------------------------------------------------------------*/

void
make_dbref(a, res, restype)
    long *a;
    PWord *res;
    int  *restype;
{
    if (a == (long *) 0) {
	*res = (PWord) 0;
	*restype = WTP_INTEGER;
    }
    else {
	PWord cv;
	int   ct;

	w_mk_term(res, restype, (PWord) TK_DBREF, 4);
// TODO LP64: check use of short
	w_install_argn(*res, 1, (PWord) (((long) a) & 0xffff), WTP_INTEGER);
	w_install_argn(*res, 2, (PWord) (((long) a) >> 16), WTP_INTEGER);
	w_install_argn(*res, 3, (PWord) (*(a + WCI_PROCIDX) & 0xffff),
		       WTP_INTEGER);
	make_number(&cv, &ct, (double) clauseId(a));
	w_install_argn(*res, 4, cv, ct);
    }
}

/*-----------------------------------------------------------------*
 | w_validate_dbref 
 |
 | is called by validate_dbref and by some assembly defined
 | builtins to validate a database reference.  The procedure takes
 | three parameters, the last known address of the clause, the 
 | procedure name index, and the clause id.
 *-----------------------------------------------------------------*/

long *
w_validate_dbref(addr, nid, cid)
    long *addr;
    int   nid;
    long  cid;
{
    long  size;

    size = *(addr + WCI_SIZE);

    if (size < 0 &&				/* see if size is in bounds */
		size > (signed) -(MAX_ICBUFSIZE * sizeof (Code) / sizeof (long)) &&
		(*(addr + WCI_PROCIDX) & 0xffff) == nid &&	/* see if has same name */
		*(addr + WCI_CLAUSEID) == cid &&		/* and same clause id   */
		*(addr - size - 1) == size) {			/* see if end is valid  */
			return addr;		/* must be ok then */
    }

    /*
     * otherwise, we have some more work to do. The procedure should be
     *   swapped in if it is presently out and we need to walk the clauses
     *   until we find the clause which we are looking for.  But for the
     *   time being, it is sufficent for our purposes to just return
     *   zero indicating that the clause isn't there
     */

    return (long *) 0;
}

/*-----------------------------------------------------------------*
 | validate_dbref 
 |
 | is called by some of the C defined builtins to check on
 | the validity of a database reference.  The actual clause id is returned
 | as the value of the function if found.  Otherwise, a coerced zero is
 | returned to indicate that the database reference is invalid.  nameid
 | is the address of a location to receive the procedure name index of
 | the procedure which the clause belongs to.
 *-----------------------------------------------------------------*/

long *
validate_dbref(ref, reftype, nameid)
    PWord ref;
    int   reftype;
    PWord *nameid;
{
    if (reftype == WTP_STRUCTURE) {
	PWord functor;
	int   arity;

	w_get_functor(&functor, ref);
	w_get_arity(&arity, ref);

	if (functor == TK_DBREF && arity == 4) {
	    PWord caddrlow, caddrhi, cid;
	    int   t1, t2, t3, t4;
	    long  clause_id;

	    w_get_argn(&caddrlow, &t1, ref, 1);
	    w_get_argn(&caddrhi, &t2, ref, 2);
	    w_get_argn(nameid, &t3, ref, 3);
	    w_get_argn(&cid, &t4, ref, 4);

	    if (t1 == WTP_INTEGER && t2 == WTP_INTEGER &&
			t3 == WTP_INTEGER && getlong(&clause_id, cid, t4))
		return w_validate_dbref(
					(long *) (caddrlow | (caddrhi << 16)),
					(int) *nameid,
					(long) clause_id);
	}
    }

    return (long *) 0;
}

/*-----------------------------------------------------------------*
 | jump_validate_dbref 
 |
 | is called by jump/2 to check on
 | the validity of a database reference.  The actual clause id is returned
 | as the value of the function if found.  Otherwise, a coerced zero is
 | returned to indicate that the database reference is invalid.  nameid
 | is the address of a location to receive the procedure name index of
 | the procedure which the clause belongs to.
 *-----------------------------------------------------------------*/
#ifdef POINTERS_IN_A0
#pragma pointers_in_D0
#endif

Code *
jump_validate_dbref(ref, term)
    PWord ref;
    PWord term;
{
    PWord v;
    int   t;
    PWord functor;
    int   arity;

    w_get(&v, &t, ref);		/* decode the object */

    if (t != WTP_STRUCTURE)
	return (Code *) 0;	/* return if not a structure */

    w_get_functor(&functor, v);
    w_get_arity(&arity, v);

    if (functor == TK_DBREF && arity == 4) {
	PWord caddrlow, caddrhi, cid, nameid;
	int   t1, t2, t3, t4;
	long *jumpAddr, clause_id;

	w_get_argn(&caddrlow, &t1, v, 1);
	w_get_argn(&caddrhi, &t2, v, 2);
	w_get_argn(&nameid, &t3, v, 3);
	w_get_argn(&cid, &t4, v, 4);

	if (t1 == WTP_INTEGER && t2 == WTP_INTEGER &&
	    t3 == WTP_INTEGER && getlong(&clause_id, cid, t4) &&
	    (jumpAddr = w_validate_dbref(
				      (long *) (caddrlow | (caddrhi << 16)),
					    (int) nameid,
					    (long) clause_id))) {
	    PWord fa = w_nametable[nameid]->tokid_arity;

	    w_get(&v, &t, term);
	    if (t == WTP_SYMBOL) {
		functor = v;
		arity = 0;
	    }
	    else if (t == WTP_STRUCTURE) {
		w_get_arity(&arity, v);
		w_get_functor(&functor, v);
	    }
	    else {
		functor = 0;	/* which is illegal */
		arity = 0;
	    }

	    if (fa == MMK_FUNCTOR(functor, arity))
		return ((Code *) (clauseCode(jumpAddr) + dstartCode(jumpAddr)));
	}
    }

    return (Code *) 0;
}
#ifdef POINTERS_IN_A0
#pragma pointers_in_A0
#endif

/*-----------------------------------------------------------------*
 | gen_indexing: 
 | 
 | Generate indexing for all procedures.
 *-----------------------------------------------------------------*/

void
gen_indexing()
{
#ifdef Indexing
    register int i;
    register ntbl_entry *ent;

    for (i = 0; i < NTBL_SIZE; i++)
	if ( (ent = w_nametable[i]) ) {
	    do_indexing(ent);
#ifdef AutoIndexing
	    if (ent->flags & NMSK_SPYSET)
		ic_install_spy(ent);
	    else
		ic_install_normal_exec_entry(ent);
#endif /* AutoIndexing */
	}
#endif /* Indexing */
}

/*-----------------------------------------------------------------*
 | decr_icount 
 |
 | decrements the icount field and, if zero, attempts to
 | generate indexing
 *-----------------------------------------------------------------*/

void
decr_icount(addr)
    Code *addr;
{
#ifdef Indexing
    register ntbl_entry *ent;
    dbprot_t odbrs;

    ent = (ntbl_entry *) (((char *) addr) - (int) ((ntbl_entry *) 0)->code);

    odbrs = w_dbprotect(DBRS_WRITABLE);
    if (!(--(ent->icount))) {
	do_indexing(ent);
	ic_install_normal_exec_entry(ent);
    }
    (void) w_dbprotect(odbrs);
#endif /* Indexing */
}

/*-----------------------------------------------------------------*
 | seticount 
 |
 | is called to set the icount field to it initial value and
 | possible put the icount decrement code in the codeg field of the entry
 *-----------------------------------------------------------------*/

void
seticount(ent)
    ntbl_entry *ent;
{
#ifdef AutoIndexing
    if (!(ent->flags & NMSK_SPYSET) &&
	(ent->flags & NMSK_USAGE) == NFLG_MULTIPLE) {
	ent->icount = INITIAL_ICOUNT;
	ic_install_decr_icount(ent);
    }
#endif /* AutoIndexing */
}

#ifdef CodeGC
/*-----------------------------------------------------------------*
 | next_choice_in_a_deleted_clause is 
 |
 | called by wm_nciadc with the address of
 | the choice entry of a deleted clause.  This function finds the next active
 | clause which comes after (or would have come after) the deleted clause and
 | returns the address at which to continue execution.
 *-----------------------------------------------------------------*/
#ifdef POINTERS_IN_A0
#pragma pointers_in_D0
#endif

long *
next_choice_in_a_deleted_clause(addr)
    long *addr;
{
    addr = next_clause(addr - WCI_CHOICEENTRY);

    /*
     * Return the choice point entry code.
     */

    if (addr)
	return (addr + WCI_CHOICEENTRY);
    else
	/* no more clauses. Trust away choice point and then fail */
	return ((long *) wm_trust_fail);
}
#ifdef POINTERS_IN_A0
#pragma pointers_in_A0
#endif

/*-----------------------------------------------------------------*
 * w_collect attempts to really free some of the blocks on the tofree list.
 *-----------------------------------------------------------------*/

void
w_collect()
{
    register PWord *h, *h_end;
    long **upd, *p;

    /*
     * Step 1: Do a gc.
     */

    gv_setcnt++;
    gc();

    /*
     * Step 2: Find database references on the heap and mark from them.
     */

    h = wm_heapbase;
    h_end = wm_H;
    while (h < h_end) {
	switch ((int) MTP_TAG(*h)) {

#ifdef MTP_CONST
	    case MTP_CONST:
		if ((*h & MTP_CONSTMASK) == MTP_FENCE)
		    h += MFENCE_VAL(*h) + 1;	/* skip over the UIA */
		else if ((*h & MTP_CONSTMASK) == MTP_SYM) {
		    if (*h == MMK_FUNCTOR(TK_DBREF, 4)) {
			long *addr;
			int   pid, cid;

			addr = (long *)
			    (MINTEGER(*(h + 1)) | (MINTEGER(*(h + 2)) << 16));
			pid = MINTEGER(*(h + 3));
			cid = MINTEGER(*(h + 4));
			if ((addr = w_validate_dbref(addr, pid, cid)) &&
			    (*(addr + WCI_MASK) & WCMSK_TOBEFREED))
			    *(addr + WCI_MASK) |= WCMSK_MARKED;
		    }
		    h += MFUNCTOR_ARITY(*h) + 1;
		}
		else
		    h++;	/* skip over the constant */
		break;

#else  /* MTP_CONST */
	    case MTP_FENCE:
		h += MFENCE_VAL(*h) + 1;	/* skip over UIA or double */
		break;

	    case MTP_SYM:
		if (*h == MMK_FUNCTOR(TK_DBREF, 4)) {
		    long *addr;
		    int   pid, cid;

		    addr = (long *)
			(MINTEGER(*(h + 1)) | (MINTEGER(*(h + 2)) << 16));
		    pid = MINTEGER(*(h + 3));
		    cid = MINTEGER(*(h + 4));
		    if ((addr = w_validate_dbref(addr, pid, cid)) &&
			(*(addr + WCI_MASK) & WCMSK_TOBEFREED))
			*(addr + WCI_MASK) |= WCMSK_MARKED;
		}
		h += MFUNCTOR_ARITY(*h) + 1;
		break;

#endif /* MTP_CONST */

	    default:
		h++;		/* skip over the object */
		break;
	}
    }


#ifndef MUNBIAS
#define MUNBIAS(x) (x)
#endif

    /*
     * Step 3: Mark from next clause addresses and continuation pointers.
     */

#if 0
    //if (wm_regidx)
#endif
    if (current_engine.reg_stack_top != current_engine.reg_stack_base)
    {
	register PWord *e, *b, *spb;

	e = wm_E;
	b = wm_B;

	while (chpt_B(b) != (long *) 0) {
	    spb = (PWord *) MUNBIAS(((PWord) chpt_SPB(b) & ~3L));
	    while (e <= spb) {
		mark_fromretaddr((Code *) *(e + 1));
		e = (PWord *) MUNBIAS(*(e));
	    }
	    mark_clause(((long *) chpt_NextClause(b)) - WCI_CHOICEENTRY);

	    e = (PWord *) MUNBIAS(*spb);
	    b = chpt_B(b);
	}
    }

    /*
     * Step 4:  Traverse the to be freed list and free up the non-marked
     *          blocks.
     */

    upd = &w_tofreelist;
    p = w_tofreelist;

    while (p != (long *) 0) {
	if (*(p + WCI_MASK) & WCMSK_MARKED) {
	    /* Clear the marked flag */
	    *(p + WCI_MASK) &= ~WCMSK_MARKED;
	    /* We're going to leave this on the tofree list */
	    upd = (long **) p + WCI_NEXTCLAUSEADDR;
	}
	else {
	    /* Take off the tofree list */
	    *upd = (long *) *(p + WCI_NEXTCLAUSEADDR);
	    w_freecode(p);
	}
	/* Move to the next item */
	p = *upd;
    }

    /* we will allow WC_FREELIMIT longwords of new space to be added to the
     * free list.
     */
    w_tofreesize = 0;

}


static long *
clause_start_from_retaddr(ra,mp)
    Code *ra;
    long *mp;
{
#ifndef GCMASK
    long *p;

    if (!ra)
	return 0;
#if defined(MacOS) && defined(arch_m68k)
    if ((*ra != GCMAGIC) && (*ra != 0x303c))
#else
    if (*ra != GCMAGIC)
#endif /* MacOS */
	fatal_error(FE_GCMAGIC_CGC, (long) ra);

    /*
     * Set the mask value if needed.
     */
    
    if (mp)
	*mp = *((long *) (ra + GCMAGICVal(ra)) + 1);
    /*
     * Find the longword at the end of the call information which tells us
     * how many Code words to go back to get to the start of the clause code
     */

    for (p = ((long *) (ra + GCMAGICVal(ra))) + 2; *p != -1; p += 2) ;
    p++;			/* advance past endmarker */

    /*
     * If the long word that p is pointing at is zero, we are done (it is
     * not a user defined clause).  Otherwise, move back to the beginning
     * of the clause and mark it.
     */

    if (*p != 0)
	return(((long *) (((Code *) p) - *p)) - WCI_CLAUSECODE);
    else
	return 0;

#else  /* GCMASK */
    int   dat;

    if (!ra)
	return 0;

    /*
     * Set the mask value if needed.
     */
    
    if (mp)
	*mp = *(ra - 1);

    /*
     * We are counting on the fact (on the SPARC and 88k) that there are
     * three words of garbage collection information between the return
     * address and the code implementing the actual call.
     */

    dat = *(ra - 3);

    if ((dat & GCMASK) != GCMAGIC)
	fatal_error(FE_GCMAGIC_CGC, (long) ra);

    dat &= 0xffff;

    /*
     * If dat is zero, this indicates that we have a builtin.  Otherwise move
     * back to the beginning of the clause and return it.
     */

    if (dat != 0)
	return(((long *) ra - dat - 3) - WCI_CLAUSECODE);
    else
	return 0;
#endif /* GCMASK */

}

static void
mark_fromretaddr(ra)
    Code *ra;
{
    long *ca = clause_start_from_retaddr(ra,0);
    if (ca)
	mark_clause(ca);
}

static void
mark_clause(c)
    long *c;
{
    long  size;

    /*
     * Check for validity of the clause.
     */

    size = sizeUsedBlock(c);
    if (size < WC_EPSILON || size > (signed) (MAX_ICBUFSIZE * sizeof (Code) / sizeof (long)))
	return;			/* return on invalid size */

    if (size != -*(c + size - 1))
	return;			/* return on unequal end sizes */

    if (*(c + WCI_PROCIDX) < 0 && *(c + WCI_PROCIDX) >= NTBL_SIZE)
	return;			/* return if procid is out of range */

    /*
     * The clause information checks out.  If the clause is on the to
     * free list then mark it.
     */

    if (*(c + WCI_MASK) & WCMSK_TOBEFREED)
	*(c + WCI_MASK) |= WCMSK_MARKED;
}
#endif /* CodeGC */

/*-----------------------------------------------------------------*
 | f is a pointer to a prolog stack frame.  The next stack frame is returned.
 | The location that ca points to is set to point at the clause block to
 | which this frame belongs.  mp is a pointer to the gc mask.  This mask
 | tells us which arguments are still valid and therefore safe to get when
 | building a term representing the frame.
 *-----------------------------------------------------------------*/

PWord *
w_frame_info(f,ca,mp)
    PWord *f;
    long **ca;
    long *mp;
{
    long *nf;
    if (*(f+1)) {
		*ca = clause_start_from_retaddr((Code *) *(f+1), mp);
		nf = (PWord *) MUNBIAS(*(f));
    }
    else {
		nf = 0;
		*ca = 0;
    }
    return nf;
}


/*-----------------------------------------------------------------*
 | w_relink 
 |
 | is given a procedure entry.  If the entry is exported, then all
 | procedures in other modules with the same procedure name and arity
 | are examined.  If these entries are imported, the resolve ref code
 | is replaced. w_relink is called from places where either a procedure
 | is defined (where none existed), or the last clause of the procedure
 | is erased.  Note that very little work is done when the procedure
 | entry is not exported.
 *-----------------------------------------------------------------*/

void
w_relink(ent)
    ntbl_entry *ent;
{
    if (ent->flags & NMSK_EXPORT) {
	register PWord key;
	register int i;
	register ntbl_entry *e;

	key = ent->tokid_arity;
	for (i = 0; i < NTBL_SIZE; i++) {
	    if ((e = w_nametable[i]) && e->tokid_arity == key &&
		(e->flags & NMSK_USAGE) == NFLG_IMPORTED) {
		e->flags = (e->flags & ~NMSK_USAGE) | NFLG_UNDEFINED;
		ic_install_resolve_ref(e);
	    }
	}
    }
}

/*-----------------------------------------------------------------*
 | w_relinkall 
 |
 | relinks all of the procedures (see above).  It is called after
 | a file has been consulted.
 |
 *-----------------------------------------------------------------*/

void
w_relinkall()
{
    register int i;
    register ntbl_entry *e;

    for (i = 0; i < NTBL_SIZE; i++) {
	if ((e = w_nametable[i]) && (e->flags & NMSK_USAGE) == NFLG_IMPORTED) {
	    e->flags = (e->flags & ~NMSK_USAGE) | NFLG_UNDEFINED;
	    ic_install_resolve_ref(e);
	}
    }
}

/*-----------------------------------------------------------------*
 | w_getnamestring 
 |
 | is given an address and attempts to return a string
 | representing the procedure pointed to by addr.  This function is used by
 | the disassembler.
 |
 | buf is the address of a buffer to put the string into.  The return value
 | of this function is important. buf is used when a name entry could be
 | found, zero is returned otherwise.
 *-----------------------------------------------------------------*/

char *
w_getnamestring(addr, buf)
    Code *addr;
    char *buf;
{
    int n;
    ntbl_entry *ent;
    const char *suffix;
    int offset;


    for (n=0; n<NTBL_SIZE; n++)
	if ((ent = w_nametable[n])	/* want assignment here */
	    && (char *) ent->overflow <= (char *) addr
	    && (char *) addr < (char *) ent + sizeof (ntbl_entry))
	    goto found;
    
    return (char *) 0;

found:
    if (addr < ent->call_entry) {
	suffix = ".overflow";
	offset = (char *) addr - (char *) ent->overflow;
    }
    else if (addr < ent->exec_entry) {
	suffix = ".call_entry";
	offset = (char *) addr - (char *) ent->call_entry;
    }
    else if (addr < ent->code) {
	suffix = ".exec_entry";
	offset = (char *) addr - (char *) ent->exec_entry;
    }
    else {
	suffix = ".code";
	offset = (char *) addr - (char *) ent->code;
    }

    /*
     * Format the string, put it into buf and return a pointer to buf.
     */

    if (offset)
	sprintf(buf, "%s:%s/%ld%s+%x (0x%lx)", TOKNAME(ent->modid),
		TOKNAME(MFUNCTOR_TOKID(ent->tokid_arity)),
		(long)MFUNCTOR_ARITY(ent->tokid_arity),
		suffix,
		offset,
		(long)addr);
    else
	sprintf(buf, "%s:%s/%ld%s (0x%lx)", TOKNAME(ent->modid),
		TOKNAME(MFUNCTOR_TOKID(ent->tokid_arity)),
		(long)MFUNCTOR_ARITY(ent->tokid_arity),
		suffix,
		(long)addr);
    return buf;
}
