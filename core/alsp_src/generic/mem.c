/*=================================================================*
 |			mem.c                
 |      Copyright (c) 1992-95 Applied Logic Systems, Inc.
 |
 |			-- memory allocation
 |
 | Author: Kevin A. Buettner
 | Creation: 12/17/92
 | 12/11/94 - C. Houpt -- Put sys/types.h and fcntl.h under ifdef control.
 | 				 -- Removed SIGBUS, SIGSEGV signal handling for Mac
 |				 -- Added HAVE_BRK for ifdef control of brk(), sbrk().
 |				 -- Ifdefed around 3 param open() call.
 |				 -- Misc char casts.
 |??/??/94 - C. Houpt -- NEED non-brk() implementation of ss_restore_state.
 |					    Maybe from the Windows version.
 *=================================================================*/
#include "defs.h"
#include "version.h"
#include "sig.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif	/* HAVE_UNISTD_H */

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef MacOS
/* for fixing PPC MPW Tool Malloc Bug. */
#include <Memory.h>
#if !defined(MPW_TOOL) && defined(__MWERKS__)
#include <unix.h>
/* MetroWerks does not define the EINTR error code. */
#define EINTR           4
#endif
#endif

#ifdef WIN32
#include "fswin32.h"
#endif

#include <errno.h>

#ifndef O_BINARY
#define O_BINARY 0
#endif

static	void	coredump_cleanup	PARAMS(( int ));

static	int	pgsize = 8192;	/* A guess at the page size */

#ifdef PARAMREVBIT
unsigned long AddressHiBit = 0x0;
unsigned long ReversedHiBit = 0x80000000;
#endif

#define SIGSTKSIZE 8192		/* Size to use for a signal stack */

/* #if defined(HAVE_MMAP) || defined(MACH_SUBSTRATE) */

#if	(defined(HAVE_MMAP) && (defined(HAVE_DEV_ZERO) || defined(HAVE_MMAP_ZERO))) || defined(MACH_SUBSTRATE)

static	int	bottom_stack_page_is_protected = 0;

#endif	/* defined(HAVE_MMAP) || defined(MACH_SUBSTRATE) */

/* #ifdef HAVE_MMAP */
#if	defined(HAVE_MMAP) && (defined(HAVE_DEV_ZERO) || defined(HAVE_MMAP_ZERO))
/*----------------- Operating systems with mmap ---------------*/

#include <sys/mman.h>

#ifdef	HAVE_UCONTEXT_H
#include <ucontext.h>
#endif /* HAVE_UCONTEXT_H */

/* FIXME: Either put a test into configure.in to handle the following or
 * make the code which calls mmap more generic in that it will try a number
 * of addresses. 
 */
#ifdef _AIX
#define	STACKSTART	0x30000000
#define CODESTART	0x38000000
#else
#define	STACKSTART	0x01000000
#define CODESTART	0x05000000
#endif

#define	NPROTECTED	1

static long signal_stack[SIGSTKSIZE];

#if defined(HAVE_MPROTECT) && defined(MISSING_EXTERN_MPROTECT)
extern	int	mprotect	PARAMS(( caddr_t, size_t, int ));
#endif
static	void	release_bottom_stack_page	PARAMS(( void ));

/*-------------------------------------------------------------------*
 * stack_overflow is a signal handler for catching stack overflows.
 *
 * With the memory allocated with the mmap call (see
 * allocate_prolog_heap_and_stack below), we make the bottom-most stack
 * page read/write protected.  At the time that this comment was written,
 * on SunOS, using the mprotect call, this will cause delivery of a SIGBUS
 * signal when a write is made to the protected region. If the region is
 * protected by remapping it (with the mmap call), the SIGSEGV signal is
 * delivered.
 *
 * We will release the bottom stack page for read/write access (if this
 * has not already been done), indicate to the Prolog environment that
 * a signal has been caught, and permit execution to continue.  Then
 * at the next inference, the prolog system should process the interrupt,
 * reduce the stack requirements (via an abort or throw probably) and
 * reprotect the bottom page so that a future stack overflow will be
 * caught.
 *-------------------------------------------------------------------*/


#if defined(HAVE_SIGACTION) && defined(SA_SIGINFO)
static	void	stack_overflow	PARAMS(( int, struct siginfo *, struct ucontext * ));
static void
stack_overflow(signum, siginf, sigcon)
    int   signum;
    struct siginfo *siginf;
    struct ucontext *sigcon;

#elif defined(HAVE_SIGVEC) || defined(HAVE_SIGVECTOR)
static	void	stack_overflow	PARAMS(( int, int, struct sigcontext *, caddr_t ));
static void
stack_overflow(signum, code, scp, addr)
    int   signum, code;
    struct sigcontext *scp;
    caddr_t addr;

#else
    die
#endif

{

    if (bottom_stack_page_is_protected) {

	/*-------------------------------------------------------------*
	 * Make bottom stack page readable/writable so execution may
	 * continue for a little ways (enough to run Prolog's interrupt
	 * handler)
	 *-------------------------------------------------------------*/

	release_bottom_stack_page();

	/*-------------------------------------------------------------*
	 * Inform the prolog system that a signal has occurred. Note
	 * that SunOS and SysVR4 have different signal mechanisms. In
	 * a few years from now, these versions should be combined
	 * well enough that we can replace it with a single piece of
	 * code.  But for now, it is just plain ugly.
	 *-------------------------------------------------------------*/

#if defined(HAVE_SIGACTION) && defined(SA_SIGINFO)
	signal_handler(ALSSIG_STACK_OVERFLOW, siginf, sigcon);
#elif defined (HAVE_SIGVEC) || defined(HAVE_SIGVECTOR)
	signal_handler(ALSSIG_STACK_OVERFLOW, code, scp, addr);
#else
	die
#endif

    }
    else {

	/*-------------------------------------------------------------*
	 * Let error occur again and core dump normally.  This could
	 * happen if we have some other problem with our code which
	 * causes a bus error.  In this case, we would like to be
	 * able to examine the core file produced by the system.
	 *-------------------------------------------------------------*/


#ifdef HAVE_SIGACTION
	struct sigaction act;

	act.sa_handler = SIG_DFL;
	act.sa_flags = SA_ONSTACK;
	(void) sigaction(signum, &act, 0);
#elif defined(HAVE_SIGVEC) || defined(HAVE_SIGVECTOR)
	struct sigvec v;

	v.sv_handler = SIG_DFL;
	v.sv_mask = 0;
	v.sv_flags = SV_ONSTACK;
#ifdef HAVE_SIGVECTOR
	sigvector(signum, &v, 0);
#else
	sigvec(signum, &v, 0);
#endif

#else
	die
#endif

	coredump_cleanup(-1);

    }
}


PWord *
allocate_prolog_heap_and_stack(size)
    size_t size;		/* number of PWords to allocate */
{
    int   fd;
    PWord *retval;

#if defined(HAVE_SIGACTION)
    stack_t si;
    struct sigaction act;

    si.ss_sp = (void *) (signal_stack + SIGSTKSIZE - 2);
    si.ss_size = SIGSTKSIZE - 2;
    si.ss_flags = 0;
    sigaltstack(&si, 0);	/* set up the signal stack */

    act.sa_handler = stack_overflow;
    act.sa_flags = SA_ONSTACK;
    (void) sigaction(SIGBUS, &act, 0);	/* establish signal handler */
    (void) sigaction(SIGSEGV, &act, 0);
    /*-------------------------------------------------------------*
     * Note: We might have to catch SIGSEGV in the above call to sigaction.
     * I don't know what the behavior will be for SVR4.
     *                                  Kev, 12/18/92
     *-------------------------------------------------------------*/
#elif defined(HAVE_SIGVEC) || defined(HAVE_SIGVECTOR)
    struct sigstack si;
    struct sigvec v;

    si.ss_sp = (char *) (signal_stack + SIGSTKSIZE - 2);
    si.ss_onstack = 0;
    sigstack(&si, 0);		/* set up the signal stack */

    v.sv_handler = stack_overflow;
    v.sv_mask = 0;
    v.sv_flags = SV_ONSTACK;
#ifdef HAVE_SIGVECTOR
    sigvector(SIGBUS, &v, 0);	/* establish stack overflow */
    sigvector(SIGSEGV, &v, 0);	/* signal handlers      */
#else
    sigvec(SIGBUS, &v, 0);	/* establish stack overflow */
    sigvec(SIGSEGV, &v, 0);	/* signal handlers      */
#endif
#endif /* HAVE_SIGACTION, HAVE_SIGVEC */


    /*-------------------------------------------------------------*
     * Set pgsize to the page size for the OS.  Having the pgsize variable
     * will let us write our code without having to call getpagesize over
     * and over again.  
     *-------------------------------------------------------------*/

#ifdef _SC_PAGESIZE
    pgsize = sysconf(_SC_PAGESIZE);
#elif defined(HAVE_GETPAGESIZE)
    pgsize = getpagesize();
#endif  /* _SC_PAGESIZE */

    /*-------------------------------------------------------------*
     * Open /dev/zero for read/write access.  /dev/zero is essentially a
     * file of infinitely (or at least as many as we need) zeros.
     *-------------------------------------------------------------*/

    if ((fd = open("/dev/zero", O_RDWR)) == -1)
	fatal_error(FE_DEVZERO, 0);

    /*-------------------------------------------------------------*
     * Call mmap to allocate the memory and map it to /dev/zero.  Note
     * that we are using the MAP_PRIVATE flag or'd with the MAP_FIXED
     * flag.
     *
     * The reason that we use the MAP_FIXED flag is because during my
     * tests on the SPARC, I noticed that if we let the system choose
     * the address to use, it gave us an address that was unacceptable
     * for use with the garbage compactor. (The most significant bit
     * was set).  I therefore specify where I want this region to start
     * via STACKSTART.  I see this as a potential problem on some
     * OS's.  We may have to ifdef this code (or the STACKSTART define) to
     * get the result that we want for all machines.
     *
     * One advantage of having a fixed location for this region of memory
     * is that it will much easier to implement saved states.
     *-------------------------------------------------------------*/

    retval = (PWord *) mmap((caddr_t) STACKSTART,
			    size * sizeof (PWord) + NPROTECTED * pgsize,
			    PROT_READ | PROT_WRITE,
			    MAP_PRIVATE | MAP_FIXED,
			    fd,
			    0);


    close(fd);			/* close /dev/zero */

    /*
     * Error out if something went wrong with mmap call above
     */

    if (retval == (PWord *) - 1)
	fatal_error(FE_BIGSTACK, 0);

    /*-------------------------------------------------------------*
     * Protect the bottom stack page.  Note that we allocated extra space
     * for this page in the mmap call above.
     *-------------------------------------------------------------*/

    protect_bottom_stack_page();

    return retval;
}

void
protect_bottom_stack_page()
{
    mprotect((caddr_t) STACKSTART, (size_t)(NPROTECTED * pgsize), PROT_NONE);
    bottom_stack_page_is_protected = 1;
}

static void
release_bottom_stack_page()
{
    mprotect((caddr_t) STACKSTART,
             (size_t)(NPROTECTED * pgsize),
	     PROT_READ | PROT_WRITE);
    bottom_stack_page_is_protected = 0;
}

#elif defined(MACH_SUBSTRATE)
	/*------- Operating systems with Mach as the substrate -------*/

#include <mach/mach.h>

#define STACKSTART	0x01000000
#define CODESTART	0x06000000
#define NPROTECTED	1

static long signal_stack[SIGSTKSIZE];

static	void	release_bottom_stack_page	PARAMS(( void ));

void
stack_overflow(signum, code, scp, addr)
    int   signum, code;
    struct sigcontext *scp;
    caddr_t addr;
{
    if (bottom_stack_page_is_protected) {
	release_bottom_stack_page();
	signal_handler(ALSSIG_STACK_OVERFLOW, code, scp, addr);
    }
    else {
	struct sigvec v;

	v.sv_handler = SIG_DFL;
	v.sv_mask = 0;
	v.sv_flags = SV_ONSTACK;
#ifdef HAVE_SIGVECTOR
	sigvector(signum, &v, 0);
#else
	sigvec(signum, &v, 0);
#endif

	coredump_cleanup(-1);
    }
}

PWord *
allocate_prolog_heap_and_stack(size)
    size_t size;				/* number of PWords to allocate */
{
    PWord *retval;
    kern_return_t stat;
    struct sigstack si;
    struct sigvec v;

    si.ss_sp = (char *) (signal_stack + SIGSTKSIZE - 2);
    si.ss_onstack = 0;
    sigstack(&si, 0);		/* set up the signal stack */

    v.sv_handler = stack_overflow;
    v.sv_mask = 0;
    v.sv_flags = SV_ONSTACK;
#ifdef HAVE_SIGVECTOR
    sigvector(SIGBUS, &v, 0);	/* establish stack overflow signal handler */
#else
    sigvec(SIGBUS, &v, 0);	/* establish stack overflow signal handler */
#endif

    pgsize = vm_page_size;
    retval = (PWord *) STACKSTART;

    stat = vm_allocate(task_self(),
		       (vm_address_t *) &retval,
		       size * sizeof (PWord) + NPROTECTED * pgsize,
		       FALSE);
    
    if (stat != KERN_SUCCESS || retval != (PWord *) STACKSTART)
	fatal_error(FE_BIGSTACK, 0);
    
    protect_bottom_stack_page();

    return retval;
}

void
protect_bottom_stack_page()
{
    kern_return_t stat;

    stat = vm_protect(task_self(),
		      (vm_address_t) STACKSTART,
	              NPROTECTED * pgsize,
	              FALSE,
	              VM_PROT_NONE );
    /* FIXME: Check stat */
    bottom_stack_page_is_protected = 1;
}

static void
release_bottom_stack_page()
{
    kern_return_t stat;

    stat = vm_protect(task_self(),
		      (vm_address_t) STACKSTART,
	              NPROTECTED * pgsize,
	              FALSE,
	              VM_PROT_READ | VM_PROT_WRITE );
    /* FIXME: Check stat */
    bottom_stack_page_is_protected = 0;
}

#else
/*------ All other operating systems and architectures --------*/

#if	defined(Portable) || defined(arch_m88k) || defined(arch_m68k)
void stack_overflow(void);

void
stack_overflow(void)
{
    fatal_error(FE_STKOVERFLOW, 0);
}

#endif /* arch_m88k */

PWord *
allocate_prolog_heap_and_stack(size)
    size_t size;			/* number of PWords to allocate */
{
#if __POWERPC__ && defined(MPW_TOOL)
    /* The malloc that comes with the StdCLib in CW7 seems to be broken,
       you can only malloc up to 0x800000 bytes. Work around by using
       NewPtr. */
    PWord *retval = (PWord *) NewPtr(sizeof (PWord) * size);
#else
    PWord *retval = (PWord *) malloc(sizeof (PWord) * size);
#endif

    if (retval == 0)
	fatal_error(FE_BIGSTACK, 0);

#ifdef PARAMREVBIT
	AddressHiBit = (((unsigned long)retval) & 0x80000000);
	ReversedHiBit = (~AddressHiBit & 0x80000000);
	if (AddressHiBit != ((((unsigned long)retval) + size) & 0x80000000))
	fatal_error(FE_TAGERR, 0);
#else
#ifdef arch_m88k
    if (((long) (retval + size) & MTP_TAGMASK) != 0)
#else  /* arch_m88k */
    if (((long) (retval + size) & 0x80000000) != 0)
#endif /* arch_m88k */
	fatal_error(FE_TAGERR, 0);
#endif

#ifdef arch_m88k
    (void) signal(SIGFPE, stack_overflow);
#endif

/* These signals are not available on the Mac. */
#ifndef MacOS
#ifdef SIGBUS
    (void) signal(SIGBUS, coredump_cleanup);
#endif
    (void) signal(SIGSEGV, coredump_cleanup);
#endif
    return retval;
}

void
protect_bottom_stack_page()
{
    /* do nothing */
}

#endif

/*
 * coredump_cleanup is called to perform those cleanup actions needed just
 * before a core dump occurs.  The way it works is this:
 *
 *      coredump_cleanup is installed as the signal handler for those signals
 *      which might core dump (SIGSEGV and SIGBUS are the most common).
 *
 *      Should the appropriate signal be delivered, we set the handler to
 *      SIG_DFL which will cause the core dump to occur (if this is in
 *      fact the default handler)
 *
 *      We perform the cleanup actions required.
 *
 *      We return from the coredump_cleanup function.  The system will attempt
 *      to reexecute the instruction which caused the signal, thus causing it
 *      again.  With no handler, the system will take the default action which
 *      is to core dump (presumably).
 */

#ifdef SIG_DFL

static void
coredump_cleanup(signum)
    int   signum;
{
    static char message[] =
    "Memory access violation detected. Attempting to core dump\r\n";

    /*
     * The reason that we are core dumping might be because the I/O buffers
     * are messed up, or at the very least became messed up in the process.
     * (This actually occurs on the 88k, SVR3).  Therefore we use the primitive
     * write to write the message out.
     */

    write(2, message, sizeof (message) - 1);


    /*
     * We call signal to set the SIG_DFL.  If signum is less than zero, this
     * means that somebody else has already performed the work for us.  See
     * stack_overflow (SunOS and SysVR4 versions) above.
     */

    if (signum > 0)
	(void) signal(signum, SIG_DFL);

#ifdef DynamicForeign
    /*
     * remove files associated with dynamic foreign interface if they
     * exist
     */
    foreign_shutdown();
#endif /* DynamicForeign */

}

#endif /* SIG_DFL */


/*
 * ALS Memory allocation	-- saved code states
 *
 * Defined below are functions which we use for memory allocation.  These
 * routines should be called only by functions whose state needs to be
 * saved. ALL pointers to space allocated by these routines should either
 * be self contained or contained in space obtained through other calls
 * to these functions.   There are times when the use of a global variable
 * is unavoidable.  
 *
 * I originally considered putting all global variables into a large
 * structure.  This is a fairly pleasing idea but for the fact that
 * it totally ruins modularity.  There are static variables in files like
 * symtab.c which manage the structure of symbol tables.  These files
 * are self contained.  The thought of opening these files up and making
 * the internal type information available to the rest of the program
 * is quite repulsive.
 *
 * What we do instead is register our global variables with these routines.
 * This means that globals should be either longs or pointers (I'm assuming
 * that the space required by these two types are equivalent).  Then prior
 * to saving state, we can get the values needed to properly restore the
 * state at a later time.
 *
 * Space can only be allocated -- never freed.  This is not a problem as
 * all of our code which uses these facilities is designed to reuse the
 * space already allocated to it if a larger space is needed.  See symtab.c
 * for an excellent example.
 *
 * als_mem is the pointer to all of the memory necessary to create a saved
 * code state.  These means that all procedure table entries, clauses,
 * module information, symbol table information, and any variables which
 * reference these entities is stored here or is registered so that it
 * may be stored here when a save is performed.
 *
 */

#include "alsmem.h"

static long *als_mem;

static	long *	alloc_big_block		PARAMS(( size_t, int ));
static	long *	ss_malloc0		PARAMS(( size_t, int, int, long * ));
static	void	ss_restore_state	PARAMS(( char *, long ));
static	int	ss_saved_state_present	PARAMS(( void ));

#define header (* (struct am_header *) als_mem)

#define FB_SIZE(p)  (* (long *) p)
#define FB_NEXT(p)  (* ((long **) p + 1))

#undef round
#define round(x,s) ((((long) (x) - 1) & ~(long)((s)-1)) + (s))

/* #if defined(HAVE_MMAP) || defined(MACH_SUBSTRATE) */
#if	(defined(HAVE_MMAP) && (defined(HAVE_DEV_ZERO) || defined(HAVE_MMAP_ZERO))) || defined(MACH_SUBSTRATE)
/*
 * next_big_block_addr is the location at which we will attempt to allocate
 * the next big block of memory that our prolog system requests.  Note that
 * this variable is only used on systems having mmap or similar facilities.
 */

static long next_big_block_addr;

#endif	/* defined(HAVE_MMAP) || defined(MACH_SUBSTRATE) */

static long *
alloc_big_block(size, fe_num)
    size_t size;
    int fe_num;
{
    long *np;

    size = round(size, pgsize);

/* #if defined(HAVE_MMAP) && defined(HAVE_DEV_ZERO) */
#if	defined(HAVE_MMAP) && (defined(HAVE_DEV_ZERO) || defined(HAVE_MMAP_ZERO))
    {
	int fd;
	/*
	 * Open /dev/zero for read/write access.  /dev/zero is essentially a
	 * file of infinitely (or at least as many as we need) zeros.
	 */

	if ((fd = open("/dev/zero", O_RDWR|O_BINARY)) == -1)
	    fatal_error(FE_DEVZERO, 0);

	/*
	 * Call mmap to allocate the memory and map it to /dev/zero.  Note
	 * that we are using the MAP_PRIVATE flag or'd with the MAP_FIXED
	 * flag.
	 */

	np = (long *) mmap((caddr_t) next_big_block_addr,
				size,
#if defined(arch_sparc)
				PROT_EXEC |
#endif  /* arch_sparc */
				PROT_READ | PROT_WRITE,
				MAP_PRIVATE | MAP_FIXED,
				fd,
				0);

	close(fd);			/* close /dev/zero */

	/*
	 * Error out if something went wrong with mmap call above
	 */

	if (np == (long *) - 1)
	    fatal_error(fe_num, 0);
	
	next_big_block_addr += size;
    }
#elif defined(MACH_SUBSTRATE)
    np = (long *) next_big_block_addr;
    if (KERN_SUCCESS != vm_allocate(task_self(), (vm_address_t *) &np,
				    size, FALSE)
	|| np != (long *) next_big_block_addr)
	fatal_error(fe_num, 0);
    next_big_block_addr += size;
#elif defined(HAVE_BRK) 
    {
	long *bp = (long *) sbrk(0);

	if (bp == (long *) -1)
	    fatal_error(fe_num,0);
	
	np = (long *) round(bp, pgsize);
    
	if (np != bp && brk(np) != 0)
	    fatal_error(fe_num, 0);
    
	if (sbrk((int)size) == (char *) -1)
	    fatal_error(fe_num, 0);
    }
#else /* HAVE_BRK */
	np = (long *)malloc(size);
	
	if (np == NULL)
	    fatal_error(fe_num, 0);
#endif /* HAVE_MMAP */
    
    return np;
}

int
als_mem_init(file,offset)
    char *file;
    long offset;
{

    /*
     * Try to get an honest value for pgsize
     */

#ifdef _SC_PAGESIZE
    pgsize = sysconf(_SC_PAGESIZE);
#elif defined (MACH_SUBSTRATE)
    pgsize = vm_page_size;
#elif defined(HAVE_GETPAGESIZE)
    pgsize = getpagesize();
#endif  /* _SC_PAGESIZE */


    if (ss_saved_state_present())
	return 1;	/* saved state loaded */

    else if (!file) {	/* no file specified */
/* #if defined(HAVE_MMAP) || defined(MACH_SUBSTRATE) */
#if	(defined(HAVE_MMAP) && (defined(HAVE_DEV_ZERO) || defined(HAVE_MMAP_ZERO))) || defined(MACH_SUBSTRATE)
	next_big_block_addr = CODESTART;
#else 
	/* Leave malloc and friends some space to work with */
	char *mp = malloc(AM_MALLOCSPACE);
	if (mp)
	    free(mp);
	else
	    fatal_error(FE_ALS_MEM_INIT,0);
#endif /* defined(HAVE_MMAP) || defined(MACH_SUBSTRATE) */

	als_mem = alloc_big_block(AM_BLOCKSIZE, FE_ALS_MEM_INIT);

	header.nblocks = 1;
	header.totsize = AM_BLOCKSIZE;
	header.nglobals = 0;
	header.blocks[0].start = als_mem;
	header.blocks[0].asize = AM_BLOCKSIZE;

	header.freelist = (long *) 
		((char *) als_mem + sizeof (struct am_header));
	FB_SIZE(header.freelist) = 
		header.blocks[0].asize - sizeof (struct am_header);
	FB_NEXT(header.freelist) = (long *) 0;

	/* Set integrity information in case saved state is created */
	header.integ_als_mem = &als_mem;
	header.integ_als_mem_init = als_mem_init;
	header.integ_w_unify = w_unify;
	strcpy(header.integ_version_num, SysVersionNum);
	strcpy(header.integ_processor, ProcStr);
	strcpy(header.integ_minor_os, MinorOSStr);

/* #if defined(HAVE_MMAP) || defined(MACH_SUBSTRATE) */
#if	(defined(HAVE_MMAP) && (defined(HAVE_DEV_ZERO) || defined(HAVE_MMAP_ZERO))) || defined(MACH_SUBSTRATE)
	ss_register_global(&next_big_block_addr);
#endif	/* defined(HAVE_MMAP) || defined(MACH_SUBSTRATE) */

	return 0;	/* no saved state */
    }
    else {		/* need to open specified file and load it */
	ss_restore_state(file,offset);
	return 1;	/* saved state loaded */
    }
}

#define SMALLEST_BLOCK_SIZE 16

static long *
ss_malloc0(size, align, fe_num, actual_sizep)
    size_t size;
    int align;
    int fe_num;
    long *actual_sizep;
{
    long **bestp = (long **) 0;
    long best_size = 0x7fffffff;
    long best_diff = 0;
    long **pp;
    long p_size;
    long p_diff;
    long *retblock;

    if (align)
	size = round(size, pgsize);
    else
	size = round(size,8);
    
    p_diff = 0;
    for (pp = &header.freelist; *pp; pp = &FB_NEXT(*pp)) {
	if (align)
	    p_diff = (char *) round(*pp, pgsize) - (char *) *pp;
	p_size = FB_SIZE(*pp) - p_diff;
	if (p_size >= 0 && size <= p_size && p_size <= best_size) {
	    bestp = pp;
	    best_size = p_size;
	    best_diff = p_diff;
	    if (best_size < size+SMALLEST_BLOCK_SIZE)
		break;		/* have best size with no more work */
	}
    }

    if (bestp == (long **) 0) {
	/* could not find block of ample size; must allocate new big block  */
	long *newblock;
	int newsize = AM_BLOCKSIZE;
	if (size > newsize)
	    newsize = round(size, pgsize);

	if (header.nblocks >= AM_MAXBLOCKS)
	    fatal_error(fe_num, 0);

	newblock = alloc_big_block((size_t)newsize, fe_num);

	FB_NEXT(newblock) = header.freelist;
	FB_SIZE(newblock) = newsize;
	header.freelist = newblock;
	bestp = &header.freelist;
	header.blocks[header.nblocks].start = newblock;
	header.blocks[header.nblocks].asize = newsize;
	header.nblocks++;
	best_size = newsize;
	best_diff = 0;		/* newblock is aligned properly */
    }

    retblock = (long *) ((char *) *bestp + best_diff);
    if (best_diff >= SMALLEST_BLOCK_SIZE)
	/* adjust size of unaligned space */
	FB_SIZE(*bestp) = best_diff;
    else
	/* remove block from free list */
	*bestp = FB_NEXT(*bestp);
    
    if (best_size - size >= SMALLEST_BLOCK_SIZE) {
	long *fb = (long *) ((char *) retblock + size);
	FB_SIZE(fb) = best_size - size;
	FB_NEXT(fb) = header.freelist;
	header.freelist = fb;
    }

    if (actual_sizep)
	*actual_sizep = size;

    return retblock;
}

long *
ss_pmalloc(size,fe_num,actual_sizep)
    size_t size;
    int fe_num;
    long *actual_sizep;
{
    return ss_malloc0(size,1,fe_num,actual_sizep);
}

long *
ss_malloc(size,fe_num)
    size_t size;
    int fe_num;
{
    return ss_malloc0(size,0,fe_num,(long *) 0);
}

/*
 * The following two functions are for use with the dynamic foreign
 * interface.
 *
 * ss_fmalloc_start returns the address where alloc_big_block will allocate
 * its next block of memory.
 *
 * ss_fmalloc actually allocates this memory and returns a pointer to
 * it.
 */

long *
ss_fmalloc_start()
{
    long *retval;
/* #if defined(HAVE_MMAP) || defined(MACH_SUBSTRATE) */
#if	(defined(HAVE_MMAP) && (defined(HAVE_DEV_ZERO) || defined(HAVE_MMAP_ZERO))) || defined(MACH_SUBSTRATE)
    retval = (long *) next_big_block_addr;
#elif defined(HAVE_BRK)
    retval = (long *) sbrk(0);
    retval = (long *) round((long) retval,pgsize);
#else
	retval = (long *) -1;
#endif	/* defined(HAVE_MMAP) || defined(MACH_SUBSTRATE) */
    if (retval == (long *) -1)
	fatal_error(FE_SS_FMALLOC,0);
    return retval;
}

long *
ss_fmalloc(size)
    size_t size;
{
    long * newblock ;

    size = round(size,pgsize);
    newblock = alloc_big_block(size,FE_SS_FMALLOC);

    header.blocks[header.nblocks].start = newblock;
    header.blocks[header.nblocks].asize = size;
    header.nblocks++;
    return newblock;
}

void
ss_register_global(addr)
    long *addr;
{
    if (header.nglobals > AM_MAXGLOBALS)
	fatal_error(FE_SS_MAXGLOBALS,0);
    header.globals[header.nglobals].addr = addr;
    header.nglobals++;
}

int
ss_save_state(filename)
    char *filename;
{
    int   ssfd;
    int   bnum, gnum;
	int errnum;

    /*
     * Open the saved state file.
     */
    
#if defined(MacOS) || defined(WIN32)
    ssfd = open(filename, O_WRONLY | O_CREAT | O_TRUNC);
#else
    ssfd = open(filename, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, 0777);
#endif
printf("Save_state:opened %s %d\n",filename, ssfd);
    if (ssfd < 0)
	return 0;
    
    /*
     * Get the global values and save in header
     */

    for (gnum=0; gnum < header.nglobals; gnum++)
	header.globals[gnum].value = *header.globals[gnum].addr;

printf("Finished globals: nglobals=%d nblocks=%d\n",header.nglobals,header.nblocks);
    
    /*
     * Compute the fsize values for each block.  The fsize value is the
     * amount of space that the block should actually occupy in the file.
     * The reason that this is not necessarily equal to asize is because
     * it is often the case that there is a large free block sitting at
     * the end of an allocated block.  Note that we need to preserve the
     * FB_SIZE and FB_NEXT information associated with this last free block.
     * This is why we add 2*sizeof(long) to the computed size in the case
     * where we've found the appropriate free block.  If we're using mmap,
     * we will need to round this size up as well. 
     */

    for (bnum=0; bnum < header.nblocks; bnum++) {
	char *fb = (char *) header.freelist;
	char *blockstart = (char *) header.blocks[bnum].start;
	char *blockend = blockstart + header.blocks[bnum].asize;

	while (fb && (fb + FB_SIZE(fb) != blockend))
	    fb = (char *) FB_NEXT(fb);
	
	header.blocks[bnum].fsize = (fb) ? fb - blockstart +  2*sizeof(long)
					: header.blocks[bnum].asize;
/* #ifdef HAVE_MMAP */
#if	defined(HAVE_MMAP) && (defined(HAVE_DEV_ZERO) || defined(HAVE_MMAP_ZERO))
	header.blocks[bnum].fsize = round(header.blocks[bnum].fsize, pgsize);
#endif /* HAVE_MMAP */

    }
printf("Finished computing blocks\n");


    /*
     * Save the blocks to the save file.
     */
    
    for (bnum=0; bnum < header.nblocks; bnum++)
	if (write(ssfd,
		  (char *)header.blocks[bnum].start,
		  (size_t)header.blocks[bnum].fsize) < 0)
	    goto ss_err;
    
printf("Wrote blocks to file\n");

    close(ssfd);
    return 1;

ss_err:
    printf("!!Save_state error writing file: bnum=%d errno=% \n",bnum,errno);
    close(ssfd);
    unlink(filename);
    return 0;
}

#define SS_MALLOCQUANT	4096
#define SS_MALLOCDIST	16384

static void
ss_restore_state(filename,offset)
    char *filename;
    long offset;
{
    int ssfd;
    int  bnum, gnum;
    struct am_header hdr;
#if (defined(__GO32__) || defined(__DJGPP__))
	unsigned char dos_exe[6];
#endif

    /* Open the file */
    ssfd = open(filename, O_RDONLY|O_BINARY);
    if (ssfd < 0)
	fatal_error(FE_SS_OPENERR,(long)filename);

#if (defined(__GO32__) || defined(__DJGPP__))
	read(ssfd, dos_exe, sizeof(dos_exe));
	if (dos_exe[0] == 'M' && dos_exe[1] == 'Z') /* skip stub */
	{
		int blocks = (unsigned int)dos_exe[4] + (unsigned int)dos_exe[5] * 256;
		int partial = (unsigned int)dos_exe[2] + (unsigned int)dos_exe[3] * 256;
		offset += blocks * 512;
		if (partial)
			offset += partial - 512;
	}
#endif

    /* Seek to header, get header, and seek back to header */
    if (lseek(ssfd, offset, 0) < 0)
	goto ss_err;
    
    if (read(ssfd, (char *)&hdr, sizeof (struct am_header)) < 0)
	goto ss_err;
    
    if (lseek(ssfd, offset, 0) < 0)
	goto ss_err;
    
    /* Check integrity information */
    
    if (hdr.integ_als_mem != &als_mem ||
		hdr.integ_als_mem_init != als_mem_init ||
		hdr.integ_w_unify != w_unify ||
		strcmp(hdr.integ_version_num, SysVersionNum) != 0 ||
		strcmp(hdr.integ_processor, ProcStr) != 0 ||
		strcmp(hdr.integ_minor_os, MinorOSStr) != 0)
	fatal_error(FE_SS_INTEGRITY,0);

/* #if	defined(HAVE_MMAP) && defined(HAVE_DEV_ZERO) */
#if	defined(HAVE_MMAP) && (defined(HAVE_DEV_ZERO) || defined(HAVE_MMAP_ZERO))
    /* Get the blocks */
    for (bnum = 0; bnum < hdr.nblocks; bnum++) {

	if ((long *) mmap((caddr_t) hdr.blocks[bnum].start,
			    (size_t)hdr.blocks[bnum].fsize,
#if defined(arch_sparc)
			    PROT_EXEC |
#endif  /* arch_sparc */
			    PROT_READ | PROT_WRITE,
			    MAP_PRIVATE | MAP_FIXED,
			    ssfd,
			    offset) != hdr.blocks[bnum].start)
	    goto ss_err;
	offset += hdr.blocks[bnum].fsize;

	/*
	 * If the size of the block in the file is not equal to the size
	 * which needs to be allocated, we need to allocate the remaining
	 * portion by mapping /dev/zero...
	 */

	if (hdr.blocks[bnum].asize != hdr.blocks[bnum].fsize) {
	    int zfd;
	    caddr_t np;
	    if ((zfd = open("/dev/zero", O_RDWR)) == -1)
		goto ss_err;

	    np = (caddr_t) mmap((caddr_t) hdr.blocks[bnum].start 
	                                  + hdr.blocks[bnum].fsize,
				(size_t)(hdr.blocks[bnum].asize 
				         - hdr.blocks[bnum].fsize),
#if defined(arch_sparc)
				PROT_EXEC |
#endif  /* arch_sparc */
				PROT_READ | PROT_WRITE,
				MAP_PRIVATE | MAP_FIXED,
				zfd,
				0);

	    close(zfd);			/* close /dev/zero */
	    if (np != (caddr_t) hdr.blocks[bnum].start + hdr.blocks[bnum].fsize)
		goto ss_err;
	}
    }

#elif	defined(MACH_SUBSTRATE)
    /* Get the blocks */
    for (bnum = 0; bnum < hdr.nblocks; bnum++) {
	long *bs = hdr.blocks[bnum].start;
        if (KERN_SUCCESS != vm_allocate(task_self(), (vm_address_t *) &bs,
				    hdr.blocks[bnum].asize, FALSE)
	|| bs != hdr.blocks[bnum].start)
	    goto ss_err;
	if (KERN_SUCCESS !=
		map_fd(	ssfd,
			(vm_offset_t) offset,
			(vm_offset_t *) &bs,
			FALSE,
			(vm_size_t) (hdr.blocks[bnum].fsize)))
	    goto ss_err;
	offset += hdr.blocks[bnum].fsize;
    }
    
#elif	defined(HAVE_BRK)
    {
	long *mchain = (long *) 0;
	long **mptr = (long **) 0;
	char *bstart, *bend;
	/* Get the blocks */
	for (bnum=0; bnum < hdr.nblocks; bnum++) {
	    bstart = (char *) hdr.blocks[bnum].start;
	    bend = bstart + hdr.blocks[bnum].asize;
	    while ((char *) sbrk(0) < bstart-SS_MALLOCDIST) {
		mptr = (long **) malloc(SS_MALLOCQUANT);
		if ((long) mptr < 0)
		    goto ss_err;
		*mptr = mchain;
		mchain = (long *) mptr;
	    }
	    if ((char *) sbrk(0) > bstart || brk(bend) < 0)
		goto ss_err;
	    if (read(ssfd,bstart,hdr.blocks[bnum].fsize) < 0)
		goto ss_err;
	}
    
	/* Free up memory malloc'd in previous step */
	while (mchain) {
	    mptr = (long **) mchain;
	    mchain = *mptr;
	    free((char *) mptr);
	}
    }
#else

    /* ceh - We need a non-brk implementation of the above code! */
    goto ss_err;

#endif	/* HAVE_MMAP */

    /* Initialize the globals */
    for (gnum=0; gnum < hdr.nglobals; gnum++)
	*hdr.globals[gnum].addr = hdr.globals[gnum].value;
    
    als_mem = hdr.blocks[0].start;
    
    close(ssfd);
    return;


ss_err:
    close(ssfd);
    fatal_error(FE_ALS_MEM_INIT,0);
}

/*
 * ss_saved_state_present will test to see if the saved state is already
 * magically present in the image.  If it is, then all we have to do
 * is set als_mem and initialize the globals.
 */

static int
ss_saved_state_present()
{
#ifdef MACH_SUBSTRATE
    kern_return_t stat;
    pointer_t dummy_buf;
    unsigned int dummy_size;

    stat = vm_read( task_self(),
		    (vm_address_t) CODESTART,
		    pgsize,
		    &dummy_buf,
		    &dummy_size );


    if (stat != KERN_SUCCESS)
	return 0;
    else {
	int gnum;

	(void) vm_deallocate( task_self(),
			      (vm_address_t) dummy_buf,
			      (vm_size_t) dummy_size );

	als_mem = (long *) CODESTART;

	/* Initialize the globals */
	for (gnum=0; gnum < header.nglobals; gnum++)
	    *header.globals[gnum].addr = header.globals[gnum].value;

	return 1;
    }
#else	/* MACH_SUBSTRATE */
    return 0;
#endif	/* MACH_SUBSTRATE */
}
