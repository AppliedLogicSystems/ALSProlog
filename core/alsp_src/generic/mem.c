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
#include <Processes.h>
#include <Resources.h>
#include <Errors.h>
#include <FileCopy.h> /* From MoreFiles - from Macintosh Sample Code library. */
#endif

#ifdef MSWin32
#include <windows.h>
#include "fswin32.h"
#endif

#include <errno.h>

#ifndef O_BINARY
#define O_BINARY 0
#endif

#ifndef PURE_ANSI
static	void	coredump_cleanup	PARAMS(( int ));
#endif /* PURE_ANSI */

static	int	pgsize = 8192;	/* A guess at the page size */

/* MAX_PAGE_SIZE is the largest possible page size of the OS */
#define MAX_PAGE_SIZE 0x10000

#ifdef PARAMREVBIT
unsigned long AddressHiBit = 0x0;
unsigned long ReversedHiBit = 0x80000000;
#endif

#define SIGSTKSIZE 8192		/* Size to use for a signal stack */

/* #if defined(HAVE_MMAP) || defined(MACH_SUBSTRATE) */

#if	(defined(HAVE_MMAP) && (defined(HAVE_DEV_ZERO) || defined(HAVE_MMAP_ZERO))) \
	|| defined(MACH_SUBSTRATE) || defined(MSWin32)

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
#elif __hp9000s800
/* On HP-UX the first Gigabyte of low memory is used for the program
   code (text).  These locations seem to work on 700 series machines
   but the memory map is slightly different under 800 - so there could
   be problems.
*/
#define	STACKSTART	0x48000000
#define CODESTART	0x4d000000
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
void stack_overflow(int, siginfo_t *, ucontext_t *);
void stack_overflow(int signum, siginfo_t *siginf, ucontext_t *sigcon)

#elif defined(HAVE_SIGVEC) || defined(HAVE_SIGVECTOR)
void	stack_overflow	PARAMS(( int, int, struct sigcontext *, caddr_t ));
void
stack_overflow(int signum, int code, struct sigcontext *scp, caddr_t addr)
#elif defined(Portable)
void   stack_overflow  PARAMS(( int ));
void
stack_overflow(int signum)

#else
#error
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
#elif defined(Portable)
   signal_handler(ALSSIG_STACK_OVERFLOW);
#else
#error
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

#elif defined(Portable)
	signal(signum, SIG_DFL);
#else
#error
#endif

	coredump_cleanup(-1);

    }
}


PWord *
allocate_prolog_heap_and_stack(size)
    size_t size;		/* number of PWords to allocate */
{
#ifndef HAVE_MMAP_ZERO
    int   fd;
#endif
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
#elif defined(_SC_PAGE_SIZE)
    pgsize = sysconf(_SC_PAGE_SIZE);
#elif defined(HAVE_GETPAGESIZE)
    pgsize = getpagesize();
#endif  /* _SC_PAGESIZE */

    /*-------------------------------------------------------------*
     * Open /dev/zero for read/write access.  /dev/zero is essentially a
     * file of infinitely (or at least as many as we need) zeros.
     *-------------------------------------------------------------*/

#ifndef HAVE_MMAP_ZERO
    if ((fd = open("/dev/zero", O_RDWR)) == -1)
	fatal_error(FE_DEVZERO, 0);
#endif

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
#ifdef HAVE_MMAP_ZERO
    retval = (PWord *) mmap((caddr_t) STACKSTART,
			    size * sizeof (PWord) + NPROTECTED * pgsize,
			    PROT_READ | PROT_WRITE,
			    MAP_ANONYMOUS | MAP_PRIVATE | MAP_FIXED,
			    -1,
			    0);
#else
    retval = (PWord *) mmap((caddr_t) STACKSTART,
			    size * sizeof (PWord) + NPROTECTED * pgsize,
			    PROT_READ | PROT_WRITE,
			    MAP_PRIVATE | MAP_FIXED,
			    fd,
			    0);


    close(fd);			/* close /dev/zero */
#endif

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
    int result;
    result = mprotect((caddr_t) STACKSTART, (size_t)(NPROTECTED * pgsize), PROT_NONE);
    if (result == -1)
        fatal_error(FE_BIGSTACK, 0);
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

#elif defined(MSWin32)

/* These fixed memory locations are very problematic.  There is no
   way to guarantee they will work.  All we know is that these numbers
   work at ALS and Customer sites.

   Previously STACKSTART was set 0x01000000, which worked fine for Win95
   and WinNT 3.51, but under WinNT 4.0 that location is in use.
*/

#define STACKSTART	0x02000000
#define CODESTART	0x06000000
#define NPROTECTED	5


static void release_bottom_stack_page(void)
{
    DWORD OldProtection;

    if (!VirtualProtect((void *)STACKSTART, NPROTECTED * pgsize, PAGE_READWRITE, &OldProtection)) {
	fatal_error(FE_STKOVERFLOW, 0);
    }
    
    bottom_stack_page_is_protected = 0;
}

static LONG WINAPI stack_overflow(struct _EXCEPTION_POINTERS *lpexpExceptionInfo)
{
    if (lpexpExceptionInfo->ExceptionRecord->ExceptionCode == EXCEPTION_ACCESS_VIOLATION
        && bottom_stack_page_is_protected) {
	release_bottom_stack_page();
	signal_handler(ALSSIG_STACK_OVERFLOW);
	return EXCEPTION_CONTINUE_EXECUTION;
    } else {
    	return EXCEPTION_CONTINUE_SEARCH;
    }	    
}

void
protect_bottom_stack_page()
{
    DWORD OldProtection;

    if (!VirtualProtect((void *)STACKSTART, NPROTECTED * pgsize, PAGE_NOACCESS /*PAGE_READWRITE | PAGE_GUARD*/, &OldProtection)) {
	fatal_error(FE_STKOVERFLOW, 0);
    }

    bottom_stack_page_is_protected = 1;
}

PWord *
allocate_prolog_heap_and_stack(size)
    size_t size;				/* number of PWords to allocate */
{
    if (win32s_system) {
    PWord *retval = (PWord *) malloc(sizeof (PWord) * size);

    if (retval == 0)
	fatal_error(FE_BIGSTACK, 0);

	AddressHiBit = (((unsigned long)retval) & 0x80000000);
	ReversedHiBit = (~AddressHiBit & 0x80000000);
	if (AddressHiBit != ((((unsigned long)retval) + size) & 0x80000000))
	fatal_error(FE_TAGERR, 0);


/* These signals are not available on the Mac. */
#ifndef MacOS
#ifdef SIGBUS
    (void) signal(SIGBUS, coredump_cleanup);
#endif
    (void) signal(SIGSEGV, coredump_cleanup);
#endif
    return retval;

    
    } else {
    LPVOID stack;
    
    SetUnhandledExceptionFilter(stack_overflow);
    
    stack = VirtualAlloc((LPVOID)STACKSTART,
    			 size * sizeof(PWord) + NPROTECTED * pgsize,
    		         MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
    
    if (stack != (LPVOID)STACKSTART)
	fatal_error(FE_BIGSTACK, 0);

    protect_bottom_stack_page();

    return (PWord *)STACKSTART;
    }
}




#elif defined(MACH_SUBSTRATE)
	/*------- Operating systems with Mach as the substrate -------*/

#include <mach/mach.h>

#define STACKSTART	0x01000000
#define CODESTART	0x06000000
#define NPROTECTED	1

#if defined(HAVE_SIGACTION) || defined(HAVE_SIGVEC)
static long signal_stack[SIGSTKSIZE];
#endif

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
void stack_overflow PARAMS((int));

void
stack_overflow(signum)
	int signum;
{
    fatal_error(FE_STKOVERFLOW, 0);
}

#endif /* arch_m88k */

PWord *
allocate_prolog_heap_and_stack(size)
    size_t size;			/* number of PWords to allocate */
{
    PWord *retval = (PWord *) malloc(sizeof (PWord) * size);

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
#ifndef PURE_ANSI
#ifndef MacOS
#ifdef SIGBUS
    (void) signal(SIGBUS, coredump_cleanup);
#endif
    (void) signal(SIGSEGV, coredump_cleanup);
#endif
#endif /* PURE_ANSI */
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

#ifndef PURE_ANSI
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
#endif /* PURE_ANSI */


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
#ifndef PURE_ANSI
static	void	ss_restore_state	PARAMS(( const char *, long ));
#endif /* PURE_ANSI */
static	int	ss_saved_state_present	PARAMS(( void ));

#define amheader (* (struct am_header *) als_mem)

#define FB_SIZE(p)  (* (long *) p)
#define FB_NEXT(p)  (* ((long **) p + 1))

#undef round
#define round(x,s) ((((long) (x) - 1) & ~(long)((s)-1)) + (s))

#define round_up(x,s) (((size_t)(x)) + (((size_t)(s)) - ((size_t)(x))%((size_t)(s)))%((size_t)(s)))
#define round_down(x,s) (((size_t)(x)) - (((size_t)(x))%((size_t)(s))))

/* #if defined(HAVE_MMAP) || defined(MACH_SUBSTRATE) */
#if	(defined(HAVE_MMAP) && (defined(HAVE_DEV_ZERO) || defined(HAVE_MMAP_ZERO))) \
	|| defined(MACH_SUBSTRATE) || defined(MSWin32)
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
#ifdef HAVE_MMAP_ZERO
	np = (long *) mmap((caddr_t) next_big_block_addr,
				size,
				PROT_READ | PROT_WRITE,
				MAP_ANONYMOUS | MAP_PRIVATE | MAP_FIXED,
				-1,
				0);
#else
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
#endif
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
#elif defined(MSWin32)
    
    if (win32s_system) {
	np = (long *)malloc(size);
	
	if (np == NULL)
	    fatal_error(fe_num, 0);
    
    } else {
    np = VirtualAlloc((void *)next_big_block_addr, size, MEM_RESERVE | MEM_COMMIT,
    			PAGE_READWRITE);
    
    if (np == NULL)
        fatal_error(fe_num, 0);
 
    next_big_block_addr += size;
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
    const char *file;
    long offset;
{

    /*
     * Try to get an honest value for pgsize
     */

#ifdef _SC_PAGESIZE
    pgsize = sysconf(_SC_PAGESIZE);
#elif _SC_PAGE_SIZE
    pgsize = sysconf(_SC_PAGE_SIZE);
#elif defined (MACH_SUBSTRATE)
    pgsize = vm_page_size;
#elif defined(HAVE_GETPAGESIZE)
    pgsize = getpagesize();
#endif  /* _SC_PAGESIZE */

    if (ss_saved_state_present())
	return 1;	/* saved state loaded */

    else if (!file) {	/* no file specified */
/* #if defined(HAVE_MMAP) || defined(MACH_SUBSTRATE) */
#if	(defined(HAVE_MMAP) && (defined(HAVE_DEV_ZERO) || defined(HAVE_MMAP_ZERO))) \
	|| defined(MACH_SUBSTRATE) || defined(MSWin32)
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

	amheader.nblocks = 1;
	amheader.totsize = AM_BLOCKSIZE;
	amheader.nglobals = 0;
	amheader.blocks[0].start = als_mem;
	amheader.blocks[0].asize = AM_BLOCKSIZE;

	amheader.freelist = (long *) 
		((char *) als_mem + sizeof (struct am_header));
	FB_SIZE(amheader.freelist) = 
		amheader.blocks[0].asize - sizeof (struct am_header);
	FB_NEXT(amheader.freelist) = (long *) 0;

	/* Set integrity information in case saved state is created */
	amheader.integ_als_mem = &als_mem;
	amheader.integ_als_mem_init = als_mem_init;
	amheader.integ_w_unify = w_unify;
	strcpy(amheader.integ_version_num, SysVersionNum);
	strcpy(amheader.integ_processor, ProcStr);
	strcpy(amheader.integ_minor_os, MinorOSStr);

/* #if defined(HAVE_MMAP) || defined(MACH_SUBSTRATE) */
#if	(defined(HAVE_MMAP) && (defined(HAVE_DEV_ZERO) || defined(HAVE_MMAP_ZERO))) || \
	defined(MACH_SUBSTRATE) || defined(MSWin32)
	ss_register_global(&next_big_block_addr);
#endif	/* defined(HAVE_MMAP) || defined(MACH_SUBSTRATE) */

	return 0;	/* no saved state */
    }
    else {		/* need to open specified file and load it */
#ifndef PURE_ANSI
	ss_restore_state(file,offset);
#endif /* PURE_ANSI */
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
    for (pp = &amheader.freelist; *pp; pp = &FB_NEXT(*pp)) {
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

	if (amheader.nblocks >= AM_MAXBLOCKS)
	    fatal_error(fe_num, 0);

	newblock = alloc_big_block((size_t)newsize, fe_num);

	FB_NEXT(newblock) = amheader.freelist;
	FB_SIZE(newblock) = newsize;
	amheader.freelist = newblock;
	bestp = &amheader.freelist;
	amheader.blocks[amheader.nblocks].start = newblock;
	amheader.blocks[amheader.nblocks].asize = newsize;
	amheader.nblocks++;
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
	FB_NEXT(fb) = amheader.freelist;
	amheader.freelist = fb;
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
#if	(defined(HAVE_MMAP) && (defined(HAVE_DEV_ZERO) || defined(HAVE_MMAP_ZERO))) || \
	defined(MACH_SUBSTRATE) || defined(MSWin32)
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

    amheader.blocks[amheader.nblocks].start = newblock;
    amheader.blocks[amheader.nblocks].asize = size;
    amheader.nblocks++;
    return newblock;
}

void
ss_register_global(addr)
    long *addr;
{
    if (amheader.nglobals > AM_MAXGLOBALS)
	fatal_error(FE_SS_MAXGLOBALS,0);
    amheader.globals[amheader.nglobals].addr = addr;
    amheader.nglobals++;
}

#ifdef SIMPLE_MICS
#ifdef MSWin32
/* ms_pecoff_end calculated the offset to the end of a
   Microsoft Portable-Executable/Common-Object-File-Format file.
   
   The result is the offset to the first byte AFTER the pe/coff data.
 */
static DWORD ms_pecoff_end(HANDLE image_file)
{
    DWORD r;
    WORD head_offset;
    IMAGE_FILE_HEADER head;
    IMAGE_SECTION_HEADER section;
    long end;
    int s;
    
    r = SetFilePointer(image_file, 0x3c, NULL, FILE_BEGIN);
    if (r == 0xFFFFFFFF) return 0;
    
    if (!ReadFile(image_file, &head_offset, sizeof(head_offset), &r, NULL)
    	|| r != sizeof(head_offset)) return 0;
    
    r = SetFilePointer(image_file, head_offset+4, NULL, FILE_BEGIN);
    if (r == 0xFFFFFFFF) return 0;
    
    if (!ReadFile(image_file, &head, sizeof(head), &r, NULL)
    	|| r != sizeof(head)) return 0;
    	
    end = head_offset + 4 + sizeof(head) + head.SizeOfOptionalHeader;

    r = SetFilePointer(image_file, end, NULL, FILE_BEGIN);
    if (r == 0xFFFFFFFF) return 0;
    
    
    for (s = 0; s < head.NumberOfSections; s++) {
	if (!ReadFile(image_file, &section, sizeof(section), &r, NULL)
	    || r != sizeof(section)) return 0;
	end = max(end, section.PointerToRawData + section.SizeOfRawData);
    }

    return end;
}

long ss_image_offset(void)
{
    char image_name[MAX_PATH];
    DWORD l, image_size, pecoff_size;
    HANDLE image_file;
    
    l = GetModuleFileName(NULL, image_name, MAX_PATH);
    if (l <= 0 || l >= MAX_PATH) return 0;
    
    image_file = CreateFile(image_name, GENERIC_READ, FILE_SHARE_READ, NULL,
                            OPEN_EXISTING, FILE_FLAG_RANDOM_ACCESS, NULL);
    if (image_file == INVALID_HANDLE_VALUE) return 0;

    pecoff_size = ms_pecoff_end(image_file);
    image_size = GetFileSize(image_file, NULL);
    
    CloseHandle(image_file);

    if (image_size == 0xFFFFFFFF) return 0;
    
    if (image_size > pecoff_size) return pecoff_size;
    else return 0;
}

int ss_save_image_with_state(const char * new_image_name)
{
    char image_name[MAX_PATH];
    DWORD l, state_offset;
    HANDLE new_image_file;
    
    l = GetModuleFileName(NULL, image_name, MAX_PATH);
    if (l <= 0 || l >= MAX_PATH) {
    	printf("ss_save_image: Couldn't get module name.\n");
    	return 0;
    }
    
    if (!CopyFile(image_name, new_image_name, FALSE)) {
    	printf("ss_save_image: Couldn't copy %s to %s\n", image_name, new_image_name);
    	return 0;
    }
    
    new_image_file = CreateFile(new_image_name, GENERIC_READ, FILE_SHARE_READ, NULL,
                            OPEN_EXISTING, FILE_FLAG_RANDOM_ACCESS, NULL);
    if (new_image_file == INVALID_HANDLE_VALUE) {
    	printf("ss_save_image: Couldn't open %s\n", new_image_name);
    	return 0;
    }
   
    state_offset = ms_pecoff_end(new_image_file);

    CloseHandle(new_image_file);

    if (state_offset == 0) {
    	printf("ss_save_image: Couldn't find end of PE/COFF file %s\n", new_image_name);
    	return 0;
    }
        
    return ss_save_state(new_image_name, state_offset);
}

#endif /* MSWin32 */

#ifdef UNIX
#if defined(HP_AOUT_800)

#include <filehdr.h>

static unsigned long image_end(int image_file)
{
    size_t r;
    struct header head;
    
    r = read(image_file, &head, sizeof(head));
    if (r != sizeof(head)) return 0;
    
    return head.som_length;
}

#endif /* HP_AOUT_800 */

#define IMAGENAME_MAX	64
#define IMAGEDIR_MAX	1024
extern char  imagename[IMAGENAME_MAX];
extern char  imagedir[IMAGEDIR_MAX];

#ifdef HAVE_LIBELF
#include <libelf.h>

#ifdef USE_ELF_SECTION_FOR_IMAGE

static Elf_Scn *find_named_section(Elf *elf, const char *name, int create)
{
    Elf32_Ehdr *header;
    Elf32_Half secstr_index;
    Elf_Scn *section, *str_section;
    Elf32_Shdr *section_header;
    char *section_name;
    Elf_Data *str_data;
    void *new_d_buf;
    Elf32_Word name_offset;
    size_t name_size, new_d_size;
    
    header = elf32_getehdr(elf);
    if (!header) return NULL;
    
    secstr_index = header->e_shstrndx;
    section = NULL;

    while ((section = elf_nextscn(elf, section))) {
    	section_header = elf32_getshdr(section);
    	if (!section_header) return NULL;
    	section_name = elf_strptr(elf, secstr_index, section_header->sh_name);
    	if (!section_name) return NULL;
    	if (strcmp(name, section_name) == 0) return section;
    }

    if (create) {
    	name_size = strlen(name)+1;
    	
    	str_section = elf_getscn(elf, secstr_index);
    	str_data = elf_getdata(str_section, NULL);
    	new_d_size = str_data->d_size + name_size;
    	new_d_buf = malloc(new_d_size);
    	if (!new_d_buf) return NULL;
    	memcpy(new_d_buf, str_data->d_buf, str_data->d_size);
    	memcpy(new_d_buf+str_data->d_size, name, name_size);
    	name_offset = str_data->d_size;
    	
    	str_data->d_buf = new_d_buf;
    	str_data->d_size = new_d_size;
    	
    	elf_flagdata(str_data, ELF_C_SET, ELF_F_DIRTY);
    	
    	section = elf_newscn(elf);
    	if (!section) return NULL;
    	section_header = elf32_getshdr(section);
    	if (!section_header) return NULL;
    	
    	section_header->sh_name = name_offset;
    	
    	return section;
    } else return NULL;
}

long ss_image_offset(void)
{
  char *imagepath = (char *) malloc(strlen(imagename)+strlen(imagedir)+1);
  int image_file, r;
  Elf *elf;
  Elf_Scn *scn;
  Elf32_Shdr *shdr;
  long image_offset;

  if (imagepath == NULL)
    return 0;

  if (elf_version(EV_CURRENT) == EV_NONE) return 0;
    
  strcpy(imagepath,imagedir);
  strcat(imagepath,imagename);

  image_file = open(imagepath, O_RDONLY);
  if (image_file == -1)
    fatal_error(FE_SS_OPENERR,(long)imagepath);
    
  elf = elf_begin(image_file, ELF_C_READ, NULL);
  if (elf == NULL)
    fatal_error(FE_SS_OPENERR,(long)imagepath);

  free(imagepath);

  scn = find_named_section(elf, "ALS Prolog State", 0);
  if (scn == NULL) image_offset = 0;
  else {
    shdr = elf32_getshdr(scn);
    if (shdr == NULL) image_offset =  0;
    else image_offset = shdr->sh_offset;
  }

  elf_end(elf);

  r = close(image_file);
  if (r == -1)
    fatal_error(FE_SS_OPENERR,(long)imagepath);

  return image_offset;
}

#else

static unsigned long image_end(int image_file)
{
    Elf *elf;
    Elf_Scn *scn;
    Elf32_Shdr *shdr;
    size_t end;
    
    if (elf_version(EV_CURRENT) == EV_NONE) return 0;

    elf = elf_begin(image_file, ELF_C_READ, NULL);
    if (elf_kind(elf) != ELF_K_ELF) return 0;

    scn = NULL;
    end = 0;
    while ((scn = elf_nextscn(elf, scn))) {
        shdr = elf32_getshdr(scn);
	if (shdr == NULL) return 0;

        if (shdr->sh_type != SHT_NOBITS) {
/*
	    printf("sh_type: %u sh_offset: %u sh_size: %u\n",
		   shdr->sh_type, shdr->sh_offset, shdr->sh_size);
*/
	    end = max(end, shdr->sh_offset + shdr->sh_size);
	}
    }

    if (elf_end(elf) != 0) return 0;

    /* printf("image_end: %u\n", end); */
    return end;
}

#endif /* USE_ELF_SECTION_FOR_IMAGE */
#endif /* HAVE_LIBELF */

#ifndef USE_ELF_SECTION_FOR_IMAGE
long ss_image_offset(void)
{
    char *imagepath = (char *) malloc(strlen(imagename)+strlen(imagedir)+1);
    unsigned long file_size, image_size;
    int file, fstat_result;
    struct stat file_status;

    if (imagepath == NULL)
	return 0;
    
    strcpy(imagepath,imagedir);
    strcat(imagepath,imagename);

    file = open(imagepath, O_RDONLY);
    
    if (file == -1)
	fatal_error(FE_SS_OPENERR,(long)imagepath);

    image_size = image_end(file);
    fstat_result = fstat(file, &file_status);
        
    close(file);

    if (fstat_result != 0)
	fatal_error(FE_SS_OPENERR,(long)imagepath);

    free(imagepath);

    file_size = file_status.st_size;
 
    if (file_size > image_size)
	return image_size;
    else return 0;
}
#endif

static int copy(const char *filename, const char *copyname)
{
    unsigned char *buf;
    int f, c, r;
    struct stat s; 
    
    f = open(filename, O_RDONLY);
    if (f == -1) return -1;
    
    r = fstat(f, &s);
    if (r != 0) {
    	close(f);
        return -1;
    }
    
    buf = malloc((size_t)s.st_size);
    if (buf == NULL) {
    	close(f);
        return -1;
    }
    
    r = read(f, buf, (size_t)s.st_size);
    if (r != s.st_size) {
    	free(buf);
    	close(f);
        return -1;
    }

    close(f);
    
    c = open(copyname, O_WRONLY | O_CREAT | O_TRUNC, 0777);
    if (c == -1) {
    	free(buf);
    	return -1;
    }
    
    r = write(c, buf, (size_t)s.st_size);
    if (r != s.st_size) {
    	free(buf);
        close(f);
        return -1;
    }
    
    close(c);
    
    free(buf);
    
    return 0;
}

#ifdef USE_ELF_SECTION_FOR_IMAGE
int ss_save_image_with_state(const char * new_image_name)
{
    char *imagepath = (char *) malloc(strlen(imagename)+strlen(imagedir)+1);
    char tmp_name[L_tmpnam];
    mem_file_info tmp_mmap;
    int fd, r;
    Elf *elf;
    Elf_Scn *scn;
    Elf32_Shdr *shdr;
    Elf_Data *data;
    
    if (imagepath == NULL)
	return 0;
    
    strcpy(imagepath,imagedir);
    strcat(imagepath,imagename);

    if (copy(imagepath, new_image_name) != 0) {
    	free(imagepath);
	printf("ss_save_image: Couldn't copy %s to %s\n", imagepath, new_image_name);
    	return 0;
    }
    
    free(imagepath);

    tmpnam(tmp_name);

    ss_save_state(tmp_name, 0);

    if (!open_memory_file(tmp_name, &tmp_mmap)) return 0;

    if (elf_version(EV_CURRENT) == EV_NONE) return 0;

    fd = open(new_image_name, O_RDWR);
    if (fd == -1) return 0;

    elf = elf_begin(fd, ELF_C_RDWR, NULL);
    if (elf == NULL) return 0;

    scn = find_named_section(elf, "ALS Prolog State", 1);
    if (scn == NULL) return 0;

    shdr = elf32_getshdr(scn);
    if (shdr == NULL) return 0;

    shdr->sh_type = SHT_PROGBITS;
  
    data = elf_getdata(scn, NULL);
    if (!data) {
      data = elf_newdata(scn);
      if (data == NULL) return 0;
    }
  
    data->d_buf = tmp_mmap.start;
    data->d_size = tmp_mmap.length;
    data->d_align = MAX_PAGE_SIZE;
    data->d_type = ELF_T_WORD;

    elf_flagdata(data, ELF_C_SET, ELF_F_DIRTY);
  
    r = elf_update(elf, ELF_C_WRITE);
    if (r == -1) return 0;
  
    elf_end(elf);

    r = close(fd);
    if (r == -1) return 0;

    close_memory_file(&tmp_mmap);

    remove(tmp_name);

    return 1;
}

#else
int ss_save_image_with_state(const char * new_image_name)
{
    char *imagepath = (char *) malloc(strlen(imagename)+strlen(imagedir)+1);
    int new_image_file;
    long state_offset;
    
    if (imagepath == NULL)
	return 0;
    
    strcpy(imagepath,imagedir);
    strcat(imagepath,imagename);

    if (copy(imagepath, new_image_name) != 0) {
    	free(imagepath);
	printf("ss_save_image: Couldn't copy %s to %s\n", imagepath, new_image_name);
    	return 0;
    }
    
    free(imagepath);

    new_image_file = open(new_image_name, O_RDONLY);
    if (new_image_file == -1) {
    	printf("ss_save_image: Couldn't open %s\n", new_image_name);
    	return 0;
    }
   
    state_offset = image_end(new_image_file);

    close(new_image_file);

    if (state_offset == 0) {
    	printf("ss_save_image: Couldn't find end of image file %s\n", new_image_name);
    	return 0;
    }
        
    return ss_save_state(new_image_name, state_offset);
}
#endif

#endif /* UNIX */

#endif /* SIMPLE_MICS */

#ifndef PURE_ANSI
int
ss_save_state(const char *filename, long offset)
{
    int   ssfd;
    int   bnum = 0, gnum;
#if defined(SIMPLE_MICS)
    long delta_offset;
#endif

    /*
     * Open the saved state file.
     */
#if defined(SIMPLE_MICS)
#ifdef UNIX
    ssfd = open(filename, O_WRONLY);
    if (ssfd == -1)
      ssfd = open(filename, O_WRONLY | O_CREAT | O_TRUNC, 0777);
#else
    ssfd = open(filename, O_WRONLY | O_BINARY);
    if (ssfd == -1) ssfd = open(filename, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY);
#endif
#elif defined(__MWERKS__)
    ssfd = open(filename, O_WRONLY | O_CREAT | O_TRUNC);
#else
    ssfd = open(filename, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, 0777);
#endif
    if (ssfd < 0)
	return 0;

#if defined(SIMPLE_MICS)    
    if (lseek(ssfd, offset, 0) != offset) goto ss_err;
    
    delta_offset = round_up(offset+sizeof(offset), MAX_PAGE_SIZE) - offset;
    if (write(ssfd, (char *)&delta_offset, sizeof(delta_offset)) < 0) goto ss_err;
    offset += delta_offset;
#endif

    if (lseek(ssfd, offset, 0) != offset) goto ss_err;
    
    /*
     * Get the global values and save in amheader
     */

    for (gnum=0; gnum < amheader.nglobals; gnum++)
	amheader.globals[gnum].value = *amheader.globals[gnum].addr;

    
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

    for (bnum=0; bnum < amheader.nblocks; bnum++) {
	char *fb = (char *) amheader.freelist;
	char *blockstart = (char *) amheader.blocks[bnum].start;
	char *blockend = blockstart + amheader.blocks[bnum].asize;

	while (fb && (fb + FB_SIZE(fb) != blockend))
	    fb = (char *) FB_NEXT(fb);
	
	amheader.blocks[bnum].fsize = (fb) ? fb - blockstart +  2*sizeof(long)
					: amheader.blocks[bnum].asize;
/* #ifdef HAVE_MMAP */
#if	defined(HAVE_MMAP) && (defined(HAVE_DEV_ZERO) || defined(HAVE_MMAP_ZERO))
	amheader.blocks[bnum].fsize = round(amheader.blocks[bnum].fsize, pgsize);
#endif /* HAVE_MMAP */

    }


    /*
     * Save the blocks to the save file.
     */
    
    for (bnum=0; bnum < amheader.nblocks; bnum++)
	if (write(ssfd,
		  (char *)amheader.blocks[bnum].start,
		  (size_t)amheader.blocks[bnum].fsize) < 0)
	    goto ss_err;
    

    close(ssfd);
    return 1;

ss_err:
    printf("!!Save_state error writing file: bnum=%d errno=%d\n",bnum,errno);
    close(ssfd);
    unlink((char *)filename);
    return 0;
}

#define SS_MALLOCQUANT	4096
#define SS_MALLOCDIST	16384

static void
ss_restore_state(filename,offset)
    const char *filename;
    long offset;
{
    int ssfd, gnum;
#ifdef SIMPLE_MICS
    long delta_offset;
#endif
#if defined(HAVE_MMAP) || defined(MSWin32) || defined(MACH_SUBSTRATE)
    int  bnum;
#endif
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

#if defined(SIMPLE_MICS)
    if (lseek(ssfd, offset, 0) < 0)
	goto ss_err;

    if (read(ssfd, (char *)&delta_offset, sizeof(delta_offset)) < 0) goto ss_err;
    offset += delta_offset;
#endif

    /* Seek to amheader, get amheader, and seek back to amheader */
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
	caddr_t mem_start;
	size_t mem_len;
	long file_offset;
	mem_start = (caddr_t) round_down(hdr.blocks[bnum].start, pgsize);
	mem_len = hdr.blocks[bnum].fsize + ((size_t)hdr.blocks[bnum].start - (size_t)mem_start);
	file_offset = round_down(offset, pgsize);
#ifdef HAVE_MMAP_ZERO
	if (mmap(mem_start,
			    mem_len,
			    PROT_READ | PROT_WRITE,
			    MAP_FILE | MAP_PRIVATE | MAP_FIXED,
			    ssfd,
			    file_offset) != mem_start)
	    goto ss_err;
#else
	if (mmap(mem_start,
			    mem_len,
#if defined(arch_sparc)
			    PROT_EXEC |
#endif  /* arch_sparc */
			    PROT_READ | PROT_WRITE,
			    MAP_PRIVATE | MAP_FIXED,
			    ssfd,
			    file_offset) != mem_start)
	    goto ss_err;
#endif
	offset += hdr.blocks[bnum].fsize;

	/*
	 * If the size of the block in the file is not equal to the size
	 * which needs to be allocated, we need to allocate the remaining
	 * portion by mapping /dev/zero...
	 */

	if (hdr.blocks[bnum].asize != hdr.blocks[bnum].fsize) {
	    caddr_t np, zmem_start, block_end;
	    size_t zmem_len;
	    zmem_start = (caddr_t)round_up(mem_start + mem_len, pgsize);
	    block_end = (caddr_t)hdr.blocks[bnum].start + hdr.blocks[bnum].asize;
	    zmem_len = block_end - zmem_start;
	    if (zmem_start < block_end) {
#ifdef HAVE_MMAP_ZERO
	    	np = mmap(zmem_start,
	  			zmem_len,
				PROT_READ | PROT_WRITE,
				MAP_ANONYMOUS | MAP_PRIVATE | MAP_FIXED,
				-1,
				0);
#else
	    	int zfd;
	    	if ((zfd = open("/dev/zero", O_RDWR)) == -1)
		    goto ss_err;

	    	np = mmap(zmem_start, zmem_len, 
#if defined(arch_sparc)
				PROT_EXEC |
#endif  /* arch_sparc */
				PROT_READ | PROT_WRITE,
				MAP_PRIVATE | MAP_FIXED,
				zfd,
				0);

	    	close(zfd);			/* close /dev/zero */
#endif
	    	if (np != zmem_start)
		    goto ss_err;
	    }
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
#elif	defined(MSWin32)
    {
        HANDLE file;
        DWORD r;
        
	file = CreateFile(filename, GENERIC_READ, FILE_SHARE_READ, NULL,
                            OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, NULL);
	if (file == INVALID_HANDLE_VALUE) fatal_error(FE_SS_OPENERR,(long)filename);
	
	r = SetFilePointer(file, offset, NULL, FILE_BEGIN);
    	if (r == 0xFFFFFFFF) goto ss_err;

	for (bnum=0; bnum < hdr.nblocks; bnum++) {

	    if (VirtualAlloc(hdr.blocks[bnum].start, hdr.blocks[bnum].asize,
		MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE) == NULL) goto ss_err;

	    if (!ReadFile(file, hdr.blocks[bnum].start, hdr.blocks[bnum].fsize, &r, NULL)
    	    	|| r != hdr.blocks[bnum].fsize) goto ss_err;
    	}
    		
	CloseHandle(file);
    }
#if 0
/* currently broken, make it work someday. */
{
    HANDLE file, file_map;
    
    file = CreateFile(filename, GENERIC_READ | GENERIC_WRITE, 0 /*FILE_SHARE_READ*/, NULL,
                            OPEN_EXISTING, FILE_FLAG_RANDOM_ACCESS, NULL);
    if (file == INVALID_HANDLE_VALUE) fatal_error(FE_SS_OPENERR,(long)filename);
printf("created file\n");

    file_map = CreateFileMapping(file, NULL, PAGE_WRITECOPY, 0, hdr.blocks[bnum].asize, NULL);
    if (file_map == INVALID_HANDLE_VALUE) fatal_error(FE_SS_OPENERR,(long)filename);
printf("created file mapping\n");    
    for (bnum=0; bnum < hdr.nblocks; bnum++) {

	if (MapViewOfFileEx(file_map, FILE_MAP_COPY, 0, offset,
		hdr.blocks[bnum].fsize, hdr.blocks[bnum].start)
		 == NULL) goto ss_err;
printf("mapped view\n");
    }
}
#endif
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
#endif /* PURE_ANSI */

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
	for (gnum=0; gnum < amheader.nglobals; gnum++)
	    *amheader.globals[gnum].addr = amheader.globals[gnum].value;

	return 1;
    }
#else	/* MACH_SUBSTRATE */
    return 0;
#endif	/* MACH_SUBSTRATE */
}

#ifdef MacOS

static OSErr DuplicateThisApplication(ConstStr255Param newAppName)
{
    OSErr err;
    Str255 AppName;
    FSSpec AppSpec, NewAppSpec, DirSpec;
    
    if (MPW_Tool) {
    	char name[256];
    	
    	strcpy(name, imagedir);
    	strcat(name, imagename);
    	c2pstrcpy(AppName, name);
    	
    	err = FSMakeFSSpec(0, 0, AppName, &AppSpec);
    	if (err != noErr) return err;
    } else {
	ProcessSerialNumber PSN;
	ProcessInfoRec info;
	
	/* Get the FSSpec for this application. */    
	PSN.highLongOfPSN = 0;
	PSN.lowLongOfPSN = kCurrentProcess;
	
	info.processInfoLength = sizeof(ProcessInfoRec);
	info.processName = AppName;
	info.processAppSpec = &AppSpec;
	
	err = GetProcessInformation(&PSN, &info);
	if (err != noErr) return err;
    }

    /* Create a FSSpec for the new app and destination directory. */
    err = FSMakeFSSpec(0, 0, newAppName, &NewAppSpec);
    if (err != noErr && err != fnfErr) return err;
    
    if (err == noErr) {
    	err = FSpDelete(&NewAppSpec);
    	if (err != noErr) return err;
    }
    
    err = FSMakeFSSpec(NewAppSpec.vRefNum, NewAppSpec.parID, "\p", &DirSpec);
    if (err != noErr && err != fnfErr) return err;

    return FSpFileCopy(&AppSpec, &DirSpec, (StringPtr) newAppName, NULL, 0, 1);
}

int pbi_save_app_with_obp(void)
{
    PWord v1, v2, v3, v4, v5;
    int t1, t2, t3, t4, t5;
    UCHAR *name;
    Str255 newAppName, OBPName;
    FSSpec newAppSpec, OBPSpec;
    OSErr err;
    short resRef, obpRef;
    
    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);
    w_get_An(&v5, &t5, 5);

    if (!getstring(&name, v1, t1)) PI_FAIL;
    
    if (t2 != PI_LIST) PI_FAIL;
    
    //if (t3 != PI_LIST) PI_FAIL;
    
    c2pstrcpy(newAppName, name);
    
    err = DuplicateThisApplication(newAppName);
    if (err != noErr) PI_FAIL;
    
    err = FSMakeFSSpec(0, 0, newAppName, &newAppSpec);
    if (err != noErr && err != fnfErr) PI_FAIL;
    
    resRef = FSpOpenResFile(&newAppSpec, fsRdWrPerm);
    if (resRef == -1) PI_FAIL;

    while (t2 == PI_LIST) {
	PWord h; int th;
    	Handle obpHandle;
    	int newResource;
	long length, readLength;
    	
	PI_gethead(&h, &th, v2);
	if (!getstring(&name, h, th)) PI_FAIL;
	c2pstrcpy(OBPName, name);
	
	err = FSMakeFSSpec(0, 0, OBPName, &OBPSpec);
	if (err != noErr && err != fnfErr) PI_FAIL;
	
	err = FSpOpenDF(&OBPSpec, fsRdPerm, &obpRef);
	if (err != noErr) PI_FAIL;

	err = GetEOF(obpRef, &length);
	if (err != noErr) PI_FAIL;

	obpHandle = Get1NamedResource('OBPT', OBPSpec.name);
	if (obpHandle) { 
	    newResource = 0;
	    SetHandleSize(obpHandle, length);
	    if (MemError() != noErr) PI_FAIL;
	} else {
	    newResource = 1;
	    obpHandle = NewHandle(length);
	    if (obpHandle == NULL) PI_FAIL;
	}
		
	HLock(obpHandle);
	if (MemError() != noErr) PI_FAIL;
		
	readLength = length;
	err = FSRead(obpRef, &readLength, *obpHandle);
	if (err != noErr || readLength != length) PI_FAIL;
		
	FSClose(obpRef);
		
	HUnlock(obpHandle); 
	if (MemError() != noErr) PI_FAIL;
	
	if (newResource) {
	    AddResource(obpHandle, 'OBPT', Unique1ID('OBPT'), OBPSpec.name);
	    if (ResError() != noErr) PI_FAIL;
	} else {
	    ChangedResource(obpHandle);
	    if (ResError() != noErr) PI_FAIL;
	}
	WriteResource(obpHandle);
	if (ResError() != noErr) PI_FAIL;
	
	ReleaseResource(obpHandle);
	if (ResError() != noErr) PI_FAIL;
			
	PI_gettail(&v2, &t2, v2);
    }
    
    {
    Handle loadHandle;
    int newResource;
    PWord h; int th;
    long length, hlen;
    Str255 pname;
    
    loadHandle = Get1Resource('STR#', 128);
    if (loadHandle) {
    	newResource = 0;
    } else {
    	newResource = 1;
    	loadHandle = NewHandle(sizeof(short));
    	if (loadHandle == NULL) PI_FAIL;
    }
    **((short **)loadHandle) = 0;
    
    while (t3 == PI_LIST) {
    	PI_gethead(&h, &th, v3);
    	if (!getstring(&name, h, th)) PI_FAIL;
    	length = strlen(name);
    	hlen = GetHandleSize(loadHandle);
	(**((short **)loadHandle))++;
	SetHandleSize(loadHandle, hlen + length + 1);
	if (MemError() != noErr) PI_FAIL;
	c2pstrcpy(pname, name);
	BlockMove(pname, *loadHandle + hlen, length+1);
    	PI_gettail(&v3, &t3, v3);
    }
    
    if (newResource) {
	AddResource(loadHandle, 'STR#', 128, "\pAuto Load Files");
	if (ResError() != noErr) PI_FAIL;
    } else {
	ChangedResource(loadHandle);
	if (ResError() != noErr) PI_FAIL;
    }
    
    WriteResource(loadHandle);
    if (ResError() != noErr) PI_FAIL;
	
    ReleaseResource(loadHandle);
    if (ResError() != noErr) PI_FAIL;
    }
    
    {
	Handle initHandle;
	int newResource;
	long len;
	Str255 pname;
	
        if (!getstring(&name, v4, t4)) PI_FAIL;
	len = strlen(name);
	c2pstrcpy(pname, name);
	
	initHandle = Get1Resource('STR ', 128);
	if (initHandle) {
    	    newResource = 0;
    	    SetHandleSize(initHandle, len+1);
    	    if (MemError() != noErr) PI_FAIL;
	} else {
    	    newResource = 1;
    	    initHandle = NewHandle(len+1);
    	    if (initHandle == NULL) PI_FAIL;
	}
	
	BlockMove(pname, *initHandle, len+1);

	if (newResource) {
	    AddResource(initHandle, 'STR ', 128, "\pInit Predicate");
	    if (ResError() != noErr) PI_FAIL;
	} else {
	    ChangedResource(initHandle);
	    if (ResError() != noErr) PI_FAIL;
	}
	WriteResource(initHandle);
	if (ResError() != noErr) PI_FAIL;
	
	ReleaseResource(initHandle);
	if (ResError() != noErr) PI_FAIL;
    }
 
     {
	Handle startHandle;
	int newResource;
	long len;
	Str255 pname;
	
        if (!getstring(&name, v5, t5)) PI_FAIL;
	len = strlen(name);
	c2pstrcpy(pname, name);
	
	startHandle = Get1Resource('STR ', 129);
	if (startHandle) {
    	    newResource = 0;
    	    SetHandleSize(startHandle, len+1);
    	    if (MemError() != noErr) PI_FAIL;
	} else {
    	    newResource = 1;
    	    startHandle = NewHandle(len+1);
    	    if (startHandle == NULL) PI_FAIL;
	}
	
	BlockMove(pname, *startHandle, len+1);

	if (newResource) {
	    AddResource(startHandle, 'STR ', 129, "\pStart Predicate");
	    if (ResError() != noErr) PI_FAIL;
	} else {
	    ChangedResource(startHandle);
	    if (ResError() != noErr) PI_FAIL;
	}
	WriteResource(startHandle);
	if (ResError() != noErr) PI_FAIL;
	
	ReleaseResource(startHandle);
	if (ResError() != noErr) PI_FAIL;
    }
   
    
    CloseResFile(resRef);
    
    PI_SUCCEED;
}
#endif

void heap_overflow(void)
{
    if (wm_normal <= DEFAULT_SAFETY/8) fatal_error(FE_OVER_HEAP, 0);
    wm_normal = wm_normal/2;

    /* raise a prolog interupt - there must be a cleaner way! */

    if (wm_regidx != 0) {
	wm_safety = -1;
    }
    wm_interrupt_caught = ALSSIG_HEAP_OVERFLOW;
}
