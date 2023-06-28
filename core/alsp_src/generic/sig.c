/*=============================================================*
 |			sig.c        
 |		Copyright (c) 1991-1995 Applied Logic Systems, Inc.
 |
 |			-- signal handling for ALS-Prolog
 |
 | Authors:     Kevin Buettner, Ilyas Cicekli
 | Creation:    5/15/91 (out of main.c)
 | Revision History:
 | 06/10/93 - raman -- merged in I386 signal handling
 | 11/21/94 - C. Houpt -- Added prototypes for set_prolog_interrupt().
 |		Added #ifdef control for some header files (HAVE_SYS_TYPES_H, 
 |		HAVE_UNISTD_H); #ifdef'd out SIGALRM because it doesn't exist on Mac.
 *=============================================================*/
#include "defs.h"
#include "sig.h"

#include <stdio.h>

#ifdef DOS
#define SIGINT 2
#define CNTRLC_INTNUM	0x1B

#else  /* not-DOS */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <signal.h>
#include <math.h>

#endif /* DOS */

#ifdef MacOS
#ifdef HAVE_GUSI
#include <GUSI.h>
#else
#ifdef MPW_TOOL
#include <fcntl.h>
#else
#include <unix.h>
#endif
#endif
#endif

#ifdef DOS

/*
 * Control/C handler and structure to store original handler.
 */

extern int cntrlc_handler (void);
extern int endof_cntrlc_handler (void);
extern int set_prolog_interrupt (void);

struct intvec_struc {
    long  prot_vec_addr;
    short prot_vec_seg;
    long  real_vec_addr;
} original_cntrlc_handler;

static int cntrcl_init_flag = 0;

#endif /* DOS */

#ifdef VMS
#include <ssdef.h>
#include <descrip.h>
#endif

#ifndef DOS

/*
 * signal_handler is a generic handler which receives C signals and then
 * passes these on to ALS-Prolog.
 */

#if defined(HAVE_UCONTEXT_H)

#ifdef arch_sparc
extern set_prolog_interrupt();
unsigned long cntrl_c_resume;
#endif /* arch_sparc */
void
signal_handler(int signum, siginfo_t *siginf, ucontext_t *sigcon)
#elif defined(arch_m88k)
void
signal_handler(signum, siginf)
    int   signum;
    struct siginfo *siginf;
#elif defined(HAVE_SIGVEC) || defined(HAVE_SIGVECTOR)
extern set_prolog_interrupt(void);
int   cntrl_c_resume;
void  signal_handler(signum, code, scp, addr)
    int   signum, code;
    struct sigcontext *scp;
    char *addr;
#elif defined(VMS)
static unsigned short chan;
extern set_prolog_interrupt();
void  signal_handler(AST_param)
    char  AST_param;
#else
#define NAIVE_SIGNAL_HANDLER 1
extern int set_prolog_interrupt	(void);
void  signal_handler(signum)
    int   signum;
#endif 		/* defined(HAVE_UCONTEXT_H) */
{
#if 0
    //if (wm_regidx != 0)
#endif
    if (current_engine.reg_stack_top != current_engine.reg_stack_base)
    {
#if !defined(arch_i386) && !defined(Portable)
	if (wm_in_Prolog) {
#ifdef arch_m88k
#ifdef HAVE_UCONTEXT_H
	    sigcon->uc_mcontext.gregs[R_R22] = -1;
	    sigcon->uc_checksum.ucs_flags = 0;
	    sigcon->uc_checksum.ucs_checksum = 0;
#else  
	    siginf->si_r22 = -1;	/* r22 is the Safety register on 88k */
#endif /* HAVE_UCONTEXT_H */
#endif /* arch_m88k */

#ifdef	VMS
	    set_prolog_interrupt();
#endif /* VMS */

#ifdef	arch_sparc
#ifdef	HAVE_UCONTEXT_H
#define SC_PC (unsigned long) (sigcon->uc_mcontext.gregs[REG_PC])
#define SC_NPC (unsigned long) (sigcon->uc_mcontext.gregs[REG_nPC])
#else	/* SunOS ; not-HAVE_UCONTEXT_H */
#define SC_PC (unsigned long) scp->sc_pc
#define SC_NPC (unsigned long) scp->sc_npc
#endif  /* HAVE_UCONTEXT_H */
	    if ((((unsigned long) set_prolog_interrupt) <= SC_PC &&
		 SC_PC <= ((int) set_prolog_interrupt) + 24) ||
		(((int) set_prolog_interrupt) <= SC_NPC &&
		 SC_NPC <= ((unsigned long) set_prolog_interrupt) + 24)) {
		/* do nothing */
	    }
	    else if (SC_NPC == SC_PC + 4) {
		cntrl_c_resume = SC_PC;
		SC_PC = (unsigned long) set_prolog_interrupt;
		SC_NPC = ((unsigned long) set_prolog_interrupt) + 4;
	    }
	    else {
		cntrl_c_resume = SC_NPC;
		SC_NPC = (unsigned long) set_prolog_interrupt;
	    }
#endif	/* arch_sparc */

#if defined(arch_m68k) && defined(HAVE_SIGVEC)
	    cntrl_c_resume = scp->sc_pc;
	    scp->sc_pc = (int) set_prolog_interrupt;
#endif /* defined(arch_m68k) && defined(HAVE_SIGVEC) */


#ifdef	NAIVE_SIGNAL_HANDLER
	    set_prolog_interrupt();	/* Set Safety register */
#endif /* NAIVE_SIGNAL_HANDLER */

	}
#endif /* !arch_i386 && !Portable */
	wm_safety = -1;
    }

#ifdef VMS
    SYS$QIOW(0, chan, 0x123, 0, 0, 0, signal_handler, 0, 0, 0, 0, 0);
    wm_interrupt_caught = SIGINT;
#else  /* not DOS or VMS */

    wm_interrupt_caught = signum;

    if (signum != ALSSIG_STACK_OVERFLOW)
#if defined(HAVE_SIGACTION) && defined(SA_SIGINFO)
    {
	struct sigaction act;

	act.sa_handler = signal_handler;
	act.sa_flags = SA_SIGINFO;
	(void) sigaction(signum, &act, 0);
    }
#elif defined(HAVE_SIGVEC) || defined(HAVE_SIGVECTOR)
    {
	struct sigvec v;

	v.sv_handler = signal_handler;
	v.sv_mask = 0;
#ifdef HAVE_SIGVECTOR
	v.sv_flags = SV_BSDSIG;	/* do not restart certain system calls */
	sigvector(signum, &v, 0);
#else
	v.sv_flags = SV_INTERRUPT;	/* do not restart certain system calls */
	sigvec(signum, &v, 0);
#endif
    }
#else 
	(void) signal(signum, signal_handler);
#endif 
#endif /* VMS */

#if defined(HAVE_UCONTEXT_H)
    setcontext(sigcon);
#endif
}

#endif /* DOS */


void
reissue_cntrlc()
{
#if 0
    //if (wm_regidx != 0)
#endif
    if (current_engine.reg_stack_top != current_engine.reg_stack_base)
    {
	wm_safety = -1;
	wm_interrupt_caught = ALSSIG_REISS_CNTRLC;
    }
}

/* signal_name_list[] is an array of paired signal numbers and their
   prolog names. Used to initilize the signal_name table. */

static struct {int signal; const char *name;} signal_name_list[] = {
/* ANSI C Signals */
    {SIGABRT, "sigabrt"},
    {SIGFPE, "sigfpe"},
    {SIGILL, "sigill"},
    {SIGINT, "sigint"},
    {SIGSEGV, "sigsegv"},
    {SIGTERM, "sigterm"},
    
/* Other unix signals - The following is a reasonable default set
   based on SunOS, but there are other signals in different
   flavors of unix. */
#ifdef UNIX
    {SIGHUP, "sighup"},
    {SIGQUIT, "sigquit"},
    {SIGTRAP, "sigtrap"},
#ifdef SIGEMT
    {SIGEMT, "sigemt"},
#endif
    {SIGKILL, "sigkill"},
    {SIGBUS, "sigbus"},
#ifdef SIGSYS
    {SIGSYS, "sigsys"},
#endif
    {SIGPIPE, "sigpipe"},
    {SIGALRM, "sigalrm"},
    {SIGURG, "sigurg"},
    {SIGSTOP, "sigstop"},
    {SIGTSTP, "sigtstp"},
    {SIGCONT, "sigcont"},
    {SIGCHLD, "sigchld"},
    {SIGTTIN, "sigttin"},
    {SIGTTOU, "sigttou"},
    {SIGIO, "sigio"},
#ifdef SIGXCPU
    {SIGXCPU, "sigxcpu"},
#endif
#ifdef SIGXFSZ
    {SIGXFSZ, "sigxfsz"},
#endif
    {SIGVTALRM, "sigvtalrm"},
    {SIGPROF, "sigprof"},
    {SIGWINCH, "sigwinch"},
#ifdef SIGLOST
    {SIGLOST, "siglost"},
#endif
    {SIGUSR1, "sigusr1"},
    {SIGUSR2, "sigusr2"},    
#endif /* unix */

/* ALS signals */
    {ALSSIG_REISS_CNTRLC, "reisscntrl_c"},
    {ALSSIG_STACK_OVERFLOW, "stack_overflow"},
    {ALSSIG_LIBLOAD, "libload"},
    {ALSSIG_HEAP_OVERFLOW, "heap_overflow"},
    {ALSSIG_ERROR, "prolog_error"},
    {ALSSIG_UNDEFINED_PRED, "undefined_predicate"},
    {ALSSIG_ALARM, "sigalrm"},

/* List end */
    {0, ""}
};

/* signal_name[] is an array that maps from signal numbers
   to their prolog name.  Initilized in init_signal_name
   and used in pbi_signal_name. */
static const char *signal_name[MAX_ALS_SIG];

/* Initilize the signal_name array. */
static void init_signal_names(void)
{
    int i;
    
    /* Set all names to NULL, so that un-named symbols are detected. */
    for (i = 0; i < MAX_ALS_SIG; i++) signal_name[i] = NULL;
    
    /* Load signal_name[] from signal_name_list[]. */
    for (i = 0; signal_name_list[i].signal; i++) {
	signal_name[signal_name_list[i].signal] = signal_name_list[i].name;
    }
}

/* pbi_signal_name/2 
   pbi_signal_name(SigNum, SigName)
   pbi_signal_name(-, +)
   
   Returns the prolog name for the signal number SigNum in SigName.
   Fails if there is no coresponding name for the signal number.
*/

int pbi_signal_name(void)
{
    PWord v1, v2, s;
    int   t1, t2, st;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    
    if (t1 != PI_INT) PI_FAIL;
    
    if (v1 >= 0 && v1 < MAX_ALS_SIG && signal_name[v1]) {
	PI_makesym(&s, &st, signal_name[v1]);
        if (PI_unify(v2, t2, s, st)) PI_SUCCEED;
	else PI_FAIL;
    } else PI_FAIL;
}

void
init_sigint()
{
    /*
     * Initilize the signal name table.
     */
    init_signal_names();

    /*
     * Establish the control C handler
     */
#ifdef DOS
    /*
     * Lock functions set_prolog_interrupt and cntrlc_handler, and
     * variables wm_regidx, wm_safety and wm_interrupt_caught
     * (they are used by those two functions) in the memory so that
     * when an interrupt occurs the interrupt handler can be in the memory
     * (in virtual memory environment).
     */
    if (vm_subsystem_present()) {
	if ((lock_or_unlock_page(1, (char *) set_prolog_interrupt,
				 (long) ((char *) endof_cntrlc_handler -
					 (char *) set_prolog_interrupt),
				 1) != 0) ||
	    (lock_or_unlock_page(1, (char *) &wm_regidx, sizeof (long), 0) != 0) ||
	    (lock_or_unlock_page(1, (char *) &wm_safety, sizeof (long), 0) != 0) ||
	    (lock_or_unlock_page(1, (char *) &wm_interrupt_caught,
				 sizeof (long), 0) != 0)) {
#ifdef PharLap
	    /* Print warning message only in PharLap environment.
	     * (Since I couldn't find out how to check
	     * virtual memory subsystem is present or not in Ergo environment,
	     * the function "vm_subsystem_present" always return 1.
	     * So, the virtual memory subsystem may not be present and
	     * it is meaningless to lock a page in that case.)
	     */
	    fprintf(stderr,
		    "\nWarning: Unable to lock CNTRL/Break Interrupt Handler in Memory.");
#endif /* PharLap */
	}
    }

    /*
     * save segment selectors for future used
     * (they are used by interrupt handlers.
     */
    save_seg_selectors();

    /*
     * Initialize Control C(Break) Interrupt handler
     */
    init_int_handler(CNTRLC_INTNUM, cntrlc_handler,
		     &(original_cntrlc_handler.prot_vec_addr));

    cntrcl_init_flag = 1;

#else  /* not DOS */
#ifdef VMS
    {
	static $DESCRIPTOR(terminal, "SYS$COMMAND");
	register status;

	if (((status = SYS$ASSIGN(&terminal, &chan, 0, 0)) & 1) != 1)
	    LIB$STOP(status);

	if (((status = SYS$QIOW(0, chan, 0x123, 0, 0, 0,
				signal_handler, 0, 0, 0, 0, 0)) & 1) != 1)
	    LIB$STOP(status);
    }
#else  /* not DOS or VMS */
#if defined(HAVE_SIGACTION) && defined(SA_SIGINFO)
    {
	struct sigaction act;

	act.sa_handler = signal_handler;
	act.sa_flags = SA_SIGINFO;
	(void) sigaction(SIGINT, &act, 0);
    }
#else
#if defined(HAVE_SIGVEC) || defined(HAVE_SIGVECTOR)
    {
	struct sigvec v;

	v.sv_handler = signal_handler;
	v.sv_mask = 0;
#ifdef HAVE_SIGVECTOR
	v.sv_flags = SV_BSDSIG;	/* do not restart certain system calls */
	sigvector(SIGINT, &v, 0);
#else
	v.sv_flags = SV_INTERRUPT;	/* do not restart certain system calls */
	sigvec(SIGINT, &v, 0);
#endif
    }
#else
    (void) signal(SIGINT, signal_handler);
#endif /* HAVE_SIGVEC */
#endif /* HAVE_SIGACTION...*/
#endif /* VMS */
#endif /* DOS */
}


void
reset_sigint()
{
#ifdef DOS
    if (cntrcl_init_flag == 1) {
	reset_int_handler(CNTRLC_INTNUM, &(original_cntrlc_handler.prot_vec_addr));

	/*
	 * Unlock functions signal_handler, and
	 * variables wm_regidx, wm_safety, wm_interrupt_caught.
	 */
	if (vm_subsystem_present()) {
	    lock_or_unlock_page(0, (char *) set_prolog_interrupt,
				(long) ((char *) endof_cntrlc_handler -
					(char *) set_prolog_interrupt), 1);
	    lock_or_unlock_page(0, (char *) &wm_regidx, sizeof (long), 0);
	    lock_or_unlock_page(0, (char *) &wm_safety, sizeof (long), 0);
	    lock_or_unlock_page(0, (char *) &wm_interrupt_caught, sizeof (long), 0);
	}
    }
#endif /* DOS */

    return;
}

int pbi_alarm(void)
{
    PWord v1, v2;
    int   t1, t2;
    double dval, dinterval;

    PI_getan(&v1, &t1, 1);
    PI_getan(&v2, &t2, 2);

    if (t1 == PI_INT)
	dval = (double) v1;
    else if (t1 == PI_DOUBLE)
	PI_getdouble(&dval, v1);
    else
	PI_FAIL;


    if (t2 == PI_INT)
	dinterval = (double) v2;
    else if (t2 == PI_DOUBLE)
	PI_getdouble(&dinterval, v2);
    else
	PI_FAIL;

    os_set_timer(dval, dinterval);

    PI_SUCCEED;
}

#ifndef MSWin32
#if defined(SIGCHLD) || defined (SIGCLD)

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#ifndef SIGCHLD
#define SIGCHLD SIGCLD
#endif

static	void	burychild	( int );

static void
burychild(signo)
    int signo;
{
#if defined(HAVE_WAITPID) && defined(WNOHANG)
    /* waitpid is preferable */
    while (waitpid(-1, 0, WNOHANG) > 0)
	;
#elif defined(HAVE_WAIT3) && defined(WNOHANG)
    /* but wait3 is ok */
    while (wait3(0, WNOHANG, 0) > 0)
	;
#else
    /* wait really sucks */
    wait(0);
#endif
    deathwatch();
}

void
deathwatch()
{
    /* FIXME: Reliable signals */
    (void) signal(SIGCHLD, burychild);
}

#endif /* SIGCHLD or SIGCLD */
#endif
