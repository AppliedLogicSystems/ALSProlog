/*
 * sig.h	-- Header file for modules which use signals in ALS-Prolog
 *	Copyright (c) 1994, Applied Logic Systems, Inc.
 *
 * Creation: 3/25/94
 * Author: Kevin A. Buettner
 */

#include <signal.h>
#include "missing.h"

#if defined(HAVE_UCONTEXT_H)
#include <ucontext.h>
#include <siginfo.h>
extern	void	signal_handler	PARAMS(( int, struct siginfo *, struct ucontext *));
#elif defined(arch_m88k)
extern	void	signal_handler	PARAMS(( int, struct siginfo * ));
#elif defined(HAVE_SIGVEC)
extern	void  signal_handler	PARAMS(( int, int, struct sigcontext *, char * ));
#elif defined(VMS)
extern	void  signal_handler	PARAMS(( char ));	/* !!? */
#else
extern	void  signal_handler	PARAMS(( int ));
#endif
