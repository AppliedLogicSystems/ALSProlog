/*====================================================================*
 |		sig.h	
 |	Copyright (c) 1994-5, Applied Logic Systems, Inc.
 |
 |		-- Header file for modules which use signals in ALS-Prolog
 |
 | Creation: 3/25/94
 | Author: Kevin A. Buettner
 *====================================================================*/

#include <signal.h>
#include "missing.h"

#if defined(HAVE_UCONTEXT_H)
#include <ucontext.h>

extern	void	signal_handler	PARAMS(( int, siginfo_t *, ucontext_t *));

#elif defined(arch_m88k)
extern	void	signal_handler	PARAMS(( int, struct siginfo * ));

#elif defined(HAVE_SIGVEC)
extern	void  signal_handler	PARAMS(( int, int, struct sigcontext *, char * ));

#elif defined(HAVE_SIGVECTOR)
extern	void  signal_handler	PARAMS(( int, int, struct sigcontext *, char * ));

#elif defined(VMS)
extern	void  signal_handler	PARAMS(( char ));	/* !!? */

#else		/* otherwise */
extern	void  signal_handler	PARAMS(( int ));

#endif
