/*================================================================*
 |			alspi.h		
 |		Copyright (c) 1986-1995 Applied Logic Systems, Inc.
 |
 |			-- Prolog types & macros for the ALS-Prolog/C-Interface
 |
 | Author: Kevin A. Buettner
 | Creation: 11/7/86
 | 06/06/87 - K.Buettner -- Sun Interface
 | 06/08/88 - K.Buettner -- Changed names for consistancy
 | 06/13/88 - C.White -- Change filename to "alspi.h"
 | 11/09/88 - K.Buettner -- Motorola Interface
 *================================================================*/
#ifndef _ALSPI_H_INCLUDED_
#define _ALSPI_H_INCLUDED_

/*
 * Set up some macros for dealing with prototypes and other ANSI C features.
 */

#ifdef __cplusplus
extern "C" {
#endif

#ifndef PARAMS
#if defined(__STDC__) || defined(__cplusplus)
#define CONST const
#define PARAMS(arglist) arglist
#else
#define CONST
#define PARAMS(arglist) ()
#endif
#endif /* PARAMS */

#ifndef PWordTypeDefined
typedef long PWord;
#define PWordTypeDefined 1
#endif

#define PI_VAR 		0		/* unbound variables */
#define PI_LIST 	1
#define PI_STRUCT 	2
#define PI_SYM 		3
#define PI_INT 		4
#define PI_UIA 		5
#define PI_DOUBLE 	6

typedef struct {
		char *name;
		int  arity;
		int (*func) PARAMS((void));
		char *funcname;
} PSTRUCT;

#define PI_BEGIN static PSTRUCT pi_init_array[] = {
#define PI_DEFINE(p,a,f) {p,a,f,((char *) -1)},
#define PI_MODULE(m) {m,-1,((int (*)PARAMS((void))) 0),((char *) -1)},
#define PI_END {((char *) -1),-1,((int (*)PARAMS((void))) 0),((char *) -1)} };

#define PI_PDEFINE(p,a,f,fn) {p,a,f,fn},

#define PI_INIT PrologInit(pi_init_array)

extern char *WinsTypeStr;

#define X_WIN_STR   "xwins"
#define MOTIF_WIN_STR "motif"
#define OL_WIN_STR    "openlook"
#define DEC_WIN_STR   "decwins"
#define NEXT_WIN_STR  "nextstep"
#define MS_WIN_STR   "mswins"
#define MAC_WIN_STR   "macwins"
#define NO_WIN_STR    "nowins"

/*
 * Added 6/11/88 - chris
 */

#define PI_FAIL		return(0)
#define PI_SUCCEED	return(1)


/*
 * Informational, warning, and error messages
 *
 * PI_app_printf must be defined by the application.
 */

typedef enum {
    PI_app_printf_banner,		/* als startup banner */
    PI_app_printf_informational,	/* informational only */
    PI_app_printf_warning,		/* message is a warning */
    PI_app_printf_error,		/* message is an error */
    PI_app_printf_fatal_error		/* message is fatal error */
} PI_app_printf_flags;

/*
 * Declarations for foreign interface functions - 6/15/88 - chris
 */

extern	char *	PI_forceuia	PARAMS(( PWord *, int * ));
extern	void	PI_getan	PARAMS(( PWord *, int *, int ));
extern	void	PI_getargn	PARAMS(( PWord *, int *, PWord, int ));
extern	void	PI_gethead	PARAMS(( PWord *, int *, PWord ));
extern	void	PI_gettail	PARAMS(( PWord *, int *, PWord ));
extern	void	PI_getdouble	PARAMS(( double *, PWord ));
extern	void	PI_getstruct	PARAMS(( PWord *, int *, PWord ));
extern	char *	PI_getsymname	PARAMS(( char *, PWord, int ));
extern	char *	PI_getuianame	PARAMS(( char *, PWord, int ));
extern	void	PI_getuiasize	PARAMS(( PWord, int * ));
extern	void	PI_makedouble	PARAMS(( PWord *, int *, double ));
extern	void	PI_makelist	PARAMS(( PWord *, int * ));
extern	void	PI_makestruct	PARAMS(( PWord *, int *, PWord, int ));
extern	void	PI_makesym	PARAMS(( PWord *, int *, char * ));
extern	void	PI_makeuia	PARAMS(( PWord *, int *, char * ));
extern	void	PI_allocuia	PARAMS(( PWord *, int *, int ));
extern	int	PI_printf	PARAMS(( char *, ... ));
extern	int	PI_aprintf	PARAMS(( char *, char *, ... ));
extern	int	PI_rungoal	PARAMS(( PWord, PWord, int ));
extern	int	PI_rungoal_with_update	PARAMS(( PWord, PWord *, int * ));
extern	int	PI_unify	PARAMS(( PWord , int, PWord , int ));
extern	void	PrologInit	PARAMS(( PSTRUCT * ));
extern	void	PI_shutdown	PARAMS(( void ));
extern	int	PI_toplevel	PARAMS(( void ));
extern	int	PI_prolog_init	PARAMS(( char *, int, char ** ));
extern	void	PI_app_printf	PARAMS(( int, ... ));
extern	const char *	PI_get_options	PARAMS(( void ));

#ifdef MacOS
extern	long	yield_interval;
extern  long	yield_counter;
extern	void	PI_yield_time	PARAMS(( void ));
#endif

#ifdef __cplusplus
}
#endif

#endif /* _ALSPI_H_INCLUDED_ */
