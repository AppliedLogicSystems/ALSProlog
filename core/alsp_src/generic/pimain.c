/*=====================================================================*
 |		pimain.c
 |	Copyright (c) 1988-1995, Applied Logic Systems, Inc.
 |
 |		-- default main() that initializes prolog and starts
 |			the development shell.
 |
 | 11/20/94 - C. Houpt -- Added Think/MetroWerks ccommand() call to allow
 |			      command line arguments for non-MPW versions.
 |			   -- Added include of pi_init header file to provide prototype.
 |			   -- Added PI_yield_time() to give other programs time.
 *=====================================================================*/

#ifdef HAVE_CONFIG_H
	/* In ALS-Prolog source tree */

#include "defs.h"

#else /* !HAVE_CONFIG_H */
	/* Not in ALS-Prolog source tree... */

#include "alspi.h"
#include <stdio.h>

#include <stdlib.h>

#ifndef NO_STDARG_H
#define HAVE_STDARG_H
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#endif /* !HAVE_CONFIG_H */

#ifdef __DJGPP__
#include <crt0.h>
/* int_crt0_startup_flags = _CRT0_FLAG_DROP_EXE_SUFFIX | _CRT0_FLAG_DROP_DRIVE_SUFFIX; */
#endif

extern	void	main	PARAMS(( int, char ** ));

#include "pi_init.h"
#include "pi_cfg.h"

#ifdef APP_PRINTF_CALLBACK
void app_printf(int messtype, va_list args);
#endif

#ifdef MacOS
#ifdef HAVE_GUSI
#include <GUSI.h>
#endif

#ifdef MPW_TOOL
#include <CursorCtl.h>

const int MPW_Tool = 1;

#else

const int MPW_Tool = 0;

#if THINK_C
#include <console.h>
#endif			/* THINK_C */
#ifdef __MWERKS__
#include <console.h>
#include <SIOUX.h>

#ifdef NO_SIOUX_MENU
tSIOUXSettings  SIOUXSettings =
        {0, 0, 0, 0, 0, 0, 4, 80, 24, 0, 0, 22, 12, 0};
#else
tSIOUXSettings  SIOUXSettings =
        {1, 1, 1, 0, 0, 0, 4, 80, 24, 0, 0, 22, 12, 0};
#endif

#endif			/* __MWERKS__ */
#endif			/* MPW_TOOL */

#include <Events.h>
#endif	/* MacOS */

#ifdef MSWin32
#include <winsock.h>

#if defined (WIN32)
	#define IS_WIN32 TRUE
#else
	#define IS_WIN32 FALSE
#endif
#define IS_NT      IS_WIN32 && (BOOL)(GetVersion() < 0x80000000)
#define IS_WIN32S  IS_WIN32 && (BOOL)(!(IS_NT) && ((GetVersion() & 0xFF)<4))
#define IS_WIN95 (BOOL)(!(IS_NT) && !(IS_WIN32S)) && IS_WIN32




#ifdef __MWERKS__
/* I'm not sure whose bug this is, but I suspect it's a MetroWerks problem.
   When a Console UI MSWin32 program is launched from Windows95, the program
   name on the simulated command line is enclosed in double-quotes because
   Win95 file name may have spaces in them.
   The argv list passed to main is not correctly parsed.  The double-quotes
   are not stripped and spaces inside the quotes are treated as argument
   seperators.
   For example, if you double click on the program "my cui app.exe", you will
   get the following results:
   
   Command Line:  "my cui app.exe"
   
   Incorrect Result:
   argc = 3
   argv = {"\"my", "cui", "app.exe\""}
   
   Correct result:
   argc = 1
   argv = {"my cui app.exe"}
   
   This simple FixArgument function only deals with double-quote stripping. 
*/

static void FixArguments(int argc, char **argv)
{
	int i, l;
	
	for (i = 0; i < argc; i++) {
		l = strlen(argv[i]);
		if (argv[i][0] == '"' && argv[i][l-1] == '"') {
			argv[i][l-1] = 0;
			memmove(argv[i], argv[i]+1, l-1);
		}
	}
}

#include <signal.h>

BOOL CtrlHandler(DWORD fdwCtrlType);
BOOL CtrlHandler(DWORD fdwCtrlType)
{
    switch (fdwCtrlType) {
    case CTRL_C_EVENT:    
	raise(SIGINT);
	return TRUE;
    case CTRL_BREAK_EVENT:
   	raise(SIGABRT);
	return TRUE;
    default:
    	return FALSE;
    }
}

#endif
#endif

void
main(int argc, char ** argv)
{
    int   exit_status;

#ifdef MacOS

#ifdef MPW_TOOL
    InitGraf((Ptr) &qd.thePort);
    InitCursorCtl(NULL);
#endif


#ifdef HAVE_GUSI
    GUSISetup(GUSIwithAppleTalkSockets);
    GUSISetup(GUSIwithInternetSockets);
    GUSISetup(GUSIwithPAPSockets);
    GUSISetup(GUSIwithPPCSockets);
    GUSISetup(GUSIwithUnixSockets);
#ifndef MPW_TOOL
    GUSISetup(GUSIwithSIOUXSockets);
#endif
#endif

#if (defined(THINK_C) || defined(__MWERKS__)) && !defined(MPW_TOOL)	
    argc = ccommand(&argv);
#endif

#endif /* MacOS */

#ifdef APP_PRINTF_CALLBACK 
    PI_set_app_printf_callback(app_printf);
#endif

#ifdef MSWin32
#ifdef __MWERKS__
    FixArguments(argc, argv);
    
    if (!(IS_WIN32S)) {
	if (!SetConsoleCtrlHandler((PHANDLER_ROUTINE)CtrlHandler, TRUE))
	    PI_app_printf(PI_app_printf_warning, "SetConsoleCtrlHandler failed !\n");
    }
#endif
    {
	WORD wVersionRequested = MAKEWORD(1, 1);
	WSADATA wsaData;
	int success = 1;
	
	success = (WSAStartup(wVersionRequested, &wsaData) == 0);
	
	if (success && wsaData.wVersion != wVersionRequested) {
	    WSACleanup();
	    success = 0;
	}

	if (!success) PI_app_printf(PI_app_printf_warning, "WinSock init failed !\n");
    }
    
#endif


    if ((exit_status = PI_prolog_init(WIN_STR, argc, argv)) != 0) {
	PI_app_printf(PI_app_printf_error, "Prolog init failed !\n");
	exit(1);
    }

#ifdef EXP_DATE
    if ((unsigned long) time(0) >= EXP_DATE) {
	PI_app_printf(PI_app_printf_error, "System validity date passed !\n");
	exit(1);
	}
#endif

    pi_init();

    if ((exit_status = PI_toplevel()) != 0) {
	PI_app_printf(PI_app_printf_error, "Prolog shell crashed !\n");
	exit(1);
    }

    PI_shutdown();
    
#ifdef MSWin32
    if (WSACleanup() != 0) {
	PI_app_printf(PI_app_printf_warning, "WinSock cleanup failed !\n");
    }
#endif
    
    
#if defined(__MWERKS__) && !defined(MPW_TOOL)
    printf("Exiting ALS Prolog.\n");
#endif

    exit(0);


}


/* PI_get_options returns a string containing option settings for ALSPro.
   Usually this string comes from getenv(), except on the Mac where it
   is stored in a preferences file.
*/
const char *PI_get_options(void)
{
#if defined(MacOS) && !defined(MPW_TOOL)
    /* This is a very simple version at the moment.  The preferences file
       must be in the same directory as the application.  This should be
       extended to also look in the preferences folder and system folder.
       The preferences file consists of a single line with the standard
       ALSPro options.
    */
    FILE *pref_file;
    static char pref_str[256];

    pref_file = fopen("ALSPro Prefs", "r");

    if (pref_file) {
	fgets(pref_str, 255, pref_file);
	fclose(pref_file);
	return pref_str;
    } else return NULL;
#else    
    return getenv("ALS_OPTIONS");
#endif
}

/*
 * PI_app_printf is called from the prolog environment to display error and
 * warning messages.  The first parameter, messtype, describes the type
 * of message.  These types are defined in alspi.h.  The message type may be
 * used to route the message supplied in va_alist to the place appropriate
 * for the application.
 *
 * Kev's note to ALS implementers:  
 *	We should be careful to only call PI_app_printf once for each
 *	particular message from Prolog.  Also, we should not make
 *	too many assumptions about what kind of device we are writing
 *	to.  In other words, line control information such as \n should
 *	probably be removed from most of our messages.  It will then
 *	be the responsiblity of PI_app_printf to output newlines or
 *	pop up windows or whatever.  It should also be the responsiblity
 *	of PI_app_printf to prepend information about the type of message.
 *	See the fatal error case (below) as an example.
 */


/*VARARGS0 */
#ifdef APP_PRINTF_CALLBACK
void app_printf(int messtype, va_list args)
#else
void
#ifdef HAVE_STDARG_H
PI_app_printf(int messtype, ...)
#else
PI_app_printf(messtype,va_alist)
    int messtype;
    va_dcl
#endif
#endif
{
#ifndef APP_PRINTF_CALLBACK
    va_list args;
#endif
    char *fmt;
#ifdef MSWin32
    FILE *f;
    char s[500];
#endif

#ifndef APP_PRINTF_CALLBACK
#ifdef HAVE_STDARG_H
    va_start(args, messtype);
#else
    va_start(args);
#endif
#endif /* APP_PRINTF_CALLBACK */

    fmt = va_arg(args, char *);

#ifdef MSWin32
    switch (messtype) {
	case PI_app_printf_banner :
	case PI_app_printf_informational :
	    f = stdout;
	    vsprintf(s, fmt, args);
	    break;
	case PI_app_printf_fatal_error :
	    f = stderr;
	    strcpy(s,"\nFatal Error: ");
	    vsprintf(s+strlen(s), fmt, args);
	    strcat(s,"\n");
	    break;
	case PI_app_printf_warning :
	case PI_app_printf_error :
	default :
	    f = stderr;
	    vsprintf(s, fmt, args);
	    break;
    }
    
    if (IS_WIN32S) {
	MessageBox(GetFocus(), s, "ALS Prolog", 0);
    } else {
    	fprintf(f, "%s", s);
    }
#else
    switch (messtype) {
	case PI_app_printf_banner :
	case PI_app_printf_informational :
	    vfprintf(stdout, fmt, args);
	    break;
	case PI_app_printf_fatal_error :
	    fprintf(stderr,"\nFatal Error: ");
	    vfprintf(stderr, fmt, args);
	    fprintf(stderr,"\n");
	    break;
	case PI_app_printf_warning :
	case PI_app_printf_error :
	default :
	    vfprintf(stderr, fmt, args);
	    break;
    }
#endif
}

#ifdef MacOS
/* PI_yield_time() is called periodically during the execution of prolog code to
   allow other processes to execute.  The MacOS uses cooperative multitasking, rather
   than preemptive.
   
   yield_interval is the number of prolog procedures executed between calls to
   PI_yield_time. yield_interval may be adjusted to provide more or fewer yields.
   
   yield_counter is used as a count-down until the next yield.
 */

long yield_interval = 100;
long yield_counter = 100;

static long last_yield = 0;

void	PI_yield_time(void)
{
    long tick;
    tick = TickCount();
#ifdef MPW_TOOL
    SpinCursor(1);
#endif
#if defined(__MWERKS__) && !defined(MPW_TOOL)
    SIOUXHandleOneEvent(NULL);
#endif
#ifdef THINK_C
    /* Do nothing for now. Think does not provide a handle event call for
       its console window. We could work around this, but it is not a priority.
    */
#endif

    /* Adjust the yield interval upwards until there are at least 3 ticks between
       yields.  */
    if (tick - last_yield <= 3) yield_interval += 100;
    last_yield = tick;
}

#if defined( __MWERKS__) && !defined(HAVE_GUSI)

#ifdef MPW_TOOL
#if __POWERPC__
#include <fcntl.h>
#else
#include <unix.h>
#endif
#else
#include <unix.h>
#endif

/* This a patch for metrowerk's open() function.  
   Hopefully this can be removed in a future release.
*/
int metrowerks_open_patch(const char *filename, int mode);
int metrowerks_open_patch(const char *filename, int mode)
{
#if defined(MPW_TOOL) && !__POWERPC__
  int mpw_mode;

/* MPW's open() mode values taken from {CIncludes}FCntl.h. */
#define MPW_O_RDONLY		 0 		/* Bits 0 and 1 are used internally */
#define MPW_O_WRONLY		 1 		/* Values 0..2 are historical */
#define MPW_O_RDWR 			 2		/* NOTE: it goes 0, 1, 2, *!* 8, 16, 32, ... */
#define MPW_O_APPEND	(1<< 3)		/* append (writes guaranteed at the end) */
#define MPW_O_RSRC 		(1<< 4)		/* Open the resource fork */
#define MPW_O_ALIAS		(1<< 5)		/* Open alias file */
#define MPW_O_CREAT		(1<< 8)		/* Open with file create */
#define MPW_O_TRUNC		(1<< 9)		/* Open with truncation */
#define MPW_O_EXCL 		(1<<10) 	/* w/ O_CREAT:  Exclusive "create-only" */
#define MPW_O_BINARY	(1<<11) 	/* Open as a binary stream */
#define MPW_O_NRESOLVE	(1<<14)		/* Don't resolve any aliases */

  switch(mode & 3) {
  case O_RDWR: mpw_mode = MPW_O_RDWR; break;
  case O_RDONLY: mpw_mode = MPW_O_RDONLY; break;
  case O_WRONLY: mpw_mode = MPW_O_WRONLY; break;
  };
  
  if (mode & O_APPEND) mpw_mode |= MPW_O_APPEND;
  if (mode & O_CREAT) mpw_mode |= MPW_O_CREAT;
  if (mode & O_EXCL) mpw_mode |= MPW_O_EXCL;
  if (mode & O_TRUNC) mpw_mode |= MPW_O_TRUNC;
  if (mode & O_BINARY) mpw_mode |= MPW_O_BINARY;
	
  mode = mpw_mode;
#endif

  return open(filename, mode);
}

#endif
#endif

