/*  Metrowerks Standard Library  Version 2.2  1997 October 17  */

/*
 *	abort_exit.c
 *	
 *		Copyright © 1995-1997 Metrowerks, Inc.
 *		All rights reserved.
 *	
 *	Routines
 *	--------
 *		abort
 *
 *		atexit
 *		exit
 *
 *		__atexit
 *		__exit
 *
 *		__setup_exit
 *	
 *
 */

#include "abort_exit.h"
#include "critical_regions.h"
#include "misc_io.h"
#include <signal.h>
#include <setjmp.h>
#include <stdlib.h>
#include <stdio.h>

#if macintosh && !defined(__dest_os)               /*MW-mm 960927a*/
  #define __dest_os __mac_os                       /*MW-mm 960927a*/
#endif                                             /*MW-mm 960927a*/

#if __dest_os == __undef_os
#error __dest_os undefined
#endif

#if __dest_os == __mac_os
#include <console.h>
#include <SegLoad.h>
#include <Processes.h>
#endif

#if __dest_os == __win32_os
#include <x86_prefix.h>
#include <Windows.h>
#endif

#define max_funcs	64

#if defined(__CFM68K__)
	#pragma import on
#endif

__extern_c
extern void __destroy_global_chain(void);
__end_extern_c

#if defined(__CFM68K__)
	#pragma import reset
#endif

static void (*atexit_funcs[max_funcs])(void);
static long	atexit_curr_func = 0;

static void (*__atexit_funcs[max_funcs])(void);
static long	__atexit_curr_func = 0;

void (* __stdio_exit)  (void) = 0;
void (* __console_exit)(void) = 0;

#if __dest_os == __be_os							/* ELR */

extern void (*__atexit_hook)(void);
extern void (*___atexit_hook)(void);

extern void ___teardown_be(int);

static short exitSetup = 0;

static void ANSI_Exit(void)
{
	exit(0);
}

static void ANSI__Exit(void)
{
	__exit(0);
}

void __setup_exit(void)
{
	if (!exitSetup)
	{
		__atexit_hook	 = ANSI_Exit;
		___atexit_hook = ANSI__Exit;

		exitSetup++;
	}
}

#endif

int	__aborting = 0;

void	abort(void)
{
	raise(SIGABRT);
	
	__aborting = 1;
	
	exit(EXIT_FAILURE);
}

int atexit(void (*func)(void))
{	

	if (atexit_curr_func == max_funcs)
		return(-1);

	__begin_critical_region(atexit_funcs_access);	/* 961218 KO */
	
	__setup_exit();
	
	atexit_funcs[atexit_curr_func++] = func;
	
	__end_critical_region(atexit_funcs_access);

	return(0);
}

int __atexit(void (*func)(void))
{	

	if (__atexit_curr_func == max_funcs)
		return(-1);

	__begin_critical_region(atexit_funcs_access);	/* 961218 KO */
	
	__setup_exit();
	
	__atexit_funcs[__atexit_curr_func++] = func;
	
	__end_critical_region(atexit_funcs_access);
	
	return(0);
}

#if __dest_os == __be_os							/* ELR */
void _libc_exit_(int status)
#else
void exit(int status)
#endif
{

		if (!__aborting)
	{
		__begin_critical_region(atexit_funcs_access);
		
		while (atexit_curr_func > 0)
			(*atexit_funcs[--atexit_curr_func])();
		
#if (__dest_os == __win32_os) && (STOP_PROGRAM_BEFORE_EXIT==1)

	if(GetFileType(GetStdHandle(STD_OUTPUT_HANDLE))==FILE_TYPE_CHAR)
{
	//printf("\n \n Press Enter to continue \n");
	//fflush(stdin);
	//getc(stdin);
}
#endif

		__end_critical_region(atexit_funcs_access);

	 /*
	 970218 bkoz
	 		need to move destroy global chain above __stdio_exit as
		 	some static objects may have destructors that flush streams
	 */
	 #if !__INTEL__
	 #if  __POWERPC__ || __CFM68K__ || (__MC68K__ && __A5__) || (__dest_os == __be_os)
		__destroy_global_chain();
	 #endif
	 #endif
		if (__stdio_exit)
		{
			(*__stdio_exit)();
			__stdio_exit = 0;
		}
	}

	__exit(status);
}


void __exit(int status)
{
#if __dest_os != __be_os
	#pragma unused(status)
#endif
	
	__begin_critical_region(atexit_funcs_access);

	while (__atexit_curr_func > 0)
		(*__atexit_funcs[--__atexit_curr_func])();
	
	__end_critical_region(atexit_funcs_access);
	
	__kill_critical_regions();
	
	if (__console_exit)
	{
		(*__console_exit)();
		__console_exit = 0;
	}

#if __dest_os == __mac_os

	ExitToShell();

#elif __dest_os == __be_os															/* ELR */

	___teardown_be(status);

#elif __dest_os == __win32_os		

	ExitProcess(status);													

#endif
}

#include "critical_regions.h"


#if __dest_os == __win32_os		

/* This function should be equivalent to the ANSI "exit" without the ExitProcess
 * call. This function is needed so that a DLL can clean up itself and return
 * to Windows which finishes cleaning up the process (other DLLs and the main
 * application might not have destructed yet).
 * I would have just separated this code out of exit but that code is already too
 * separated...
 */

void _CleanUpMSL()
{
	__begin_critical_region(atexit_funcs_access);
	
	while (atexit_curr_func > 0)
		(*atexit_funcs[--atexit_curr_func])();
	
	__end_critical_region(atexit_funcs_access);
	
	if (__stdio_exit)
	{
		(*__stdio_exit)();
		__stdio_exit = 0;
	}

	__destroy_global_chain();
	
	__kill_critical_regions();
	
	if (__console_exit)
	{
		(*__console_exit)();
		__console_exit = 0;
	}
}

#endif	/* __dest_os == win32 */

/*     Change record
 *	14-Sep-95 JFH  First code release.
 *	12-Oct-95 JFH  Added #include of <SegLoad.h> for ExitToShell() (in case
 *								 MacHeaders not included).
 *	31-Oct-95 JFH  Fixed exit() to longjmp(__program_exit,1) instead of call _exit on PPC
 *	15-Dec-95 JFH  Reworked abort/exit handling to conform to new runtime architecture.
 *	20-Dec-95 JFH  Renamed _atexit/_exit to __atexit/__exit for ANSI naming conformance
 *	27-Dec-95 JFH  Pulled guts out of __setup_exit for new runtime. Tossed __program_exit
 *								 and added __aborting for PPC and CFM68K projects.
 *	12-Feb-96 JFH  Tossed __setup_exit, which had become a NOP.
 *	 1-Mar-96 JFH  Merged Be code into source. For the moment that means: __setup_exit(),
 *								 it's baaack!
 *	26-Apr-96 JFH  Merged Win32 changes in.
 *						CTV
 * MW-mm 960927a   Made sure dest_os set for Macintosh
 *  18-Dec-96 KO   Moved the begin_critical_region call after the error check. Before, if
 *                 there was an error, the critical section would be entered and never left.
 *  19-Dec-96 KO   Added CleanUpMSL.
 *  18-feb-97 bkoz line 154 moved call of __destroy_global_chain() up to 
 *			       exit() before stdio closed
 *  21-Aug-97 mdf  added printf/scanf to exit for win32 to allow control of
 *                 console apps  	
 *  30-Sept-97 mdf wrapped immediately above change in GetFileType to prevent app from
 *				   stopping when output is redirected to a file.
 *                 Also add fflush so getc will have an empty stream
                   when called in the exit routine		   
*/

