/*=====================================================================*
 |		winpimain.c
 |	Copyright (c) 1988-1995, Applied Logic Systems, Inc.
 |
 |		-- default WinMain() that initializes prolog and starts
 |			the development shell.
 |
 *=====================================================================*/

#ifdef HAVE_CONFIG_H
	/* In ALS-Prolog source tree */

#include "defs.h"

#else /* !HAVE_CONFIG_H */
	/* Not in ALS-Prolog source tree... */

#include <stdio.h>
#include <stdlib.h>

#include "alspi.h"

#endif /* !HAVE_CONFIG_H */


#include "pi_init.h"
#include "pi_cfg.h"


#include <windows.h>
#include <winsock.h>


static int ConsoleIO(int port, char *buf, size_t size)
{
    HANDLE f;
    int result;
    
    switch(port) {
    case CONSOLE_READ:
    	f = GetStdHandle(STD_INPUT_HANDLE);
    	if (ReadFile(f, buf, size, &result, NULL)) return result;
    	else return -1; 
    	break;
    case CONSOLE_WRITE:
    	f = GetStdHandle(STD_OUTPUT_HANDLE);
    	if (WriteFile(f, buf, size, &result, NULL)) return result;
    	else return -1;
    	break;
    case CONSOLE_ERROR:
    	f = GetStdHandle(STD_ERROR_HANDLE);
    	if (WriteFile(f, buf, size, &result, NULL)) return result;
    	else return -1;
    	break;
    }
    
}

#ifdef __MWERKS__
extern char **__argv;
extern int __argc;

void _SetupArgsFix();
#endif

extern HINSTANCE WinMain_Instance;
extern HINSTANCE WinMain_PrevInstance;
extern LPSTR WinMain_CmdLine;
extern int WinMain_CmdShow;

int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
    int   exit_status;

#ifdef __MWERKS__
    /* Parse the command line via patched version of MWCRTL's _SetupArgsFix.
       This will setup __argc and __argv. */ 
    _SetupArgsFix();
 #endif
  
    WinMain_Instance = hInstance;
    WinMain_PrevInstance = hPrevInstance;
    WinMain_CmdLine = lpCmdLine;
    WinMain_CmdShow = nCmdShow;
    
    if (!AllocConsole()) exit(EXIT_FAILURE);
    PI_set_console_callback(ConsoleIO);

    if ((exit_status = PI_prolog_init(__argc, __argv)) != 0) {
	PI_app_printf(PI_app_printf_error, "Prolog init failed !\n");
	exit(EXIT_FAILURE);
    }

    pi_init();

    if ((exit_status = PI_toplevel()) != 0) {
	PI_app_printf(PI_app_printf_error, "Prolog shell crashed !\n");
	exit(EXIT_FAILURE);
    }

    PI_shutdown();
    
    exit(EXIT_SUCCESS);
}
