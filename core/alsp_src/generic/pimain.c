/*=====================================================================*
 |	pimain.c
 |	Copyright (c) 1988-1996, Applied Logic Systems, Inc.
 |
 |		-- default main() that initializes prolog and starts
 |			the development shell.
 |
 *=====================================================================*/

	/* In ALS-Prolog source tree */

#include "defs.h"
#include <limits.h>
#include <ctype.h>

#ifdef MacOS
#include <Errors.h>
#include <Processes.h>
#endif
#if defined(__MWERKS__) && defined(macintosh)
#include <console.h>
#include <SIOUX.h>
#endif

/* temp to fix getenv() problem */
#ifdef MSWin32
#include "fswin32.h"
#endif

static char *
isopt(const char *opt, char *str)
{
    size_t len = strlen(opt);
    if (strncmp(opt,str,len) == 0)
	return str+len;
    else
	return 0;
}

EXPORT ALSPI_API(int)	PI_main(int argc, char *argv[], void (*init)(void))
{
    int   exit_status, success;
    char *als_opts;
    PI_system_setup setup;

#ifdef MSWin32
	TCHAR old_title[MAX_PATH];
	BOOL got_title, got_input_mode;
	DWORD old_input_mode;
#endif

#if defined(KERNAL) && defined(__MWERKS__) && defined(macintosh)
    argc = 0; argv = NULL;
#endif

    /* Fill setup struct with defaults */
    setup.heap_size = 0;
    setup.stack_size = 0;
    setup.icbuf_size = 0;
    setup.alsdir = NULL;
    setup.saved_state = NULL;
    setup.load_executable_state = 1;
    setup.argc = argc;
    setup.argv = argv;
#ifdef WIN32
    setup.hInstance = NULL;
    setup.hPrevInstance = NULL;
    setup.lpCmdLine = NULL;
    setup.nCmdShow = 1;
#endif
    
    als_opts = getenv("ALS_OPTIONS");

    if (als_opts) {
	char *opt, *val;
	int i;
	
	als_opts = strdup(als_opts);
	if (als_opts == NULL)
	    fatal_error(FE_ALS_OPTIONS, 0);

	opt = strtok(als_opts, " ,");
	while (opt) {
	    if ( (val = isopt("heap_size:",opt)) ) {
	        i = atoi(val);
		if (i < 0 || i > (signed)(ULONG_MAX/256)) fatal_error(FE_ALS_OPTIONS, 0);
		setup.heap_size = i * 256;
	    } else if ( (val = isopt("stack_size:",opt)) ) {
	        i = atoi(val);
		if (i < 0 || i > (signed)(ULONG_MAX/256)) fatal_error(FE_ALS_OPTIONS, 0);
		setup.stack_size = i * 256;
	    } else if ( (val = isopt("saved_state:",opt)) ) {
		setup.saved_state = strdup(val);
		if (setup.saved_state == 0)
		    fatal_error(FE_ALS_OPTIONS, 0);
	    }
	    else if ( (val = isopt("icbuf_size:",opt)) )
		setup.icbuf_size = atoi(val) * 1024;
/*
	what to do about thi
	    else if ( (val = isopt("debug_shell", opt)) && *val == 0)
		noautoload = 1;
*/
	    else
		PI_app_printf(PI_app_printf_warning,
			      "unrecognized option: %s\n", opt);
	    opt = strtok(NULL, " ,");
	}

	free(als_opts);
    }

    /* Scan for -heap and -stack command line arguments. */ 
    {
    	int i;
    	unsigned long value;
	char *arg_end;
    	enum {arg_scan, heap_scan, stack_scan, finished} state;
    	
    	for (state = arg_scan, i = 0; state != finished && i < argc; i++) {
    	    switch (state) {
    	    case arg_scan:
    	    	if      (strcmp(argv[i], "-p") == 0)     state = finished;
    	    	else if (strcmp(argv[i], "-heap") == 0)  state = heap_scan;
    	    	else if (strcmp(argv[i], "-stack") == 0) state = stack_scan;
    	    	break;
    	    case heap_scan:
    	    	value = strtoul(argv[i], &arg_end, 10);
    	    	if ((*arg_end && !isspace((int)*arg_end))
		    || value == 0 || value > ULONG_MAX/256) {
    	    	    fprintf(stderr, "Usage: -heap N\n");
    	    	    exit(EXIT_ERROR);
    	    	} else {
    	    	    setup.heap_size = value * 256;
    	    	    state = arg_scan;
    	    	}
    	    	break;
    	    case stack_scan:
    	    	value = strtoul(argv[i], &arg_end, 10);
    	    	if ((*arg_end && !isspace((int)*arg_end))
		    || value == 0 || value > ULONG_MAX/256) {
    	    	    fprintf(stderr, "Usage: -stack N\n");
    	    	    exit(EXIT_ERROR);
    	    	} else {
    	    	    setup.stack_size = value * 256;
    	    	    state = arg_scan;
    	    	}
    	    	break;
    	    case finished:
    	    	break;
    	    }
    	}
    	
    	/* Check for incomplete scanning of arguments. */
    	switch (state) {
    	case heap_scan:
	    fprintf(stderr, "Usage: -heap N\n");
	    exit(EXIT_ERROR);
    	    break;
    	case stack_scan:
    	    fprintf(stderr, "Usage: -stack N\n");
    	    exit(EXIT_ERROR);
    	    break;
    	default:
    	    break;
    	}
    }

#ifdef MacOS
    {
    	OSErr err;
	ProcessSerialNumber PSN;
	ProcessInfoRec info;
	
	PSN.highLongOfPSN = 0;
	PSN.lowLongOfPSN = kCurrentProcess;
	
	info.processInfoLength = sizeof(ProcessInfoRec);
	info.processName = NULL;
	info.processAppSpec = NULL;

	err = GetProcessInformation(&PSN, &info);
	if (err != noErr) exit(EXIT_ERROR);
    
        /* Based on 7MB partition with 0x40000 word heap */
	setup.heap_size = setup.stack_size = info.processSize/28;
    }

    InstallConsole(0);
    SIOUXSetTitle("\pALS Prolog");
    
    PI_set_console_functions(standard_console_read, standard_console_write,
    				standard_console_error);
    
#endif

#ifdef MSWin32
	/* Set the console mode, in order to disable mouse input.
	
	   Mouse input is enabled by default. This causes problems when using
	   the Windows95 MS-DOS console in full screen mode.  At first no
	   mouse cursor is visible, but it unexpectedly appears after system/1
	   calls.
	  */
	got_input_mode = GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE),
		&old_input_mode);
	SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE),
		ENABLE_PROCESSED_INPUT | ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT);
    
	got_title = GetConsoleTitle(old_title, MAX_PATH);
    SetConsoleTitle("ALS Prolog");
#endif

    if ((exit_status = PI_startup(&setup)) != 0) {
	PI_app_printf(PI_app_printf_error, "Prolog init failed !\n");
	exit(EXIT_ERROR);
    }

    if (init) init();

    if ((exit_status = PI_toplevel(&success)) != 0) {
	PI_app_printf(PI_app_printf_error, "Prolog shell crashed !\n");
	exit(EXIT_ERROR);
    }

    PI_shutdown();

#ifdef MSWin32
	if (got_title) SetConsoleTitle(old_title);
	if (got_input_mode)
	SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), old_input_mode);
#endif
#if defined (MacOS) && defined(__MWERKS__)
    if (!MPW_Tool) printf("Exiting ALS Prolog.\n");
#endif
     
    switch (success) {
    case 0:
    	return EXIT_FAILURE;
    case 1:
    	return EXIT_SUCCESS;
    case 2:
    default:
    	return EXIT_ERROR;
	}
}

#if 1
int main(int argc, char *argv[])
{
    return PI_main(argc, argv, NULL);
}
#endif

#ifdef MacOS
int shlib_found;
FSSpec shlib_location;

pascal OSErr __initialize(const CFragInitBlock *theInitBlock);
pascal OSErr alspro_shlib_initialize(const CFragInitBlock *theInitBlock);
pascal OSErr alspro_shlib_initialize(const CFragInitBlock *theInitBlock)
{

    switch (theInitBlock->fragLocator.where) {
    case kDataForkCFragLocator:
    	shlib_location = *theInitBlock->fragLocator.u.onDisk.fileSpec;
    	shlib_found = 1;
    	break;
	case kResourceCFragLocator:
    	shlib_location = *theInitBlock->fragLocator.u.inSegs.fileSpec;
    	shlib_found = 1;
    	break;
    default:
    	shlib_found = 0;
    	break;
    }
	
#ifdef __MWERKS__
	return __initialize(theInitBlock);
#else
#error
#endif
}
#endif
