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

#ifdef MacOS
#include <Errors.h>
#include <Processes.h>
#endif
#if defined(__MWERKS__) && defined(macintosh)
#include <console.h>
#include <SIOUX.h>
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

ALSPI_API(int)	PI_main(int argc, char *argv[], void (*init)(void))
{
    int   exit_status, success;
    char *als_opts;
    PI_system_setup setup;

#if defined(KERNAL) && defined(__MWERKS__) && defined(macintosh)
    argc = 0; argv = NULL;
#endif

    /* Fill setup struct with defaults */
    setup.heap_size = 0;
    setup.stack_size = 0;
    setup.icbuf_size = 0;
    setup.alsdir = NULL;
    setup.saved_state = NULL;
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
		if (i < 0 || i > ULONG_MAX/256) fatal_error(FE_ALS_OPTIONS, 0);
		setup.heap_size = i * 256;
	    } else if ( (val = isopt("stack_size:",opt)) ) {
	        i = atoi(val);
		if (i < 0 || i > ULONG_MAX/256) fatal_error(FE_ALS_OPTIONS, 0);
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
    	int i, r;
    	unsigned long value;
    	enum {arg_scan, heap_scan, stack_scan, finished} state;
    	
    	for (state = arg_scan, i = 0; state != finished && i < argc; i++) {
    	    switch (state) {
    	    case arg_scan:
    	    	if      (strcmp(argv[i], "-p") == 0)     state = finished;
    	    	else if (strcmp(argv[i], "-heap") == 0)  state = heap_scan;
    	    	else if (strcmp(argv[i], "-stack") == 0) state = stack_scan;
    	    	break;
    	    case heap_scan:
    	    	r = sscanf(argv[i], "%lu", &value);
    	    	if (r != 1 || value == 0 || value > ULONG_MAX/256) {
    	    	    fprintf(stderr, "Usage: -heap N\n");
    	    	    exit(EXIT_ERROR);
    	    	} else {
    	    	    setup.heap_size = value * 256;
    	    	    state = arg_scan;
    	    	}
    	    	break;
    	    case stack_scan:
    	    	r = sscanf(argv[i], "%lu", &value);
    	    	if (r != 1 || value == 0 || value > ULONG_MAX/256) {
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

#if defined (MacOS) && defined(__MWERKS__)
    if (!MPW_Tool) printf("Exiting ALS Prolog.\n");
#endif
       
    if (success) exit(EXIT_SUCCESS);
    else exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
    return PI_main(argc, argv, NULL);
}

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
