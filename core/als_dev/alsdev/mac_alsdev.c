/* 
 * tkMacAppInit.c --
 *
 *	Provides a version of the Tcl_AppInit procedure for the example shell.
 *
 * Copyright (c) 1993-1994 Lockheed Missle & Space Company, AI Center
 * Copyright (c) 1995-1997 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tkMacAppInit.c 1.35 97/07/28 11:18:55
 */
 
#include <Gestalt.h>
#include <ToolUtils.h>
#include <Fonts.h>
#include <Dialogs.h>
#include <SegLoad.h>
#include <Traps.h>

#include "tk.h"
#include "tkInt.h"
#include "tkMacInt.h"
#include "tclMac.h"

#ifdef TK_TEST
EXTERN int		Tktest_Init _ANSI_ARGS_((Tcl_Interp *interp));
#endif /* TK_TEST */

#ifdef TCL_TEST
EXTERN int		TclObjTest_Init _ANSI_ARGS_((Tcl_Interp *interp));
EXTERN int		Tcltest_Init _ANSI_ARGS_((Tcl_Interp *interp));
#endif /* TCL_TEST */

Tcl_Interp *gStdoutInterp = NULL;

int 	TkMacConvertEvent _ANSI_ARGS_((EventRecord *eventPtr));

/*
 * Prototypes for functions the ANSI library needs to link against.
 */
short			InstallConsole _ANSI_ARGS_((short fd));
void			RemoveConsole _ANSI_ARGS_((void));
long			WriteCharsToConsole _ANSI_ARGS_((char *buff, long n));
long			ReadCharsFromConsole _ANSI_ARGS_((char *buff, long n));
extern char *		__ttyname _ANSI_ARGS_((long fildes));
short			SIOUXHandleOneEvent _ANSI_ARGS_((EventRecord *event));

/*
 * Prototypes for functions from the tkConsole.c file.
 */
 
EXTERN void		TkConsoleCreate _ANSI_ARGS_((void));
EXTERN int		TkConsoleInit _ANSI_ARGS_((Tcl_Interp *interp));
EXTERN void		TkConsolePrint _ANSI_ARGS_((Tcl_Interp *interp,
			    int devId, char *buffer, long size));
/*
 * Forward declarations for procedures defined later in this file:
 */

static int		MacintoshInit _ANSI_ARGS_((void));
static int		SetupMainInterp _ANSI_ARGS_((Tcl_Interp *interp));
void SetupALSProlog(void);

/*
 *----------------------------------------------------------------------
 *
 * main --
 *
 *	Main program for Wish.
 *
 * Results:
 *	None. This procedure never returns (it exits the process when
 *	it's done
 *
 * Side effects:
 *	This procedure initializes the wish world and then 
 *	calls Tk_Main.
 *
 *----------------------------------------------------------------------
 */


static int SetupConsole(Tcl_Interp *interp)
{
    TkConsoleCreate();
	
    if (Tcl_Init(interp) == TCL_ERROR) {
		return TCL_ERROR;
    }
    if (Tk_Init(interp) == TCL_ERROR) {
		return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Tk", Tk_Init, Tk_SafeInit);

	Tcl_SetVar(interp, "tcl_interactive", "1", TCL_GLOBAL_ONLY);
    
    TkMacInitMenus(interp);

	if (TkConsoleInit(interp) == TCL_ERROR) {
	    return TCL_ERROR;
	}

    gStdoutInterp = interp;
	
	return TCL_OK;
}

void
main(void)
{
	Tcl_Interp *interp;
	
	/* Initialize the Macintosh */
    if (MacintoshInit()  != TCL_OK) {
	Tcl_Exit(1);
    }


	/* Initialize Tcl */
    tcl_macQdPtr = &qd;
    Tcl_MacSetEventProc(TkMacConvertEvent);


    Tcl_FindExecutable("foo");
    
#if 0   
    interp = Tcl_CreateInterp();


    if (SetupConsole(interp) != TCL_OK) {
	TkpDisplayWarning(interp->result, "Application initialization failed");    
    }
#endif

	SetupALSProlog();
    //Tk_MainLoop();
    //Tcl_DeleteInterp(interp);
    //Tcl_Exit(0);
}


Boolean SIOUXIsAppWindow(WindowPtr w)
{
return 0;
}
/*
 *----------------------------------------------------------------------
 *
 * Tcl_AppInit --
 *
 *	This procedure performs application-specific initialization.
 *	Most applications, especially those that incorporate additional
 *	packages, will have their own version of this procedure.
 *
 * Results:
 *	Returns a standard Tcl completion code, and leaves an error
 *	message in interp->result if an error occurs.
 *
 * Side effects:
 *	Depends on the startup script.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_AppInit(
    Tcl_Interp *interp)		/* Interpreter for application. */
{
    if (Tcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    if (Tk_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Tk", Tk_Init, Tk_SafeInit);
	
    /*
     * Call the init procedures for included packages.  Each call should
     * look like this:
     *
     * if (Mod_Init(interp) == TCL_ERROR) {
     *     return TCL_ERROR;
     * }
     *
     * where "Mod" is the name of the module.
     */

#ifdef TCL_TEST
    if (Tcltest_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Tcltest", Tcltest_Init,
            (Tcl_PackageInitProc *) NULL);
    if (TclObjTest_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
#endif /* TCL_TEST */

#ifdef TK_TEST
    if (Tktest_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Tktest", Tktest_Init,
            (Tcl_PackageInitProc *) NULL);
#endif /* TK_TEST */

    /*
     * Call Tcl_CreateCommand for application-specific commands, if
     * they weren't already created by the init procedures called above.
     * Each call would look like this:
     *
     * Tcl_CreateCommand(interp, "tclName", CFuncCmd, NULL, NULL);
     */

    SetupMainInterp(interp);

    /*
     * Specify a user-specific startup script to invoke if the application
     * is run interactively.  On the Mac we can specifiy either a TEXT resource
     * which contains the script or the more UNIX like file location
     * may also used.  (I highly recommend using the resource method.)
     */

    Tcl_SetVar(interp, "tcl_rcRsrcName", "tclshrc", TCL_GLOBAL_ONLY);
    /* Tcl_SetVar(interp, "tcl_rcFileName", "~/.tclshrc", TCL_GLOBAL_ONLY); */

    SetupALSProlog();
    
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * MacintoshInit --
 *
 *	This procedure calls Mac specific initilization calls.  Most of
 *	these calls must be made as soon as possible in the startup
 *	process.
 *
 * Results:
 *	Returns TCL_OK if everything went fine.  If it didn't the 
 *	application should probably fail.
 *
 * Side effects:
 *	Inits the application.
 *
 *----------------------------------------------------------------------
 */

static int
MacintoshInit()
{
    int i;
    long result, mask = 0x0700; 		/* mask = system 7.x */

#if GENERATING68K && !GENERATINGCFM
    SetApplLimit(GetApplLimit() - (TK_MAC_68K_STACK_GROWTH));
#endif
    MaxApplZone();
    for (i = 0; i < 4; i++) {
	(void) MoreMasters();
    }

    /*
     * Tk needs us to set the qd pointer it uses.  This is needed
     * so Tk doesn't have to assume the availablity of the qd global
     * variable.  Which in turn allows Tk to be used in code resources.
     */
    tcl_macQdPtr = &qd;

    InitGraf(&tcl_macQdPtr->thePort);
    InitFonts();
    InitWindows();
    InitMenus();
    InitDialogs((long) NULL);		
    InitCursor();

    /*
     * Make sure we are running on system 7 or higher
     */
     
    if ((NGetTrapAddress(_Gestalt, ToolTrap) == 
    	    NGetTrapAddress(_Unimplemented, ToolTrap))
    	    || (((Gestalt(gestaltSystemVersion, &result) != noErr)
	    || (result < mask)))) {
	panic("Tcl/Tk requires System 7 or higher.");
    }

    /*
     * Make sure we have color quick draw 
     * (this means we can't run on 68000 macs)
     */
     
    if (((Gestalt(gestaltQuickdrawVersion, &result) != noErr)
	    || (result < gestalt32BitQD13))) {
	panic("Tk requires Color QuickDraw.");
    }

    
    FlushEvents(everyEvent, 0);
    SetEventMask(everyEvent);


    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * SetupMainInterp --
 *
 *	This procedure calls initalization routines require a Tcl 
 *	interp as an argument.  This call effectively makes the passed
 *	iterpreter the "main" interpreter for the application.
 *
 * Results:
 *	Returns TCL_OK if everything went fine.  If it didn't the 
 *	application should probably fail.
 *
 * Side effects:
 *	More initilization.
 *
 *----------------------------------------------------------------------
 */

static int
SetupMainInterp(
    Tcl_Interp *interp)
{
    /*
     * Initialize the console only if we are running as an interactive
     * application.
     */

    TkMacInitAppleEvents(interp);
    TkMacInitMenus(interp);

    if (strcmp(Tcl_GetVar(interp, "tcl_interactive", TCL_GLOBAL_ONLY), "1")
	    == 0) {
	if (TkConsoleInit(interp) == TCL_ERROR) {
	    goto error;
	}
    }

    /*
     * Attach the global interpreter to tk's expected global console
     */

    gStdoutInterp = interp;

    return TCL_OK;

error:
    panic(interp->result);
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * InstallConsole, RemoveConsole, etc. --
 *
 *	The following functions provide the UI for the console package.
 *	Users wishing to replace SIOUX with their own console package 
 *	need only provide the four functions below in a library.
 *
 * Results:
 *	See SIOUX documentation for details.
 *
 * Side effects:
 *	See SIOUX documentation for details.
 *
 *----------------------------------------------------------------------
 */

short 
InstallConsole(short fd)
{
#pragma unused (fd)

	return 0;
}

void 
RemoveConsole(void)
{
}

long 
WriteCharsToConsole(char *buffer, long n)
{
//    TkConsolePrint(gStdoutInterp, TCL_STDOUT, buffer, n);
    return n;
}

long 
ReadCharsFromConsole(char *buffer, long n)
{
    return 0;
}

extern char *
__ttyname(long fildes)
{
    static char *__devicename = "null device";

    if (fildes >= 0 && fildes <= 2) {
	return (__devicename);
    }
    
    return (0L);
}

short
SIOUXHandleOneEvent(EventRecord *event)
{
    return 0;
}




#include <alspi.h>
#include <new_alspi.h>

extern long standard_console_read(char *buf, long n);
extern long standard_console_write(char *buf, long n);
extern long standard_console_error(char *buf, long n);

static char *simple_write(AP_World *w, AP_Obj obj, char *s)
{
    switch (AP_ObjType(w, obj)) {
    case AP_VARIABLE:
    	s += sprintf(s, "V%lx", obj.p);
	break;
    case AP_LIST:
	{
		AP_Obj p = obj;

		s += sprintf(s, "[");
		
		while (AP_ObjType(w, p) == AP_LIST) {
			s = simple_write(w, AP_ListHead(w, p), s);
			p = AP_ListTail(w, p);
		}
		
		if (!AP_IsNullList(w, p)) {
			s += sprintf(s, "|");
			s = simple_write(w, p, s);
		}
		
		s += sprintf(s, "]");
	}	
	break;
    case AP_STRUCTURE:
    {
    	int arity, i;
    	
    	s += sprintf(s, "%s(", AP_GetAtomStr(w, AP_GetStructureFunctor(w, obj)));
    	
    	arity = AP_GetStructureArity(w, obj);
    	
    	i = 1;
    	while (1) {
    		s = simple_write(w, AP_GetArgument(w, obj, i), s);
    		i++;
    		if (i > arity) break;
    		s += sprintf(s, ",");
    	}
    	
    	s += sprintf(s, ")");
    }
	break;
    case AP_ATOM:
		s += sprintf(s, "%s", AP_GetAtomStr(w, obj));
	break;
    case AP_INTEGER:
    	s += sprintf(s, "%ld", AP_GetLong(w, obj));
	break;
    case AP_FLOAT:
    	s += sprintf(s, "%f", AP_GetDouble(w, obj));
	break;
    }
    
    return s;
}

static void tcltk_yield (void)
{
	Tcl_DoOneEvent(TCL_DONT_WAIT);
}

void SetupALSProlog(void)
{
    PI_system_setup setup;
    int exit_status;
	AP_World *w = NULL;
	AP_Obj term;

    /* Fill setup struct with defaults */
    setup.heap_size = 0;
    setup.stack_size = 0;
    setup.icbuf_size = 0;
    setup.alsdir = NULL;
    setup.saved_state = NULL;
    setup.load_executable_state = 1;
    setup.argc = 0;
    setup.argv = NULL;
    

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
	if (err != noErr) return;
    
        /* Based on 7MB partition with 0x40000 word heap */
	setup.heap_size = setup.stack_size = info.processSize/28;
    }


    PI_set_console_functions(standard_console_read, standard_console_write,
				standard_console_error);

    if ((exit_status = PI_startup(&setup)) != 0) return;

    pi_init();
{
int success;
//PI_status_toplevel(&success);
}

    term = AP_NewInitStructure(w, AP_NewSymbolFromStr(w, "consult"),
		1, AP_NewSymbolFromStr(w, "blt_dvsh"));
		
    AP_Call(w, AP_NewSymbolFromStr(w, "builtins"), &term);

	PI_set_yield_proc(tcltk_yield);

    term = AP_NewSymbolFromStr(w, "start_alsdev");
    {
    	AP_Result r;
    	r = AP_Call(w, AP_NewSymbolFromStr(w, "builtins"), &term);
    	if (r == AP_EXCEPTION) {
    		char s[1000];
    		simple_write(w, AP_GetException(w), s);
    		panic(s);
    	} 
	}
	
    PI_shutdown();
}


