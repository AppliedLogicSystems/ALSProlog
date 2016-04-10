#include <Gestalt.h>
#include <ToolUtils.h>
#include <Fonts.h>
#include <Dialogs.h>
#include <SegLoad.h>
#include <Traps.h>

#include "tk.h"
//#include "tkInt.h"
//#include "tkMacInt.h"
//#include "tclMac.h"

extern void tcl_interface_init(void);

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

static int AEPackageInit(Tcl_Interp *interp)
{
	extern void TkMacInitAppleEvents(Tcl_Interp *interp);
	TkMacInitAppleEvents(interp);
	return TCL_OK;
}

typedef int (*Tcl_MacConvertEventPtr) (EventRecord *eventPtr);
extern void  Tcl_MacSetEventProc (Tcl_MacConvertEventPtr procPtr);
extern void 		TkMacInitMenus (Tcl_Interp 	*interp);
extern QDGlobalsPtr tcl_macQdPtr;
extern void panic(const char *);

void main(void)
{
	
	/* Initialize the Macintosh */
	if (MacintoshInit() != TCL_OK) {
		Tcl_Exit(1);
	}


	/* Initialize Tcl/Tk */
    tcl_macQdPtr = &qd;
    Tcl_MacSetEventProc(TkMacConvertEvent);

    Tcl_FindExecutable("foo");
    
    TkMacInitMenus(NULL);

	/* Install an Apple Events package */
	Tcl_StaticPackage(NULL, "appleevents", AEPackageInit, NULL);

	SetupALSProlog();
}


Boolean SIOUXIsAppWindow(WindowPtr);
Boolean SIOUXIsAppWindow(WindowPtr)
{
return 0;
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
WriteCharsToConsole(char */* buffer */, long n)
{
//    TkConsolePrint(gStdoutInterp, TCL_STDOUT, buffer, n);
    return n;
}

long 
ReadCharsFromConsole(char *, long)
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
SIOUXHandleOneEvent(EventRecord *)
{
    return 0;
}




#include "alspi.h"
#include "new_alspi.h"

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

static int enable_tcl_yield(void)
{
	PI_set_yield_proc(tcltk_yield);
	PI_SUCCEED;
}

PI_BEGIN
	PI_MODULE("builtins")
	PI_DEFINE("enable_tcl_yield",0,enable_tcl_yield)
PI_END

#ifdef DEMO
void setup_alsdev_demo(void);
void shutdown_alsdev_demo(void);
#endif

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

#ifdef DEMO
	setup_alsdev_demo();
#endif

    if ((exit_status = PI_startup(&setup)) != 0) return;

#ifdef DEMO
	shutdown_alsdev_demo();
#endif
    tcl_interface_init();

	PI_INIT;
{
	extern char executable_path[1024];
	extern long ss_image_offset(const char *image_name);

	if (!ss_image_offset(executable_path)) {
#if 0
		term = AP_NewInitStructure(w, AP_NewSymbolFromStr(w, "consult"),
					1, AP_NewSymbolFromStr(w, "blt_dvsh"));
			
	    AP_Call(w, AP_NewSymbolFromStr(w, "builtins"), &term);
#endif
	    panic("This is a stub!");
	}
}

//    term = AP_NewSymbolFromStr(w, "$start");
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


