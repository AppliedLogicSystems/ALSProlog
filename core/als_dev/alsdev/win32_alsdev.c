#include <alspi.h>
#include <new_alspi.h>

/* Includes needed to access C runtime __argc and __argv */
#if defined(__MWERKS__)
#include <crtl.h>
#elif defined(__GNUC__)
#include <stdlib.h>
#endif

#include <tcl.h>
#include <tk.h>

extern void tcl_interface_init(void);

//extern void panic(const char *);

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

/* 

Maintaining a Single Instance and Opening Documents from the Explorer

There is no standard way to do these things.  This code implements one
of the simplest method.

Single Instance

We only want one instance (process) of ALS Prolog running at a time.
To do this, we check for a mutex whenever this program is launched,
and exit if it exists.

Opening Documents

When a document is double-clicked in the Explorer or drag-and-dropped on
ALS Prolog, a new instance is created.  If another ALS Prolog is running,
a message (WM_COPYDATA) is sent to the other ALS Prolog to make it open
the document.

Problems

The current code relies on the main ALS Prolog shell window always being
open and always being named "ALS Prolog Environment".

Launching ALS Prolog via a multi-file open in the Explorer only opens one
document.

Future Directions

The Win32 Knowledge Base suggests using named objects to syncronize opening
multiple documents.

Tcl 8.1 has some support for DDE.  If possible we should use DDE for
communicating between instances of ALS Prolog.

*/

/* ActivateInstance activates another instance of the application and
   sends a copy-data message with the command line for processing.
 */

static void ActivateInstance(void)
{
	HWND w;
	
	w = FindWindow("TkTopLevel", "ALS Prolog Environment");
	if (w) {
		COPYDATASTRUCT data;
		
		if (IsIconic(w)) ShowWindow(w, SW_RESTORE);
		SetForegroundWindow (w);
		
		if (__argc == 2) {
			data.dwData = 0; 
	    	data.cbData = strlen(__argv[1])+1; 
	    	data.lpData = __argv[1]; 
	
			SendMessage(w, WM_COPYDATA, 0, (long)&data);
		}
	}
}

/* TkWindowProc and TkWindowInterp are Globals to hold Tk's window proc, and the Tcl interpreter. */

static WNDPROC TkWindowProc;
static Tcl_Interp *TkWindowInterp;

/* OpenDocWindowProc() is a window proc that catches and processes copy-data messages.
   All other messages are passed to Tk's window proc.
 */

static LRESULT CALLBACK OpenDocWindowProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	if (message == WM_COPYDATA) {
		Tcl_Obj *list, *elements[2];
		elements[0] = Tcl_NewStringObj("tkOpenDocument", -1);
		elements[1] = Tcl_NewStringObj(((COPYDATASTRUCT *)lParam)->lpData, -1);
		list = Tcl_NewListObj(2, elements);
		if (Tcl_EvalObj(TkWindowInterp, list)
			!= TCL_OK) Tcl_BackgroundError(TkWindowInterp);
		return TRUE;
	} else {
		return CallWindowProc((void *)TkWindowProc, hWnd, message, wParam, lParam);
	}
}

/* The AttachHandler command installs OpenDocWindowProc() as he window proc for the main
   ALS Prolog shell window.
 */

static int AttachHandlerObjCmd(ClientData c, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
#ifdef __MWERKS__
#pragma unused(c)
#endif

	HWND w;
	
	if (objc != 1) {
		Tcl_WrongNumArgs(interp, 1, objv, "window");
		return TCL_ERROR;
	}
	
	w = FindWindow("TkTopLevel", "ALS Prolog Environment");
	if (!w) return TCL_ERROR;
	
	TkWindowInterp = interp;
	TkWindowProc = (WNDPROC)GetWindowLong(w, GWL_WNDPROC);
	if (!TkWindowProc) return TCL_ERROR;
	
	if (!SetWindowLong(w, GWL_WNDPROC, (long)OpenDocWindowProc)) return TCL_ERROR;
			
	return TCL_OK;
}

/* Initialization routine for the OpenDocument package */

static int Opendocument_Init(Tcl_Interp *interp)
{
	if (!Tcl_PkgRequire(interp, "Tcl", "8.0", 0)
		|| !Tcl_PkgRequire(interp, "Tk", "8.0", 0)
		|| !Tcl_CreateObjCommand(interp, "AttachOpenDocumentHandler", AttachHandlerObjCmd, 0, NULL))
		return TCL_ERROR;
	return Tcl_PkgProvide(interp, "OpenDocument", "1.0");
}

    	extern long ss_image_offset(const char *image_name);

int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
	int   exit_status;
	HANDLE mutex;
	PI_system_setup setup;
	AP_Obj term;
	AP_World *w = NULL;
	
	Tcl_FindExecutable(NULL);
	
	/* Allow only one instance to run and have it process the command line. */
	mutex = CreateMutex(NULL, FALSE, "ALS Prolog Environment Mutex");
	if (!mutex) return FALSE;
	
	if (GetLastError() == ERROR_ALREADY_EXISTS) {
		ActivateInstance();
		return FALSE;
	}
	
	/* Make the OpenDocument package available to Tcl */
	Tcl_StaticPackage(NULL, "Opendocument", Opendocument_Init, NULL);
	
    /* Fill setup struct with defaults */
    setup.heap_size = 0;
    setup.stack_size = 0;
    setup.icbuf_size = 0;
    setup.alsdir = getenv("DEV_ALSDIR");
    setup.saved_state = NULL;
    setup.load_executable_state = 1;
    setup.argc = __argc;
    setup.argv = __argv;
    setup.hInstance = hInstance;
    setup.hPrevInstance = hPrevInstance;
    setup.lpCmdLine = lpCmdLine;
    setup.nCmdShow = nCmdShow;

    //PI_set_console_functions(standard_console_read, standard_console_write,
    //				standard_console_error);

	if ((exit_status = PI_startup(&setup)) != 0) {
		PI_app_printf(PI_app_printf_error, "Prolog init failed !\n");
		exit(EXIT_ERROR);
    }

	tcl_interface_init();

	/* Only load blt_dvsh when there is no saved state.
	   Since blt_dvsh is part of the state, reconsulting
	   does not work correctly. */
{
  //	extern char executable_path[1024];

	if (!ss_image_offset(executable_path)) {
#if 0
		term = AP_NewInitStructure(w, AP_NewSymbolFromStr(w, "consult"),
					1, AP_NewSymbolFromStr(w, "ldr_dvsh"));
			
	    AP_Call(w, AP_NewSymbolFromStr(w, "builtins"), &term);
#endif
//		MessageBox(GetFocus(), "This is just a stub!", "ALS Prolog", 0);

	}
}

    term = AP_NewSymbolFromStr(w, "$start");
    {
    	AP_Result r;
    	r = AP_Call(w, AP_NewSymbolFromStr(w, "builtins"), &term);
    	if (r == AP_EXCEPTION) {
    		char s[1000];
    		simple_write(w, AP_GetException(w), s);
    		MessageBox(GetFocus(), s, "ALS Prolog", 0);
    	} 
	}
	
    PI_shutdown();
    
    CloseHandle(mutex);
    
    return 0;
}

