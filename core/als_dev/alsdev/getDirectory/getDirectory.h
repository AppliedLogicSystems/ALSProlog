#include "tcl.h"
#include "tk.h"

__declspec(dllexport) int  Getdirectory_Init(Tcl_Interp *interp);
int GetDirectory(Tcl_Interp *interp, Tcl_DString *initdir,
	Tk_Window parent, const char *prompt);
