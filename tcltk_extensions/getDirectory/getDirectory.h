#include "tcl.h"
#include "tk.h"

int GetDirectory(Tcl_Interp *interp, Tcl_DString *initdir,
	Tk_Window parent, const char *prompt);
