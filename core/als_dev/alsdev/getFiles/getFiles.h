#ifdef macintosh
#define MAC_TCL
#endif

#include "tcl.h"
#include "tk.h"

#ifdef __cplusplus
extern "C" {
#endif

int  Getfiles_Init(Tcl_Interp *interp);
int GetFiles(Tcl_Interp *interp, Tcl_DString *initdir,
	Tk_Window parent, const char *prompt);

#ifdef __cplusplus
}
#endif
