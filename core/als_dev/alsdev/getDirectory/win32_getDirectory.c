#include "getDirectory.h"
#include <shlobj.h>

/* Tk_GetHWND proto is from tkWin.h */
HWND Tk_GetHWND(Window window);

int GetDirectory(Tcl_Interp *interp, Tcl_DString *initdir,
	Tk_Window parent, const char *prompt)
{
#pragma unused(initdir)

	BROWSEINFO bi;
	char name[MAX_PATH];
	LPITEMIDLIST idlist;
	int oldMode;
	
	bi.hwndOwner = parent ? Tk_GetHWND(Tk_WindowId(parent)) : GetTopWindow(NULL);
	bi.pidlRoot = NULL;
	bi.pszDisplayName = name;
	bi.lpszTitle = prompt ? prompt : "";
	bi.ulFlags = BIF_RETURNONLYFSDIRS;
	bi.lpfn = NULL;
	bi.lParam = 0;
	
	oldMode = Tcl_SetServiceMode(TCL_SERVICE_ALL);
	
	idlist = SHBrowseForFolder(&bi);
	
	Tcl_SetServiceMode(oldMode);
	
	if (idlist) {
		SHGetPathFromIDList(idlist, name);
		Tcl_SetObjResult(interp, Tcl_NewStringObj(name, -1));
	}
	
	return TCL_OK;
}