#include <windows.h>

#include "getFiles.h"

#include <string.h>

/* Tk_GetHWND proto is from tkWin.h */
HWND Tk_GetHWND(Window window);

int GetFiles(Tcl_Interp *interp, Tcl_DString *initdir,
	Tk_Window parent, const char *title)
{

	OPENFILENAME of;
	Tcl_Obj *result;
	char file_list[MAX_PATH*100];
	int oldMode;

	of.lStructSize = sizeof(of);
	of.hwndOwner = parent ? Tk_GetHWND(Tk_WindowId(parent)) : GetTopWindow(NULL);
	of.lpstrFilter = NULL;
	of.lpstrCustomFilter = NULL;

	of.nFilterIndex = 0;
	file_list[0] = 0;
	of.lpstrFile = file_list;
	of.nMaxFile = MAX_PATH*100;
	of.lpstrFileTitle = NULL;
	of.lpstrInitialDir = Tcl_DStringValue(initdir);
	of.lpstrTitle = title ? title : "";;
	of.Flags = OFN_ALLOWMULTISELECT | OFN_EXPLORER;
	of.nFileOffset = 0;
	of.nFileExtension = 0;
	of.lpstrDefExt = NULL;
	
	result = Tcl_NewListObj(0, NULL);

	oldMode = Tcl_SetServiceMode(TCL_SERVICE_ALL);

	if (GetOpenFileName(&of)) {
		
		if (*(of.lpstrFile + strlen(of.lpstrFile)+1)) {
			char *prefix, *f, path[MAX_PATH];
			
			for (prefix = of.lpstrFile, f = prefix+of.nFileOffset; *f; f += strlen(f)+1) {
				strcpy(path, prefix);
				strcat(path, "\\");
				strcat(path, f);
				Tcl_ListObjAppendElement(interp, result, Tcl_NewStringObj(path, -1));
			}
		} else {
			Tcl_ListObjAppendElement(interp, result, Tcl_NewStringObj(of.lpstrFile, -1));
		}
	}
	

	Tcl_SetServiceMode(oldMode);

	Tcl_SetObjResult(interp, result);
	
	return TCL_OK;
}