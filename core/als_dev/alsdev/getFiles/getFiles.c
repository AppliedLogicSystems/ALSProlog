/* 
 *  getFiles
 *
 *	This file is the platform independed portion of the getFiles
 *  command for Tcl/Tk.
 *
 * Copyright (c) 1998 by Applied Logic Systems, Inc.
 *
 */

#include "getFiles.h"
#include "getFiles_version.h"

static int
getFiles_ObjCmd(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
#pragma unused(clientData)

	enum {INITIAL_DIR, PARENT, PROMPT};
	char *options[] = {"-initialdir", "-parent", "-prompt", NULL};
	char *ophelp[] = {"a directory path", "a window name", "a prompt string"};
	int index, i, result;
	Tcl_DString initdir;
	Tk_Window parent = NULL;
	char *prompt = NULL;
	
	Tcl_DStringInit(&initdir);

	for (i = 1; i < objc; i+=2) {
		if (Tcl_GetIndexFromObj(interp, objv[1], options, "option", 0, &index)
			!= TCL_OK) {result = TCL_ERROR; goto exit;}
		
		if (i+1 >= objc) {
			Tcl_AppendStringsToObj(Tcl_GetObjResult(interp),"\"", options[index],
				"\" option must be followed by ", ophelp[index], NULL);
			{result = TCL_ERROR; goto exit;}
		}
		
		switch (index) {
		case INITIAL_DIR:
		    if (Tcl_TranslateFileName(interp, Tcl_GetStringFromObj(objv[i+1], NULL),
		    	&initdir) == NULL) {
		        {result = TCL_ERROR; goto exit;}
		    }
			break;
		case PARENT:
		    parent = Tk_NameToWindow(interp, Tcl_GetStringFromObj(objv[i+1], NULL),
		    	Tk_MainWindow(interp));
		    if (parent == NULL) {result = TCL_ERROR; goto exit;}
			break;
		case PROMPT:
			prompt = Tcl_GetStringFromObj(objv[i+1], NULL);
			if (prompt == NULL) {result = TCL_ERROR; goto exit;}
			break;
		}
	}
	
	result = GetFiles(interp, &initdir, parent, prompt);

exit:
	Tcl_DStringFree(&initdir);
	
	return result;
}


int Getfiles_Init(Tcl_Interp *interp)
{
	if (!Tcl_PkgRequire(interp, "Tcl", TCL_VERSION, 0)
		|| !Tcl_PkgRequire(interp, "Tk", TK_VERSION, 0))
		return TCL_ERROR;
	
    if (!Tcl_CreateObjCommand(interp, "getFiles", getFiles_ObjCmd, NULL, NULL))
    	return TCL_ERROR;
    
    return Tcl_PkgProvide(interp, "getFiles", VERSION_STRING);
}


