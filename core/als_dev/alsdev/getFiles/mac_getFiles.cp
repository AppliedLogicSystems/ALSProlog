#include "getFiles.h"

#include "UGetMultipleFiles.h"

#include <string.h>

/* Protos from tclMac.h */
extern "C" {
pascal	OSErr	FSpGetDirectoryID(const FSSpec *spec, long *theDirID, Boolean *isDirectory);
int 	FSpLocationFromPath (int length, char *path, FSSpecPtr theSpec);
OSErr 	FSpPathFromLocation (FSSpecPtr theSpec, int *length, Handle *fullPath);
}

int GetFiles(Tcl_Interp *interp, Tcl_DString *initdir,
	Tk_Window parent, const char *title)
{
#pragma unused(parent)
#pragma unused(title)

	FSSpec dirSpec;
	long dirID;
	OSErr err;
	Boolean isDirectory;
	Tcl_Obj *result;

	if (initdir->length > 0) {
		if (FSpLocationFromPath(initdir->length, initdir->string, &dirSpec)
			!= noErr) {
			Tcl_AppendStringsToObj(Tcl_GetObjResult(interp),
				"bad directory \"", initdir->string, "\"", NULL);
			return TCL_ERROR;
		}

		err = FSpGetDirectoryID(&dirSpec, &dirID, &isDirectory);
		if ((err != noErr) || !isDirectory) {
			Tcl_AppendStringsToObj(Tcl_GetObjResult(interp),
				"bad directory \"", initdir->string, "\"", NULL);
			return TCL_ERROR;
		}
	}


	UGetMultipleFiles	*selectFiles = nil;

	selectFiles = new UGetMultipleFiles("\pSelect files to add...",
								(short) -1,		// number of types to show (-1 means all)
								(unsigned long *) nil,	// the list of types
								nil,			// the list of files to exclude
								&dirSpec,			// where to begin selection
								0);	// options flags

	result = Tcl_NewListObj(0, NULL);

	if ((selectFiles->GetSFReply()).sfGood) {
		LArray *files = selectFiles->GetFSSpecs();
		
		for (short i = 1; i <= files->GetCount(); i++) {
			FSSpec *spec = (FSSpec *)files->GetItemPtr(i);
			int length;
			Handle pathHandle = NULL;
			
			FSpPathFromLocation(spec, &length, &pathHandle);

			if (pathHandle != NULL) {
				HLock(pathHandle);
				Tcl_ListObjAppendElement(interp, result, Tcl_NewStringObj(*pathHandle, length));
				HUnlock(pathHandle);
				DisposeHandle(pathHandle);
			}
		}
	} else {
		//return empty string
	}

	Tcl_SetObjResult(interp, result);

	delete selectFiles;

    return TCL_OK;
}
