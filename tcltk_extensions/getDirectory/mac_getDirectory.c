#include "getDirectory.h"

#include <string.h>
#include "StandardGetFolder.h"
//#include <MoreFilesExtras.h>
#include <LowMem.h>

/* Protos from tclMac.h */
pascal	OSErr	FSpGetDirectoryID(const FSSpec *spec, long *theDirID, Boolean *isDirectory);
int 	FSpLocationFromPath (int length, char *path, FSSpecPtr theSpec);
OSErr 	FSpPathFromLocation (FSSpecPtr theSpec, int *length, Handle *fullPath);

static pascal Boolean OnlyVisibleFoldersCustomFileFilter(CInfoPBPtr myCInfoPBPtr, Ptr dataPtr)
{
#pragma unused (dataPtr)

	// return true if this item is invisible or a file

	Boolean visibleFlag;
	Boolean folderFlag;
	
	visibleFlag = ! (myCInfoPBPtr->hFileInfo.ioFlFndrInfo.fdFlags & kIsInvisible);
	folderFlag = (myCInfoPBPtr->hFileInfo.ioFlAttrib & 0x10);
	
	// because the semantics of the filter proc are "true means don't show
	// it" we need to invert the result that we return
	
	return !(visibleFlag && folderFlag);
}

int GetDirectory(Tcl_Interp *interp, Tcl_DString *initdir,
	Tk_Window parent, const char *title)
{
#pragma unused(parent)
#pragma unused(title)
	
	FileFilterYDUPP 	invisiblesExcludedCustomFilterUPP;
	StandardFileReply	reply;
	FSSpec dirSpec;
	long dirID;
	OSErr err;
	Boolean isDirectory;

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
		/*
		 * Make sure you negate -dirSpec.vRefNum because the standard file
		 * package wants it that way !
		 */
		LMSetSFSaveDisk(-dirSpec.vRefNum);
		LMSetCurDirStore(dirID);
	}
	
	invisiblesExcludedCustomFilterUPP = NewFileFilterYDProc(OnlyVisibleFoldersCustomFileFilter);

	StandardGetFolder(invisiblesExcludedCustomFilterUPP, &reply);

	DisposeRoutineDescriptor(invisiblesExcludedCustomFilterUPP);

	if (reply.sfGood) {
		int length;
		Handle pathHandle = NULL;
		char * pathName = NULL;
    	
		FSpPathFromLocation(&reply.sfFile, &length, &pathHandle);

		if (pathHandle != NULL) {
			HLock(pathHandle);
			Tcl_SetObjResult(interp, Tcl_NewStringObj(*pathHandle, length));
			HUnlock(pathHandle);
			DisposeHandle(pathHandle);
		}
	}

    return TCL_OK;
}
