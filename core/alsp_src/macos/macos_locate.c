#include "defs.h"

#include <FullPath.h>

static void full_path(char *s, const FSSpec *spec)
{
	OSErr err;
	short PathLength;
	Handle PathHandle;

	err = FSpGetFullPath(spec, &PathLength, &PathHandle);
	if (err != noErr) fatal_error(FE_INFND, 0);
		
	if (PathLength >= IMAGEDIR_MAX) fatal_error(FE_INFND, 0);
		
	HLock(PathHandle);
	if (MemError() != noErr) fatal_error(FE_INFND, 0);

	strncpy(s, *PathHandle, PathLength);

	DisposeHandle(PathHandle);
	if (MemError() != noErr) fatal_error(FE_INFND, 0);
}

void locate_library_executable(int argc, char *argv[])
{
    if (MPW_Tool) {
		//command_line_locate_executable(argv[0]);
    } else {
		OSErr err;
		ProcessSerialNumber PSN;
		ProcessInfoRec info;
		FSSpec AppSpec, DirSpec;
		const FSSpec *LibrarySpec;
		
		extern shlib_found;
		extern FSSpec shlib_location;
		

	    /* Get the FSSpec for this application. */    
		PSN.highLongOfPSN = 0;
		PSN.lowLongOfPSN = kCurrentProcess;
		
		info.processInfoLength = sizeof(ProcessInfoRec);
		info.processName = NULL;
		info.processAppSpec = &AppSpec;
		
		err = GetProcessInformation(&PSN, &info);
		if (err != noErr) fatal_error(FE_INFND, 0);
		
		full_path(executable_path, &AppSpec);
		
		if (shlib_found) LibrarySpec = &shlib_location;
		else LibrarySpec = &AppSpec;
		
		full_path(library_path, LibrarySpec);

		err = FSMakeFSSpec(LibrarySpec->vRefNum, LibrarySpec->parID, "\p", &DirSpec);
		if (err != noErr && err != fnfErr)  fatal_error(FE_INFND, 0);

		full_path(library_dir, &DirSpec);
	}
}
