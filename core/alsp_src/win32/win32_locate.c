#include "defs.h"

void locate_library_executable(int argc, char *argv[])
{
    DWORD l;
    char *endpath;
#ifdef DLL
	HANDLE dll;
#endif
    
    l = GetModuleFileName(NULL, executable_path, IMAGEDIR_MAX);
    if (l == 0 || l >= IMAGEDIR_MAX) fatal_error(FE_INFND, 0);

#ifdef DLL
    dll = GetModuleHandle(DLL_NAME);
    if (dll == NULL) fatal_error(FE_INFND, 0);
    
    l = GetModuleFileName(dll, library_path, IMAGEDIR_MAX);
    if (l == 0 || l >= IMAGEDIR_MAX) fatal_error(FE_INFND, 0);
#else
	strcpy(library_path, executable_path);
#endif /* DLL */

	strcpy(library_dir, library_path);

    endpath = strrchr(library_dir, '\\');
    if (endpath == NULL) fatal_error(FE_INFND, 0);
    endpath++;  /* include the \ */
    *endpath = 0;
}
