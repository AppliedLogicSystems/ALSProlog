#include "defs.h"

#ifdef MSWin32

#include <windows.h>

/*
 * $getDirEntries/3 (--> getDirEntries/3 )
 * $getDirEntries(DirName, FilePattern, List)
 *
 * Input:
 *	DirName		-- UIA giving a directory path
 *	FilePattern	-- UIA giving a regular expression created via
 *			   make_reg_exp/2;
 * Output:
 *	List		-- List of UIAs giving file names of files residing
 *                         in DirName and matching FilePattern
 *				(cf. directory/3 in fsunix.pro)
 */

int
getDirEntries()
{
    PWord v1, v2, v3;
    int   t1, t2, t3;
    char *dirName, *pattern, path[MAX_PATH];
    size_t pathLen;
    PWord prev, item, head, sym, nil;
    int   prevType, itemType, headType, symType, nilType;
    HANDLE findHandle;
    WIN32_FIND_DATA findData;

    PI_getan(&v1, &t1, 1);
    PI_getan(&v2, &t2, 2);
    PI_getan(&v3, &t3, 3);

    /* Make sure file name & pattern are atoms or UIAs */
    if (!getstring((UCHAR **)&dirName, v1, t1)
     || !getstring((UCHAR **)&pattern, v2, t2))
	PI_FAIL;

    strncpy(path, dirName, MAX_PATH-1);
    path[MAX_PATH-1] = 0;
    
    pathLen = strlen(path);
    
    if (pathLen > 0 && path[pathLen-1] != '\\') {
        strncat(path, "\\", MAX_PATH-1-pathLen);
        path[MAX_PATH-1] = 0;
        pathLen = strlen(path);
    }
    
    strncat(path, pattern, MAX_PATH-1-pathLen);
    path[MAX_PATH-1] = 0;

    PI_makesym(&nil, &nilType, "[]");
    
    findHandle = FindFirstFile(path, &findData); 
    
    if (findHandle != INVALID_HANDLE_VALUE) {
    
    	prev = v3; prevType = t3;
    	
    	do {
    	
    	    PI_makelist(&item, &itemType);
    	    PI_gethead(&head, &headType, item);
    	    PI_makeuia(&sym, &symType, findData.cFileName);
    	    if (!PI_unify(prev, prevType, item, itemType) ||
    	    	!PI_unify(head, headType, sym, symType)) {
    	    	FindClose(findHandle);
    	        PI_FAIL;
    	    }
    	    
    	    PI_gettail(&prev, &prevType, item);    	    
    	} while (FindNextFile(findHandle, &findData));
    
    	if (!PI_unify(prev, prevType, nil, nilType)) {
    	    FindClose(findHandle);
    	    PI_FAIL;
    	}
	if (!FindClose(findHandle)) PI_FAIL;
    } else {
    	if (!PI_unify(v3, t3, nil, nilType)) PI_FAIL;
    }
        
    PI_SUCCEED;     
}
#endif
