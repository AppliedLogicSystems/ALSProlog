#include "defs.h"

#ifdef MSWin32

#include <windows.h>
#include <errno.h>

#include "fswin32.h"

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

#ifdef __MWERKS__
int als_access(const char *path, int mode)
{
	/* ALS MOD */
    DWORD attributes;
    int result;
    
    attributes = GetFileAttributes(path);
 
	if (attributes == 0xFFFFFFFF) {
		if (GetLastError() == ERROR_INVALID_ACCESS) errno = EACCES;
		else errno = ENOENT;
		result = -1;
	} else {
		if (mode & W_OK && attributes & FILE_ATTRIBUTE_READONLY) {
			errno = EACCES;
			result = -1;
		} else {
			errno = ENOERR;
			result = 0;
		}
	}
	
	return result;
}

/*** ALS MOD - derived from __get_time() in time.win32.c ***/
static time_t FileTimeToTime_T(const FILETIME *fileTime)
{
	SYSTEMTIME time;
	long years, leap_years, leap_days, days, i, secs;
	
	static int MonthDays[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
							//jan feb mar apr may jun jul aug sep oct nov dec

	// First, convert file time to system time

	FileTimeToSystemTime(fileTime, &time);

	// Calculate the number of full years

	years = time.wYear - 1900;

	// Calculate the number of leap years in that time

	leap_years = years;
	if (time.wMonth > 2)
	{
	    leap_years++;
	}
	leap_days = (leap_years + 1) / 4;


	// Calculate total days

	days = (years * 365) + leap_days;

	// Calculate days in this year

	for (i=1; i<time.wMonth; i++)
	{
	    days += MonthDays[i-1];
	}
	days += time.wDay - 1;
	
	// and calculate the seconds

	secs =  (((((days * 24) + time.wHour) * 60) + time.wMinute) * 60) + time.wSecond;
	
	return secs;

}

/*
 *	int stat(char *path, struct stat *buf)
 *
 *		Returns information about a file.
 */
int als_stat(const char *path, struct als_stat *buf)
{
/*** ALS MOD ***/
	HANDLE findHandle;
	WIN32_FIND_DATA findFileData;
	int result;
	
	if (path) {
		findHandle = FindFirstFile(path, &findFileData);
		
		if (findHandle != INVALID_HANDLE_VALUE) {
			
			if (findFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
				buf->st_mode = S_IFDIR;
				buf->st_nlink = 2;
			} else {
				buf->st_mode = S_IFREG;
				buf->st_nlink = 1;
			}
			
			buf->st_ino = 0;
			buf->st_dev = 0;
			buf->st_uid = 0;
			buf->st_gid = 0;
			buf->st_rdev = 0;
			buf->st_size = findFileData.nFileSizeHigh ? MAXDWORD : findFileData.nFileSizeLow;
			buf->st_atime = FileTimeToTime_T(&findFileData.ftLastAccessTime);
			buf->st_mtime = FileTimeToTime_T(&findFileData.ftLastWriteTime);
			buf->st_ctime = FileTimeToTime_T(&findFileData.ftCreationTime);
			buf->st_blksize = 0;
			buf->st_blocks = 0;
			
			if (FindClose(findHandle)) result = 0;
			else result = -1;
			
		} else result = -1;
		
	} else result = -1;

	return result;
}

char * als_getenv(const char * name)
{
	#define VAL_MAX	1024
	static TCHAR value[VAL_MAX];
	DWORD result;
	
	result = GetEnvironmentVariable(name, value, VAL_MAX);
	
	if (result <= 0 || result > VAL_MAX) return NULL;
	else return value;
}

int als_system(const char * command)
{
/* Bug - this doesn't work from a network directory.  It looks like COMMAND.COM
   is unhappy with working directories like \\machine\share\dir.  I think
   this is something for Microsoft to fix. */
	#define COM_MAX 1024
	TCHAR commandLine[COM_MAX] = "COMMAND.COM";
	TCHAR title[MAX_PATH];
	BOOL got_title;
	STARTUPINFO startInfo;
	PROCESS_INFORMATION procInfo;
	DWORD result;
    
	GetEnvironmentVariable("COMSPEC", commandLine, COM_MAX);
    
	strncat(commandLine, " /C ", COM_MAX-1);
    
	strncat(commandLine, command, COM_MAX-1);

	startInfo.cb = sizeof(STARTUPINFO);
	startInfo.lpReserved = NULL;
	startInfo.lpDesktop = NULL;
	startInfo.lpTitle = NULL;
	startInfo.dwFlags = 0;
	startInfo.cbReserved2 = 0;
	startInfo.lpReserved2 = NULL;

	/* Some commands (for example Unix95's ls) change the console title without
	   restoring it, so lets save and restore the title. */
	got_title = GetConsoleTitle(title, MAX_PATH);

	if (!CreateProcess(NULL, commandLine, NULL, NULL, TRUE, 0, NULL, NULL,
		&startInfo, &procInfo)) return 0;

	if (WaitForSingleObject(procInfo.hProcess, INFINITE) != WAIT_OBJECT_0
		|| !GetExitCodeProcess(procInfo.hProcess, &result)) result = 0;

	if (got_title) SetConsoleTitle(title);

	return result;
}

#endif

unsigned char *open_memory_file(const char *file_name, mem_file_info *info)
{
    HANDLE file, file_map;
    unsigned char *mem;
    
    file = CreateFile(file_name, GENERIC_READ, FILE_SHARE_READ,
    		      NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (file == INVALID_HANDLE_VALUE) goto error;
    
    file_map = CreateFileMapping(file, NULL, PAGE_READONLY, 0, 0, NULL);
    if (file_map == INVALID_HANDLE_VALUE) goto close_error;
    
    mem = MapViewOfFile(file_map, FILE_MAP_READ, 0, 0, 0);
    if (mem == NULL) goto close_close_error;
    
    CloseHandle(file_map);
    CloseHandle(file);
    
    info->start = mem;
    info->length = 0;
    
    return mem;
    
close_close_error:
    CloseHandle(file_map);
close_error:
    CloseHandle(file);
error:
    return NULL;
}

void close_memory_file(mem_file_info *info)
{
    UnmapViewOfFile(info->start);
}

#endif
