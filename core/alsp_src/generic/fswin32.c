#include "defs.h"

#ifdef MSWin32
#include "fswin32.h"
#include <windows.h>

/* A replacement for system until Metrowerks fixes its version!. */
int system_patch(const char *command)
{
    #define COM_MAX 1024
    TCHAR commandLine[COM_MAX] = "COMMAND.COM";
    TCHAR title[MAX_PATH];
    DWORD got_title;
    STARTUPINFO startInfo;
    PROCESS_INFORMATION procInfo;
    DWORD result;
    
    GetEnvironmentVariable("COMSPEC", commandLine, COM_MAX-1);
    
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


#if 0

int _stat_patch(const char *fname, struct _stat_patch *newstat)
{
	char b1[1000] = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
	struct _stat oldstat;
	char b2[1000];
	int result;
	
	printf("Size of struct _stat: %ld\n", sizeof(struct _stat));
	printf("Size of struct _stat_patch: %ld\n", sizeof(struct _stat_patch));
	
	result = _stat(fname, (struct _stat *)newstat);
	
	if (result != -1) {
		printf("st_dev: %ld\n", newstat->st_dev);
		printf("st_ino: %ld\n", newstat->st_ino);
		printf("st_mode: %ld\n", newstat->st_mode);
		printf("st_nlink: %ld\n", newstat->st_nlink);
		printf("st_uid: %ld\n", newstat->st_uid);
		printf("st_gid: %ld\n", newstat->st_gid);
		printf("st_rdev: %ld\n", newstat->st_rdev);
		printf("st_size: %ld\n", newstat->st_size);
		printf("st_atime: %ld\n", newstat->st_atime);
		printf("st_mtime: %ld\n", newstat->st_mtime);
		printf("st_ctime: %ld\n", newstat->st_ctime);
/*
		newstat->st_dev = oldstat.st_dev;
		newstat->st_ino = oldstat.st_ino;
		newstat->st_mode = oldstat.st_mode;
		newstat->st_nlink = oldstat.st_nlink;
		newstat->st_uid = oldstat.st_uid;
		newstat->st_gid = oldstat.st_gid;
		newstat->st_rdev = oldstat.st_rdev;
		newstat->st_size = oldstat.st_size;
		newstat->st_atime = oldstat.st_atime;
		newstat->st_mtime = oldstat.st_mtime;
		newstat->st_ctime = oldstat.st_ctime;
*/
	}
	
	return result;
}

long 
get_file_modified_time(fname)
    CONST char *fname;
{
    struct stat buf;

    if (stat(fname, &buf) == -1 || buf.st_mode & S_IFDIR)
	return (long) 0;

    return buf.st_mtime;
}

int stat(const char *path, struct stat *buf)
{
	return 0;
}
/*
 * Returns 1 if fname is a directory, 0 otherwise.
 *
 */
int
isdir(fname)
    CONST char *fname;
{
    struct stat buf;

    if (stat(fname, &buf) == -1)
	return (0);

    return (buf.st_mode & S_IFDIR);
}


char *getcwd(char *buf, int size)
{
	DWORD dwResult;
	
	dwResult = GetCurrentDirectory(size, buf);
	
	if (dwResult > 0 && dwResult < size) return buf;
	else return 0;
}

char *getdcwd(int x, char *buf, int size)
{
	DWORD dwResult;
	
	dwResult = GetCurrentDirectory(size, buf);
	
	if (dwResult > 0 && dwResult < size) return buf;
	else return 0;
}

int chdir(const char *dirname)
{
	if (SetCurrentDirectory(dirname)) return 0;
	else return -1;
}

static int mkdir(const char *path, int mode)
{
	if (CreateDirectory(path, 0)) return 0;
	else return -1;
}

static int rmdir(const char *path)
{
	if (RemoveDirectory(path)) return 0;
	else return -1;
}


/*
ACCESS(2V)                SYSTEM CALLS                 ACCESS(2V)

NAME
     access - determine accessibility of file

SYNOPSIS
     #include <unistd.h>

     int access(path, mode)
     char *path;
     int mode;

DESCRIPTION
     path points to a path name naming a file.   access()  checks
     the named file for accessibility according to mode, which is
     an inclusive or of the following bits:

          R_OK           test for read permission

          W_OK           test for write permission

          X_OK           test for execute or search permission

     The following value may also be supplied for mode:

          F_OK           test whether the directories leading  to
                         the  file  can  be searched and the file
                         exists.

     The real user ID and the supplementary group IDs  (including
     the real group ID) are used in verifying permission, so this
     call is useful to set-UID programs.

     Notice that only access bits are checked.  A  directory  may
     be indicated as writable by access(), but an attempt to open
     it for writing will fail  (although  files  may  be  created
     there);  a  file may look executable, but execve() will fail
     unless it is in proper format.

RETURN VALUES
     access() returns:

     0    on success.

     -1   on failure and sets errno to indicate the error.

ERRORS
     EACCES              Search permission is denied for  a  com-
                         ponent of the path prefix of path.

                         The file access permissions do not  per-
                         mit  the  requested  access  to the file
                         named by path.

Sun Release 4.1   Last change: 21 January 1990                  1

ACCESS(2V)                SYSTEM CALLS                 ACCESS(2V)

     EFAULT              path points outside the process's  allo-
                         cated address space.

     EINVAL              An invalid value was specified for mode.

     EIO                 An I/O error occurred while reading from
                         or writing to the file system.

     ELOOP               Too many symbolic links were encountered
                         in translating path.

     ENAMETOOLONG        The length of the path argument  exceeds
                         {PATH_MAX}.

                         A  pathname  component  is  longer  than
                         {NAME_MAX} while {_POSIX_NO_TRUNC} is in
                         effect (see pathconf(2V)).

     ENOENT              The file named by path does not exist.

     ENOTDIR             A component of the path prefix  of  path
                         is not a directory.

     EROFS               The file named by path is on a read-only
                         file   system   and   write  access  was
                         requested.

SYSTEM V ERRORS
     In addtion to the above, the following may also occur:

     ENOENT              path points to an empty string.

SEE ALSO
     chmod(2V), stat(2V)

Sun Release 4.1   Last change: 21 January 1990                  2
*/

int access(const char *path, int x)
{
	DWORD fattr;
	
	fattr = GetFileAttributes(path);
	if (fattr != -1) return 0;
	else return -1;
}
#endif

#if 0
#ifndef MAXPATHLEN
#ifdef PATHSIZE
#define MAXPATHLEN  PATHSIZE
#else
#define MAXPATHLEN  1024
#endif
#endif

int
pgetcwd(void)
{
    PWord v1, sym;
    int   t1, symType;
    char  pathName[MAXPATHLEN];

    PI_getan(&v1, &t1, 1);

#ifdef	HAVE_GETWD
    if (getwd(pathName) == 0)
{
	PI_FAIL;
}
#else	/* HAVE_GETWD */
    if (getcwd(pathName, MAXPATHLEN) == 0)
{
	PI_FAIL;
}
#endif	/* !HAVE_GETWD */

    PI_makeuia(&sym, &symType, pathName);

    if (!PI_unify(v1, t1, sym, symType))
	PI_FAIL;

    PI_SUCCEED;
}

/*
 * chdir/1 (--> pchdir/1 )
 * chdir(+Path)
 *
 * Changes the current working directory to be that given by the input.
 */

int
pchdir()
{
    PWord v1;
    int   t1;
    char *pathName;

    PI_getan(&v1, &t1, 1);

    /* Make sure file name & pattern are atoms or UIAs */
    if (!getstring((UCHAR **)&pathName, v1, t1))
	PI_FAIL;

    if (chdir(pathName) == -1)
	PI_FAIL;

    PI_SUCCEED;
}


/*
 * unlink/1 (--> punlink/1 )
 * unlink(+FilePath)
 *
 * Unlinks the indicated file from the file system
 */

int
punlink(void)
{
    PWord v1;
    int   t1;
    char *pathName;

    PI_getan(&v1, &t1, 1);

    /* Make sure file name & pattern are atoms or UIAs */
    if (!getstring((UCHAR **)&pathName, v1, t1))
	PI_FAIL;

    if (unlink(pathName) == -1)
	PI_FAIL;

    PI_SUCCEED;
}
#endif


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
    
    strncat(path, pattern, MAX_PATH-1-strlen(pattern));
    path[MAX_PATH-1] = 0;

    PI_makesym(&nil, &nilType, "[]");
    
    findHandle = FindFirstFile(path, &findData); 
    
    if (findHandle != INVALID_HANDLE_VALUE) {
    
    	prev = v3; prevType = t3;
    	
    	while (FindNextFile(findHandle, &findData)) {
    	
    	    PI_makelist(&item, &itemType);
    	    PI_gethead(&head, &headType, item);
    	    PI_makeuia(&sym, &symType, findData.cFileName);
    	    if (!PI_unify(prev, prevType, item, itemType) ||
    	    	!PI_unify(head, headType, sym, symType)) {
    	    	FindClose(findHandle);
    	        PI_FAIL;
    	    }
    	    
    	    PI_gettail(&prev, &prevType, item);    	    
    	}
    
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



#if 0
/*
 * comp_file_times/2       (--> pcmp_fs/2)
 * comp_file_times(+File1, +File2)
 *
 * Succeeds if the last modification time of File1 is earlier than the
 * last modification time of File2.
 */

int
pcmp_fs(void)
{
    PWord v1, v2;
    int   t1, t2;
    char *pathName1, *pathName2;
	HANDLE f1, f2;
	FILETIME wt1, wt2;
	int success;
	

    PI_getan(&v1, &t1, 1);
    PI_getan(&v2, &t2, 2);

    /* Make sure file name & pattern are atoms or UIAs */
    if (!getstring((UCHAR **)&pathName1, v1, t1)
     || !getstring((UCHAR **)&pathName2, v2, t2))
	PI_FAIL;

	f1 = CreateFile(pathName1, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
	f2 = CreateFile(pathName2, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);

	if (f1 != INVALID_HANDLE_VALUE && f2 != INVALID_HANDLE_VALUE) {
		if (GetFileTime(f1, NULL, NULL, &wt1) && GetFileTime(f2, NULL, NULL, &wt2)) {
			success = (CompareFileTime(&wt1, &wt2) == -1);
		} else success = 0;
	} else success = 0;

	if (f1 != INVALID_HANDLE_VALUE) CloseHandle(f1);
	if (f2 != INVALID_HANDLE_VALUE) CloseHandle(f2);


	if (success) PI_SUCCEED;
	else PI_FAIL;
}


/*
 * rmdir/1 (--> prmdir/1 )
 * rmdir(+DirPath)
 *
 * Removes the indicated directory
 */

int
prmdir()
{
    PWord v1;
    int   t1;
    char *pathName;

    PI_getan(&v1, &t1, 1);

    /* Make sure file name & pattern are atoms or UIAs */
    if (!getstring((UCHAR **)&pathName, v1, t1))
	PI_FAIL;

    if (rmdir(pathName) == -1)
	PI_FAIL;

    PI_SUCCEED;
}


/*
 *      mkdir/2 (--> pmkdir/2 )
 *      mkdir(+DirPath,+Permissions)
 *
 *      Creates the indicated directory.
 */

int
pmkdir()
{
    PWord v1,v2;
    int   t1,t2;
    char *pathName;

    PI_getan(&v1, &t1, 1);
    PI_getan(&v2, &t2, 2);

    /* Make sure file name & pattern are atoms or UIAs */
    if ((!getstring((UCHAR **)&pathName, v1, t1)) ||
		t2 != PI_INT)
	PI_FAIL;

#if defined(MacOS) || defined(MSWin32)
    if (mkdir(pathName) == -1)
#else
    if (mkdir(pathName, v2) == -1)
#endif
	PI_FAIL;

    PI_SUCCEED;
}

pgetpid()
{
    PWord v1, vpid;
    int   t1, tpid;

    PI_getan(&v1, &t1, 1);
    PI_makedouble(&vpid, &tpid, (double) 0);
    if (PI_unify(v1, t1, vpid, tpid))
	PI_SUCCEED;
    else
	PI_FAIL;
}
#endif
#endif
