/* MSWin32 CRT headers. */

#ifdef __MWERKS__
#include <unix.h>

/* There are no symbolic links in Win32, so lstat == stat.
   But what about Win95 aliases? */
#define lstat stat

/* unistd.win32.h is missing these defines */
#define R_OK 1
#define W_OK 2
#define X_OK 4
#define F_OK 8

int als_access(const char *path, int mode);
#undef access
#define access als_access

struct als_stat
{
	mode_t		st_mode;		/* File mode; see #define's below */
	ino_t		st_ino;			/* File serial number */
	dev_t		st_dev;			/* ID of device containing this file */
	nlink_t		st_nlink;		/* Number of links */
	uid_t		st_uid;			/* User ID of the file's owner */
	gid_t		st_gid;			/* Group ID of the file's group */
	dev_t		st_rdev;		/* Device type */
	off_t		st_size;		/* File size in bytes */
	time_t		st_atime;		/* Time of last access */
	time_t		st_mtime;		/* Time of last data modification */
	time_t		st_ctime;		/* Time of last file status change */
	long		st_blksize;		/* Optimal blocksize */
	long		st_blocks;		/* blocks allocated for file */
};

int als_stat(const char *path, struct als_stat *buf);
#undef stat
#define stat als_stat

#define EINTR	3001

#ifndef S_IFBLK
#define S_IFBLK		0
#endif
#ifndef S_IFIFO
#define S_IFIFO		0
#endif

#ifndef S_ISBLK
#define S_ISBLK(m) 0
#endif

char * als_getenv(const char * name);
int als_system(const char * command);
#define getenv als_getenv
#define system als_system

#elif defined(__GNUC__)
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>

#define lstat stat
pid_t _getpid(void);
#define getpid _getpid
#else

#include <sys/stat.h>
#include <unistd.h>

#endif

