/* Win32 CRT headers. */

#ifdef __MWERKS__

typedef long _dev_t;			/* device code */
#define _DEV_T_DEFINED

#define _open	open
#define _read	read
#define _write	write
#define _close	close
#define _lseek	lseek

#define EINTR	4 //(_NERR+1)
#endif

#include <io.h>
#include <fcntl.h>
#include <direct.h>
#include <stat.h>

/* from Win32 SDK's <fcntl.h> */
#ifdef __MWERKS__
/* MetroWerks open() uses a strange mix of Mac CW UNIX and Microsoft CRT flags.
   These flags are taken from the MacOS Support version of fcntl.h */
#define O_RDWR			0x0			/* open the file in read/write mode */
#define O_RDONLY		0x1			/* open the file in read only mode */
#define O_WRONLY		0x2			/* open the file in write only mode */
#else
#define O_RDONLY	_O_RDONLY
#define O_WRONLY	_O_WRONLY
#define O_RDWR		_O_RDWR
#endif
#define O_APPEND	_O_APPEND
#define O_CREAT 	_O_CREAT
#define O_TRUNC 	_O_TRUNC
#define O_EXCL		_O_EXCL
#define O_TEXT		_O_TEXT
#define O_BINARY	_O_BINARY
#define O_RAW		_O_BINARY
#define O_TEMPORARY	_O_TEMPORARY
#define O_NOINHERIT	_O_NOINHERIT
#define O_SEQUENTIAL	_O_SEQUENTIAL
#define O_RANDOM	_O_RANDOM

/* from Win32 SDK's <io.h> */
#ifdef __MWERKS__
#define access	   _access
#define unlink	   _unlink
#else
#define access	   _access
#define chmod	   _chmod
#define chsize	   _chsize
#define close	   _close
#define creat	   _creat
#define dup	   _dup
#define dup2	   _dup2
#define eof	   _eof
#define filelength _filelength
#define isatty	   _isatty
#define locking    _locking
#define lseek	   _lseek
#define mktemp	   _mktemp
#define open	   _open
#define read	   _read
#define setmode    _setmode
#define sopen	   _sopen
#define tell	   _tell
#define umask	   _umask
#define unlink	   _unlink
#define write	   _write
#endif

/* For some reason <io.h> doesn't define R_OK, W_OK, etc */
#define F_OK	0
#define W_OK	2
#define R_OK	4
#define X_OK	F_OK

/* from Win32 SDK's <direct.h> */
#define chdir	_chdir
#define getdcwd	_getdcwd
#define getcwd	_getcwd
#define mkdir	_mkdir
#define rmdir	_rmdir
#define diskfree_t  _diskfree_t

/* from Win32 SDK's <stat.h> */
#define S_IFMT	 _S_IFMT
#define S_IFDIR  _S_IFDIR
#define S_IFCHR  _S_IFCHR
#define S_IFREG  _S_IFREG
#define S_IREAD  _S_IREAD
#define S_IWRITE _S_IWRITE
#define S_IEXEC  _S_IEXEC


#define fstat	 _fstat
#define stat	 _stat
#define lstat	 _stat


#if 0
/* Fix a problem with the definition of _dev_t, and the MSVC 2.0 Library we use. */
#pragma pack(push,4)

struct _stat_patch {
	long st_dev;
	_ino_t st_ino;
	unsigned short st_mode;
	short st_nlink;
	short st_uid;
	short st_gid;
	long st_rdev;
	_off_t st_size;
	time_t st_atime;
	time_t st_mtime;
	time_t st_ctime;
	};
#pragma pack(pop)

#define _fstat_patch(f, sp)	_fstat((f), ((struct _stat *)sp))
#define _stat_patch(f, sp)		_stat((f), ((struct _stat *)sp))


#define fstat	_fstat_patch
#define stat	_stat_patch


/* Extra definitions. */
#define lstat _stat_patch
#endif
#define getpid() 0

#ifndef S_IRUSR
#define S_IRUSR  S_IREAD
#endif

#ifndef S_IWUSR
#define S_IWUSR  S_IWRITE
#endif

#ifndef S_IXUSR
#define S_IXUSR  S_IEXEC
#endif

#ifndef S_IRWXU
#define S_IRWXU  (S_IREAD | S_IWRITE | S_IEXEC)
#endif

#ifndef S_ISDIR
#define S_ISDIR(m)  (((m)& S_IFMT) == S_IFDIR)
#endif

#ifndef S_ISCHR
#define S_ISCHR(m)  (((m)& S_IFMT) == S_IFCHR)
#endif

#ifndef S_ISBLK
#define S_ISBLK(m)  (0)
#endif

#ifndef S_ISREG
#define S_ISREG(m)  (((m)& S_IFMT) == S_IFREG)
#endif

#if defined(S_IFLNK) && !defined(S_ISLNK)
#define S_ISLNK(m)  (((m)& S_IFMT) == S_IFLNK)
#endif

#if defined(S_IFSOCK) && !defined(S_ISSOCK)
#define S_ISSOCK(m) (((m)& S_IFMT) == S_IFSOCK)
#endif

#ifndef S_ISFIFO
#define S_ISFIFO(m) (0)
#endif
