From vellino@bnr.caFri Oct 20 19:48:51 1995
Date: Fri, 20 Oct 1995 17:01:00 -0400 
From: "andre (a.n.) vellino" <vellino@bnr.ca>
To: ken@als.com
Subject: hardware.h 

#ifndef _H_hardware
#define _H_hardware

#include "base.h"
#include <stdio.h>

#if defined(unix)
#ifndef mdelta                    /* Not defined on 68000 */
#define DBL_DIG			15
#if defined(ultrix) || defined(sgi)
/* DBL_MAX already defined */
#else
#define DBL_MAX			MAXDOUBLE
#endif
#endif
#endif

#ifdef mdelta
#define MAXPATHLEN              256
/***** Doesn't have getwd either, fake it 
REMOVED by Oz 04/95 #define getwd(buff)             getcwd(buff, MAXPATHLEN)
******/

/*These two macros were expected in sys/time.h, but aren't there... */
#define timercmp(tvp, uvp, cmp) \
        ((tvp)->tv_sec cmp (uvp)->tv_sec || \
         (tvp)->tv_sec EQ (uvp)->tv_sec && (tvp)->tv_usec cmp (uvp)->tv_usec)
#define timerclear(tvp)         ((tvp)->tv_sec = (tvp)->tv_usec = 0)

/*All this stuff should have been in sys/stat.h, and wasn't */
#define _S_IFMT         0170000 /* type of file */
#define _S_IFREG        0100000 /* regular */
#define _S_IFBLK        0060000 /* block special */
#define _S_IFCHR        0020000 /* characteer special */
#define _S_IFDIR        0040000 /* directory */
#define _S_IFIFO        0010000 /* pipe or FIFO */

#define S_ISDIR(_M)     ((_M & _S_IFMT) EQ _S_IFDIR) /* test for directory */
#define S_ISCHR(_M)     ((_M & _S_IFMT) EQ _S_IFCHR) /* test for char special */
#define S_ISBLK(_M)     ((_M & _S_IFMT) EQ _S_IFBLK) /* test for block special */
#define S_ISREG(_M)     ((_M & _S_IFMT) EQ _S_IFREG) /* test for regular file */
#define S_ISFIFO(_M)    ((_M & _S_IFMT) EQ _S_IFIFO) /* test for pipe or FIFO */

/* This should have been in sys/signal.h and wasn't */
/* macro to find proper bit in a signal bit mask */
#define sigmask(signo)  (1L << (signo - 1))
#endif

extern long BNRPMaxLong;
extern short BNRPFPPrecision;
#define BNRP_getMaxLong()		BNRPMaxLong
fp BNRP_getMaxFP();
#define BNRP_getFPPrecision()	BNRPFPPrecision
unsigned long BNRP_getUserTime(BNRP_Boolean delay, fp *adjustment);
unsigned long BNRP_getElapsedTime(void);
BNRP_Boolean BNRP_truncateFile(FILE *f, long len);
BNRP_Boolean BNRP_setConfig(char *s);
char *BNRP_getConfig();

typedef struct fileDate {
	int year;
	int month;
	int day;
	int hour;
	int minute;
	int second;
	int dayOfWeek;
	} fileDate;
typedef struct fileInfo {
	char fileCreator[5];
	char fileType[5];
	BNRP_Boolean DFOpen;
	long DSize;
	BNRP_Boolean RFOpen;
	long RSize;
	fileDate created;
	fileDate modified;
	} fileInfo;
typedef struct BNRP_dirEntry {
	char *name;
	BNRP_Boolean isDir;
	} BNRP_dirEntry;
	
int getFileInfo(char *path, fileInfo *info);
int exists(char *filename);

#ifdef Macintosh
#define MAXPATHLEN			256
char *getwd(char *buff);
int chdir(char *path);
#else
#include <sys/param.h>		/* for MAXPATHLEN */
#endif

#if defined(hpux) || defined(AUX) || defined(solaris) || defined(nav) || defined(mdelta)
/* char *getcwd(char *buff, size_t len); */
#define getwd(buff)			getcwd(buff, MAXPATHLEN)
#else
char *getwd(char *buff);
#endif

char *realpath(char *path, char *resolvedpath);

#if defined(Macintosh)
int BNRP_mkdir(char *path);
int BNRP_rmdir(char *path);
#define mkdir(path, mode)	BNRP_mkdir(path)
#define rmdir(path)			BNRP_rmdir(path)
#else
#define remove		unlink
/* #if defined(sun)		 Newer compiler's than gcc 2.2.2 don't need this 
int mkdir(char *path, int mode);
int rmdir(char *path);
#endif*/
#endif

BNRP_Boolean BNRP_opendir(char *name);
void BNRP_closedir(void);
BNRP_dirEntry *BNRP_readdir(void);
BNRP_Boolean BNRP_openvols(void);
BNRP_dirEntry *BNRP_readvol(void);
void BNRP_setTimer(fp *time, void (*func)());
void BNRP_sleep(fp time, int ignoreTicks);

#ifdef Macintosh
void *BNRP_malloc(unsigned long size);
void *BNRP_realloc(void *p, unsigned long size);
void BNRP_free(void *ptr);
#else
#ifdef sun
#define BNRP_malloc(size)		(void *)memalign(headerAlignment, size)
#else
#define BNRP_malloc(size)		(void *)malloc(size)
#endif
#define BNRP_realloc(p, size)	(void *)realloc(p, size)
#define BNRP_free(p)			free(p)
#endif

#ifdef Macintosh
#define FS_SEPARATOR		':'
#else
#define FS_SEPARATOR		'/'
#endif


/* on System V implementations, real-time timer can interrupt system calls   */
/* (reads from a terminal, wait, pause).  Set this flag to compile code that */
/* checks for it.  On BSD systems, the timer is queued until we return to    */
/* user code, which is OK since the user only gets to see it at the next LIP.*/
#if defined(hpux) || defined(mdelta)  /* Added mdelta */
#define TICK_INTERRUPTS_SYSTEM_CALLS
#endif

#endif
