/*===========================================================================*
 |              fsunix.c
 |      Copyright (c) 1991-1997 Applied Logic Systems, Inc.
 |
 |      -- File System Access & Manipulation -- Unix/DJGPP/
 |
 |  This is one of a group of files providing file system access:
 |		fsunix.c, fsdos.c, fsmac.c, ..., etc.,...
 |  ALL of these files are loaded when the system is built.  However,
 |  each file, after the initial  #include "defs.h"  is #ifdef'd with
 |  the appropriate constant (UNIX, DOS, ....);  this #ifdef applies to
 |  the entire file, so only one of the files is actually loaded.
 *===========================================================================*/
#include "defs.h"

#include <sys/time.h>
#include <sys/resource.h>

#include <math.h>

#include <limits.h>
#ifndef PATH_MAX
#define PATH_MAX 2048
#endif

#ifdef MISSING_GLOB
#include "compat_glob.h"
#else
#include <glob.h>
#endif
#include <errno.h>

/* unistd.h defines _POSIX_VERSION on POSIX.1 systems.  */

#include <dirent.h>
#define NLENGTH(dirent) (strlen((dirent)->d_name))


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
#define S_ISBLK(m)  (((m)& S_IFMT) == S_IFBLK)
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
#define S_ISFIFO(m) (((m)& S_IFMT) == S_IFIFO)
#endif

int
pgetcwd(void)
{
    PWord v1, sym;
    int   t1, symType;
    char  pathName[PATH_MAX];

    PI_getan(&v1, &t1, 1);

    if (getcwd(pathName, PATH_MAX) == 0)
{
	PI_FAIL;
}

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
pchdir(void)
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
getDirEntries(void)
{
    PWord v1, v2, v3;
    int   t1, t2, t3;
    char *dirName, *pattern;
    char path[PATH_MAX], *p;
    PWord list, newlist, head, tail, sym;
    int   listType, newlistType, headType, tailType, symType;
    glob_t gd;
    int i, r;

    PI_getan(&v1, &t1, 1);
    PI_getan(&v2, &t2, 2);
    PI_getan(&v3, &t3, 3);

    /* Make sure file name & pattern are atoms or UIAs */
    if (!getstring((UCHAR **)&dirName, v1, t1)
     || !getstring((UCHAR **)&pattern, v2, t2))
	PI_FAIL;

    strcpy(path, dirName);
    strcat(path, "/");
    strcat(path, pattern);

    r = glob(path, 0, NULL, &gd);
/*
#if UNIX_DARWIN / * Darwin is POSIX.1 * /
    if (r != 0) PI_FAIL;
#else / * Most others are POSIX.2 * /
    if (r != 0 && r != GLOB_NOMATCH) PI_FAIL;
#endif
*/
    if (r != 0 && r != GLOB_NOMATCH) PI_FAIL;


    for (i = gd.gl_pathc, PI_makesym(&list, &listType, "[]"); i;
	 i--, list=newlist, listType=newlistType) {
      p = strrchr(gd.gl_pathv[i-1], '/');
      PI_makeuia(&sym, &symType, p ? p+1 : gd.gl_pathv[i]);
      PI_makelist(&newlist, &newlistType);
      PI_gethead(&head, &headType, newlist);
      PI_gettail(&tail, &tailType, newlist);
      PI_unify(head, headType, sym, symType);
      PI_unify(tail, tailType, list, listType);
    };

    globfree(&gd);

    return PI_unify(v3, t3, list, listType);
}



/*
 * $getFileStatus/2 (--> getFileStatus/2 )
 * $getFileStatus(FilePath, StatusTerm)
 *
 * Input:
 *	FilePath	-- UIA giving a path to file;
 * Output:
 *	StatusTerm	-- a 5-ary term providing info about the file:
 *
 *		fileStatus(FileType,ModTime,OwnPermiss,ByteSize,Blocks)
 */

int
getFileStatus(void)
{
    PWord v1, v2, vtime;
    int   t1, t2, ttime;
    char *pathName;
    struct stat fileStats;
    PWord sTag, pstructure, arg;
    int   sTagType, pstructureType, argType;
    int   fileMode, fileType, ownerPermiss;

    PI_getan(&v1, &t1, 1);
    PI_getan(&v2, &t2, 2);

    	/* Make sure file name & pattern are atoms or UIAs */
    if (!getstring((UCHAR **)&pathName, v1, t1))
	PI_FAIL;

    if (lstat(pathName, &fileStats) == -1)
	PI_FAIL;
    fileMode = fileStats.st_mode;

    PI_makesym(&sTag, &sTagType, "fileStatus");
    PI_makestruct(&pstructure, &pstructureType, sTag, 5);
    if (!PI_unify(v2, t2, pstructure, pstructureType))
	PI_FAIL;

    if (S_ISDIR(fileMode))
	fileType = 1;
    else if (S_ISCHR(fileMode))
	fileType = 2;
    else if (S_ISBLK(fileMode))
	fileType = 3;
    else if (S_ISREG(fileMode))
	fileType = 4;
#ifdef S_ISLNK
    else if (S_ISLNK(fileMode))
	fileType = 5;
#endif /* S_ISLNK */
#ifdef S_ISSOCK
    else if (S_ISSOCK(fileMode))
	fileType = 6;
#endif /* S_ISSOCK */
    else if (S_ISFIFO(fileMode))
	fileType = 7;
    else
	fileType = 0;

    ownerPermiss = (((fileMode & S_IRUSR) > 0) << 2) |
	(((fileMode & S_IWUSR) > 0) << 1) |
	((fileMode & S_IXUSR) > 0);

    /* File Type: */
    PI_getargn(&arg, &argType, pstructure, 1);
    if (!PI_unify(arg, argType, fileType, PI_INT))
	PI_FAIL;

    /* File Mod Time: */
    PI_getargn(&arg, &argType, pstructure, 2);
    PI_makedouble(&vtime, &ttime, (double) fileStats.st_mtime);
    if (!PI_unify(arg, argType, vtime, ttime))
	PI_FAIL;

    /* File Owner Permissions: */
    PI_getargn(&arg, &argType, pstructure, 3);
    if (!PI_unify(arg, argType, ownerPermiss, PI_INT))
	PI_FAIL;

    /* File Byte Size: */
    PI_getargn(&arg, &argType, pstructure, 4);
    if (!PI_unify(arg, argType, fileStats.st_size, PI_INT))
	PI_FAIL;

    /* File Blocks Allocated: */
    PI_getargn(&arg, &argType, pstructure, 5);
#ifdef HAVE_ST_BLOCKS
    if (!PI_unify(arg, argType, fileStats.st_blocks, PI_INT))
	PI_FAIL;
#else
    if (!PI_unify(arg, argType, 0, PI_INT))
	PI_FAIL;
#endif

    PI_SUCCEED;
}


/*
 * read_link/2
 * read_link(SourcePath, ResultPath)
 *
 * Input:
 *	SourcePath	-- UIA giving a path to file which is a symbolic link;
 *			   Assumes we have already determined that SourcePath
 *			   is a symbolic link
 * Output:
 *	ResultPath	-- path to the file = value of the link
 */

int
read_link(void)
{
    PWord v1, v2;
    int   t1, t2;
    char *pathName;
    PWord sym;
    int   symType;
    int   pathSize;
    char  buffer1[1024];

    PI_getan(&v1, &t1, 1);
    PI_getan(&v2, &t2, 2);

    /* Make sure SourcePath name is an atom or UIA */
    if (!getstring((UCHAR **)&pathName, v1, t1))
	PI_FAIL;

    pathSize = readlink(pathName, buffer1, 1024);
	if (pathSize == -1)
	PI_FAIL;
	buffer1[pathSize] = 0;

    PI_makeuia(&sym, &symType, buffer1);
    if (!PI_unify(v2, t2, sym, symType))
	PI_FAIL;

    PI_SUCCEED;
}

/*
 * make_symlink/2
 * make_symlink(SourcePath, LinkPath)
 *
 * Inputs:
 *	SourcePath	-- UIA giving a path to file which is a to be the
 *			   target of the symbolic link;
 *	LinkPath	-- path to the file which will be the symbolic link
 * Note:
 *	The call
 *		make_symlink(foo,bar)
 *	will fail;  one needs a more complete path, such as:
 *		make_symlink('./foo','./bar')
 */

int
make_symlink(void)
{
    PWord v1, v2;
    int   t1, t2;
    char *pathName1, *pathName2;

    PI_getan(&v1, &t1, 1);
    PI_getan(&v2, &t2, 2);

    /* Make sure SourcePath name is an atom or UIA */
    if (!getstring((UCHAR **)&pathName1, v1, t1)
     		|| !getstring((UCHAR **)&pathName2, v2, t2))
	PI_FAIL;

/*
    if (!symlink(pathName1, pathName2))
	PI_FAIL;

    PI_SUCCEED;
*/
    if (symlink(pathName1, pathName2))
    	PI_SUCCEED;
    else
	PI_FAIL;
}


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
    struct stat fileStats1, fileStats2;

    PI_getan(&v1, &t1, 1);
    PI_getan(&v2, &t2, 2);

    /* Make sure file name & pattern are atoms or UIAs */
    if (!getstring((UCHAR **)&pathName1, v1, t1)
     || !getstring((UCHAR **)&pathName2, v2, t2))
	PI_FAIL;

    if ((stat(pathName1, &fileStats1) == -1) ||
	(stat(pathName2, &fileStats2) == -1))
	PI_FAIL;

    if (fileStats1.st_mtime < fileStats2.st_mtime)
	PI_SUCCEED;

    PI_FAIL;
}


/*
 * rmdir/1 (--> prmdir/1 )
 * rmdir(+DirPath)
 *
 * Removes the indicated directory
 */

int
prmdir(void)
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
pmkdir(void)
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

    if (mkdir(pathName, (mode_t) v2) == -1)
	PI_FAIL;

    PI_SUCCEED;
}






/*-----------------------------------------------------------------------------*
 * Some of the following code was "borrowed" from the GNU C library (getcwd.c)
 * It has been largely rewritten and bears little resemblance to what it started 
 * out as.
 *
 * The purpose of this code is to generalize getcwd.  The idea is
 * to pass it some path name as input.  This pathname can be relative, absolute,
 * whatever.  It may have elements which reference symbolic links.  The output
 * from this function will be the absolute pathname representing the same
 * file.  Actually, it only returns the directory.  If the thing you pass it
 * is a directory, you'll get that directory back (canonicalized).  If you
 * pass it a path to an ordinary file, you'll get back the canonicalized
 * directory which contains that file.
 *
 * The way that this code works is similar to the classic implementation
 * of getcwd (or getwd).  The difference is that once it finds a directory,
 * it will cache it.  If that directory is referenced again, finding it will
 * be very fast.  We use malloc and realloc to make it all work.  The
 * callee of this function should not free up the pointer which is returned.
 * This will be done automatically by the caching code.  The value returned
 * will exist at least up until the next call to canonical_pathname.  It
 * should not be relied on any longer than this.  Care should be taken not
 * to corrupt the value returned.
 *-----------------------------------------------------------------------------*/

#define TEMP_NAME_INCR (64*3)		/* additional amount of space to
					 * allocate when temp_name overflows
					 */
#define INITIAL_PATH_SIZE 512		/* starting value for our path
					 * buffer
					 */
#define CPN_CACHE_MASK 0xff
#define CPN_CACHE_SIZE (CPN_CACHE_MASK+1)

#ifdef CCANONP

#if !defined(HAVE_SETITIMER) || !defined(HAVE_SIGACTION)
#define STAT stat
#else /* !defined(HAVE_SETITIMER) */

#define STAT stat_with_timeout

static	void	stat_timedout		( void );
static	int	stat_with_timeout	( const char *, struct stat * );

static void
stat_timedout(void)
{
}

static int
stat_with_timeout(const char *path, struct stat *statbuf)
{
    struct sigaction newact;
    struct sigaction oldact;
    sigset_t newset;
    sigset_t oldset;
    struct itimerval timeout;
    struct itimerval oldtimerval;
    int retval, stat_errno;

    newact.sa_handler = stat_timedout;
    newact.sa_flags = 0;
    sigemptyset(&newact.sa_mask);

    sigfillset(&newset);
    sigdelset(&newset, SIGPROF);

    timeout.it_interval.tv_sec = 0;
    timeout.it_interval.tv_usec = 0;
    timeout.it_value.tv_sec = 0;
    timeout.it_value.tv_usec = 75000;

    if (sigprocmask(SIG_BLOCK, &newset, &oldset) < 0)
	return -1;

    if (sigaction(SIGPROF,&newact, &oldact) < 0) {
	sigprocmask(SIG_SETMASK, &oldset, NULL);
	return -1;
    }

    setitimer(ITIMER_PROF, &timeout, &oldtimerval);
    retval = stat(path, statbuf);
    stat_errno = errno;
    timeout.it_value.tv_usec = 0;
    setitimer(ITIMER_PROF, &timeout, NULL);

    sigaction(SIGPROF, &oldact, NULL);
    sigprocmask(SIG_SETMASK, &oldset, NULL);
    setitimer(ITIMER_PROF, &oldtimerval, NULL);

    return retval;
}
#endif /* !defined(HAVE_SETITIMER) */

static char *
canonical_pathname(char *path_name, char **file_namep)
{
    dev_t rootdev, thisdev;
    ino_t rootino, thisino;
    register char *pathp;
    struct stat  st;

    static char *temp_name = 0;
    static size_t temp_name_size = 0;
    char        *tnp = temp_name;	/* temp name pointer */

    char        *path;		/* the path that we've determined */
    int          path_size;	/* size of allocated path area */
    int          len;		/* temporary for length computations */
    
    static struct cpn_cache {
	dev_t ce_dev;
	ino_t ce_ino;
	char *ce_dirname;
    } cache_entries[256];

    struct cpn_cache *cachep;

    *file_namep = NULL;
    len = strlen(path_name);

    if (len > temp_name_size) {
	temp_name_size = len + TEMP_NAME_INCR;
	if (temp_name)
	    temp_name = realloc(temp_name, temp_name_size);
	else
	    temp_name = malloc(temp_name_size);
	if (temp_name == NULL)
	    return NULL;
    }

    path_size = INITIAL_PATH_SIZE >= len ? INITIAL_PATH_SIZE : len;
    path = malloc((size_t)path_size);

    if (path == NULL)
	return NULL;

    pathp = path + path_size;
    *--pathp = '\0';

    strcpy(temp_name, path_name);
    tnp = temp_name + len;

    if (stat(temp_name, &st) < 0 || !S_ISDIR(st.st_mode)) {
	while (tnp >= temp_name && *tnp != '/')
	    tnp--;
	if (tnp < temp_name) {
	    *++tnp = '.';
	    tnp++;
	    *file_namep = path_name;
	}
	else
	    *file_namep = path_name + (tnp - temp_name) + 1;
	if (tnp == temp_name && *tnp == '/') /* initial slash */
	    *++tnp = '\0';
	else
	    *tnp = '\0';

	if (stat(temp_name, &st) < 0 || !S_ISDIR(st.st_mode)) {
	    free(path);
	    return NULL;
	}
    }

    thisdev = st.st_dev;
    thisino = st.st_ino;

    cachep =  &cache_entries[(thisdev ^ thisino) & CPN_CACHE_MASK];
    if (cachep->ce_dirname && cachep->ce_ino == thisino
			   && cachep->ce_dev == thisdev)
	return cachep->ce_dirname;
    else {
	cachep->ce_ino = thisino;
	cachep->ce_dev = thisdev;
	if (cachep->ce_dirname)
	    free(cachep->ce_dirname);
	cachep->ce_dirname = NULL;	/* fill in later if everything works */
    }

    if (STAT("/", &st) < 0) {
	free(path);
	return NULL;
    }
    rootdev = st.st_dev;
    rootino = st.st_ino;

    while (!(thisdev == rootdev && thisino == rootino)) {
	register DIR *dirstream;
	register struct dirent *d;
	dev_t dotdev;
	ino_t dotino;
	char  mount_point;
	int   namelen = 0;

	if (tnp-temp_name > temp_name_size-4) {
	    int tlen = tnp - temp_name;
	    temp_name_size += TEMP_NAME_INCR;
	    temp_name = (char *) realloc(temp_name, temp_name_size);
	    if (temp_name == NULL) {
		free(path);
		return NULL;
	    }
	    tnp = temp_name + tlen;
	}
	*tnp++ = '/';
	*tnp++ = '.';
	*tnp++ = '.';
	*tnp = '\0';

	/* Figure out if this directory is a mount point.  */
	if (STAT(temp_name, &st) < 0) {
	    free(path);
	    return NULL;
	}
	dotdev = st.st_dev;
	dotino = st.st_ino;
	mount_point = dotdev != thisdev;

	/* Search for the last directory.  */
	dirstream = opendir(temp_name);
	if (dirstream == NULL) {
	    free(path);
	    return NULL;
	}
	while ((d = readdir(dirstream)) != NULL) {
	    namelen = strlen(d->d_name);
	    if (d->d_name[0] == '.' &&
	    (namelen == 1 || (namelen == 2 && d->d_name[1] == '.')))
		continue;
	    if (mount_point || d->d_ino == thisino) {
		if (tnp-temp_name > temp_name_size-namelen-1) {
		    int tlen = tnp - temp_name;
		    temp_name_size += TEMP_NAME_INCR;
		    temp_name = (char *) realloc(temp_name, temp_name_size);
		    if (temp_name == NULL) {
			(void) closedir(dirstream);
			free(path);
			return NULL;
		    }
		    tnp = temp_name + tlen;
		}

		*tnp = '/';
		strcpy(tnp+1,d->d_name);

		if (STAT(temp_name, &st) == 0
		 && st.st_dev == thisdev && st.st_ino == thisino)
		    break;
	    }
	}

	if (d == NULL) {
	    (void) closedir(dirstream);
	    free(path);
	    return NULL;
	}
	else {
	    if (pathp - path < namelen + 1) {
		int pos = pathp - path;
		int incr = namelen+INITIAL_PATH_SIZE;
		path = realloc(path, (size_t)(path_size+incr));
		if (path == NULL) {
		    (void) closedir(dirstream);
		    return NULL;
		}
		memmove(path+incr,path,(size_t)path_size);
		path_size += incr;
		pathp = path + incr + pos;
	    }
	    pathp -= namelen;
	    (void) memcpy(pathp, d->d_name, (size_t)namelen);
	    *--pathp = '/';
	    (void) closedir(dirstream);
	}

	thisdev = dotdev;
	thisino = dotino;
    }

    if (pathp == &path[path_size - 1])
	*--pathp = '/';

    memmove(path, pathp, (size_t)(path + path_size - pathp));
    path = realloc(path, (size_t)(path + path_size - pathp));
    cachep->ce_dirname = path;	/* squirrel it away for later */
    return path;
}



static int
canonicalize_pathname(void)
{
    PWord v1, v2, vp;
    int   t1, t2, tp;
    char *inpath;
    char *outpath;
    char *filename;

    PI_getan(&v1, &t1, 1);
    PI_getan(&v2, &t2, 2);

    if (!getstring((UCHAR **)&inpath, v1, t1)
     || (outpath = canonical_pathname(inpath,&filename)) == (char *) 0)
	PI_FAIL;

    if (filename) {
	int plen = strlen(outpath);
	char *hs;
	PI_allocuia(&vp, &tp, (int)(plen+strlen(filename)+1));
	hs = PI_getuianame(0,vp,0);
	memcpy(hs,outpath,(size_t)plen);
	hs[plen] = '/';
	strcpy(hs+plen+1,filename);
    }
    else
	PI_makeuia(&vp, &tp, outpath);

    if (PI_unify(v2, t2, vp, tp))
	PI_SUCCEED;
    else
	PI_FAIL;
}
#endif /* CCANONP */

long 
get_file_modified_time(const char *fname)
{
    struct stat buf;

    if (stat(fname, &buf) == -1 || buf.st_mode & S_IFDIR)
	return (long) 0;

    return buf.st_mtime;
}

/*
 * Returns 1 if fname is a directory, 0 otherwise.
 *
 */
int
isdir(const char *fname)
{
    struct stat buf;

    if (stat(fname, &buf) == -1)
	return (0);

    return (buf.st_mode & S_IFDIR);
}

int
pgetpid(void)
{
    PWord v1, vpid;
    int   t1, tpid;

    PI_getan(&v1, &t1, 1);
    PI_makedouble(&vpid, &tpid, (double) getpid());
    if (PI_unify(v1, t1, vpid, tpid))
	PI_SUCCEED;
    else
	PI_FAIL;
}

#include <sys/mman.h>
#include <fcntl.h>

#ifdef MACH_SUBSTRATE
#include <mach/mach.h>
#endif

#ifndef MAP_FAILED
#define MAP_FAILED ((void *)-1)
#endif

unsigned char *open_memory_file(const char *file_name, mem_file_info *info)
{
    int file, r;
    struct stat s;
    void *mem;
#ifdef MACH_SUBSTRATE
    kern_return_t kr;
#endif
    
    file = open(file_name,  O_RDONLY | O_BINARY);
    if (file == -1) goto error;

    r = fstat(file, &s);
    if (r != 0) goto close_error;
    
#ifdef HAVE_MMAP_ZERO
    mem = mmap(NULL, s.st_size, PROT_READ, MAP_FILE | MAP_VARIABLE | MAP_PRIVATE, file, 0);
    if (mem == (unsigned char *)-1) goto close_error;
#elif defined(MACH_SUBSTRATE)
    kr = map_fd(file, 0, (vm_offset_t *)&mem, TRUE, s.st_size);
    if (kr != KERN_SUCCESS) goto close_error;
#else
    mem = mmap(NULL, s.st_size, PROT_READ, MAP_PRIVATE, file, 0);
    if (mem == MAP_FAILED) goto close_error;
#endif
    
    close(file);

    info->start = mem;
    info->length = s.st_size;
    
    return mem;

close_error:
    close(file);
error:
    return NULL;

}

void close_memory_file(mem_file_info *info)
{
#ifdef MACH_SUBSTRATE
    (void) vm_deallocate(task_self(), (vm_address_t)info->start, info->length);
#else
    munmap(info->start, info->length);
#endif
}



/* *INDENT-OFF* */
PI_BEGIN
#ifndef PURE_ANSI
    PI_PDEFINE("getcwd", 1, pgetcwd, "_pgetcwd")
    PI_PDEFINE("chdir", 1, pchdir, "_pchdir")
    PI_PDEFINE("unlink", 1, punlink, "_punlink")
#ifdef FSACCESS
    PI_PDEFINE("$getDirEntries", 3, getDirEntries, "_getDirEntries")
    PI_PDEFINE("$getFileStatus", 2, getFileStatus, "_getFileStatus")
    PI_PDEFINE("comp_file_times", 2, pcmp_fs, "_pcmp_fs")
    PI_PDEFINE("rmdir", 1, prmdir, "_prmdir")
    PI_PDEFINE("mkdir", 2, pmkdir, "_pmkdir")
#ifdef HAVE_SYMLINK
    PI_PDEFINE("read_link", 2, read_link, "_read_link")
    PI_PDEFINE("make_symlink", 2, make_symlink, "_make_symlink")
#endif /* HAVE_SYMLINK */
#ifdef CCANONP
    PI_PDEFINE("canonicalize_pathname", 2, canonicalize_pathname, "_canonicalize_pathname")
#endif /* CCANONP */
    PI_PDEFINE("getpid",1,pgetpid,"_pgetpid")
#endif /* FSACCESS */
#endif /* PURE_ANSI */
PI_END
/* *INDENT-ON* */

void
init_fsutils(void)
{
    PI_INIT;
}


int os_copy_file(const char *from_file, const char *to_file)
{
    unsigned char *buf;
    int f, c, r;
    struct stat s; 
    
    f = open(from_file, O_RDONLY | O_BINARY);
    if (f == -1) return 0;
    
    r = fstat(f, &s);
    if (r != 0) {
    	close(f);
        return 0;
    }
    
    buf = malloc((size_t)s.st_size);
    if (buf == NULL) {
    	close(f);
        return 0;
    }
    
    r = read(f, buf, (size_t)s.st_size);
    if (r != s.st_size) {
    	free(buf);
    	close(f);
        return 0;
    }

    close(f);
    
    c = open(to_file, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, 0777);
    if (c == -1) {
    	free(buf);
    	return 0;
    }
    
    r = write(c, buf, (size_t)s.st_size);
    if (r != s.st_size) {
    	free(buf);
        close(f);
        return 0;
    }
    
    close(c);
    
    free(buf);
    
    return 1;
}


static double clock_ticks_per_second;
static double process_start_time;

#define TV2SEC(x) ((double) x.tv_sec + (double) x.tv_usec * 0.000001)

void os_init_time(void)
{
	struct timeval time;
	
	gettimeofday(&time, NULL);
	process_start_time = TV2SEC(time);
	clock_ticks_per_second = sysconf(_SC_CLK_TCK);
}

double os_cputime(void)
{
#ifdef USE_GETRUSAGE
	struct rusage usage;

	if (getrusage(RUSAGE_SELF, &usage) == 0)
		return TV2SEC(usage.ru_utime) + TV2SEC(usage.ru_stime);
	else return 0;
#else
	struct tms buf;

	if (times(&buf) != -1)
		return ((double) buf.tms_utime + (double) buf.tms_stime)
			/ clock_ticks_per_second;
	else return 0;
#endif
}

double os_realtime(void)
{
	struct timeval time;
	
	gettimeofday(&time, NULL);
	return TV2SEC(time) - process_start_time;
}

int os_set_timer(double dval, double dinterval)
{
    struct itimerval itv;

    itv.it_interval.tv_sec = (long) floor(dinterval);
    itv.it_interval.tv_usec = (long) (1000000 * (dinterval - floor(dinterval)));

    itv.it_value.tv_sec = (long) floor(dval);
    itv.it_value.tv_usec = (long) (1000000 * (dval - floor(dval)));

    if (setitimer(ITIMER_REAL, &itv, (struct itimerval *) 0) != 0) {
	perror("pbi_alarm");
    }

#if defined(HAVE_SIGACTION) && defined(SA_SIGINFO)
    {
	struct sigaction act;

	act.sa_handler = signal_handler;
	act.sa_flags = SA_SIGINFO;
	(void) sigaction(SIGALRM, &act, 0);
    }
#elif defined(HAVE_SIGVEC) || defined(HAVE_SIGVECTOR)
    {
	struct sigvec v;

	v.sv_handler = signal_handler;
	v.sv_mask = 0;
#ifdef HAVE_SIGVECTOR
	v.sv_flags = SV_BSDSIG;	/* do not restart certain system calls */
	sigvector(SIGALRM, &v, 0);
#else
	v.sv_flags = SV_INTERRUPT;	/* do not restart certain system calls */
	sigvec(SIGALRM, &v, 0);
#endif	/* HAVE_SIGVECTOR */
    }
#else				/* !HAVE_SIGVEC && !HAVE_SIGVECTOR */
    (void) signal(SIGALRM, signal_handler);
#endif				 /* defined(HAVE_SIGVEC) || defined(HAVE_SIGVECTOR) */

	return 1;
}
