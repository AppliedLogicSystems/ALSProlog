/*=================================================================*
 |			fsdos.c
 |	Copyright (c) 1991-1995 Applied Logic Systems, Inc.
 |
 |		File System Access & Manipulation -- DOS
 |
 *=================================================================*/
#include "defs.h"

#if defined(DOS) || defined(__GO32__) || defined(OS2)
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

#ifndef MAXPATHLEN
#define MAXPATHLEN FILENAME_MAX
#endif

#ifdef HAVE_REGCMP
#include <sys/types.h>
#include <regex.h>
#endif

/*
 * $getDirEntries/3 (--> getDirEntries/3 )
 * $getDirEntries(DirName, FilePattern, List)
 *
 * Input:
 *	DirName		-- UIA giving a directory path
 *      FilePattern	-- UIA giving a regular expression created via
 *			   make_reg_exp/2;
 * Output:
 *	List		-- List of UIAs giving file names of files residing
 *			   in DirName and matching FilePattern
 *				(cf. directory/3 in fsunix.pro)
 */

getDirEntries(void)
{
    PWord v1, v2, v3;
    int   t1, t2, t3;
    char *dirName, *fName, *pattern;
    DIR  *dirp;
    struct dirent *dirEntry;
    PWord consCell, head, sym, tail, nil;
    int   consType, headType, symType, tailType, nilType;
/*    char *regexComp;   */
    int   regexVal;
#ifdef HAVE_REGCMP
	regex_t regstruct;
#endif

    PI_getan(&v1, &t1, 1);
    PI_getan(&v2, &t2, 2);
    PI_getan(&v3, &t3, 3);

    /* Make sure file name & pattern are atoms or UIAs */
    if (!getstring((UCHAR **)&dirName, v1, t1)
     || !getstring((UCHAR **)&pattern, v2, t2))
	PI_FAIL;


    /* printf("dirName=%s pattern=%s \n",dirName,pattern); */

#ifdef HAVE_REGCMP
    if (regcomp(&regstruct, pattern, REG_ICASE | REG_NOSUB) )
		PI_FAIL;
#else
    if (re_comp(pattern) != NULL)
		PI_FAIL;
#endif

    dirp = opendir(dirName);

    for (dirEntry = readdir(dirp); dirEntry != NULL; dirEntry = readdir(dirp)) {

#ifdef HAVE_REGCMP
	if ((regexVal = regexec(&regstruct, dirEntry->d_name, 0, 0, 0)) == 0) {
#else
	if ((regexVal = re_exec(dirEntry->d_name)) == 1) {
#endif
	    PI_makelist(&consCell, &consType);
	    if (!PI_unify(v3, t3, consCell, consType)) {
#ifdef HAVE_REGCMP
    		regfree(&regstruct);
#endif
		PI_FAIL;
		}
	    PI_gethead(&head, &headType, consCell);
	    PI_makeuia(&sym, &symType, dirEntry->d_name);
	    if (!PI_unify(head, headType, sym, symType)){
#ifdef HAVE_REGCMP
    		regfree(&regstruct);
#endif
		PI_FAIL;
		}
	    PI_gettail(&v3, &t3, consCell);

	    /* printf("regexVal=%d ok-dirEntry=%d dirEntry->d_name=%s\n",
	     * regexVal,dirEntry,dirEntry->d_name);
	     */
	}
    }

#ifdef HAVE_REGCMP
    regfree(&regstruct);
#endif
    PI_makesym(&nil, &nilType, "[]");

    /*
     * printf("regexVal=%d NO-dirEntry=%d dirEntry->d_name=%s\n",
     * regexVal,dirEntry,dirEntry->d_name);
     */

    if (!PI_unify(v3, t3, nil, nilType))
	PI_FAIL;

    closedir(dirp);

    PI_SUCCEED;
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

    if (stat(pathName, &fileStats) == -1)
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
#ifndef	MOT_DELTA68k
#ifndef SCO_UNIX
#if !defined(__GO32__) && !defined(OS2)
    else if (S_ISLNK(fileMode))
	fileType = 5;
    else if (S_ISSOCK(fileMode))
	fileType = 6;
#endif /* !__GO32__ && !OS2 */
#endif /* SCO_UNIX */
#endif /* MOT_DELTA68k */
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
    if (!PI_unify(arg, argType, (fileStats.st_size+511)/512, PI_INT))
	PI_FAIL;

    PI_SUCCEED;
}


#ifdef HAVE_SYMLINK
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

read_link(void)
{
    PWord v1, v2;
    int   t1, t2;
    char *pathName;
    PWord sym;
    int   symType;
    char  buffer1[1024];

    PI_getan(&v1, &t1, 1);
    PI_getan(&v2, &t2, 2);

    /* Make sure SourcePath name is an atom or UIA */
    if (!getstring((UCHAR **)&pathName, v1, t1))
	PI_FAIL;

    if (!readlink(pathName, buffer1, 1024))
	PI_FAIL;

    PI_makeuia(&sym, &symType, buffer1);
    if (!PI_unify(v2, t2, sym, symType))
	PI_FAIL;

    PI_SUCCEED;
}

/*
 * make_symlink/2
 * make_symlink(TargetPath, LinkPath)
 *
 * Inputs:
 *	SourcePath	-- UIA giving a path to file which is a to be the
 *			   target of the symbolic link;
 *	LinkPath	-- path to the file which will be the symbolic link
 * Note:
 *	The call
 *		make_symlink(foo,bar)
 *	will fail;  one needs are more complete path, such as:
 *		make_symlink('./foo','./bar')
 */

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

    if (!symlink(pathName1, pathName2))
	PI_FAIL;

    PI_SUCCEED;
}
#endif /* HAVE_SYMLINK */

/*
 * comp_file_times/2       (--> pcmp_fs/2)
 * comp_file_times(+File1, +File2)
 *
 * Succeeds if the last modification time of File1 is earlier than the
 * last modification time of File2.
 */

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
 *      mkdir/1 (--> pmkdir/1 )
 *      mkdir(+DirPath)
 *
 *      Creates the indicated directory.
 */

pmkdir(void)
{
    PWord v1;
    int   t1;
    char *pathName;

    PI_getan(&v1, &t1, 1);

    /* Make sure file name & pattern are atoms or UIAs */
    if (!getstring((UCHAR **)&pathName, v1, t1))
	PI_FAIL;

    if (mkdir(pathName, S_IRWXU) == -1)
	PI_FAIL;

    PI_SUCCEED;
}

pgetcwd(void)
{
    PWord v1, sym;
    int   t1, symType;
    char  pathName[MAXPATHLEN];

    PI_getan(&v1, &t1, 1);

    if (getcwd(pathName, MAXPATHLEN) == 0)
	PI_FAIL;

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

pchdir(PE)
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

int
canonicalize_pathname(void)
{
    PWord v1, v2, vp;
    int   t1, t2, tp;
    char *inpath;
    char *outpath;
    char *filename;
    char filbuf[100];

    PI_getan(&v1, &t1, 1);
    PI_getan(&v2, &t2, 2);

    if (getstring((UCHAR **)&inpath, v1, t1))
    {
      _fixpath(inpath, filbuf);
      outpath = strdup(filbuf);
      strcat(filbuf, "/.");
      if (access(filbuf, 0))
      {
	filename = strrchr(outpath, '/');
	*filename++ = 0;
      }
    }
    else
      fprintf(stderr, "dj: no getstring\n");

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

int
pgetpid(void)
{
    PWord v1, vpid;
    int t1;
    if (PI_unify(v1, t1, 0, PI_INT))
	PI_SUCCEED;
    else
	PI_FAIL;
}

/* *INDENT-OFF* */
PI_BEGIN
PI_PDEFINE("$getDirEntries", 3, getDirEntries, "_getDirEntries")
PI_PDEFINE("$getFileStatus", 2, getFileStatus, "_getFileStatus")
PI_PDEFINE("comp_file_times", 2, pcmp_fs, "_pcmp_fs")
PI_PDEFINE("rmdir", 1, prmdir, "_prmdir")
PI_PDEFINE("mkdir", 1, pmkdir, "_pmkdir")
PI_PDEFINE("unlink", 1, punlink, "_punlink")
PI_PDEFINE("getcwd", 1, pgetcwd, "_pgetcwd")
PI_PDEFINE("chdir", 1, pchdir, "_pchdir")
#ifdef HAVE_SYMLINK
PI_PDEFINE("read_link", 2, read_link, "_read_link")
PI_PDEFINE("make_symlink", 2, make_symlink, "_make_symlink")
#endif /* HAVE_SYMLINK */
    PI_PDEFINE("canonicalize_pathname", 2, canonicalize_pathname, "_canonicalize_pathname")
    PI_PDEFINE("getpid",1,pgetpid,"_pgetpid")
PI_END
/* *INDENT-ON* */
void
init_fsutils(void)
{
    PI_INIT;
}

#endif /* DOS */
