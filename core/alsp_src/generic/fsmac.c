/*===========================================================================*
 |              fsunix.c
 |      Copyright (c) 1991-1994 Applied Logic Systems, Inc.
 |
 |      -- File System Access & Manipulation -- Mac
 |
 |  This is one of a group of files providing file system access:
 |		fsunix.c, fsdos.c, fsmac.c, ..., etc.,...
 |  ALL of these files are loaded when the system is built.  However,
 |  each file, after the initial  #include "defs.h"  is #ifdef'd with
 |  the appropriate constant (UNIX, DOS, ....);  this #ifdef applies to
 |  the entire file, so only one of the files is actually loaded.
 |
 | Author:  Ron DiNapoli
 | Date:    Created 8/27/92
 | 12/11/94 - C. Houpt -- Updated file routines.
 *===========================================================================*/
#include "defs.h"

#ifdef MacOS

#ifdef HAVE_GUSI
#include <GUSI.h>

static Boolean MatchName(const char * name, const char * pattern)
{
    while (*pattern) {
	switch (*pattern) {
	case '\\':
	case '¶':
	   if (!*++pattern)
			--pattern;	// special case at end of pattern
	// Fall through
	default:
	    if (!*name || toupper(*name) != toupper(*pattern))
		return false;
	    ++name;
	    ++pattern;
	    break;

	case '?':
	    if (!*name) return false;
	    ++name;
	    ++pattern;
			
	    break;
	case '*':
	case 'Å':
	    while (!MatchName(name, pattern+1)) {
		if (!*name)
		    return false;	// "a", "*b"
		++name;			// "ba", "*a"  or "ba", "*b"
	    }
	    return true; 		// "a", "*a"
	}
    }
	
    return !*pattern && !*name;
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

static int
getDirEntries()
{
    PWord v1, v2, v3;
    int   t1, t2, t3;
    char *dirName, *pattern;
    DIR  *dirp;
    struct dirent *dirEntry;
    PWord consCell, head, sym, nil;
    int   consType, headType, symType, nilType;
#ifdef HAVE_REGCMP
    char *regexComp;
#endif

    PI_getan(&v1, &t1, 1);
    PI_getan(&v2, &t2, 2);
    PI_getan(&v3, &t3, 3);

    /* Make sure file name & pattern are atoms or UIAs */
    if (!getstring((UCHAR **)&dirName, v1, t1)
     || !getstring((UCHAR **)&pattern, v2, t2))
	PI_FAIL;

#ifdef HAVE_REGCMP
    if ((regexComp = regcmp(pattern, (char *) 0)) == NULL)
	{
		PI_FAIL;
	}
#else
#ifndef MacOS
    if (re_comp(pattern) != NULL)
	{
		PI_FAIL;
	}
#endif
#endif

    dirp = opendir(dirName);
	if (dirp == NULL)
	{
#ifdef HAVE_REGCMP
		free(regexComp);
#endif
		PI_FAIL;
	}

    for (dirEntry = readdir(dirp); dirEntry != NULL; dirEntry = readdir(dirp)) {
#ifdef HAVE_REGCMP
	if (regex(regexComp, dirEntry->d_name) != NULL)
#else
#ifdef MacOS
	if (MatchName(dirEntry->d_name, pattern))
#else
	if (re_exec(dirEntry->d_name) == 1)
#endif
#endif
	{
	    PI_makelist(&consCell, &consType);
	    if (!PI_unify(v3, t3, consCell, consType)) {
#ifdef HAVE_REGCMP
		free(regexComp);
#endif
    		closedir(dirp);
		PI_FAIL;
	    }
	    PI_gethead(&head, &headType, consCell);
	    PI_makeuia(&sym, &symType, dirEntry->d_name);
	    if (!PI_unify(head, headType, sym, symType)) {
#ifdef HAVE_REGCMP
		free(regexComp);
#endif
    		closedir(dirp);
		PI_FAIL;
	    }
	    PI_gettail(&v3, &t3, consCell);
	}
    }
    closedir(dirp);

#ifdef HAVE_REGCMP
    free(regexComp);
#endif

    PI_makesym(&nil, &nilType, "[]");

    if (!PI_unify(v3, t3, nil, nilType))
	PI_FAIL;


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

static int getFileStatus(void)
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
    else if (S_ISLNK(fileMode))
	fileType = 5;
    else if (S_ISSOCK(fileMode))
	fileType = 6;
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
    if (!PI_unify(arg, argType, fileStats.st_blocks, PI_INT))
	PI_FAIL;

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

static int
read_link()
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

static int
make_symlink()
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

    if (symlink(pathName1, pathName2))
	PI_FAIL;

    PI_SUCCEED;
}

/*
 * comp_file_times/2       (--> pcmp_fs/2)
 * comp_file_times(+File1, +File2)
 *
 * Succeeds if the last modification time of File1 is earlier than the
 * last modification time of File2.
 */

static int
pcmp_fs()
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

static int
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

static int pmkdir(void)
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

#ifdef MacOS
    if (mkdir(pathName) == -1)
#else
    if (mkdir(pathName, v2) == -1)
#endif
	PI_FAIL;

    PI_SUCCEED;
}

/*
 * Returns 1 if fname is a directory, 0 otherwise.
 *
 */
int isdir(CONST char *fname)
{
    struct stat buf;

    if (stat(fname, &buf) == -1)
	return (0);

    return (buf.st_mode & S_IFDIR);
}

long 
get_file_modified_time(fname)
    char *fname;
{
    struct stat buf;

    if (stat(fname, &buf) == -1 || buf.st_mode & S_IFDIR)
	return (long) 0;

    return buf.st_mtime;
}


/*
 * rmdir/1 (--> prmdir/1 )
 * rmdir(+DirPath)
 *
 * Removes the indicated directory
 */

static int prmdir(void)
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

static int pgetpid(void)
{
    PWord v1, vpid;
    int   t1, tpid;

    PI_getan(&v1, &t1, 1);
#ifdef MacOS
    {
	ProcessSerialNumber PSN;
	
	if(GetCurrentProcess(&PSN) != noErr) PI_FAIL;
	
	PI_makedouble(&vpid, &tpid, (double)PSN.highLongOfPSN * ULONG_MAX + (double)PSN.lowLongOfPSN);
    }
#else
    PI_makedouble(&vpid, &tpid, (double) getpid());
#endif
    if (PI_unify(v1, t1, vpid, tpid))
	PI_SUCCEED;
    else
	PI_FAIL;
}











#else /* GUSI */

#include <ctype.h>

#include <unix.h>
#include <stdio.h>
#include <limits.h>

#include <Types.h>
#include <Files.h>
#include <ToolUtils.h>
#include <Memory.h>
#include <OSUtils.h>
#include <Processes.h>
#include <Errors.h>


#if defined(THINK_C)
#include <pascal.h>
#elif defined(__MWERKS__)
#include <Strings.h>
#endif

#define MIN(a,b) (((a)<(b))?(a):(b))


#ifndef MAXPATHLEN
#ifdef PATHSIZE
#define MAXPATHLEN  PATHSIZE
#else
#define MAXPATHLEN  1024
#endif
#endif

static int
pgetcwd()
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

static int pchdir(void)
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

static int punlink(void)
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



/* ceh - I'm not really sure what cononicalize_pathname does, or wether it really applies
   on the Mac.  This function is a NOP. */
static int canonicalize_pathname(void)
{
    PWord v1, v2, vp;
    int   t1, t2, tp;
    char *inpath;
    char *outpath;
    char *filename;

    PI_getan(&v1, &t1, 1);
    PI_getan(&v2, &t2, 2);

    if (!getstring((UCHAR **)&inpath, v1, t1))
	PI_FAIL;

    filename = inpath;
    outpath = "";
    
    if (filename) {
	int plen = strlen(outpath);
	char *hs;
	PI_allocuia(&vp, &tp, (int)(plen+strlen(filename)+1));
	hs = PI_getuianame(0,vp,0);
	memcpy(hs,outpath,(size_t)plen);
	strcpy(hs+plen,filename);
    }
    else
	PI_makeuia(&vp, &tp, outpath);

    if (PI_unify(v2, t2, vp, tp))
	PI_SUCCEED;
    else
	PI_FAIL;
}



/* Other routines. */


int absolute_pathname(const char *name)
{    
    return *name != ':' && (strchr(name, ':') != NULL);
}

/*************************************************************************************/

static unsigned char *pStrcat(unsigned char *dest, const unsigned char *src)
{
    long  sLen = MIN(*src, 255 - *dest);

    BlockMove(src + 1, dest + *dest + 1, sLen);
    *dest += sLen;
    return (dest);
}

static unsigned char *
pStrcpy(unsigned char *dest, const unsigned char *src)
{
    BlockMove(src, dest, (long) *src + 1);
    return (dest);
}

static unsigned char *cpy_c2pstr(unsigned char *pstr, const char *cstr)
{
    *pstr = MIN(strlen(cstr), 255);
    memmove(pstr+1, cstr, *pstr);
    return pstr;
}

#if !defined(__MPW_MWERKS__) && !defined(applec)
int access(const char *path, int x)
{
    Str255 pPath;
    CInfoPBRec fp;
    int result;
    
    cpy_c2pstr(pPath, path);

    fp.hFileInfo.ioCompletion = 0;
    fp.hFileInfo.ioNamePtr = pPath;
    fp.hFileInfo.ioVRefNum = 0;
    fp.hFileInfo.ioFVersNum = 0;
    fp.hFileInfo.ioFDirIndex = 0;
    fp.hFileInfo.ioDirID = 0;

    if (PBGetCatInfo((CInfoPBPtr) & fp, false)) result = -1;
    else result = 0;
    
    return result;
}
#endif



static OSErr PathNameFromDirID(long DirID, short vRefNum, char *buf, size_t size)
{
    OSErr err;
    CInfoPBRec	block;
    Str255	directoryName;
    char *pathName;
    size_t pathNameLen;
    
    /* For efficiency, individual directory names are accumulated at the END
       of the buffer.  PathName is a valid C string, that grows to the left. */
    buf[size-1] = 0;
    pathName = buf + size - 1;
    pathNameLen = 0;
    
    block.dirInfo.ioNamePtr = directoryName;
    block.dirInfo.ioDrParID = DirID;
    
    do {
	block.dirInfo.ioVRefNum = vRefNum;
	block.dirInfo.ioFDirIndex = -1;
	block.dirInfo.ioDrDirID = block.dirInfo.ioDrParID;

	err = PBGetCatInfo(&block,false);
	if (err != noErr) goto fail;
	
	if (directoryName[0] + pathNameLen + 1 > size) {
	   err = memFullErr;
	   goto fail;
	}
	pathName--;
	*pathName = ':';
	pathName -= directoryName[0];
	memmove(pathName, directoryName+1, directoryName[0]);
	pathNameLen += directoryName[0] + 1;
    } while (block.dirInfo.ioDrDirID != fsRtDirID);

    /* Finally, move pathName from the end of the buffer to the front. */
    memmove(buf, pathName, pathNameLen + 1);

    fail:
    
    return err;
}


char *getcwd(char *buf, int size)
{
    OSErr	err;
    Str255 volName;
    WDPBRec pBlock;

    if (buf == NULL || size < 1) return NULL;
    
    pBlock.ioCompletion = NULL;
    pBlock.ioNamePtr = volName;
    err = PBHGetVol(&pBlock, 0);
    if (err != noErr) return NULL;
    
    err = PathNameFromDirID(pBlock.ioWDDirID, pBlock.ioWDVRefNum, buf, size);
    
    if (err == noErr) return buf;
    else return NULL;
}

long	get_file_modified_time(const char *fname)
{
    Str255 pfname;
    FileParam fp;
    short workingDirectory;

    cpy_c2pstr(pfname, fname);

    GetVol(NULL, &workingDirectory);
    fp.ioCompletion = 0;
    fp.ioNamePtr = pfname;
    fp.ioVRefNum = workingDirectory;
    fp.ioFVersNum = 0;
    fp.ioFDirIndex = 0;
 
    if (PBGetFInfo((ParmBlkPtr) & fp, false)) return (0);

    return (fp.ioFlMdDat);
}

int	isdir(const char *fname)
{
    Str255 pfname;    
    FileParam fp;

    cpy_c2pstr(pfname, fname);

    fp.ioCompletion = 0;
    fp.ioNamePtr = pfname;
    fp.ioVRefNum = 0;
    fp.ioFVersNum = 0;
    fp.ioFDirIndex = 0;

    PBGetFInfo((ParmBlkPtr) & fp, false);

    if (BitTst(&fp.ioFlAttrib, 3)) return (1);

    return (0);
}

/*
 *
 *  $getFileStatus/2
 *  $getFileStatus(FilePath,StatusTerm)
 *
 *  Input:
 *     FilePath    --   UIA giving a path to the file
 *  Output:
 *     StatusTerm  --   A 5-ary term provinding info about the file
 *
 *     fileStatus(FileType,ModTime,OwnPermiss,ByteSize,Blocks)
 *
 *  MACINTOSH NOTE:  On the Mac, the "Blocks" field is somewhat useless
 *                   and will always return 0.
 *                   The FileType field is bound to the Macintosh
 *                   filetype, and then mapped accordingly in fsmac.pro
 */

static int getFileStatus(void)
{
    PWord v1, v2, vtime;
    int   t1, t2, ttime;
    char *cPathName;
    Str255 pathName;
    PWord sTag, pstructure, arg;
    int   sTagType, pstructureType, argType;
    int   fileType, ownerPermiss;
    CInfoPBRec fp;


    FILE *f;
    short is_directory = 0;

    PI_getan(&v1, &t1, 1);
    PI_getan(&v2, &t2, 2);

    if (!getstring((UCHAR **)&cPathName, v1, t1))
	PI_FAIL;

    cpy_c2pstr(pathName, cPathName);
    
    fp.hFileInfo.ioCompletion = 0;
    fp.hFileInfo.ioNamePtr = pathName;
    fp.hFileInfo.ioVRefNum = 0;
    fp.hFileInfo.ioFVersNum = 0;
    fp.hFileInfo.ioFDirIndex = 0;

    if (PBGetFInfo((ParmBlkPtr) & fp, false)) {
	if (PBGetCatInfo((CInfoPBPtr) & fp, false)) {
	    PI_FAIL;
	}
	is_directory = 1;
    }

    PI_makesym(&sTag, &sTagType, "fileStatus");
    PI_makestruct(&pstructure, &pstructureType, sTag, 5);

    if (!PI_unify(v2, t2, pstructure, pstructureType))
	PI_FAIL;

    if (is_directory) {
	ownerPermiss = *(((char *) &fp) + 31) & 6;
    }
    else {
	if ((f = fopen(cPathName, "r")) == NULL)
	    ownerPermiss = 0;
	else {
	    fclose(f);
	    if ((f = fopen(cPathName, "a")) == NULL)
		ownerPermiss = 4;
	    else {
		fclose(f);
		ownerPermiss = 6;
	    }
	}
    }

    /* File Type */
    if (is_directory) fileType = 1; else fileType = 4;

    PI_getargn(&arg, &argType, pstructure, 1);
    if (!PI_unify(arg, argType, fileType, PI_INT))
	PI_FAIL;
    
#if 0
    PI_makeuia(&vftype, &tftype, "Fldr");
    if (!is_directory) {
	uia_buf = PI_getuianame(0, vftype, 0);
	*(long *) uia_buf = fp.hFileInfo.ioFlFndrInfo.fdType;
    }
    PI_getargn(&arg, &argType, pstructure, 1);
    if (!PI_unify(arg, argType, vftype, tftype))
	PI_FAIL;
#endif

    /* File Modification Date */
    PI_getargn(&arg, &argType, pstructure, 2);
    PI_makedouble(&vtime, &ttime, (double) fp.hFileInfo.ioFlMdDat);
    if (!PI_unify(arg, argType, vtime, ttime))
	PI_FAIL;

    /* File Owner Permissions */
    PI_getargn(&arg, &argType, pstructure, 3);
    if (!PI_unify(arg, argType, ownerPermiss, PI_INT))
	PI_FAIL;

    /* File Size */
    PI_getargn(&arg, &argType, pstructure, 4);
    if (!PI_unify(arg, argType, fp.hFileInfo.ioFlPyLen + fp.hFileInfo.ioFlRPyLen, PI_INT))
	PI_FAIL;

    /* Block Allocation (not defined on the Mac) */
    PI_getargn(&arg, &argType, pstructure, 5);
    if (!PI_unify(arg, argType, 0, PI_INT))
	PI_FAIL;

    PI_SUCCEED;
}



/*
 * chdir/1
 */

/*
 * ChangeVolume() checks to see if we are dealing with an absolute
 * pathname and, if so, sets the volume separately by a call to
 * setvol()
 */

static OSErr ChangeVolume(const char *pathname)
{
    const char *ptr1, *ptr2;
    Str255 volname;
    short i;

    if (!absolute_pathname(pathname)) return fnfErr; /* Not an absolute path */

    ptr2 = strchr(pathname, ':');
    ptr1 = pathname;

    i = 1;
    while (ptr1 <= ptr2) {
	volname[i++] = *ptr1++;
    }

    volname[0] = i-1;
    return SetVol(volname, 0);
}

/* What is this old stuff!!!! */
#if 0
static OSErr SetDirectoryFromPath(const char *path)
{
    CInfoPBRec pb;
    short WDRefNum;
    short mountedvol;
    Str27 mvname, ioName;
    OSErr err;

    err = ChangeVolume(path);
    if (err != noErr) goto fail;
    
    err = GetVol(mvname, &mountedvol);
    if (err != noErr) goto fail;

    cpy_c2pstr(ioName, path);
    pb.dirInfo.ioNamePtr = ioName;
    pb.dirInfo.ioVRefNum = mountedvol;
    pb.dirInfo.ioDrDirID = 0;
    pb.dirInfo.ioFDirIndex = 0;

    err = PBGetCatInfo(&pb, 0);
    if (err != noErr) goto fail;

    /* Set the directory... */

    err = OpenWD((short) mountedvol, pb.dirInfo.ioDrDirID, 'ALSN', &WDRefNum);
    if (err != noErr) goto fail;
    
    err = SetVol(0, WDRefNum);
    if (err != noErr) goto fail;
    
    fail:
    
    return err;
}
#endif
static OSErr	GetDirID(short vRefNum,
						 long dirID,
						 StringPtr name,
						 long *theDirID,
						 Boolean *isDirectory)
{
	CInfoPBRec pb;
	OSErr error;

	pb.hFileInfo.ioNamePtr = name;
	pb.hFileInfo.ioVRefNum = vRefNum;
	pb.hFileInfo.ioDirID = dirID;
	pb.hFileInfo.ioFDirIndex = 0;	/* use ioNamePtr and ioDirID */
	error = PBGetCatInfoSync(&pb);
	*theDirID = pb.hFileInfo.ioDirID;
	*isDirectory = (pb.hFileInfo.ioFlAttrib & ioDirMask) != 0;
	return ( error );
}

static	OSErr	DirIDFromFSSpec(const FSSpec *spec,
								long *theDirID,
								Boolean *isDirectory)
{
	return ( GetDirID(spec->vRefNum, spec->parID, (StringPtr)spec->name,
			 theDirID, isDirectory) );
}

static OSErr SetDirectoryFromPath(const char *path)
{
	Str255 ppath;
    OSErr err;
    WDPBRec pb;
    long newWDDirID;
    Boolean isDirectory;
	FSSpec spec;
	
	
	pb.ioCompletion = NULL;
	pb.ioNamePtr = ppath;
	err = PBHGetVol(&pb, false);
	if (err != noErr) goto fail;

	cpy_c2pstr(ppath, path);
	err = FSMakeFSSpec(pb.ioWDVRefNum, pb.ioWDDirID, ppath, &spec);
	if (err != noErr) goto fail;
	
	err = DirIDFromFSSpec(&spec, &newWDDirID, &isDirectory);
	if (err != noErr) goto fail;
	
	if (!isDirectory) {
		err = fnfErr;
		goto fail;
	}
	
	pb.ioCompletion = NULL;
	pb.ioNamePtr = NULL;
	pb.ioVRefNum = spec.vRefNum;
	pb.ioWDDirID = newWDDirID;
	err = PBHSetVol(&pb, false);
	if (err != noErr) goto fail;
	
fail:
	
	return err;
}


int chdir(const char *dirname)
{
    if (SetDirectoryFromPath(dirname) != noErr) return -1;
    else return 0;
}


static int pgetpid(void)
{
    PWord v1, vpid;
    int   t1, tpid;
    ProcessSerialNumber PSN;

    PI_getan(&v1, &t1, 1);
    
    if(GetCurrentProcess(&PSN) != noErr) PI_FAIL;
    
    PI_makedouble(&vpid, &tpid, (double)PSN.highLongOfPSN * ULONG_MAX + (double)PSN.lowLongOfPSN);
    if (PI_unify(v1, t1, vpid, tpid))
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

static OSErr FileModTime(const char *pathName, unsigned long *modTime)
{
    Str255 pPathName;
    FileParam fp;
    OSErr err;

    cpy_c2pstr(pPathName, pathName);
    
    fp.ioCompletion = 0;
    fp.ioNamePtr = pPathName;
    fp.ioVRefNum = 0;
    fp.ioFVersNum = 0;
    fp.ioFDirIndex = 0;

    err = PBGetFInfo((ParmBlkPtr) & fp, false);
    if (err != noErr) return err;
    
    *modTime = fp.ioFlMdDat;
    return noErr;
}

static int
pcmp_fs(void)
{
    PWord v1, v2;
    int   t1, t2;
    char *pathName1, *pathName2;
    unsigned long fileModTime1, fileModTime2;

    PI_getan(&v1, &t1, 1);
    PI_getan(&v2, &t2, 2);

    /* Make sure file name & pattern are atoms or UIAs */
    if (!getstring((UCHAR **)&pathName1, v1, t1)
     || !getstring((UCHAR **)&pathName2, v2, t2))
	PI_FAIL;

    if ((FileModTime(pathName1, &fileModTime1) != noErr) ||
	(FileModTime(pathName2, &fileModTime2) != noErr))
	PI_FAIL;

    if (fileModTime1 < fileModTime2)
	PI_SUCCEED;

    PI_FAIL;
}


#endif /* HAVE_GUSI */



/* *INDENT-OFF* */
PI_BEGIN
    PI_PDEFINE("getcwd",1,pgetcwd,"_pgetcwd")
    PI_PDEFINE("chdir",1,pchdir,"_pchdir")
    PI_PDEFINE("unlink", 1, punlink, "_punlink")

#ifdef HAVE_GUSI
    PI_PDEFINE("$getDirEntries", 3, getDirEntries, "_getDirEntries")
    PI_PDEFINE("$getFileStatus",2,getFileStatus,"_getFileStatus") 
    PI_PDEFINE("comp_file_times", 2, pcmp_fs, "_pcmp_fs")
    PI_PDEFINE("rmdir", 1, prmdir, "_prmdir")
    PI_PDEFINE("mkdir", 2, pmkdir, "_pmkdir")

    PI_PDEFINE("read_link", 2, read_link, "_read_link")
    PI_PDEFINE("make_symlink", 2, make_symlink, "_make_symlink")
#else
    PI_PDEFINE("$getFileStatus",2,getFileStatus,"_getFileStatus") 
    PI_PDEFINE("comp_file_times", 2, pcmp_fs, "_pcmp_fs")
#endif

    PI_PDEFINE("canonicalize_pathname", 2, canonicalize_pathname, "_canonicalize_pathname")
    PI_PDEFINE("getpid",1,pgetpid,"_pgetpid")
PI_END
/* *INDENT-ON* */

void init_fsutils(void)
{
    PI_INIT;
}


#endif /* MacOS */
