/*
 * fsmac.c
 *      Copyright (c) 1991-1993 Applied Logic Systems, Inc.
 *
 * File System Access & Manipulation -- Mac
 *
 * Author:  Ron DiNapoli
 * Date:    Created on 8/27/92
 */

#include "defs.h"

#ifdef MacOS

#include <stdio.h>

#include <StdDef.h>
#include <Types.h>
#include <Files.h>
#include <ToolUtils.h>
#include <Memory.h>
#include <OSUtils.h>

pascal void
debugger()
    extern 0xa9ff;

/*
 *  Local globals needed to maintain directory status...
 */

#define CurDirStore *(long *) 0x398
#define SFSaveDisk *(short *) 0x214

    static long gCurrentWDDirID;
    static short gCurrentVRefNum;


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


getFileStatus()
{
    PWord v1, v2, vtime, vftype;
    int   t1, t2, ttime, tftype;
    char *pathName, *uia_buf;
    PWord sTag, pstructure, arg;
    int   sTagType, pstructureType, argType;
    int   fileMode, fileType, ownerPermiss;
    CInfoPBRec fp;


    FILE *f;
    short is_directory = 0;

    PI_getan(&v1, &t1, 1);
    PI_getan(&v2, &t2, 2);

    if (!getstring((UCHAR **)&pathName, v1, t1))
	PI_FAIL;

    c2pstr(pathName);
    fp.hFileInfo.ioCompletion = 0;
    fp.hFileInfo.ioNamePtr = pathName;
    fp.hFileInfo.ioVRefNum = 0;
    fp.hFileInfo.ioFVersNum = 0;
    fp.hFileInfo.ioFDirIndex = 0;

    if (PBGetFInfo((ParmBlkPtr) & fp, false)) {
	if (PBGetCatInfo((CInfoPBPtr) & fp, false)) {
	    p2cstr(pathName);
	    PI_FAIL;
	}
	is_directory = 1;
    }

    p2cstr(pathName);

    PI_makesym(&sTag, &sTagType, "fileStatus");
    PI_makestruct(&pstructure, &pstructureType, sTag, 5);

    if (!PI_unify(v2, t2, pstructure, pstructureType))
	PI_FAIL;

    if (is_directory) {
	ownerPermiss = *(((char *) &fp) + 31) & 6;
    }
    else {
	if ((f = fopen(pathName, "r")) == NULL)
	    ownerPermiss = 0;
	else {
	    fclose(f);
	    if ((f = fopen(pathName, "a")) == NULL)
		ownerPermiss = 4;
	    else {
		fclose(f);
		ownerPermiss = 6;
	    }
	}
    }

    /* File Type */
    PI_makeuia(&vftype, &tftype, "Fldr");
    if (!is_directory) {
	uia_buf = PI_getuianame(0, vftype, 0);
	*(long *) uia_buf = fp.hFileInfo.ioFlFndrInfo.fdType;
    }
    PI_getargn(&arg, &argType, pstructure, 1);
    if (!PI_unify(arg, argType, vftype, tftype))
	PI_FAIL;

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
 *   getcwd/1, and friends...
 *
 */

#define gHaveAUX 0
#define MIN(a,b) (((a)<(b))?(a):(b))

char *
pStrcat(dest, src)
    unsigned char *dest, *src;
{
    long  sLen = MIN(*src, 255 - *dest);

    BlockMove(src + 1, dest + *dest + 1, sLen);
    *dest += sLen;
    return (dest);
}


char *
pStrcpy(dest, src)
    unsigned char *dest, *src;
{
    BlockMove(src, dest, (long) *src + 1);
    return (dest);
}


char *
PathNameFromDirID(DirID, VRefNum, s)
    long  DirID;
    short VRefNum;
    char *s;
{
    CInfoPBRec block;
    Str255 directoryName;
    OSErr err;

    *s = 0;
    block.dirInfo.ioNamePtr = directoryName;
    block.dirInfo.ioDrParID = DirID;

    do {
	block.dirInfo.ioVRefNum = VRefNum;
	block.dirInfo.ioFDirIndex = -1;
	block.dirInfo.ioDrDirID = block.dirInfo.ioDrParID;

	err = PBGetCatInfo(&block, false);
	if (gHaveAUX) {
	    if (directoryName[1] != '/')
		pStrcat(directoryName, "\p/");
	}
	else
	    pStrcat(directoryName, "\p:");

	pStrcat(directoryName, s);
	pStrcpy(s, directoryName);
    } while (block.dirInfo.ioDrDirID != fsRtDirID);

    return (s);
}

pgetcwd()
{
    PWord v1, sym;
    int   t1, symType;
    char  pathName[255];
    WDPBRec pBlock;

    PI_getan(&v1, &t1, 1);

    pBlock.ioNamePtr = NewPtr(255);
    PBHGetVol(&pBlock, 0);
    DisposPtr(pBlock.ioNamePtr);
    PathNameFromDirID(pBlock.ioWDDirID, pBlock.ioWDVRefNum, pathName);
    p2cstr(pathName);

    PI_makeuia(&sym, &symType, pathName);

    if (!PI_unify(v1, t1, sym, symType))
	PI_FAIL;

    PI_SUCCEED;
}

/*
 * chdir/1
 */

static short CurWDRefNum = 0;

/*
 * ChangeVolume() checks to see if we are dealing with an absolute
 * pathname and, if so, sets the volume separately by a call to
 * setvol()
 */

ChangeVolume(pathname)
    char *pathname;
{
    char *ptr1, *ptr2, volname[256];
    short i;

    if (pathname[0] == ':')
	return;			/* Not an absolute path */
    if (!(ptr2 = (char *) strchr(pathname, (char) ':')))
	return;
    ptr1 = &pathname[0];
    i = 0;

    while (ptr1 <= ptr2) {
	volname[i++] = *ptr1++;
    }

    volname[i] = 0;
    setvol(volname, nil);
}

SetDirectoryFromPath(path)
    char *path;
{
    WDPBRec paramBlock;
    CInfoPBRec pb;
    short refnum, errnum, WDRefNum;
    long  theDirID, theProcID;
    short saveVol, mountedvol, dirChanged;
    char  mvname[80];

    getvol(nil, &saveVol);
    ChangeVolume(path);
    getvol(nil, &mountedvol);
    getvol(mvname, nil);

    pb.dirInfo.ioNamePtr = NewPtr(strlen(path) + 1);
    strcpy(pb.dirInfo.ioNamePtr, path);
    c2pstr(pb.dirInfo.ioNamePtr);
    pb.dirInfo.ioVRefNum = paramBlock.ioVRefNum = mountedvol;
    pb.dirInfo.ioDrDirID = 0;
    pb.dirInfo.ioFDirIndex = 0;

    if (PBGetCatInfo(&pb, 0))
	return (0);

    /* Set the directory... */

    if (!(errnum = OpenWD((short) mountedvol, pb.dirInfo.ioDrDirID,
			  'ALSN', &WDRefNum))) {
	if (CurWDRefNum != WDRefNum)
	    dirChanged = true;
    }
    else {
	fprintf(stderr, "%% SetDirectoryFromPath: Error opening new WD (%d)\n",
		errnum);
	return (0);
    }

    if (errnum = setvol(0, WDRefNum)) {
	return (0);
    }

    CurWDRefNum = WDRefNum;
}

pchdir()
{
    PWord v1, sym;
    int   t1, symtype;
    char *pathName;
    WDPBRec pBlock;

    PI_getan(&v1, &t1, 1);

    if (!getstring((UCHAR **)&pathName, v1, t1))
	PI_FAIL;

    SetDirectoryFromPath(pathName);

    PI_SUCCEED;
}

pgetpid()
{
    PWord v1, vpid;
    if (PI_unify(v1, t1, 0, PI_INT))
	PI_SUCCEED;
    else
	PI_FAIL;
}

/* *INDENT-OFF* */
PI_BEGIN
    PI_PDEFINE("$getFileStatus",2,getFileStatus,"_getFileStatus") 
    PI_PDEFINE("getcwd",1,pgetcwd,"_pgetcwd")
    PI_PDEFINE("chdir",1,pchdir,"_pchdir")
    PI_PDEFINE("getpid",1,pgetpid,"_pgetpid")
PI_END
/* *INDENT-ON* */

init_fsutils()
{
    PI_INIT;
}


#endif /* MacOS */
