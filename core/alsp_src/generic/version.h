/*===============================================================*
 *                      version.h
 *       Copyright (c) 1990-1996 by Applied Logic Systems, Inc.
 *
 *          Version number - Thu Apr 4 09:28:36 EST 1996
 *===============================================================*/

#define VERSION_STRING "2.08fBETA"
#define VERSION_MAJOR 2
#define VERSION_MINOR 0
#define VERSION_PATCH 8

/* 
Win32 File Flag choices:

VS_FF_DEBUG
VS_FF_PRERELEASE
VS_FF_PATCHED
VS_FF_PRIVATEBUILD
VS_FF_INFOINFERRED
VS_FF_SPECIALBUILD
0x0L for final release
*/
#define VERSION_WIN32_FILEFLAGS VS_FF_PRERELEASE

/* 
MacOS Release Stage choices:

development
alpha
beta
final
*/

#define VERSION_MACOS_RELEASE_STAGE beta