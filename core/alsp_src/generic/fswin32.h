/* MSWin32 CRT headers. */

#include <unix.h>

/* There are no symbolic links in Win32, so lstat == stat.
   But what about Win95 aliases? */
#define lstat stat

#define access _access

#define EINTR	3001

#define S_IFBLK	0
#define S_IFIFO	0

#define S_IRUSR	0
#define S_IWUSR	0
#define S_IXUSR	0
